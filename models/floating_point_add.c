#include "mex.h"
// #define DEBUG_PRINTS

// Floating-point bit mapping
#define NUM_BITS 32
#define EXP_BITS 8
#define MANTISSA_BITS 23

// Location of sign bit and exponent field
#define SIGN_BIT (NUM_BITS - 1)
#define EXP_BIT  (SIGN_BIT - EXP_BITS)

// Masks to extract exponent and mantissa fields
#define EXP_MASK      ((1 << EXP_BITS) - 1)
#define MANTISSA_MASK ((1 << MANTISSA_BITS) - 1)

// Maximum exponent field
#define MAX_EXP EXP_MASK

// NaN bit-mapping
#define NAN ((MAX_EXP << MANTISSA_BITS) | (1 << (MANTISSA_BITS - 2)))

// Padded mantissa, +1 bit for sign, +1 bit for rounding
#define PAD_WIDTH   (MANTISSA_BITS + 2)

// Determine which bits to truncate when rounding
// Should be padding + bit growth from add
#define TRUNC_WIDTH (PAD_WIDTH + 1)

// Determine the round bit
// 1 less than truncated bits
#define ROUND_BIT   (TRUNC_WIDTH - 1)

// Determine masking for rounding
// Should select all bits below ROUND_BIT
#define ROUND_MASK  ((1 << ROUND_BIT) - 1)

float floating_point_add(float a, float b)
{
    // View the floating point numbers as uint32's
    unsigned int aUint32, bUint32;
    memcpy(&aUint32, &a, sizeof(float));
    memcpy(&bUint32, &b, sizeof(float));

    #ifdef DEBUG_PRINTS
    mexPrintf("a = 0x%08X\n", aUint32);
    mexPrintf("b = 0x%08X\n", bUint32);
    mexPrintf("\n");
    #endif

    // Extract the sign, exponent, and mantissa fields from the floating point numbers
    unsigned int aSign, bSign;
    unsigned int aExp, bExp;
    unsigned int aMantissa, bMantissa;

    aSign = aUint32 >> SIGN_BIT;
    bSign = bUint32 >> SIGN_BIT;

    aExp = (aUint32 >> EXP_BIT) & EXP_MASK;
    bExp = (bUint32 >> EXP_BIT) & EXP_MASK;

    aMantissa = aUint32 & MANTISSA_MASK;
    bMantissa = bUint32 & MANTISSA_MASK;

    #ifdef DEBUG_PRINTS
    mexPrintf("aSign = %d, aExp = 0x%02X, aMantissa = 0x%06X\n", aSign, aExp, aMantissa);
    mexPrintf("bSign = %d, bExp = 0x%02X, bMantissa = 0x%06X\n", bSign, bExp, bMantissa);
    mexPrintf("\n");
    #endif

    unsigned int sumUint32;
    float sum;

    // Handle Infinity and NaN inputs
    if ((aExp == MAX_EXP) || (bExp == MAX_EXP))
    {
        // NaN
        if ((aMantissa != 0) || (bMantissa != 0))
        {
            sumUint32 = NAN;
        }
        // Infinity
        else
        {
            // -Inf + Inf = NaN
            if ((aExp == MAX_EXP) && (bExp == MAX_EXP) && (aSign != bSign))
            {
                sumUint32 = NAN;
            }
            // a = +/- Inf, b = finite number
            if (aExp == MAX_EXP)
            {
                sumUint32 = (aSign << SIGN_BIT) | (MAX_EXP << EXP_BIT);
            }
            // b = +/- Inf, a = finite number
            else
            {
                sumUint32 = (bSign << SIGN_BIT) | (MAX_EXP << EXP_BIT);
            }
        }
        
        // View the uint32's as floats
        memcpy(&sum, &sumUint32, sizeof(float));
        return sum;
    }

    // Normal floating point number
    // 2^(exp - 127) * (1.fraction)
    if (aExp > 0)
    {
        aMantissa = aMantissa | (1 << MANTISSA_BITS);
    }
    // Subnormal floating point number
    // 2^(-126) * (0.fraction)
    else
    {
        aMantissa = aMantissa << 1;
    }

    // Normal floating point number
    // 2^(exp - 127) * (1.fraction)
    if (bExp > 0)
    {
        bMantissa = bMantissa | (1 << MANTISSA_BITS);
    }
    // Subnormal floating point number
    // 2^(-126) * (0.fraction)
    else
    {
        bMantissa = bMantissa << 1;
    }

    // Create signed operands from mantissa's and sign bits
    long long aOperand = (long long) aMantissa;
    long long bOperand = (long long) bMantissa; 

    if (aSign)
    {
        aOperand = -aOperand;
    }

    if (bSign)
    {
        bOperand = -bOperand;
    }

    #ifdef DEBUG_PRINTS
    mexPrintf("aOperand = 0x%08X\n", (int) aOperand);
    mexPrintf("bOperand = 0x%08X\n", (int) bOperand);
    mexPrintf("\n");
    #endif

    // Determine the maximum exponents and corresponding operands
    unsigned int maxExp = aExp;
    unsigned int minExp = bExp;
    long long maxOperand = aOperand;
    long long minOperand = bOperand;

    if (bExp > aExp)
    {
        maxExp = bExp;
        minExp = aExp;
        maxOperand = bOperand;
        minOperand = aOperand;
    }

    #ifdef DEBUG_PRINTS
    mexPrintf("maxExp = 0x%02X\n", maxExp);
    mexPrintf("minExp = 0x%02X\n", minExp);
    mexPrintf("maxOperand = 0x%08X\n", (int) maxOperand);
    mexPrintf("minOperand = 0x%08X\n", (int) minOperand);
    mexPrintf("\n");
    #endif

    // Pack each operand into a common type with same exponent
    unsigned int expShift = maxExp - minExp;
    if (expShift > PAD_WIDTH)
    {
        expShift = PAD_WIDTH;
    }

    maxOperand = maxOperand << PAD_WIDTH;
    minOperand = minOperand << (PAD_WIDTH - expShift);

    #ifdef DEBUG_PRINTS
    mexPrintf("maxOperand = 0x%016llX\n", maxOperand);
    mexPrintf("minOperand = 0x%016llX\n", minOperand);
    mexPrintf("\n");
    #endif

    // Sum operand with common exponent
    long long sumOperand = maxOperand + minOperand;

    #ifdef DEBUG_PRINTS
    mexPrintf("sumOperand = 0x%016llX\n", sumOperand);
    mexPrintf("\n");
    #endif

    // Make result unsigned and retain sign
    unsigned int sumSign = 0;
    if (sumOperand < 0)
    {
        sumSign = 1;
        sumOperand = -sumOperand;
    }

    #ifdef DEBUG_PRINTS
    mexPrintf("sumSign = %d\n", sumSign);
    mexPrintf("sumOperand = 0x%016llX\n", sumOperand);
    mexPrintf("\n");
    #endif

    // Determine highest bit with '1'
    int maxBit = -1;
    for (int i = 0; i < (2*PAD_WIDTH); ++i)
    {
        if ((sumOperand >> i) & 1)
        {
            maxBit = i;
        }
    }

    #ifdef DEBUG_PRINTS
    mexPrintf("maxBit = %d\n", maxBit);
    mexPrintf("\n");
    #endif

    // If result has non-zero "mantissa"
    if (maxBit > 0)
    {
        // Determine shift to place significant bits in MSB
        // Limit shift to handle subnormal results
        unsigned int maxShift = maxExp + 1;
        unsigned int shift = 2*PAD_WIDTH - (unsigned int) maxBit - 1;
        if (shift > maxShift)
        {
            shift = maxShift;
        }

        #ifdef DEBUG_PRINTS
        mexPrintf("maxShift = %d\n", maxShift);
        mexPrintf("shift = %d\n", shift);
        mexPrintf("\n");
        #endif

        // Shift operand so significant bits are in MSB
        sumOperand = sumOperand << shift;

        #ifdef DEBUG_PRINTS
        mexPrintf("sumOperand = %016llX\n", sumOperand);
        mexPrintf("sumOperand >> 27 = %016llX\n", sumOperand >> 27);
        mexPrintf("sumOperand >> 26 = %016llX\n", sumOperand >> 26);
        mexPrintf("sumOperand(25:0) = %016llX\n", sumOperand & 0x3FFFFFF);
        mexPrintf("\n");
        #endif

        // Determine rounding bit needed for convergent rounding
        long long roundBit = 0;

        if (((sumOperand >> ROUND_BIT) & 1) == 1)
        {
            if (((sumOperand & ROUND_MASK) != 0) || (((sumOperand >> TRUNC_WIDTH) & 1) == 1))
            {
                roundBit = 1; 
            }
        }

        #ifdef DEBUG_PRINTS
        mexPrintf("roundBit = %d\n", roundBit);
        mexPrintf("\n");
        #endif

        // Round result
        sumOperand = sumOperand >> TRUNC_WIDTH;
        sumOperand = sumOperand + roundBit;
        if (((sumOperand >> PAD_WIDTH) & 1) == 1)
        {
            sumOperand = sumOperand >> 1;
            maxExp = maxExp + 1;
        }

        #ifdef DEBUG_PRINTS
        mexPrintf("sumOperand = %08X\n", (int) sumOperand);
        mexPrintf("\n");
        #endif

        // Determine mantissa 1.fraction for normal numbers and 0.fraction for subnormal numbers
        unsigned int sumMantissa = (unsigned int) (sumOperand & MANTISSA_MASK);

        #ifdef DEBUG_PRINTS
        mexPrintf("sumMantissa = %08X\n", sumMantissa);
        mexPrintf("\n");
        #endif

        // Determine exponent must be between 0 and 255
        int sumExpUnbounded = (int) maxExp + (maxBit - 2*(PAD_WIDTH-1));
        if (sumExpUnbounded < 0)
        {
            sumExpUnbounded = 0;
        }
        else if (sumExpUnbounded > MAX_EXP)
        {
            sumExpUnbounded = MAX_EXP;
        }
        unsigned int sumExp = (unsigned int) sumExpUnbounded;

        // Handle infinite results
        if (sumExp == MAX_EXP)
        {
            sumMantissa = 0;
        }

        #ifdef DEBUG_PRINTS
        mexPrintf("sumExp = 0x%02X\n", sumExp);
        mexPrintf("\n");
        #endif

        // Pack uint32 data into a float
        unsigned int sumUint32 = sumSign << SIGN_BIT | sumExp << EXP_BIT | sumMantissa;
        float sum;

        memcpy(&sum, &sumUint32, sizeof(float));

        return sum;
    }
    // Result has zero "mantissa"
    else
    {
        return 0.0f;
    }
}

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    plhs[0] = mxCreateNumericArray(mxGetNumberOfDimensions(prhs[0]), 
        mxGetDimensions(prhs[0]), mxSINGLE_CLASS, mxREAL);
    mwSize numElements = mxGetNumberOfElements(prhs[0]);
    float* sum = mxGetData(plhs[0]);
    float* a = mxGetData(prhs[0]);
    float* b = mxGetData(prhs[1]);
    for (mwSize i = 0; i < numElements; ++i)
    {
        sum[i] = floating_point_add(a[i], b[i]);
    }
}