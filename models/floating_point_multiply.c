#include "mex.h"
// #define DEBUG_PRINT

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

// Floating point Bias
#define BIAS ((1 << (EXP_BITS - 1)) - 1)

// Mantissa product width
#define PROD_WIDTH (2 * (MANTISSA_BITS + 1))

float floating_point_multiply(float a, float b)
{
    // View the floating point numbers as uint32's
    unsigned int aUint32, bUint32;
    memcpy(&aUint32, &a, sizeof(float));
    memcpy(&bUint32, &b, sizeof(float));

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

    #ifdef DEBUG_PRINT
        printf("aSign = %d\n", aSign);
        printf("bSign = %d\n\n", aSign);

        printf("aExp = %d\n", aExp);
        printf("bExp = %d\n\n", bExp);

        printf("aMantissa = 0x%08X\n", aMantissa);
        printf("bMantissa = 0x%08X\n\n", bMantissa);
    #endif
    
    // Determine if either of the inputs are infinity or NaN
    unsigned int aNaN = 0;
    unsigned int bNaN = 0;
    unsigned int aInf = 0;
    unsigned int bInf = 0;
    if (aExp == MAX_EXP)
    {
        if (aMantissa == 0)
        {
            aInf = 1;
        }
        else
        {
            aNaN = 1;
        }
    }
    if (bExp == MAX_EXP)
    {
        if (bMantissa == 0)
        {
            bInf = 1;
        }
        else
        {
            bNaN = 1;
        }
    }

    // Product is NaN if either input is NaN
    unsigned int prodNaN = aNaN | bNaN;

    // Determine if either input is zero
    unsigned int aZero = 0;
    unsigned int bZero = 0;

    if (aExp == 0 && aMantissa == 0)
    {
        aZero = 1;
    }
    if (bExp == 0 && bMantissa == 0)
    {
        bZero = 1;
    }

    // Product is NaN if one operand is Inf and the other is zero
    prodNaN = prodNaN | (aInf & bZero) | (bInf & aZero);

    // Determine if the product is Inf. NaN takes precedence over Inf
    // in final if statement so this can be 1 even if prodNaN = 1
    unsigned int prodInf = aInf | bInf;

    // Add implicit leading 1's for normal floating point numbers
    // Shift by 1 for subnormal floating point numbers

    // For normal floating point number:
    // 2^(exp - 127) * (1.fraction)

    // For subnormal floating point number
    // 2^(-126) * (0.fraction)
    if (aExp != 0)
    {
        aMantissa = aMantissa | (1 << MANTISSA_BITS);
    }
    else
    {
        aMantissa = aMantissa << 1;
    }
    
    if (bExp != 0)
    {
        bMantissa = bMantissa | (1 << MANTISSA_BITS);
    }
    else
    {
        bMantissa = bMantissa << 1;
    }

    #ifdef DEBUG_PRINT
        printf("aMantissa = 0x%08X\n", aMantissa);
        printf("bMantissa = 0x%08X\n\n", bMantissa);
    #endif

    // Determine signal of result
    unsigned int prodSign = aSign ^ bSign;

    // Multiply mantissas
    unsigned long long prod = ((unsigned long long) aMantissa) * 
        ((unsigned long long) bMantissa);

    // Determine shift required to place make MSB '1'
    int prodShift = 0;
    unsigned int i;
    for (i = 0; i < PROD_WIDTH; ++i)
    {
        if ((prod >> i) & 1)
        {
            prodShift = (PROD_WIDTH - 1) - i;
        }
    }

    #ifdef DEBUG_PRINT
        printf("prod = 0x%016llX\n\n", prod);
        printf("prodShift = %d\n\n", prodShift);
    #endif

    // Compute exponent of result
    // Adding 1 to account for shift by 1 in output data
    int prodExp = (int) aExp + (int) bExp - BIAS + 1;

    // Force exponent to zero for zero products
    if (aZero || bZero)
    {
        prodExp = 0;
    }

    #ifdef DEBUG_PRINT
        printf("prodExp = %d\n\n", prodExp);
    #endif

    // Place '1' in MSB
    prod = prod << prodShift;

    // Determine resulting exponent
    prodExp = prodExp - prodShift;

    #ifdef DEBUG_PRINT
        printf("prodExp = %d\n\n", prodExp);
    #endif

    // Determine shift required to make exponent positive
    // Need a maximum value of 25 so no significant bits result from round.
    if (prodExp > 0)
    {
        prodShift = 0;
    }
    else
    {
        if (prodExp <= -24)
        {
            prodShift = 25;
        }
        else
        {
            prodShift = -prodExp + 1;
        }
    }

    // Clamp exponent at zero
    if (prodExp < 0)
    {
        prodExp = 0;
    }

    // Determine bits shifted from mantissa
    unsigned long long truncBits = prod << (64 - PROD_WIDTH - 1); 
    truncBits = truncBits << (MANTISSA_BITS + 2 - prodShift);
    truncBits = truncBits >> (64 - PROD_WIDTH - 1);

    // Determine mantissa
    unsigned int prodMantissa = prod >> (MANTISSA_BITS + 1 + prodShift);

    #ifdef DEBUG_PRINT
        printf("prodMantissa = 0x%018X\n\n", prodMantissa);
    #endif

    // Determine round bit for convergent round
    unsigned int roundBit = 0;
    if ((truncBits >> PROD_WIDTH) & 1)
    {
        unsigned long long mask = ((((unsigned long long) 1) << PROD_WIDTH) - 1);

        #ifdef DEBUG_PRINT
            printf("mask = 0x%016llX\n\n", mask);
        #endif

        if ((prodMantissa & 1) || (truncBits & mask))
        {
            roundBit = 1;
        }
    }

    #ifdef DEBUG_PRINT
        printf("truncBits = 0x%016llX\n\n", truncBits);
        printf("roundBit = %d\n\n", roundBit);
    #endif

    // Round result
    prodMantissa = prodMantissa + roundBit;

    // Determine if exponent is updated by round
    if (prodShift == 0)
    {
        if ((prodMantissa >> (MANTISSA_BITS + 1)) && 1)
        {
            prodExp++;
        }
    }
    else
    {
        if ((prodMantissa >> MANTISSA_BITS) && 1)
        {
            prodExp++;
        }
    }

    // Mask out resulting mantissa
    prodMantissa = prodMantissa & MANTISSA_MASK;

    #ifdef DEBUG_PRINT
        printf("prodExp = %d\n\n", prodExp);
    #endif

    // Determine if the result is infinity
    if (prodExp >= MAX_EXP)
    {
        prodInf = 1;
    }

    // Handle special cases
    if (prodNaN)
    {
        prodExp = MAX_EXP;
        prodMantissa = 1 << (MANTISSA_BITS - 1);
    }
    else if (prodInf)
    {
        prodExp = MAX_EXP;
        prodMantissa = 0;
    }
    
    // Pack result
    unsigned int prodUint32 = (prodSign << SIGN_BIT) | (prodExp << EXP_BIT) | prodMantissa;
    
    // Copy into floating point result
    float prodFloat;
    memcpy(&prodFloat, &prodUint32, sizeof(float));

    return prodFloat;
}

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    plhs[0] = mxCreateNumericArray(mxGetNumberOfDimensions(prhs[0]), 
        mxGetDimensions(prhs[0]), mxSINGLE_CLASS, mxREAL);
    mwSize numElements = mxGetNumberOfElements(prhs[0]);
    float* prod = mxGetData(plhs[0]);
    float* a = mxGetData(prhs[0]);
    float* b = mxGetData(prhs[1]);
    for (mwSize i = 0; i < numElements; ++i)
    {
        prod[i] = floating_point_multiply(a[i], b[i]);
    }
}