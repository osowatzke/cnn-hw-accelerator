function res = multiply_and_accumulate(a,b)
    
    a = single(a(:));
    b = single(b(:));

    if length(a) ~= length(b)
        error('Inputs a and b must be the same size');
    end

    if mod(length(a), 8) == 0
        padSize = 0;
    else
        padSize = 8 - mod(length(a), 8);
    end

    a = [a; zeros(padSize,1,'single')];
    b = [b; zeros(padSize,1,'single')];

    a = reshape(a,8,[]);
    b = reshape(b,8,[]);

    % Element-wise multiplication
    prod = a .* b;
    
    % Adder tree
    stageInput = prod;
    numStages = log2(size(a,1));
    for i = 1:numStages
        stageOutput = zeros(size(stageInput),'single');
        stageOutput(1:(size(stageOutput,1)/2),:) = ...
            stageInput(1:2:end,:) + stageInput(2:2:end,:);
        stageInput = stageOutput;
    end
    adderTreeOutput = stageOutput(1,:);

    % Accumulator
    addLatency = 14;
    if mod(length(adderTreeOutput), addLatency) == 0
        padSize = 0;
    else
        padSize = addLatency - mod(length(adderTreeOutput), addLatency);
    end
    adderTreeOutput = [adderTreeOutput, zeros(1,padSize,'single')];
    adderTreeOutput = reshape(adderTreeOutput,addLatency,[]);
    accumOutput = sum(adderTreeOutput,2);

    % Circular shift to get correct ordering of operands
    % First padded sample is the first input to the adder
    accumOutput = circshift(accumOutput, padSize);

    % Adder Tree
    numStages = ceil(log2(size(accumOutput,1)));
    padSize = 2^numStages - length(accumOutput);
    accumOutput = [accumOutput; zeros(padSize,1,'single')];
    stageInput = accumOutput;
    for i = 1:numStages
        stageOutput = zeros(size(stageInput),'single');
        stageOutput(1:(size(stageOutput,1)/2),:) = ...
            stageInput(1:2:end,:) + stageInput(2:2:end,:);
        stageInput = stageOutput;
    end
    res = stageOutput(1);
end