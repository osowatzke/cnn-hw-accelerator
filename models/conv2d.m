function Y = conv2d(X,H)

    % Cast input matrix to single precision
    X = single(X);
    H = single(H);

    % Get the size of the output matrix
    nRows = size(X,1) - size(H,1) + 1;
    nCols = size(X,2) - size(H,2) + 1;

    % Initialize the output matrix
    Y = zeros(nRows, nCols, 'single');

    % Compute each element of the output matrix
    for i = 1:numel(Y)
        % Determine Column and row index
        rowIdx = mod(i-1, nRows) + 1;
        colIdx = floor((i-1)/nRows) + 1;

        % Transpose because C indexing is reversed
        xVec = X(rowIdx:(rowIdx+size(H,1)-1), colIdx:(colIdx+size(H,2)-1)).';
        hVec = H.';

        % Add zeros to fill vector units
        if mod(size(xVec,1),8) ~= 0
            padSize = 8 - mod(size(xVec,1),8);
            xVec = [xVec; zeros(padSize, size(xVec,2), 'single')];
            hVec = [hVec; zeros(padSize, size(hVec,2), 'single')];
        end

        % Perform multiply and accumulate operation
        Y(i) = multiply_and_accumulate(xVec(:),hVec(:));
    end
end