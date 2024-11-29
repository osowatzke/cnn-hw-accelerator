N = 32;
rng(0);
X = randn(N, N, 'single');
H = randn(1, N, 'Single');
Y = conv2d(X,H);

fid = fopen('data.txt', 'w');
fprintf(fid, '%08X\n', size(X,2));
fprintf(fid, '%08X\n', size(X,1));
X = X.';  % Transpose because C indexing is reversed
for i = 1:numel(X)
    fprintf(fid, '%08X\n', typecast(X(i), 'uint32'));
end
fclose(fid);

fid = fopen('filt.txt', 'w');
fprintf(fid, '%08X\n', size(H,2));
fprintf(fid, '%08X\n', size(H,1));
H = H.';  % Transpose because C indexing is reversed
for i = 1:numel(H)
    fprintf(fid, '%08X\n', typecast(H(i), 'uint32'));
end
fclose(fid);

fid = fopen('output.txt', 'w');
Y = Y.'; % Transpose because C indexing is reversed
for i = 1:numel(Y)
    fprintf(fid, '%08X\n', typecast(Y(i), 'uint32'));
end
fclose(fid);
