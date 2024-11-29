% Number of random samples to generate
N = 1000;

% Random number generator seed
rng(0)

% Generate random integers
a = randi([0,15], N, 1, 'single');
b = randi([0,15], N, 1, 'single');

% Perform reference operations
y = multiply_and_accumulate(a,b);

% Save input data to a file
fid = fopen("input_a.txt", "w");
for i = 1:length(a)
    fprintf(fid, "%08X\n", typecast(a(i),'uint32'));
end
fclose(fid);

fid = fopen("input_b.txt", "w");
for i = 1:length(b)
    fprintf(fid, "%08X\n", typecast(b(i),'uint32'));
end
fclose(fid);

% Save output data to a file
fid = fopen("output.txt", "w");
for i = 1:length(y)
    fprintf(fid, "%08X\n", typecast(y(i),'uint32'));
end
fclose(fid);