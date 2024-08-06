% Ledalab('/Users/rachelxia/CNS/EDA/testdata/Baseline/Done/', ...
%     'open', 'text', ...
%     'filter', [2 30], ...
%     'downsample', 5, ...
%     'analyze','CDA', ...
%     'optimize', 2) 
% %%
% myDir = '/Users/rachelxia/CNS/EDA/testdata/Baseline/Done/';
% myFiles = dir(fullfile(myDir, "*.mat")); %gets all mat files in struct
% outputDir = '/Users/rachelxia/CNS/EDA/testdata/Baseline/Ledalab output/';
% 
% numFiles = length(myFiles);
% 
% for i = 1:numFiles
%     mat = load(myFiles(i).name, 'analysis', 'data');
% 
%     GSC = mat.data.conductance(:);
%     seconds = mat.data.time(:);
%     tonic = mat.analysis.tonicData(:);
%     phasic = mat.analysis.phasicData(:);
% 
%     downsample_data = table(seconds, GSC, tonic, phasic, ...
%     'VariableNames', {'seconds', 'GSR','tonic','phasic'});
% 
%     [filepath, name, ~] = fileparts(myFiles(i).name);
%     filename = [name, '_downsample.csv'];
%     outputPath = fullfile(outputDir, filename);
%     writetable(downsample_data, outputPath)
% end

% Step 1: Call Ledalab Function
Ledalab('/Users/rachelxia/CNS/EDA/testdata/Baseline/Done/', ...
    'open', 'text', ...
    'filter', [2 30], ...
    'downsample', 5, ...
    'analyze', 'CDA', ...
    'optimize', 2); 

% Step 2: Define Directories
inputDir = '/Users/rachelxia/CNS/EDA/testdata/Baseline/Done/';
outputDir = '/Users/rachelxia/CNS/EDA/testdata/Baseline/Ledalab output/';

% Step 3: Ensure Output Directory Exists
if ~isfolder(outputDir)
    mkdir(outputDir);
end

% Step 4: Get All .mat Files in the Input Directory
myFiles = dir(fullfile(inputDir, "*.mat"));

% Check if files are found
if isempty(myFiles)
    error('No .mat files found in directory: %s', inputDir);
end

% Step 5: Process Each File
for i = 1:length(myFiles)
    try
        % Debug output: current file being processed
        fprintf('Processing file %d of %d: %s\n', i, length(myFiles), myFiles(i).name);
        
        % Load the .mat file
        filePath = fullfile(myFiles(i).folder, myFiles(i).name);
        mat = load(filePath, 'analysis', 'data');
        
        % Check if required fields are present
        if isfield(mat, 'data') && isfield(mat.data, 'conductance') && isfield(mat.data, 'time') && ...
           isfield(mat, 'analysis') && isfield(mat.analysis, 'tonicData') && isfield(mat.analysis, 'phasicData')
           
            % Extract data
            GSC = mat.data.conductance(:);
            seconds = mat.data.time(:);
            tonic = mat.analysis.tonicData(:);
            phasic = mat.analysis.phasicData(:);

            % Create a table
            downsample_data = table(seconds, GSC, tonic, phasic, ...
                'VariableNames', {'seconds', 'GSR', 'tonic', 'phasic'});

            % Create output filename
            [~, name, ~] = fileparts(myFiles(i).name);
            outputFileName = [name, '_downsample.csv'];
            outputPath = fullfile(outputDir, outputFileName);

            % Write table to CSV
            writetable(downsample_data, outputPath);
        else
            warning('File %s does not contain the expected fields.', filePath);
        end
    catch ME
        % Catch and display any errors
        fprintf('Error processing file %s: %s\n', myFiles(i).name, ME.message);
    end
end

fprintf('Processing completed.\n');
