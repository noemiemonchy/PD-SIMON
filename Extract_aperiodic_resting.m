%% Extract aperiodic parameters resting state

%% HC
% path to subject files
spec_path = uigetdir;
addpath(spec_path);
cd(spec_path);
sublist = dir('sub*');
nsub =  length(sublist);

for subi = [1 :nsub]
    cd(sublist(subi).name)
    subpath = dir('sub*');
    cd(subpath(1).name)
    specpath = dir('*specparam*.mat');
    
    temp = load(specpath(1).name);
    if ~contains(temp.Comment, 'scalp')
        temp = load(specpath(2).name);
    end
    specparam_hc(subi) = temp.Options.FOOOF;
    cd ..
    cd ..
end

%% PD
% path to subject files
spec_path = uigetdir;
addpath(spec_path);
cd(spec_path);
sublist = dir('sub*');
nsub =  length(sublist);

for subi = [1:7, 9:nsub]
    cd(sublist(subi).name)
    subpath = dir('sub*');
    cd(subpath(1).name)
    specpath = dir('*specparam*.mat');
    
    temp = load(specpath(1).name);
    if ~contains(temp.Comment, 'scalp')
        temp = load(specpath(2).name);
    end
    specparam_pd(subi) = temp.Options.FOOOF;
    cd ..
    cd ..
end
specparam_pd(8) = [];