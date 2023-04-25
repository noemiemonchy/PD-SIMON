%% Extract aperiodic parameters simon task - post stim

%% SPECPARAM STRUCTURES
% Post stimulus
%% HC
% path to subject files

spec_path = uigetdir;
addpath(spec_path);
cd(spec_path);

spec_files = dir('*specparam.mat'); %chemin pour spec post

specparam_post_HC = struct;

if contains(spec_path, 'PD')
    group = 'PD';
else
    group = 'HC';
end

ic_count = 1;
c_count = 1;

for i = 1:length(spec_files)
    
    
    load(spec_files(i).name);
    
    if or(contains(Comment, 'INCONG'), contains(Comment, 'incong'))  
        specparam_post_HC(ic_count).ic = Options.FOOOF;
        specparam_post_HC(ic_count).ic.sub = Comment(8:10);
        specparam_post_HC(ic_count).ic.group = group;
        ic_count = ic_count+1;
    else
        specparam_post_HC(c_count).c = Options.FOOOF;
        specparam_post_HC(c_count).c.sub = Comment(8:10);
        specparam_post_HC(c_count).c.group = group;
        c_count = c_count+1;
   end
    
    
end

spec_post_HC = load("SPEC_POST_HC_2304.mat");

%% PD
% path to subject files

spec_path = uigetdir;
addpath(spec_path);
cd(spec_path);

spec_files = dir('*specparam.mat'); %chemin pour spec post

specparam_post_PD = struct;

if contains(spec_path, 'PD')
    group = 'PD';
else
    group = 'HC';
end

ic_count = 1;
c_count = 1;

for i = 1:length(spec_files)
    
    
    load(spec_files(i).name);
    
    if or(contains(Comment, 'INCONG'), contains(Comment, 'incong'))
        specparam_post_PD(ic_count).ic = Options.FOOOF;
        specparam_post_PD(ic_count).ic.sub = Comment(1:5);
        specparam_post_PD(ic_count).ic.group = group;
        ic_count = ic_count+1;
    else
        specparam_post_PD(c_count).c = Options.FOOOF;
        specparam_post_PD(c_count).c.sub = Comment(1:5);
        specparam_post_PD(c_count).c.group = group;
        c_count = c_count+1;
   end
    
    
end

%% SPECPARAM STRUCTURES
% Pre stimulus or resting
%% PD
% path to subject files

spec_path = uigetdir;
addpath(spec_path);
cd(spec_path);

spec_files = dir('*specparam.mat'); %chemin pour spec post

specparam_pre_PD = struct;

if contains(spec_path, 'PD')
    group = 'PD';
else
    group = 'HC';
end


for i = 1:length(spec_files)
    
    
    load(spec_files(i).name);
    
        specparam_pre_PD(i).spec = Options.FOOOF;
        specparam_pre_PD(i).sub = Comment(1:5);
        specparam_pre_PD(i).group = group;    
end


%% HC
% path to subject files

spec_path = uigetdir;
addpath(spec_path);
cd(spec_path);

spec_files = dir('*specparam.mat'); %chemin pour spec post

specparam_pre_HC = struct;

if contains(spec_path, 'PD')
    group = 'PD';
else
    group = 'HC';
end


for i = 1:length(spec_files)
    
    
    load(spec_files(i).name);
    
        specparam_pre_HC(i).spec = Options.FOOOF;
        specparam_pre_HC(i).sub = Comment(1:5);
        specparam_pre_HC(i).group = group;
    
end

%% EXTRACT APERIODIC PARAMETERS

specpo_hc = load('SPEC_POST_HC_2304.mat') 
specpo_pd = load('SPEC_POST_PD_2304.mat')


%% HC

% OFFSET

offposthc = zeros(size(length(specpo_hc.specparam_post_HC),2),2,199);
for subi = 1:length(specpo_hc.specparam_post_HC)
    for eleci = 1:199
        offposthc(subi,1,eleci) = specpo_hc.specparam_post_HC(subi).c.data(eleci).aperiodic_params(1);
        
        offposthc(subi,2,eleci) = specpo_hc.specparam_post_HC(subi).ic.data(eleci).aperiodic_params(1);
    end
end

% EXPONENT

expoposthc = zeros(size(length(specpo_hc.specparam_post_HC),2),2,199);
for subi = 1:length(specpo_hc.specparam_post_HC)
    for eleci = 1:199
        expoposthc(subi,1,eleci) = specpo_hc.specparam_post_HC(subi).c.data(eleci).aperiodic_params(2);
        
        expoposthc(subi,2,eleci) = specpo_hc.specparam_post_HC(subi).ic.data(eleci).aperiodic_params(2);
    end
end


%% PD

% OFFSET

offpostpd = zeros(size(length(specpo_pd.specparam_post_PD),2),2,199);
for subi = 1:length(specpo_pd.specparam_post_PD)
    for eleci = 1:199
        offpostpd(subi,1,eleci) = specpo_pd.specparam_post_PD(subi).c.data(eleci).aperiodic_params(1);
        
        offpostpd(subi,2,eleci) = specpo_pd.specparam_post_PD(subi).ic.data(eleci).aperiodic_params(1);
    end
end

% EXPONENT

expopostpd = zeros(size(length(specpo_pd.specparam_post_PD),2),2,199);
for subi = 1:length(specpo_pd.specparam_post_PD)
    for eleci = 1:199
        expopostpd(subi,1,eleci) = specpo_pd.specparam_post_PD(subi).c.data(eleci).aperiodic_params(2);
        
        expopostpd(subi,2,eleci) = specpo_pd.specparam_post_PD(subi).ic.data(eleci).aperiodic_params(2);
    end
end


%% GRAPHIC REPRESENTATIONS
%% 
% Load channels location : 199chanlocs.mat
cd(uigetdir())
load('199chanlocs.mat'); % add your path to the channel file

% offset
%  get limits from both datasets

offposthc_m = mean(offposthc,2);
offposthc_mm = squeeze(mean(offposthc_m,1));

offpostpd_m = mean(offpostpd,2);
offpostpd_mm = squeeze(mean(offpostpd_m,1));

forlims = cat(1, offposthc_mm, offpostpd_mm);
clim = [-12.314, -11.002];

offset_hc_fig = figure
topoplot(squeeze(offposthc_mm),chanlocs, 'electrodes', 'off', ...
    'gridscale', 500)
set(gca, 'clim', clim);
colormap(parula)
title(['Offset HC'])
set(gcf,'Position',[100 100 300 300])
colorbar

offset_pd_fig = figure
topoplot(squeeze(offpostpd_mm),chanlocs, 'electrodes', 'off', ...
    'gridscale', 500)
set(gca, 'clim', clim);
colormap(parula)
title(['Offset PD'])
set(gcf,'Position',[100 100 300 300])
colorbar


% exponent
%  get limits from both datasets

expoposthc_m = mean(expoposthc,2);
expoposthc_mm = squeeze(mean(expoposthc_m,1));

expopostpd_m = mean(expopostpd,2);
expopostpd_mm = squeeze(mean(expopostpd_m,1));

forlims = cat(1, expoposthc_mm, expopostpd_mm);
clim = [0.422, 1.2197];

exponent_hc_fig = figure
topoplot(squeeze(expoposthc_mm),chanlocs, 'electrodes', 'off', ...
    'gridscale', 500)
set(gca, 'clim', clim);
title(['Exponent HC'])
colormap(parula)
set(gcf,'Position',[100 100 300 300])
colorbar

exponent_pd_fig = figure
topoplot(squeeze(expopostpd_mm),chanlocs, 'electrodes', 'off', ...
    'gridscale', 500)
set(gca, 'clim', clim);
title(['Exponent PD'])
colormap(parula)
set(gcf,'Position',[100 100 300 300])
colorbar

%% TABLEAUX VIOLIN PLOTS
offposthc_v = mean(offposthc,3);
offpostpd_v = mean(offpostpd,3);

csvwrite("offsetpost_hc.csv",offposthc_v);
csvwrite("offsetpost_pd.csv",offpostpd_v);

expoposthc_v = mean(expoposthc,3);
expopostpd_v = mean(expopostpd,3);

csvwrite("expopost_hc.csv",expoposthc_v);
csvwrite("expopost_pd.csv",expopostpd_v);


%% EXTRACTION DONNÉES PÉRIODIQUES  

% en regardant au niveau du set d'electrodes midfrontales 

elec_names = {'Fz', 'FCz', 'Cz', 'F1h', 'F2h', 'FC1', 'FC2', 'FCC1h', 'FCC2h','F1', 'F2', 'FFFC1', 'FFC2','FC3h', 'FC4h', 'FCC1', 'FCC2', 'C1h', 'C2h'};
elec_idx = [15, 8, 199, 16, 7, 17, 177, 9, 167, 23, 6, 24, 184, 43, 176, 44, 166, 45, 122];

perio_post_hc_midfr = zeros(size(elec_idx,2),2,30);
for subi = 1:length(specpo_hc.specparam_post_HC)
    for eleci=1: size(elec_idx,2)
        perio_post_hc_midfr(eleci,1,subi) = specpo_hc.specparam_post_HC(subi).c.data(elec_idx(eleci)).peak_params(1,1);
        
        perio_post_hc_midfr(eleci,2,subi) = specpo_hc.specparam_post_HC(subi).ic.data(elec_idx(eleci)).peak_params(1,1);
        
    end
end

csvwrite('peaksmidfr_post_hc.csv', perio_post_hc_midfr);


perio_post_pd_midfr = zeros(size(elec_idx,2),2,29);
for subi = 1:length(specpo_pd.specparam_post_PD)
    for eleci=1: size(elec_idx,2)
        perio_post_pd_midfr(eleci,1,subi) = specpo_pd.specparam_post_PD(subi).c.data(elec_idx(eleci)).peak_params(1,1);
        
        perio_post_pd_midfr(eleci,2,subi) = specpo_pd.specparam_post_PD(subi).ic.data(elec_idx(eleci)).peak_params(1,1);
        
    end
end

csvwrite('peaksmidfr_post_pd.csv', perio_post_pd_midfr);

