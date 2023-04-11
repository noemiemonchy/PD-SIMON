%% Extract aperiodic parameters simon task - post stim

%% SPECPARAM STRUCTURES
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
    
    if contains(Comment, 'INCONG')
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
    
    if contains(Comment, 'INCONG')
        specparam_post_PD(ic_count).ic = Options.FOOOF;
        specparam_post_PD(ic_count).ic.sub = Comment(8:10);
        specparam_post_PD(ic_count).ic.group = group;
        ic_count = ic_count+1;
    else
        specparam_post_PD(c_count).c = Options.FOOOF;
        specparam_post_PD(c_count).c.sub = Comment(8:10);
        specparam_post_PD(c_count).c.group = group;
        c_count = c_count+1;
   end
    
    
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

offpostpd = zeros(size(length(specpo_pd.specparam_post_PD),2),2,199)
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
clim = [min(forlims), max(forlims)];

offset_hc_fig = figure
topoplot(squeeze(offposthc_mm),chanlocs, 'electrodes', 'off', ...
    'gridscale', 500)
set(gca, 'clim', clim);
colormap(parula)
title(['Offset HC'])
set(gcf,'Position',[100 100 300 300])
colorbar

offset_pd_fig = figure
topoplot(mean(offpostpd_mm, 2),chanlocs, 'electrodes', 'off', ...
    'gridscale', 500)
set(gca, 'clim', clim);
colormap(parula)
title(['Offset PD'])
set(gcf,'Position',[100 100 300 300])
colorbar


% exponent
%  get limits from both datasets
forlims = cat(1, mean(expoposthc, 2), mean(expopostpd, 2));
clim = [min(forlims), max(forlims)];

exponent_hc_fig = figure
topoplot(mean(expoposthc, 2),chanlocs, 'electrodes', 'off', ...
    'gridscale', 500)
set(gca, 'clim', clim);
title(['Exponent HC'])
colormap(parula)
set(gcf,'Position',[100 100 300 300])
colorbar

exponent_pd_fig = figure
topoplot(mean(expopostpd, 2),chanlocs, 'electrodes', 'off', ...
    'gridscale', 500)
set(gca, 'clim', clim);
title(['Exponent PD'])
colormap(parula)
set(gcf,'Position',[100 100 300 300])
colorbar


