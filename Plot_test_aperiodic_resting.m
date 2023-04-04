%% Plot aperiodic parameters on scalp and test 
%  group difference significance

% Make sure eeglab is in the path !
addpath(genpath(uigetdir()));


% go to aperiodic files folder and load specparam files
cd(uigetdir())
load('specparam_hc')
load('specparam_pd')

% create result matrices
offset_hc = zeros(199, length(specparam_hc)); % channels*sub
exponent_hc = zeros(199, length(specparam_hc)); % channels*sub
offset_pd = zeros(199, length(specparam_pd)); % channels*sub
exponent_pd = zeros(199, length(specparam_pd)); % channels*sub

% fill with data
for subi = 1:size(offset_hc, 2)
    offset_hc(:, subi) = [specparam_hc(subi).aperiodics.offset];
    exponent_hc(:, subi) = [specparam_hc(subi).aperiodics.exponent];
end

for subi = 1:size(offset_pd, 2)
    offset_pd(:, subi) = [specparam_pd(subi).aperiodics.offset];
    exponent_pd(:, subi) = [specparam_pd(subi).aperiodics.exponent];
end

% Load channels location : 199chanlocs.mat
cd(uigetdir())
load('199chanlocs.mat'); % add your path to the channel file

% offset
%  get limits from both datasets
forlims = cat(1, mean(offset_hc, 2), mean(offset_pd, 2));
clim = [min(forlims), max(forlims)];

offset_hc_fig = figure
topoplot(mean(offset_hc, 2),chanlocs, 'electrodes', 'off', ...
    'gridscale', 500)
set(gca, 'clim', clim);
colormap(parula)
title(['Offset HC'])
set(gcf,'Position',[100 100 300 300])
colorbar

offset_pd_fig = figure
topoplot(mean(offset_pd, 2),chanlocs, 'electrodes', 'off', ...
    'gridscale', 500)
set(gca, 'clim', clim);
colormap(parula)
title(['Offset PD'])
set(gcf,'Position',[100 100 300 300])
colorbar

% exponent
%  get limits from both datasets
forlims = cat(1, mean(exponent_hc, 2), mean(exponent_pd, 2));
clim = [min(forlims), max(forlims)];

exponent_hc_fig = figure
topoplot(mean(exponent_hc, 2),chanlocs, 'electrodes', 'off', ...
    'gridscale', 500)
set(gca, 'clim', clim);
title(['Exponent HC'])
colormap(parula)
set(gcf,'Position',[100 100 300 300])
colorbar

exponent_pd_fig = figure
topoplot(mean(exponent_pd, 2),chanlocs, 'electrodes', 'off', ...
    'gridscale', 500)
set(gca, 'clim', clim);
title(['Exponent PD'])
colormap(parula)
set(gcf,'Position',[100 100 300 300])
colorbar



%% For significance

%% Stats on region-specific group changes in exponent and offset

% Make sure FDR and CohenD function is in the path
addpath(genpath(uigetdir()))

results = zeros(199,2); % channel*aperio (1=exponent, 2 = offset)
% two-sided t tests
for roi = 1:199
    [h,p,ci,stats] = ttest2(exponent_hc(roi, :), exponent_pd(roi, :));
    results(roi, 1) = p;

    [h,p,ci,stats] = ttest2(offset_hc(roi, :), offset_pd(roi, :));
    results(roi, 2) = p;
end

[h, crit_p, adj_ci_cvrg, adj_p] = fdr_bh(results(:,1))
expo_fdr = adj_p<0.05;

[h, crit_p, adj_ci_cvrg, adj_p] = fdr_bh(results(:,2))
offset_fdr = adj_p<0.05;


% On the channel-averaged data

 [h,p,ci,stats] = ttest2(mean(exponent_hc, 1), mean(exponent_pd, 1));
 [h,p,ci,stats] = ttest2(mean(offset_hc, 1), mean(offset_pd, 1));
 
 % Effect size
 computeCohen_d(mean(offset_hc, 1), mean(offset_pd, 1))

% Plot significant channels on scalp
% create fake data containing only the number of channels that are
% significant

fake_data = zeros(1, sum(offset_fdr));

% Offset
pvaldata1offset = figure
topoplot(fake_data,chanlocs(offset_fdr), 'electrodes', 'on', ...
    'emarkersize', 25, 'electcolor', [0.8500 0.3250 0.0980])
colormap(redbluecmap)
set(gcf,'Position',[100 100 300 300])
print(pvaldata1offset, "pdata1off", '-dpng', '-r1000') % pour la résolution  de l'image

% Exponent
fake_data = zeros(1, sum(expo_fdr));
pvaldata1expo = figure
topoplot(fake_data,chanlocs(expo_fdr), 'electrodes', 'on', ...
    'emarkersize', 25, 'electcolor', [0.8500 0.3250 0.0980])
colormap(redbluecmap)
set(gcf,'Position',[100 100 300 300])
print(pvaldata1expo, "pdata1off", '-dpng', '-r1000') % pour la résolution  de l'image



