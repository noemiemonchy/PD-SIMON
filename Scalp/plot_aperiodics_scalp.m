%% Plot aperiodic data on cortex


%% Create a group-averaged exponent and offset

% Load specparam results for HC and PD in Pre/Post/Rest conditions

path = uigetdir;
cd(path);
addpath(genpath(path))
% Rest
load('specrest_HC_sorted.mat')
load('specrest_PD_sorted.mat')
% Pre
load('specpre_HC_sorted.mat')
load('specpre_PD_sorted.mat')
% Post
load('specpost_HC_sorted.mat')
load('specpost_PD_sorted.mat')

nelec = 199;
% Create a matrix for PD and HC with all the results
% nsub * elec * condition (1 rest / 2 pre/3 post) * aperio (1 expo/2 offset) 
HC = zeros(length(specparam_post_HC_sorted), nelec, 3, 2);
% Same but for the task, differenciating c and ic, % nsub * elec * condition (1 c / 2 ic) * aperio (1 expo/2 offset) 
HCtask = zeros(length(specparam_post_HC_sorted), nelec, 2, 2);

% Same for PD
PD = zeros(length(specparam_post_PD_sorted), nelec, 3, 2);
PDtask = zeros(length(specparam_post_PD_sorted), nelec, 2, 2);

% For HC
for subi = 1:size(HC, 1) 
    HC(subi, : , 1, 1) = [specparam_scalp_hc_sorted(subi).spec.aperiodics.exponent]  ; % rest, expo
    HC(subi, : , 1, 2) = [specparam_scalp_hc_sorted(subi).spec.aperiodics.offset]  ; % rest, offset
    
    HC(subi, : , 2, 1) = [specparam_pre_HC_sorted(subi).FOOF.aperiodics.exponent]  ; % pre, expo
    HC(subi, : , 2, 2) = [specparam_pre_HC_sorted(subi).FOOF.aperiodics.offset]  ; % pre, offset
    
    temp_exp(:,1) = [specparam_post_HC_sorted(subi).c.aperiodics.exponent];
    temp_exp(:,2) = [specparam_post_HC_sorted(subi).ic.aperiodics.exponent];
    
    HC(subi, : , 3, 1) = mean(temp_exp,2)  ; % post, expo
    
    
    temp_off(:,1) = [specparam_post_HC_sorted(subi).c.aperiodics.offset];
    temp_off(:,2) = [specparam_post_HC_sorted(subi).ic.aperiodics.offset];
    HC(subi, : , 3, 2) = mean(temp_off,2) ; % post, offset
    
    HCtask(subi,:,1,1) = [specparam_post_HC_sorted(subi).c.aperiodics.exponent];
    HCtask(subi,:,2,1) = [specparam_post_HC_sorted(subi).ic.aperiodics.exponent];
    HCtask(subi,:,1,2) = [specparam_post_HC_sorted(subi).c.aperiodics.offset];
    HCtask(subi,:,2,2) = [specparam_post_HC_sorted(subi).ic.aperiodics.offset];
end


% For PD
for subi = 1:size(PD, 1) 
    PD(subi, : , 1, 1) = [specparam_scalp_pd(subi).spec.aperiodics.exponent]  ; % rest, expo
    PD(subi, : , 1, 2) = [specparam_scalp_pd(subi).spec.aperiodics.offset]  ; % rest, offset
    
    PD(subi, : , 2, 1) = [specparam_pre_PD_sorted(subi).FOOF.aperiodics.exponent]  ; % pre, expo
    PD(subi, : , 2, 2) = [specparam_pre_PD_sorted(subi).FOOF.aperiodics.offset]  ; % pre, offset
    
    temp_exp(:,1) = [specparam_post_PD_sorted(subi).c.aperiodics.exponent];
    temp_exp(:,2) = [specparam_post_PD_sorted(subi).ic.aperiodics.exponent];
    
    PD(subi, : , 3, 1) = mean(temp_exp,2)  ; % post, expo
    
    
    temp_off(:,1) = [specparam_post_PD_sorted(subi).c.aperiodics.offset];
    temp_off(:,2) = [specparam_post_PD_sorted(subi).ic.aperiodics.offset];
    PD(subi, : , 3, 2) = mean(temp_off,2) ; % post, offset
    
    PDtask(subi,:,1,1) = [specparam_post_PD_sorted(subi).c.aperiodics.exponent];
    PDtask(subi,:,2,1) = [specparam_post_PD_sorted(subi).ic.aperiodics.exponent];
    PDtask(subi,:,1,2) = [specparam_post_PD_sorted(subi).c.aperiodics.offset];
    PDtask(subi,:,2,2) = [specparam_post_PD_sorted(subi).ic.aperiodics.offset];
end

temp_exp_HC = squeeze(HC(:,:,:,1));
temp_exp_PD = squeeze(PD(:,:,:,1));

temp_off_HC = squeeze(HC(:,:,:,2));
temp_off_PD = squeeze(PD(:,:,:,2));

clim_expo = [min(min(temp_exp_HC(:)), min(temp_exp_PD(:))), max(max(temp_exp_HC(:)), max(temp_exp_PD(:)))];
clim_off = [min(min(temp_off_HC(:)), min(temp_off_PD(:))), max(max(temp_off_HC(:)), max(temp_off_PD(:)))];

rest_HC_exp = squeeze(mean(HC(:,:,1,1), 1));
rest_PD_exp =  squeeze(mean(PD(:,:,1,1), 1));
rest_HC_off = squeeze(mean(HC(:,:,1,2), 1));
rest_PD_off = squeeze(mean(PD(:,:,1,2), 1));
pre_HC_exp = squeeze(mean(HC(:,:,2,1), 1));
pre_PD_exp = squeeze(mean(PD(:,:,2,1), 1));
pre_HC_off = squeeze(mean(HC(:,:,2,2), 1));
pre_PD_off = squeeze(mean(PD(:,:,2,2), 1));
post_HC_exp = squeeze(mean(HC(:,:,3,1), 1));
post_PD_exp = squeeze(mean(PD(:,:,3,1), 1));
post_HC_off = squeeze(mean(HC(:,:,3,2), 1));
post_PD_off = squeeze(mean(PD(:,:,3,2), 1));
post_HC_exp_c =  squeeze(mean(HCtask(:,:,1,1), 1));
post_HC_exp_ic = squeeze(mean(HCtask(:,:,2,1), 1));
post_PD_exp_c = squeeze(mean(PDtask(:,:,1,1), 1));
post_PD_exp_ic = squeeze(mean(PDtask(:,:,2,1), 1));
post_HC_off_c = squeeze(mean(HCtask(:,:,1,2), 1));
post_HC_off_ic = squeeze(mean(HCtask(:,:,2,2), 1));
post_PD_off_c = squeeze(mean(PDtask(:,:,1,2), 1));
post_PD_off_ic = squeeze(mean(PDtask(:,:,2,2), 1));


%% Create table to get all source-averaged parameters in a single csv file

% with headers:
% Sub, Gp, mean exponent, mean offset

group = {'HC', 'PD'};

i = 1; % initiate counter to fill rows

varnames = {'sub', 'group','period','averaged_exponent', 'averaged_offset'};
vartypes = {'double', 'string', 'string',...
    'double','double'};

t = table('Size', [10000, 5], 'VariableTypes', vartypes, 'VariableNames', varnames);
condition = {'Rest', 'Pre-stimulus', 'Post-stimulus'};
% HC
for subi = 1 : size(HC, 1)
    for condi = 1:3
            sub = subi ;
            gp = 'HC';
            period = condition{condi};
            averaged_exponent = squeeze(mean(HC(subi,:, condi,1), 2));
            averaged_offset = squeeze(mean(HC(subi,:, condi,2), 2));
            t(i,:) = {sub, gp, period, averaged_exponent, averaged_offset};
            i = i+1;
    end
end

% PD
for subi = 1 : size(PD, 1)
    for condi = 1:3
            sub = subi ;
            gp = 'PD';
            period = condition{condi};
            averaged_exponent = squeeze(mean(PD(subi,:, condi,1), 2));
            averaged_offset = squeeze(mean(PD(subi,:, condi,2), 2));
            t(i,:) = {sub, gp, period, averaged_exponent, averaged_offset};
            i = i+1;
    end
end

graph_t = rmmissing(t);
writetable(graph_t, ['scalp_aperiodic_param.csv']);



%% Create table to get all elec, group and subject-specific parameters 
%  in a single csv file, only for the 3 periods

% with headers:
% Sub, Gp, period, elec, exponent, offset

group = {'HC', 'PD'};

i = 1; % initiate counter to fill rows

varnames = {'sub', 'group','period','elec','exponent', 'offset'};
vartypes = {'double', 'string', 'string','double'...
    'double','double'};

t = table('Size', [10000, 6], 'VariableTypes', vartypes, 'VariableNames', varnames);
condition = {'Rest', 'Pre-stimulus', 'Post-stimulus'};
% HC
for subi = 1 : size(HC, 1)
    for condi = 1:3
        for elec = 1:199
            sub = subi ;
            gp = 'HC';
            period = condition{condi};
            exponent = HC(subi,elec, condi,1);
            offset = HC(subi,elec, condi,2);
            t(i,:) = {sub, gp, period, elec, exponent, offset};
            i = i+1;
        end
    end
end

% PD
for subi = 1 : size(PD, 1)
    for condi = 1:3
        for elec = 1:199
            sub = subi ;
            gp = 'PD';
            period = condition{condi};
            exponent = PD(subi,elec, condi,1);
            offset = PD(subi,elec, condi,2);
            t(i,:) = {sub, gp, period, elec, exponent, offset};
            i = i+1;
        end
    end
end
graph_t = rmmissing(t);
writetable(graph_t, ['scalp_aperiodic_param_all.csv']);


%% Create table to get all elec, group and subject-specific parameters 
%  in a single csv file, only for post stimulus and according to congruence

% with headers:
% Sub, Gp, congruence, elec, exponent, offset

group = {'HC', 'PD'};

i = 1; % initiate counter to fill rows

varnames = {'sub', 'group','congruence','elec','exponent', 'offset'};
vartypes = {'double', 'string', 'string','double'...
    'double','double'};

t = table('Size', [10000, 6], 'VariableTypes', vartypes, 'VariableNames', varnames);
condition = {'C', 'IC'};
% HC
for subi = 1 : size(HC, 1)
    for condi = 1:2
        for elec = 1:199
            sub = subi ;
            gp = 'HC';
            congruence = condition{condi};
            exponent = HCtask(subi,elec, condi,1);
            offset = HCtask(subi,elec, condi,2);
            t(i,:) = {sub, gp, congruence, elec, exponent, offset};
            i = i+1;
        end
    end
end

% PD
for subi = 1 : size(PD, 1)
    for condi = 1:2
        for elec = 1:199
            sub = subi ;
            gp = 'PD';
            congruence = condition{condi};
            exponent = PDtask(subi,elec, condi,1);
            offset = PDtask(subi,elec, condi,2);
            t(i,:) = {sub, gp, congruence, elec, exponent, offset};
            i = i+1;
        end
    end
end
graph_t = rmmissing(t);
writetable(graph_t, ['scalp_aperiodic_param_post_task.csv']);

%% Stats and topoplot on region-specific group changes in exponent and offset
results = zeros(199,2); % first col is exponent, second is offset

exp4test_hc = squeeze(HC(:, : , 3, 1));
exp4test_pd = squeeze(PD(:, : , 3, 1));

off4test_hc = squeeze(HC(:, : , 3, 2));
off4test_pd = squeeze(PD(:, : , 3, 2));

% two-sided t tests
for elec = 1:199
    [h,p,ci,stats] = ttest2(exp4test_hc(:,elec), exp4test_pd(:,elec));
    results(elec, 1) = p;

    [h,p,ci,stats] = ttest2(off4test_hc(:,elec), off4test_pd(:,elec));
    results(elec, 2) = p;
end
FDR = zeros(199, 2);
FDR(:,1) = mafdr(results(:,1));
FDR(:,2) = mafdr(results(:,2));
FDR_bin = FDR<0.05;
[FDR,Q,aPrioriProb] =  mafdr(results(:,1));

[h, crit_p, adj_ci_cvrg, adj_p] = fdr_bh(results(:,1))
expo_fdr = adj_p<0.05;

[h, crit_p, adj_ci_cvrg, adj_p] = fdr_bh(results(:,2))
offset_fdr = adj_p<0.05;

% sign offset
for elec = 1:199
    if mean(off4test_pd(:,elec), 1) < mean(off4test_hc(:,elec), 1)
        offset_fdr(elec) = - offset_fdr(elec);
    end
end

% sign expo
for elec = 1:199
    if mean(exp4test_pd(:,elec), 1) < mean(exp4test_hc(:,elec), 1)
        expo_fdr(elec) = - expo_fdr(elec);
    end
end

% plot exponent post stim

% Load channels location : 199chanlocs.mat
cd(uigetdir())
load('199chanlocs.mat'); % add your path to the channel file

%  get limits from both datasets
forlims = cat(1, mean(exp4test_hc, 2), mean(exp4test_pd, 2));
clim = [min(forlims), max(forlims)];

expo_hc_fig = figure
topoplot(mean(exp4test_hc, 1),chanlocs, 'electrodes', 'off', ...
    'gridscale', 500)
set(gca, 'clim', clim);
colormap(parula)
title({'HC', 'Exponent'}, 'FontSize', 20, 'FontWeight', 'normal')
set(gcf,'Position',[100 100 300 400])
colorbar
print(gcf, "expo_hc_post_fig", '-dpng', '-r1000') % pour la résolution  de l'image


expo_pd_fig = figure
topoplot(mean(exp4test_pd, 1),chanlocs, 'electrodes', 'off', ...
    'gridscale', 500)
set(gca, 'clim', clim);
colormap(parula)
title({'PD', 'Exponent'}, 'FontSize', 20, 'FontWeight', 'normal')
set(gcf,'Position',[100 100 300 400])
colorbar
print(gcf, "expo_pd_post_fig", '-dpng', '-r1000') % pour la résolution  de l'image

figure
topoplot(zeros(1,199), chanlocs, 'electrodes', 'off',...
    'emarkersize', 25, 'electcolor', [0.8500 0.3250 0.0980],...
    'gridscale', 300)
title('p-value')
colorbar
colormap(redbluecmap)
title({'Group effect', 'significant electrodes'}, 'FontSize', 20, 'FontWeight', 'normal')
set(gcf,'Position',[100 100 300 400])
colorbar
print(gcf, "expo_pval_post_fig", '-dpng', '-r1000') % pour la résolution  de l'image


% plot offset post stim

%  get limits from both datasets
forlims = cat(1, mean(off4test_hc, 2), mean(off4test_pd, 2));
clim = [min(forlims), max(forlims)];

off_hc_fig = figure
topoplot(mean(off4test_hc, 1),chanlocs, 'electrodes', 'off', ...
    'gridscale', 500)
set(gca, 'clim', clim);
colormap(parula)
title({'HC', 'Offset'}, 'FontSize', 20, 'FontWeight', 'normal')
set(gcf,'Position',[100 100 300 400])
colorbar
print(gcf, "off_hc_post_fig", '-dpng', '-r1000') % pour la résolution  de l'image


off_pd_fig = figure
topoplot(mean(off4test_pd, 1),chanlocs, 'electrodes', 'off', ...
    'gridscale', 500)
set(gca, 'clim', clim);
colormap(parula)
title({'PD', 'Offset'}, 'FontSize', 20, 'FontWeight', 'normal')
set(gcf,'Position',[100 100 300 400])
colorbar
print(gcf, "off_pd_post_fig", '-dpng', '-r1000') % pour la résolution  de l'image

figure
topoplot(zeros(1,sum(offset_fdr)), chanlocs(offset_fdr), 'electrodes', 'on',...
    'emarkersize', 20, 'electcolor', [0.8500 0.3250 0.0980],...
    'gridscale', 300)
colorbar
colormap(redbluecmap)
title({'Group effect', 'significant electrodes'}, 'FontSize', 20, 'FontWeight', 'normal')
set(gcf,'Position',[100 100 300 400])
colorbar
print(gcf, "off_pval_post_fig", '-dpng', '-r1000') % pour la résolution  de l'image


% Get cong/incong data
post_exp = zeros(59,199,2);
for subi =1:30
    post_exp(subi,:,1) =  squeeze(HCtask(subi,:,1,1));
    post_exp(subi,:,2) = squeeze(HCtask(subi,:,2,1));
end
for subi =1:29
    post_exp(30+subi,:,1) =  squeeze(PDtask(subi,:,1,1));
    post_exp(30+subi,:,2) = squeeze(PDtask(subi,:,2,1));
end


post_off = zeros(59,199,2);
for subi =1:30
    post_off(subi,:,1) =  squeeze(HCtask(subi,:,1,2));
    post_off(subi,:,2) = squeeze(HCtask(subi,:,2,2));
end
for subi =1:29
    post_off(30+subi,:,1) =  squeeze(PDtask(subi,:,1,2));
    post_off(30+subi,:,2) = squeeze(PDtask(subi,:,2,2));
end

% post_HC_exp_c =  squeeze(mean(HCtask(:,:,1,1), 1));
% post_HC_exp_ic = squeeze(mean(HCtask(:,:,2,1), 1));
% post_PD_exp_c = squeeze(mean(PDtask(:,:,1,1), 1));
% post_PD_exp_ic = squeeze(mean(PDtask(:,:,2,1), 1));
% post_HC_off_c = squeeze(mean(HCtask(:,:,1,2), 1));
% post_HC_off_ic = squeeze(mean(HCtask(:,:,2,2), 1));
% post_PD_off_c = squeeze(mean(PDtask(:,:,1,2), 1));
% post_PD_off_ic = squeeze(mean(PDtask(:,:,2,2), 1));



% plot exponent post stim cong/incong

expo_c_fig = figure
topoplot(squeeze(mean(post_exp(:,:,1), 1)),chanlocs, 'electrodes', 'off', ...
    'gridscale', 500)
set(gca, 'clim', clim);
colormap(parula)
title({'Congruent', 'Exponent'}, 'FontSize', 20, 'FontWeight', 'normal')
set(gcf,'Position',[100 100 300 400])
colorbar
print(gcf, "expo_c_post_fig", '-dpng', '-r1000') % pour la résolution  de l'image


expo_ic_fig = figure
topoplot(squeeze(mean(post_exp(:,:,2), 1)),chanlocs, 'electrodes', 'off', ...
    'gridscale', 500)
set(gca, 'clim', clim);
colormap(parula)
title({'Incongruent', 'Exponent'}, 'FontSize', 20, 'FontWeight', 'normal')
set(gcf,'Position',[100 100 300 400])
colorbar
print(gcf, "expo_ic_post_fig", '-dpng', '-r1000') % pour la résolution  de l'image

figure
topoplot(zeros(1,199), chanlocs, 'electrodes', 'off',...
    'emarkersize', 25, 'electcolor', [0.8500 0.3250 0.0980],...
    'gridscale', 300)
title('p-value')
colorbar
colormap(redbluecmap)
title({'Congruence effect', 'significant electrodes'}, 'FontSize', 20, 'FontWeight', 'normal')
set(gcf,'Position',[100 100 300 400])
colorbar
print(gcf, "expo_pval_congruence_post_fig", '-dpng', '-r1000') % pour la résolution  de l'image





% plot offset post stim cong/incong


off_c_fig = figure
topoplot(squeeze(mean(post_off(:,:,1), 1)),chanlocs, 'electrodes', 'off', ...
    'gridscale', 500)
set(gca, 'clim', clim);
colormap(parula)
title({'Congruent', 'Offset'}, 'FontSize', 20, 'FontWeight', 'normal')
set(gcf,'Position',[100 100 300 400])
colorbar
print(gcf, "off_c_post_fig", '-dpng', '-r1000') % pour la résolution  de l'image


off_ic_fig = figure
topoplot(squeeze(mean(post_off(:,:,2), 1)),chanlocs, 'electrodes', 'off', ...
    'gridscale', 500)
set(gca, 'clim', clim);
colormap(parula)
title({'Incongruent', 'Offset'}, 'FontSize', 20, 'FontWeight', 'normal')
set(gcf,'Position',[100 100 300 400])
colorbar
print(gcf, "off_ic_post_fig", '-dpng', '-r1000') % pour la résolution  de l'image

figure
topoplot(zeros(1,199), chanlocs, 'electrodes', 'off',...
    'emarkersize', 25, 'electcolor', [0.8500 0.3250 0.0980],...
    'gridscale', 300)
title('p-value')
colorbar
colormap(redbluecmap)
title({'Congruence effect', 'significant electrodes'}, 'FontSize', 20, 'FontWeight', 'normal')
set(gcf,'Position',[100 100 300 400])
colorbar
print(gcf, "off_pval_congruence_post_fig", '-dpng', '-r1000') % pour la résolution  de l'image






























% On the regio averaged data

 [h,p,ci,stats] = ttest2(mean(exponent_hc, 2), mean(exponent_pd, 2));
 [h,p,ci,stats] = ttest2(mean(offset_hc, 2), mean(offset_pd, 2));
 
 computeCohen_d(mean(offset_hc, 2), mean(offset_pd, 2))