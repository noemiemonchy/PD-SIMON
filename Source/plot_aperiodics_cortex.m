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

nROI = 68;
% Create a matrix for PD and HC with all the results
% nsub * ROI * condition (1 rest / 2 pre/3 post) * aperio (1 expo/2 offset) 
HC = zeros(length(specparam_post_HC_sorted), nROI, 3, 2);
% Same but for the task, differenciating c and ic, % nsub * ROI * condition (1 c / 2 ic) * aperio (1 expo/2 offset) 
HCtask = zeros(length(specparam_post_HC_sorted), nROI, 2, 2)

% Same for PD
PD = zeros(length(specparam_post_PD_sorted), nROI, 3, 2);
PDtask = zeros(length(specparam_post_PD_sorted), nROI, 2, 2)

% For HC
for subi = 1:size(HC, 1) 
    HC(subi, : , 1, 1) = [specparam_source_hc_sorted(subi).spec.aperiodics.exponent]  ; % rest, expo
    HC(subi, : , 1, 2) = [specparam_source_hc_sorted(subi).spec.aperiodics.offset]  ; % rest, offset
    
    HC(subi, : , 2, 1) = [specparam_pre_HC_sorted(subi).spec.aperiodics.exponent]  ; % pre, expo
    HC(subi, : , 2, 2) = [specparam_pre_HC_sorted(subi).spec.aperiodics.offset]  ; % pre, offset
    
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
    PD(subi, : , 1, 1) = [specparam_source_pd_sorted(subi).spec.aperiodics.exponent]  ; % rest, expo
    PD(subi, : , 1, 2) = [specparam_source_pd_sorted(subi).spec.aperiodics.offset]  ; % rest, offset
    
    PD(subi, : , 2, 1) = [specparam_pre_PD_sorted(subi).spec.aperiodics.exponent]  ; % pre, expo
    PD(subi, : , 2, 2) = [specparam_pre_PD_sorted(subi).spec.aperiodics.offset]  ; % pre, offset
    
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
writetable(graph_t, ['sources_aperiodic_param.csv']);



%% Create table to get all ROI, group and subject-specific parameters 
%  in a single csv file, only for the 3 periods

% with headers:
% Sub, Gp, period, ROI, exponent, offset

group = {'HC', 'PD'};

i = 1; % initiate counter to fill rows

varnames = {'sub', 'group','period','ROI','exponent', 'offset'};
vartypes = {'double', 'string', 'string','double'...
    'double','double'};

t = table('Size', [10000, 6], 'VariableTypes', vartypes, 'VariableNames', varnames);
condition = {'Rest', 'Pre-stimulus', 'Post-stimulus'};
% HC
for subi = 1 : size(HC, 1)
    for condi = 1:3
        for ROI = 1:68
            sub = subi ;
            gp = 'HC';
            period = condition{condi};
            exponent = HC(subi,ROI, condi,1);
            offset = HC(subi,ROI, condi,2);
            t(i,:) = {sub, gp, period, ROI, exponent, offset};
            i = i+1;
        end
    end
end

% PD
for subi = 1 : size(PD, 1)
    for condi = 1:3
        for ROI = 1:68
            sub = subi ;
            gp = 'PD';
            period = condition{condi};
            exponent = PD(subi,ROI, condi,1);
            offset = PD(subi,ROI, condi,2);
            t(i,:) = {sub, gp, period, ROI, exponent, offset};
            i = i+1;
        end
    end
end
graph_t = rmmissing(t);
writetable(graph_t, ['sources_aperiodic_param_all.csv']);


%% Create table to get all ROI, group and subject-specific parameters 
%  in a single csv file, only for post stimulus and according to congruence

% with headers:
% Sub, Gp, congruence, ROI, exponent, offset

group = {'HC', 'PD'};

i = 1; % initiate counter to fill rows

varnames = {'sub', 'group','congruence','ROI','exponent', 'offset'};
vartypes = {'double', 'string', 'string','double'...
    'double','double'};

t = table('Size', [10000, 6], 'VariableTypes', vartypes, 'VariableNames', varnames);
condition = {'C', 'IC'};
% HC
for subi = 1 : size(HC, 1)
    for condi = 1:2
        for ROI = 1:68
            sub = subi ;
            gp = 'HC';
            congruence = condition{condi};
            exponent = HCtask(subi,ROI, condi,1);
            offset = HCtask(subi,ROI, condi,2);
            t(i,:) = {sub, gp, congruence, ROI, exponent, offset};
            i = i+1;
        end
    end
end

% PD
for subi = 1 : size(PD, 1)
    for condi = 1:2
        for ROI = 1:68
            sub = subi ;
            gp = 'PD';
            congruence = condition{condi};
            exponent = PDtask(subi,ROI, condi,1);
            offset = PDtask(subi,ROI, condi,2);
            t(i,:) = {sub, gp, congruence, ROI, exponent, offset};
            i = i+1;
        end
    end
end
graph_t = rmmissing(t);
writetable(graph_t, ['sources_aperiodic_param_post_task.csv']);

%% Stats on region-specific group changes in exponent and offset
results = zeros(68,2); % first col is exponent, second is offset
% two-sided t tests
for roi = 1:68
    [h,p,ci,stats] = ttest2(exponent_hc(:,roi), exponent_pd(:,roi));
    results(roi, 1) = p;

    [h,p,ci,stats] = ttest2(offset_hc(:,roi), offset_pd(:,roi));
    results(roi, 2) = p;
end
FDR = zeros(68, 2);
FDR(:,1) = mafdr(results(:,1));
FDR(:,2) = mafdr(results(:,2));
FDR_bin = FDR<0.05;
[FDR,Q,aPrioriProb] =  mafdr(results(:,1));

[h, crit_p, adj_ci_cvrg, adj_p] = fdr_bh(results(:,1))
expo_fdr = adj_p<0.05;

[h, crit_p, adj_ci_cvrg, adj_p] = fdr_bh(results(:,2))
offset_fdr = adj_p<0.05;
for roi = 1:68
    if mean(offset_pd(:,roi), 1) < mean(offset_hc(:,roi), 1)
        offset_fdr(roi) = - offset_fdr(roi);
    end
end


% On the regio averaged data

 [h,p,ci,stats] = ttest2(mean(exponent_hc, 2), mean(exponent_pd, 2));
 [h,p,ci,stats] = ttest2(mean(offset_hc, 2), mean(offset_pd, 2));
 
 computeCohen_d(mean(offset_hc, 2), mean(offset_pd, 2))