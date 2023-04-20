library(tidyverse)
library(ggpubr)
library(see)
library(ggbeeswarm)
library(rstatix)

# ROI-averaged exponent - period -----------------------------------------------

# Plot

data= as_tibble(read.table('sources_aperiodic_param.csv', sep = ",", 
                           header = TRUE))

exp <- data %>%
  mutate(period = fct_relevel(period, "Rest", 
                              "Pre-stimulus", "Post-stimulus")) %>%
  ggplot(aes(x = period, y = averaged_exponent, fill = group))+
  scale_fill_manual(values=c("#884da7", "#b67823")) +
  geom_violin()+
  geom_boxplot(notch = F,  outlier.size = -1, color="black", 
               lwd=0.5, alpha = 0.7,show.legend = T, width = 0.15,
               position = position_dodge(width = 0.9))+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5, 
               position = position_dodge(width = 0.9))+
  theme_minimal()+
  ylab(c("ROI-averaged exponent"))  +
  xlab(c(""))+
  theme(
    axis.line = element_line(colour = "black",size=1),
    axis.ticks = element_line(size=1,color="black"),
    axis.text = element_text(color="black"),
    axis.ticks.length=unit(0.2,"cm"),
    legend.position = c(0.95, 0.15),
    plot.title = element_text(size = 20))+
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15) +  
  font("legend.text",size = 15)+
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black")))


# Stats
data <- data %>%
convert_as_factor(sub, period, group)

# Descriptive stats
data2 %>%
  group_by(period, group) %>%
  get_summary_stats(averaged_exponent, type = "mean_sd")
# Fast plot
bxp <- ggboxplot(
  data2, x = "period", y = "averaged_exponent",
  color = "group", palette = "jco"
)
bxp
# Identify outliers
data2 %>%
  group_by(period, group) %>%
  identify_outliers(averaged_exponent)
# Normality check
data2 %>%
  group_by(period, group) %>%
  shapiro_test(averaged_exponent)

ggqqplot(data2, "averaged_exponent", ggtheme = theme_bw()) +
  facet_grid(period ~ group, labeller = "label_both")
# Calculate anova
res.aov <- anova_test(
  data = data2, dv = averaged_exponent, wid = sub,
  between = group, within = period
)
get_anova_table(res.aov, correction = c("auto")) # auto applies 
# correction if Mauchly test shows violation of sphericity

# Posthoc
pwc <- data2 %>%
    pairwise_t_test(
    averaged_exponent ~ period, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc


# ROI-averaged offset - period -------------------------------------------------


# Plot

off <- data %>%
  mutate(period = fct_relevel(period, "Rest", 
                              "Pre-stimulus", "Post-stimulus")) %>%
  ggplot(aes(x = period, y = averaged_offset, fill = group))+
  scale_fill_manual(values=c("#884da7", "#b67823")) +
  geom_violin()+
  geom_boxplot(notch = F,  outlier.size = -1, color="black",lwd=0.5, 
               alpha = 0.7,show.legend = T, width = 0.15,
               position = position_dodge(width = 0.9))+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5, 
               position = position_dodge(width = 0.9))+
  theme_minimal()+
  ylab(c("ROI-averaged offset"))  +
  xlab(c(""))+
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=2),
    axis.line = element_line(colour = "black",size=1),
    axis.ticks = element_line(size=1,color="black"),
    axis.text = element_text(color="black"),
    axis.ticks.length=unit(0.2,"cm"),
    legend.position = c(0.95, 0.15),
    plot.title = element_text(size = 20))+
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15) +  
  font("legend.text",size = 15)+
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black")))


# Stats

# Descriptive stats
data2 %>%
  group_by(period, group) %>%
  get_summary_stats(averaged_offset, type = "mean_sd")
# Fast plot
bxp <- ggboxplot(
  data2, x = "period", y = "averaged_offset",
  color = "group", palette = "jco"
)
bxp

# Identify outliers
data2 %>%
  group_by(period, group) %>%
  identify_outliers(averaged_offset)

# Normality check
data2 %>%
  group_by(period, group) %>%
  shapiro_test(averaged_offset)

ggqqplot(data2, "averaged_offset", ggtheme = theme_bw()) +
  facet_grid(period ~ group, labeller = "label_both")

# Calculate anova
res.aov <- anova_test(
  data = data2, dv = averaged_offset, wid = sub,
  between = group, within = period
)
get_anova_table(res.aov, correction = c("auto")) # auto applies 
# correction if Mauchly test shows violation of sphericity

# Posthoc
pwc <- data2 %>%
  pairwise_t_test(
    averaged_offset ~ period, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc



# ROI-specific stats with FDR correction - period ------------------------------

data = as_tibble(read.table('sources_aperiodic_param_all.csv', 
                            sep = ",", header = TRUE))

# Exponent

# create array to store results
# create matrix with 3 columns (p group, p period, p interaction) and 68 rows
expo_results = matrix(nrow=68, ncol=3, byrow=TRUE)

# specify the column names and row names of matrix
colnames(expo_results) = c('p_group','p_period','p_inter')

# # assign to table
# expo_results = as.table(expo_results)

# Do a two way repeated measure anova (group x period) on all ROIs

for (roi in 1:68){
  data2= data %>%
    filter(ROI == roi)
  
    res.aov <- anova_test(data2,
      dv = exponent, wid = sub,
      between = group, within = period
    )
    expo_results[roi,1] = res.aov$ANOVA$p[1]
    expo_results[roi,2] = res.aov$ANOVA$p[2]
    expo_results[roi,3] = res.aov$ANOVA$p[3]
    
    }

# apply FDR correction on results
expo_results_fdr = matrix(nrow=68, ncol=3, byrow=TRUE)
colnames(expo_results_fdr) = c('p_group','p_period','p_inter')

for (pi in 1:3){
expo_results_fdr[,pi] = p.adjust(expo_results[,pi], method = 'fdr', 
                                 n = length(expo_results[,pi]))
}
write.csv(expo_results_fdr, "expo_results_fdr.csv", row.names=TRUE)

# ROI-specific posthoc multiple comparisons

# Create matrix with 3 columns (p post-pre, p post-rest, p rest-pre) and 68 rows
# and a 68*3 matrix for each comparisons to show whether the change is + or -
# We'll do post minus pre, post minus rest, pre minus rest.
# if stat > 0, then the first term has the greatest value

off_post_results = matrix(nrow=68, ncol=3, byrow=TRUE)
colnames(off_post_results_sign) = c('post-pre','post-rest','rest-pre')

off_post_results_sign = matrix(nrow=68, ncol=3, byrow=TRUE)
colnames(off_post_results_sign) = c('post-pre','post-rest','rest-pre')

for (roi in 1:68){
  data2 = data %>%
    filter(ROI == roi)
  pwc <- data2 %>%
    pairwise_t_test(
      offset ~ period, paired = TRUE,
      p.adjust.method = "bonferroni"
    )
  off_post_results[roi, 1] = pwc$p.adj[1]
  off_post_results[roi, 2] = pwc$p.adj[2]
  off_post_results[roi, 3] = pwc$p.adj[3]
  off_post_results_sign[roi, 1] = pwc$statistic[1]
  off_post_results_sign[roi, 2] = pwc$statistic[2]
  off_post_results_sign[roi, 3] = pwc$statistic[3]
}

# apply FDR correction on results
off_post_results_fdr = matrix(nrow=68, ncol=3, byrow=TRUE)
colnames(off_post_results_fdr) = c('post-pre','post-rest','rest-pre')

for (pi in 1:3){
  off_post_results_fdr[,pi] = p.adjust(off_post_results[,pi], method = 'fdr', 
                                   n = length(off_post_results[,pi]))
}

write.csv(off_post_results_fdr, "off_period_post_results_fdr.csv",
          row.names=TRUE)
write.csv(off_post_results_sign, "off_period_post_results_sign.csv",
          row.names=TRUE)

# Create a 68*3 matrix for each comparisons to show whether the change is + or -
# We'll do post minus pre, post minus rest, pre minus rest.
off_post_results_sign = matrix(nrow=68, ncol=3, byrow=TRUE)
colnames(off_post_results_sign) = c('post-pre','post-rest','rest-pre')
for (roi in 1:68){
  data2 = data %>%
    filter(ROI == roi, period == 'Post-stimulus')
}

# Offset

# create array to store results
# create matrix with 3 columns (p group, p period, p interaction) and 68 rows
offset_results = matrix(nrow=68, ncol=3, byrow=TRUE)

# specify the column names and row names of matrix
colnames(offset_results) = c('p_group','p_period','p_inter')

# # assign to table
# offset_results = as.table(offset_results)

# Do a two way repeated measure anova (group x period) on all ROIs

for (roi in 1:68){
  data2= data %>%
    filter(ROI == roi)
  
  res.aov <- anova_test(data2,
                        dv = offset, wid = sub,
                        between = group, within = period
  )
  offset_results[roi,1] = res.aov$ANOVA$p[1]
  offset_results[roi,2] = res.aov$ANOVA$p[2]
  offset_results[roi,3] = res.aov$ANOVA$p[3]
  
}

# apply FDR correction on results
offset_results_fdr = matrix(nrow=68, ncol=3, byrow=TRUE)
colnames(offset_results_fdr) = c('p_group','p_period','p_inter')

for (pi in 1:3){
  offset_results_fdr[,pi] = p.adjust(offset_results[,pi], method = 'fdr', 
                                   n = length(offset_results[,pi]))
}
write.csv(offset_results_fdr, "offset_results_fdr.csv", row.names=TRUE)

# ROI-averaged exponent - task (gp x cong) -------------------------------------

data = as_tibble(read.table('sources_aperiodic_param_post_task.csv', 
                            sep = ",", header = TRUE))

# Plot
exp <- data %>%
  group_by(sub, group, congruence) %>% 
  summarize(avg.exp = mean(exponent)) %>%
  ggplot(aes(x = congruence, y = avg.exp, fill = group))+
  scale_fill_manual(values=c("#884da7", "#b67823")) +
  geom_violin()+
  geom_boxplot(notch = F,  outlier.size = -1, color="black", 
               lwd=0.5, alpha = 0.7,show.legend = T, width = 0.15,
               position = position_dodge(width = 0.9))+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5, 
               position = position_dodge(width = 0.9))+
  theme_minimal()+
  ylab(c("ROI-averaged exponent"))  +
  xlab(c(""))+
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=2),
    axis.line = element_line(colour = "black",size=1),
    axis.ticks = element_line(size=1,color="black"),
    axis.text = element_text(color="black"),
    axis.ticks.length=unit(0.2,"cm"),
    legend.position = c(0.95, 0.15),
    plot.title = element_text(size = 20))+
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15) +  
  font("legend.text",size = 15)+
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black")))

# Stats
data2 <- data %>%
  group_by(sub, group, congruence) %>% 
  summarize(averaged_exponent = mean(exponent)) %>%
  convert_as_factor(sub, congruence, group)

# Descriptive stats
data2 %>%
  group_by(congruence, group) %>%
  get_summary_stats(averaged_exponent, type = "mean_sd")
# Fast plot
bxp <- ggboxplot(
  data2, x = "congruence", y = "averaged_exponent",
  color = "group", palette = "jco"
)
bxp
# Identify outliers
data2 %>%
  group_by(congruence, group) %>%
  identify_outliers(averaged_exponent)
# Normality check
data2 %>%
  group_by(congruence, group) %>%
  shapiro_test(averaged_exponent)

ggqqplot(data2, "averaged_exponent", ggtheme = theme_bw()) +
  facet_grid(congruence ~ group, labeller = "label_both")
# Calculate anova
data2 <- as.data.frame(data2)

res.aov <- anova_test(
  data = data2, dv = averaged_exponent, wid = sub,
  between = group, within = congruence
)
get_anova_table(res.aov, correction = c("auto")) # auto applies 
# correction if Mauchly test shows violation of sphericity



# ROI-averaged offset - task (gp x cong) ---------------------------------------

# Plot
off <- data %>%
  group_by(sub, group, congruence) %>% 
  summarize(avg.off = mean(offset)) %>%
  ggplot(aes(x = congruence, y = avg.off, fill = group))+
  scale_fill_manual(values=c("#884da7", "#b67823")) +
  geom_violin()+
  geom_boxplot(notch = F,  outlier.size = -1, color="black",lwd=0.5, 
               alpha = 0.7,show.legend = T, width = 0.15,
               position = position_dodge(width = 0.9))+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5, 
               position = position_dodge(width = 0.9))+
  theme_minimal()+
  ylab(c("ROI-averaged offset"))  +
  xlab(c(""))+
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=2),
    axis.line = element_line(colour = "black",size=1),
    axis.ticks = element_line(size=1,color="black"),
    axis.text = element_text(color="black"),
    axis.ticks.length=unit(0.2,"cm"),
    legend.position = c(0.95, 0.15),
    plot.title = element_text(size = 20))+
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15) +  
  font("legend.text",size = 15)+
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black")))

# Stats
data2 <- data %>%
  group_by(sub, group, congruence) %>% 
  summarize(averaged_offset = mean(offset)) %>%
  convert_as_factor(sub, congruence, group)

# Descriptive stats
data2 %>%
  group_by(congruence, group) %>%
  get_summary_stats(averaged_offset, type = "mean_sd")
# Fast plot
bxp <- ggboxplot(
  data2, x = "congruence", y = "averaged_offset",
  color = "group", palette = "jco"
)
bxp
# Identify outliers
data2 %>%
  group_by(congruence, group) %>%
  identify_outliers(averaged_offset)
# Normality check
data2 %>%
  group_by(congruence, group) %>%
  shapiro_test(averaged_offset)

ggqqplot(data2, "averaged_offset", ggtheme = theme_bw()) +
  facet_grid(congruence ~ group, labeller = "label_both")
# Calculate anova
data2 <- as.data.frame(data2)

res.aov <- anova_test(
  data = data2, dv = averaged_offset, wid = sub,
  between = group, within = congruence
)
get_anova_table(res.aov, correction = c("auto")) # auto applies 
# correction if Mauchly test shows violation of sphericity


# ROI-specific exponent - task (gp x cong) -------------------------------------

# create array to store results
# create matrix with 3 columns (p group, p period, p interaction) and 68 rows
expo_results = matrix(nrow=68, ncol=3, byrow=TRUE)

# specify the column names and row names of matrix
colnames(expo_results) = c('p_group','p_congruence','p_inter')

# # assign to table
# expo_results = as.table(expo_results)

# Do a two way repeated measure anova (group x period) on all ROIs

for (roi in 1:68){
  data2= data %>%
    filter(ROI == roi)
  
  res.aov <- anova_test(data2,
                        dv = exponent, wid = sub,
                        between = group, within = congruence
  )
  expo_results[roi,1] = res.aov$p[1]
  expo_results[roi,2] = res.aov$p[2]
  expo_results[roi,3] = res.aov$p[3]
  
}

# apply FDR correction on results
expo_results_fdr = matrix(nrow=68, ncol=3, byrow=TRUE)
colnames(expo_results_fdr) = c('p_group','p_congruence','p_inter')

for (pi in 1:3){
  expo_results_fdr[,pi] = p.adjust(expo_results[,pi], method = 'fdr', 
                                   n = length(expo_results[,pi]))
}

write.csv(expo_results_fdr, "expo_results_task_fdr.csv", row.names=TRUE)

# ROI-specific offset - task (gp x cong) ---------------------------------------


# create array to store results
# create matrix with 3 columns (p group, p period, p interaction) and 68 rows
offset_results = matrix(nrow=68, ncol=3, byrow=TRUE)

# specify the column names and row names of matrix
colnames(offset_results) = c('p_group','p_congruence','p_inter')


# Do a two way repeated measure anova (group x period) on all ROIs

for (roi in 1:68){
  data2= data %>%
    filter(ROI == roi)
  
  res.aov <- anova_test(data2,
                        dv = offset, wid = sub,
                        between = group, within = congruence
  )
  offset_results[roi,1] = res.aov$p[1]
  offset_results[roi,2] = res.aov$p[2]
  offset_results[roi,3] = res.aov$p[3]
  
}

# apply FDR correction on results
offset_results_fdr = matrix(nrow=68, ncol=3, byrow=TRUE)
colnames(offset_results_fdr) = c('p_group','p_congruence','p_inter')

for (pi in 1:3){
  offset_results_fdr[,pi] = p.adjust(offset_results[,pi], method = 'fdr', 
                                   n = length(offset_results[,pi]))
}

write.csv(offset_results_fdr, "offset_results_task_fdr.csv", row.names=TRUE)


