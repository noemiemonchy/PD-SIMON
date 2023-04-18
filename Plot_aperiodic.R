library(tidyverse)
library(ggpubr)
library(see)
library(ggbeeswarm)

## Plot n nodes of kept  after 1/f thresh

data_conn = as_tibble(read.table('graph_table_nodes_kept_1f_only.csv', sep = ",", header = TRUE))
data_conn$frequencies = as.factor(data_conn$frequencies)

p <- data_conn %>%
  mutate(frequencies = fct_relevel(frequencies, "delta", "theta", "alpha", "beta", "gamma")) %>%
  ggplot(aes(x = frequencies, y = n_nodes_kept, fill = group))+
  scale_fill_manual(values=c("#884da7", "#b67823")) +
  #  geom_violin(alpha=0.9, position = position_dodge(width = .75),size=1,color=NA) +
  geom_boxplot(notch = F,  outlier.size = -1, color="black",lwd=0.5, alpha = 0.7,show.legend = T)+
  # geom_point( shape = 21,size=2, position = position_jitterdodge(), color="black",alpha=1)+
  ggbeeswarm::geom_quasirandom(shape = 21,size=2, dodge.width = .75, color="black",alpha=.5,show.legend = F)+
  theme_minimal()+
  ylab(  c("Number of kept nodes after 1/f correction")  )  +
  xlab(  c("Frequencies")  )  +
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=2),
    axis.line = element_line(colour = "black",size=1),
    axis.ticks = element_line(size=1,color="black"),
    axis.text = element_text(color="black"),
    axis.ticks.length=unit(0.2,"cm"),
    legend.position = c(0.95, 0.85),
    plot.title = element_text(size = 20))+
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15) +  
  font("legend.text",size = 15)+
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black")))

p


## Plot percentage of kept connections after 1/f thresh

data_conn = as_tibble(read.table('graph_table_connexions_kept_1f_only.csv', sep = ",", header = TRUE))
data_conn$frequencies = as.factor(data_conn$frequencies)

p <- data_conn %>%
  mutate(frequencies = fct_relevel(frequencies, "delta", "theta", "alpha", "beta", "gamma")) %>%
  ggplot(aes(x = frequencies, y = pct_cx_kept_only1f, fill = group))+
  scale_fill_manual(values=c("#884da7", "#b67823")) +
  #  geom_violin(alpha=0.9, position = position_dodge(width = .75),size=1,color=NA) +
  geom_boxplot(notch = F,  outlier.size = -1, color="black",lwd=0.5, alpha = 0.7,show.legend = T)+
  # geom_point( shape = 21,size=2, position = position_jitterdodge(), color="black",alpha=1)+
  ggbeeswarm::geom_quasirandom(shape = 21,size=2, dodge.width = .75, color="black",alpha=.5,show.legend = F)+
  theme_minimal()+
  ylab(  c("% of kept connections 1/f only")  )  +
  xlab(  c("Frequencies")  )  +
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=2),
    axis.line = element_line(colour = "black",size=1),
    axis.ticks = element_line(size=1,color="black"),
    axis.text = element_text(color="black"),
    axis.ticks.length=unit(0.2,"cm"),
    legend.position = c(0.95, 0.85),
    plot.title = element_text(size = 20))+
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15) +  
  font("legend.text",size = 15)+
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black")))

p


## Plot percentage of kept connections after 5% and 1/f thresh

data_conn = as_tibble(read.table('graph_table_connexions_kept_5pct_and_1f.csv', sep = ",", header = TRUE))
data_conn$frequencies = as.factor(data_conn$frequencies)
data_conn = data_conn[which(data_conn$fc_meth == "wpli"),]


p2 <- data_conn %>%
  mutate(frequencies = fct_relevel(frequencies, "delta", "theta", "alpha", "beta", "gamma")) %>%
  ggplot(aes(x = frequencies, y = pct_cx_kept, fill = group))+
  scale_fill_manual(values=c("#884da7", "#b67823")) +
#  geom_violin(alpha=0.9, position = position_dodge(width = .75),size=1,color=NA) +
  geom_boxplot(notch = F,  outlier.size = -1, color="black",lwd=0.5, alpha = 0.7,show.legend = F)+
  # geom_point( shape = 21,size=2, position = position_jitterdodge(), color="black",alpha=1)+
  ggbeeswarm::geom_quasirandom(shape = 21,size=2, dodge.width = .75, color="black",alpha=.5,show.legend = F)+
  theme_minimal()+
  ylab(  c("% of kept connections")  )  +
  xlab(  c("Frequencies")  )  +
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=2),
    axis.line = element_line(colour = "black",size=1),
    axis.ticks = element_line(size=1,color="black"),
    axis.text = element_text(color="black"),
    axis.ticks.length=unit(0.2,"cm"),
    legend.position = c(0.95, 0.85),
    plot.title = element_text(size = 20))+
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15) +  
  font("legend.text",size = 15)+
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black")))+
  ggtitle("wPLI")

ggarrange(p1, p2, p3, p4, p5,
          ncol = 2, nrow = 3)


## Plot graph metrics

data = as_tibble(read.table('graph_table.csv', sep = ",", header = TRUE))
data$frequencies = as.factor(data$frequencies)
data = data[which(data$fc_meth == "oenv"),]
data = data[which(data$thresh_met == "node_1f"),]



p2 <- data %>%
  
  mutate(frequencies = fct_relevel(frequencies, "delta", "theta", "alpha", "beta", "gamma")) %>%
  ggplot(aes(x = frequencies, y = mean_betweenness, fill = group))+
  scale_fill_manual(values=c("#884da7", "#b67823")) +
  #  geom_violin(alpha=0.9, position = position_dodge(width = .75),size=1,color=NA) +
  geom_boxplot(notch = F,  outlier.size = -1, color="black",lwd=0.5, alpha = 0.7,show.legend = F)+
  # geom_point( shape = 21,size=2, position = position_jitterdodge(), color="black",alpha=1)+
  ggbeeswarm::geom_quasirandom(shape = 21,size=2, dodge.width = .75, color="black",alpha=.5,show.legend = F)+
  theme_minimal()+
  ylab(  c("Mean betweenness centrality")  )  +
  xlab(  c("Frequencies")  )  +
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=2),
    axis.line = element_line(colour = "black",size=1),
    axis.ticks = element_line(size=1,color="black"),
    axis.text = element_text(color="black"),
    axis.ticks.length=unit(0.2,"cm"),
    legend.position = c(0.95, 1),
    plot.title = element_text(size = 20))+
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15) +  
  font("legend.text",size = 15)+
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black")))+
  ggtitle("5% proportional + 1/f")+
  ylim(0, 175)

ggarrange(p1, p2,
          ncol = 2, nrow = 1)


## Plot degree distribution

degree_data = as_tibble(read.table('graph_table_degree_distribution.csv', sep = ",", header = TRUE))
degree_data <- degree_data %>% 
  filter(!(group == 'PD' & sub==8))

  deg_beta <- degree_data %>% 
  filter(frequencies == 'beta', fc_meth == 'oenv', thresh_met == 'node')
  p = ggplot(deg_beta, aes(x=degree, color=group)) +
    stat_ecdf(geom = "step", pad = F)+
    theme_minimal()+
    ylab(  c("Empirical cumulative distribution")  )  +
    xlab(  c("Beta orthogonalized aec degree")  )  +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
          axis.line = element_line(colour = "black",size=1),
          axis.ticks = element_line(size=1,color="black"),
          axis.text = element_text(color="black"),
          axis.ticks.length=unit(0.2,"cm"),
          legend.position = c(0.875, 0.8),
          plot.title = element_text(size = 20))+
    font("xylab",size=15)+  
    font("xy",size=15)+ 
    font("xy.text", size = 15) +  
    font("legend.text",size = 15)+
    guides(fill = guide_legend(override.aes = list(beta = 1,color="black")))+
    ggtitle("5% thresholding")+
    scale_color_manual(values=c("#884da7", "#b67823"))+
    ylim(0, 1)
  
  
  deg_beta <- degree_data %>% 
    filter(frequencies == 'beta', fc_meth == 'oenv', thresh_met == 'node_1f')
  p2 = ggplot(deg_beta, aes(x=degree, color=group)) +
    stat_ecdf(geom = "step",pad = F, show.legend = F)+
    theme_minimal()+
    ylab(  c("Empirical cumulative distribution")  )  +
    xlab(  c("Beta orthogonalized aec degree")  )  +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
          axis.line = element_line(colour = "black",size=1),
          axis.ticks = element_line(size=1,color="black"),
          axis.text = element_text(color="black"),
          axis.ticks.length=unit(0.2,"cm"),
          legend.position = c(0.875, 0.9),
          plot.title = element_text(size = 20))+
    font("xylab",size=15)+  
    font("xy",size=15)+ 
    font("xy.text", size = 15) +  
    font("legend.text",size = 15)+
    guides(fill = guide_legend(override.aes = list(beta = 1,color="black")))+
    ggtitle("5% thresholding + 1/f")+
    scale_color_manual(values=c("#884da7", "#b67823"))+
    ylim(0, 1)
  
  ggarrange(p, p2,
            ncol = 2, nrow = 1)