cfr <- read.csv("/Users/gcgibson/cfr_numpyro/cp/cfr_cp.csv")
mb<- read.csv("/Users/gcgibson/cfr_numpyro/cp/mb_cp.csv")
cfr$model <- "cfr"
mb$model <- "mb"
library(tidyverse)
library(ggplot2)

data <-cfr %>% dplyr::left_join(mb, by=c("state","date","h"))
data_long <-rbind(cfr,mb)




data_long$h <- factor(data_long$h)
library(plyr)
data_long$h <-plyr::revalue(data_long$h , c("0"="1", "1"="2","2"="3","3"="4"))

panel_3 <- ggplot(data_long %>%  dplyr::group_by(h,model) %>% dplyr::summarize(cp=mean(cp)),aes(x=h,y=cp,col=model)) + geom_point()+ ylim(c(.90,1.0)) +
  theme_bw() + ylab("Coverage Probability 95%") + xlab("Horizon") 


panel_4 <- ggplot(data_long %>%  dplyr::group_by(date,model) %>% dplyr::summarize(cp=mean(cp)),aes(x=as.Date(date),y=cp,col=model)) + geom_point()+ 
  theme_bw() + ylab("Coverage Probability 95%") + xlab("Forecast Date") +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



library(cowplot)

full_plot <- cowplot::plot_grid(panel_3+theme(legend.position = "none"),
                                panel_4+theme(legend.position = "none"),ncol=2,align = 'vh')

ggsave("/Users/gcgibson/cfr_numpyro/full_plot_cp.png",full_plot,device="png",width=8,height =8)

