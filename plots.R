cfr <- read.csv("/Users/gcgibson/cfr_numpyro/mae/cfr_mae.csv")
mb<- read.csv("/Users/gcgibson/cfr_numpyro/mae/mb_mae.csv")
cfr$model <- "cfr"
mb$model <- "mb"
library(tidyverse)
library(ggplot2)

data <-cfr %>% dplyr::left_join(mb, by=c("state","date","h"))
data_long <-rbind(cfr,mb)


panel_1 <- ggplot(data,aes(x=sort(mae.y),y=sort(mae.x))) + geom_point() +
ylim(c(0,5000)) + xlim(c(0,5000)) + geom_abline(slope=1,intercept = 0) + 
  theme_bw() + ylab("CFR Mortality MAE") + xlab("MechBayes Mortality MAE")


panel_2 <- ggplot(data %>% dplyr::group_by(state) %>% dplyr::summarize(mae.x = mean(mae.x),mae.y=mean(mae.y)),aes(x=mae.y,y=mae.x,label=state)) + geom_text(size=2) +
  theme_bw() + ylab("CFR Mortality MAE") + xlab("MechBayes Mortality MAE") + ylim(0,250) + xlim(0,250) +geom_abline(slope=1,intercept = 0)


data_long$h <- factor(data_long$h)
library(plyr)
data_long$h <-plyr::revalue(data_long$h , c("0"="1", "1"="2","2"="3","3"="4"))

panel_3 <- ggplot(data_long,aes(x=h,y=log(mae),col=model)) + geom_boxplot()+ 
  theme_bw() + ylab("Mortality log(MAE)") + xlab("Horizon")


panel_4 <- ggplot(data_long %>% dplyr::group_by(date,model) %>% dplyr::summarize(mae=mean(mae)),aes(x=as.Date(date),y=log(mae),col=model)) + geom_point()+ 
  theme_bw() + ylab("Mortality MAE") + xlab("Forecast Date") +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



library(cowplot)

full_plot <- cowplot::plot_grid(panel_1,
                                panel_2,
                                panel_3+theme(legend.position = "none"),
                                panel_4+theme(legend.position = "none"),ncol=2,align = 'vh')

ggsave("/Users/gcgibson/cfr_numpyro/full_plot.png",full_plot,device="png",width=8,height =8)

