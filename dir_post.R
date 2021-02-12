library(reticulate)
np <- import("numpy")

dir <- np$load("/Users/gcgibson/cfr_numpyro/dirichlet_post.npz.npy",allow_pickle = T)

df_ <- data.frame(x=1:40,y=colMeans(dir))

df_full <- data.frame(x=rep(1:40,2000),y=c(t(dir)),group=rep(1:2000,each=40))

library(ggplot2)


time_to_death <- ggplot(df_,aes(x=x,y=rev(y)),fill=1) + geom_line() + theme_bw() + xlab("t") + ylab("P(T=t)")

time_to_death_total <- ggplot(df_full,aes(x=x,y=y,group=group)) + geom_line(alpha=.9) + theme_bw() + xlab("t") + ylab("P(T=t)")

ggsave("/Users/gcgibson/cfr_numpyro/time_to_death",time_to_death,device = "png",width=6,height=4)
