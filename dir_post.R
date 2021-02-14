library(reticulate)
date <- "2020-11-14"

np <- import("numpy")

dir <- np$load(paste0("/Users/gcgibson/cfr_numpyro/samples/NY/",date,"_dirichlet_post.npz.npy"),allow_pickle = T)

df_ <- data.frame(x=1:40,y=colMeans(dir))

df_full <- data.frame(x=rep(1:40,2000),y=c(t(dir)),group=rep(1:2000,each=40))

library(ggplot2)


time_to_death <- ggplot(df_,aes(x=x,y=rev(y)),fill=1) + geom_line() + theme_bw() + xlab("t") + ylab("P(T=t)")

#time_to_death_total <- ggplot(df_full,aes(x=x,y=y,group=group)) + geom_line(alpha=.9) + theme_bw() + xlab("t") + ylab("P(T=t)")

ggsave("/Users/gcgibson/cfr_numpyro/time_to_death",time_to_death,device = "png",width=6,height=4)



det_prob <- np$load(paste0("/Users/gcgibson/cfr_numpyro/samples/NY/",date,"_det_prob.npz.npy"),allow_pickle = T)
num_steps <- dim(det_prob)[2]
det_prob_full <- data.frame(x=1:num_steps,y=colMeans(det_prob),y_uq = colQuantiles(det_prob,probs = .975),y_lq = colQuantiles(det_prob,probs = .025))
det_prob_full$date <- seq(as.Date("2020-03-04"),as.Date(date),by="day")
det_prob_plot <- ggplot(det_prob_full,aes(x=date,y=y)) + 
  geom_line() +geom_ribbon(aes(x=date,ymax=y_uq,ymin=y_lq),alpha=.2,fill="blue") + theme_bw() + xlab("Date") + ylab("CFR")
