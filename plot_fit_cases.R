date <- "2020-11-14"
  region <- "NY"
preds <- read.csv(paste0("/Users/gcgibson/cfr_numpyro/",region,"/",date,"case_preds.csv"),col.names = F)
preds_lq <- read.csv(paste0("/Users/gcgibson/cfr_numpyro/",region,"/",date,"case_preds_lq.csv"),col.names = F)
preds_uq <- read.csv(paste0("/Users/gcgibson/cfr_numpyro/",region,"/",date,"case_preds_uq.csv"),col.names = F)


length_t <- seq(as.Date("2020-01-25") ,as.Date(date) + 28,by="day")

preds_df <- data.frame(t=length_t, preds=preds$FALSE.,preds_uq=preds_uq$FALSE.,preds_lq=preds_lq$FALSE.)

truth_cases <- read.csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-truth/truth-Incident%20Cases.csv")



library(ggplot2)

ggplot(preds_df,aes(x=t,y=preds)) + geom_line() + theme_bw() +
  geom_ribbon(aes(x=t, ymin = preds_lq, ymax =preds_uq),alpha=.5,fill='blue') +
  geom_point(data=truth_cases[truth_deaths$location_name == "New York" & truth_deaths$date <= as.Date(date) + 28 ,],aes(x=as.Date(date),y=value),size=.5) +
  geom_vline(xintercept = as.Date(date))



