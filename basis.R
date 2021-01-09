library("splines")
library("rstan")
library(covidcast)

start_day <- "2020-03-15"
timezero_day <-  "2020-12-06"
forecast_day <-  as.Date("2020-12-06") + 27
region <- "ca"

#ca_deaths <- covidcast_signal(data_source = "jhu-csse",
 #                             signal ="deaths_incidence_num",
  #                           start_day = start_day, end_day = timezero_day,
   #                           geo_type = "state", geo_values =region)

ca_deaths$value[ca_deaths$value <=0] <- NA
interp_obj <- approx(1:nrow(ca_deaths),ca_deaths$value,xout =which(is.na(ca_deaths$value)) )
ca_deaths[interp_obj$x,]$value<-interp_obj$y
ca_deaths$value[is.na(ca_deaths$value)] <- 0

#write.csv(ca_deaths,"ca_deaths.csv")

#ca_cases <- covidcast_signal(data_source = "jhu-csse",
 #                            signal ="confirmed_incidence_num",
  #                           start_day =start_day, end_day = timezero_day,
   #                          geo_type = "state", geo_values = region)


#write.csv(ca_cases,"ca_cases.csv")


num_data <- nrow(ca_cases)
lambda_ = 21
mask <- matrix(0,nrow=num_data,ncol=num_data)
for (i in 1:num_data){
  tmp=rev(dgeom(seq(0,num_data-1)[1:(i)],1/21))
  mask[i,1:(i)]=tmp
}


mask_w_pred <-  matrix(0,nrow=num_data+27,ncol=num_data+27)
for (i in 1:(num_data+27)){
  tmp=rev(dgeom(seq(0,(num_data+27)-1)[1:(i)],1/21))
  mask_w_pred[i,1:(i)]=tmp
}



library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
sm<-stan_model("llm.stan")
fit<-sampling(sm,iter=500,data=list(y_deaths=ca_deaths$value,
                                    y_cases=ca_cases$value,
                                    n=nrow(ca_deaths),
                                    day_of_week = rep(1:7,length.out=nrow(ca_deaths)),
                                    mask=mask,
                                    mask_w_pred=mask_w_pred),
                     control = list(adapt_delta=0.95))

fit_yhat <- extract(fit,pars=c(),
        permuted = TRUE, include = FALSE)



median_point_forecasts <- tail(apply(fit_yhat$y_hat_deaths_total,2,function(x){quantile(x,probs=.5)}),27)

plot(colMeans(fit_yhat$yhat_deaths),type='l')
points(ca_deaths$value,col='red',cex=.2)

plot(colMeans(fit_yhat$y_hat_cases_total),type='l')
points(ca_cases$value,col='red',cex=.2)

plot(colMeans(fit_yhat$y_hat_deaths_total))


