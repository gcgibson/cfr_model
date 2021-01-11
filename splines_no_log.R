library("splines")
library("rstan")
library(covidcast)

start_day <- "2020-03-15"
timezero_day <-  "2020-12-03"
forecast_day <-  as.Date(timezero_day) + 28
region <- "ca"

ca_deaths <- covidcast_signal(data_source = "jhu-csse",
                              signal ="deaths_incidence_num",
                              start_day = start_day, end_day = timezero_day,
                              geo_type = "state", geo_values =region)



ca_deaths$value[ca_deaths$value <=0] <- NA
interp_obj <- approx(1:nrow(ca_deaths),ca_deaths$value,xout =which(is.na(ca_deaths$value)) )
ca_deaths[interp_obj$x,]$value<-interp_obj$y
ca_deaths$value[is.na(ca_deaths$value)] <- 0




ca_cases <- covidcast_signal(data_source = "jhu-csse",
                              signal ="confirmed_incidence_num",
                              start_day = start_day, end_day = timezero_day,
                              geo_type = "state", geo_values =region)





library("splines")
X <- seq(1,length(ca_deaths$value))
X_new <- seq(length(X)+1,length(X)+28)

B_total_cases <- t(ns(c(X,X_new), knots=seq(1,length(X)+length(X_new),by=20),intercept = TRUE)) # creating the B-splinesl
B_total_deaths <- t(ns(c(X,X_new), knots=seq(1,length(X)+length(X_new),by=20),intercept = TRUE)) # creating the B-splinesl

num_data <- nrow(ca_cases)
lambda_ = 21
mask <- matrix(0,nrow=num_data,ncol=num_data)
for (i in 1:num_data){
  tmp=rev(dgeom(seq(0,num_data-1)[1:(i)],1/21))
  mask[i,1:(i)]=tmp
}


mask_w_pred <-  matrix(0,nrow=num_data+28,ncol=num_data+28)
for (i in 1:(num_data+28)){
  tmp=rev(dgeom(seq(0,(num_data+28)-1)[1:(i)],1/21))
  mask_w_pred[i,1:(i)]=tmp
}


num_basis <- nrow(B_total_cases)
B_cases <- B_total_cases[1:num_basis,1:length(X)]
B_deaths <- B_total_deaths[1:num_basis,1:length(X)]


B_predict_cases <- B_total_cases[1:num_basis,seq(length(X)+1,length(X)+28)]
B_predict_deaths <- B_total_deaths[1:num_basis,seq(length(X)+1,length(X)+28)]


num_data <- length(X);

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
sm<-stan_model("splines_no_log.stan")
fit<-sampling(sm,iter=100,chains=1,data=list(y_deaths=pmax(0,ca_deaths$value),
                                             y_cases=pmax(0,ca_cases$value),
                                             n=nrow(ca_deaths),
                                             day_of_week = rep(1:7,length.out=nrow(ca_deaths)),
                                             mask=mask,
                                             num_data=num_data,
                                             num_basis_cases=num_basis,
                                             num_basis_deaths=num_basis,
                                             X_cases=X,
                                             X_cases_new=X_new,
                                             B_cases=B_cases,
                                             B_cases_predict=B_predict_cases,
                                             X_deaths=X,
                                             X_deaths_new=X_new,
                                             B_deaths=B_deaths,
                                             B_deaths_predict=B_predict_deaths,
                                                                                          mask_pred=mask_w_pred),
              control = list(adapt_delta=0.95))

fit_yhat <- extract(fit,pars=c(),
                    permuted = TRUE, include = FALSE)


validation_death <-  covidcast_signal(data_source = "jhu-csse",
                                            signal ="deaths_incidence_num",
                                            start_day = as.Date(timezero_day)+1, end_day = forecast_day,
                                            geo_type = "state", geo_values =region)


validation_cases <-  covidcast_signal(data_source = "jhu-csse",
                                      signal ="confirmed_incidence_num",
                                      start_day = as.Date(timezero_day)+1, end_day = forecast_day,
                                      geo_type = "state", geo_values =region)




plot(c(colMeans(fit_yhat$Y_hat_cases),colMeans(fit_yhat$pred_cases)),type='l',ylim=c(0,max(ca_cases$value)))
points(ca_cases$value,cex=.2,col='red')
points(seq(nrow(ca_deaths)+1,nrow(ca_deaths)+28),validation_cases$value,col='blue',cex=.2)

plot(c(colMeans(fit_yhat$Y_hat_deaths)),type='l',ylim=c(0,max(ca_deaths$value)))
points(ca_deaths$value,cex=.2,col='red')


plot(c(colMeans(fit_yhat$pred_death_total)),type='l',ylim=c(0,max(ca_deaths$value)))
lines(c(apply(fit_yhat$pred_death_total,2,function(x){quantile(x,.975)})),type='l',ylim=c(0,max(ca_deaths$value)))
lines(c(apply(fit_yhat$pred_death_total,2,function(x){quantile(x,.025)})),type='l',ylim=c(0,max(ca_deaths$value)))

points(ca_deaths$value,cex=.2,col='red')
points(seq(nrow(ca_deaths)+1,nrow(ca_deaths)+28),validation_death$value,col='blue',cex=.2)

mean(fit_yhat$sigma_cases)
mean(fit_yhat$sigma_deaths)

