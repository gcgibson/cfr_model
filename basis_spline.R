library("splines")
library("rstan")
library(covidcast)

start_day <- "2020-03-15"
timezero_day <-  "2020-11-10"
forecast_day <-  as.Date(timezero_day) + 28
region <- "nm"

ca_deaths <- covidcast_signal(data_source = "jhu-csse",
                             signal ="deaths_incidence_num",
                           start_day = start_day, end_day = timezero_day,
                           geo_type = "state", geo_values =region)

ca_deaths$value[ca_deaths$value <=0] <- NA
interp_obj <- approx(1:nrow(ca_deaths),ca_deaths$value,xout =which(is.na(ca_deaths$value)) )
ca_deaths[interp_obj$x,]$value<-interp_obj$y
ca_deaths$value[is.na(ca_deaths$value)] <- 0

#write.csv(ca_deaths,"ca_deaths.csv")

ca_cases <- covidcast_signal(data_source = "jhu-csse",
                            signal ="confirmed_incidence_num",
                           start_day =start_day, end_day = timezero_day,
                          geo_type = "state", geo_values = region)


#write.csv(ca_cases,"ca_cases.csv")


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

library("splines")
X <- seq(1,length(ca_deaths$value))
X_new <- seq(length(X)+1,length(X)+28)

B_total <- t(ns(c(X,X_new), knots=seq(1,length(X)+length(X_new),by=20),intercept = TRUE)) # creating the B-splinesl

num_basis <- nrow(B_total)
B <- B_total[1:num_basis,1:length(X)]
B_predict <- B_total[1:num_basis,seq(length(X)+1,length(X)+28)]
num_data <- length(X);

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
sm<-stan_model("spline.stan")
fit<-sampling(sm,iter=100,chains=1,data=list(y_deaths=pmax(0,ca_deaths$value),
                                    y_cases=pmax(0,ca_cases$value),
                                    n=nrow(ca_deaths),
                                    day_of_week = rep(1:7,length.out=nrow(ca_deaths)),
                                    mask=mask,
                                    num_data=num_data,
                                    num_basis=num_basis,
                                    X=X,
                                    X_new=X_new,
                                    B=B,
                                    B_predict=B_predict,
                                    mask_pred=mask_w_pred),
              control = list(adapt_delta=0.95))

fit_yhat <- extract(fit,pars=c(),
                    permuted = TRUE, include = FALSE)






plot(colMeans(fit_yhat$predicted_death),type='l')
points(log(ca_deaths$value+1),cex=.2,col='red')

plot(exp(colMeans(fit_yhat$predicted_death)),type='l')
points(ca_deaths$value,cex=.2,col='red')


# point forecast

point_fcast <- tail(exp(colMeans(fit_yhat$predicted_death)),28)
validation_data <- covidcast_signal(data_source = "jhu-csse",
                                    signal ="deaths_incidence_num",
                                    start_day = as.Date(timezero_day)+1, end_day = forecast_day,
                                    geo_type = "state", geo_values =region)

library(zoo)

truth <- rollapply(validation_data$value, 7, sum, by = 7)
point_fcast_weekly <- rollapply(point_fcast, 7, sum, by = 7)
mean(abs(truth-point_fcast_weekly))


plot(exp(colMeans(fit_yhat$predicted_death)),type='l',ylim=c(0,max(c(ca_deaths$value,validation_data$value))))
points(c(ca_deaths$value,validation_data$value),cex=.2,col='blue')


