library("splines")
library("rstan")
library(covidcast)
ca_deaths <- covidcast_signal(data_source = "jhu-csse",
                              signal ="deaths_incidence_num",
                             start_day = "2020-03-15", end_day = "2021-01-03",
                              geo_type = "state", geo_values = "ma")


#write.csv(ca_deaths,"ca_deaths.csv")

ca_cases <- covidcast_signal(data_source = "jhu-csse",
                             signal ="confirmed_incidence_num",
                             start_day = "2020-03-15", end_day = "2021-01-03",
                             geo_type = "state", geo_values = "ma")


#write.csv(ca_cases,"ca_cases.csv")


num_data <- nrow(ca_cases)
lambda_ = 21
mask <- matrix(0,nrow=num_data,ncol=num_data)
for (i in 1:num_data){
  tmp=rev(dexp(seq(0,num_data-1)[1:(i)],21))
  mask[i,1:(i)]=tmp
}

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
sm<-stan_model("llm.stan")
fit<-sampling(sm,iter=100,data=list(y_deaths=log(pmax(0,ca_deaths$value)+1),
                                    y_cases=log(pmax(0,ca_cases$value)+1),
                                    n=nrow(ca_deaths),
                                    day_of_week = rep(1:7,length.out=nrow(ca_deaths)),
                                    mask=mask),
                     control = list(adapt_delta=0.95))

fit_yhat <- extract(fit,pars=c(),
        permuted = TRUE, include = FALSE)


plot(c(colMeans(fit_yhat$mu_deaths),colMeans(fit_yhat$y_hat_pred)),type='l')
points(log(pmax(0,ca_deaths$value)+1),col='red',cex=.3)


plot(exp(c(colMeans(fit_yhat$mu_deaths),colMeans(fit_yhat$y_hat_pred))),type='l')
points(ca_deaths$value,col='red',cex=.3)

plot(c(colMeans(fit_yhat$mu_cases)),type='l')
points(log(pmax(0,ca_cases$value)+1),col='red',cex=.3)

plot(log(pmax(0,ca_cases$value)+1),col='red',cex=.3)
lines(c(colMeans(fit_yhat$mu_cases)),type='l')

