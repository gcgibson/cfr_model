library(rstan)

stan_dat <- read_rdump('gp-fit-data.R')

fit_predict <- stan(file="gp-fit-pois.stan",   
                    data=stan_dat,
                    iter=200, chains=3);
print(fit_predict, pars = c('rho','alpha','sigma'))
fit_yhat <- extract(fit_predict,pars=c(),
                    permuted = TRUE, include = FALSE)$f

plot(colMeans(fit_yhat))
#points(stan_dat$y1)
plot(stan_dat$x1,stan_dat$y1)
