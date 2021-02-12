library("splines")
library("rstan")
library(covidcast)

start_day <- "2020-03-15"
timezero_day <-  "2021-01-01"
forecast_day <-  as.Date(timezero_day) + 28
region <- "tx"

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


# FIGURE 1
library(ggplot2)
ca_cases$type <- "cases"
ca_deaths$type <- "deaths"

ca_total <- rbind(ca_cases,ca_deaths)

p1 <- ggplot(ca_cases[ca_cases$time_value >= "2020-03-15",],aes(x=time_value,y=log(Lag(pmax(0,value)+1,8)),col="Cases"))+ geom_line() +
  geom_line(data=ca_deaths[ca_deaths$time_value >= "2020-03-15",],aes(x=time_value,y=log(Lag(pmax(0,value)+1,8)),col="Deaths")) + xlab("Date") + ylab("Log") + theme_bw()




start_day <- "2020-03-15"
timezero_day <-  "2021-01-01"
forecast_day <-  as.Date(timezero_day) + 28
region <- "ny"

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


# FIGURE 1
library(ggplot2)
ca_cases$type <- "cases"
ca_deaths$type <- "deaths"

ca_total <- rbind(ca_cases,ca_deaths)

p2 <- ggplot(ca_cases[ca_cases$time_value >= "2020-03-15",],aes(x=time_value,y=log(Lag(pmax(0,value)+1,8)),col="Cases"))+ geom_line() +
  geom_line(data=ca_deaths[ca_deaths$time_value >= "2020-03-15",],aes(x=time_value,y=log(Lag(pmax(0,value)+1,8)),col="Deaths")) + xlab("Date") + ylab("Log") + theme_bw()


#################
start_day <- "2020-03-15"
timezero_day <-  "2021-01-01"
forecast_day <-  as.Date(timezero_day) + 28
region <- "fl"

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


# FIGURE 1
library(ggplot2)
ca_cases$type <- "cases"
ca_deaths$type <- "deaths"

ca_total <- rbind(ca_cases,ca_deaths)

p3 <- ggplot(ca_cases[ca_cases$time_value >= "2020-03-15",],aes(x=time_value,y=log(Lag(pmax(0,value)+1,8)),col="Cases"))+ geom_line() +
  geom_line(data=ca_deaths[ca_deaths$time_value >= "2020-03-15",],aes(x=time_value,y=log(Lag(pmax(0,value)+1,8)),col="Deaths")) + xlab("Date") + ylab("Log") + theme_bw()


###################
start_day <- "2020-03-15"
timezero_day <-  "2021-01-01"
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


# FIGURE 1
library(ggplot2)
ca_cases$type <- "cases"
ca_deaths$type <- "deaths"

ca_total <- rbind(ca_cases,ca_deaths)

p4<-ggplot(ca_cases[ca_cases$time_value >= "2020-03-15",],aes(x=time_value,y=log(Lag(pmax(0,value)+1,8)),col="Cases"))+ geom_line() +
  geom_line(data=ca_deaths[ca_deaths$time_value >= "2020-03-15",],aes(x=time_value,y=log(Lag(pmax(0,value)+1,8)),col="Deaths")) + xlab("Date") + ylab("Log") + theme_bw()

library(cowplot)

fig1 <- cowplot::plot_grid(p1+ theme(legend.position =  "none"),p2+ theme(legend.position =  "none"),p3+ theme(legend.position =  "none"),p4+ theme(legend.position =  "none"),labels=c("NY","CA","TX","FL"),ncol=2,align='v')
ggsave(fig1)
