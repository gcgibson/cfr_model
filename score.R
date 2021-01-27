library(covidHubUtils)
library(dplyr)
obs <- covidHubUtils::load_truth("JHU", "inc death") %>%
  dplyr::rename(true_value = value) %>%
  dplyr::select(-model)
dates <- seq(as.Date("2020-05-11"),as.Date("2020-10-12"),by="week")

fcast_df <- data.frame()#id=length(dates)*52*4,date=NA,horizon=NA,region=NA,target_end_date=NA,stringsAsFactors = FALSE)
files <- list.files("output/")

row_id <- 1
for (f in files){
  if (grepl("deaths",f)){
    tmp <- read.csv(paste0("output/",f))
    for (date_ in as.list(dates)){
      for (horizon in 1:4){
        fcast_row <- tmp[tmp$date == date_ & tmp$h == horizon,] %>% select(-X)
        fcast_row$region <- substr(f,1,2)
        fcast_row$target_end_date <- as.Date(date_ + 7*horizon -1)
        fcast_df<-rbind(fcast_df, fcast_row)
        row_id <- row_id + 1
        
      }
    }
  }
}

fips_csv <- read.csv(url("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-locations/locations.csv"))
obs <- obs %>% left_join(fips_csv,by='location')
obs$region <- tolower(obs$abbreviation)
obs <- obs[nchar(obs$region)==2,]
fcast_df_w_truth <- fcast_df %>% left_join(obs[obs$target_variable == "inc death",],by=c("target_end_date","region"))

