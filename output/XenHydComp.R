library(tidyverse)
library(suncalc)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

latitude <- 51.05
longitude <- 3.72

folders <- c("onbekend_akkerpoel","onbekend_annelies","onbekend_eeuwenhout","onbekend_vistrap") # Replace with your folder paths

final_list <- NULL

for (folder in folders) {
  files <- list.files(paste0(folder,"/likely/"), full.names = TRUE)
  if (length(files) == 0) {
    next  # Skip to the next iteration
  }
  folder_df <- data.frame(poel = folder, file_name = basename(files), stringsAsFactors = FALSE)
  final_list[[folder]] <- folder_df
}

final_df <- bind_rows(final_list) %>% 
  mutate(date = format(as.Date(substr(file_name, 1, 8), format = "%Y%m%d"), "%d/%m/%Y"), 
         date = as.Date(date, format = "%d/%m/%Y"),
         time_old = format(strptime(substr(file_name, 10, 15), format = "%H%M%S"), "%H:%M"), 
         time = as.POSIXct(paste(date, time_old), format = "%Y-%m-%d %H:%M"),
         date_real = date, 
         date_fix = if_else(format(time, "%H") < "12", date - 1, date)) %>% 
  distinct(date,date_real, date_fix, time_old, time, poel)

datasun <- getSunlightTimes(date = final_df$date_real, lat = latitude, lon = longitude, keep = c("sunrise","sunset")) %>% 
  distinct(date, lat, lon, sunrise, sunset)

final_df <- final_df %>% 
  left_join(datasun) %>% 
  mutate(day_night = ifelse(time > sunrise & time < sunset, "day","night"))

final_df %>% 
  ggplot(aes(date_fix, fill = day_night)) + geom_bar() + facet_wrap(~poel,ncol=1,scales="free_y")
save(final_df, file = "my_dataframe.rda")
ggsave(paste0("Hydromoth_daysum_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".jpg"))
  
final_df %>%
  mutate(unidate = as.Date(ifelse(date_real == date_fix, "2025-01-01", "2025-01-02")), 
         time_comp = as.POSIXct(paste(unidate, time_old), format = "%Y-%m-%d %H:%M"),
         month = month(date_fix)) %>% 
  ggplot(aes(time_comp,fill=day_night)) + geom_bar() + facet_grid(month~poel)
ggsave(paste0("Hydromoth_night_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".jpg"))

final_df %>%
  mutate(unidate = as.Date(ifelse(date_real == date_fix, "2025-01-01", "2025-01-02")), 
         time_comp = as.POSIXct(paste(unidate, time_old), format = "%Y-%m-%d %H:%M"),
         time_comp_hourly = floor_date(time_comp, unit = "hour"),  # Round down to the nearest hour
         month = month(date_fix)) %>% 
  distinct(date_real, time_comp_hourly, day_night, month, poel) %>% 
  ggplot(aes(time_comp_hourly,fill=day_night)) + geom_bar() + facet_grid(month~poel)
ggsave(paste0("Hydromoth_night_2_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".jpg"))
