
#this is a script to look at Moberly MOE water quality data over the series of time
# from 1964 to 2020. Unfortunately rems requires download of ALL ems data, so
# this can take a while and only needs to be updated periodically.

#To investigate: trends in TP, turbidity, and secchi

# EMS ID: E207907


#library(devtools)
#install_github("bcgov/rems")
library(rems)

download_historic_data(ask = FALSE)
two_year <- get_ems_data(which = "2yr", ask = FALSE)
filtered_2yr <- filter_ems_data(two_year, emsid = c("E207907"))

library(dplyr)
hist_db_con <- connect_historic_db()

hist_tbl <- attach_historic_data(hist_db_con)

filtered_historic2 <- hist_tbl %>%
  select(EMS_ID, PARAMETER, COLLECTION_START, RESULT) %>%
  filter(EMS_ID %in% c("E207907")) 

filtered_historic2 <- collect(filtered_historic2) %>%
  mutate(COLLECTION_START = ems_posix_numeric(COLLECTION_START))
range(filtered_historic2$COLLECTION_START)


all_data <- bind_ems_data(filtered_2yr, filtered_historic2)

range(all_data$COLLECTION_START)
  
  
disconnect_historic_db(hist_db_con)


#not can start visualizing data
head(all_data)
unique(all_data$PARAMETER)
unique(all_data$UNIT)

of.interest <- c("Turbidity-Field","Specific Conductivity-Field","Temperature-Field",
                 "Nitrogen Total","Turbidity","Extinction Depth","Phosphorus Total",
                 "E Coli")
#note: extinction depth is Secchi disc depth

subset <- all_data %>% 
  filter(PARAMETER %in% of.interest) 

unique(paste(subset$PARAMETER,subset$UNIT))


library(ggplot2)

ggplot(data=subset)+
  geom_point(aes(x=COLLECTION_START, y=RESULT))+
  facet_wrap(~PARAMETER,scales="free")+
  labs(x="Date",y="Value")





