

library(dplyr)
library(stringr)

#Loading in datasets 
youth_obesity <- read.csv("Nutrition_Physical_Activity.csv")
national_rates <- read.csv("National_Obesity_By_State.csv")

#joining the datasets
colnames(youth_obesity)[colnames(youth_obesity) == "LocationDesc"] <- "State"
colnames(national_rates)[colnames(national_rates) == "NAME"] <- "State"

obesity_rates_youth <- left_join(youth_obesity, national_rates, by = "State")

#data cleaning
obesity_rates_youth <- obesity_rates_youth[obesity_rates_youth$YearStart == 2015, ]
obesity_rates_youth <- obesity_rates_youth[obesity_rates_youth$YearEnd == 2015, ]