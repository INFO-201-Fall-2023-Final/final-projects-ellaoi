

library(dplyr)
library(stringr)

#Loading in datasets 
overall_ob <- read.csv("2022-overall-prevalence.csv")
youth_ob <- read.csv("Nutrition__Physical_Activity__and_Obesity_-_Youth_Risk_Behavior_Surveillance_System.csv")

#joining the datasets

colnames(youth_ob)[colnames(youth_ob) == "LocationDesc"] <- "State"

ob <- left_join(youth_ob, overall_ob, by = "State")

dg
