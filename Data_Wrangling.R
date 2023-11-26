

library(dplyr)
library(stringr)

#Loading in datasets 
youth_obesity <- read.csv("Nutrition_Physical_Activity.csv")
national_rates <- read.csv("National_Obesity_By_State.csv")

#renaming the column in common to be the same
colnames(youth_obesity)[colnames(youth_obesity) == "LocationDesc"] <- "State"
colnames(national_rates)[colnames(national_rates) == "NAME"] <- "State"

#joining the datasets
ob <- left_join(youth_obesity, national_rates, by = "State")

#data cleaning & augmentation
#making it the year they have in common
ob <- ob[ob$YearStart == 2015, ]
ob <- ob[ob$YearEnd == 2015, ]

is.na(ob)
obesity_rates_youth <- na.omit(ob)
obesity_rates_youth <- unique(ob)

obesity_rates_youth <- ob[ob$State != "Guam", ]
obesity_rates_youth <- ob[ob$State != "National", ]

rownames(obesity_rates_youth) <- NULL

#renaming some columns to make the data easier to understand 
colnames(obesity_rates_youth)[colnames(obesity_rates_youth) == "Obesity"] <- "Prevalence_in_the_States"

#adding new columns 

#categorical: According to the CDC if the obesity prevalence is 20 - <25% that is considered low,
#25 - <30% is considered low/medium, 30 - <35% considered medium/high, and 35 - 40% is considered high

prevalence_category <- function(Prevalence_in_the_States){
  if (Prevalence_in_the_States < 30) {
    return("Low")
  } else if(Prevalence_in_the_States >= 30 && Prevalence_in_the_States <= 70){
    return("Medium")
  } else {
    return("High")
  }
}

obesity_rates_youth <- obesity_rates_youth[complete.cases(obesity_rates_youth$Prevalence_in_the_States), ]
obesity_rates_youth$prevalence_scale <- sapply(obesity_rates_youth$Prevalence_in_the_States, prevalence_category)

#continuous: the number of people of did that question

obesity_rates_youth$amount_of_students <- (obesity_rates_youth$Data_Value / 100) * obesity_rates_youth$Sample_Size
obesity_rates_youth$amount_of_students <- as.numeric(obesity_rates_youth$amount_of_students)

#summarization data frame
summary_prevalence <- data.frame(Mean_Prevalence = mean(obesity_rates_youth$Prevalence_in_the_States),
                                                 Max_Prevalence = max(obesity_rates_youth$Prevalence_in_the_States),
                                                 Min_Prevalence = min(obesity_rates_youth$Prevalence_in_the_States),
                                                 Total_States = length(obesity_rates_youth$Prevalence_in_the_States))

print(summary_prevalence)

