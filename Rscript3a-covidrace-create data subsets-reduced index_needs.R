library(tidyverse)
#consider changing gather to pivot_longer

#cleaned and indexed data
covid <- read.table('data-covidrace-v3-indexed-reduced index_needs.csv',header=TRUE, stringsAsFactors = TRUE, sep=",", dec=".")
covid.reduced <- covid[!is.na(covid$Index_Needs),]
rm(covid)

#Food Changes
#create dataset
foodchanges <- dplyr::select(covid.reduced, US_Race, MinorityEthnic_Race_Status, MinorityEthnic_Race_Status_Binary,
                             Gender, Sexual_Orientation, Index_SES, Index_Needs, Index_ST_Loss,
                             SmellTasteLoss_Cause_General, SmellTasteLoss_Cause_VirusSpecific,
                             Index_Olfactory_Change, Index_Olfactory_Recovery, Index_Olfactory_Long_Haul,
                             Rate_BeerAle, Rate_Broccoli, Rate_BrusselSprouts, Rate_Cabbage, Rate_Cauliflower,
                             Rate_Coffee_NoMilk, Rate_TartApple, Rate_BeerLager, Rate_Lettuce, Rate_RedWine,
                             Rate_SweetApple, Rate_Spinach, Rate_BlackTea_NoMilk, Rate_WhiteWine,
                             Rate_BeerAle_Number, Rate_Broccoli_Number, Rate_BrusselSprouts_Number,
                             Rate_Cabbage_Number, Rate_Cauliflower_Number,
                             Rate_Coffee_NoMilk_Number, Rate_TartApple_Number, Rate_BeerLager_Number,
                             Rate_Lettuce_Number, Rate_RedWine_Number,
                             Rate_SweetApple_Number, Rate_Spinach_Number, Rate_BlackTea_NoMilk_Number, Rate_WhiteWine_Number)

foodchanges.stloss <- subset(foodchanges, Index_ST_Loss=='ST_Loss')
foodchanges.stloss <-dplyr::select(foodchanges.stloss, -Index_ST_Loss)
#write data
write.csv(foodchanges.stloss, "data-covidrace-foodchanges-WIDE-reduced index_needs.csv", quote=FALSE, row.names = FALSE)

#food-long format data
#gather into one column all ratings labelled by food
foodchanges.long <- foodchanges.stloss %>%
    gather(key=Food, value=FoodRating, -US_Race, -MinorityEthnic_Race_Status, -MinorityEthnic_Race_Status_Binary,
           -Gender, -Sexual_Orientation, -Index_SES, -Index_Needs,
           -SmellTasteLoss_Cause_General, -SmellTasteLoss_Cause_VirusSpecific,
           -Index_Olfactory_Change, -Index_Olfactory_Recovery, -Index_Olfactory_Long_Haul)
foodchanges.long <-foodchanges.stloss %>%
    pivot_longer(cols = ends_with("Number"), names_to = 'Food', values_to = "FoodRating")
#write data
write.csv(foodchanges.long, "data-covidrace-foodchanges-LONG-reduced index_needs.csv", quote=FALSE, row.names = FALSE)
rm(foodchanges.long)

#Household
#create dataset
household <- dplyr::select(covid.reduced, US_Race, MinorityEthnic_Race_Status,
                    Gender, Sexual_Orientation,
                    Index_SES, Index_Needs,ZipCode,
                    Index_Household, Index_Environment,
                    SmellTasteLoss_Cause_VirusSpecific)
#write data
write.csv(household, "data-covidrace-household-reduced index_needs.csv", quote=FALSE, row.names = FALSE)
rm(household)

#Dry Mouth
#create dataset
drymouth <- dplyr::select(covid.reduced, US_Race, MinorityEthnic_Race_Status,
                   Gender, Sexual_Orientation,
                   Index_Needs, Index_SES,
                   Index_ST_Loss_Cause, Index_ST_Loss_Cause_Number,
                   Index_ST_Loss_Cause_PooledCovid, Index_ST_Loss_Cause_PooledCovid_Number,
                   Index_ST_Loss_Cause_VirusOnly, Index_ST_Loss_Cause_VirusOnly_Number,
                   Oral_Condition_Lickert, Oral_Condition_Number, Symptom_DryMouth)
drymouth <- rename(drymouth, Index_DryMouth = Symptom_DryMouth)
#write data
write.csv(drymouth, "data-covidrace-drymouth-reduced index_needs.csv", quote=FALSE, row.names = FALSE)
rm(drymouth)

#Relative SES
#create dataset
relativeSES <- dplyr::select(covid.reduced, US_Race, MinorityEthnic_Race_Status, Gender, Sexual_Orientation,
                      Have_Income, Compare_Income_Race, Compare_Income_GeneralPopulation,
                      Have_Resources, Compare_Resources_Race, Compare_Resources_GeneralPopulation,
                      Index_SES, Index_Needs)

#write data
write.csv(relativeSES, "data-covidrace-relativeSES-reduced index_needs.csv", quote=FALSE, row.names = FALSE)


#tidy
rm(list = ls())
gc()

