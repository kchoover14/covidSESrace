library(tidyverse)

#cleaned data from 'Rscript1-covidrace-clean.R'
covid.index <- read.table('data-covidrace-v2-cleaned.csv', header=TRUE, sep=",", dec=".")

#index_Needs: branching logic did not work and several answer combinations are implausible
#delete income responses if no income
#delete resource responses in no resource
covid.index <- covid.index %>% mutate(Compare_Income_Race=replace(Compare_Income_Race, Have_Income=="No", ""))
covid.index <- covid.index %>% mutate(Compare_Income_GeneralPopulation=replace(Compare_Income_GeneralPopulation, Have_Income=="No", ""))
covid.index <- covid.index %>% mutate(Income_Meets_Needs=replace(Income_Meets_Needs, Have_Income=="No", ""))

covid.index <- covid.index %>% mutate(Compare_Resources_Race=replace(Compare_Resources_Race, Have_Resources=="No", ""))
covid.index <- covid.index %>% mutate(Compare_Resources_GeneralPopulation=replace(Compare_Resources_GeneralPopulation, Have_Resources=="No", ""))
covid.index <- covid.index %>% mutate(Resources_Meets_Needs=replace(Resources_Meets_Needs, Have_Resources=="No", ""))

#create index_needs
covid.index <- covid.index %>% mutate(Index_Needs = case_when(
    Have_Income=="Yes" & Have_Resources=="No" & Income_Meets_Needs=="Yes" ~ 'Needs_Met_Income',
    Have_Income=="No" & Have_Resources=="Yes" & Resources_Meets_Needs=="Yes" ~ 'Needs_Met_Resources',
    Have_Income=="Yes" & Have_Resources=="No" & Income_Meets_Needs=="No" ~ 'Needs_Not_Met',
    Have_Income=="No" & Have_Resources=="Yes" & Resources_Meets_Needs=="No" ~ 'Needs_Not_Met',
    Have_Income=="Yes" & Have_Resources=="Yes" & Income_Meets_Needs=="Yes" & Resources_Meets_Needs=="Yes" ~ 'Needs_Met_IncomeAndResources',
    Have_Income=="Yes" & Have_Resources=="Yes" & Income_Meets_Needs=="No" & Resources_Meets_Needs=="Yes" ~ 'Needs_Met_IncomeAndResources',
    Have_Income=="Yes" & Have_Resources=="Yes" & Income_Meets_Needs=="No" & Resources_Meets_Needs=="No" ~ 'Needs_Not_Met',
    Income_Meets_Needs=="No" & Resources_Meets_Needs=="No" ~ 'Needs_Not_Met',
    Have_Income=="No" & Have_Resources=="No"  ~ 'No_Income_or_Resources'
))

covid.index <- covid.index %>% mutate(Index_Needs_Number = case_when(
    Have_Income=="Yes" & Have_Resources=="No" & Income_Meets_Needs=="Yes" ~ 3,
    Have_Income=="No" & Have_Resources=="Yes" & Resources_Meets_Needs=="Yes" ~ 2,
    Have_Income=="Yes" & Have_Resources=="No" & Income_Meets_Needs=="No" ~ 0,
    Have_Income=="No" & Have_Resources=="Yes" & Resources_Meets_Needs=="No" ~ 0,
    Have_Income=="Yes" & Have_Resources=="Yes" & Income_Meets_Needs=="Yes" & Resources_Meets_Needs=="Yes" ~ 1,
    Have_Income=="Yes" & Have_Resources=="Yes" & Income_Meets_Needs=="No" & Resources_Meets_Needs=="Yes" ~ 1,
    Have_Income=="Yes" & Have_Resources=="Yes" & Income_Meets_Needs=="No" & Resources_Meets_Needs=="No" ~ 0,
    Income_Meets_Needs=="No" & Resources_Meets_Needs=="No" ~ 0
))

#index_SES
#implausible answers so index is ranked by having income and amount of income.
covid.index <- covid.index %>% mutate(Index_SES = case_when(
    Have_Income=="No" ~ 1,
    Income=="Below32" ~ 2,
    Income=="Upto41" ~ 3
))

#Index_Household
covid.index <- covid.index %>% mutate(Index_Household = case_when(
    Location == "Urban" & Generations_per_Household == 1 ~ 'Urban_1to2_Generations',
    Location == "Urban" & Generations_per_Household == 2 ~ 'Urban_1to2_Generations',
    Location == "Urban" & Generations_per_Household > 2 ~ 'Urban_3Plus_Generations',
    Location == "Rural" & Generations_per_Household == 1 ~ 'Rural_1to2_Generations',
    Location == "Rural" & Generations_per_Household == 2 ~ 'Rural_1to2_Generations',
    Location == "Rural" & Generations_per_Household > 2 ~ 'Rural_3Plus_Generations',
    Location == "Urban" & Household_Size == 1 ~ 'Urban_1to2_Generations',
    Location == "Rural" & Household_Size == 1 ~ 'Rural_1to2_Generations',
    Location == "Urban" & Household_Size == 2 ~ 'Urban_1to2_Generations',
    Location == "Rural" & Household_Size == 2 ~ 'Rural_1to2_Generations',
    Location == "Urban" & Household_Size > 2 ~ 'Urban_3Plus_Generations',
    Location == "Rural" & Household_Size > 2 ~ 'Rural_3Plus_Generations',
    Location == "Urban" & Household_Size == 0 ~ 'Urban_1to2_Generations',
    Location == "Rural" & Household_Size == 0 ~ 'Rural_1to2_Generations'
))
covid.index$Index_Household <- as.factor(covid.index$Index_Household)

#Index_Environment
covid.index$Index_Environment = rowSums(covid.index[ , 18:21], na.rm=TRUE)

#Index_ST_Loss_Cause (n=80)
covid.index <- covid.index %>% mutate(Index_ST_Loss_Cause = case_when(
    SmellTasteLoss_Cause_General == "Medication_or_Cancer"  ~ 'NonViral',
    SmellTasteLoss_Cause_General == "Unknown"  ~ 'NonViral',
    SmellTasteLoss_Cause_General == "Other"  ~ 'NonViral',
    SmellTasteLoss_Cause_General == "Occupational"  ~ 'NonViral',
    SmellTasteLoss_Cause_General == "Injury"  ~ 'NonViral',
    SmellTasteLoss_Cause_General == "Congenital"  ~ 'NonViral',
    SmellTasteLoss_Cause_General == "Bacterial"  ~ 'NonViral',
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid' & Diagnosis_Method == "Lab_Test" ~ 'Viral_Covid_confirmed',
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid' & Diagnosis_Method == "Clinical"   ~ 'Viral_Covid_confirmed',
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid'  ~ 'Viral_Covid_unconfirmed',
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Other'  ~ 'Viral_nonCovid',
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'NonCovid_Virus'  ~ 'Viral_nonCovid',
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Flu'  ~ 'Viral_nonCovid'
))
covid.index$Index_ST_Loss_Cause <- as.factor(covid.index$Index_ST_Loss_Cause)

covid.index <- covid.index %>% mutate(Index_ST_Loss_Cause_Number = case_when(
    SmellTasteLoss_Cause_General == "Medication_or_Cancer"  ~ 1,
    SmellTasteLoss_Cause_General == "Unknown"  ~ 1,
    SmellTasteLoss_Cause_General == "Other"  ~ 1,
    SmellTasteLoss_Cause_General == "Occupational"  ~ 1,
    SmellTasteLoss_Cause_General == "Injury"  ~ 1,
    SmellTasteLoss_Cause_General == "Congenital"  ~ 1,
    SmellTasteLoss_Cause_General == "Bacterial"  ~ 1,
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid' & Diagnosis_Method == "Lab_Test" ~ 4,
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid' & Diagnosis_Method == "Clinical"   ~ 4,
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid'  ~ 3,
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Other'  ~ 2,
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'NonCovid_Virus'  ~ 2,
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Flu'  ~ 2
))

#pooled Covid, dx and no dx
covid.index <- covid.index %>% mutate(Index_ST_Loss_Cause_PooledCovid = case_when(
    SmellTasteLoss_Cause_General == "Medication_or_Cancer"  ~ 'NonViral',
    SmellTasteLoss_Cause_General == "Unknown"  ~ 'NonViral',
    SmellTasteLoss_Cause_General == "Other"  ~ 'NonViral',
    SmellTasteLoss_Cause_General == "Occupational"  ~ 'NonViral',
    SmellTasteLoss_Cause_General == "Injury"  ~ 'NonViral',
    SmellTasteLoss_Cause_General == "Congenital"  ~ 'NonViral',
    SmellTasteLoss_Cause_General == "Bacterial"  ~ 'NonViral',
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid' & Diagnosis_Method == "Lab_Test" ~ 'Viral_Covid',
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid' & Diagnosis_Method == "Clinical"   ~ 'Viral_Covid',
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid'  ~ 'Viral_Covid',
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Other'  ~ 'Viral_nonCovid',
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'NonCovid_Virus'  ~ 'Viral_nonCovid',
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Flu'  ~ 'Viral_nonCovid'
))
covid.index <- covid.index %>% mutate(Index_ST_Loss_Cause_PooledCovid_Number = case_when(
    SmellTasteLoss_Cause_General == "Medication_or_Cancer"  ~ 1,
    SmellTasteLoss_Cause_General == "Unknown"  ~ 1,
    SmellTasteLoss_Cause_General == "Other"  ~ 1,
    SmellTasteLoss_Cause_General == "Occupational"  ~ 1,
    SmellTasteLoss_Cause_General == "Injury"  ~ 1,
    SmellTasteLoss_Cause_General == "Congenital"  ~ 1,
    SmellTasteLoss_Cause_General == "Bacterial"  ~ 1,
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid' & Diagnosis_Method == "Lab_Test" ~ 1,
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid' & Diagnosis_Method == "Clinical"   ~ 1,
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid'  ~ 1,
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Other'  ~ 2,
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'NonCovid_Virus'  ~ 3,
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Flu'  ~ 3
))

#virus v nonvirus
covid.index <- covid.index %>% mutate(Index_ST_Loss_Cause_VirusOnly = case_when(
    SmellTasteLoss_Cause_General == "Medication_or_Cancer"  ~ 'NonViral',
    SmellTasteLoss_Cause_General == "Unknown"  ~ 'NonViral',
    SmellTasteLoss_Cause_General == "Other"  ~ 'NonViral',
    SmellTasteLoss_Cause_General == "Occupational"  ~ 'NonViral',
    SmellTasteLoss_Cause_General == "Injury"  ~ 'NonViral',
    SmellTasteLoss_Cause_General == "Congenital"  ~ 'NonViral',
    SmellTasteLoss_Cause_General == "Bacterial"  ~ 'NonViral',
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid' & Diagnosis_Method == "Lab_Test" ~ 'Viral',
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid' & Diagnosis_Method == "Clinical"   ~ 'Viral',
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid'  ~ 'Viral',
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Other'  ~ 'Viral',
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'NonCovid_Virus'  ~ 'Viral',
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Flu'  ~ 'Viral'
))

covid.index <- covid.index %>% mutate(Index_ST_Loss_Cause_VirusOnly_Number = case_when(
    SmellTasteLoss_Cause_General == "Medication_or_Cancer"  ~ 1,
    SmellTasteLoss_Cause_General == "Unknown"  ~ 1,
    SmellTasteLoss_Cause_General == "Other"  ~ 1,
    SmellTasteLoss_Cause_General == "Occupational"  ~ 1,
    SmellTasteLoss_Cause_General == "Injury"  ~ 1,
    SmellTasteLoss_Cause_General == "Congenital"  ~ 1,
    SmellTasteLoss_Cause_General == "Bacterial"  ~ 1,
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid' & Diagnosis_Method == "Lab_Test" ~ 2,
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid' & Diagnosis_Method == "Clinical"   ~ 2,
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid'  ~ 2,
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Other'  ~ 2,
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'NonCovid_Virus'  ~ 2,
    SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Flu'  ~ 2))


#Index_Symptoms
covid.index$Index_Symptoms = rowSums(covid.index[ , 33:50], na.rm=TRUE)

#Index_ST_Loss
covid.index <- covid.index %>% mutate(Index_ST_Loss = case_when(
    Experienced_SmellTasteLoss == "No"  ~ 'No_ST_Loss',
    Experienced_SmellTasteLoss == "Yes"  ~ 'ST_Loss',
    Symptom_ChangesFoodFlavor == 1  ~ 'ST_Loss',
    Symptom_ChangesSmell == 1  ~ 'ST_Loss'
))
covid.index$Index_ST_Loss <- as.factor(covid.index$Index_ST_Loss)

#Index_Odor_Awareness
#Marga

#Change food awareness to number
covid.index <- covid.index %>% mutate(Rate_BeerAle_Number = case_when(
    Rate_BeerAle == 'Liked_less'  ~ -1,
    Rate_BeerAle == 'Unchanged'  ~ 0,
    Rate_BeerAle == 'Liked_more'  ~ 1))
covid.index <- covid.index %>% mutate(Rate_Broccoli_Number = case_when(
    Rate_Broccoli == 'Liked_less'  ~ -1,
    Rate_Broccoli == 'Liked_more'  ~ 0,
    Rate_Broccoli == 'Unchanged'  ~ 1))
covid.index <- covid.index %>% mutate(Rate_BrusselSprouts_Number = case_when(
    Rate_BrusselSprouts == 'Liked_less'  ~ -1,
    Rate_BrusselSprouts == 'Unchanged'  ~ 0,
    Rate_BrusselSprouts == 'Liked_more'  ~ 1))
covid.index <- covid.index %>% mutate(Rate_Cabbage_Number = case_when(
    Rate_Cabbage == 'Liked_less'  ~ -1,
    Rate_Cabbage == 'Unchanged'  ~ 0,
    Rate_Cabbage == 'Liked_more'  ~ 1))
covid.index <- covid.index %>% mutate(Rate_Cauliflower_Number = case_when(
    Rate_Cauliflower == 'Liked_less'  ~ -1,
    Rate_Cauliflower == 'Unchanged'  ~ 0,
    Rate_Cauliflower == 'Liked_more'  ~ 1))
covid.index <- covid.index %>% mutate(Rate_Coffee_NoMilk_Number = case_when(
    Rate_Coffee_NoMilk == 'Liked_less'  ~ -1,
    Rate_Coffee_NoMilk == 'Unchanged'  ~ 0,
    Rate_Coffee_NoMilk == 'Liked_more'  ~ 1))
covid.index <- covid.index %>% mutate(Rate_TartApple_Number = case_when(
    Rate_TartApple == 'Liked_less'  ~ -1,
    Rate_TartApple == 'Unchanged'  ~ 0,
    Rate_TartApple == 'Liked_more'  ~ 1))
covid.index <- covid.index %>% mutate(Rate_BeerLager_Number = case_when(
    Rate_BeerLager == 'Liked_less'  ~ -1,
    Rate_BeerLager == 'Unchanged'  ~ 0,
    Rate_BeerLager == 'Liked_more'  ~ 1))
covid.index <- covid.index %>% mutate(Rate_Lettuce_Number = case_when(
    Rate_Lettuce == 'Liked_less'  ~ -1,
    Rate_Lettuce == 'Unchanged'  ~ 0,
    Rate_Lettuce == 'Liked_more'  ~ 1))
covid.index <- covid.index %>% mutate(Rate_RedWine_Number = case_when(
    Rate_RedWine == 'Liked_less'  ~ -1,
    Rate_RedWine == 'Unchanged'  ~ 0,
    Rate_RedWine == 'Liked_more'  ~ 1))
covid.index <- covid.index %>% mutate(Rate_SweetApple_Number = case_when(
    Rate_SweetApple == 'Liked_less'  ~ -1,
    Rate_SweetApple == 'Unchanged'  ~ 0,
    Rate_SweetApple == 'Liked_more'  ~ 1))
covid.index <- covid.index %>% mutate(Rate_Spinach_Number = case_when(
    Rate_Spinach == 'Liked_less'  ~ -1,
    Rate_Spinach == 'Unchanged'  ~ 0,
    Rate_Spinach == 'Liked_more'  ~ 1))
covid.index <- covid.index %>% mutate(Rate_BlackTea_NoMilk_Number = case_when(
    Rate_BlackTea_NoMilk == 'Liked_less'  ~ -1,
    Rate_BlackTea_NoMilk == 'Unchanged'  ~ 0,
    Rate_BlackTea_NoMilk == 'Liked_more'  ~ 1))
covid.index <- covid.index %>% mutate(Rate_WhiteWine_Number = case_when(
    Rate_WhiteWine == 'Liked_less'  ~ -1,
    Rate_WhiteWine == 'Unchanged'  ~ 0,
    Rate_WhiteWine == 'Liked_more'  ~ 1))

#create new race variable that excludes 'unsure' response for JASP analysis
covid.index <- covid.index %>% mutate(MinorityEthnic_Race_Status_Binary = case_when(
    MinorityEthnic_Race_Status=="Yes" ~ 'Yes',
    MinorityEthnic_Race_Status=="No" ~ 'No'))

#Index_Health
covid.index <- covid.index %>% relocate(Smoke_Vape_Daily, .before = Chronic_Illnesses)
covid.index$Index_Health = rowSums(covid.index[ , 75:79], na.rm=TRUE)

#Index_Olfactory_Change
covid.index$Index_Olfactory_Change = (covid.index$OlfactoryAbility_DuringSmellLoss - covid.index$OlfactoryAbility_BeforeSmellLoss)

#Index_Olfactory_Recovery
covid.index$Index_Olfactory_Recovery = (covid.index$OlfactoryAbility_Current - covid.index$OlfactoryAbility_DuringSmellLoss)

#Index_Olfactory_Long_Haul
covid.index$Index_Olfactory_Long_Haul = (covid.index$OlfactoryAbility_Current - covid.index$OlfactoryAbility_BeforeSmellLoss)

#test for presence of implausible combinations of answers
#reported ST Loss but no change in OA n=41
table(covid.index$Index_ST_Loss, covid.index$Index_Olfactory_Change)
#eliminate 41 individuals that reported ST Loss but did not have any change in OA
bad.oa <- covid.index %>% filter(Index_ST_Loss == "ST_Loss" & Index_Olfactory_Change == 0)
covid.index <- dplyr::anti_join(covid.index, bad.oa)

#reported seeking diagnosis because of st loss but did not report ST Loss; n=0
table(covid.index$Index_ST_Loss, covid.index$Diagnosis_due_to_SmellTasteLoss)

#save data
write.csv(covid.index, "data-covidrace-v3-indexed-modified index_needs.csv", quote=FALSE, row.names = FALSE)

#tidy
rm(list = ls())
gc()

