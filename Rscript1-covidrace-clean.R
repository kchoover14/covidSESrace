library(tidyverse); library(janitor)

#check for masking effects of other programs on tidyverse before running code
#best to either modify code with :: to require package OR
#close all libraries except tidyverse and janitor

#raw data from server
#these data are not shared due to possible PII
#the cleaned data produced by this script are shared
covid.raw <- read.table('data-covidrace-v1-raw.csv', header=TRUE, skip = 1, sep=",", dec=".",
                    stringsAsFactors = TRUE)

#filter QC which is quality control score for completeness and inclusion criteria (below 18, consented)
covid.raw <- filter(covid.raw, gc == 1)

#clean names
covid.raw <- clean_names(covid.raw)

#eliminate meta data columns
covid.raw <- dplyr::select(covid.raw, -c(start_date, end_date, progress, finished,
                                  q_recaptcha_score, q_relevant_id_duplicate,
                                  q_relevant_id_duplicate_score,
                                  q_relevant_id_fraud_score))

#rename long variables
covid.raw <-rename(covid.raw,Survey_Completion_Time = duration_in_seconds )
covid.raw <-rename(covid.raw,Date = recorded_date )
covid.raw <-rename(covid.raw,Quality_Score = gc )
covid.raw <-rename(covid.raw,Age = what_is_your_age )
covid.raw <-rename(covid.raw,Income = what_is_your_yearly_income )
covid.raw <-rename(covid.raw,US_Race = we_are_interested_in_the_interaction_of_income_and_race_in_the_us_on_covid_19_which_do_you_identify_as_please_check_all_that_apply )
covid.raw <-rename(covid.raw,MinorityEthnic_Race_Status = do_you_consider_yourself_to_be_part_of_a_minority_ethnic_or_racial_group_in_the_country_where_you_currently_live )
covid.raw <-rename(covid.raw,Have_Income = explanation_of_household_income_from_any_member_contributing_to_household_maintenance_household_income_is_receiving_money_in_exchange_for_labor_or_services_from_the_sale_of_goods_or_property_or_as_profit_from_financial_investments_income_does_not_include_government_assistance_for_example_disability_unemployment_welfare_example_annual_salary_with_a_renewal_contract_short_term_contract_work_gig_work_part_time_employment_outside_the_home_selling_goods_at_markets_or_online_markets_do_you_or_your_household_have_an_income )
covid.raw <-rename(covid.raw,Compare_Income_Race = how_does_your_household_income_compare_to_others_in_your_ethnicity_or_race_group_in_the_country_where_you_currently_live )
covid.raw <-rename(covid.raw,Compare_Income_GeneralPopulation = how_does_your_household_income_compare_to_the_general_population_regardless_of_race_or_ethnicity_group_in_the_country_where_you_currently_live )
covid.raw <-rename(covid.raw,Income_Meets_Needs = does_your_household_income_meet_your_needs )
covid.raw <-rename(covid.raw,Have_Resources = explanation_of_household_resources_apart_from_income_your_household_may_have_other_resources_available_household_resources_are_the_things_needed_for_daily_life_like_food_toilet_paper_cleaning_supplies_shelter_that_are_acquired_by_means_other_than_income_such_as_government_assistance_unemployment_welfare_disability_other_or_social_community_assistance_e_g_food_banks_meals_delivered_to_home_through_charities_charitable_donations_do_you_have_other_household_resources_available )
covid.raw <-rename(covid.raw,Compare_Resources_Race = how_do_the_household_resources_available_to_you_compare_to_others_in_your_ethnicity_or_race_group_in_the_country_where_you_currently_live )
covid.raw <-rename(covid.raw,Compare_Resources_GeneralPopulation = how_do_the_household_resources_available_to_you_compare_to_the_general_population_regardless_of_race_or_ethnicity_group_in_the_country_where_you_currently_live )
covid.raw <-rename(covid.raw,Resources_Meets_Needs = do_the_household_resources_available_to_you_meet_your_needs )
covid.raw <-rename(covid.raw,Household_Size = how_many_persons_are_part_of_your_household )
covid.raw <-rename(covid.raw,MultiGeneration_Household = do_you_live_in_a_household_with_more_than_two_generations_more_than_the_youngest_generation_and_their_parents )
covid.raw <-rename(covid.raw,Generations_per_Household = how_many_generations_live_with_you )
covid.raw <-rename(covid.raw,Exposure_Chemicals = in_your_occupation_or_home_environment_are_you_exposed_to_any_of_the_following_on_a_regular_basis_most_days_in_a_week_check_all_that_apply_chemicals_e_g_cleaning_solvents_sanitation_hair_salon_pesticides_chemical_plant_manufacturing_processing_treatment_facility )
covid.raw <-rename(covid.raw,Exposure_AirPollution = in_your_occupation_or_home_environment_are_you_exposed_to_any_of_the_following_on_a_regular_basis_most_days_in_a_week_check_all_that_apply_air_pollution_e_g_outdoor_work_in_heavy_traffic_like_sanitation_or_gardening_mining_manufacturing_heavy_traffic_fire_fighting )
covid.raw <-rename(covid.raw,Exposure_Tobacco = in_your_occupation_or_home_environment_are_you_exposed_to_any_of_the_following_on_a_regular_basis_most_days_in_a_week_check_all_that_apply_tobacco_use_e_g_working_in_a_place_where_smoking_is_allowed_and_common )
covid.raw <-rename(covid.raw,Exposure_PoorVentilation = in_your_occupation_or_home_environment_are_you_exposed_to_any_of_the_following_on_a_regular_basis_most_days_in_a_week_check_all_that_apply_poor_ventilation_e_g_closed_interior_environment_poor_air_circulation )
covid.raw <-rename(covid.raw,Exposure_NotApplicable = in_your_occupation_or_home_environment_are_you_exposed_to_any_of_the_following_on_a_regular_basis_most_days_in_a_week_check_all_that_apply_not_applicable )
covid.raw <-rename(covid.raw,Location = where_do_you_live )
covid.raw <-rename(covid.raw,Live_in_USA = do_you_live_in_the_united_states_of_america )
covid.raw <-rename(covid.raw,ZipCode = please_enter_your_zipcode_5_digits )
covid.raw <-rename(covid.raw,Gender = what_is_your_gender )
covid.raw <-rename(covid.raw,Sexual_Orientation = what_is_your_sexual_orientation_how_you_feel_even_if_you_are_not_sexually_active )
covid.raw <-rename(covid.raw,Experienced_SmellTasteLoss = have_you_ever_experienced_loss_of_smell_or_taste_or_do_you_currently_have_an_impaired_sense_of_smell_or_taste )
covid.raw <-rename(covid.raw,Time_Since_SmellTasteLoss = how_long_ago_was_your_most_recent_smell_or_taste_loss )
covid.raw <-rename(covid.raw,SmellTasteLoss_Cause_General = what_is_the_cause_of_your_most_recent_smell_or_taste_loss )
covid.raw <-rename(covid.raw,SmellTasteLoss_Cause_VirusSpecific = what_viral_illness_caused_your_most_recent_smell_or_taste_loss )
covid.raw <-rename(covid.raw,Diagnosis_Method = how_was_that_viral_illness_diagnosed )
covid.raw <-rename(covid.raw,Symptom_Fever = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_fever )
covid.raw <-rename(covid.raw,Symptom_DryCough = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_dry_cough )
covid.raw <-rename(covid.raw,Symptom_DryMouth = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_dry_mouth )
covid.raw <-rename(covid.raw,Symptom_CoughMucus = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_cough_with_mucus )
covid.raw <-rename(covid.raw,Symptom_DifficultyBreathing = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_difficulty_breathing_shortness_of_breat )
covid.raw <-rename(covid.raw,Symptom_ChestTightness = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_chest_tightness )
covid.raw <-rename(covid.raw,Symptom_RunnyNose = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_runny_nose )
covid.raw <-rename(covid.raw,Symptom_SoreThroat = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_sore_throat )
covid.raw <-rename(covid.raw,Symptom_ChangesFoodFlavor = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_changes_in_food_flavor )
covid.raw <-rename(covid.raw,Symptom_ChangesSmell = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_changes_in_smell )
covid.raw <-rename(covid.raw,Symptom_LostAppetite = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_loss_of_appetite )
covid.raw <-rename(covid.raw,Symptom_Headache = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_headache )
covid.raw <-rename(covid.raw,Symptom_Muscleache = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_muscle_aches )
covid.raw <-rename(covid.raw,Symptom_Fatigue = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_fatigue )
covid.raw <-rename(covid.raw,Symptom_Diarrhea = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_diarrhea )
covid.raw <-rename(covid.raw,Symptom_AbdominalPain = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_abdominal_pain )
covid.raw <-rename(covid.raw,Symptom_Nausea = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_nausea )
covid.raw <-rename(covid.raw,Symptom_OtherSymptoms = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_other_symptoms )
covid.raw <-rename(covid.raw,Symptom_NoSymptoms = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_no_symptoms )
covid.raw <-rename(covid.raw,Diagnosis_due_to_SmellTasteLoss = was_a_change_in_your_sense_of_smell_or_taste_a_factor_in_your_decision_to_seek_a_diagnosis_for_your_recent_viral_illness )
covid.raw <-rename(covid.raw,OlfactoryAbility_BeforeSmellLoss = rate_from_0_10_your_ability_to_smell_before_your_most_recent_smell_or_taste_loss )
covid.raw <-rename(covid.raw,OlfactoryAbility_DuringSmellLoss = rate_from_0_10_your_ability_to_smell_during_your_most_recent_smell_or_taste_loss_when_your_loss_was_at_its_worst )
covid.raw <-rename(covid.raw,OlfactoryAbility_Current = rate_from_0_10_your_current_ability_to_smell )
covid.raw <-rename(covid.raw,Rate_BeerAle = please_read_the_following_list_of_food_items_please_indicate_for_each_item_whether_you_like_it_more_or_less_after_recovery_from_your_smell_or_taste_loss_compared_to_before_loss_if_your_liking_is_unchanged_check_unchanged_for_any_foods_you_don_t_know_or_don_t_remember_ever_having_tried_please_select_a_oenot_applicablea_ale_ipa )
covid.raw <-rename(covid.raw,Rate_Broccoli = please_read_the_following_list_of_food_items_please_indicate_for_each_item_whether_you_like_it_more_or_less_after_recovery_from_your_smell_or_taste_loss_compared_to_before_loss_if_your_liking_is_unchanged_check_unchanged_for_any_foods_you_don_t_know_or_don_t_remember_ever_having_tried_please_select_a_oenot_applicablea_brocolli )
covid.raw <-rename(covid.raw,Rate_BrusselSprouts = please_read_the_following_list_of_food_items_please_indicate_for_each_item_whether_you_like_it_more_or_less_after_recovery_from_your_smell_or_taste_loss_compared_to_before_loss_if_your_liking_is_unchanged_check_unchanged_for_any_foods_you_don_t_know_or_don_t_remember_ever_having_tried_please_select_a_oenot_applicablea_brussel_sprouts )
covid.raw <-rename(covid.raw,Rate_Cabbage = please_read_the_following_list_of_food_items_please_indicate_for_each_item_whether_you_like_it_more_or_less_after_recovery_from_your_smell_or_taste_loss_compared_to_before_loss_if_your_liking_is_unchanged_check_unchanged_for_any_foods_you_don_t_know_or_don_t_remember_ever_having_tried_please_select_a_oenot_applicablea_cabbage )
covid.raw <-rename(covid.raw,Rate_Cauliflower = please_read_the_following_list_of_food_items_please_indicate_for_each_item_whether_you_like_it_more_or_less_after_recovery_from_your_smell_or_taste_loss_compared_to_before_loss_if_your_liking_is_unchanged_check_unchanged_for_any_foods_you_don_t_know_or_don_t_remember_ever_having_tried_please_select_a_oenot_applicablea_cauliflower )
covid.raw <-rename(covid.raw,Rate_Coffee_NoMilk = please_read_the_following_list_of_food_items_please_indicate_for_each_item_whether_you_like_it_more_or_less_after_recovery_from_your_smell_or_taste_loss_compared_to_before_loss_if_your_liking_is_unchanged_check_unchanged_for_any_foods_you_don_t_know_or_don_t_remember_ever_having_tried_please_select_a_oenot_applicablea_coffee_without_milk )
covid.raw <-rename(covid.raw,Rate_TartApple = please_read_the_following_list_of_food_items_please_indicate_for_each_item_whether_you_like_it_more_or_less_after_recovery_from_your_smell_or_taste_loss_compared_to_before_loss_if_your_liking_is_unchanged_check_unchanged_for_any_foods_you_don_t_know_or_don_t_remember_ever_having_tried_please_select_a_oenot_applicablea_tart_apple_e_g_granny_smith )
covid.raw <-rename(covid.raw,Rate_BeerLager = please_read_the_following_list_of_food_items_please_indicate_for_each_item_whether_you_like_it_more_or_less_after_recovery_from_your_smell_or_taste_loss_compared_to_before_loss_if_your_liking_is_unchanged_check_unchanged_for_any_foods_you_don_t_know_or_don_t_remember_ever_having_tried_please_select_a_oenot_applicablea_regular_beer_lager )
covid.raw <-rename(covid.raw,Rate_Lettuce = please_read_the_following_list_of_food_items_please_indicate_for_each_item_whether_you_like_it_more_or_less_after_recovery_from_your_smell_or_taste_loss_compared_to_before_loss_if_your_liking_is_unchanged_check_unchanged_for_any_foods_you_don_t_know_or_don_t_remember_ever_having_tried_please_select_a_oenot_applicablea_lettuce_e_g_romaine_iceberg_or_butter )
covid.raw <-rename(covid.raw,Rate_RedWine = please_read_the_following_list_of_food_items_please_indicate_for_each_item_whether_you_like_it_more_or_less_after_recovery_from_your_smell_or_taste_loss_compared_to_before_loss_if_your_liking_is_unchanged_check_unchanged_for_any_foods_you_don_t_know_or_don_t_remember_ever_having_tried_please_select_a_oenot_applicablea_red_wine )
covid.raw <-rename(covid.raw,Rate_SweetApple = please_read_the_following_list_of_food_items_please_indicate_for_each_item_whether_you_like_it_more_or_less_after_recovery_from_your_smell_or_taste_loss_compared_to_before_loss_if_your_liking_is_unchanged_check_unchanged_for_any_foods_you_don_t_know_or_don_t_remember_ever_having_tried_please_select_a_oenot_applicablea_sweet_apple_e_g_golden_delicious )
covid.raw <-rename(covid.raw,Rate_Spinach = please_read_the_following_list_of_food_items_please_indicate_for_each_item_whether_you_like_it_more_or_less_after_recovery_from_your_smell_or_taste_loss_compared_to_before_loss_if_your_liking_is_unchanged_check_unchanged_for_any_foods_you_don_t_know_or_don_t_remember_ever_having_tried_please_select_a_oenot_applicablea_spinach )
covid.raw <-rename(covid.raw,Rate_BlackTea_NoMilk = please_read_the_following_list_of_food_items_please_indicate_for_each_item_whether_you_like_it_more_or_less_after_recovery_from_your_smell_or_taste_loss_compared_to_before_loss_if_your_liking_is_unchanged_check_unchanged_for_any_foods_you_don_t_know_or_don_t_remember_ever_having_tried_please_select_a_oenot_applicablea_black_tea_without_milk )
covid.raw <-rename(covid.raw,Rate_WhiteWine = please_read_the_following_list_of_food_items_please_indicate_for_each_item_whether_you_like_it_more_or_less_after_recovery_from_your_smell_or_taste_loss_compared_to_before_loss_if_your_liking_is_unchanged_check_unchanged_for_any_foods_you_don_t_know_or_don_t_remember_ever_having_tried_please_select_a_oenot_applicablea_white_wine )
covid.raw <-rename(covid.raw,Odor_Importance = how_important_are_odors_to_you_in_your_everyday_life )
covid.raw <-rename(covid.raw,Notice_Other_House_Smells = when_you_visit_someone_elsea_s_house_do_you_notice_how_it_smells )
covid.raw <-rename(covid.raw,Odors_Revive_Memories_Emotions = do_odors_revive_memories_or_emotions_in_you )
covid.raw <-rename(covid.raw,Notice_Odors = do_you_notice_the_odors_good_or_bad_around_you )
covid.raw <-rename(covid.raw,Chronic_Illnesses = do_you_have_any_chronic_illnesses )
covid.raw <-rename(covid.raw,Cancer_Treatment = have_you_ever_received_cancer_treatment_chemotherapy_radiation_or_iodine_treatment )
covid.raw <-rename(covid.raw,Daily_Medications = do_you_use_any_daily_medications )
covid.raw <-rename(covid.raw,Smoke_Vape_Daily = do_you_smoke_or_vape_daily )
covid.raw <-rename(covid.raw,Smoke_Traditional = which_kind_of_products_do_you_smoke_or_vape_check_all_that_apply_traditional_tobacco_products_like_cigarettes_cigars_and_pipes )
covid.raw <-rename(covid.raw,Smoke_Vape = which_kind_of_products_do_you_smoke_or_vape_check_all_that_apply_e_cigarettes_and_other_vaping_devices )
covid.raw <-rename(covid.raw,Oral_Condition_Lickert = how_would_you_describe_the_overall_condition_of_your_teeth_dentures_or_gums )
covid.raw <-rename(covid.raw,Informed_Consent = global_consortium_for_chemosensory_science_nds004_study_covid_19_race_and_socio_economic_status_in_collaboration_with_the_global_consortium_for_chemosensory_research_we_are_interested_in_researching_the_effects_of_socio_economic_status_and_race_on_taste_and_smell_loss_research_projects_are_reviewed_by_an_ethics_panel_to_protect_the_rights_and_welfare_of_the_people_involved_in_the_research_this_study_has_been_reviewed_and_approved_by_the_university_of_alaska_fairbanks_institutional_review_board_irb_a_group_that_reviews_research_projects_involving_people_the_letter_of_approval_is_found_here_https_github_com_kchoover14_covid_se_srace_blob_main_irb_20approval_pdf_if_you_have_questions_or_concerns_about_your_rights_as_a_research_participant_or_in_the_event_of_a_research_related_harm_you_can_contact_the_uaf_office_of_research_integrity_at_474_7800_fairbanks_area_or_1_866_876_7800_toll_free_outside_the_fairbanks_area_or_uaf_irb_alaska_edu_sars_co_v2_attaches_to_air_pollution_variation_in_individual_exposure_to_air_pollution_results_in_geographic_differences_in_infection_and_mortality_often_in_alignment_with_socio_economic_status_and_minority_race_loss_of_smell_and_or_taste_in_the_absence_of_other_symptoms_is_a_key_indicator_of_covid_19_infection_but_loss_of_smell_may_not_be_as_noticeable_to_individuals_in_communities_already_suffering_from_pre_existing_impairment_to_the_sense_of_smell_due_to_the_effects_of_high_pollution_exposure_the_purpose_of_the_study_is_to_understand_chemosensory_symptoms_in_covid_19_within_the_context_of_race_minority_status_and_socioeconomic_status_lower_income_we_are_asking_you_to_participate_in_a_20_30_minute_online_survey_time_to_complete_the_survey_will_vary_depending_on_responses_to_questions_the_survey_does_not_impose_any_known_risks_or_benefits_to_participants_your_responses_to_this_survey_will_remain_anonymous_we_are_not_collecting_personal_identifying_information_name_address_email_ip_address_most_but_not_all_questions_are_required_if_a_question_is_required_you_will_be_prompted_to_answer_it_before_proceeding_or_submitting_the_survey_if_you_prefer_not_to_answer_a_required_question_you_cannot_complete_the_survey_if_you_exit_prior_to_submitting_the_survey_your_responses_are_not_submitted_after_you_submit_the_survey_you_cannot_retrieve_your_responses_and_we_cannot_remove_your_data_because_we_have_no_way_to_identify_you_if_you_have_any_questions_please_contact_professor_kara_c_hoover_at_the_university_of_alaska_kchoover_alaska_edu_or_dr_maria_veldhuizen_at_mersin_university_maria_veldhuizen_yale_edu_you_must_be_18_years_of_age_or_older_to_participate_in_this_survey_would_you_like_to_participate_by_clicking_a_yesa_you_confirm_that_you_have_read_the_above_information_are_18_years_of_age_or_older_consent_to_participate_in_the_survey_and_consent_to_sharing_your_responses_anonymously_for_the_purpose_of_a_research_publication_you_will_then_be_directed_to_the_survey )

#convert data and time to posixct class
covid.raw$Date <- as.character(covid.raw$Date)
covid.raw$Date <- as.POSIXct(covid.raw$Date, format="%m/%d/%Y %H:%M", tz="MST")

#clean gender
covid.raw$Gender <- gsub("[[:punct:]]", " ", covid.raw$Gender)
covid.raw$Gender <- as.factor(covid.raw$Gender)
covid.raw <- covid.raw %>% mutate(Gender=str_replace(Gender, "Cis woman  same as assigned at birth ", "CisWoman"))
covid.raw <- covid.raw %>% mutate(Gender=str_replace(Gender, "Cis man  same as assigned at birth ", "CisMan"))
covid.raw <- covid.raw %>% mutate(Gender=str_replace(Gender, "GenderQueer or Non binary  gender non conforming ", "NonBinary"))
covid.raw <- covid.raw %>% mutate(Gender=str_replace(Gender, "Trans man  different from that assigned at birth ", "TransMan"))
covid.raw <- covid.raw %>% mutate(Gender=str_replace(Gender, "Trans woman  different from that assigned at birth ", "TransWoman"))

#clean variable responses
#income
covid.raw$Income <- gsub("[[:punct:]]", " ", covid.raw$Income)
covid.raw <- covid.raw %>% mutate(Income=str_replace(Income, "Less than  32  000", "Below32"))
covid.raw <- covid.raw %>% mutate(Income=str_replace(Income, " 32 000    41  000", "Upto41"))
covid.raw <- covid.raw %>% mutate(Income=str_replace(Income, "More than  41  000", "41K_Plus"))

#daily  medications
covid.raw <- covid.raw %>% mutate(Daily_Medications=str_replace(Daily_Medications, "2 or more", "2_Plus"))

#smoking
covid.raw <- covid.raw %>% mutate(Smoke_Traditional=str_replace(Smoke_Traditional, "Traditional tobacco products, like cigarettes, cigars and pipes", "Yes"))
covid.raw <- covid.raw %>% mutate(Smoke_Vape=str_replace(Smoke_Vape, "E-cigarettes and other vaping devices", "Yes"))

#chronic illnesses
covid.raw <- covid.raw %>% mutate(Chronic_Illnesses=str_replace(Chronic_Illnesses, "2 or more", "2_Plus"))

#Diagnosis Method
covid.raw$Diagnosis_Method <- gsub("[[:punct:]]", " ", covid.raw$Diagnosis_Method)
covid.raw <- covid.raw %>% mutate(Diagnosis_Method=str_replace(Diagnosis_Method, "Lab test  swab or nose throat  saliva or blood test ", "Lab_Test"))
covid.raw <- covid.raw %>% mutate(Diagnosis_Method=str_replace(Diagnosis_Method, "Clinical  interview with medical expert ", "Clinical"))

#Symptoms
covid.raw <- covid.raw %>% mutate(Symptom_Fever=str_replace(Symptom_Fever,"Fever","1"))
covid.raw <- covid.raw %>% mutate(Symptom_DryCough=str_replace(Symptom_DryCough,"Dry cough","1"))
covid.raw <- covid.raw %>% mutate(Symptom_DryMouth=str_replace(Symptom_DryMouth,"Dry mouth","1"))
covid.raw <- covid.raw %>% mutate(Symptom_CoughMucus=str_replace(Symptom_CoughMucus,"Cough with mucus","1"))
covid.raw <- covid.raw %>% mutate(Symptom_DifficultyBreathing=str_replace(Symptom_DifficultyBreathing,"Difficulty breathing/shortness of breat","1"))
covid.raw <- covid.raw %>% mutate(Symptom_ChestTightness=str_replace(Symptom_ChestTightness,"Chest tightness","1"))
covid.raw <- covid.raw %>% mutate(Symptom_RunnyNose=str_replace(Symptom_RunnyNose,"Runny nose","1"))
covid.raw <- covid.raw %>% mutate(Symptom_SoreThroat=str_replace(Symptom_SoreThroat,"Sore throat","1"))
covid.raw <- covid.raw %>% mutate(Symptom_ChangesFoodFlavor=str_replace(Symptom_ChangesFoodFlavor,"Changes in food flavor","1"))
covid.raw <- covid.raw %>% mutate(Symptom_ChangesSmell=str_replace(Symptom_ChangesSmell,"Changes in smell","1"))
covid.raw <- covid.raw %>% mutate(Symptom_LostAppetite=str_replace(Symptom_LostAppetite,"Loss of appetite","1"))
covid.raw <- covid.raw %>% mutate(Symptom_Headache=str_replace(Symptom_Headache,"Headache","1"))
covid.raw <- covid.raw %>% mutate(Symptom_Muscleache=str_replace(Symptom_Muscleache,"Muscle aches","1"))
covid.raw <- covid.raw %>% mutate(Symptom_Fatigue=str_replace(Symptom_Fatigue,"Fatigue","1"))
covid.raw <- covid.raw %>% mutate(Symptom_Diarrhea=str_replace(Symptom_Diarrhea,"Diarrhea","1"))
covid.raw <- covid.raw %>% mutate(Symptom_AbdominalPain=str_replace(Symptom_AbdominalPain,"Abdominal pain","1"))
covid.raw <- covid.raw %>% mutate(Symptom_Nausea=str_replace(Symptom_Nausea,"Nausea","1"))
covid.raw <- covid.raw %>% mutate(Symptom_OtherSymptoms=str_replace(Symptom_OtherSymptoms,"Other symptoms","1"))
covid.raw <- covid.raw %>% mutate(Symptom_NoSymptoms=str_replace(Symptom_NoSymptoms, "No symptoms", "0"))

#SmellTasteLoss_Cause_General
covid.raw$SmellTasteLoss_Cause_General <- gsub("[[:punct:]]", " ", covid.raw$SmellTasteLoss_Cause_General)
covid.raw <- covid.raw %>% mutate(SmellTasteLoss_Cause_General=str_replace(SmellTasteLoss_Cause_General, "Medication use or treatment for cancer", "Medication_or_Cancer"))
covid.raw <- covid.raw %>% mutate(SmellTasteLoss_Cause_General=str_replace(SmellTasteLoss_Cause_General, "Viral illness  like COVID 19  common cold  or flu  influenza ", "Viral"))
covid.raw <- covid.raw %>% mutate(SmellTasteLoss_Cause_General=str_replace(SmellTasteLoss_Cause_General, "I never had a sense of smell  congenital ", "Congenital"))
covid.raw <- covid.raw %>% mutate(SmellTasteLoss_Cause_General=str_replace(SmellTasteLoss_Cause_General, "Head injury or any other injury", "Injury"))
covid.raw <- covid.raw %>% mutate(SmellTasteLoss_Cause_General=str_replace(SmellTasteLoss_Cause_General, "Occupational causes  like exposure to chemicals at work ", "Occupational"))
covid.raw <- covid.raw %>% mutate(SmellTasteLoss_Cause_General=str_replace(SmellTasteLoss_Cause_General, "Bacterial illness  strep throat ", "Bacterial"))

#SmellTasteLoss_Cause_VirusSpecific
covid.raw$SmellTasteLoss_Cause_VirusSpecific <- gsub("[[:punct:]]", " ", covid.raw$SmellTasteLoss_Cause_VirusSpecific)
covid.raw <- covid.raw %>% mutate(SmellTasteLoss_Cause_VirusSpecific=str_replace(SmellTasteLoss_Cause_VirusSpecific, "Flu  influenza ", "Flu"))
covid.raw <- covid.raw %>% mutate(SmellTasteLoss_Cause_VirusSpecific=str_replace(SmellTasteLoss_Cause_VirusSpecific, "COVID 19", "Covid"))
covid.raw <- covid.raw %>% mutate(SmellTasteLoss_Cause_VirusSpecific=str_replace(SmellTasteLoss_Cause_VirusSpecific, "Common cold  rhinoviruses  coronaviruses other than Covid  adenoviruses and enteroviruses ", "NonCovid Virus"))

#time since loss
covid.raw$Time_Since_SmellTasteLoss <- gsub("[[:punct:]]", " ", covid.raw$Time_Since_SmellTasteLoss)
covid.raw <- covid.raw %>% mutate(Time_Since_SmellTasteLoss=str_replace(Time_Since_SmellTasteLoss, "Less than 3 months", "3 months or less"))
covid.raw <- covid.raw %>% mutate(Time_Since_SmellTasteLoss=str_replace(Time_Since_SmellTasteLoss, "3 months   2 years", "3 months to 2 years"))
covid.raw <- covid.raw %>% mutate(Time_Since_SmellTasteLoss=str_replace(Time_Since_SmellTasteLoss, "2 10 years", "2 to 10 years"))
covid.raw <- covid.raw %>% mutate(Time_Since_SmellTasteLoss=str_replace(Time_Since_SmellTasteLoss, "More than 10 years", "10 years or more"))

#Sexual orientation
covid.raw$Sexual_Orientation <- gsub("[[:punct:]]", " ", covid.raw$Sexual_Orientation)
covid.raw <- covid.raw %>% mutate(Sexual_Orientation=str_replace(Sexual_Orientation, "Pansexual  attracted to anyone  not tied to binary gender ", "Pansexual"))
covid.raw <- covid.raw %>% mutate(Sexual_Orientation=str_replace(Sexual_Orientation, "Homosexual  attracted to same sex or gender ", "Homosexual"))
covid.raw <- covid.raw %>% mutate(Sexual_Orientation=str_replace(Sexual_Orientation, "Heterosexual  attracted to opposite sex or gender ", "Heterosexual"))
covid.raw <- covid.raw %>% mutate(Sexual_Orientation=str_replace(Sexual_Orientation, "Bisexual  attracted to men and women ", "Bisexual"))
covid.raw <- covid.raw %>% mutate(Sexual_Orientation=str_replace(Sexual_Orientation, "Asexual  not interested in sex not sexual ", "Asexual"))

#gender
covid.raw <- covid.raw %>% mutate(Gender=str_replace_all(Gender, "Trans man (different from that assigned at birth)", "Trans_Man"))
covid.raw <- covid.raw %>% mutate(Gender=str_replace_all(Gender, "GenderQueer or Non-binary (gender non-conforming)", "GenderQueer"))
covid.raw <- covid.raw %>% mutate(Gender=str_replace_all(Gender, "Cis woman (same as assigned at birth)", "Cis_Woman"))
covid.raw <- covid.raw %>% mutate(Gender=str_replace_all(Gender, "Cis man (same as assigned at birth)", "Cis_Man"))

#US Race
covid.raw$US_Race <- gsub("[[:punct:]]", " ", covid.raw$US_Race)
covid.raw <- covid.raw %>% mutate(US_Race=str_replace(US_Race, "Black or African American", "Black"))

#Exposure
covid.raw$Exposure_Chemicals <- gsub("[[:punct:]]", " ", covid.raw$Exposure_Chemicals)
covid.raw$Exposure_AirPollution <- gsub("[[:punct:]]", " ", covid.raw$Exposure_AirPollution)
covid.raw$Exposure_Tobacco <- gsub("[[:punct:]]", " ", covid.raw$Exposure_Tobacco)
covid.raw$Exposure_PoorVentilation <- gsub("[[:punct:]]", " ", covid.raw$Exposure_PoorVentilation)

covid.raw <- covid.raw %>% mutate(Exposure_Chemicals=str_replace(Exposure_Chemicals,"Chemicals  e g   cleaning solvents sanitation  hair salon  pesticides  chemical plant  manufacturing  processing treatment facility ","1"))
covid.raw <- covid.raw %>% mutate(Exposure_AirPollution=str_replace(Exposure_AirPollution,"Air pollution  e g   outdoor work in heavy traffic like sanitation or gardening  mining  manufacturing  heavy traffic  fire fighting ","1"))
covid.raw <- covid.raw %>% mutate(Exposure_Tobacco=str_replace(Exposure_Tobacco,"Tobacco use  e g   working in a place where smoking is allowed and common ","1"))
covid.raw <- covid.raw %>% mutate(Exposure_PoorVentilation=str_replace(Exposure_PoorVentilation,"Poor ventilation  e g   closed interior environment  poor air circulation ","1"))

#Health
covid.raw <- covid.raw %>% mutate(Chronic_Illnesses=str_replace(Chronic_Illnesses,"None","0"))
covid.raw <- covid.raw %>% mutate(Chronic_Illnesses=str_replace(Chronic_Illnesses,"2_Plus","2"))
covid.raw <- covid.raw %>% mutate(Cancer_Treatment=str_replace(Cancer_Treatment,"Yes","1"))
covid.raw <- covid.raw %>% mutate(Cancer_Treatment=str_replace(Cancer_Treatment,"No","0"))
covid.raw <- covid.raw %>% mutate(Daily_Medications=str_replace(Daily_Medications,"No","0"))
covid.raw <- covid.raw %>% mutate(Daily_Medications=str_replace(Daily_Medications,"2_Plus","2"))
covid.raw <- covid.raw %>% mutate(Smoke_Traditional=str_replace(Smoke_Traditional,"Yes","1"))
covid.raw <- covid.raw %>% mutate(Smoke_Vape=str_replace(Smoke_Vape,"Yes","1"))

#Create oral Condition binary variable
#copy column
covid.raw <- covid.raw %>% mutate(Oral_Condition_Lickert=str_replace_all(Oral_Condition_Lickert, " ", "_"))
covid.raw$Oral_Condition_Number <- covid.raw$Oral_Condition_Lickert
#recode
covid.raw <- covid.raw %>% mutate(Oral_Condition_Number=str_replace(Oral_Condition_Number,"Excellent","2"))
covid.raw <- covid.raw %>% mutate(Oral_Condition_Number=str_replace(Oral_Condition_Number,"Very_good","2"))
covid.raw <- covid.raw %>% mutate(Oral_Condition_Number=str_replace(Oral_Condition_Number,"Good","1"))
covid.raw <- covid.raw %>% mutate(Oral_Condition_Number=str_replace(Oral_Condition_Number,"Fair","0"))
covid.raw <- covid.raw %>% mutate(Oral_Condition_Number=str_replace(Oral_Condition_Number,"Poor","0"))

#Location
covid.raw$Location <- gsub("[[:punct:]]", " ", covid.raw$Location)
covid.raw <- covid.raw %>% mutate(Location=str_replace(Location, "Urban  towns  cities  suburbs ", "Urban"))
covid.raw <- covid.raw %>% mutate(Location=str_replace(Location, "Rural  agricultural  rural  outside towns  cities  suburbs ", "Rural"))

#replace spaces with underscore in columns
covid.raw <- covid.raw %>% mutate(Age=str_replace_all(Age, " ", "_"))
covid.raw <- covid.raw %>% mutate(Smoke_Vape_Daily=str_replace_all(Smoke_Vape_Daily, " ", "_"))
covid.raw <- covid.raw %>% mutate(Chronic_Illnesses=str_replace_all(Chronic_Illnesses, " ", "_"))
covid.raw <- covid.raw %>% mutate(Cancer_Treatment=str_replace_all(Cancer_Treatment, " ", "_"))
covid.raw <- covid.raw %>% mutate(Notice_Odors=str_replace_all(Notice_Odors, " ", "_"))
covid.raw <- covid.raw %>% mutate(Odors_Revive_Memories_Emotions=str_replace_all(Odors_Revive_Memories_Emotions, " ", "_"))
covid.raw <- covid.raw %>% mutate(Odor_Importance=str_replace_all(Odor_Importance, " ", "_"))
covid.raw <- covid.raw %>% mutate(Notice_Other_House_Smells=str_replace_all(Notice_Other_House_Smells, " ", "_"))
covid.raw <- covid.raw %>% mutate(OlfactoryAbility_Current=str_replace_all(OlfactoryAbility_Current, " ", "_"))
covid.raw <- covid.raw %>% mutate(Rate_BeerAle=str_replace_all(Rate_BeerAle, " ", "_"))
covid.raw <- covid.raw %>% mutate(Rate_Broccoli=str_replace_all(Rate_Broccoli, " ", "_"))
covid.raw <- covid.raw %>% mutate(Rate_BrusselSprouts=str_replace_all(Rate_BrusselSprouts, " ", "_"))
covid.raw <- covid.raw %>% mutate(Rate_Cabbage=str_replace_all(Rate_Cabbage, " ", "_"))
covid.raw <- covid.raw %>% mutate(Rate_Cauliflower=str_replace_all(Rate_Cauliflower, " ", "_"))
covid.raw <- covid.raw %>% mutate(Rate_Coffee_NoMilk=str_replace_all(Rate_Coffee_NoMilk, " ", "_"))
covid.raw <- covid.raw %>% mutate(Rate_TartApple=str_replace_all(Rate_TartApple, " ", "_"))
covid.raw <- covid.raw %>% mutate(Rate_BeerLager=str_replace_all(Rate_BeerLager, " ", "_"))
covid.raw <- covid.raw %>% mutate(Rate_Lettuce=str_replace_all(Rate_Lettuce, " ", "_"))
covid.raw <- covid.raw %>% mutate(Rate_RedWine=str_replace_all(Rate_RedWine, " ", "_"))
covid.raw <- covid.raw %>% mutate(Rate_SweetApple=str_replace_all(Rate_SweetApple, " ", "_"))
covid.raw <- covid.raw %>% mutate(Rate_Spinach=str_replace_all(Rate_Spinach, " ", "_"))
covid.raw <- covid.raw %>% mutate(Rate_BlackTea_NoMilk=str_replace_all(Rate_BlackTea_NoMilk, " ", "_"))
covid.raw <- covid.raw %>% mutate(Rate_WhiteWine=str_replace_all(Rate_WhiteWine, " ", "_"))
covid.raw <- covid.raw %>% mutate(OlfactoryAbility_BeforeSmellLoss=str_replace_all(OlfactoryAbility_BeforeSmellLoss, " ", "_"))
covid.raw <- covid.raw %>% mutate(OlfactoryAbility_DuringSmellLoss=str_replace_all(OlfactoryAbility_DuringSmellLoss, " ", "_"))
covid.raw <- covid.raw %>% mutate(Diagnosis_Method=str_replace_all(Diagnosis_Method, " ", "_"))
covid.raw <- covid.raw %>% mutate(Symptom_Fever=str_replace_all(Symptom_Fever, " ", "_"))
covid.raw <- covid.raw %>% mutate(Symptom_DryCough=str_replace_all(Symptom_DryCough, " ", "_"))
covid.raw <- covid.raw %>% mutate(Symptom_DryMouth=str_replace_all(Symptom_DryMouth, " ", "_"))
covid.raw <- covid.raw %>% mutate(Symptom_CoughMucus=str_replace_all(Symptom_CoughMucus, " ", "_"))
covid.raw <- covid.raw %>% mutate(Symptom_DifficultyBreathing=str_replace_all(Symptom_DifficultyBreathing, " ", "_"))
covid.raw <- covid.raw %>% mutate(Symptom_ChestTightness=str_replace_all(Symptom_ChestTightness, " ", "_"))
covid.raw <- covid.raw %>% mutate(Symptom_RunnyNose=str_replace_all(Symptom_RunnyNose, " ", "_"))
covid.raw <- covid.raw %>% mutate(Symptom_SoreThroat=str_replace_all(Symptom_SoreThroat, " ", "_"))
covid.raw <- covid.raw %>% mutate(Symptom_ChangesFoodFlavor=str_replace_all(Symptom_ChangesFoodFlavor, " ", "_"))
covid.raw <- covid.raw %>% mutate(Symptom_ChangesSmell=str_replace_all(Symptom_ChangesSmell, " ", "_"))
covid.raw <- covid.raw %>% mutate(Symptom_LostAppetite=str_replace_all(Symptom_LostAppetite, " ", "_"))
covid.raw <- covid.raw %>% mutate(Symptom_Headache=str_replace_all(Symptom_Headache, " ", "_"))
covid.raw <- covid.raw %>% mutate(Symptom_Muscleache=str_replace_all(Symptom_Muscleache, " ", "_"))
covid.raw <- covid.raw %>% mutate(Symptom_Fatigue=str_replace_all(Symptom_Fatigue, " ", "_"))
covid.raw <- covid.raw %>% mutate(Symptom_Diarrhea=str_replace_all(Symptom_Diarrhea, " ", "_"))
covid.raw <- covid.raw %>% mutate(Symptom_AbdominalPain=str_replace_all(Symptom_AbdominalPain, " ", "_"))
covid.raw <- covid.raw %>% mutate(Symptom_Nausea=str_replace_all(Symptom_Nausea, " ", "_"))
covid.raw <- covid.raw %>% mutate(Symptom_OtherSymptoms=str_replace_all(Symptom_OtherSymptoms, " ", "_"))
covid.raw <- covid.raw %>% mutate(Symptom_NoSymptoms=str_replace_all(Symptom_NoSymptoms, " ", "_"))
covid.raw <- covid.raw %>% mutate(Diagnosis_due_to_SmellTasteLoss=str_replace_all(Diagnosis_due_to_SmellTasteLoss, " ", "_"))
covid.raw <- covid.raw %>% mutate(SmellTasteLoss_Cause_General=str_replace_all(SmellTasteLoss_Cause_General, " ", "_"))
covid.raw <- covid.raw %>% mutate(SmellTasteLoss_Cause_VirusSpecific=str_replace_all(SmellTasteLoss_Cause_VirusSpecific, " ", "_"))
covid.raw <- covid.raw %>% mutate(Time_Since_SmellTasteLoss=str_replace_all(Time_Since_SmellTasteLoss, " ", "_"))
covid.raw <- covid.raw %>% mutate(Sexual_Orientation=str_replace_all(Sexual_Orientation, " ", "_"))
covid.raw <- covid.raw %>% mutate(Experienced_SmellTasteLoss=str_replace_all(Experienced_SmellTasteLoss, " ", "_"))
covid.raw <- covid.raw %>% mutate(MinorityEthnic_Race_Status=str_replace_all(MinorityEthnic_Race_Status, " ", "_"))
covid.raw <- covid.raw %>% mutate(US_Race=str_replace_all(US_Race, " ", "_"))
covid.raw <- covid.raw %>% mutate(Compare_Income_Race=str_replace_all(Compare_Income_Race, " ", "_"))
covid.raw <- covid.raw %>% mutate(Income_Meets_Needs=str_replace_all(Income_Meets_Needs, " ", "_"))
covid.raw <- covid.raw %>% mutate(Compare_Income_GeneralPopulation=str_replace_all(Compare_Income_GeneralPopulation, " ", "_"))
covid.raw <- covid.raw %>% mutate(Compare_Resources_Race=str_replace_all(Compare_Resources_Race, " ", "_"))
covid.raw <- covid.raw %>% mutate(Compare_Resources_GeneralPopulation=str_replace_all(Compare_Resources_GeneralPopulation, " ", "_"))
covid.raw <- covid.raw %>% mutate(Resources_Meets_Needs=str_replace_all(Resources_Meets_Needs, " ", "_"))
covid.raw <- covid.raw %>% mutate(Household_Size=str_replace_all(Household_Size, " ", "_"))
covid.raw <- covid.raw %>% mutate(MultiGeneration_Household=str_replace_all(MultiGeneration_Household, " ", "_"))
covid.raw <- covid.raw %>% mutate(Exposure_Chemicals=str_replace_all(Exposure_Chemicals, " ", "_"))
covid.raw <- covid.raw %>% mutate(Exposure_AirPollution=str_replace_all(Exposure_AirPollution, " ", "_"))
covid.raw <- covid.raw %>% mutate(Exposure_Tobacco=str_replace_all(Exposure_Tobacco, " ", "_"))
covid.raw <- covid.raw %>% mutate(Exposure_PoorVentilation=str_replace_all(Exposure_PoorVentilation, " ", "_"))
covid.raw <- covid.raw %>% mutate(Exposure_NotApplicable=str_replace_all(Exposure_NotApplicable, " ", "_"))
covid.raw <- covid.raw %>% mutate(Generations_per_Household=str_replace_all(Generations_per_Household, " ", "_"))
covid.raw <- covid.raw %>% mutate(Location=str_replace_all(Location, " ", "_"))

#fix data errors
#Generations per household response issues
covid.raw <- covid.raw %>% mutate(Generations_per_Household=str_replace(Generations_per_Household, "One", "1"))
covid.raw <- covid.raw %>% mutate(Generations_per_Household=str_replace(Generations_per_Household, "Hfjdhds", ""))
covid.raw <- covid.raw %>% mutate(Generations_per_Household=str_replace(Generations_per_Household, "76", ""))

#individual 370 reported many symptoms but also checked Symptoms_NoSymptoms
covid.raw[282,51] = ''

#fill missing values for multigeneration household with information from household size
covid.raw <- covid.raw %>%
    mutate(MultiGeneration_Household=replace(MultiGeneration_Household, Household_Size==0, "No"))
covid.raw <- covid.raw %>%
    mutate(MultiGeneration_Household=replace(MultiGeneration_Household, Household_Size==1, "No"))
covid.raw <- covid.raw %>%
    mutate(MultiGeneration_Household=replace(MultiGeneration_Household, Household_Size==2, "No"))

#exclude individuals reporting no income and then reporting an income bracket starting at 32K
table(covid.raw$Income, covid.raw$Have_Income)
#kara's solution
bad.income <- covid.raw %>% filter(Have_Income == "No" & Income == "32K_41K")
covid.raw <- dplyr::anti_join(covid.raw, bad.income)

#save data
write.csv(covid.raw, "data-covidrace-v2-cleaned.csv", quote=FALSE, row.names = FALSE)
rm(covid.raw, bad.income)

covid.clean <- read.table('data-covidrace-v2-cleaned.csv', header=TRUE, sep=",", dec=".", stringsAsFactors = TRUE)

#replace missing values with NA
covid.clean[covid.clean==""]<-NA

#replace NA with 0 in Symptoms
covid.clean$Symptom_Fever[is.na(covid.clean$Symptom_Fever)] <- 0
covid.clean$Symptom_DryCough[is.na(covid.clean$Symptom_DryCough)] <- 0
covid.clean$Symptom_DryMouth[is.na(covid.clean$Symptom_DryMouth)] <- 0
covid.clean$Symptom_CoughMucus[is.na(covid.clean$Symptom_CoughMucus)] <- 0
covid.clean$Symptom_DifficultyBreathing[is.na(covid.clean$Symptom_DifficultyBreathing)] <- 0
covid.clean$Symptom_ChestTightness[is.na(covid.clean$Symptom_ChestTightness)] <- 0
covid.clean$Symptom_RunnyNose[is.na(covid.clean$Symptom_RunnyNose)] <- 0
covid.clean$Symptom_SoreThroat[is.na(covid.clean$Symptom_SoreThroat)] <- 0
covid.clean$Symptom_ChangesFoodFlavor[is.na(covid.clean$Symptom_ChangesFoodFlavor)] <- 0
covid.clean$Symptom_ChangesSmell[is.na(covid.clean$Symptom_ChangesSmell)] <- 0
covid.clean$Symptom_LostAppetite[is.na(covid.clean$Symptom_LostAppetite)] <- 0
covid.clean$Symptom_Headache[is.na(covid.clean$Symptom_Headache)] <- 0
covid.clean$Symptom_Muscleache[is.na(covid.clean$Symptom_Muscleache)] <- 0
covid.clean$Symptom_Fatigue[is.na(covid.clean$Symptom_Fatigue)] <- 0
covid.clean$Symptom_Diarrhea[is.na(covid.clean$Symptom_Diarrhea)] <- 0
covid.clean$Symptom_AbdominalPain[is.na(covid.clean$Symptom_AbdominalPain)] <- 0
covid.clean$Symptom_Nausea[is.na(covid.clean$Symptom_Nausea)] <- 0
covid.clean$Symptom_OtherSymptoms[is.na(covid.clean$Symptom_OtherSymptoms)] <- 0
covid.clean$Symptom_NoSymptoms[is.na(covid.clean$Symptom_NoSymptoms)] <- 0

#save data
#these data are shared and contain no PII
write.csv(covid.clean, "data-covidrace-v2-cleaned.csv", quote=FALSE, row.names = FALSE)

#tidy
rm(list = ls())
gc()
