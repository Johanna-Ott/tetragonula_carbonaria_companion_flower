#01_Data wrangling
#Preparing data and having a first look
#Johanna Ott, 01.02.2025

#reading in libraries----
library(tidyr)
library(tidyverse)
library(dplyr)

#reading in data----
raw_data<-read.csv("data/stingless_bees.csv")
treatment_description<-read_csv("data/treatment_description.csv")
#first look----
head(raw_data)

#preparing for join:

treatment_description$treatment<-as.factor(treatment_description$treatment)

#wrangling----
#Reshaping data frame to long format
long_data <-raw_data %>% 
  mutate(total_visitation = number_stingless_target + number_stingless_companion,
         relative_target_visitation = number_stingless_target/total_visitation ) %>% 
 #putting all the numbers in one column "number_individuals" and one identifier column "flowertype"
  pivot_longer(cols= c("number_stingless_target","number_honeybee_target","number_stingless_companion","number_honeybee_companion"), names_to = "flowertype",values_to =  "number_individuals") %>% 
  #splitting flower type into the species and the flower type
   tidyr::separate(col="flowertype",into=c("number","species","flowertype"),sep="_",remove=T) %>%
  #making information about treatment more accessible by creating different columns
  tidyr::separate(treatment, into = c("treatment", "similarity", "companion_concentration"), 
                             sep = "(?<=\\d)\\s?\\(|,|\\)", remove = TRUE) %>% 
  #this line just creates the entries as "2" instead of "treatment 2" in the treatment column
  mutate(treatment = str_extract(treatment, "\\d+")) %>% 
  left_join(treatment_description,by= "treatment") %>% 
  #getting rid of unnecessary column "number", just picking pertinent columns
  select(c("timeslot","colony","treatment","similarity","target_color","target_concentration","companion_color","companion_concentration.y","species","total_visitation", "flowertype","number_individuals", "relative_target_visitation","date", "start_time_of_experiment","temp_at_start_of_experiment","comments")) %>% 
  #removing entries for colonies 1 and 2 as they did not complete all treatments
  filter(!colony %in% c(1, 2),
         date != "2025-02-21",
         date != "2025-02-14") %>% 
  rename(companion_concentration=companion_concentration.y) %>% 
  #renaming timeslots into number 1-12
  mutate(
    timeslot = case_when( 
    timeslot == "Minutes 1-10" ~ "1",
    timeslot == "Minutes 11-20" ~ "2",
    timeslot == "Minutes 21-30" ~ "3",
    timeslot == "Minutes 31-40" ~ "4",
    timeslot == "Minutes 41-50" ~ "5",
    timeslot == "Minutes 51-60" ~ "6",
    timeslot == "Minutes 61-70" ~ "7",
    timeslot == "Minutes 71-80" ~ "8",
    timeslot == "Minutes 81-90" ~ "9",
    timeslot == "Minutes 91-100" ~ "10",
    timeslot == "Minutes 101-110" ~ "11",
    timeslot == "Minutes 111-120" ~ "12"),
    #accounting for the fact that control has twice as many target flowers
    number_individuals =   case_when(treatment == "5" ~ round(0.5* number_individuals),
                                     treatment!= "5" ~ number_individuals),
  #creating ID column with colony_treatment_timeslot
   id = paste(colony, treatment,  sep="_"),
  companion_color = case_when( 
    companion_color == "light_blue_" ~ "light_blue",
    companion_color!= "light_blue_" ~ companion_color),
 attractiveness = case_when(companion_concentration =="20" ~ "low",
                                    companion_concentration == "40" ~ "high",
                            companion_concentration =="10"~"control"))

#Adjusting classes of some columns
long_data$timeslot<-as.numeric(long_data$timeslot)
long_data$colony<-as.factor(long_data$colony)
long_data$companion_concentration<-as.factor(long_data$companion_concentration)
long_data$attractiveness<-as.factor(long_data$attractiveness)




save(long_data, file="data/stingless_bees_tidy.RData")
write.csv(long_data, "data/stingless_bees_tidy.csv")

#For checking that dataset is complete
summary_allcols<-long_data %>% 
  filter(colony %in% c("8","9","10","11","12","13")) %>% 
  group_by(colony,treatment) %>% 
  tally()
print(summary_allcols)
# as all colony-treatment combinations appear 48 times, the data is complete
