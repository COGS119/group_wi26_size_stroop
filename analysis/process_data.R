library(here)
library(tidyverse)
library(jsonlite)

processed_data_directory <- here("..","data","processed_data")
file_name <- "size_stroop"

#read experiment data
exp_data <- read_csv(here(processed_data_directory,paste0(file_name,"-alldata.csv")))

#code for dealing with atypical participant id storage
participant_ids <- exp_data %>%
  select(random_id,response) %>%
  filter(str_detect(response,"participant_id")) %>%
  #extract response to participant_id
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>%
  unnest(cols = c(json)) %>%
  #clean up participant ids
  mutate(
    participant_id = case_when(
      participant_id == "9252" ~ "parrot",
      participant_id == "A18534325" ~ "moose",
      TRUE ~ trimws(tolower(participant_id))
    )
  ) %>%
  select(random_id,participant_id)

#join in to exp_data
exp_data <- exp_data %>%
  left_join(participant_ids,by="random_id")

#double check that participant ids are unique
counts_by_random_id <- exp_data %>%
  group_by(random_id,participant_id) %>%
  count()
#output to track participants
write_csv(counts_by_random_id,here(processed_data_directory,paste0(file_name,"-participant-list.csv")))

#extract reward question
final_questions <- exp_data %>% 
  filter(trial_index >5) %>%
  select(-c(Q0:Q3)) %>%
  filter(trial_type =="survey-text") %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) %>%
  rename(
    experiment_thoughts = Q0,
    what_exp_about = Q1,
    which_half_more_difficult = Q2,
    tech_issues = Q3
  ) %>%
  distinct(random_id, participant_id,experiment_thoughts,what_exp_about,which_half_more_difficult,tech_issues)

#join back in
exp_data <- exp_data %>%
  left_join(final_questions)

#extract slider response
experiment_rating <- exp_data %>%
  filter(trial_type=="html-slider-response") %>%
  rename(experiment_rating = response) %>%
  select(random_id,participant_id,experiment_rating)

#join into exp_data
exp_data <- exp_data %>%
  left_join(experiment_rating)

#filter dataset
exp_data <- exp_data %>%
  filter(trial_type=="image-keyboard-response")

#filter participant ids
filter_ids <- c("3332","nonononono","hsdhfasdhf")

#identify participants from the experiment group
group_members <- c("gazelle","penguin","goose","lemur","wallaby","cub")

processed_data <- exp_data %>%
  filter(!(participant_id %in% filter_ids)) %>%
  #flag for group participants
  mutate(participant_is_group_member = case_when(
    participant_id %in% group_members ~ TRUE,
    TRUE ~ FALSE
  
  )) %>%
  #remove unneeded columns
  select(-c(success,plugin_version,timeout:failed_video)) %>%
  # add congruent column
  mutate(
    congruency = case_when(
      str_detect(task_type,"incongruent") ~ "incongruent",
      str_detect(task_type,"congruent") ~ "congruent",
      TRUE ~ NA_character_
    )
  ) %>%
  #add trial_number
  group_by(participant_id) %>%
  mutate(trial_number = row_number()) %>%
  relocate(trial_number,.after=trial_index)
  
#store processed and prepped data
write_csv(processed_data,here(processed_data_directory,paste0(file_name,"-processed-data.csv")))
