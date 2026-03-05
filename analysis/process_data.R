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
free_recall <- exp_data %>% 
  filter(trial_index %in% seq(4,16)) %>%
  #fill in stimulus list
  fill(stimulus,.direction="down") %>%
  filter(trial_type =="survey-text") %>%
  rename(stimulus_list = stimulus) %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) %>%
  rename(
    word_1 = Q0,
    word_2 = Q1,
    word_3 = Q2,
    word_4 = Q3,
    word_5 = Q4,
    word_6 = Q5,
    word_7 = Q6,
    word_8 = Q7,
    word_9 = Q8,
    word_10 = Q9,
    word_11 = Q10,
    word_12 = Q11,
    word_13 = Q12,
    word_14 = Q13,
    word_15 = Q14
  )

#join back in
exp_data <- exp_data %>%
  left_join(free_recall)

#extract likert responses
recognition_trials <- exp_data %>%
  filter(task=="recognition") %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>%
  unnest(json) %>%
  rename(likert_rating = Q0)

#join into exp_data
exp_data <- exp_data %>%
  left_join(recognition_trials)

#extract final questionnaire responses
questionnaire_responses <- exp_data %>% 
  filter(trial_index ==100) %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) %>%
  rename(
    experiment_about = Q0,
    feel_while_completing = Q1,
    subjective_task_difficulty = Q2,
    technical_issues = Q3,
    feedback = Q4
  ) %>%
  select(random_id,experiment_about,feel_while_completing,subjective_task_difficulty,technical_issues,feedback)

#join into exp_data
exp_data <- exp_data %>%
  left_join(questionnaire_responses)

#filter dataset
exp_data <- exp_data %>%
  filter(!is.na(task))

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
  #add trial_number
  group_by(participant_id) %>%
  mutate(trial_number = row_number()) %>%
  relocate(trial_number,.after=trial_index)
  
#store processed and prepped data
write_csv(processed_data,here(processed_data_directory,paste0(file_name,"-processed-data.csv")))
