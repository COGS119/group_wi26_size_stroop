library(tidyverse)
library(here)
library(lme4)
library(lmerTest)
library(patchwork)

processed_data_directory <- here("..","data","processed_data")
file_name <- "size_stroop"

processed_data <- read_csv(here(processed_data_directory,paste0(file_name,"-processed-data.csv")))

#quick look
avg_rt_subj_pre_excl <- processed_data %>%
  #original paper was 1500
  filter(rt>=200&rt<=1500) %>%
  group_by(participant_id,task_order,congruency,size_condition) %>%
  summarize(
    N=n(),
    accuracy = mean(correct),
    average_rt = mean(rt[correct])
  ) %>%
  arrange(participant_id,task_order,congruency,size_condition)

#exclude participants with very large RTs
exclusion_participants <- avg_rt_subj_pre_excl %>%
  group_by(participant_id) %>%
  summarize(overall_avg_rt = mean(average_rt)) %>%
  filter(overall_avg_rt>1000) %>%
  pull(participant_id)

avg_rt_subj <- avg_rt_subj_pre_excl %>%
  filter(!(participant_id %in% exclusion_participants))

overall_rt <- avg_rt_subj %>%
  group_by(congruency,size_condition) %>%
  summarize(
    N=n(),
    avg_correct = mean(accuracy),
    avg_rt = mean(average_rt),
    sd = sd(average_rt),
    sem = sd / sqrt(N)
  ) %>%
  arrange(congruency,size_condition)


ggplot(overall_rt,aes(congruency,avg_rt,color=congruency,fill=congruency))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=avg_rt-sem,ymax=avg_rt+sem),width=0,color="black")+
  facet_wrap(~size_condition)

p1 <- ggplot(filter(avg_rt_subj,size_condition=="big"),aes(congruency,average_rt,color=congruency))+
  geom_violin(fill=NA)+
  geom_line(aes(group=participant_id),color="black",alpha=0.1,position=position_jitter(width=0.05,height=0,seed=123))+
  geom_jitter(position=position_jitter(width=0.05,height=0,seed=123),alpha=0.3)+
  geom_line(data=filter(overall_rt,size_condition=="big"),aes(y=avg_rt,group=1),linewidth=1.5,color="black")+
  geom_point(data=filter(overall_rt,size_condition=="big"),aes(y=avg_rt),size=4)+
  geom_errorbar(data=filter(overall_rt,size_condition=="big"),aes(y=avg_rt,ymin=avg_rt-sem,ymax=avg_rt+sem),width=0)+
  xlab("Stimulus Type")+
  ylab("Average Reaction Time (in ms)")+
  theme_bw(base_size=16)+
  theme(legend.position="none")+
  ggtitle("BIGGER") +
  theme(plot.title = element_text(hjust = 0.5))
p2 <- ggplot(filter(avg_rt_subj,size_condition=="small"),aes(congruency,average_rt,color=congruency))+
  geom_violin(fill=NA)+
  geom_line(aes(group=participant_id),color="black",alpha=0.1,position=position_jitter(width=0.05,height=0,seed=123))+
  geom_jitter(position=position_jitter(width=0.05,height=0,seed=123),alpha=0.3)+
  geom_line(data=filter(overall_rt,size_condition=="small"),aes(y=avg_rt,group=1),linewidth=1.5,color="black")+
  geom_point(data=filter(overall_rt,size_condition=="small"),aes(y=avg_rt),size=4)+
  geom_errorbar(data=filter(overall_rt,size_condition=="small"),aes(y=avg_rt,ymin=avg_rt-sem,ymax=avg_rt+sem),width=0)+
  xlab("Stimulus Type")+
  ylab("Average Reaction Time (in ms)")+
  theme_bw(base_size=16)+
  theme(legend.position="none")+
  ggtitle("SMALLER") +
  theme(plot.title = element_text(hjust = 0.5))
p1+p2

processed_data <- processed_data %>%
  mutate(
    congruency_c = ifelse(congruency=="congruent",-0.5,0.5),
    size_condition_c = ifelse(size_condition=="small",-0.5,0.5)
  )

#random effect w/ interaction leads to singular fit
m <- lmer(rt ~ congruency_c*size_condition_c +(1+size_condition_c+congruency_c|participant_id),data=filter(processed_data,correct&rt>=200&rt<1500))
summary(m)

#fit an anova predicting congruency by size_condition
m_anova <- avg_rt_subj %>% 
  ungroup() %>%
  rstatix::anova_test(average_rt ~ congruency*size_condition + Error(participant_id/(congruency*size_condition)))
m_anova %>% get_anova_table()
