setwd("~/Desktop/texas_workplace_raid")

library(dplyr)
library(data.table)
library(tidyr)
library(ggiplot)
library(ggplot2)
library(broom)
library(stargazer)
library(knitr)
library(kableExtra)
library(stringr)
library(ggpubr)
library(janitor)
library(sjmisc)
library(ggmap)
library(ggsn)
library(fixest)
library(MatchIt)
library('starpolishr')
source('stat_binscatter.R')

theme <- theme_bw() + theme(text = element_text(family = "Georgia",size=8)) 
theme_set(theme)

time_df = fread('data/campus/staar_data_full.csv') %>%
  filter(year >= 18 & 
           group %in% c('hisp') & 
           grade == 6 & !is.na(time_from_raid)) %>%
  group_by(campus,group,subject) %>%
  mutate(count = n()) %>%
  filter(count == 2) %>%
  ungroup() %>%
  filter(year >= 18) %>%
  mutate(time_from_raid = time_from_raid / 60,
         passed = passed*100) %>%
  filter(time_from_raid < 45)

filter_summarise_data = function(df,type){
  if(type == 'combined'){
    df = df %>%
      group_by(campus,
               year,
               district,
               group,
               time_from_raid,
               percent_hispanic,
               percent_white,
               num_students,
               percent_lep,
               percent_econdis) %>%
      summarise(score = weighted.mean(score,num),
                passed = weighted.mean(passed,num),
                mastered = weighted.mean(mastered,num),
                num = mean(num))
  } else {
    df = df %>% filter(subject == type)
  }
  df = df %>%
    rename(Score = score,
           Passed = passed,
           Mastered = mastered)
  return(df)
}

prepare_data = function(df){
  df %>%
    ungroup() %>%
    select(campus,year,time_from_raid,Score,Passed) %>%
    gather(measure,value,Score,Passed) %>%
    spread(year,value) %>%
    mutate(change = (`19`-`18`)) %>%
    select(-`18`,-`19`) %>%
    spread(measure,change)
}

reading = filter_summarise_data(time_df,'reading')
math = filter_summarise_data(time_df,'math')
combined = filter_summarise_data(time_df,'combined')

dfs = lapply(list(combined,reading,math),prepare_data)

estimate_model = function(df,type){
  base_formula = paste0(type,"~ time_from_raid")
  mod = lm(as.formula(base_formula),data=df %>% filter(time_from_raid < 27.7))
  return(mod)
}

score_mods = lapply(dfs,estimate_model,"Score")
passing_mods = lapply(dfs,estimate_model,"Passed")
score_passing_reading_math = list(score_mods[2:3],passing_mods[2:3])
score_passing_combined = score_mods[1]
score_passing_combined[2] = passing_mods[1]



get_stargazer = function(mods,labs){
  star = stargazer(mods,
                   ci=F,
                   column.labels = labs,
                   column.separate = c(1,1),
                   dep.var.caption = '',
                   dep.var.labels = '',
                   keep.stat = c("n"),
                   model.numbers=FALSE,
                   model.names = FALSE)
}

stargazer_models_combined = stargazer(score_passing_combined,
                                      ci=F,
                                      column.labels = c('Score','Passing Rate'),
                                      column.separate = c(1,1),
                                      dep.var.caption = '',
                                      dep.var.labels = '',
                                      keep.stat = c("n"),
                                      model.numbers=FALSE,
                                      model.names = FALSE)

star_tex_write(stargazer_models_combined,
               file='results/tables/distance_performance_combined.tex')


stargazer_models_reading_math = lapply(score_passing_reading_math,
                                       get_stargazer,
                                       c('Reading',
                                         'Math'))
panel_reading_math = star_panel(stargazer_models_reading_math[[1]],
                                stargazer_models_reading_math[[2]],
                   panel.names = c("Score", 
                                   "Passing Rate"),
                   same.summary.stats = TRUE)
star_tex_write(panel_reading_math,
               file='results/tables/distance_performance_reading_math.tex')

df = rbind(dfs[[1]] %>% mutate(test = 'Combined'),
           dfs[[2]] %>% mutate(test = 'Reading'),
           dfs[[3]] %>% mutate(test = 'Math')) %>%
  gather(test_measure,value,Score,Passed)

temp = df %>% filter(test == 'Combined' & test_measure == 'Score')

df %>%
  ggplot(aes(time_from_raid,y=value)) +
  #stat_summary_bin(fun.y='mean', bins=9,size=1, geom='point') +
  stat_binscatter(bins=10,geom = "pointrange",size=0.1) +
  geom_vline(xintercept=27.7,linetype='dashed') +
  labs(x="Time from raid",
       y="Change from 2018 to 2019") +
  facet_grid(test_measure ~ test,scales='free_y')

ggsave('results/figures/distance_performance.png',width=7.5,height=4.5)
