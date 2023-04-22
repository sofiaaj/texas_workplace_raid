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
  mutate(time_from_raid = time_from_raid / 60) %>%
  filter(time_from_raid < 60)

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
  mod = lm(as.formula(base_formula),data=df %>% filter(time_from_raid < 28))
  return(mod)
}

score_mods = lapply(dfs,estimate_model,"Score")
passing_mods = lapply(dfs,estimate_model,"Passed")
score_passing = list(score_mods,passing_mods)

get_stargazer = function(mods){
  star = stargazer(mods,
                   ci=F,
                   column.labels = c('Combined',
                                     'Reading',
                                     'Math'),
                   column.separate = c(1,1,1),
                   dep.var.caption = '',
                   dep.var.labels = '',
                   keep.stat = c("n"),
                   model.numbers=FALSE,
                   model.names = FALSE)
}

stargazer_models = lapply(score_passing,get_stargazer)
panel = star_panel(stargazer_models[[1]],
                   stargazer_models[[2]],
                   panel.names = c("Score", 
                                   "Passing Rate"),
                   same.summary.stats = TRUE)
star_tex_write(panel,file='results/tables/distance_performance.tex')

df = rbind(dfs[[1]] %>% mutate(test = 'Combined'),
           dfs[[2]] %>% mutate(test = 'Reading'),
           dfs[[3]] %>% mutate(test = 'Math')) %>%
  gather(test_measure,value,Score,Passed)

df %>%
  ggplot(aes(time_from_raid,y=value)) +
  #stat_summary_bin(fun.y='mean', bins=15,size=1, geom='point') +
  stat_binscatter(bins=20) +
  geom_vline(xintercept=28.5,linetype='dashed') +
  labs(x="Time from raid",
       y="Change from 2018 to 2019") +
  facet_grid(test_measure ~ test,scales='free_y')

ggsave('results/figures/distance_performance.png',width=7.5,height=4.5)
