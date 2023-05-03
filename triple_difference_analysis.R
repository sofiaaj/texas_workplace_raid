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

theme <- theme_bw() + theme(text = element_text(family = "Georgia",size=8)) 
theme_set(theme)

combined = readRDS('data/analysis/combined_sixthgrade.rds')
c = combined[[2]] %>% mutate(campus = str_replace(campus,'white|hisp','')) %>% pull(campus) %>% unique()
triple_df = fread('data/campus/staar_data_full.csv') %>%
  filter(year >= 15 & group %in% c('hisp','white') & grade == 6) %>%
  filter(campus %in% c) %>%
  group_by(campus,group) %>%
  mutate(count = n()) %>%
  filter(count == 10)


filter_summarise_data = function(df,type){
  if(type == 'combined'){
    df = df %>%
      group_by(campus,year,district,group,time_from_raid,percent_hispanic,percent_lep,percent_econdis) %>%
      summarise(score = weighted.mean(score,num),
                passed = weighted.mean(passed,num),
                num = mean(num))
    
  } else {
    df = df %>% filter(subject == type)
  }
  df = df %>%
    mutate(campus = paste0(as.character(campus),group),
           year = as.factor(year),
           campus = as.factor(campus),
           treated = ifelse(district == 43901 & group == 'hisp' & year == '19',"1","0")) %>%
    rename(Score = score,
           Passed = passed)
  return(df)
}

prepare_data = function(df){
  df = df %>%
    mutate(allen = ifelse(district == 43901,"1","0"),
           hispanic = ifelse(group == 'hisp',"1","0"),
           hisp_after = ifelse(hispanic == "1" & year == "19","1","0"),
           allen_after = ifelse(allen == "1" & year == "19","1","0"))
}

estimate_model = function(df,type){
  base_formula = paste0(type,"~ treated + 
              campus + 
              year + 
              allen_after + 
              hisp_after +
              percent_hispanic +
              percent_lep +
              percent_econdis")
  mod = lm(as.formula(base_formula),data=df,weights=num)
  return(mod)
}

triple_reading = filter_summarise_data(triple_df,'reading')
triple_math = filter_summarise_data(triple_df,'math')
triple_combined = filter_summarise_data(triple_df,'combined')
dfs = list(triple_combined,triple_reading,triple_math)
dfs = lapply(dfs,prepare_data)

score_mods = lapply(dfs,estimate_model,"Score")
passing_mods = lapply(dfs,estimate_model,"Passed")
score_passing = list(score_mods,passing_mods)

get_stargazer = function(mods){
  star = stargazer(mods,
                   keep=c('treated'),
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

star_tex_write(panel,file='results/tables/ddd_panel_sixthgrade.tex')