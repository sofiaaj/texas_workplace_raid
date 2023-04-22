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
library(MatchIt)
library('starpolishr')

theme <- theme_bw() + theme(text = element_text(family = "Georgia",size=8)) 
theme_set(theme)

# Run DiD analysis using alternative matching strategies

#(Base) Same region, same NCES type, average of variables 2015-2019
#(A) Used all schools in region regardless of NCES type
#(B) All schools in NCES type regardless of region
#(C) Same region, same NCES but added the following to the set of matching variables:
#  1.  Share of white students
#  2.  Total number of students in school
# (D) Matched each school to four control schools (instead of two)
# (E) Matched each school to one control school (instead of two)
# (F) Matched using only the values of the variables from the
# 2018-2019 school year (instead of taking average of 2015-2019)
# (G) Matched using only the values of the variables from the
# 2017-2018 school year (year prior to the raid)

full_df = fread('data/campus/staar_data_full.csv') %>%
  filter(year >= 15 & group %in% c('hisp','white') & grade == 6) %>%
  filter(!is.na(percent_hispanic) & 
           !is.na(percent_lep) & 
           !is.na(percent_econdis)) %>%
  group_by(campus,group,subject) %>%
  mutate(count = n()) %>%
  filter(count == 5) %>%
  ungroup() %>%
  mutate(time_from_raid = time_from_raid / 60) %>%
  # remove charter schools 
  filter(charter == 'N') %>%
  # remove schools close to the raid but not in Allen ISD
  filter(grade == 6 & !(time_from_raid < 28.5 & district != 43901)) %>%
  group_by(district,campus,year) %>% 
  mutate(count=n()) %>% 
  # only keep Allen schools that have observations for Hispanic and white students
  filter(!(district == 43901 & count == 2)) %>% 
  # make sure we have data for math and reading 
  # (some campuses only have complete reading data, for example)
  group_by(district,campus,year,group) %>%
  mutate(count=n()) %>%
  filter(count == 2)


get_matches <- function(type,m='nearest'){
  df = full_df %>% filter(group == 'hisp')
  if(type == "A"){
    df = df %>% filter(region %in% c(7,8,10,11,12))
  } else if(type == "B"){
    df = df %>% filter(nces_district == 21)
  } else {
    df = df %>% 
      filter(region %in% c(7,8,10,11,12))%>%
      filter(nces_district == 21)
  }
  if(type == 'F'){
    df = df %>% filter(year == '19')
  } else if(type == 'G'){
    df = df %>% filter(year == '18')
  }
  match_data = df %>%
    group_by(campus,district) %>%
    summarise(percent_hispanic = mean(percent_hispanic),
              percent_econdis = mean(percent_econdis),
              percent_lep = mean(percent_lep),
              percent_white = mean(percent_white),
              num_students = mean(num_students)) %>%
    mutate(treated = ifelse(district == 43901,1,0))
  r = 2
  if(type == 'D'){
    r = 4
  } else if(type == 'E'){
    r = 1
  }
  base_formula = 'treated ~ percent_hispanic + percent_lep + percent_econdis'
  if(type == 'C'){
    base_formula = paste0(base_formula,' + percent_white + num_students')
  } 
  mod_match <- matchit(as.formula(base_formula),
                       method = m,
                       ratio=r,
                       data = match_data)
  
  control_campus = match.data(mod_match)$campus
}


grouping_vars = c("campus",
                  "district",
                  "group", 
                  "year",
                  "percent_hispanic",
                  "percent_lep", 
                  "percent_econdis")

combined_scores <- function(x){
  df <- x %>%
    group_by_at(grouping_vars) %>%
    summarise(score = weighted.mean(score,num),
              passed = weighted.mean(passed,num),
              num = mean(num))
}

filter_subject <- function(x,s){
  df <- x %>%
    filter(subject == s)
}

get_df <- function(control_campus){
  df = full_df %>%
    filter(campus %in% control_campus & group == 'hisp') %>%
    mutate(year = as.factor(year),
           campus = as.factor(campus))
  
}

types = list('Base','A','B','C','D','E','F','G')
matches = lapply(types,get_matches)
matched_dfs = lapply(matches,get_df)
combined_psm = lapply(matched_dfs,combined_scores)
reading_psm = lapply(matched_dfs,filter_subject,'reading')
math_psm = lapply(matched_dfs,filter_subject,'math')
analysis_dfs = list(combined=combined_psm,reading=reading_psm,math=math_psm)
all_results = data.frame()
for(i in 1:8){
  for(j in 1:3){
    test_data = analysis_dfs[[j]]
    test_name = names(analysis_dfs)[j]
    match_type = types[[i]]
    curr_df = test_data[[i]]
    curr_df = curr_df %>% mutate(treated = ifelse(district == 43901 & year == "19",1,0))
    reg1 = lm(score ~ treated + year + campus,weights=num,data=curr_df)
    reg2 = lm(passed ~ treated + year + campus,weights=num,data=curr_df)
    results = rbind(tidy(reg1),
                    tidy(reg2))
    results = results %>%
      filter(term == 'treated') %>%
      mutate(err = std.error*1.96,
             min = estimate-err,
             max = estimate+err) %>%
      select(estimate,min,max) %>%
      mutate(test = test_name,
             match = match_type)
    results$test_measure = c('score','passing rate')
    all_results = rbind(all_results,results)
  }
}

all_results$match <- factor(all_results$match, 
                            levels=c("G",
                                     "F",
                                     "E",
                                     "D",
                                     "C",
                                     "B",
                                     "A",
                                     "Base"))
all_results %>%
  mutate(test = factor(test,levels=c('combined','reading','math')),
         test = str_to_title(test),
         test_measure = str_to_title(test_measure)) %>%
  ggplot(aes(x=match,y=estimate)) +
  geom_point() +
  labs(x="") +
  geom_errorbar(aes(ymin=min,ymax=max),width=0) +
  geom_hline(yintercept = 0,linetype='dashed') +
  facet_grid(test ~ test_measure,scales='free_x') +
  coord_flip()

ggsave('results/figures/psm_robustness.png',width=7.5,height=4.5)