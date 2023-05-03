setwd("~/Desktop/texas_workplace_raid")
library(dplyr)
library(data.table)
library(tidyr)
library(ggiplot)
library(ggplot2)
library(stargazer)
library(knitr)
library(kableExtra)
library(stringr)
library(MatchIt)
library(optmatch)
library(ggpubr)
library('starpolishr')


theme <- theme_bw() + theme(text = element_text(family = "Georgia",size=8)) 
theme_set(theme)

full_df = fread('data/campus/staar_data_full.csv')

att_df = full_df %>% 
  filter(!is.na(attendance_rate_hispanic)) %>%
  filter(charter == 'N') %>%
  filter(year >= 15) %>%
  mutate(time_from_raid = time_from_raid / 60) %>%
  filter(!(time_from_raid < 28 & district != 43901)) %>%
  filter(region %in% c(10) & nces_district == 21) %>%
  select(campus,
         year,
         district,
         percent_hispanic,
         percent_white,
         percent_econdis,
         percent_lep,
         num_students,
         attendance_rate_hispanic,
         attendance_rate_lep) %>%
  unique() %>%
  group_by(campus) %>%
  mutate(count=n()) %>%
  filter(count==5)

match_data = att_df %>%
  group_by(campus,district) %>%
  summarise(percent_hispanic = mean(percent_hispanic),
            percent_econdis = mean(percent_econdis),
            percent_lep = mean(percent_lep),
            percent_white = mean(percent_white),
            num_students = mean(num_students)) %>%
  mutate(treated = ifelse(district == 43901,1,0))

mod_match <- matchit(treated ~ percent_hispanic +
                       percent_lep +
                       percent_econdis,
                     method = 'nearest',
                     ratio=2,
                     data = match_data)

estimate_did_model = function(df,covariates){
  df = df %>%
    mutate(treated = ifelse(district == 43901 & year == 19,"1","0"),
           campus = as.factor(campus),
           year = as.factor(year)) 
  if(covariates == "T"){
    reg = lm(attendance_rate_hispanic ~ treated + campus + year + percent_hispanic +
               percent_lep + percent_white + 
               percent_econdis,data=df,weights=num_students)
  }else{
    reg = lm(attendance_rate_hispanic ~ treated + campus + year,
             data=df,weights=num_students)
  }
  return(reg)
}

dfs = list(att_df,att_df %>% filter(campus %in% match.data(mod_match)$campus))
models = list(estimate_did_model(dfs[[1]],'F'),
              estimate_did_model(dfs[[1]],'T'),
              estimate_did_model(dfs[[2]],'F'),
              estimate_did_model(dfs[[2]],'T'))
sg = stargazer(models,keep=c('treated','Constant'),
               ci=F,
               column.labels = c('Hispanic',
                                 'Hispanic PSM'),
               column.separate = c(2,2),
               dep.var.caption = '',
               dep.var.labels = '',
               keep.stat = c("n"),
               model.numbers=FALSE,
               model.names = FALSE)
star_tex_write(sg,file='results/tables/attendance_rate_did_panel_sixthgrade.tex')

rbind(dfs[[1]] %>% mutate(strategy = 'Schools in region'),
      dfs[[2]] %>% mutate(strategy = 'Matched schools')) %>%
  mutate(treated = ifelse(district == 43901,"Treated","Control"),
         year = as.numeric(as.character(year))) %>%
  group_by(treated,year,strategy) %>%
  summarise(attendance_rate_hispanic = weighted.mean(attendance_rate_hispanic,num_students)) %>%
  ggplot(aes(x=year,y=attendance_rate_hispanic,linetype=treated)) +
  geom_line() + 
  geom_point() + 
  geom_vline(xintercept=18,linetype='dashed') +
  coord_cartesian(ylim=c(96,98)) +
  facet_wrap(~strategy,nrow=2) +
  labs(x="Year",y="Attendance Rate of Hispanic Students") +
  theme(legend.position='bottom',legend.title=element_blank())

ggsave('results/figures/did_trends_attendance_rate.png',width=6,height=4)
