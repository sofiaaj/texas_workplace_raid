
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

# Makes event study plots

reading = readRDS('data/analysis/reading_sixthgrade.rds')[1:3]
math = readRDS('data/analysis/math_sixthgrade.rds')[1:3]
combined = readRDS('data/analysis/combined_sixthgrade.rds')[1:3]
dfs = readRDS('data/analysis/all_data_controls_groups.rds')

# Event study plots
run_event_study <- function(df,type){
  df = df %>%
    mutate(year=as.integer(as.character(year))) %>%
    mutate(campus = as.factor(campus),
           treat= ifelse(group == "hisp" & district == 43901,1,0),
           time_to_treat = ifelse(treat==1,year-19,0))
  if(type == 'Passed'){
    test = feols(Passed ~ i(time_to_treat,treat,ref=-1) +
                   percent_hispanic + percent_lep + percent_econdis + percent_white |
                   campus + year,
                 weights=~num,
                 cluster = ~campus,
                 data=df)
  }else{
    test = feols(Score ~ i(time_to_treat,treat,ref=-1) +
                   percent_hispanic + percent_lep + percent_econdis + percent_white|
                   campus + year,
                 weights=~num,
                 cluster = ~campus,
                 data=df)
  }
}

make_event_table = function(i){
  print(i)
  dfs = all_dfs[[i]]
  test = names(all_dfs)[[i]]
  score_plots = lapply(dfs,run_event_study,"Score")
  passing_plots = lapply(dfs,run_event_study,"Passed")
  es_table = etable(c(score_plots,passing_plots), tex = TRUE)
  filename = paste0('results/tables/',test,'_event_study.tex')
  star_tex_write(es_table,file=filename)
}

all_dfs = list(combined,reading,math)
names(all_dfs) = c('combined','reading','math')
lapply(1:3,make_event_table)


make_event_study_plot = function(dfs,type){
  es <- lapply(dfs,run_event_study,type=type)
  groups <- c('White',
              'Hispanic (Regions)',
              'Hispanic (PSM)')
  names(es) <- groups
  plot = ggiplot(es,
                 ci_level=0.95,
                 main="",
                 xlab="",
                 ylab="",
                 theme=theme(text=element_text(family="Georgia",size=8)))
  plot = plot + scale_color_grey() + geom_point()
}

score_plots = lapply(all_dfs,make_event_study_plot,"Score")
passed_plots = lapply(all_dfs,make_event_study_plot,"Passed")

get_labels = function(text,angle){
  lab = ggplot() +
    annotate(geom = 'text',
             x=1,
             y=1,
             label=text,
             angle=angle,
             family = "Georgia") +
    theme_void()
  return(lab)
}

row1 = get_labels('Combined',90)
row2 = get_labels('Reading',90)
row3 = get_labels('Math',90)
col1 = get_labels('Score',0)
col2 = get_labels('% Passed',0)
emptycol = get_labels('',0)

plots = ggarrange(emptycol,
                  col1,
                  col2,
                  row1,
                  score_plots[[1]],
                  passed_plots[[1]],
                  row2,
                  score_plots[[2]],
                  passed_plots[[2]],
                  row3,
                  score_plots[[3]],
                  passed_plots[[3]],
                  nrow=4,
                  ncol=3,
                  heights=c(0.2,1,1,1),
                  widths=c(0.1,1,1),
                  common.legend = T,
                  legend="bottom")

ggsave('results/figures/event_study.png',width=7.5,height=6)
