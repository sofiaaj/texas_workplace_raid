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

# Runs diff-in-diff analysis for white and hispanic control groups
# Output is panel table in latex + figures showing trends

reading = readRDS('data/analysis/reading_sixthgrade.rds')
math = readRDS('data/analysis/math_sixthgrade.rds')
combined = readRDS('data/analysis/combined_sixthgrade.rds')

#Makes panel with all regression models
estimate_did_model = function(df,covariates){
  if(covariates == "T"){
    reg = lm(Mastered ~ treated + campus + year + percent_hispanic + percent_lep + percent_econdis +
               percent_white,data=df,weight=num)
    }else{
      reg = lm(Mastered ~ treated + campus + year,data=df,weight=num)
    }
  return(reg)
}

get_models = function(df){
  nocov = lapply(df,estimate_did_model,'F')
  cov = lapply(df,estimate_did_model,'T')
  idx = c(1,4,2,5,3,6)
  final = (c(nocov,cov))[idx]
}


reading_master = get_models(reading)
math_master = get_models(math)
combined_master = get_models(combined)

all_models = list(combined_master,
                  reading_master,
                  math_master)

get_stargazer = function(models){
  star = stargazer(models,keep=c('treated'),
                   ci=F,
                   column.labels = c('White',
                                     'Hispanic',
                                     'Hispanic PSM'),
                   column.separate = c(2,2,2),
                   dep.var.caption = '',
                   dep.var.labels = '',
                   keep.stat = c("n"),
                   model.numbers=FALSE,
                   model.names = FALSE)
}

stargazer_models = lapply(all_models,get_stargazer)
panel = star_panel(stargazer_models[[1]],
                   stargazer_models[[2]],
                   stargazer_models[[3]],
                   panel.names = c("% Mastered, math and reading combined", 
                                   "% Mastered, reading",
                                   "% Mastered, math"),
                   same.summary.stats = TRUE)

star_tex_write(panel,file='results/tables/mastered_did_panel_sixthgrade.tex')


# Make plots -> one 3x3 panel for each metric
data_plots = function(i,dfs,s){
  cg_type = control_group_types[i]
  df = dfs[[i]]
  df = df %>%
    mutate(treated = ifelse(district == 43901 & 
                            group == 'hisp',"Treated","Control")) %>%
    group_by(treated,year) %>%
    summarise(Mastered = weighted.mean(Mastered,num)) %>%
    mutate(control_group = cg_type,
           subject = s) %>%
    gather(Type,Results,Mastered)
  return(df)
}

control_group_types = c('White Students (Allen)',
                        'Hispanic Students (Region)',
                        'Hispanic Students (PSM)')

plot_dfs = bind_rows(bind_rows(lapply(1:3,data_plots,reading,'Reading')),
                     bind_rows(lapply(1:3,data_plots,math,'Math')),
                     bind_rows(lapply(1:3,data_plots,combined,'Combined')))


make_plot = function(t){
  plot_dfs %>%
    filter(Type==t) %>%
    ggplot(aes(x=as.numeric(as.character(year)),
               y=Results,linetype=treated)) +
    geom_line() +
    geom_vline(xintercept=18,linetype='dashed') +
    facet_grid(subject ~ control_group) +
    labs(x='Year',
         y=t) +
    theme(legend.position='bottom',
          legend.title=element_blank())
  filename = paste0('results/figures/did_trends_',t,'.png')
  ggsave(filename,width=7,height=6)
}

type = c('Mastered')
lapply(type,make_plot)


