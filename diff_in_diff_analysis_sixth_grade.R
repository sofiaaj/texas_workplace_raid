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
estimate_did_model = function(df,type,covariates){
  if(type == 'score'){
    if(covariates == "T"){
      reg = lm(Score ~ treated + campus + year + percent_hispanic + percent_lep + percent_econdis + percent_white,
               data=df,weight=num)
    }else{
      reg = lm(Score ~ treated + campus + year,data=df,weight=num)
    }
  } else{
    if(covariates == "T"){
      reg = lm(Passed ~ treated + campus + year + percent_hispanic + percent_lep + percent_econdis + percent_white,
               data=df,weight=num)
    }else{
      reg = lm(Passed ~ treated + campus + year,data=df,weight=num)
    }
  }
  return(reg)
}

get_models = function(df,type){
  nocov = lapply(df,estimate_did_model,type,'F')
  cov = lapply(df,estimate_did_model,type,'T')
  idx = c(1,4,2,5,3,6)
  final = (c(nocov,cov))[idx]
}

combined_score = get_models(combined,'score')
combined_passed = get_models(combined,'passed')
reading_score = get_models(reading,'score')
reading_passed = get_models(reading,'passed')
math_score = get_models(math,'score')
math_passed = get_models(math,'passed')

all_models = list(combined_score,
                  combined_passed,
                  reading_score,
                  reading_passed,
                  math_score,
                  math_passed)

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
                   stargazer_models[[3]],
                   stargazer_models[[5]],
                   stargazer_models[[2]],
                   stargazer_models[[4]],
                   stargazer_models[[6]],
                   panel.names = c("Score, math and reading combined", 
                                   "Score, reading",
                                   "Score, math",
                                   "Passing rate, math and reading combined", 
                                   "Passing rate, reading",
                                   "Passing rate, math"),
                   same.summary.stats = TRUE)

star_tex_write(panel,file='results/tables/did_panel_sixthgrade.tex')


# Make plots -> one 3x3 panel for each metric
data_plots = function(i,dfs,s){
  cg_type = control_group_types[i]
  df = dfs[[i]]
  df = df %>%
    mutate(treated = ifelse(district == 43901 & 
                            group == 'hisp',"Treated","Control")) %>%
    group_by(treated,year) %>%
    summarise(Score = weighted.mean(Score,num),
              Passed = weighted.mean(Passed,num)) %>%
    mutate(control_group = cg_type,
           subject = s) %>%
    gather(Type,Results,Score,Passed)
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

type = c('Passed','Score')
lapply(type,make_plot)


