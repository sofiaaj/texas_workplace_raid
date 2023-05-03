library(dplyr)
library(data.table)
library(tidyr)
library(ggiplot)
library(ggplot2)
library(stargazer)
library(knitr)
library(ggpubr)
library(kableExtra)
library(gridExtra)
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
  if(type == 'Score'){
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

estimate_ddd_model = function(df,type,covariates){
  base_formula = paste0(type,"~ treated + 
              campus + 
              year + 
              allen_after + 
              hisp_after")
  if(covariates == "T"){
    base_formula = paste0(base_formula,"+ percent_hispanic + percent_lep + percent_econdis")
  }
  reg = lm(as.formula(base_formula),data=df,weights=num)
  return(reg)
}

get_models = function(df,type){
  df_triple = df[[4]]
  df = df[1:3]
  nocov = lapply(df,estimate_did_model,type,'F')
  cov = lapply(df,estimate_did_model,type,'T')
  nocov[[4]] = estimate_ddd_model(df_triple,type,'F')
  cov[[4]] = estimate_ddd_model(df_triple,type,'T')
  idx = c(1,5,2,6,3,7,4,8)
  final = (c(nocov,cov))[idx]
}

combined_score = get_models(combined,'Score')
combined_passed = get_models(combined,'Passed')
reading_score = get_models(reading,'Score')
reading_passed = get_models(reading,'Passed')
math_score = get_models(math,'Score')
math_passed = get_models(math,'Passed')

all_models = list(combined_score,
                  combined_passed,
                  reading_score,
                  reading_passed,
                  math_score,
                  math_passed)

get_stargazer = function(models){
  star = stargazer(models,keep=c('Constant','treated'),
                   ci=F,
                   digits = 2,
                   digit.separator = "",
                   column.labels = c('White',
                                     'Hispanic',
                                     'Hispanic PSM',
                                     'White and Hispanic'),
                   column.separate = c(2,2,2,2),
                   dep.var.caption = '',
                   dep.var.labels = '',
                   keep.stat = c("n"),
                   model.numbers=FALSE,
                   model.names = FALSE)
}


stargazer_models = lapply(all_models,get_stargazer)

panel_main = star_panel(stargazer_models[[1]],
                        stargazer_models[[2]],
                        panel.names = c("Score, math and reading combined", 
                                        "Passing rate, math and reading combined"),
                        same.summary.stats = TRUE)

panel_appendix = star_panel(stargazer_models[[3]],
                            stargazer_models[[5]],
                            stargazer_models[[4]],
                            stargazer_models[[6]],
                            panel.names = c("Score, reading",
                                            "Score, math",
                                            "Passing rate, reading",
                                            "Passing rate, math"),
                            same.summary.stats = TRUE)

star_tex_write(panel_main,file='results/tables/did_panel_sixthgrade_combined.tex')
star_tex_write(panel_appendix,file='results/tables/did_panel_sixthgrade_math_reading.tex')



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


make_plot = function(t,combined){
  if(combined){
    curr_dfs = plot_dfs %>% filter(subject == 'Combined')
    file_ext = '_combined'
  } else{
    curr_dfs = plot_dfs %>% filter(subject != 'Combined') %>% filter(Type==t) %>%
    file_ext = '_math_reading'
  }
  curr_dfs %>%
    ggplot(aes(x=as.numeric(as.character(year)),
               y=Results,linetype=treated)) +
    geom_line() +
    geom_vline(xintercept=18,linetype='dashed') +
    facet_grid(subject + Type ~ control_group) +
    labs(x='Year',
         y=t) +
    theme(legend.position='bottom',
          legend.title=element_blank())
  filename = paste0('results/figures/did_trends_',t,file_ext,'.png')
  ggsave(filename,width=7,height=6)
}


# MAKE PLOT FOR COMBINED PERFORMANCE MEASURE

score = plot_dfs %>%
  filter(subject == 'Combined' & Type == 'Score') %>%
  ggplot(aes(x=as.numeric(as.character(year)),
             y=Results,linetype=treated)) +
  geom_line() +
  geom_point(size=1) +
  geom_vline(xintercept=18,linetype='dashed') +
  facet_wrap(~control_group,nrow=1) +
  coord_cartesian(ylim=c(1550,1800)) +
  labs(x='',
       y='Score') +
  theme(legend.position='bottom',
        axis.text.x=element_blank(),
        legend.title=element_blank())

passed = plot_dfs %>%
  filter(subject == 'Combined' & Type == 'Passed') %>%
  ggplot(aes(x=as.numeric(as.character(year)),
             y=Results,linetype=treated)) +
  geom_line() +
  geom_point(size=1) +
  geom_vline(xintercept=18,linetype='dashed') +
  facet_wrap(~control_group,nrow=1) +
  coord_cartesian(ylim=c(0.55,1)) +
  labs(x='Year',
       y='% Passed') +
  theme(legend.position='bottom',
        strip.text.x = element_blank(),
        strip.background = element_blank(),
        legend.title=element_blank())


ggarrange(score,passed, nrow=2, common.legend = TRUE, legend="bottom")
ggsave('results/figures/did_trends_combined.png',width=7,height=5)

make_plot = function(t){
  if(t == 'Passed'){
    ylims = c(0.5,1)
  } else{
    ylims = c(1500,1800)
  }
  file_ext = '_math_reading'
  plot_dfs %>% 
    filter(subject != 'Combined') %>% 
    filter(Type==t) %>%
    ggplot(aes(x=as.numeric(as.character(year)),
               y=Results,linetype=treated)) +
    geom_line() +
    geom_point(size=1) +
    geom_vline(xintercept=18,linetype='dashed') +
    facet_grid(subject ~ control_group) +
    coord_cartesian(ylim=ylims) +
    labs(x='Year',
         y=t) +
    theme(legend.position='bottom',
          legend.title=element_blank())
  filename = paste0('results/figures/did_trends_',t,file_ext,'.png')
  ggsave(filename,width=7,height=5)
}


type = c('Passed','Score')
lapply(type,make_plot)



