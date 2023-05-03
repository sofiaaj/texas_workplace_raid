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

sg = fread('data/safegraph/weekly_patterns/safegraph_staar.csv')

match_data = sg %>%
  group_by(EndDateWeek19,campus,district) %>%
  summarise(percent_hispanic = mean(percent_hispanic),
            percent_lep = mean(percent_lep),
            percent_white = mean(percent_white),
            percent_econdis = mean(percent_econdis),
            num_students = mean(num_students)) %>%
  mutate(treated = ifelse(district == 43901,1,0))

mod_match <- matchit(treated ~ percent_hispanic + percent_lep + percent_econdis,
                     method = 'nearest',
                     ratio=1,
                     data = match_data)

sg_df = sg %>% filter(campus %in% match.data(mod_match)$campus)

sg_df %>%
  mutate(treated = ifelse(district == 43901,"Treated","Control")) %>%
  filter(StartDateWeek19 != '2019-03-11') %>%
  filter(StartDateWeek19 <= '2019-04-08' & 
           StartDateWeek19 > '2019-02-24') %>%
  mutate(wday19 = lubridate::wday(Date19)) %>%
  #filter(!(wday19 %in% c(1,7))) %>%
  filter(location_name != 'Dr E T Boon El') %>%
  group_by(treated,EndDateWeek19) %>%
  summarise(visits18 = sum(visits18scale),
            visits19 = sum(visits19scale)) %>%
  mutate(change = visits19 - visits18,
         change = change/visits18) %>%
  ggplot(aes(x=EndDateWeek19,y=change,linetype=treated)) +
  geom_line() +
  geom_point() +
  theme(legend.title=element_blank(),legend.position='bottom') +
  labs(x='Date',y='Year-over-year change in number of visits') +
  geom_vline(xintercept = as.numeric(as.Date("2019-04-02")), 
             linetype=4) +
  coord_cartesian(ylim=c(-0.3,0.3))
ggsave('results/figures/safegraph_visit_trends.png',width=7.5,height=5)

event_df = sg_df %>%
  filter(StartDateWeek19 != '2019-03-11') %>%
  filter(StartDateWeek19 <= '2019-04-08' & 
           StartDateWeek19 > '2019-02-24') %>%
  mutate(wday19 = lubridate::wday(Date19)) %>%
  group_by(campus,district,EndDateWeek19) %>%
  summarise(visits18 = sum(visits18scale),
            visits19 = sum(visits19scale)) %>%
  mutate(change = visits19 - visits18,
         change = change/visits18) %>%
  mutate(Time = as.factor(EndDateWeek19)) %>%
  mutate(Time = as.numeric(Time)) %>%
  mutate(campus = as.factor(campus),
         treat= ifelse(district == 43901,1,0),
         time_to_treat = ifelse(treat==1,Time-5,0))

es = feols(change ~ i(time_to_treat,treat,ref=-1) |
               campus + Time,
             cluster = ~campus,
             data=event_df)

es_table = etable(es, tex = TRUE)
star_tex_write(es_table,file='results/tables/foot_traffic_effects.tex')

event_df_temp = event_df %>% 
  mutate(treated = ifelse(district == 43901 & Time >= 5,1,0)) %>%
  mutate(campus = as.factor(campus),
         year = as.factor(Time))
mod = lm(change ~ treated + campus + year,data=event_df_temp)

ggiplot(es,
        ci_level=0.95,
        main="",
        xlab="Week",
        ylab="Year-over-year change in foot traffic (2018-2019)",
        theme=theme(text=element_text(family="Georgia",size=8))) +
  coord_cartesian(ylim=c(-0.5,0.5))+
  geom_line()
ggsave('results/figures/safegraph_event_study.png',width=7.5,height=6)
