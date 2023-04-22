library(dplyr)
library(data.table)
library(tidyr)
library(ggiplot)
library(ggplot2)
library(stargazer)
library(knitr)
library(kableExtra)
library(stringr)
library(ggpubr)
library(janitor)
library(sjmisc)
library(ggmap)
library(ggsn)
library('starpolishr')

theme <- theme_bw() + theme(text = element_text(family = "Georgia",size=10)) 
theme_set(theme)

# Makes tables and maps to compare treated and control groups
setwd("~/Desktop/texas_workplace_raid")
dfs = readRDS('data/analysis/all_data_controls_groups.rds')

#Descriptive tables
region = dfs[[2]] %>% mutate(psm = "Regional")
psm = dfs[[3]] %>% mutate(psm = "PSM")
df = rbind(region,psm)

table = df %>%
  filter(subject == 'reading') %>%
  filter(!(district == 43901 & psm == "PSM")) %>%
  mutate(counter = ifelse(year == 15,1,0),
         type = case_when(
           district == 43901 ~ "Allen",
           psm == "PSM" ~ "PSM",
           TRUE ~ "Regional"
         )) %>%
  group_by(type) %>%
  summarise(num_schools = sum(counter),
            num_observations = n(),
            avg_hispanic = mean(percent_hispanic),
            avg_lep = mean(percent_lep),
            avg_econdis = mean(percent_econdis),
            avg_attendance = mean(attendance_rate)) %>%
  rotate_df() %>%
  row_to_names(row_number = 1) %>%
  mutate_if(is.character,list(as.numeric)) %>%
  mutate(across(1:3,round,2)) %>%
  select(Allen,Regional,PSM) %>%
  kable('latex',booktabs=T)

writeLines(table, 'descriptive_information/tables/treated_control_schools.tex')

allen = df %>% 
  filter(district == 43901 & subject == 'math') %>%
  ungroup() %>%
  select(x,y) %>%
  mutate(type = 'Elementary Schools Allen ISD') %>%
  rbind(.,data.frame(x=-96.6812401,
                     y=33.085883,
                     type="Site of raid"))
left = min(allen$x)-0.01
right = max(allen$x)+0.01
bottom = min(allen$y)-0.01
top = max(allen$y)+0.01
map = get_map(location = c(left,bottom,right,top),
              source="stamen",
              color="bw")
ggmap(map) +
  geom_point(data=allen,mapping=aes(x=x,y=y,shape=type,size=type)) +
  labs(x="",y="") +
  scale_shape_manual(values=c(17,8)) +
  scale_size_manual(values=c(4,5)) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position='bottom',
        legend.direction="horizontal",
        legend.title=element_blank(),
        text = element_text(family = "Georgia",size=12))

ggsave('descriptive_information/figures/allen_school_map.png')


left = min(df$x)-0.2
right = max(df$x)+0.2
bottom = min(df$y)-0.2
top = max(df$y)+0.2
map = get_map(location = c(left,bottom,right,top),
              source="stamen",
              maptype="terrain",
              color="bw")
df = df %>% mutate(allen = ifelse(district != 43901,"Control schools",
                                  "Schools in Allen"),
                   psm = factor(psm,levels=c('Regional','PSM')),
                   psm = case_when(
                     psm == 'Regional' ~ 'All schools in neighboring regions',
                     TRUE ~ 'Matched schools'
                   ))
ggmap(map) +
  geom_point(data=df,mapping=aes(x=x,y=y,shape=allen)) +
  labs(x="",y="") +
  scale_shape_manual(values=c(15,17)) +
  facet_wrap(~psm) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position='bottom',
        legend.direction="horizontal",
        legend.title=element_blank())

ggsave('descriptive_information/figures/treated_control_school_map.png')