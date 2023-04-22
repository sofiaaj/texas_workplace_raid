library(haven)
library(dplyr)
library(scales)
library(data.table)
library(tidyr)
library(gmapsdistance)
library(R.utils)
##set.api.key(key="")

acs = fread('data/acs/acs2019.csv.gz')

# Takes raw STAAR campus-level STAAR test data from 5th, 6th, and 7th grade 
# and returns a clean file with relevant information
# raw data: https://tea.texas.gov/student-assessment/testing/staar/staar-aggregate-data

# Root directory with data folder
setwd("~/Desktop/texas_workplace_raid")

# Reading list of relevant variables and new names for cleaning purposes
vars12_15 <- fread('data/variables/variable_list_2012-2015.csv')
vars16 <- fread('data/variables/variable_list_2016.csv')
vars17_19 <- fread('data/variables/variable_list_2017-2019.csv')


# opens file, selects relevant variables, and renames
open <- function(filepath,l,y,g){
  print(y)
  if(y %in% c('12','13','14','15')){
    vars <- vars12_15
  } else if(y == '16'){
    vars <- vars16
  } else {
    vars <- vars17_19
  }
  df <- read_sas(filepath) %>%
    dplyr::select(vars$variable) %>%
    purrr::set_names(vars$new_name) %>%
    mutate(language = l,
           year = y,
           grade = g)
  return(df)
}


# Reshapes data so variables relating to math and reading performance are in separate rows
process <- function(type,df){
  gen_vars <- c(names(campus)[1:7],'language')
  # math and reading test variables all contain the respective test name
  reading_vars <- c(gen_vars,colnames(df)[grepl('reading', colnames(df), fixed = TRUE)])
  math_vars <- c(gen_vars,colnames(df)[grepl('math', colnames(df), fixed = TRUE)])
  
  # split subjects into different rows
  reading <- df %>% select(reading_vars) %>% mutate(subject = 'reading')
  colnames(reading) <-  sub("_reading", "", colnames(reading))
  
  math <- df %>% select(math_vars) %>% mutate(subject = 'math')
  colnames(math) <-  sub("_math", "", colnames(math))
  
  df <- rbind(math,reading)
  
  group_vars <- colnames(df)[grepl('hisp|white|lep|lep1|lep2|esl2|bil2|bil|bil5|esl', colnames(df))]
  vars <- c(gen_vars,group_vars,'subject')
  df <- df %>% select(vars)
  return(df)
}

# reshape data to have groups into different rows
split_groups <- function(df){
  vars = c(names(df)[1:7],'language','subject')
  df <- df %>% 
    gather(variable_name,value,
           -any_of(vars)) %>% 
    separate(variable_name, into = c("variable", "group")) %>%
    mutate(complete = ifelse(variable == 'num' & value >= 5,1,0)) %>%
    group_by(campus,year,grade,language,subject,group) %>%
    mutate(complete = sum(complete)) %>%
    filter(complete == 1) %>%
    ungroup() %>%
    unique() %>%
    spread(variable,value) %>%
    mutate(passed = (num-notpass)/num,
           mastered = masters/num)
}


campus <- data.frame()
grades <- as.character(5:7)
years <- as.character(12:19) 
language <- c('s','e')
for(g in grades){
  for(y in years){
    for(l in language){
      if(!(g %in% c('6','7') & l == 's')){ # no spanish test in 6th grade
        fy = paste0('20',y)
        file = paste0('data/campus/',fy,'/cfy',y,l,g,'.sas7bdat')
        curr <- open(file,l,y,g)
        campus <- rbind(campus,curr) 
      }
    }
  }
}

campus <- process('campus',campus)
campus <- split_groups(campus)

# ADD DEMOGRAPHIC DATA
# Obtain STAAR test data and variable list for demographic data
# Data can be obtained from: 
# https://tea.texas.gov/texas-schools/accountability/academic-accountability/performance-reporting/texas-academic-performance-reports

df <- campus %>%
  mutate(year = as.character(year))

v <- fread('data/variables/variable_list_school_data.csv')

open <- function(filepath,type,y){
  vars <- v %>% filter(file == type)
  if(type == 'attendance'){
    vars <- vars %>% filter(year == y)
  }
  df <- read.csv(filepath) %>%
    dplyr::select(vars$variable) %>%
    purrr::set_names(vars$new_name) %>%
    mutate(year = y)
  return(df)
}

process <- function(t){
  campus <- data.frame()
  years <- as.character(12:19)
  for(y in years){
    print(y)
    fy = paste0('20',y)
    file = paste0('data/campus/',fy,'/campus_',t,'_',y,'.dat')
    curr <- open(file,t,y)
    campus <- rbind(campus,curr)
  }
  return(campus)
}

# Relevant covariates relate to school demographics and attendance data
demo <- process('demo')
attendance <- process('attendance')
preds <- merge(demo,
               attendance,
               by=c('campus','year'),
               all.x=TRUE) %>%
  mutate(year = as.character(year))


df <- merge(df %>% mutate(campus = as.numeric(campus)),
            preds,
            by=c('campus','year'),all.x=TRUE)

df[df == "-1"] <- NA
df[df == "."] <- NA

# Remove schools that don't appear on all 5 years (2015-2019). These are schools that closed down. 

open = df %>% 
  filter(year>=15) %>% 
  group_by(campus,year) %>% 
  summarise(count=n()) %>% 
  group_by(campus) %>% 
  summarise(count=n()) %>% 
  filter(count == 5) %>%
  pull(campus)

df = df %>% filter(campus %in% open)

# add district information

district_data <- fread('data/district/district_information.csv')
df = df %>% mutate(district=as.numeric(district))
df <- merge(df,district_data,by='district')

#Takes campus-level data and adds information on:
# Campus latitude and longitude
# Distance to place of the raid

vars = fread('data/variables/variable_list_geography.csv') %>% mutate(year = as.character(year))
get_geo = function(yr){
  print(yr)
  curr_vars = vars %>% filter(year == yr)
  filename = paste0('data/campus/geography/geography_',yr,'.csv')
  df = fread(filename) %>%
    select(curr_vars$variable) %>%
    purrr::set_names(curr_vars$new_name) %>%
    mutate(zipcode = as.character(zipcode),
           campus = stringr::str_replace(campus,'\'',''),
           campus = as.integer(campus),
           year = yr)
  return(df)
}
geo_dfs = lapply(as.character(2014:2018),get_geo)

geo_campus = bind_rows(geo_dfs) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(campus) %>%
  top_n(1, year) %>%
  select(-year)


# APRIL 5, 2023 AT 8 AM (because time must be in the future)
get_dist_time = function(x){
  print(x)
  result = gmapsdistance(x,"33.085883+-96.6812401",mode='driving',
                         dep_date="2023-04-05",
                         dep_time="08:00:00",
                         traffic_model="best_guess")
  return(cbind(x,result[[1]],result[[2]]))
}


geo_campus = geo_campus %>%
  mutate(latlong = paste(y,x,sep="+"))

latlong = geo_campus %>%
  pull(latlong)

get_results = function(start){
  print(start)
  end = start+500
  start = start+1
  curr_latlong = latlong[start:end]
  print(length(curr_latlong))
  results = lapply(curr_latlong,get_dist_time)
  distance_time = as.data.frame(matrix(unlist(results),ncol=3,byrow=TRUE))
  names(distance_time) = c('latlong','distance_from_raid','time_from_raid')
  return(distance_time)
}


ranges = seq(0,length(latlong), by=500)
# gmaps function freezes when sent too many requests
distances = lapply(ranges,get_results)

# if latlong repeated, assume best driving time
distance_time = bind_rows(distances) %>% 
  na.omit() %>% 
  unique() %>%
  group_by(latlong) %>%
  top_n(1, time_from_raid)
geo_campus = merge(geo_campus,distance_time,by='latlong',all.x = TRUE)
fwrite(geo_campus,'data/campus/geography/full_geography_info2.csv')


df = merge(df %>% select(-cname),geo_campus,by='campus')
names(df) = tolower(names(df))
fwrite(df,'data/campus/staar_data_full.csv')


