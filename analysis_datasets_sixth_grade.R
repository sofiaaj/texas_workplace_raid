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
library('starpolishr')

theme <- theme_bw() + theme(text = element_text(family = "Georgia",size=8)) 
theme_set(theme)

setwd("~/Desktop/texas_workplace_raid")

# Create datasets to be used for 6th grade analysis

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
  filter(grade == 6 & !(time_from_raid < 27.7 & district != 43901)) %>%
  group_by(district,campus,year) %>% 
  mutate(count=n()) %>% 
  # only keep Allen schools that have observations for Hispanic and white students
  filter(!(district == 43901 & count == 2)) %>% 
  # make sure we have data for math and reading 
  # (some campuses only have complete reading data, for example)
  group_by(district,campus,year,group) %>%
  mutate(count=n()) %>%
  filter(count == 2)

white_df = full_df %>% filter(dname == 'ALLEN ISD')

region_df = full_df %>% 
  filter(group == 'hisp' & region %in% c(7,8,10,11,12) & nces_district == 21)

# PSM procedure
match_data = region_df %>%
  group_by(campus,district) %>%
  summarise(percent_hispanic = mean(percent_hispanic),
            percent_econdis = mean(percent_econdis),
            percent_lep = mean(percent_lep),
            percent_white = mean(percent_white),
            num_students = mean(num_students)) %>%
  mutate(treated = ifelse(district == 43901,1,0))

mod_match <- matchit(treated ~ percent_hispanic + percent_lep + percent_econdis,
                     method = 'nearest',
                     ratio=2,
                     data = match_data)

psm_df = region_df %>% filter(campus %in% match.data(mod_match)$campus)


dfs = list(white_df,region_df,psm_df)

# Makes 3 datasets: reading, math, and combined
filter_summarise_data = function(df,type){
  if(type == 'combined'){
    df = df %>%
      group_by(campus,
               year,
               district,
               group,
               time_from_raid,
               percent_hispanic,
               percent_white,
               num_students,
               percent_lep,
               percent_econdis) %>%
      summarise(score = weighted.mean(score,num),
                passed = weighted.mean(passed,num),
                mastered = weighted.mean(mastered,num),
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
           Passed = passed,
           Mastered = mastered)
  return(df)
}

reading = lapply(dfs,filter_summarise_data,'reading')
math = lapply(dfs,filter_summarise_data,'math')
combined = lapply(dfs,filter_summarise_data,'combined')

saveRDS(dfs,'data/analysis/all_data_controls_groups.rds')
saveRDS(reading,'data/analysis/reading_sixthgrade.rds')
saveRDS(math,'data/analysis/math_sixthgrade.rds')
saveRDS(combined,'data/analysis/combined_sixthgrade.rds')

# make Safegraph dataset
sg_0 = fread('data/safegraph/weekly_patterns/full_data.csv')
staar_0 = fread('data/campus/staar_data_full.csv')

sg = sg_0 %>%
  select(placekey,
         location_name,
         latitude,
         longitude,
         street_address,
         city,
         postal_code) %>%
  rename(cname = location_name,
         y.sg = latitude,
         x.sg = longitude,
         street_address.sg = street_address,
         city.sg = city,
         zipcode = postal_code) %>%
  mutate(cname = toupper(cname),
         cname.sg = cname) %>%
  unique()

staar = staar_0 %>%
  filter(charter == 'N' & region %in% c(7,8,10,11,12)) %>%
  filter(year >= 15) %>%
  filter(!is.na(percent_hispanic) & 
           !is.na(percent_lep) & 
           !is.na(percent_econdis)) %>%
  group_by(campus,group,subject,grade) %>%
  mutate(count = n()) %>%
  filter(count == 5) %>%
  ungroup() %>%
  mutate(time_from_raid = time_from_raid / 60) %>%
  #28.5
  filter(!(time_from_raid < 27.1 & district != 43901)) %>%
  select(campus,cname,district,y,x,street_address,city,zipcode,distance_from_raid) %>%
  unique()

clean_cname = function(df){
  df = df %>%
    mutate(cname = str_replace(cname,' ELEMENTARY| ELEM',' EL'),
           cname = str_replace_all(cname,'-|/',' '),
           cname = str_replace(cname,' H S',' HS'),
           cname = str_replace(cname,' INTERMEDIATE',' INT'),
           cname = str_replace(cname,' MIDDLE SCHOOL',' MIDDLE'),
           cname = str_replace(cname,' J H| JUNIOR HIGH| JR HIGH',' MIDDLE'),
           cname = str_replace(cname,' PK',' EL'))
  return(df)
}

sg = clean_cname(sg)
staar = clean_cname(staar)

merged = merge(staar,sg,by=c('cname','zipcode'),all.x=TRUE) 
missing_staar = merged %>% 
  filter(is.na(cname.sg)) %>%
  select(1:9)

zips = missing_staar %>%
  pull(zipcode) %>%
  unique()

merged = merged %>% na.omit()
missing_sg = sg %>% filter(!(placekey %in% merged$placekey) & zipcode %in% zips)

# merge by address. if multiple addresses match, pick location with closest name
merge.address = merge(missing_staar,
                missing_sg %>% 
                  select(-cname.sg) %>%
                  rename(cname.sg = cname,
                         street_address = street_address.sg),
                by=c('zipcode','street_address')) %>%
  mutate(difference_distances = stringdist(cname.sg,cname)) %>%
  group_by(street_address) %>%
  top_n(1, -difference_distances) %>%
  ungroup()

staar_to_sg = rbind(merged %>% select(campus,placekey),
                    merge.address %>% select(campus,placekey))


sg_staar = merge(sg_0,staar_to_sg,by='placekey')

data = sg_staar %>% 
  mutate(scale = normalized_visits_by_state_scaling/raw_visit_counts) %>%
  mutate(year = year(Date)) %>%
  mutate(Date = as.Date(Date),
         DateOriginal = Date,
         Date = case_when(
           year==2018 ~ Date-as.difftime(1,unit='days'),
           year==2019~Date)) %>%
  mutate(visits_scaled = visits_by_day*scale,
         month = month(Date),
         day = format(as.Date(Date,format="%Y-%m-%d"), format = "%d")) %>%
  select(campus,Date,DateOriginal,date_range_start,date_range_end,month,year,day,
         location_name,
         city,
         visits_by_day,
         visits_scaled)

data19 = data %>% 
  filter(year == 2019) %>% 
  select(-year) %>% 
  rename(Date19 = Date,
         StartDateWeek19 = date_range_start,
         EndDateWeek19 = date_range_end,
         visits19 = visits_by_day,
         visits19scale = visits_scaled) %>%
  mutate(StartDateWeek19 = format(StartDateWeek19,"%Y-%m-%d"),
         EndDateWeek19 = format(EndDateWeek19,"%Y-%m-%d")) %>%
  unique()

data18 = data %>% 
  filter(year == 2018) %>% 
  select(-year) %>% 
  rename(Date18 = Date,
         OriginalDate18 = DateOriginal,
         StartDateWeek18 = date_range_start,
         EndDateWeek18 = date_range_end,
         visits18 = visits_by_day,
         visits18scale = visits_scaled) %>%
  mutate(StartDateWeek18 = format(StartDateWeek18,"%Y-%m-%d"),
         EndDateWeek18 = format(EndDateWeek18,"%Y-%m-%d")) %>%
  unique()

data.merge = merge(data19,
                   data18,
                   by=c('month',
                        'day',
                        'location_name',
                        'city',
                        'campus'))

staar_controls = staar_0 %>%
  filter(year >= 15) %>%
  group_by(campus,district,region,charter) %>%
  summarise(num_students = mean(num_students),
            percent_hispanic = mean(percent_hispanic),
            percent_white = mean(percent_white),
            percent_lep = mean(percent_lep),
            percent_econdis = mean(percent_econdis),
            time_from_raid = mean(time_from_raid))

sg_staar_data = merge(data.merge,staar_controls,by='campus')
fwrite(sg_staar_data,'data/safegraph/weekly_patterns/safegraph_staar.csv')

