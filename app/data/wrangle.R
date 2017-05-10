## ----setup, include=FALSE------------------------------------------------

library(tidyverse)
library(lubridate)
library(forcats)
library(knitr)

set.seed(8193298)

## ------------------------------------------------------------------------
nc_vhis <- read_tsv("../data/nc_voter/ncvhis_Statewide.txt") %>%
  select(county_desc:voting_method, voted_party_desc:ncid) %>%
  mutate(election_lbl = mdy(election_lbl)) %>%
  filter(year(election_lbl) %in% c(2016, 2014, 2012)) %>%
  mutate(new_id = paste0(voter_reg_num, ncid))

nc_voter <- read_tsv("../data/nc_voter/ncvoter_Statewide.txt") %>%
  filter(status_cd == "A" | status_cd == "S") %>%
  select(county_desc:voter_reg_num,
         zip_code,
         race_code:birth_age,
         registr_dt:precinct_desc,
         ncid) %>%
  mutate(new_id = paste0(voter_reg_num, ncid))

# nc <- nc_voter %>%
#   left_join(nc_vhis, by = c("new_id" = "new_id")) %>%
#   select(county_desc.x,
#          election_lbl:pct_description,
#          new_id,
#          zip_code:registr_dt) %>%
#   rename(county = county_desc.x,
#          party_code = party_cd,
#          age = birth_age) %>% 
#   mutate(registr_dt = mdy(registr_dt)) %>% 
  mutate(race = recode(race_code,
                       "B" = "Black",
                       "W" = "White",
                       .default = "Other"),
         race = ifelse(ethnic_code == "HL", "Hispanic", race),
         gender = recode(gender_code,
                         "F" = "Female",
                         "M" = "Male",
                         "U" = NA_character_),
         hispanic = recode(ethnic_code,
                           "HL" = "Yes",
                           "NL" = "No",
                           "UN" = NA_character_),
         registered = "Yes",
         voted = ifelse(is.na(voting_method), "No", "Yes"),
         source = "NC")

get_age <- function(year, age2016) {
  return(age2016 - 2016 + year)
}

make_election_tbl <- function(elec_date){
  reg_date <- elec_date - 25
  
  # All elligible voters in the registration file
  elligible <- nc_voter %>%
    mutate(registr_dt = mdy(registr_dt),
           age = get_age(year(elec_date), birth_age)) %>% 
    filter(registr_dt <= reg_date)
  
  # All ballots cast in the November 2016 election
  voted <- nc_vhis %>% 
    filter(election_lbl == elec_date)
  
  # Join with NA for no vote
  nc <- elligible %>% 
    left_join(voted, by = c("new_id" = "new_id")) %>% 
    mutate(race = recode(race_code,
                         "B" = "Black",
                         "W" = "White",
                         .default = "Other"),
           race = ifelse(ethnic_code == "HL", "Hispanic", race),
           gender = recode(gender_code,
                           "F" = "Female",
                           "M" = "Male",
                           "U" = NA_character_),
           hispanic = recode(ethnic_code,
                             "HL" = "Yes",
                             "NL" = "No",
                             "UN" = NA_character_),
           registered = "Yes",
           voted = ifelse(is.na(voting_method), "No", "Yes"),
           source = "NC")
  
  return(nc)
}




size <- 1e5

nc_2012 <- make_election_tbl(ymd("2012-11-06")) %>%
  select(age, race, gender, hispanic, registered, voted, source) %>% 
  sample_n(size)

nc_2014 <- make_election_tbl(ymd("2014-11-04")) %>% 
  select(age, race, gender, hispanic, registered, voted, source) %>% 
  sample_n(size)

nc_2016 <- make_election_tbl(ymd("2016-11-08")) %>%
  select(age, race, gender, hispanic, registered, voted, source) %>% 
  sample_n(size)

# rm(nc_vhis, nc_voter)
rm(size)

## ------------------------------------------------------------------------
#setwd("/Users/alisakwok/Desktop/math241/nc-elections-project")
# purl("../wrangle/cps.Rmd", output = "../cps.R") %>% source(chdir = TRUE)
# file.remove("../cps.R")
# 
# cps_sample <- cps %>%
#   mutate(WTFINL = as.numeric(WTFINL)) %>%
#   sample_n(size = nrow(cps), replace = TRUE, weight = WTFINL)
# 
# rm(cps)
# 
# cps_2012_sample_ge <- cps_sample %>%
#   filter(YEAR == "2012", VOTEREG == "Voted") %>%
#   mutate(AGE = as.numeric(AGE, na.rm = TRUE),
#          SEX = recode(SEX, "Male" = "M", "Female" = "F"))
# 
# cps_2012_sample <- cps_sample %>%
#   filter(YEAR == "2012", VOTEREG %in% c("Voted", "Registered, did not vote")) %>%
#   mutate(AGE = as.numeric(AGE, na.rm = TRUE),
#          SEX = recode(SEX, "Male" = "M", "Female" = "F"))
# 
# cps_2014_sample <- cps_sample %>%
#   filter(YEAR == "2014", VOTEREG %in% c("Voted", "Registered, did not vote")) %>%
#   mutate(AGE = as.numeric(AGE, na.rm = TRUE),
#          SEX = recode(SEX, "Male" = "M", "Female" = "F"))
# 
# rm(cps_sample)

## ------------------------------------------------------------------------
cces_2012 <- read_tsv("../data/cces/CCES12_Common_VV.tab.tsv") %>%
  filter(inputstate == 37) %>%
  mutate(birthyr = paste(birthyr, "-01-01", sep = ""),
         age = as.numeric(as.duration(ymd(birthyr) %--% mdy("01-01-2012")), "years"),
         race = as.factor(recode(race,
                           `1` = "White",
                           `2` = "Black",
                           `3` = "Hispanic" ,
                           .default = "Other")),
         gender = as.factor(recode(gender,
                               `1` = "Male",
                               `2` = "Female")),
         hispanic = as.factor(recode(hispanic,
                               `1` = "Yes",
                               `0` = "No")),
         votereg_post = as.factor(recode(votereg_post,
                               `1` = "Yes",
                               `2` = "No",
                               `3` = "Don't know",
                               .default = NA_character_,
                               .missing = NA_character_)),
         CC401 = as.factor(recode(CC401,
                        `1` = "No",
                        `2` = "No",
                        `3` = "No",
                        `4` = "No",
                        `5` = "Yes",
                        .default = NA_character_,
                        .missing = NA_character_))) %>% 
  rename(voted = CC401, registered = votereg_post) %>% 
  select(age, race, gender, hispanic, registered, voted, weight_vv_post)
  
cces_2014 <- read_tsv("../data/cces/CCES14_Common_Content_Validated.tab.tsv") %>%
  filter(inputstate == 37) %>% # 37 = North Carolina
  mutate(birthyr = paste(birthyr, "-01-01", sep = ""),
         age = as.numeric(as.duration(ymd(birthyr) %--% mdy("01-01-2014")), "years"),
         race = as.factor(recode(race,
                           `1` = "White",
                           `2` = "Black",
                           `3` = "Hispanic" ,
                           .default = "Other")),
         gender = as.factor(recode(gender,
                               `1` = "Male",
                               `2` = "Female")),
         hispanic = as.factor(recode(hispanic,
                               `1` = "Yes",
                               `0` = "No")),
         registered = recode(votereg,
                             `1` = "Yes",
                             `2` = "No",
                             `3` = "Don't know",
                             .default = NA_character_,
                             .missing = NA_character_),
         voted = recode(CC401,
                        `1` = "No",
                        `2` = "No",
                        `3` = "No",
                        `4` = "No",
                        `5` = "Yes",
                        .default = NA_character_,
                        .missing = NA_character_)
  ) %>% 
  select(age, race, gender, hispanic, registered, voted, weight)

cces_2016 <- read_tsv("../data/cces/CCES2016_Common_FirstRelease.tab.tsv") %>%
  filter(inputstate == 37) %>% # 37 = North Carolina
  mutate(gender = as.factor(recode(gender,
                                   `1` = "Male",
                                   `2` = "Female")),
         birthyr = paste(birthyr, "-01-01", sep = ""),
         age = as.numeric(as.duration(ymd(birthyr) %--% mdy("01-01-2016")), "years"),
         race = recode(race,
                       `1` = "White",
                       `2` = "Black",
                       `3` = "Hispanic",
                       `98` = NA_character_,
                       `99` = NA_character_,
                       .missing = NA_character_,
                       .default = "Other"),
         hispanic = recode(hispanic,
                           `1` = "Yes",
                           `2` = "No",
                           .default = NA_character_,
                           .missing = NA_character_),
         registered = recode(votereg,
                             `1` = "Yes",
                             `2` = "No",
                             `3` = "Don't know",
                             .default = NA_character_,
                             .missing = NA_character_),
         voted = recode(CC16_401,
                        `1` = "No",
                        `2` = "No",
                        `3` = "No",
                        `4` = "No",
                        `5` = "Yes",
                        .default = NA_character_,
                        .mssing = NA_character_)
         ) %>% 
  select(age, race, gender, hispanic, registered, voted, commonweight_post)

cces_2012_sample <- cces_2012 %>%
  filter(!is.na(weight_vv_post), registered == "Yes") %>%
  sample_n(size = sum(cces_2012$registered == "Yes", na.rm = TRUE),
           replace = TRUE, weight = weight_vv_post) %>% 
  select(-weight_vv_post) %>% 
  mutate(source = "CCES")

rm(cces_2012)

cces_2014_sample <- cces_2014 %>%
  filter(!is.na(weight), registered == "Yes") %>%
  sample_n(size = sum(cces_2014$registered == "Yes", na.rm = TRUE),
           replace = TRUE, weight = weight) %>% 
  select(-weight) %>% 
  mutate(source = "CCES")

rm(cces_2014)

cces_2016_sample <- cces_2016 %>%
  filter(!is.na(commonweight_post), registered == "Yes") %>%
  sample_n(size = sum(cces_2016$registered == "Yes", na.rm = TRUE),
           replace = TRUE, weight = commonweight_post) %>% 
  select(-commonweight_post) %>% 
  mutate(source = "CCES")

rm(cces_2016)

all_2012 <- bind_rows(nc_2012, cces_2012_sample) %>% 
  filter(age >= 18)

all_2014 <- bind_rows(nc_2014, cces_2014_sample) %>% 
  filter(age >= 18)

all_2016 <- bind_rows(nc_2016, cces_2016_sample) %>% 
  filter(age >= 18)

# rm(cces_2012_sample, cces_2014_sample, cces_2016_sample,
#    nc_2012, nc_2014, nc_2016)

save(all_2012, all_2014, all_2016 , file = "data/wrangled.RData")

## ------------------------------------------------------------------------
# lcolors <- c("CPS" = "#51acb4", "CCES" = "#8d8d8d", "VOTER" = "#ff6767")
# ltypes <- c("M" = 2, "F" = 1)

# cces_2014_sample_ge %>%
#   ggplot(aes(x = age)) +
#   geom_line(aes(color = "CCES"), stat = "density") +
#   geom_line(data = cps_2012_sample_ge, aes(x = AGE, color = "CPS"), stat = "density") +
#   geom_line(data = nc_2012_ge, aes(x = age, color = "VOTER"), stat = "density") +
#   geom_vline(aes(xintercept = median(nc_2012_ge$age), color = "VOTER"), alpha = 0.5) +
#   geom_vline(aes(xintercept = median(cps_2012_sample_ge$AGE), color = "CPS"), alpha = 0.5) +
#   geom_vline(aes(xintercept = median(cces_2014_sample_ge$age), color = "CCES"), alpha = 0.5) +
#   scale_color_manual(name = "Data", values = lcolors) +
#   labs(title = "2012 General Election: NC voter age distributions, \n reported by the CCES, CPS, and voter file", x = "Age", y = "Density") +
#   theme(plot.title = element_text(hjust = 0.5))

## ------------------------------------------------------------------------
# nc_2012_gen <- nc_2012 %>%
#   filter(gender_code %in% c("M", "F")) %>%
#   distinct(new_id, .keep_all = TRUE)
# 
# rm(nc_2012)
# 
# nc_2014_gen <- nc_2014 %>%
#   filter(gender_code %in% c("M", "F")) %>%
#   distinct(new_id, .keep_all = TRUE)
# 
# rm(nc_2014)
# 
# nc_2016_gen <- nc_2016 %>%
#   filter(gender_code %in% c("M", "F")) %>%
#   distinct(new_id, .keep_all = TRUE)
# 
# rm(nc_2016)

## ------------------------------------------------------------------------
# nc_2012_gen %>%
#   ggplot(aes(x = age)) +
#   geom_line(aes(color = "VOTER", linetype = gender_code), stat = "density") +
#   geom_line(data = cps_2012_sample, aes(x = AGE, color = "CPS", linetype = SEX), stat = "density") +
#   geom_line(data = cces_2012_sample, aes(x = age, color = "CCES", linetype = gender), stat = "density") +
#   geom_vline(aes(xintercept = median(nc_2012_gen$age), color = "VOTER"), alpha = 0.5) +
#   geom_vline(aes(xintercept = median(cps_2012_sample$AGE), color = "CPS"), alpha = 0.5) +
#   geom_vline(aes(xintercept = median(cces_2012_sample$age), color = "CCES"), alpha = 0.5) +
#   scale_color_manual(name = "Data", values = lcolors) +
#   scale_linetype_manual(name = "Gender", values = ltypes) +
#   labs(title = "2012 NC voter age distributions by gender, \n reported by the CCES, CPS, and voter file", x = "Age", y = "Density") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# nc_2014_gen %>%
#   ggplot(aes(x = age)) +
#   geom_line(aes(color = "VOTER", linetype = gender_code), stat = "density") +
#   geom_line(data = cps_2014_sample, aes(x = AGE, color = "CPS", linetype = SEX), stat = "density") +
#   geom_line(data = cces_2014_sample, aes(x = age, color = "CCES", linetype = gender), stat = "density") +
#   geom_vline(aes(xintercept = median(nc_2014_gen$age), color = "VOTER"), alpha = 0.5) +
#   geom_vline(aes(xintercept = median(cps_2014_sample$AGE), color = "CPS"), alpha = 0.5) +
#   geom_vline(aes(xintercept = median(cces_2014_sample$age), color = "CCES"), alpha = 0.5) +
#   scale_color_manual(name = "Data", values = lcolors) +
#   scale_linetype_manual(name = "Gender", values = ltypes) +
#   labs(title = "2014 NC voter age distributions by gender, \n reported by the CCES, CPS, and voter file", x = "Age", y = "Density") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# nc_2016_gen %>%
#   ggplot(aes(x = age)) +
#   geom_line(aes(color = "VOTER", linetype = gender_code), stat = "density") +
#   geom_line(data = cces_2016_sample, aes(x = age, color = "CCES", linetype = gender), stat = "density") +
#   geom_vline(aes(xintercept = median(nc_2014_gen$age), color = "VOTER"), alpha = 0.5) +
#   geom_vline(aes(xintercept = median(cces_2014_sample$age), color = "CCES"), alpha = 0.5) +
#   scale_color_manual(name = "Data", values = lcolors) +
#   scale_linetype_manual(name = "Gender", values = ltypes) +
#   labs(title = "2016 NC voter age distributions by gender, \n reported by the CCES and voter file", x = "Age", y = "Density") +
#   theme(plot.title = element_text(hjust = 0.5))

