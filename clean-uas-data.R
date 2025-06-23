library(tidyverse)

# alter function to not take in charitable food
recode_demographics <- function(responses) {
  # TODO recode gender, race, etc.
  responses %>% mutate(ethnicity=ifelse(!is.na(race) & race==1 & hisplatino==1, 2, hisplatino)) %>% # 0 - not hisp/latino, 1 - hisp/latino/white, 3 - hisp/latino/non-white
    mutate(wave=factor(wave, ordered=TRUE)) %>%
    mutate(educatg=case_when(education<9 ~ 1, # less than hs
                             education==9 ~ 1, # HS/GED
                             education>9 & education<13 ~ 2, # Some college / associ
                             education>=13 ~ 3),
           educatg=factor(educatg, levels = 1:3, labels = c("HS/GED or less", "Some college", "Bachelors+"))) %>%
    # mutate(racecatg=case_when(race==1 ~ "white",
    #                           race==2 ~ "black", 
    #                           race==3 ~ "", 
    #                           race==6 ~ "mixed")) %>%
    # TODO deal with missing data
    mutate(agecatg=case_when(age<31 ~ 1, 
                             age<41 ~ 2, 
                             age<51 ~ 3,
                             age<65 ~ 4, 
                             age>=65 ~ 5, 
                             TRUE ~ NA),
           agecatg = factor(agecatg, levels = 1:5, labels = c("18-30", "31-40", "41-50", "51-64", "65+"))
    ) %>%
    # TODO deal with missing data
    mutate(hhincome = factor(hhincome,
                             levels = 1:16,
                             labels = c(
                               '1 Less than $5,000',
                               '2 5,000 to 7,499',
                               '3 7,500 to 9,999',
                               '4 10,000 to 12,499',
                               '5 12,500 to 14,999',
                               '6 15,000 to 19,999',
                               '7 20,000 to 24,999',
                               '8 25,000 to 29,999',
                               '9 30,000 to 34,999',
                               '10 35,000 to 39,999',
                               '11 40,000 to 49,999',
                               '12 50,000 to 59,999',
                               '13 60,000 to 74,999',
                               '14 75,000 to 99,999',
                               '15 100,000 to 149,999',
                               '16 150,000 or more'
                             ), ordered = TRUE)) %>%
    mutate(hhincomex = as.integer(hhincome)) %>% # convert to integer for poverty calculations
    mutate(poverty_12 = case_when(
      hhincomex %in% 1:4 ~ 1,  # Below 100% poverty line for 1 person
      hhincomex < 6 & hhmembernumber > 0 ~ 1,  # $17,240 for 2 people
      hhincomex < 7 & hhmembernumber > 1 ~ 1,  # $21,720 for 3 people
      hhincomex < 8 & hhmembernumber > 2 ~ 1,  # $26,200 for 4 people
      hhincomex < 9 & hhmembernumber > 3 ~ 1,  # $30,680 for 5 people
      hhincomex < 10 & hhmembernumber > 4 ~ 1,  # $35,160 for 6 people
      hhincomex < 10 & hhmembernumber > 5 ~ 1,  # $39,640 for 7 people
      hhincomex < 11 & hhmembernumber > 6 ~ 1,  # $44,120 for 8 people
      hhincomex < 11 & hhmembernumber > 7 ~ 1,  # $48,600 for 9 people
      hhincomex < 12 & hhmembernumber > 8 ~ 1,  # $53,080 for 10 people
      TRUE ~ NA
    )) %>%
    mutate(poverty200_12 = case_when(
      hhincomex < 8 ~ 1,  # Below 200% poverty line for 1 person
      hhincomex <= 9 & hhmembernumber > 0 ~ 1,  # $34,480 for 2 people
      hhincomex < 11 & hhmembernumber > 1 ~ 1,  # $43,440 for 3 people
      hhincomex < 12 & hhmembernumber > 2 ~ 1,  # $52,400 for 4 people
      hhincomex < 13 & hhmembernumber > 3 ~ 1,  # $61,360 for 5 people
      hhincomex < 13 & hhmembernumber > 4 ~ 1,  # $70,320 for 6 people
      hhincomex < 14 & hhmembernumber > 5 ~ 1,  # $79,280 for 7 people
      hhincomex < 14 & hhmembernumber > 6 ~ 1,  # $88,240 for 8 people
      hhincomex < 14 & hhmembernumber > 7 ~ 1,  # $97,200 for 9 people
      hhincomex < 15 & hhmembernumber > 8 ~ 1,  # $106,160 for 10 people
      TRUE ~ NA
    )) %>%
    mutate(poverty300_12 = case_when(
      hhincomex < 10 ~ 1,  # Below 300% poverty line for 1 person
      hhincomex < 12 & hhmembernumber > 0 ~ 1,  # $51,720 for 2 people
      hhincomex < 13 & hhmembernumber > 1 ~ 1,  # $65,160 for 3 people
      hhincomex < 14 & hhmembernumber > 2 ~ 1,  # $78,600 for 4 people
      hhincomex < 14 & hhmembernumber > 3 ~ 1,  # $92,040 for 5 people
      hhincomex < 15 & hhmembernumber > 4 ~ 1,  # $105,480 for 6 people
      hhincomex < 15 & hhmembernumber > 5 ~ 1,  # $118,920 for 7 people
      hhincomex < 15 & hhmembernumber > 6 ~ 1,  # $132,360 for 8 people
      hhincomex < 15 & hhmembernumber > 7 ~ 1,  # $145,800 for 9 people
      hhincomex < 16 & hhmembernumber > 8 ~ 1,  # $159,240 for 10 people
      TRUE ~ NA
    )) %>%
    mutate(
      poverty_12 = factor(poverty_12, levels = c(0, 1), labels = c("Above 100% FPL", "Below 100% FPL"), ordered = TRUE),
      poverty200_12 = factor(poverty200_12, levels = c(0, 1), labels = c("Above 200% FPL", "Below 200% FPL"), ordered = TRUE),
      poverty300_12 = factor(poverty300_12, levels = c(0, 1), labels = c("Above 300% FPL", "Below 300% FPL"), ordered = TRUE),
      povertycatg = case_when(
        poverty_12 == "Below 100% FPL" ~ 1,
        poverty_12 == "Above 100% FPL" & poverty200_12 == "Below 200% FPL" ~ 2,
        poverty_12 == "Above 100% FPL" & poverty200_12 == "Above 200% FPL" & poverty300_12 == "Below 300% FPL" ~ 3,
        poverty_12 == "Above 100% FPL" & poverty200_12 == "Above 200% FPL" & poverty300_12 == "Above 300% FPL"  ~ 4,
        TRUE ~ NA # This will be "Above 300% FPL"
      ),
      povertycatg = factor(povertycatg, levels = c(1, 2, 3, 4), labels = c("Below 100% FPL", "Below 200% FPL", "Below 300% FPL", "Above 300% FPL"), ordered = TRUE)
    ) %>% mutate(
      snap = factor(
        ei005f, 
        labels = c("SNAP", "No SNAP", "Unsure")), # 1 - SNAP use, 2 - no SNAP use, 3 - unsure
      wic = factor(
        ei005h, 
        labels = c("WIC", "No WIC", "Unsure")), # 1 - WIC use, 2 - no WIC use, 3 - unsure 
      charitablefood = ifelse(is.na(fd008e), NA, factor(
        fd008e, 
        labels = c("Food pantry", "No food pantry", "Unsure"))) # 1 - SNAP use, 2 - no SNAP use, 3 - unsure  
    ) %>% # snap, wic and food pantry use as factor
    mutate(ni=as.integer(ni)) %>% # 1 - nutrition insecure, 0 - nutrition secure
    mutate(ns=as.integer(ns)) %>% # 1 - nutrition secure, 0 -  nutrition insecure
    mutate(fi=as.integer(fi)) %>% # 1 - food insecure, 0 - food secure
    mutate(fs=as.integer(fs)) %>%
    mutate_at(dem_catgn, factor, ordered=F) %>%
    mutate_at(dem_catgo, factor, ordered=T)
  
}
