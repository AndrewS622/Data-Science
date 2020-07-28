## Exploring NHANES Health Data
# dplyr basics
library(dplyr)
library(NHANES)
data(NHANES)

# filtering, summarize, removal of NA
tab <- NHANES %>% 
  filter(AgeDecade == " 20-29" & Gender == "female")

ref <- NHANES %>% 
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))

ref_avg <- NHANES %>%
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm=TRUE)) %>% .$average

NHANES %>%
  filter(AgeDecade == " 20-29"  & Gender == "female") %>% 
  summarize(minbp = min(BPSysAve, na.rm = TRUE), maxbp = max(BPSysAve, na.rm = TRUE))

NHANES %>%
  filter(Gender == "female") %>% 
  group_by(AgeDecade) %>% 
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))

NHANES %>% 
  filter(Gender == "male") %>% group_by(AgeDecade) %>% 
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))

# grouping
NHANES %>% 
  group_by(AgeDecade, Gender) %>% 
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))

# arrange
NHANES %>% 
  filter(Gender == "male" & AgeDecade == " 40-49") %>% 
  group_by(Race1) %>% 
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE)) %>% 
  arrange(average)