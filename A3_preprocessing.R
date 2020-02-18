#Data Viz A3: Preprocessing

library(tidyverse)
library(here)

dta_18 <- read_csv(here::here("2018_data.csv"))
dta_17 <- read_csv(here::here("2017_data.csv"))
dta_16 <- read_csv(here::here("2016_data.csv"))
dta_13 <- read_csv(here::here("2013_data.csv"))
dta_08 <- read_csv(here::here("2008_data.csv"))
dta_03 <- read_csv(here::here("2003_data.csv"))

#create a function for cleaning the data before appending
clean_data <- function(data, date, new = T) {
  if (new == T) {
    cln_data <- data %>%
      #filter for general surgery and medicine occupations in the private sector
      filter(NAICS_TITLE == "General Medical and Surgical Hospitals" |
               NAICS_TITLE == "Specialty (except Psychiatric and Substance Abuse) Hospitals" &
               OWNERSHIP == "Private") %>%
      #select the important variables
      select(NAICS_TITLE, OCC_TITLE, TOT_EMP, A_MEAN) %>%
      #mutate character variables to numeric
      mutate(TOT_EMP = as.numeric(gsub(",","",TOT_EMP)),
             A_MEAN = as.numeric(gsub(",","",A_MEAN))) %>%
      #remove duplicates
      distinct(OCC_TITLE, .keep_all = T) %>%
      #add year variable
      mutate(year = as.Date(date))
  }
  
  if (new == F) {
    cln_data <- data %>%
      #filter for general surgery and medicine occupations in the private sector
      filter(NAICS_TITLE == "General Medical and Surgical Hospitals" |
               NAICS_TITLE == "Specialty (except Psychiatric and Substance Abuse) Hospitals") %>%
      #select the important variables
      select(NAICS_TITLE, OCC_TITLE, TOT_EMP, A_MEAN) %>%
      #mutate character variables to numeric
      mutate(TOT_EMP = as.numeric(gsub(",","",TOT_EMP)),
             A_MEAN = as.numeric(gsub(",","",A_MEAN))) %>%
      #remove duplicates
      distinct(OCC_TITLE, .keep_all = T) %>%
      #add year variable
      mutate(year = as.Date(date))
  }
  
  return(cln_data)
}

cln_18 <- clean_data(dta_18, date = "2018/05/01", new = T)
cln_17 <- clean_data(dta_17, date = "2017/05/01", new = T)
cln_16 <- clean_data(dta_16, date = "2016/05/01", new = T)
cln_13 <- clean_data(dta_13, date = "2013/05/01", new = T)
cln_08 <- clean_data(dta_08, date = "2008/05/01", new = F)
cln_03 <- clean_data(dta_03, date = "2003/05/01", new = F)

yoy_long <- bind_rows(cln_18, cln_17, cln_16)

five_long <- bind_rows(cln_18, cln_13, cln_08, cln_03)

yoy_wide <- yoy_long %>%
  unite(employment_salary, TOT_EMP:A_MEAN, sep = "_") %>%
  pivot_wider(names_from = year, values_from = employment_salary) %>%
  separate("2018-05-01", into = c("emp2018", "salary2018")) %>%
  separate("2017-05-01", into = c("emp2017", "salary2017")) %>%
  separate("2016-05-01", into = c("emp2016", "salary2016")) %>%
  mutate(emp2018 = as.numeric(emp2018),
         emp2017 = as.numeric(emp2017),
         emp2016 = as.numeric(emp2016),
         salary2018 = as.numeric(salary2018),
         salary2017 = as.numeric(salary2017),
         salary2016 = as.numeric(salary2016))

five_wide <- five_long %>%
  unite(employment_salary, TOT_EMP:A_MEAN, sep = "_") %>%
  pivot_wider(names_from = year, values_from = employment_salary) %>%
  separate("2018-05-01", into = c("emp2018", "salary2018")) %>%
  separate("2013-05-01", into = c("emp2013", "salary2013")) %>%
  separate("2008-05-01", into = c("emp2008", "salary2008")) %>%
  separate("2003-05-01", into = c("emp2003", "salary2003")) %>%
  mutate(emp2018 = as.numeric(emp2018),
         emp2013 = as.numeric(emp2013),
         emp2008 = as.numeric(emp2008),
         emp2003 = as.numeric(emp2003),
         salary2018 = as.numeric(salary2018),
         salary2013 = as.numeric(salary2013),
         salary2008 = as.numeric(salary2008),
         salary2003 = as.numeric(salary2003))

write_csv(yoy_long, here("yoy_long.csv"))

write_csv(five_long, here("five_long.csv"))

write_csv(yoy_wide, here("wide_long.csv"))

write_csv(five_wide, here("five_wide.csv"))

  
