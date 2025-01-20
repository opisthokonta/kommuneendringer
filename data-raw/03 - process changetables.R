


library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(fs)
library(stringr)


changetable_raw <- fromJSON('changetables.json')$codeChanges


changetable_raw %>%
  select(-oldShortName, -newShortName) %>%
  mutate(oldName = str_squish(oldName),
         newName= str_squish(newName)) %>%
  mutate(changeOccurred = ymd(changeOccurred)) -> changetable



# Add county names
county_names_numbers <- read.csv('county_names_numbers.txt', encoding = 'UTF-8', colClasses = 'character')

fylke_parenthesis_ptrn_remove <- ' [(].+[)]$'


changetable %>%
  mutate(oldName = str_remove(oldName, fylke_parenthesis_ptrn_remove),
         newName = str_remove(newName, fylke_parenthesis_ptrn_remove)) %>%
  mutate(county_num_old = str_sub(oldCode, 1, 2),
         county_num_new = str_sub(newCode, 1, 2)) %>%
  left_join(county_names_numbers, by = c('county_num_old' = 'county_code')) %>%
  rename(county_name_old = county_name) %>%
  left_join(county_names_numbers, by = c('county_num_new' = 'county_code')) %>%
  rename(county_name_new = county_name) %>%
  mutate(oldName = sprintf('%s (%s)', oldName, county_name_old),
         newName = sprintf('%s (%s)', newName, county_name_new)) %>%
  select(oldCode, oldName, newCode, newName, changeOccurred) -> changetable



usethis::use_data(changetable, overwrite = TRUE)

