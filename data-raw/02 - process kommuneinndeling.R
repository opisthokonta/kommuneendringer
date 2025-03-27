


library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(fs)
library(stringr)

kommuneinndeling_files <- dir_ls(regexp = 'Kommuneinndeling')


# Names of kommuner with names in different languages.
# Used to get the right name in the column corresponding to the language.
# Norwegian and Sami names are in separate text files, to avoid encoding problems.

# List of norwegian names of kommuner:
names_nor <- readLines('norske_navn.txt')

# Sami names:
names_sami <- readLines('samiske_navn.txt')

# Kven names:
names_kven <- c('Porsanki', 'Kaivuono', 'Omasvuono', 'Raisi')


# County names and county numbers.
county_names_numbers <- read.csv('county_names_numbers.txt', encoding = 'UTF-8', colClasses = 'character')



kommuneinndelinger_merged <- data.frame()


# Merge files ----

for (ii in 1:length(kommuneinndeling_files)){

  dta <- fromJSON(kommuneinndeling_files[ii])
  year_string <- str_extract(dta$name, '[0-9]+')
  print(year_string)

  dta_kommuner_raw <- as.data.frame(dta$classificationItems)


  # Some kommuner has county name in parenthesis, Put these in a separate column.
  dta_kommuner_raw %>%
    # Remove whitespace just to be sure.
    mutate(name = str_squish(name)) %>%
    select(code, name) %>%
    rename(full_name = name) %>%
    mutate(year = year_string) %>%
    select(year, code, full_name) -> dta_kommune_core_names


  kommuneinndelinger_merged <- bind_rows(kommuneinndelinger_merged, dta_kommune_core_names)


}

# Remove 9999 Uoppgitt
kommuneinndelinger_merged %>%
  filter(code != '9999') -> kommuneinndelinger_merged


## Counties ----

# Useful regular expressions to remove and extract county names from
# # parentheses after municipality name.
fylke_parenthesis_ptrn_remove <- ' [(].+[)]$'
fylke_parenthesis_ptrn_extract <- '(?<=[(]).+(?=[)])' # Extract the text inside the parenthesis.



# Usually, kommuner with county name in parenthesis are duplicate names for different
# kommuner, but sometimes the kommune ceased to exist before the year the data covers.
# So we remove the county names, and add them back to the full name only when
# the kommune name is duplicated in that year.


# Version of duplicated() that also return TRUE for the first element that is
# in among the duplicated elements.
# duplicated(c('x', 'x', 'c'))
# duplicated_elements(c('x', 'x', 'c'))
duplicated_elements <- function(x){

  x_dup_idx <- duplicated(x)

  if (any(x_dup_idx)){
    x_dup_elements <- unique(x[x_dup_idx])
    x_dup_idx <- x %in% x_dup_elements
  }

  return(x_dup_idx)
}



# Only county name when duplcate kommune name
# kommuneinndelinger_merged %>%
#   mutate(county = str_extract(full_name, fylke_parenthesis_ptrn_extract),
#          full_name = str_remove(full_name, fylke_parenthesis_ptrn_remove)) %>%
#   # Add back county name in parenthesis only when there are duplicate kommune names.
#   group_by(year) %>%
#   mutate(full_name = ifelse(duplicated_elements(full_name), sprintf('%s (%s)', full_name, county) , full_name)) %>%
#   ungroup()  %>%
#   # Add county names to all municipalities.
#   mutate(county_num = str_sub(code, 1, 2)) %>%
#   left_join(county_names_numbers, by = c('county_num' = 'county_code')) -> kommuneinndelinger_with_counties

# County name in parenthesis for ALL kommuner.
kommuneinndelinger_merged %>%
  mutate(full_name = str_remove(full_name, fylke_parenthesis_ptrn_remove)) %>%
  # Add county names to all municipalities.
  mutate(county_num = str_sub(code, 1, 2)) %>%
  left_join(county_names_numbers, by = c('county_num' = 'county_code')) %>%
  mutate(full_name = sprintf('%s (%s)',full_name, county_name)) -> kommuneinndelinger_with_counties



# Check that the county names from the county_names_numbers.txt file matches the ones
# extracted from the municipality names.
stopifnot(all(na.omit(kommuneinndelinger_with_counties$county == kommuneinndelinger_with_counties$county_name)))

# Check that there are no NA in county name.
stopifnot(!any(is.na(kommuneinndelinger_with_counties$county_name)))



# Save ----

kommuneinndelinger_with_counties %>%
  select(-county_num, -county_name) -> kommuneinndelinger
  #rename(county = county_name) -> kommuneinndelinger

usethis::use_data(kommuneinndelinger, overwrite = TRUE)



