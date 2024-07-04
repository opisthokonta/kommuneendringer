


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



kommuneinndelinger <- data.frame()

for (ii in 1:length(kommuneinndeling_files)){
  
  dta <- fromJSON(kommuneinndeling_files[ii])
  year_string <- str_extract(dta$name, '[0-9]+')
  print(year_string)
  
  dta_kommuner_raw <- as.data.frame(dta$classificationItems)
  
  
  # Some kommuner has county name in parenthesis, Put these in a separate column.
  dta_kommuner_raw %>% 
    mutate(county = str_extract(name, pattern = '\\(.+\\)'),
           county = str_remove_all(county, pattern = '[()]')) %>% 
           
           # remove county from name.
           #name = str_remove(name, pattern = ' *\\(.+')) %>% 
    # Remove whitespace just to be sure.
    mutate(name = str_squish(name),
           county = str_squish(county)) %>% 
    select(code, name, county) %>% 
    # Separate out names in different languages (norwegian, sami, kven, not necessarily in that order).
    # separate(name, 
    #          sep = ' - ', 
    #          into = paste0('name_', c(1, 2, 3)),
    #          remove = FALSE) %>% 
    # mutate(name_nor = name_1,
    #        name_nor = ifelse(name_nor %in% names_nor, name_1, name_nor),
    #        name_nor = ifelse(name_2 %in% names_nor, name_2, name_nor),
    #        name_nor = ifelse(name_3 %in% names_nor, name_3, name_nor),
    #        
    #        name_smi = name_1,
    #        name_smi = ifelse(name_smi %in% names_sami, name_smi, NA),
    #        name_smi = ifelse(name_2 %in% names_sami, name_2, name_smi),
    #        name_smi = ifelse(name_3 %in% names_sami, name_3, name_smi),
    #        
    #        name_fkv = name_1,
    #        name_fkv = ifelse(name_fkv %in% names_kven, name_fkv, NA),
    #        name_fkv = ifelse(name_2 %in% names_kven, name_2, name_fkv),
    #        name_fkv = ifelse(name_3 %in% names_kven, name_3, name_fkv)) %>% 
    # select(-name_1, -name_2, -name_3) %>% 
    # rename(full_name = name) %>% 
    # mutate(year = year_string) %>% 
    # select(year, code, full_name, name_nor, name_smi, name_fkv, county) -> dta_kommune_core_names
    rename(full_name = name) %>% 
    mutate(year = year_string) %>%
    select(year, code, full_name, county) -> dta_kommune_core_names
    
  
  kommuneinndelinger <- bind_rows(kommuneinndelinger, dta_kommune_core_names)
  
  
}

saveRDS(kommuneinndelinger, 
        file = path('C:', 'Users', 'JCLI', 'OneDrive - Folkehelseinstituttet', 'ssb kommune', 'data', 'kommuneindelinger.RDS'))



