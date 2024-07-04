
library(jsonlite)
library(dplyr)
library(lubridate)

# https://www.ssb.no/klass/klassifikasjoner/131/

# Download the classification, with links to all versions.
download.file('https://data.ssb.no/api/klass/v1/classifications/131.json',
              destfile = '131.json',
              , mode = 'wb')


# Get data.frame with links to all versions, and dates for when they are valid.
kommuneinndelinger_versjoner <- fromJSON('131.json')$version


kommuneinndelinger_versjoner %>% 
  mutate(validFrom = ymd(validFrom),
         validTo = ymd(validTo)) -> kommuneinndelinger_versjoner


# Get links to versions to download.
kommuneinndelinger_versjoner %>% 
  filter(validFrom >= '1977-01-01') -> kommuneinndelinger_versjoner_filtered


# The href column is nested within some other columns.
kommuneinndelinger_versjoner_filtered$json_links <- paste0(kommuneinndelinger_versjoner_filtered$`_links`$self$href, '.json')


# Download Kommuneinndelinger.
for (ii in 1:nrow(kommuneinndelinger_versjoner_filtered)){
  
  if (ii > 1){
    Sys.sleep(2.5)
  }
  
  fname <- paste0(kommuneinndelinger_versjoner_filtered$name[ii], '.json')
  download.file(kommuneinndelinger_versjoner_filtered$json_links[ii], destfile = fname, mode = 'wb')
}



# Get change tables

download.file('https://data.ssb.no/api/klass/v1/classifications/131/changes.json?from=1977-01-01&to=2024-01-02', 
              destfile = 'changetables.json', mode = 'wb')


# Download the master list of all kommuneendringer.
# Link can be found here:
# https://www.ssb.no/metadata/alle-endringer-i-de-regionale-inndelingene

# add download date to the file name.
masterlist_fname <- paste0('KOM_Masterliste_1838_2024_', as.character(Sys.Date()), '.xlsx')

download.file('https://www.ssb.no/metadata/alle-endringer-i-de-regionale-inndelingene/_/attachment/download/8101a12d-926a-4a62-9dd8-ffe2610cf642:24ab1fa5ff5075c6f2763980191df065ab112b8e/KOM_Masterliste_1838_2024.xlsx',
              destfile = masterlist_fname, mode = 'wb')




