


library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(fs)
library(stringr)


changetable_raw <- fromJSON('changetables.json')$codeChanges


changetable_raw %>% 
  select(-oldShortName, -newShortName) %>% 
  mutate(changeOccurred = ymd(changeOccurred)) -> changetable



saveRDS(changetable, 
        file = path('C:', 'Users', 'JCLI', 'OneDrive - Folkehelseinstituttet', 'ssb kommune', 'data', 'changetable.RDS'))






