


library(fs)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(DiagrammeR)


# Load data ----

datapath <- path('C:', 'Users', 'JCLI', 'OneDrive - Folkehelseinstituttet', 'ssb kommune', 'data')

FINAL_DATE <- ymd('2034-12-31')


changetable <- readRDS(path(datapath, 'changetable.RDS'))
kommuneindelinger <- readRDS(path(datapath, 'kommuneindelinger.RDS'))


# TODO: jan 2020: 1867 - Bø (Nordland)

# Clean changetable ----

# make absolutely sure the changetable is properly sorted.
changetable %>% 
  
  # Remove Evenes change in 2020, probably a mistake?
  filter(!(oldCode == '1853' & newCode == '1853' & changeOccurred == '2020-01-01')) %>% 
  
  # Remove Røros change in 2020, probably a mistake?
  filter(!(oldCode == '5025' & newCode == '5025' & changeOccurred == '2020-01-01')) %>% 
  
  # Name changes of Karasjok and Nesseby not in changetable. Add manually.
  
  add_row(oldCode = '2021',
          oldName = 'Karasjok',
          newCode= '2021',
          newName = 'Kárášjohka - Karasjok',
          changeOccurred = as.Date('1988-01-01')) %>% 
  
  # Commented out. See below.
  # add_row(oldCode = '2027',
  #         oldName = 'Nesseby',
  #         newCode = '2027',
  #         newName = nesseby,
  #         changeOccurred = as.Date('1988-01-01')) %>% 
  
  arrange(changeOccurred) %>% 
  mutate(EDGE = 1:n()) -> changetable


# for some reason add_row() does not work for the sami Nesseby name,
# and having it as string in the code does not work either. 
# Here is an inelegant workaround. 
kommuneindelinger %>% filter(year == 1988, code == '2027') %>% pull(full_name) -> nesseby_name

nesseby_df <- data.frame(oldCode = '2027',
           oldName = 'Nesseby',
           newCode = '2027',
           newName = nesseby_name,
           changeOccurred = as.Date('1988-01-01'))


changetable <- bind_rows(changetable, nesseby_df)

changetable %>% 
  arrange(changeOccurred) %>% 
  mutate(EDGE = 1:n()) -> changetable


# some checks.
all_codes <- unique(kommuneindelinger$code)
all_codes_changes <- unique(c(changetable$oldCode, changetable$newCode))
all(all_codes_changes %in% all_codes) # Should be TRUE.


# Make nodes from kommuneinndelinger ----

# Initialize nodes

kommuneindelinger %>% 
  filter(year == 1977) %>% 
  mutate(start_date = ymd('1977-01-01'),
         end_date = FINAL_DATE) %>% 
  select(-year) %>% 
  mutate(NODE = 1:n()) -> nodes_init
  
nrow_nodes_init <- nrow(nodes_init)


# We infer the new nodes from the changetable (ie the edges).
# When looking at the new nodes, we need to use distinct to remove 
# duplicates that happen when two municipalities merge.

changetable %>%
  distinct(newCode, newName, changeOccurred) %>% 
  rename(code = newCode, 
         full_name = newName,
         start_date = changeOccurred) %>% 
  mutate(NODE = nrow_nodes_init + (1:n()),
         end_date = FINAL_DATE) -> new_nodes




# 4 rows removed by distinct(), for municipalities that were split
# Tysfjord, Snilfjord, Ålesund

changetable %>%
  distinct(oldCode, oldName, changeOccurred) %>% 
  rename(code = oldCode, 
         full_name = oldName,
         end_date2 = changeOccurred) %>% 
  mutate(end_date2 = end_date2 - 1) -> nodes_to_update


# look at those that were removed in the previous step (split municipalities).
# changetable %>%
#   mutate(DD = duplicated(paste(oldCode, oldName, changeOccurred))) %>% View()


nodes_combined <- bind_rows(nodes_init, new_nodes)



# Some nodes need manual handling:
# 1804 Bodo was merged with 1842 Skjerstad in 2005, but kept the code 1804 and name Bodo.
# Same with 1103 Stavanger.
# Therefore it must be a 'many-to-many' relationship in the join.
# This creates a problem with Trondheim, since it has a more complicated history, but with
# similar duplication as Bodo and Stavanger


# Remove Trondheim bore updating nodes. These dates are manually added later.
nodes_to_update %>% 
  filter(!str_detect(full_name, 'Trondheim')) -> nodes_to_update


nodes_combined %>%
  left_join(nodes_to_update, by = c('code', 'full_name'), relationship = 'many-to-many') %>% 
  mutate(end_date = if_else(!is.na(end_date2), end_date2, end_date)) %>%
  select(-end_date2) -> nodes_combined2


# Fix the end dates for 1804 Bodo and 1103 Stavanger, and Trondheim
nodes_combined2 %>% 
  mutate(end_date = if_else(code == '1804' & start_date == ymd('2005-01-01'), FINAL_DATE, end_date),
         end_date = if_else(code == '1103' & start_date == ymd('2020-01-01'), FINAL_DATE, end_date),
         # Manually fix Tronheim.
         end_date = if_else(code == '1601' & start_date == ymd('1977-01-01'), ymd('2017-12-31'), end_date),
         end_date = if_else(code == '5001' & start_date == ymd('2018-01-01'), ymd('2019-12-31'), end_date),
         end_date = if_else(code == '5001' & start_date == ymd('2020-01-01'), ymd('2022-12-31'), end_date)) -> nodes_combined2



# Separate out county parenthesis, sami and kven names, from the full name.

# List of norwegian names of kommuner:
names_nor <- readLines('norske_navn.txt', encoding = 'UTF-8')

# Sami names:
names_sami <- readLines('samiske_navn.txt', encoding = 'UTF-8')


# Kven names:
names_kven <- c('Porsanki', 'Kaivuono', 'Omasvuono', 'Raisi')

nodes_combined2 %>% 
  #Separate out names in different languages (norwegian, sami, kven, not necessarily in that order).
  separate(full_name,
           sep = ' - ',
           into = paste0('name_', c(1, 2, 3)),
           remove = FALSE) %>% 
  mutate(name_nor = name_1,
         name_nor = ifelse(name_nor %in% names_nor, name_1, name_nor),
         name_nor = ifelse(name_2 %in% names_nor, name_2, name_nor),
         name_nor = ifelse(name_3 %in% names_nor, name_3, name_nor),

         name_smi = name_1,
         name_smi = ifelse(name_smi %in% names_sami, name_smi, NA),
         name_smi = ifelse(name_2 %in% names_sami, name_2, name_smi),
         name_smi = ifelse(name_3 %in% names_sami, name_3, name_smi),

         name_fkv = name_1,
         name_fkv = ifelse(name_fkv %in% names_kven, name_fkv, NA),
         name_fkv = ifelse(name_2 %in% names_kven, name_2, name_fkv),
         name_fkv = ifelse(name_3 %in% names_kven, name_3, name_fkv)) %>% 
  select(-name_1, -name_2, -name_3) -> nodes_combined2



# Edges ----

# need to get the node numbers


nodes_combined2 %>% 
  mutate(changeOccurred = end_date + 1) %>% 
  select(code, changeOccurred, NODE) %>% 
  rename(NODE_FROM = NODE) -> nodes_from


nodes_combined2 %>% 
  mutate(changeOccurred = start_date) %>% 
  select(code, changeOccurred, NODE) %>% 
  rename(NODE_TO = NODE)-> nodes_to



changetable %>% 
  left_join(nodes_from, by = c('oldCode' = 'code', 'changeOccurred')) %>% 
  left_join(nodes_to, by = c('newCode' = 'code', 'changeOccurred')) -> changetable_with_nodes


# DO some checks. 
# Merge the edges with the node list, then check the names. 

nodes_combined2 %>% 
  left_join(changetable_with_nodes, by = c('NODE' = 'NODE_TO')) %>% 
  mutate(OK = full_name == newName,
         DATE_OK = changeOccurred == start_date) -> dta_node_edge_test_1


nodes_combined2 %>% 
  left_join(changetable_with_nodes, by = c('NODE' = 'NODE_FROM')) %>% 
  mutate(OK = full_name == oldName,
         DATE_OK = changeOccurred == (end_date + 1)) -> dta_node_edge_test_2




# Should be TRUE
all(dta_node_edge_test_1$OK, na.rm = TRUE)
all(dta_node_edge_test_1$DATE_OK, na.rm = TRUE)
all(dta_node_edge_test_2$OK, na.rm = TRUE)
all(dta_node_edge_test_2$DATE_OK, na.rm = TRUE)

all(changetable_with_nodes$NODE_FROM %in% nodes_combined2$NODE)
all(changetable_with_nodes$NODE_TO %in% nodes_combined2$NODE)


## Make graph object ----



changetable_with_nodes %>% 
  rename(from = NODE_FROM,
         to = NODE_TO) %>% 
  select(from, to, EDGE, changeOccurred) -> dta_edges

head(dta_edges)


nodes_combined2 %>% 
  rename(id = NODE) %>% 
  select(id, everything()) -> dta_nodes

head(dta_nodes)

# Small check
all(dta_edges$from %in% dta_nodes$id)



create_graph(directed = TRUE) %>% 
  add_node_df(dta_nodes) %>% 
  add_edge_df(dta_edges) -> kommunegraph


# render_graph(kommunegraph, layout = "fr")


saveRDS(kommunegraph, 
        file = path('C:', 'Users', 'JCLI', 'OneDrive - Folkehelseinstituttet', 'ssb kommune', 'data', 'kommunegraph.RDS'))



