


library(fs)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(DiagrammeR)


# Load data ----

datapath <- path('..', 'data')

FINAL_DATE <- ymd('2034-12-31')


load(path(datapath, 'changetable.rda'))
load(path(datapath, 'kommuneinndelinger.rda'))


# Clean changetable ----

# make absolutely sure the changetable is properly sorted.
changetable %>%
  arrange(changeOccurred) %>%
  mutate(EDGE = 1:n()) -> changetable


# some checks.
all_codes <- unique(kommuneinndelinger$code)
all_codes_changes <- unique(c(changetable$oldCode, changetable$newCode))
stopifnot(all(all_codes_changes %in% all_codes))


# Make nodes from kommuneinndelinger ----

# Initialize nodes

kommuneinndelinger %>%
  filter(year == 1977) %>%
  mutate(start_date = ymd('1977-01-01'),
         end_date = as.Date(FINAL_DATE)) %>%
  select(-year) %>%
  mutate(NODE = 1:n()) -> nodes_init

nrow_nodes_init <- nrow(nodes_init)



changetable %>%
  # Remove Varteig -> Rakkestad.
  filter(!(oldCode == '0114' & newCode == '0128' & changeOccurred == '1992-01-01')) %>%
  # Remove Sande (More og Romsdal) change in 2020 that is not really a change.
  filter(!(oldCode == '1514' & newCode == '1514' & changeOccurred == '2020-01-01')) %>%
  # Remove Bø (Nordland) change in 2020 that is not really a change.
  filter(!(oldCode == '1867' & newCode == '1867' & changeOccurred == '2020-01-01'))-> changetable_filtered



# infer the new nodes
changetable_filtered %>%
  distinct(newCode, newName, changeOccurred) %>%
  rename(code = newCode,
         full_name = newName,
         start_date = changeOccurred) %>%
  mutate(NODE = nrow_nodes_init + (1:n()),
         end_date = FINAL_DATE) -> new_nodes

all_nodes <- bind_rows(nodes_init, new_nodes)


# 4 rows removed by distinct(), for municipalities that were split
# Tysfjord, Snilfjord.


changetable_filtered %>%
  distinct(oldCode, oldName, changeOccurred) %>%
  rename(code = oldCode,
         full_name = oldName,
         end_date2 = changeOccurred) %>%
  mutate(end_date2 = end_date2 - 1) %>%
  mutate(NODE_UPDATE_ID = 1:n()) -> nodes_to_update



# Remove Trondheim before updating nodes. These dates are manually added later.
nodes_to_update %>%
  filter(!str_detect(full_name, 'Trondheim')) -> nodes_to_update

# Remove Tønsberg before updating nodes. The dates are manually added later.
# Because of encoding problems, I use a regular expression wildcard for the ø.
nodes_to_update %>%
  filter(!str_detect(full_name, 'T.nsberg')) -> nodes_to_update


nodes_to_update %>%
  filter(!str_detect(full_name, 'Ringsaker')) -> nodes_to_update


all_nodes %>%
  left_join(nodes_to_update, by = c('code', 'full_name'), relationship = 'many-to-many') %>%
  mutate(end_date = if_else(!is.na(end_date2), end_date2, end_date)) %>%
  select(-end_date2) -> all_nodes2



# Some nodes need manual handling:
# 1804 Bodo was merged with 1842 Skjerstad in 2005, but kept the code 1804 and name Bodo.
# Same with 1103 Stavanger.
# 0412 Ringsaker that was "split" in 1992, by giving a part to Hamar.
# Therefore it must be a 'many-to-many' relationship in the join.
# This creates a problem with Trondheim, since it has a more complicated history, but with
# similar duplication as Bodo and Stavanger



# Fix the end dates for 1804 Bodo and 1103 Stavanger, and Trondheim
all_nodes2 %>%
  mutate(end_date = if_else(code == '1804' & start_date == ymd('2005-01-01'), FINAL_DATE, end_date),
         end_date = if_else(code == '1103' & start_date == ymd('2020-01-01'), FINAL_DATE, end_date),

         # Manually fix Trondheim.
         end_date = if_else(code == '1601' & start_date == ymd('1977-01-01'), ymd('2017-12-31'), end_date),
         end_date = if_else(code == '5001' & start_date == ymd('2018-01-01'), ymd('2019-12-31'), end_date),
         end_date = if_else(code == '5001' & start_date == ymd('2020-01-01'), ymd('2022-12-31'), end_date),

         # Manually fix Tønsberg.
         end_date = if_else(code == '0705' & start_date == ymd('1977-01-01'), ymd('1987-12-31'), end_date),
         end_date = if_else(code == '0704' & start_date == ymd('1988-01-01'), ymd('2016-12-31'), end_date),
         end_date = if_else(code == '0704' & start_date == ymd('2017-01-01'), ymd('2019-12-31'), end_date),
         end_date = if_else(code == '3803' & start_date == ymd('2020-01-01'), ymd('2023-12-31'), end_date),

         # Manually fix Ringsaker.
         end_date = if_else(code == '0412' & start_date == ymd('1977-01-01'), ymd('1991-12-31'), end_date),
         end_date = if_else(code == '0412' & start_date == ymd('1992-01-01'), ymd('2019-12-31'), end_date),

         # Rakkestad.
         #end_date = if_else(code == '0128' & start_date == ymd('1977-01-01'), ymd('1991-12-31'), end_date)
         ) -> all_nodes3




# Edges ----

# need to get the node numbers

all_nodes3 %>%
  mutate(changeOccurred = end_date + 1) %>%
  select(code, changeOccurred, NODE) %>%
  rename(NODE_FROM = NODE) -> nodes_from


all_nodes3 %>%
  mutate(changeOccurred = start_date) %>%
  select(code, changeOccurred, NODE) %>%
  rename(NODE_TO = NODE)-> nodes_to



changetable_filtered %>%
  left_join(nodes_from, by = c('oldCode' = 'code', 'changeOccurred')) %>%
  left_join(nodes_to, by = c('newCode' = 'code', 'changeOccurred')) -> changetable_with_nodes



# Do some checks.
# Merge the edges with the node list, then check the names.

# Checks the new nodes (after 1977)
all_nodes3 %>%
  left_join(changetable_with_nodes, by = c('NODE' = 'NODE_TO')) %>%
  mutate(NAME_OK = full_name == newName,
         DATE_OK = changeOccurred == start_date) -> dta_node_edge_test_1


# Checks the inital nodes (1977)
all_nodes3 %>%
  left_join(changetable_with_nodes, by = c('NODE' = 'NODE_FROM')) %>%
  mutate(NAME_OK = full_name == oldName,
         DATE_OK = changeOccurred == (end_date + 1)) -> dta_node_edge_test_2



# Should be TRUE
all(dta_node_edge_test_1$NAME_OK, na.rm = TRUE)
all(dta_node_edge_test_1$DATE_OK, na.rm = TRUE)
all(dta_node_edge_test_2$NAME_OK, na.rm = TRUE)
all(dta_node_edge_test_2$DATE_OK, na.rm = TRUE)

all(changetable_with_nodes$NODE_FROM %in% all_nodes3$NODE)
all(changetable_with_nodes$NODE_TO %in% all_nodes3$NODE)


## Tidy up the data bit more ----


# Add county names.

county_names_numbers <- read.csv('county_names_numbers.txt', encoding = 'UTF-8', colClasses = 'character')

fylke_parenthesis_ptrn_remove <- ' [(].+[)]$'

# Only county name when duplcate kommune name
all_nodes3 %>%
  mutate(full_name = str_remove(full_name, fylke_parenthesis_ptrn_remove)) %>%

  # Add county data
  mutate(county_num = str_sub(code, 1, 2)) %>%
  left_join(county_names_numbers, by = c('county_num' = 'county_code')) %>%
  select(-county_num) -> all_nodes3_counties


# Separate out norwegian, sami and kven names, from the full name.

# List of norwegian names of kommuner:
names_nor <- readLines('norske_navn.txt', encoding = 'UTF-8')

# Sami names:
names_sami <- readLines('samiske_navn.txt', encoding = 'UTF-8')

# Kven names:
names_kven <- c('Porsanki', 'Kaivuono', 'Omasvuono', 'Raisi')


all_nodes3_counties %>%
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
  select(-name_1, -name_2, -name_3) -> all_nodes3_with_names




## Make graph object ----

changetable_with_nodes %>%
  rename(from = NODE_FROM,
         to = NODE_TO) %>%
  select(from, to, EDGE, changeOccurred) -> dta_edges

head(dta_edges)


all_nodes3_with_names %>%
  rename(id = NODE) %>%
  select(id, everything()) -> dta_nodes

head(dta_nodes)

# Small check
all(dta_edges$from %in% dta_nodes$id)



create_graph(directed = TRUE) %>%
  add_node_df(dta_nodes) %>%
  add_edge_df(dta_edges) -> kommunegraph


 # render_graph(kommunegraph, layout = "fr")

usethis::use_data(kommunegraph, overwrite = TRUE)


