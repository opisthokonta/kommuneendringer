

library(fs)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(DiagrammeR)


# Load data ----

datapath <- path('..', 'data')

FINAL_DATE <- ymd('2035-12-31')


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
  filter(year == 1971) %>%
  mutate(start_date = ymd('1971-01-01'),
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



# Identify new nodes using the changetable. ----

all_nodes <- nodes_init

change_dates <- sort(unique(changetable_filtered$changeOccurred))
n_changetables <- length(change_dates)


for (ii in 1:n_changetables){

  print(change_dates[ii])

  # Current changetable to apply
  changetable1 <- changetable_filtered %>%
    filter(changeOccurred == change_dates[ii])

  # Add temporary variable currently_valid to all_nodes.
  # This will be removed later.
  all_nodes %>%
    mutate(currently_valid = end_date >= change_dates[ii]) -> all_nodes

  # Some checks.
  all_nodes %>%
    filter(currently_valid) -> all_currently_valid_nodes

  stopifnot(!any(duplicated(all_currently_valid_nodes$code)))
  stopifnot(nrow(all_currently_valid_nodes) > 0)


  # Identify new nodes, make rows to add to node list.
  changetable1 %>%
    filter(!duplicated(newCode)) %>%
    select(newCode, newName, changeOccurred) %>%
    rename(code = newCode,
           full_name = newName,
           start_date = changeOccurred) %>%
    mutate(end_date = as.Date(FINAL_DATE),
           NODE = (1:n()) + max(all_nodes$NODE)) -> new_nodes

  # update end_date in all_nodes
  all_nodes %>%
    mutate(end_date = if_else(currently_valid & code %in% changetable1$oldCode, as.Date(change_dates[ii]) - 1, end_date)) -> all_nodes

  # Add new nodes to all_nodes
  all_nodes %>%
    bind_rows(new_nodes) -> all_nodes


}


nrow(all_nodes)

all_nodes %>%
  select(-currently_valid) -> all_nodes


# Some checks.
stopifnot(length(unique(all_nodes$NODE)) == nrow(all_nodes))
stopifnot(all_nodes$NODE == 1:nrow(all_nodes))




# Edges ----

# need to get the node numbers

all_nodes %>%
  mutate(changeOccurred = end_date + 1) %>%
  select(code, changeOccurred, NODE) %>%
  rename(NODE_FROM = NODE) -> nodes_from

all_nodes %>%
  mutate(changeOccurred = start_date) %>%
  select(code, changeOccurred, NODE) %>%
  rename(NODE_TO = NODE)-> nodes_to


changetable_filtered %>%
  left_join(nodes_from, by = c('oldCode' = 'code', 'changeOccurred')) %>%
  left_join(nodes_to, by = c('newCode' = 'code', 'changeOccurred')) -> changetable_with_nodes




# Do some checks.
# Merge the edges with the node list, then check the names.

# Checks the new nodes (after 1971)
all_nodes %>%
  left_join(changetable_with_nodes, by = c('NODE' = 'NODE_TO')) %>%
  mutate(NAME_OK = full_name == newName,
         DATE_OK = changeOccurred == start_date) -> dta_node_edge_test_1


# Checks the inital nodes (1977)
all_nodes %>%
  left_join(changetable_with_nodes, by = c('NODE' = 'NODE_FROM')) %>%
  mutate(NAME_OK = full_name == oldName,
         DATE_OK = changeOccurred == (end_date + 1)) -> dta_node_edge_test_2


stopifnot(all(dta_node_edge_test_1$NAME_OK, na.rm = TRUE))
stopifnot(all(dta_node_edge_test_1$DATE_OK, na.rm = TRUE))
stopifnot(all(dta_node_edge_test_2$NAME_OK, na.rm = TRUE))
stopifnot(all(dta_node_edge_test_2$DATE_OK, na.rm = TRUE))

stopifnot(all(changetable_with_nodes$NODE_FROM %in% all_nodes$NODE))
stopifnot(all(changetable_with_nodes$NODE_TO %in% all_nodes$NODE))



## Tidy up the data bit more ----

# Add county names.

county_names_numbers <- read.csv('county_names_numbers.txt', encoding = 'UTF-8', colClasses = 'character')

fylke_parenthesis_ptrn_remove <- ' [(].+[)]$'

# Only county name when duplcate kommune name
all_nodes %>%
  mutate(full_name = str_remove(full_name, fylke_parenthesis_ptrn_remove)) %>%

  # Add county data
  mutate(county_num = str_sub(code, 1, 2)) %>%
  left_join(county_names_numbers, by = c('county_num' = 'county_code')) %>%
  select(-county_num) -> all_nodes_counties


# Separate out norwegian, sami and kven names, from the full name.

# List of norwegian names of kommuner:
names_nor <- readLines('norske_navn.txt', encoding = 'UTF-8')

# Sami names:
names_sami <- readLines('samiske_navn.txt', encoding = 'UTF-8')

# Kven names:
names_kven <- c('Porsanki', 'Kaivuono', 'Omasvuono', 'Raisi')


all_nodes_counties %>%
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
  select(-name_1, -name_2, -name_3) -> all_nodes_with_names





## Make graph object ----

changetable_with_nodes %>%
  rename(from = NODE_FROM,
         to = NODE_TO) %>%
  select(from, to, EDGE, changeOccurred) -> dta_edges

head(dta_edges)


all_nodes_with_names %>%
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


