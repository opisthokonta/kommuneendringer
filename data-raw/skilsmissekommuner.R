



skilsmissekommuner <- function(dta_nodes){
  
  # 2024: Alesund/Haram split ----
  
  # 2024: Deal with the Alesund/Haram split, after
  # Alesund, Haram, Sandøy, Skodje and Ørskog merger in 2020.
  
  # Get 2019 node ids
  
  # 2019-Kommuner som i 2024 er del av Alseund.
  dta_nodes %>% 
    filter(code %in% c('1546', '1529', '1504', '1523'),
           end_date == '2019-12-31') %>% 
    pull(id) -> node_id_2019_aalesund
  
  
  # Haram 2019 node id
  dta_nodes %>% 
    filter(code %in% c('1534'),
           end_date == '2019-12-31') %>% 
    pull(id) -> node_id_2019_haram
  
  
  # Aalesund 2024 node id
  dta_nodes %>% 
    filter(code %in% c('1508'),
           start_date == '2024-01-01') %>% 
    pull(id) -> node_id_2024_aalesund
  
  
  # Haram 2024 node id
  dta_nodes %>% 
    filter(code %in% c('1580'),
           start_date == '2024-01-01') %>% 
    pull(id)-> node_id_2024_haram
  
  
  
  dta_edges_1 <- data.frame(from = c(node_id_2019_aalesund, node_id_2019_haram), 
                            to  = c(rep(node_id_2024_aalesund, 4), node_id_2024_haram),
                            changeOccurred = '2024-01-01',
                            edge_type = 2)
  
  # Other ----
  
  
  # Return----
  
  return(dta_edges_1)
  
}

