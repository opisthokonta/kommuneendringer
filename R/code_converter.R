

library(dplyr)
library(fs)
library(DiagrammeR)
#

FIRST_DATE <- as.Date('1971-01-01')
FINAL_DATE <- as.Date('2035-12-31')


is_date <- function(x){
  'Date' %in% class(x)
}


# Small function to check if we have found the correct node.
check_node <- function(graph_obj, node_id, date){

  node_end_date <- DiagrammeR::get_node_attrs(graph_obj,
                                              node_attr = 'end_date',
                                              nodes = node_id)

  node_start_date <- DiagrammeR::get_node_attrs(graph_obj,
                                              node_attr = 'start_date',
                                              nodes = node_id)

  if (date <= node_end_date & date >= node_start_date){
    return(TRUE)
  } else {
    return(FALSE)
  }

}


get_successors_by_edge_type <- function(gr, node_id, edge_types){

  edf <- DiagrammeR::get_edge_df(gr)
  edf <- dplyr::filter(edf, from == node_id)
  edf <- dplyr::filter(edf, edge_type %in% edge_types)
  res <- dplyr::pull(edf, to)

  if (length(res) == 0){
    res <- NA
  }

  return(res)

}


get_predecessors_by_edge_type <- function(gr, node_id, edge_types){

  # gr %>%
  #   DiagrammeR::get_edge_df() %>%
  #   dplyr::filter(to == node_id) %>%
  #   dplyr::filter(edge_type %in% edge_types) %>%
  #   dplyr::pull(from) -> res

  edf <- DiagrammeR::get_edge_df(gr)
  edf <- dplyr::filter(edf, to == node_id)
  edf <- dplyr::filter(edf, edge_type %in% edge_types)
  res <- dplyr::pull(edf, from)

  if (length(res) == 0){
    res <- NA
  }

  return(res)
}






# Internal function that actually does the translation. Only one at a time
translate_knr_internal <- function(knr, from_date, to_date, allow_reversals = FALSE, show_warnings = TRUE){


  start_node <- DiagrammeR::get_node_ids(kommunegraph, conditions = code == knr & end_date >= from_date & start_date <= from_date)

  stopifnot(length(start_node) == 1)


  if (is.na(start_node)){
    return(NA)
  }

  if (from_date == to_date){
    return(knr)
  }


  if (from_date < to_date){
    forward <- TRUE
  } else {
    forward <- FALSE
  }

  # Initialize the current_node_id for while loop.
  current_node_id <- start_node


  # Check if correct node is already found. If so, no need to traverse the graph.
  node_found <- check_node(kommunegraph, node_id = current_node_id, date = to_date)

  if (node_found){
    traverse_graph <- FALSE
  } else {
    traverse_graph <- TRUE
  }


  # Traverse the graph.
  while (traverse_graph){


    if (allow_reversals){

      follow_edge_type_1 <- FALSE

      if (forward){
        next_node_id <- get_successors_by_edge_type(kommunegraph, node = current_node_id, edge_types = 2)
      } else {
        next_node_id <- get_predecessors_by_edge_type(kommunegraph, node = current_node_id, edge_types = 2)
      }


      # Assume that it's always length 1.
      stopifnot(length(next_node_id) <= 1)
      #browser()

      # Check if the node should be followed
      if (is.na(next_node_id)){

        follow_edge_type_1 <- TRUE

      } else {

        if (forward){

          # kommunegraph %>%
          #   get_node_df() %>%
          #   filter(id == next_node_id) %>%
          #   pull(start_date) -> sd_tmp

          gndf <- DiagrammeR::get_node_df(kommunegraph)
          gndf <- dplyr::filter(gndf, id == next_node_id)
          sd_tmp <- dplyr::pull(gndf, start_date)

          if(to_date < sd_tmp){
            follow_edge_type_1 <- TRUE
          }

        } else {

          # Backward
          # kommunegraph %>%
          #   get_node_df() %>%
          #   filter(id == next_node_id) %>%
          #   pull(end_date) -> ed_tmp

          gndf <- DiagrammeR::get_node_df(kommunegraph)
          gndf <- dplyr::filter(gndf, id == next_node_id)
          ed_tmp <- pull(gndf, end_date)

          if(to_date > ed_tmp){
            follow_edge_type_1 <- TRUE
          }

        }

      }
    } else {
      follow_edge_type_1 <- TRUE
    }

    if (follow_edge_type_1){

      if (forward){
        next_node_id <- get_successors_by_edge_type(kommunegraph, node = current_node_id, edge_types = 1)
      } else {
        next_node_id <- get_predecessors_by_edge_type(kommunegraph, node = current_node_id, edge_types = 1)
      }

      # If there are multiple successors/predecessors, it means that:
      # * the municipality was split into several municipalities (forward in time)
      # * or merged (backward in time)
      if (length(next_node_id) > 1){

        if (show_warnings){
          warning(sprintf('knr %s no unambigious code translation possible from %s to %s.', knr, from_date, to_date))
        }

        current_node_id <- NA
        break
      }

      # next_node_id is NA if no sucessors.
      if (is.na(next_node_id)){
        break
      }

    }

    node_found <- check_node(kommunegraph, node_id = next_node_id, date = to_date)

    current_node_id <- next_node_id

    if (node_found){
      break
    } else {
      next
    }


  } # End while.


  #print(get_node_attrs(kommunegraph, node_attr = 'full_name', nodes = current_node_id))

  if (!is.na(current_node_id)){
    new_code <- DiagrammeR::get_node_attrs(kommunegraph, node_attr = 'code', nodes = current_node_id)
  } else {
    new_code <- NA
  }

  names(new_code) <- NULL

  return(new_code)


}



#' Translate municipal numbers from one time point to another
#'
#'
#'
#' @param knr Character. The four-digits municipal number.
#' @param from_date The date from which the knr is.
#' @param to_date The date in which to get the knr for.
#'
#' @returns A character vector with the municipal numbers used on to_date.
#'
#' @export
translate_knr <- function(knr, from_date, to_date, allow_reversals = FALSE, show_warnings = TRUE){


  # # All input must have same length
  # stopifnot(length(knr) == length(from_date),
  #           length(from_date) == length(to_date),
  #           length(show_warnings) == 1)

  stopifnot(length(from_date) == length(knr) | length(from_date) == 1,
            length(to_date) == length(knr) | length(to_date) == 1,
            length(show_warnings) == 1)

  # Check types
  stopifnot(is.character(knr),
            is_date(from_date) | is.character(from_date),
            is_date(to_date) | is.character(to_date),
            is.logical(show_warnings))

  # Coerce to Date.
  from_date <- as.Date(from_date)
  to_date <- as.Date(to_date)

  if (length(from_date) == 1){
    from_date <- rep(from_date, length(knr))
  }

  if (length(to_date) == 1){
    to_date <- rep(to_date, length(knr))
  }

  # Check the date ranges
  if (any(from_date < FIRST_DATE) | any(from_date > FINAL_DATE)){
    stop(sprintf('argument from_date out of range: %s. Must be between %s and %s', from_date, FIRST_DATE, FINAL_DATE))
  }

  if (any(to_date < FIRST_DATE) | any(to_date > FINAL_DATE)){
    stop(sprintf('argument to_date out of range: %s. Must be between %s and %s', to_date, FIRST_DATE, FINAL_DATE))
  }


  # Make id string for each input element.
  id_string <- apply(cbind(knr, from_date, to_date), MARGIN = 1, FUN = function(x){paste0(x, collapse= '')})

  # Check for duplicates
  res_idx <- match(id_string, id_string)


  n_knr <- length(knr)

  res <- numeric(n_knr)

  for (ii in 1:n_knr){

    # Check if result already computed
    if (ii > res_idx[ii]){
      res[ii] <- res[res_idx[ii]]
      next
    }

    res[ii] <- translate_knr_internal(knr = knr[ii], from_date = from_date[ii], to_date = to_date[ii], allow_reversals = allow_reversals, show_warnings = show_warnings)
  }

  return(res)

}


