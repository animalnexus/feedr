get_counts <- function(c, filter = NULL, summarize_by = NULL) {

  if(!is.null(filter)){
    if("species" %in% names(filter))   c <- filter(c, species %in% filter$species)
    if("date" %in% names(filter))      c <- filter(c, date %within% interval(filter$date[1], filter$date[2]))
    if("bird_id" %in% names(filter))   c <- filter(c, bird_id %in% filter$bird_id)
    if("feeder_id" %in% names(filter)) c <- filter(c, feeder_id %in% filter$feeder_id)
  }

  if(!is.null(summarize_by)){
    c <- c %>%
      group_by_(summarize_by) %>%
      summarize(sum = sum(count)) %>%
      complete_(summarize_by, fill = list('sum' = 0)) %>%
      arrange_(summarize_by) %>%
      mutate_(name = lazyeval::interp(~ paste0(var, " (", sum, ")"), var = as.name(summarize_by)),
              variable = ~summarize_by) %>%
      rename_("choices" = summarize_by) %>%
      mutate(choices = as.character(choices))
  }
  return(c)
}



choices <- function(s, var){
  c <- s$choices[s$variable == var]
  names(c) <- s$name[s$variable == var]
  return(c)
}

selected <- function(s, var){
  s <- s$choices[s$variable == var & s$sum > 0]
  return(s)
}

# Get list of values from i and make sure all have the same levels
# i = NULL means start from scratch
# i = input (or reactive) means dealing with ui input values
# i = anything else means dealing with selection values
values_list <- function(i = NULL){
  if(any(class(i) == "reactivevalues")){
    d <- list(
      'species' = i$data_species,
      'date' = i$data_date,
      'bird_id' = i$data_bird_id,
      'feeder_id' = i$data_feeder_id)
  } else {
    if(is.null(i)) {
      i <- counts_sum
    }
    if("choices" %in% names(i)) {
      d <- list(
        'species' = selected(i, "species"),
        'date' = c(min(as.Date(selected(i, "date"))), max(as.Date(selected(i, "date")))),
        'bird_id' = selected(i, "bird_id"),
        'feeder_id' = selected(i, "feeder_id"))
    } else {
      d <- list('species' = as.character(unique(i$species)),
           'date' = c(min(i$date), max(i$date)),
           'bird_id' = as.character(unique(i$bird_id)),
           'feeder_id' = as.character(unique(i$feeder_id)))
    }
  }

  d$species <- sort(as.character(d$species))
  d$date <- as.Date(d$date)
  d$bird_id <- sort(as.character(d$bird_id))
  d$feeder_id <- sort(as.character(d$feeder_id))

  return(d)

}


