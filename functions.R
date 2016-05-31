get_counts <- function(c, filter = NULL, summarize_by = NULL) {
  
  if(!is.null(filter)){
    if("site_name" %in% names(filter)) c <- filter(c, site_name %in% filter$site_name)
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


# Get list of values from i and make sure all have the same levels
# i = NULL means start from scratch
# i = input (or reactive) means dealing with ui input values
# i = anything else means dealing with selection values
choices <- function(s, var){
  c <- s$choice[s$var == var]
  names(c) <- s$name[s$var == var]
  return(c)
}

selected <- function(s, var){
  s <- s$choice[s$var == var & s$sum > 0]
  return(s)
}

values_list <- function(i = NULL){
  if(any(class(i) == "reactivevalues")){
    d <- list(
      'site_name' = i$data_site_name,
      'species' = i$data_species,
      'date' = i$data_date,
      'bird_id' = if("data_bird_id" %in% names(i)) i$data_bird_id else unique(counts$bird_id[counts$site_name %in% i$data_site_name & counts$species %in% i$data_species & counts$date >= i$data_date[1] & counts$date <= i$data_date[2]]),
      'feeder_id' = if("data_feeder_id" %in% names(i)) i$data_feeder_id else unique(counts$feeder_id[counts$site_name %in% i$data_site_name & counts$species %in% i$data_species & counts$date >= i$data_date[1] & counts$date <= i$data_date[2]]))
  } else {
    if(is.null(i)) {
      i <- counts_sum
    }
    d <- list(
      'site_name' = selected(i, "site_name"),
      'species' = selected(i, "species"),
      'date' = c(min(as.Date(selected(i, "date"))), max(as.Date(selected(i, "date")))),
      'bird_id' = selected(i, "bird_id"),
      'feeder_id' = selected(i, "feeder_id"))
  }
  
  d$site_name <- factor(sort(d$site_name), levels = sort(sites_all$site_name))
  d$species <- factor(sort(d$species), levels = unique(sort(birds_all$species)))
  d$date <- as.Date(d$date)
  d$bird_id <- factor(sort(d$bird_id), levels = sort(birds_all$bird_id))
  d$feeder_id <- factor(sort(d$feeder_id), levels = sort(feeders_all$feeder_id))
  
  return(d)
  
}


