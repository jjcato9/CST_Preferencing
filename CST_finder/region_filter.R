region_filter <- function(data,input){
  df <- data
  filter_list <- c()
  if('All' %in% input){
    filter_list <- c('Health Education England East Midlands','Health Education England East of England',
                     'Health Education England Kent, Surrey and Sussex','Health Education England London',
                     'Health Education England North East','Health Education England North West',
                     'Health Education England South West','Health Education England Thames Valley',
                     'Health Education England Wessex','Health Education England West Midlands',
                     'Health Education England Yorkshire and the Humber','Scotland','Wales')
  }
  ## build filter list with selected regions
  if('East Midlands' %in% input){
    filter_list <- c(filter_list,'Health Education England East Midlands')
  }
  if('East of England' %in% input){
    filter_list <- c(filter_list,'Health Education England East of England')
  }
  if('Kent, Surrey and Sussex' %in% input){
    filter_list <- c(filter_list,'Health Education England Kent, Surrey and Sussex')
  }
  if('London' %in% input){
    filter_list <- c(filter_list,'Health Education England London')
  }
  if('North East' %in% input){
    filter_list <- c(filter_list,'Health Education England North East')
  }
  if('North West' %in% input){
    filter_list <- c(filter_list,'Health Education England North West')
  }
  if('South West' %in% input){
    filter_list <- c(filter_list,'Health Education England South West')
  }
  if('Thames Valley' %in% input){
    filter_list <- c(filter_list,'Health Education England Thames Valley')
  }
  if('Wessex' %in% input){
    filter_list <- c(filter_list,'Health Education England Wessex')
  }
  if('West Midlands' %in% input){
    filter_list <- c(filter_list,'Health Education England West Midlands')
  }
  if('Yorkshire and the Humber' %in% input){
    filter_list <- c(filter_list,'Health Education England Yorkshire and the Humber')
  }
  if('Scotland' %in% input){
    filter_list <- c(filter_list,'Scotland')
  }
  if('Wales' %in% input){
    filter_list <- c(filter_list,'Wales')
  }
  df_final <- df %>% subset(Region %in% filter_list)
  return(df_final)
  }
