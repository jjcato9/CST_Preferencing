map_builder <- function(data) {
  loc <- read.csv('data/locations_correct.csv')
  data_summary <- data %>% group_by(Region) %>% summarise(n=n())
  map_data <- merge(loc,data_summary,by.x='regions',by.y='Region')
  
  worldmap <- map_data('world')
  
  g <- list(
    scope = 'europe',
    showland = TRUE,
    landcolor = toRGB("gray95"),
    subunitcolor = toRGB("gray85"),
    countrycolor = toRGB("gray85"),
    countrywidth = 0.5,
    subunitwidth = 0.5
  )
  
  fig <- plot_geo(map_data, lat = ~lat, lon = ~lon,size = ~n)
  fig <- fig %>% add_markers(
    text = ~paste(map_data$regions,map_data$n,sep = ': '),hoverinfo = "text")
  fig <- fig %>% layout(geo = g)
  
  fig
}

