library(leaflet)


popupContent <- paste(sep = "<br/>",
      "<b><a href='http://portal.gulfcouncil.org'>Coral portal</a></b>",
      "More text here")

df <- data.frame(x=c(-85.0), y=c(28.0), URL=popupContent)

map <- leaflet(df) %>%
  #addTiles() %>%
  addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
           options = providerTileOptions(noWrap = TRUE)) %>%
  addCircleMarkers(lng = ~x, lat=~y, popup=df$URL) %>%
  setView(-88, 27, zoom=5)
map

"<b><a href='http://portal.gulfcouncil.org'>Coral portal</a></b><br/>More text here"


