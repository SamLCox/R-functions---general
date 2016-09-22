labelLocation_ggplot2 <- function(x, propLoc, locCode){
  
  # x is a ggplot item.
  # propLoc is the % inset of the label.
  # locCode is the location of the label - "topLeft", "topRight", "bottomLeft", "bottomRight".
  
  xLimit <- ggplot_build(x)$panel$ranges[[1]]$x.range 
  yLimit <- ggplot_build(x)$panel$ranges[[1]]$y.range 
  
  xRange <- xLimit[2] - xLimit[1]
  yRange <- yLimit[2] - yLimit[1]
  
  switch(locCode,
         topLeft = {
           xLoc <- xLimit[1] + ((xRange/100) * propLoc)
           yLoc <- yLimit[2] - ((yRange/100) * propLoc)
         },
         topRight = {
           xLoc <- xLimit[2] - ((xRange/100) * propLoc)
           yLoc <- yLimit[2] - ((yRange/100) * propLoc)
         },
         bottomLeft = {
           xLoc <- xLimit[1] + ((xRange/100) * propLoc)
           yLoc <- yLimit[1] + ((yRange/100) * propLoc)
         },
         bottomRight = {
           xLoc <- xLimit[2] - ((xRange/100) * propLoc)
           yLoc <- yLimit[1] + ((yRange/100) * propLoc)
         }
  )
  
  return(unname(data.frame(xLoc, yLoc)))
  
}