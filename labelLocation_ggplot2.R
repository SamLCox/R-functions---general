labelLocation_ggplot2 <- function(x, propLoc, locCode){
  
  # this will find the coordinate locations for placing lables on figures made via ggplot2. 
  # these coordinates can then be fed into, for example, annotate etc.
  
  # x is a ggplot item. e.g. x <- ggplot2(.....)
  # propLoc is the % inset of the label.
  # locCode is the location of the label - "topLeft", "topRight", "bottomLeft", "bottomRight".
  
  # SLC 22-09-2016
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  xLimit <- ggplot_build(x)$layout$panel_ranges[[1]]$x.range 
  yLimit <- ggplot_build(x)$layout$panel_ranges[[1]]$y.range 
  
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
  
  return(data.frame(xLoc, yLoc))
  
}
