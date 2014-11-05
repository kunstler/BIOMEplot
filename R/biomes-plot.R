##' Load biomes SpatialPolygons object
##' 
##' The Biomes are the Whittaker' biomes modified by Ricklefs (2008) in function
##' of mean annual temperature (MAT) and mean annual precipitation (MAP)
##' (MAT in degree Celsius and MAP in cm).
##' 
##' @references R.H. Whittaker. 1975. Communities and Ecosystems.
##' 2d ed. Macmillan New York.
##' 
##' @references Ricklefs, R. E. (2008). The economy of nature.
##' W. H. Freeman and Company.
##' Chapter 5, Biological Communities, The biome concept.
##'
##' @title fun.poly.obj
##' @return a SpatialPolygonsDataFrame of the biomes
##' @author Kunstler
##' @export
##' @import sp
fun.poly.obj <- function(){
  biomes.data <-  read.csv(system.file("extdata/biomes.csv",
                                       package="BIOMEplot"),
                           stringsAsFactors = FALSE)
  list.coord <- lapply(unique(biomes.data$biome),
                      function(biome.id,data)
                       cbind(data$y[data$biome == biome.id] * 10,
                             data$x[data$biome == biome.id]),
                      data=biomes.data)
  names(list.coord) <- unique(biomes.data$biome)
  list.Polygon <- lapply(list.coord, sp::Polygon)
  list.Polygons <- lapply(1:length(list.Polygon),
                          function(i, x) {sp::Polygons(list(x[[i]]),
                                                       names(x)[i])},
                          x = list.Polygon)
  poly.biomes <- sp::SpatialPolygons(list.Polygons)
  DF <- data.frame(biomes = names(poly.biomes),
                   row.names = names(poly.biomes))
  poly.DF <-  sp::SpatialPolygonsDataFrame(poly.biomes, DF)
  return(list(poly.DF = poly.DF))
}


##' Plot the biomes
##' 
##' Takes the biomes SpatialPolygons object and plot it,
##' adding or not a legend
##' 
##' @title plot.biome.map
##' @param poly biomes SpatialPolygons object 
##' @param col.biomes  vector of colors for the biomes (TODO add name of biomes)
##' @param add.legend  TRUE or FALSE
##' @return plot
##' @author Kunstler
##' @export
##' @import sp
plot_biome <- function(poly = fun.poly.obj()$poly.DF,
                           col.biomes = make.transparent(c("navajowhite3",
                                                           "darkgoldenrod1",
                                                           "sienna",
                                                           "darkolivegreen4",
                                                           "darkseagreen3",
                                                           "forestgreen",
                                                           "darkgreen",
                                                           "olivedrab",
                                                           "gray"),
                                                         0.9),
                           add.legend = TRUE){
  par(mar = c(5.1, 3.5, 0.1, 0.1),
      mgp
      = c(1.8, 0.6,0))
  plot(0,0, type = 'n', xlim = c(0, 450), ylim = c(30, -15),
       xlab = "Mean annual precipitation (cm)",
       ylab = expression(paste('Mean annual temperature ', (~degree~C))))
  # MAKE POLYGONS
  sp::plot(poly,col =  col.biomes,
           border = NA,
           add = TRUE)
   if(add.legend) {
      legend("topright", legend = poly$biomes,
             fill =make.transparent(c("navajowhite3", "darkgoldenrod1",
                                      "sienna","darkolivegreen4",
                                      "darkseagreen3", "forestgreen",
                                      "darkgreen","olivedrab","gray"),
                                      0.9),
             bty = 'n')}
}

##' make rgb color transparent
##'
##' make rgb color transparent
##' @title make.rgb.transparent
##' @param col  a vector of color
##' @param opacity percentage of opacity
##' @return a vector of colors of length length(col)
##' @author Rich
make.transparent <- function(col, opacity=0.5) {
  tmp <- col2rgb(col)/255
  rgb(tmp[1,], tmp[2,], tmp[3,], alpha=opacity)
}
