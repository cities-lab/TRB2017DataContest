if (!require("pacman")) {install.packages("pacman"); library(pacman)}
p_load(rgdal)

read.shpfile <- function(shpfile, stringsAsFactors=F, ...) {
  readOGR(dsn=dirname(shpfile), 
          layer=strsplit(basename(shpfile), "[.]")[[1]][1],
          stringsAsFactors=stringsAsFactors, ...)
}
