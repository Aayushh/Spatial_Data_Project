#https://r-spatial.org/book/01-hello.html#fig-first-map
library(tidyverse)
library(sf)
system.file("gpkg/nc.gpkg", package="sf") |>
  read_sf() -> nc
nc.32119 <- st_transform(nc, 'EPSG:32119')
nc.32119 |>
  select(BIR74) |>
  plot(graticule = TRUE, axes = TRUE)

#Print first 3 lines 
nc |> print(n=3)
nc |> select(AREA, BIR74, SID74) |> print(n = 3)

library(mapview) |> suppressPackageStartupMessages()
mapviewOptions(fgb = FALSE)
nc.32119 |> mapview(zcol = "BIR74", legend = TRUE, col.regions = sf.colors)

#Creating a sf structure from scratch
library(sf)
# Linking to GEOS 3.11.1, GDAL 3.6.2, PROJ 9.1.1; sf_use_s2() is TRUE
p1 <- st_point(c(7.35, 52.42))
p2 <- st_point(c(7.22, 52.18))
p3 <- st_point(c(7.44, 52.19))
sfc <- st_sfc(list(p1, p2, p3), crs = 'OGC:CRS84')
st_sf(elev = c(33.2, 52.1, 81.2), 
      marker = c("Id01", "Id02", "Id03"), geom = sfc)|>
  plot(st_geometry())


#Reading FIles
system.file("gpkg/nc.gpkg", package = "sf") |>
st_read() -> nc
# st_read requires file path and layer number, to query number of layers use
st_layers()
#Writing files use- st_write()
(file = tempfile(fileext = ".gpkg"))
# [1] "/tmp/RtmpOZFI4M/file1228b52e9be34.gpkg"
st_write(nc, file, layer = "layer_nc")

#Subsetting - subsetting a part of the data 
nc[2:5,3:7] #select rows 2-5 and columns 3-7

#Checking intersection of polygons
nc5 <- nc[1:5, ]
nc7 <- nc[1:7, ]
(i <- st_intersects(nc5, nc7))
plot(st_geometry(nc7))
plot(st_geometry(nc5), add = TRUE, border = "brown")
cc = st_coordinates(st_centroid(st_geometry(nc7)))
text(cc, labels = 1:nrow(nc7), col = "blue")

##Chp 7 Stars
tif <- system.file("tif/L7_ETMs.tif", package = "stars")
library(stars)
(r <- read_stars(tif))

length(r)
# [1] 1
class(r[[1]])
# [1] "array"
dim(r[[1]])
#    x    y band 
#  349  352    6
st_bbox(r)
filter(r, x > 289000, x < 290000)
r[,1:100, seq(1, 250, 5), 4] |> dim()



