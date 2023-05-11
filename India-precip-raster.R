setwd("/Users/aayus/OneDrive/Desktop/data_spatial_yashika")
install.packages("ncdf4")
library(ncdf4)
install.packages("dplyr")
library(dplyr)
install.packages("sf")
library(sf)
india <- st_read("polbnda_ind.shp") 
install.packages("stars")
library(stars)

install.packages("sp")
library(sp)
udel_temp<-read_stars("precip_udel.nc")

class(udel_temp)
typeof(udel_temp)
dim(udel_temp)
unclass(udel_temp)
udel_temp
str(udel_temp)
names(udel_temp)
names(udel_temp)<-"temp"
st_crs(udel_temp)
st_dimensions(udel_temp)
st_bbox(udel_temp)

#Setting the coordinate reference system
st_crs(udel_temp)
udel_temp = st_set_crs(udel_temp, 4326)

r <- udel_temp
r[,1:2, seq(1, 5, 6)] |> dim()
r[,1:2,5:10]

#Setting a bounding box
b <- st_bbox(r) |>
  st_as_sfc() |>
  st_centroid() |>
  st_buffer(units::set_units(500, m))
r[b]

plot(r[b][,,1], reset = FALSE)
plot(b, border = 'brown', lwd = 2, col = NA, add = TRUE)





class(udel_temp[[1]])
units(udel_temp[[1]])<-NULL
class(udel_temp[[1]])

plot(udel_temp[,,,1:4], breaks = "equal", col = hcl.colors(5, "Spectral"), key.pos = 4)

plot(st_geometry(india), add=TRUE)
plot(st_geometry(india))
plot(st_geometry(state), border = "brown",add=TRUE)

plot(udel_temp_state[,,,1210], breaks = "equal", col = hcl.colors(12, "Spectral"), key.pos = 4, add=TRUE)


state <- india[india$nam == "RAJASTHAN",]
udel_temp_state = udel_temp[state]
plot(udel_temp_state[,,,1204:1210], breaks = "equal", col = hcl.colors(12, "Spectral"), key.pos = 4)

district <- india[india$laa == "KOLAR"]
udel_temp_dis = udel_temp[district]

layer_mean = st_apply(udel_temp, 3, mean, na.rm = TRUE)
layer_mean_mat<-layer_mean[[1]]
monthly_mean_100<-layer_mean_mat[1:1416]

monthly_mean_100<-as.data.frame(monthly_mean_100)
names(monthly_mean_100)<-"mean"

monthly_mean_100$month<-rep(1:12,118)
monthly_mean_100$year<-rep(1900:2017,each=12)

india_annual_mean<-aggregate(monthly_mean_100$mean, by=list(monthly_mean_100$year), FUN=mean) 

district1 <- india[india$laa == "TUMKUR",]
udel_temp_dis1 = udel_temp[district1]

layer_mean1 = st_apply(udel_temp_dis1, 3, mean, na.rm = TRUE)
layer_mean_mat1<-layer_mean1[[1]]
monthly_mean_100_1<-layer_mean_mat1[1:1416]

monthly_mean_100_1<-as.data.frame(monthly_mean_100_1)
names(monthly_mean_100_1)<-"mean"

monthly_mean_100_1$month<-rep(1:12,118)
monthly_mean_100_1$year<-rep(1900:2017,each=12)

india_annual_mean1<-aggregate(monthly_mean_100_1$mean, by=list(monthly_mean_100_1$year), FUN=mean)

district2 <- india[india$laa == "MYSORE",]
udel_temp_dis2 = udel_temp[district2]

layer_mean2 = st_apply(udel_temp_dis2, 3, mean, na.rm = TRUE)
layer_mean_mat2<-layer_mean2[[1]]
monthly_mean_100_2<-layer_mean_mat2[1:1416]

monthly_mean_100_2<-as.data.frame(monthly_mean_100_2)
names(monthly_mean_100_2)<-"mean"

monthly_mean_100_2$month<-rep(1:12,118)
monthly_mean_100_2$year<-rep(1900:2017,each=12)

india_annual_mean2<-aggregate(monthly_mean_100_2$mean, by=list(monthly_mean_100_2$year), FUN=mean)


district3 <- india[india$laa == "HASSAN",]
udel_temp_dis3 = udel_temp[district3]

layer_mean3 = st_apply(udel_temp_dis3, 3, mean, na.rm = TRUE)
layer_mean_mat3<-layer_mean3[[1]]
monthly_mean_100_3<-layer_mean_mat3[1:1416]

monthly_mean_100_3<-as.data.frame(monthly_mean_100_3)
names(monthly_mean_100_3)<-"mean"

monthly_mean_100_3$month<-rep(1:12,118)
monthly_mean_100_3$year<-rep(1900:2017,each=12)

india_annual_mean3<-aggregate(monthly_mean_100_3$mean, by=list(monthly_mean_100_3$year), FUN=mean)


district4 <- india[india$laa == "CHITRADURGA",]
udel_temp_dis4 = udel_temp[district4]

layer_mean4 = st_apply(udel_temp_dis4, 3, mean, na.rm = TRUE)
layer_mean_mat4<-layer_mean4[[1]]
monthly_mean_100_4<-layer_mean_mat4[1:1416]

monthly_mean_100_4<-as.data.frame(monthly_mean_100_4)
names(monthly_mean_100_4)<-"mean"

monthly_mean_100_4$month<-rep(1:12,118)
monthly_mean_100_4$year<-rep(1900:2017,each=12)

india_annual_mean4<-aggregate(monthly_mean_100_4$mean, by=list(monthly_mean_100_4$year), FUN=mean)


district6 <- india[india$laa == "DHARWAD",]
udel_temp_dis6 = udel_temp[district6]

layer_mean6 = st_apply(udel_temp_dis6, 3, mean, na.rm = TRUE)
layer_mean_mat6<-layer_mean6[[1]]
monthly_mean_100_6<-layer_mean_mat6[1:1416]

monthly_mean_100_6<-as.data.frame(monthly_mean_100_6)
names(monthly_mean_100_6)<-"mean"

monthly_mean_100_6$month<-rep(1:12,118)
monthly_mean_100_6$year<-rep(1900:2017,each=12)

india_annual_mean6<-aggregate(monthly_mean_100_6$mean, by=list(monthly_mean_100_6$year), FUN=mean)


district7 <- india[india$laa == "BELGAUM",]
udel_temp_dis7 = udel_temp[district7]

layer_mean7 = st_apply(udel_temp_dis7, 3, mean, na.rm = TRUE)
layer_mean_mat7<-layer_mean7[[1]]
monthly_mean_100_7<-layer_mean_mat7[1:1416]

monthly_mean_100_7<-as.data.frame(monthly_mean_100_7)
names(monthly_mean_100_7)<-"mean"

monthly_mean_100_7$month<-rep(1:12,118)
monthly_mean_100_7$year<-rep(1900:2017,each=12)

india_annual_mean7<-aggregate(monthly_mean_100_7$mean, by=list(monthly_mean_100_7$year), FUN=mean)

district8 <- india[india$laa == "BIJAPUR",]
udel_temp_dis8 = udel_temp[district8]

layer_mean8 = st_apply(udel_temp_dis8, 3, mean, na.rm = TRUE)
layer_mean_mat8<-layer_mean8[[1]]
monthly_mean_100_8<-layer_mean_mat8[1:1416]

monthly_mean_100_8<-as.data.frame(monthly_mean_100_8)
names(monthly_mean_100_8)<-"mean"

monthly_mean_100_8$month<-rep(1:12,118)
monthly_mean_100_8$year<-rep(1900:2017,each=12)

india_annual_mean8<-aggregate(monthly_mean_100_8$mean, by=list(monthly_mean_100_8$year), FUN=mean)

district9 <- india[india$laa == "BIDAR",]
udel_temp_dis9 = udel_temp[district9]

layer_mean9 = st_apply(udel_temp_dis9, 3, mean, na.rm = TRUE)
layer_mean_mat9<-layer_mean9[[1]]
monthly_mean_100_9<-layer_mean_mat9[1:1416]

monthly_mean_100_9<-as.data.frame(monthly_mean_100_9)
names(monthly_mean_100_9)<-"mean"

monthly_mean_100_9$month<-rep(1:12,118)
monthly_mean_100_9$year<-rep(1900:2017,each=12)

india_annual_mean9<-aggregate(monthly_mean_100_9$mean, by=list(monthly_mean_100_9$year), FUN=mean)


district10 <- india[india$laa == "RAICHUR",]
udel_temp_dis10 = udel_temp[district10]

layer_mean10 = st_apply(udel_temp_dis10, 3, mean, na.rm = TRUE)
layer_mean_mat10<-layer_mean10[[1]]
monthly_mean_100_10<-layer_mean_mat10[1:1416]

monthly_mean_100_10<-as.data.frame(monthly_mean_100_10)
names(monthly_mean_100_10)<-"mean"

monthly_mean_100_10$month<-rep(1:12,118)
monthly_mean_100_10$year<-rep(1900:2017,each=12)

india_annual_mean10<-aggregate(monthly_mean_100_10$mean, by=list(monthly_mean_100_10$year), FUN=mean)


district12 <- india[india$laa == "BANGALORE RURAL",]
udel_temp_dis12 = udel_temp[district12]

layer_mean12 = st_apply(udel_temp_dis12, 3, mean, na.rm = TRUE)
layer_mean_mat12<-layer_mean12[[1]]
monthly_mean_100_12<-layer_mean_mat12[1:1416]

monthly_mean_100_12<-as.data.frame(monthly_mean_100_12)
names(monthly_mean_100_12)<-"mean"

monthly_mean_100_12$month<-rep(1:12,118)
monthly_mean_100_12$year<-rep(1900:2017,each=12)

india_annual_mean12<-aggregate(monthly_mean_100_12$mean, by=list(monthly_mean_100_12$year), FUN=mean)

district13 <- india[india$laa == "SHIMOGA",]
udel_temp_dis13 = udel_temp[district13]

layer_mean13 = st_apply(udel_temp_dis13, 3, mean, na.rm = TRUE)
layer_mean_mat13<-layer_mean13[[1]]
monthly_mean_100_13<-layer_mean_mat13[1:1416]

monthly_mean_100_13<-as.data.frame(monthly_mean_100_13)
names(monthly_mean_100_13)<-"mean"

monthly_mean_100_13$month<-rep(1:12,118)
monthly_mean_100_13$year<-rep(1900:2017,each=12)

india_annual_mean13<-aggregate(monthly_mean_100_13$mean, by=list(monthly_mean_100_13$year), FUN=mean)

district14 <- india[india$laa == "CHIKMAGALUR",]
udel_temp_dis14 = udel_temp[district14]

layer_mean14 = st_apply(udel_temp_dis14, 3, mean, na.rm = TRUE)
layer_mean_mat14<-layer_mean14[[1]]
monthly_mean_100_14<-layer_mean_mat14[1:1416]

monthly_mean_100_14<-as.data.frame(monthly_mean_100_14)
names(monthly_mean_100_14)<-"mean"

monthly_mean_100_14$month<-rep(1:12,118)
monthly_mean_100_14$year<-rep(1900:2017,each=12)

india_annual_mean14<-aggregate(monthly_mean_100_14$mean, by=list(monthly_mean_100_14$year), FUN=mean)


district15 <- india[india$laa == "UTTAR KANNAD",]
udel_temp_dis15 = udel_temp[district15]

layer_mean15 = st_apply(udel_temp_dis15, 3, mean, na.rm = TRUE)
layer_mean_mat15<-layer_mean15[[1]]
monthly_mean_100_15<-layer_mean_mat15[1:1416]

monthly_mean_100_15<-as.data.frame(monthly_mean_100_15)
names(monthly_mean_100_15)<-"mean"

monthly_mean_100_15$month<-rep(1:12,118)
monthly_mean_100_15$year<-rep(1900:2017,each=12)

india_annual_mean15<-aggregate(monthly_mean_100_15$mean, by=list(monthly_mean_100_15$year), FUN=mean)

state1 <- india[india$nam == "KARNATKA",]
udel_temp_state = udel_temp[state1]
plot(udel_temp_state[,,,1081:1092], breaks = "equal", col = hcl.colors(11, "Spectral"), reset = FALSE)

state1 <- india[india$nam == "RAJASTHAN"]
udel_temp_state = udel_temp[state1]
plot(udel_temp_state[,,,1:4], breaks = "equal", col = hcl.colors(11, "Spectral"), key.pos = 4)


