library(tmap)
library(raster)
library(readxl)
library(spdep)
library(rgdal)
library(psych)
library(dplyr)
library(skater)

#input file shp
spBrebes = shapefile("D:/KULIAH/SEMESTER 6/00 Tugas Akhir/Peta/Peta Kabupaten Brebes.shp")
names(spBrebes)

#menampilkan peta Brebes
tm_shape(spBrebes)+tm_polygons()

#import data 
data=read_excel("D:/KULIAH/SEMESTER 6/00 Tugas Akhir/VARIABEL JUMLAH PENDIDIKAN DAN KESEHATAN.xls")
data
#menambahkan kolom variabel dalam file shp
spBrebes$X1=data$X1
spBrebes$X2=data$X2
spBrebes$X3=data$X3
spBrebes$X4=data$X4
spBrebes$X5=data$X5
spBrebes$X6=data$X6
spBrebes$X7=data$X7
spBrebes$X8=data$X8
spBrebes$X9=data$X9
spBrebes$X10=data$X10
spBrebes$X11=data$X11
spBrebes$X12=data$X12
spBrebes$X13=data$X13
spBrebes$X14=data$X14
spBrebes$X15=data$X15
spBrebes$X16=data$X16
spBrebes$X17=data$X17
spBrebes$X18=data$X18

names(spBrebes)

#standarisasi data
data1 <- data.frame(scale(as.data.frame(spBrebes)[,9:26]))
describe(data1)
data1

#Neighbour List
queen.nb=poly2nb(spBrebes, queen=T) #Pembobot queen
queen.nb

#Memeriksa link antar lokasi bertetanggan
xy <- sp::coordinates(spBrebes)
plot(spBrebes, border=grey(.5))
plot(queen.nb, coordinates(spBrebes), col="blue", add=TRUE)

#calculating costs
lcosts <- nbcosts(queen.nb, data1)
lcosts

#convert nb to listw type
queen.listw=nb2listw(queen.nb, lcosts, style="B",zero.policy=TRUE) 
queen.brebes= queen.listw
queen.brebes

#menyimpan matriks pembobot
bobot.queen = listw2mat(queen.listw) #convert listw to matrix
bobot.queen

#moran test
moran.test(spBrebes$X1,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X2,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X3,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X4,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X5,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X6,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X7,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X8,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X9,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X10,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X11,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X12,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X13,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X14,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X15,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X16,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X17,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X18,queen.brebes,randomisation=FALSE)


#Data yg diteliti
data2 = select(data1, c(1:7, 10, 12, 14, 18))
names(data2)

#SKATER
#Menghitung MST
#1.Menghitung edge costs
lcosts <- nbcosts(queen.nb, data2)
lcosts

queen.listw=nb2listw(queen.nb, lcosts, style="B",zero.policy=TRUE) #convert nb to listw type
queen.brebes= queen.listw
queen.brebes
summary(queen.brebes)

#2. MST
brebes.mst <- mstree(queen.brebes)
class(brebes.mst)
dim(brebes.mst)
head(brebes.mst)
#Hasil MST
plot(spBrebes, border=gray())
plot.mst(brebes.mst, coordinates(spBrebes), 
         col="blue", cex.lab=0.7, cex.circles=0.005, add=TRUE)

#Menghitung cluster spasial dengan SKATER
cluster3 <- skater(brebes.mst[,1:2],data2, method="euclidean", 2)
cluster3
str(cluster3)
ccs3 <- cluster3$groups
ccs3
table(ccs3)

cluster1_desa <- spBrebes$NAMOBJ[ccs3 == 1]
cluster2_desa <- spBrebes$NAMOBJ[ccs3 == 2]
cluster3_desa <- spBrebes$NAMOBJ[ccs3 == 3]

# Menampilkan nama desa dalam setiap klaster
cat("Desa dalam Cluster 1:", cluster1_desa, "\n")
cat("Desa dalam Cluster 2:", cluster2_desa, "\n")
cat("Desa dalam Cluster 3:", cluster3_desa, "\n")

data3 = select(data, c(2:8, 11, 13, 15, 19))
describe(data3)
mean = aggregate(data3, by = list(ccs3), FUN = mean)
mean

# Menghitung nilai rata-rata setiap variabel dalam Cluster 1
mean_cluster1 <- colMeans(data_cluster1)

# Mencari desa dengan nilai terendah dalam Cluster 1
desa_terburuk <- cluster1_desa[which.min(mean_cluster1)]
cat("Desa dengan performa terburuk di Cluster 1:", desa_terburuk, "\n")



cluster4 <- skater(brebes.mst[,1:2],data2, method="euclidean", 3)
str(cluster4)
ccs4 <- cluster4$groups
ccs4
table(ccs4)

cluster5 <- skater(brebes.mst[,1:2],data2, method="euclidean",4)
str(cluster5)
ccs5 <- cluster5$groups
ccs5
table(ccs5)

#Analisis MANOVA
uji <- manova(cbind(X1,X2,X3,X4,X5,X6,X7,X10,X12,X14,X18)
              ~ ccs3 + ccs4 + ccs5, data=data2)
uji
summary.manova(uji, test = "Wilks")
summary.manova(uji)
summary.manova(uji, test = "Roy")
summary.manova(uji, test = "Hotelling-Lawley")

#Partisi MST
plot(spBrebes, border=gray())
plot(cluster3, coordinates(spBrebes), cex.lab=.7,
     groups.colors=c("yellow","red","purple"), cex.circles=0.005, add=TRUE)

#Memvisualisasikan cluster dalam peta 
plot(spBrebes,col=c("yellow","red","purple")[cluster3$groups])

plot(spBrebes, col = c("yellow", "red", "purple")[cluster3$groups])
legend("right", legend = unique(cluster3$groups), fill = c("yellow", "red", "purple"), title = "Cluster")

plot(spBrebes, col = c("yellow", "red", "purple")[cluster3$groups])
legend("right", legend = c("Rendah", "Tinggi", "Sedang"), fill = c("yellow", "red", "purple"), title = "Cluster")