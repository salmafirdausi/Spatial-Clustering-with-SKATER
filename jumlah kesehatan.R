library(tmap)
library(raster)
library(readxl)
library(spdep)
library(rgdal)
library(psych)
library(dplyr)

#input file shp
spBrebes = shapefile("D:/KULIAH/SEMESTER 6/00 Tugas Akhir/Peta/Peta Kabupaten Brebes.shp")
names(spBrebes)

#menampilkan peta Brebes
tm_shape(spBrebes)+tm_polygons()

#import data 
data=read_excel("D:/KULIAH/SEMESTER 6/00 Tugas Akhir/VARIABEL JUMLAH PENDIDIKAN DAN KESEHATAN.xls")

#menambahkan kolom variabel dalam file shp
spBrebes$X19=data$X19
spBrebes$X20=data$X20
spBrebes$X21=data$X21
spBrebes$X22=data$X22
spBrebes$X23=data$X23
spBrebes$X24=data$X24
spBrebes$X25=data$X25
spBrebes$X26=data$X26
names(spBrebes)

#standarisasi
data1 <- data.frame(scale(as.data.frame(spBrebes)[,9:16]))
describe(data1)
data1

#matriks pembobot
queen.nb=poly2nb(spBrebes, queen=T) #Pembobot queen
queen.nb

plot(spBrebes, border=grey(.5))
plot(queen.nb, coordinates(spBrebes), col="blue", add=TRUE)

#calculating costs
lcosts <- nbcosts(queen.nb, data1)

queen.listw=nb2listw(queen.nb, lcosts, style="W",zero.policy=TRUE) #convert nb to listw type
queen.brebes= queen.listw
queen.brebes

#menyimpan matriks pembobot
bobot.queen = listw2mat(queen.listw) #convert listw to matrix
bobot.queen

#moran test
moran.test(spBrebes$X19,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X20,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X21,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X22,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X23,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X24,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X25,queen.brebes,randomisation=FALSE)
moran.test(spBrebes$X26,queen.brebes,randomisation=FALSE)

#Data yg diteliti
data2 = select(data1, c(1,4:5,8))
describe(data2)
names(data1)

#SKATER
#Menghitung MST
#1.Menghitung edge costs
lcosts <- nbcosts(queen.nb, data2)
lcosts 

#matriks pembobot
queen.listw=nb2listw(queen.nb, lcosts, style="B",zero.policy=TRUE) #convert nb to listw type
queen.brebes= queen.listw
queen.brebes
summary(queen.brebes)

#2. MST
brebes.mst <- mstree(queen.brebes)
class(brebes.mst)
dim(brebes.mst)
head(brebes.mst)

plot(spBrebes, border=gray())
plot.mst(brebes.mst, coordinates(spBrebes), 
         col="purple", cex.lab=0.7, cex.circles=0.005, add=TRUE)

#Menghitung cluster spasial dengan SKATER
cluster3 <- skater(brebes.mst[,1:2],data2, method="euclidean", 2)
str(cluster3)
cluster3
ccs3 <- cluster3$groups
ccs3


cluster1_desa <- spBrebes$NAMOBJ[ccs3 == 1]
cluster2_desa <- spBrebes$NAMOBJ[ccs3 == 2]
cluster3_desa <- spBrebes$NAMOBJ[ccs3 == 3]

# Menampilkan nama desa dalam setiap klaster
cat("Desa dalam Cluster 1:", cluster1_desa, "\n")
cat("Desa dalam Cluster 2:", cluster2_desa, "\n")
cat("Desa dalam Cluster 3:", cluster3_desa, "\n")

data3 = select(data, c(20, 23:24, 27))
describe(data3)
mean = aggregate(data3, by = list(ccs3), FUN = mean)
mean
names(data3)

cluster4 <- skater(brebes.mst[,1:2],data2, method="euclidean", 3)
str(cluster4)
ccs4 <- cluster4$groups
ccs4
table(ccs4)

cluster5 <- skater(brebes.mst[,1:2],data2, method="euclidean", 4)
str(cluster5)
ccs5 <- cluster5$groups
ccs5
table(ccs5)

#Analisis MANOVA
uji <- manova(cbind(X19,X22,X23,X26)
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
legend("right", legend = c("Rendah", "Tinggi", "Sedang"), fill = c("yellow", "red", "purple"), title = "Cluster")