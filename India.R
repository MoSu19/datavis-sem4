
setwd("E:\Mohit Suhasaria\VIT\VIT2020\4th Sem\CSE3020_DV\DA\Data vis _lab")
par(mar = c(10,10,10,10))

library(ggplot2)
library(grid)
library(gridExtra)
library(extrafont)
library(ggmap)
library(maptools)
library(plyr)
library(maps)

p <- read.csv("India_gridded_data.csv") 
t=dim(p)
t[2]
mat1<-matrix(0, nrow = t[2],ncol=2, byrow = FALSE)
for(i in 1:(t[2]))
{
  m = mean(p[,i])  
  mat1[i,1]<-m
  s<-sd(p[,i])
  mat1[i,2]<-s       
}
write.csv(mat1,'mean_sd_India.csv')

q<-read.csv("mean_sd_India_grid.csv")

chart<-list()
w <- c(0,90,180,300,400)

cols <- c("[0,90)" = "red", "[90,180)" = "blue", "[180,300)" = "darkgreen", "[300,400)" = "darkorange")
q$A1 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="White", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") 
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[1]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Mean of Indian grid")


cols <- c("[0,90)" = "red", "[90,180)" = "blue", "[180,300)" = "yellow", "[300,400)" = "cyan")
q$A2 <- cut(q$Std,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") 
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[2]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Std of Indian grid")





#########################


mat<-matrix(0, nrow = t[2],ncol=3, byrow = FALSE)

for(i in 1:(t[2]))
{
  
  s1<-sum(p[1:840,i])
  mat[i,1]<-(s1/70)
  
  s2<-sum(p[841:1368,i])
  mat[i,2]<-(s2/44)
  
  d<-(s1/70)-(s2/44)
  mat[i,3]<-d
}

write.csv(mat,'mean_GlobalWarming_India.csv')

##############

q<-read.csv("mean_GlobalWarming_India_Gridded.csv")

chart<-list()

w <- c(50,1000,1500,2500,4500)

cols <- c("[50,1e+03)" = "chartreuse", "[1e+03,1.5e+03)" = "yellow", "[1.5e+03,2.5e+03)" = "darkgreen", "[2.5e+03,4.5e+03)" = "blue")
q$A1 <- cut(q$Mean1,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="White", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") 
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[1]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Avg Annual 1900-1970")


cols <- c("[50,1e+03)" = "orange", "[1e+03,1.5e+03)" = "darkorange", "[1.5e+03,2.5e+03)" = "red", "[2.5e+03,4.5e+03)" = "darkred")
q$A2 <- cut(q$Mean2,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="White", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") 
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[2]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Avg Annual 1971-2014")


w <- c(-215,-100,0,100,250)

cols <- c("[-215,-100)" = "lightgreen", "[-100,0)" = "green", "[0,100)" = "darkgreen", "[100,250)" = "blue")
q$A1 <- cut(q$Diff,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="White", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") 
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[3]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Difference in avg")


############################################################
##########################################################


mat<-matrix(0, nrow = t[2],ncol=3, byrow = FALSE)

for(i in 1:(t[2]))
{
  s1<-NULL
  s2<-NULL
  
  for(j in 1:70)
    s1<-sum(s1,sum(p[((12*(j-1))+6):((12*(j-1))+10),i]))
  
  mat[i,1]<-(s1/70)
  
  for(j in 71:114)
    s2<-sum(s2,sum(p[((12*(j-1))+6):((12*(j-1))+10),i]))
  
  mat[i,2]<-(s2/44)
  
  d<-(s1/70)-(s2/44)
  mat[i,3]<-d
  
}

write.csv(mat,'Seasonal_mean_GlobalWarming_India.csv')

q<-read.csv("Seasonal_mean_GlobalWarming_India_Gridded.csv")

chart<-list()
w <- c(2,750,1650,2500,3300)

cols <- c("[2,750)" = "hotpink", "[750,1.65e+03)" = "hotpink2", "[1.65e+03,2.5e+03)" = "hotpink3", "[2.5e+03,3.3e+03)" = "hotpink4")
q$A1 <- cut(q$Mean1,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="White", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") 
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[1]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Seasonal Avg 1900-1970")


cols <- c("[2,750)" = "indianred", "[750,1.65e+03)" = "indianred2", "[1.65e+03,2.5e+03)" = "indianred3", "[2.5e+03,3.3e+03)" = "indianred4")
q$A2 <- cut(q$Mean2,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="White", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude")
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[2]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Seasonal Avg 1971-2014")

w <- c(-206,-100,0,146,290)

cols <- c("[-206,-100)" = "darkorchid1", "[-100,0)" = "darkorchid2", "[0,146)" = "darkorchid3", "[146,290)" = "darkorchid4")
q$A1 <- cut(q$Diff,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="White", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude")
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[3]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Seasonal Avg Difference")

###########################################################
###########################################################


mat<-matrix(0, nrow = t[2],ncol=3, byrow = FALSE)

for(i in 1:(t[2]))
{
  s1<-NULL
  s2<-NULL
  
  for(j in 1:20)
    s1<-sum(s1,sum(p[((12*(j-1))+6):((12*(j-1))+10),i]))
  
  mat[i,1]<-(s1/20)
  
  for(j in 90:114)
    s2<-sum(s2,sum(p[((12*(j-1))+6):((12*(j-1))+10),i]))
  
  mat[i,2]<-(s2/24)
  
  d<-(s1/20)-(s2/24)
  mat[i,3]<-d
  
}

write.csv(mat,'Seasonal_period_mean_GlobalWarming_India.csv')

q<-read.csv("Seasonal_period_mean_GlobalWarming_India_Gridded.csv")

chart<-list()
w <- c(0,750,1650,2175,3300)

cols <- c("[0,750)" = "cyan1", "[750,1.65e+03)" = "cyan2", "[1.65e+03,2.18e+03)" = "cyan3", "[2.18e+03,3.3e+03)" = "cyan4")
q$A1 <- cut(q$Mean1,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="White", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") 
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[1]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Seasonal Avg 1900-1920")


cols <- c("[0,750)" = "brown1", "[750,1.65e+03)" = "brown2", "[1.65e+03,2.18e+03)" = "brown3", "[2.18e+03,3.3e+03)" = "brown4")
q$A2 <- cut(q$Mean2,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="White", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude")
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[2]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Seasonal Avg 1990-2014")

w <- c(-623,-180,0,102,251)

cols <- c("[-623,-180)" = "cadetblue1", "[-180,0)" = "cadetblue2", "[0,102)" = "cadetblue3", "[102,251)" = "cadetblue4")
q$A1 <- cut(q$Diff,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="White", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude")
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[3]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Seasonal Avg Difference")

