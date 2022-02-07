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

p <- read.csv("Brahmputra_basin_data.csv") ## Import the data
t=dim(p)
t[2]
mat1<-matrix(0, nrow = t[2],ncol=2, byrow = FALSE)
for(i in 1:(t[2]-1))
{
  m = mean(p[,i+1])   ## monthly mean
  mat1[i,1]<-m
  s<-sd(p[,i+1])
  mat1[i,2]<-s        ## monthly standard deviation
}
write.csv(mat1,'mean_sd.csv')

q<-read.csv("Bramhaputra_grid.csv")

chart<-list()
w <- c(62,90,180,220,290)

cols <- c("[62,90)" = "red", "[90,180)" = "blue", "[180,220)" = "darkgreen", "[220,290)" = "darkorange")
q$A1 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("11-9-2019-3772456")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="White", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") 
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[1]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Mean of River basin")


cols <- c("[62,90)" = "red", "[90,180)" = "blue", "[180,220)" = "yellow", "[220,290)" = "cyan")
q$A2 <- cut(q$Std,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("11-9-2019-3772456")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") 
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[2]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Std of River basin")





#########################


mat<-matrix(0, nrow = t[2],ncol=3, byrow = FALSE)

for(i in 1:(t[2]))
{
 
    s1<-sum(p[1:840,i])
    mat[i,1]<-(s1/70)
    
    s2<-sum(p[841:1380,i])
    mat[i,2]<-(s2/45)
    
    d<-(s1/70)-(s2/45)
    mat[i,3]<-d
}

write.csv(mat,'mean_GlobalWarming_Brahmaputra.csv')

##############

q<-read.csv("mean_GlobalWarming_Brahmaputra_Gridded.csv")

chart<-list()

w <- c(750,1462,2174,2886,3598)

cols <- c("[750,14.6e+03)" = "chartreuse", "[14.6e+03,2.17e+03)" = "yellow", "[2.17e+03,2.89e+03)" = "darkgreen", "[2.89e+03,3.6e+03)" = "blue")
q$A1 <- cut(q$Mean1,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("11-9-2019-3772456")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="White", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") 
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[1]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Avg Annual 1900-1970")


cols <- c("[750,1.46e+03)" = "orange", "[1.46e+03,2.17e+03)" = "darkorange", "[2.17e+03,2.89e+03)" = "red", "[2.89e+03,3.6e+03)" = "darkred")
q$A2 <- cut(q$Mean2,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("11-9-2019-3772456")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="White", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") 
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[2]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Avg Annual 1971-2015")

cols <- c("[750,14.6e+03)" = "lightgreen", "[14.6e+03,2.17e+03)" = "green", "[2.17e+03,2.89e+03)" = "darkgreen", "[2.89e+03,3.6e+03)" = "blue")
q$A1 <- cut(q$Mean1,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("11-9-2019-3772456")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="White", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") 
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[1]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Avg Annual 1900-1970")

w <- c(-15,35,85,135,185)

cols <- c("[-15,35)" = "lightgreen", "[35,85)" = "green", "[85,135)" = "darkgreen", "[135,185)" = "blue")
q$A1 <- cut(q$Diff,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("11-9-2019-3772456")
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
  
  for(j in 71:115)
    s2<-sum(s2,sum(p[((12*(j-1))+6):((12*(j-1))+10),i]))
  
  mat[i,2]<-(s2/45)
  
  d<-(s1/70)-(s2/45)
  mat[i,3]<-d
  
}

write.csv(mat,'Seasonal_mean_GlobalWarming_Brahmaputra.csv')

q<-read.csv("Seasonal_mean_GlobalWarming_Brahmaputra_Gridded.csv")

chart<-list()
w <- c(600,1125,1650,2175,2700)

cols <- c("[600,1.12e+03)" = "hotpink", "[1.12e+03,1.65e+03)" = "hotpink2", "[1.65e+03,2.18e+03)" = "hotpink3", "[2.18e+03,2.7e+03)" = "hotpink4")
q$A1 <- cut(q$Mean1,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("11-9-2019-3772456")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="White", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") 
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[1]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Seasonal Avg 1900-1970")


cols <- c("[600,1.12e+03)" = "indianred", "[1.12e+03,1.65e+03)" = "indianred2", "[1.65e+03,2.18e+03)" = "indianred3", "[2.18e+03,2.7e+03)" = "indianred4")
q$A2 <- cut(q$Mean2,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("11-9-2019-3772456")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="White", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude")
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[2]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Seasonal Avg 1971-2015")

w <- c(14,58,102,146,190)

cols <- c("[14,58)" = "darkorchid1", "[58,102)" = "darkorchid2", "[102,146)" = "darkorchid3", "[146,190)" = "darkorchid4")
q$A1 <- cut(q$Diff,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("11-9-2019-3772456")
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
  
  for(j in 90:115)
    s2<-sum(s2,sum(p[((12*(j-1))+6):((12*(j-1))+10),i]))
  
  mat[i,2]<-(s2/25)
  
  d<-(s1/20)-(s2/25)
  mat[i,3]<-d
  
}

write.csv(mat,'Seasonal_period_mean_GlobalWarming_Brahmaputra.csv')

q<-read.csv("Seasonal_period_mean_GlobalWarming_Brahmaputra_Gridded.csv")

chart<-list()
w <- c(598,1125,1650,2175,2700)

cols <- c("[598,1.12e+03)" = "cyan1", "[1.12e+03,1.65e+03)" = "cyan2", "[1.65e+03,2.18e+03)" = "cyan3", "[2.18e+03,2.7e+03)" = "cyan4")
q$A1 <- cut(q$Mean1,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("11-9-2019-3772456")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="White", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") 
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[1]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Seasonal Avg 1900-1920")


cols <- c("[598,1.12e+03)" = "brown1", "[1.12e+03,1.65e+03)" = "brown2", "[1.65e+03,2.18e+03)" = "brown3", "[2.18e+03,2.7e+03)" = "brown4")
q$A2 <- cut(q$Mean2,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("11-9-2019-3772456")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="White", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude")
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[2]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Seasonal Avg 1990-2015")

w <- c(-49,0,58,102,160)

cols <- c("[-49,0)" = "cadetblue1", "[0,58)" = "cadetblue2", "[58,102)" = "cadetblue3", "[102,160)" = "cadetblue4")
q$A1 <- cut(q$Diff,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y,fill = A1)) + geom_tile(aes()) +
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("11-9-2019-3772456")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="White", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude")
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[3]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Seasonal Avg Difference")

