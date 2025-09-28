# R Programming Language Codes for the manuscript:
# "Do Technology Usage and Innovation Promote an Environmental Friendly Economy? Evidence from Selected 30 Asian Economies"
#

install.packages("Benchmarking")
library(Benchmarking)
library(FEAR) # Install from https://pww.people.clemson.edu/Software/FEAR/fear.html

setwd("E:/Drive/Calismalar/Akademik/DR Sonrasi/Makaleler/Bireysel Makaleler/Efficiency/data")

D <-as.matrix(read.table("datain.txt"))   
X <-as.matrix(D[,c(1:8)])
X1<-as.matrix(D[,c(2:8)])
X2<-as.matrix(D[,c(1,3:8)])
X3<-as.matrix(D[,c(1:2,4:8)])
X4<-as.matrix(D[,c(1:3,5:8)])
X5<-as.matrix(D[,c(1:4,6:8)])
X6<-as.matrix(D[,c(1:5,7:8)])
X7<-as.matrix(D[,c(1:6,8)])
X8<-as.matrix(D[,c(1:7)])

  
Y1<-as.matrix(D[,9])
Y2<-as.matrix(D[,10])
name <- c("JOR","ISR","LBN","YEM","KHM","CHN","IDN","LAO","MYS","MNG","MMR","PNG","THA",
          "PHL","SLB","VNM","KAZ","KGZ","ARM","AZE","RUS","UKR","BGD","IND","GEO","NPL","PAK","LKA","TJK","TUR")

# Super Efficiency Score Calculations
# ------------------------------------------------
ecef <- sdea(X,Y1, RTS="crs",ORIENTATION = "out") # Economic Efficiency
ecef0<-  dea(t(X),t(Y1), RTS=3 ,ORIENTATION=1)
ecef <- as.matrix(cbind(name,format(round(ecef$eff, digits=3),nsmall=3)))
ecef <- data.frame(ecef)
ecef[,3:4]<- cbind(format(round(1/as.numeric(ecef$V2),digits=3),nsmall=3),format(round(ecef0,digits=3),nsmall=3))
colnames(ecef)  <- c("Country","Economical S.Ef.(Theta)","1 / Theta","Efficiency")
#ecef <- ecef[order(ecef[,2]),]  # To get ordered scores delete the first "#"
ecef

# ------------------------------------------------
enef <- sdea(X,Y2, RTS="crs",ORIENTATION = "out") # Environment In-Efficiency
enef0<-  dea(t(X),t(Y2), RTS=3 ,ORIENTATION=1)
enef <- as.matrix(cbind(name,format(round(enef$eff, digits=3),nsmall=3)))
enef <- data.frame(enef)
enef[,3:4]<- cbind(format(round(1/as.numeric(enef$V2),digits=3),nsmall=3),format(round(enef0,digits=3),nsmall=3))
colnames(enef)  <- c("Country","Environmetal S.Ef.(Theta)","1 / Theta","Efficiency")
#enef <- enef[order(enef[,2]),]  # To get ordered scores delete the first "#"
enef

cbind(ecef,enef)

bitmap("Figure3.tiff", height = 4, width = 4, units = 'in', type="tifflzw", res=300)
plot(x, y)
dev.off()
par(mfrow = c(1,1))

# Graphical Outcomes
windowsFonts(A = windowsFont("Times New Roman"))

Sys.setenv(R_GSCMD="C:/Program Files/gs/gs10.04.0/bin/gswin64c.exe")
# Figure 3 ------------------------------------------------
ecef   <- eff(sdea(X,Y1,RTS="crs",ORIENTATION = "out"))
eninef <- eff(sdea(X,Y2,RTS="crs",ORIENTATION = "out"))
ecef   <- as.matrix(1/ecef)
eninef <- as.matrix(1/eninef)
row.names(ecef) <- c("JOR","ISR","LBN","YEM","KHM","CHN","IDN","LAO","MYS","MNG","MMR","PNG","THA","PHL","SLB","VNM","KAZ","KGZ","ARM","AZE","RUS","UKR","BGD","IND","GEO","NPL","PAK","LKA","TJK","TUR")
sel             <- c(10,3,8,21,9,13,30,29,2,14,7,1,16,26,17,22,18,24,25,12)
tiff("Figure3.tiff", height = 5.5, width = 5.5, units = 'in', res=300)
plot(ecef,eninef, xlim=range(ecef,eninef), family="A", ylim=range(ecef,eninef),xlab="Economic Efficiency",ylab="Environmental Inefficiency",pch=16,cex.lab=1.2,cex.axis = 1.1,
panel.first=abline(0,1,lwd=2, col="red"))
text(ecef[sel],eninef[sel],labels=row.names(ecef)[sel], cex=0.8,pos=4,col="blue")
dev.off()

# Figure 4 ------------------------------------------------
K     <- length(ecef)
tiff("Figure4.tiff", height = 5.8, width = 5.8, units = 'in', res=300)
plot(sort(ecef), (1:K)/K, type="s",family="A" , ylim=c(0,1),ylab="Probability", xlab="Efficiency",cex.lab=1.2,cex.axis = 1.1,lwd=1.5)
lines(sort(eninef), (1:K)/K, type="s", lty="dashed",lwd=1.5)
rect(xleft=1.5, xright=2.28, ybottom = par("usr")[3], ytop = par("usr")[4], 
  border = NA, col = adjustcolor("red", alpha = 0.3))
rect(xleft=0.66, xright=0.975, ybottom = par("usr")[3], ytop = par("usr")[4], 
  border = NA, col = adjustcolor("red", alpha = 0.3))
rect(xleft=1.0416609, xright=1.05794538, ybottom = par("usr")[3], ytop = par("usr")[4], 
  border = NA, col = adjustcolor("red", alpha = 0.3))
rect(xleft=1.2905757, xright=1.31123253, ybottom = par("usr")[3], ytop = par("usr")[4], 
  border = NA, col = adjustcolor("red", alpha = 0.3))
legend("bottomright",c("Eco.Efficiency","Env.Inefficiency"),lty=c("solid","dashed"),bty="n", cex=1.1, text.font = 6)
dev.off()
#-------------------------------------------------
res <-as.matrix(read.table("res.txt"))   
res <-res[order(res[,2]),]
plot(res[,2], res[,1], type="l",ylab="Estimated Env. Inefficiency", xlab="Income per Capita",cex.lab=1.2,cex.axis = 1.1,lwd=1.5, pch=16)

# Average Super Efficiency
ecef <- sdea(X,Y1, RTS="crs",ORIENTATION = "out") # Economic Efficiency
enef <- sdea(X,Y2, RTS="crs",ORIENTATION = "out") # Environment In-Efficiency
round(mean(ecef$eff),3)
round(mean(enef$eff),3)
#1
ecef1 <- sdea(X1,Y1, RTS="crs",ORIENTATION = "out") # Economic Efficiency
enef1 <- sdea(X1,Y2, RTS="crs",ORIENTATION = "out") # Environment In-Efficiency
round(mean(ecef1$eff),3)
round(mean(enef1$eff),3)
#2
ecef2 <- sdea(X2,Y1, RTS="crs",ORIENTATION = "out") # Economic Efficiency
enef2 <- sdea(X2,Y2, RTS="crs",ORIENTATION = "out") # Environment In-Efficiency
round(mean(ecef2$eff),3)
round(mean(enef2$eff),3)
#3
ecef3 <- sdea(X3,Y1, RTS="crs",ORIENTATION = "out") # Economic Efficiency
enef3 <- sdea(X3,Y2, RTS="crs",ORIENTATION = "out") # Environment In-Efficiency
round(mean(ecef3$eff),3)
round(mean(enef3$eff),3)
#4
ecef4 <- sdea(X4,Y1, RTS="crs",ORIENTATION = "out") # Economic Efficiency
enef4 <- sdea(X4,Y2, RTS="crs",ORIENTATION = "out") # Environment In-Efficiency
round(mean(ecef4$eff),3)
round(mean(enef4$eff),3)
#5
ecef5 <- sdea(X5,Y1, RTS="crs",ORIENTATION = "out") # Economic Efficiency
enef5 <- sdea(X5,Y2, RTS="crs",ORIENTATION = "out") # Environment In-Efficiency
round(mean(ecef5$eff),3)
round(mean(enef5$eff),3)
#6
ecef6 <- sdea(X6,Y1, RTS="crs",ORIENTATION = "out") # Economic Efficiency
enef6 <- sdea(X6,Y2, RTS="crs",ORIENTATION = "out") # Environment In-Efficiency
round(mean(ecef6$eff),3)
round(mean(enef6$eff),3)
#7
ecef7 <- sdea(X7,Y1, RTS="crs",ORIENTATION = "out") # Economic Efficiency
enef7 <- sdea(X7,Y2, RTS="crs",ORIENTATION = "out") # Environment In-Efficiency
round(mean(ecef7$eff),3)
round(mean(enef7$eff),3)
#8
ecef8 <- sdea(X8,Y1, RTS="crs",ORIENTATION = "out") # Economic Efficiency
enef8 <- sdea(X8,Y2, RTS="crs",ORIENTATION = "out") # Environment In-Efficiency
round(mean(ecef8$eff),3)
round(mean(enef8$eff),3)
#
# Spearman Rank Correlation Test
# ECEF
cor.test(ecef$eff, ecef1$eff, method = "spearman")
cor.test(ecef$eff, ecef2$eff, method = "spearman")
cor.test(ecef$eff, ecef3$eff, method = "spearman")
cor.test(ecef$eff, ecef4$eff, method = "spearman")
cor.test(ecef$eff, ecef5$eff, method = "spearman")
cor.test(ecef$eff, ecef6$eff, method = "spearman")
cor.test(ecef$eff, ecef7$eff, method = "spearman")
cor.test(ecef$eff, ecef8$eff, method = "spearman")
# ENEF
cor.test(enef$eff, enef1$eff, method = "spearman")
cor.test(enef$eff, enef2$eff, method = "spearman")
cor.test(enef$eff, enef3$eff, method = "spearman")
cor.test(enef$eff, enef4$eff, method = "spearman")
cor.test(enef$eff, enef5$eff, method = "spearman")
cor.test(enef$eff, enef6$eff, method = "spearman")
cor.test(enef$eff, enef7$eff, method = "spearman")
cor.test(enef$eff, enef8$eff, method = "spearman")
#
# Mann-Whitney U Test
# ECEF
wilcox.test(ecef$eff, ecef1$eff,paired = F)
wilcox.test(ecef$eff, ecef2$eff,paired = F)
wilcox.test(ecef$eff, ecef3$eff,paired = F)
wilcox.test(ecef$eff, ecef4$eff,paired = F)
wilcox.test(ecef$eff, ecef5$eff,paired = F)
wilcox.test(ecef$eff, ecef6$eff,paired = F)
wilcox.test(ecef$eff, ecef7$eff,paired = F)
wilcox.test(ecef$eff, ecef8$eff,paired = F)
# ENEF
wilcox.test(enef$eff, enef1$eff,paired = F, exact = F)
wilcox.test(enef$eff, enef2$eff,paired = F, exact = F)
wilcox.test(enef$eff, enef3$eff,paired = F, exact = F)
wilcox.test(enef$eff, enef4$eff,paired = F, exact = F)
wilcox.test(enef$eff, enef5$eff,paired = F, exact = F)
wilcox.test(enef$eff, enef6$eff,paired = F, exact = F)
wilcox.test(enef$eff, enef7$eff,paired = F, exact = F)
wilcox.test(enef$eff, enef8$eff,paired = F, exact = F)
#
# Bootstap DEA
#
# Install required package
install.packages("rDEA")
library(rDEA)
# Bootstrap DEA with 5000 replications
#
set.seed(123)
ecef   <- sdea(X,Y1, RTS="crs",ORIENTATION = "out")
ecef_b <- dea.boot(X,Y1,NREP=5000, ORIENTATION=2,RTS = "crs")
ecef   <- as.matrix(cbind(name,format(round(ecef$eff, digits=3),nsmall=3),
                        format(round(ecef_b$eff   , digits=3),nsmall=3),
                        format(round(ecef_b$eff.bc, digits=3),nsmall=3)
                        ))
ecef <- data.frame(ecef)
colnames(ecef)  <- c("Country","Economical Super Eff.","Economical Efficiency","Economical Bootstrap Eff.")
#ecef <- ecef[order(ecef[,2]),]
ecef

#
set.seed(123)
enef   <- sdea(X,Y2, RTS="crs",ORIENTATION = "out")
enef_b <- dea.boot(X,Y2,NREP=5000, ORIENTATION=2,RTS = "crs")
enef   <- as.matrix(cbind(name,format(round(enef$eff, digits=3),nsmall=3),
                          format(round(enef_b$eff   , digits=3),nsmall=3),
                          format(round(enef_b$eff.bc, digits=3),nsmall=3)
))
enef <- data.frame(enef)
colnames(enef)  <- c("Country","Economical Super Eff.","Economical Efficiency","Economical Bootstrap Eff.")
#enef <- enef[order(enef[,2]),]
enef

# End #





























