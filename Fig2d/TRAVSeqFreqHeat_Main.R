

library(gplots)
library("RColorBrewer")


#Functions
rbind.all.columns <- function(x, y) {
  
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))

  x[, c(as.character(y.diff))] <- NA
  
  y[, c(as.character(x.diff))] <- NA

  
  return(rbind(x, y))
}

mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}


setwd("csv_raw")
temp = list.files(pattern="*.csv")
myfiles = lapply(temp[1:11], read.csv)
ComboDF2 <- read.csv("comboDFv2.csv")
ComboDF2.Score <- ComboDF2[,"SumScore"]
for(i in 1:11){
  if(i==1){
    colnames(myfiles[[i]]) <- gsub("[^[:alpha:] ]", "", colnames(myfiles[[i]]))
    comboDF <- myfiles[[i]]
  }
  if(i>1){
    colnames(myfiles[[i]]) <- gsub("[^[:alpha:] ]", "", colnames(myfiles[[i]]))
    #print(i)
    #print(colnames(myfiles[[i]]))
    comboDF <- rbind.all.columns(comboDF, myfiles[[i]])
  }
}

tempSin <- sapply(strsplit(temp[1:11], split='.', fixed=TRUE), function(x) (x[1]))
rownames(comboDF) <- tempSin

tempSeqs <- gsub("[^[:alpha:] ]", "", colnames(comboDF))
table(tempSeqs)

color4HM <- (brewer.pal(9,"Reds"))[2:7]


dat <- t(as.matrix(comboDF))
factor(rownames(dat))


write.csv(dat, file = "comboDF.csv")


heatmap.2(dat, 
          Rowv=FALSE, Colv=FALSE, dendrogram='none', key=TRUE, 
          cellnote=dat, notecol="black", notecex=.5, trace="none", na.color='black',
          margins = c(9,5), lwid = c(.1,.1),col=color4HM,
          density.info="none")

# oldpar <- par("mar")
# par(mar=c(6, 6, 6, 0.5))

heatmap.2((dat), 
          Rowv=FALSE, Colv=FALSE, dendrogram='none', key=F,col=color4HM, 
          cellnote=round(dat,1), notecol="black", notecex=.7, trace='none', na.color='white', 
          margins = c(5,8), lhei = c(.02,3), lwid = c(.01,.8))

brewer.pal(9, "Pastel1")


#col2 <- rep("#F2F2F2", nrow(dat))

ComboDF2.Colors <- replace(ComboDF2.Score, ComboDF2.Score==1, "black")
ComboDF2.Colors <- replace(ComboDF2.Colors, ComboDF2.Colors==2, "white")

rownames(dat)
dev.off()

tiff(file = "HeatMap_highres.tif", bg = "transparent", width = 3000, height = 3600, units = "px", res=400,compression="jpeg")
heatmap.2((dat), 
               Rowv=FALSE, Colv=FALSE, dendrogram='none', key=F,col=color4HM,   
               RowSideColors=ComboDF2.Colors, colsep =c(1,4,7,9), sepwidth=c(0.05,0.05), sepcolor="black",
               cellnote=round(dat,1), notecol="black", notecex=.7, trace='none', na.color='white', 
               margins = c(6,11), lhei = c(.1,3), lwid = c(.1,.8))
dev.off()













