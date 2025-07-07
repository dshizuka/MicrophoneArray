##Soundfinder

#install.packages("SoundFinder_1.0.tar.gz", repos = NULL, type ='source')
library(SoundFinder)
library(tidyverse)
library(stringr)
library(geodist)
library(rgdal)
library(RColorBrewer)
library(ggplot2)

options(digits=10)
folders=list.files(pattern="MC")
use.folder=folders[5]
use.colony=str_sub(use.folder,str_locate_all(use.folder, "_")[[1]][2,1]+1,nchar(use.folder))

load(paste(use.folder, "xcorr.results_99_bpfilter.rdata", sep="/"))

#result.df



mic.positions.dat=read.csv("micarray_trimblewaypoints_2021.csv")
mic.pos.use=mic.positions.dat[which(mic.positions.dat$SITE==use.colony),]

#dist.matrix=geodist(mic.pos.use[,c("long","lat")])
coords=SpatialPoints(cbind(mic.pos.use$long, mic.pos.use$lat), proj4string = CRS("+proj=longlat"))
coords.utm=spTransform(coords, CRS("+proj=utm + zone=14 +datum=WGS84"))
coords.utm.matrix=coords.utm@coords

coords.xy=as.data.frame(coords.utm.matrix)
#coords.xy[,1]=coords.xy[,1]-min(coords.xy[,1])
#coords.xy[,2]=coords.xy[,2]-min(coords.xy[,2])

names(coords.xy)=c("east", "north")
coords.xy


#hist(sapply(result.df, function(x) min(x$peak.score)), xlim=c(0,1))

#drop channel with lowest peak score
# for(i in 1:length(result.df)){
#   result.df[[i]]$peak.time[which(result.df[[i]]$peak.score==min(result.df[[i]]$peak.score))]=NA
#   result.df[[i]]$peak.score[which(result.df[[i]]$peak.score==min(result.df[[i]]$peak.score))]=NA
# }
# hist(sapply(result.df, function(x) min(x$peak.score, na.rm=T)), xlim=c(0,1))

sound.type=sapply(result.df, function(x) x$call_type[1])

sound.results=as.data.frame(t(sapply(result.df, function(x) x$peak.time)))
names(sound.results)=c("t1", "t2", "t3", "t4", "t5")
temps=rep(21.5, nrow(sound.results))

loc.result=localize(mics=coords.xy, sounds=sound.results, temps=temps)
loc.result$sound.type=sound.type
loc.result$low.peak.score=sapply(result.df, function(x) min(x$peak.score, na.rm=T))
head(loc.result)

types=unique(sound.type)
png(paste("Timeline", use.folder, "png",sep="."), width=8, height=6, units="in", res=300)
par(mar=c(4, 7, 2,2))
plot(seq(0, max(loc.result$time), length=length(types)), 1:length(types), type="n", xlab="Time", ylab="", yaxt="n", main=paste(use.folder))
axis(2, at=1:length(types), labels=types, las=1)
for(i in 1:nrow(loc.result)){
  points(loc.result$time[i], match(loc.result$sound.type[i], types), pch="|")
}
dev.off()
