##Soundfinder

#install.packages("SoundFinder_1.0.tar.gz", repos = NULL, type ='source')
library(SoundFinder)
library(tidyverse)
library(stringr)
library(geodist)
library(rgdal)
library(RColorBrewer)
library(ggplot2)

folders=list.files(pattern="MC")
use.folder=folders[1]
use.colony=str_sub(use.folder,str_locate(use.folder, "_")[1]+1,nchar(use.folder))

load(paste(use.folder, "xcorr.results_99_bpfilter.rdata", sep="/"))

#result.df

#drop channel with lowest peak score
for(i in 1:length(result.df)){
  result.df[[i]]$peak.time[which(result.df[[i]]$peak.score==min(result.df[[i]]$peak.score))]=NA
}

mic.positions.dat=read.csv("micarray_trimblewaypoints_2021.csv")
mic.pos.use=mic.positions.dat[which(mic.positions.dat$SITE==use.colony),]

#dist.matrix=geodist(mic.pos.use[,c("long","lat")])
coords=SpatialPoints(cbind(mic.pos.use$long, mic.pos.use$lat), proj4string = CRS("+proj=longlat"))

coords.utm=spTransform(coords, CRS("+proj=utm + zone=14 +datum=WGS84"))
coords.utm.matrix=coords.utm@coords

coords.xy=as.data.frame(coords.utm.matrix)
coords.xy[,1]=coords.xy[,1]-min(coords.xy[,1])
coords.xy[,2]=coords.xy[,2]-min(coords.xy[,2])

names(coords.xy)=c("east", "north")

sound.type=sapply(result.df, function(x) x$call_type[1])

sound.results=as.data.frame(t(sapply(result.df, function(x) x$peak.time)))
names(sound.results)=c("t1", "t2", "t3", "t4", "t5")
temps=rep(21.5, nrow(sound.results))

loc.result=localize(mics=coords.xy, sounds=sound.results, temps=temps)
loc.result
loc.result$sound.type=sound.type

plot(err.metres~sound.type, data=loc.result, ylim=c(0,200))

minimum_score=sapply(result.df, function(x) min(x$peak.score))
boxplot(minimum_score~loc.result$sound.type)

result.df[which(loc.result$sound.type=="dickcissel")]

sound.type.color=data.frame(type=unique(loc.result$sound.type), color=brewer.pal(7, "Set1"))

loc.result.trim=loc.result[which(loc.result$err.metres<100),]

plot(loc.result.trim$east, loc.result.trim$north, xlim=c(-100, 200), ylim=c(-100,200), pch=21, bg=sound.type.color[match(loc.result.trim$sound.type, sound.type.color$type), "color"], las=1, xlab="Easting", ylab="Northing", main="Bambi, June 15, 7:10-7:15am")
points(coords.xy, pch="x", col="black", cex=2)
legend("bottomright", legend=sound.type.color$type, pch=21, pt.bg=sound.type.color$color)

#plot(loc.result$east, loc.result$north)
