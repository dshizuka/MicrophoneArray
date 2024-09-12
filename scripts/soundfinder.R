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
use.folder=folders[2]
use.colony=str_sub(use.folder,str_locate(use.folder, "_")[1]+1,nchar(use.folder))

load(paste(use.folder, "xcorr.results_99_bpfilter.rdata", sep="/"))

#result.df

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

plot(err.metres~sound.type, data=loc.result)

rare.voc=names(which(table(loc.result$sound.type)<5))
loc.result.trim=loc.result %>%
  filter(!sound.type%in%rare.voc) %>%
  filter(err.metres<100)

# loc.result.trim=loc.result %>% 
#   filter(sound.type!=rare.voc) 

sound.type.color=data.frame(type=unique(factor(loc.result.trim$sound.type)), color=brewer.pal(length(unique(factor(loc.result.trim$sound.type))), "Set1"))


plot(loc.result.trim$east, loc.result.trim$north, xlim=c(-50, 150), ylim=c(-50,150), pch=21, bg=sound.type.color[match(loc.result.trim$sound.type, sound.type.color$type), "color"], las=1, xlab="Easting", ylab="Northing", main="Poison Ivy Creek, June 17, 9:25am")
points(coords.xy, pch="x", col="black", cex=2)
legend("bottomleft", legend=sound.type.color$type, pch=21, pt.bg=sound.type.color$color)

#plot(loc.result$east, loc.result$north)
