##Soundfinder
## Nov 2023: rgdal is now depricated

#install.packages("SoundFinder_1.0.tar.gz", repos = NULL, type ='source')
library(SoundFinder)
library(tidyverse)
library(stringr)
library(geodist)
library(sf)
library(sp)
library(RColorBrewer)
library(ggplot2)

options(digits=10)
folders=list.files(pattern="MC")
use.folder=folders[7]
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
loc.result
loc.result$sound.type=sound.type
loc.result$low.peak.score=sapply(result.df, function(x) min(x$peak.score, na.rm=T))


##save data frame in folder
#Wwrite.csv(loc.result, paste(use.folder, "/localization_results_v2_20221021.csv", sep=""))

######
plot(loc.result$north, loc.result$east, pch=21, bg=gray(loc.result$err.metres/max(loc.result$err.metres)))

plot(err.metres~sound.type, data=loc.result %>% filter(err.metres<500))

rare.voc=names(which(table(loc.result$sound.type)<5))
loc.result.trim=loc.result %>%
  filter(!sound.type%in%rare.voc) %>%
  filter(north<max(coords.xy$north) & east<max(coords.xy$east) & north>min(coords.xy$north) & east>min(coords.xy$east)) 

# loc.result.trim=loc.result %>% 
#   filter(sound.type!=rare.voc) 

sound.type.color=data.frame(type=unique(factor(loc.result.trim$sound.type)), color=brewer.pal(length(unique(factor(loc.result.trim$sound.type))), "Set1"))


plot(loc.result.trim$east, loc.result.trim$north, pch=21, xlim=c(min(loc.result.trim$east-20), max(loc.result.trim$east+20)), ylim=c(min(loc.result.trim$north-20), max(loc.result.trim$north+20)), bg=sound.type.color[match(loc.result.trim$sound.type, sound.type.color$type), "color"], las=1, xlab="Easting", ylab="Northing", main=use.folder)
#points(coords.xy, pch="x", col="black", cex=2)
text(coords.xy$east, coords.xy$north,pch="x", col="black", cex=2, labels=rownames(coords.xy))
legend("bottomleft", legend=sound.type.color$type, pch=21, pt.bg=sound.type.color$color)

png("plot_alarm_bp_2010630.png", width=8, height=8, units="in", res=150)
plot(loc.result.trim$east, loc.result.trim$north, pch=21, xlim=c(min(loc.result.trim$east-20), max(loc.result.trim$east+20)), ylim=c(min(loc.result.trim$north-20), max(loc.result.trim$north+20)), bg=adjustcolor(sound.type.color[match(loc.result.trim$sound.type, sound.type.color$type), "color"], alpha.f=1), las=1, xlab="Easting", ylab="Northing", main=use.folder)
#points(coords.xy, pch="x", col="black", cex=2)
text(coords.xy$east, coords.xy$north,pch="x", col="black", cex=2, labels=rownames(coords.xy))
legend("bottomleft", legend=sound.type.color$type, pch=21, pt.bg=adjustcolor(sound.type.color$color, alpha.f=1))
dev.off()


