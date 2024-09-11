##Soundfinder
## Nov 2023: rgdal is now depricated
#For plotting results of initial trials at Beaver Pond

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
filename=list.files("data/xcorr_results", full.names=T)
treatment=c("alarm", "control")
load(filename[1])
result_alarm=result.df

load(filename[2])
result_control=result.df

results_list=list(result_alarm, result_control)

#result.df

mic.positions.dat=read.csv("micarray_trimblewaypoints_2021.csv")
mic.pos.use=mic.positions.dat[which(mic.positions.dat$SITE=="beaver pond"),]

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

loc.results_list=list()
for (i in 1:length(results_list)){
  result.df=results_list[[i]]
sound.type=sapply(result.df, function(x) x$call_type[1]) %>% str_replace("oriole song?", "oriole")

sound.results=as.data.frame(t(sapply(result.df, function(x) x$peak.time)))
names(sound.results)=c("t1", "t2", "t3", "t4", "t5")
temps=rep(21.5, nrow(sound.results))

loc.result=localize(mics=coords.xy, sounds=sound.results, temps=temps)
loc.result$sound.type=sound.type
loc.result$low.peak.score=sapply(result.df, function(x) min(x$peak.score, na.rm=T))

loc.results_list[[i]]=loc.result
}
##save data frame in folder
#write.csv(loc.result, paste(use.folder, "/localization_results_v2_20221021.csv", sep=""))

######
# plot(loc.result$north, loc.result$east, pch=21, bg=gray(loc.result$err.metres/max(loc.result$err.metres)))
# 
# plot(err.metres~sound.type, data=loc.result %>% filter(err.metres<500))

loc.results.trim=list()
for(i in 1:length(loc.results_list)){
loc.results.trim[[i]]=loc.results_list[[i]] %>%
  filter(north<max(coords.xy$north) & east<max(coords.xy$east) & north>min(coords.xy$north) & east>min(coords.xy$east)) 
}

sapply(loc.results.trim, function(x) unique(x$sound.type))

#sound.type.color=data.frame(type=unique(factor(loc.result.trim$sound.type)), color=brewer.pal(length(unique(factor(loc.result.trim$sound.type))), "Set1"))

color.code=data.frame(type=c("cheer", "cheer var", "check", "distress", "chonk", "chit","tsew",  "oakalee", "dickcissel", "oriole?", "yellowthroat"), color=c("#E41A1C","#E41A1C", "#4DAF4A", "#4DAF4A", "#4DAF4A", "#4DAF4A","#fec44f", "#5e3c99", "#2b83ba", "#2b83ba", "#2b83ba"))

par(mfrow=c(1,2))
for(i in 1:length(loc.results.trim)){
  loc.result.trim=loc.results.trim[[i]]
plot(loc.result.trim$east, loc.result.trim$north, pch=21, xlim=c(min(loc.result.trim$east-20), max(loc.result.trim$east+20)), ylim=c(min(loc.result.trim$north-20), max(loc.result.trim$north+20)), bg=color.code[match(loc.result.trim$sound.type, color.code$type), "color"], las=1, xlab="Easting", ylab="Northing", main=treatment[i])
#points(coords.xy, pch="x", col="black", cex=2)
text(coords.xy$east, coords.xy$north,pch="x", col="black", cex=2, labels=rownames(coords.xy))
legend("bottomleft", legend=color.code$type, pch=21, pt.bg=color.code$color, bty="n")
}

##ggplot
# 

for(i in 1:length(loc.results.trim)){
  loc.results.trim[[i]]$treatment=treatment[i]
}
loc.results.all=bind_rows(loc.results.trim) %>% mutate(sound.type=factor(sound.type, levels=c("cheer", "cheer var", "check", "chonk", "chit", "distress", "tsew", "oakalee", "dickcissel", "oriole?", "yellowthroat")))

colors1=color.code[match(color.code[,1],sort(unique(loc.results.all$sound.type))),2]

ggplot(loc.results.all, aes(x=east, y=north, color=sound.type))+
   geom_point() + scale_color_manual(values=colors1) +
  facet_wrap(~treatment) 



png(filename="control_all.png")
#plot layers
xlims=c(min(loc.result.trim$east-20), max(loc.result.trim$east+20))
ylims=c(min(loc.result.trim$north-20), max(loc.result.trim$north+20))
plot(1, type="n", xlim=xlims, ylim=ylims, las=1, xlab="Easting", ylab="Northing", , xaxt="n", yaxt="n")
text(coords.xy$east, coords.xy$north,pch="x", col="black", cex=2, labels=rownames(coords.xy))
legend("bottomleft", legend=c("'Cheer' alarm", "Other RW alarm", "Dickcissel alarm", "RW song", "Other spp song"), pch=21, pt.bg=c("#E41A1C", "#4DAF4A", "#FFFF33", "#5e3c99", "#2b83ba"))

#songs
songs.result=loc.result.trim %>% filter(sound.type=="oakalee"|sound.type=="dickcissel"|sound.type=="yellowthroat"|sound.type=="oriole?")
points(songs.result$east, songs.result$north, pch=21, xlim=xlims, ylim=ylims, bg=color.code[match(songs.result$sound.type, color.code$type), "color"], cex=1.2)

#cheer only
cheer.result=loc.result.trim %>% filter(sound.type=="cheer"|sound.type=="cheer var")
points(cheer.result$east, cheer.result$north, pch=21, xlim=xlims, ylim=ylims, bg=color.code[match(cheer.result$sound.type, color.code$type), "color"])

#other RWBL
other_rwbl.result=loc.result.trim %>% filter(sound.type=="check"|sound.type=="distress"|sound.type=="chonk")
points(other_rwbl.result$east, other_rwbl.result$north, pch=21, xlim=xlims, ylim=ylims, bg=color.code[match(other_rwbl.result$sound.type, color.code$type), "color"])
#points(coords.xy, pch="x", col="black", cex=2)

#tsew
tsew.result=loc.result.trim %>% filter(sound.type=="tsew")
points(tsew.result$east, tsew.result$north, pch=21, xlim=xlims, ylim=ylims, bg=color.code[match(tsew.result$sound.type, color.code$type), "color"])
#points(coords.xy, pch="x", col="black", cex=2)

dev.off()

# 
# png("plot_alarm_bp_2010630.png", width=8, height=8, units="in", res=150)
# plot(loc.result.trim$east, loc.result.trim$north, pch=21, xlim=c(min(loc.result.trim$east-20), max(loc.result.trim$east+20)), ylim=c(min(loc.result.trim$north-20), max(loc.result.trim$north+20)), bg=adjustcolor(sound.type.color[match(loc.result.trim$sound.type, sound.type.color$type), "color"], alpha.f=1), las=1, xlab="Easting", ylab="Northing", main=use.folder)
# #points(coords.xy, pch="x", col="black", cex=2)
# text(coords.xy$east, coords.xy$north,pch="x", col="black", cex=2, labels=rownames(coords.xy))
# legend("bottomleft", legend=sound.type.color$type, pch=21, pt.bg=adjustcolor(sound.type.color$color, alpha.f=1))
# dev.off()


