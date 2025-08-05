##Soundfinder

#install.packages("SoundFinder_1.0.tar.gz", repos = NULL, type ='source')
library(SoundFinder)
library(tidyverse)
library(stringr)
library(geodist)
#library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(colors3d)
library(cowplot)

options(digits=10)
filename=list.files("data/xcorr_results", full.names=T)
treatment=c("alarm", "control")
load(filename[1])
result_alarm=result.df

load(filename[2])
result_control=result.df

results_list=list(result_alarm, result_control)
#result.df



mic.positions.dat=read.csv("data/micarray_trimblewaypoints_2021.csv")
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

d <- expand_grid(x = floor(min(coords.xy$east)):ceiling(max(coords.xy$east)), y =floor(min(coords.xy$north)):ceiling(max(coords.xy$north)))
d$colors2d <- colors2d(d[, 1:2])

ggplot(d, aes(x, y, fill = colors2d)) +
  geom_raster() +
  scale_fill_identity() + 
  theme_classic() +
  annotate("text", x=coords.xy$east, y=coords.xy$north, label=rep("x", 5), cex=8)

ggsave("colorgradient.pdf")

loc.results_list=list()
for (i in 1:length(results_list)){
  result.df=results_list[[i]]
  sound.type=sapply(result.df, function(x) x$call_type[1]) %>% str_replace("oriole song\\?", "oriole") %>% str_replace("cheer var", "cheer")
  
  sound.results=as.data.frame(t(sapply(result.df, function(x) x$peak.time)))
  names(sound.results)=c("t1", "t2", "t3", "t4", "t5")
  temps=rep(21.5, nrow(sound.results))
  
  loc.result=localize(mics=coords.xy, sounds=sound.results, temps=temps)
  loc.result$sound.type=sound.type
  loc.result$low.peak.score=sapply(result.df, function(x) min(x$peak.score, na.rm=T))
  
  loc.results_list[[i]]=loc.result
}

loc.results.trim=list()
for(i in 1:length(loc.results_list)){
  loc.results.trim[[i]]=loc.results_list[[i]] %>%
    filter(north<max(coords.xy$north) & east<max(coords.xy$east) & north>min(coords.xy$north) & east>min(coords.xy$east)) %>%
    mutate(east_round=round(east), north_round=round(north)) %>%
    left_join(., d, by=join_by(east_round==x, north_round==y))
}

loc.results.trim[[1]]

loc.results.trim[[1]] %>% group_by(sound.type) %>% summarize(mean.err=mean(err.metres), median_err=median(err.metres), n_calls=n())

i=1

timeline_dat=loc.results.trim[[i]] %>% filter(sound.type!="oakalee" & sound.type!="yellowthroat")

types=unique(timeline_dat$sound.type)
#png(filename=paste("Timeline", treatment[i], "png",sep="."), width=8, height=6, units="in", res=300)
par(mar=c(4, 7, 2,2))
plot(seq(0, max(timeline_dat$time), length=length(types)), 1:length(types), type="n", xlab="Time", ylab="", yaxt="n", main=treatment[i])
axis(2, at=1:length(types), labels=types, las=1)
for(j in 1:nrow(timeline_dat)){
  points(timeline_dat$time[j], match(timeline_dat$sound.type[j], types), pch="|", col=timeline_dat$colors2d[j])
}
#dev.off()

## only cheers, chonks, checks, and distress calls
sound.types=sapply(loc.results.trim, function(x) unique(x$sound.type))

i=1
loc.result_2calls=loc.results.trim[[i]] %>% filter(sound.type=="check"|sound.type=="cheer"|sound.type=="chonk"|sound.type=="distress")

types=unique(loc.result_2calls$sound.type)

pdf(file=paste("Timeline", treatment[i], "check_cheer", "pdf", sep="."), width=8, height=3.5)
plot(seq(0, max(loc.result_2calls$time), length=4), 1:4, type="n", xlab="Time", ylab="", yaxt="n", main=treatment[i], ylim=c(0.5,4.5))
axis(2, at=1:length(types), labels=types, las=1)
for(j in 1:nrow(loc.result_2calls)){
  points(loc.result_2calls$time[j], match(loc.result_2calls$sound.type[j], types), pch="|", cex=2, col=loc.result_2calls$colors2d[j])
}
dev.off()

## or plot both on one line
i=1
loc.result_2calls=loc.results.trim[[i]] %>% filter(sound.type=="check"|sound.type=="cheer"|sound.type=="chonk"|sound.type=="distress")

types=unique(loc.result_2calls$sound.type)

pdf(file=paste("Timeline", treatment[i], "check_cheer_oneline", "pdf",sep="."), width=10, height=2)
par(mar=c(4, 7, 2,2))
plot(seq(0, max(loc.result_2calls$time), length=2), 1:2, type="n", xlab="Time", ylab="", yaxt="n", main=treatment[i], ylim=c(0.9,1.1))
for(j in 1:nrow(loc.result_2calls)){
  points(loc.result_2calls$time[j],1, pch="|", cex=2, col=loc.result_2calls$colors2d[j])
}
dev.off()

##just plot the songs

i=2
loc.result_2calls=loc.results.trim[[i]] %>% filter(sound.type=="oakalee")

types=unique(loc.result_2calls$sound.type)

pdf(file=paste("Timeline", treatment[i], "oakalee", "pdf", sep="."), width=8, height=3.5)
plot(seq(0, max(loc.result_2calls$time), length=2), 1:2, type="n", xlab="Time", ylab="", yaxt="n", main=treatment[i], ylim=c(0.5,4.5))
axis(2, at=1:length(types), labels=types, las=1)
for(j in 1:nrow(loc.result_2calls)){
  points(loc.result_2calls$time[j], match(loc.result_2calls$sound.type[j], types), pch="|", cex=2, col=loc.result_2calls$colors2d[j])
}
dev.off()


## plot in space, with raster as background
for(i in 1:length(loc.results.trim)){
  loc.results.trim[[i]]$treatment=treatment[i]
}
plot_data=bind_rows(loc.results.trim) %>% left_join(., color.code, by=join_by("sound.type"=="type")) %>%
  mutate(categories=factor(categories, level=c("'cheer' call", "RW other alarm", "Other spp alarm", "RW song", "Other spp song", "dummy"))) %>% select(north, east, time, sound.type, treatment, categories, color, color2)
plot_data=plot_data %>% mutate(sec=floor(time)) %>% select(-time)

#set color palette
colors2=color.code[match(sort(unique(plot_data$categories)), color.code[,2]),4]
xlims=c(min(loc.result.trim$east-5), max(loc.result.trim$east+5))
ylims=c(min(loc.result.trim$north-5), max(loc.result.trim$north+5))

ggplot(loc.results.trim[[i]], aes(x=east, y=north))+
  geom_point(pch=21, size=3) +
  xlim(xlims) +
  ylim(ylims) +
  theme_bw() +
  theme(panel.background = element_rect(fill="transparent"),
        plot.background=element_rect(fill="transparent"),
        legend.background=element_rect(fill="transparent"),
        legend.box.background=element_rect(fill="transparent"))+
  annotate("text", x=coords.xy$east, y=coords.xy$north, label="X") +
  geom_raster(data=d, aes(x, y, fill = colors2d)) +
  scale_fill_identity()

i=1
ggplot(data=d, aes(x, y, fill = adjustcolor(colors2d, alpha.f=0.8))) +
  geom_raster() +
  scale_fill_identity() +
  geom_point(data=loc.results.trim[[i]], aes(x=east, y=north), pch=21, cex=3) +
  theme_cowplot()

ggsave("alarm_colorgradient.pdf")
