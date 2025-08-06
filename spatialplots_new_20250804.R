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
library(gganimate)
library(gifski)
library(ggdensity)
library(cowplot)
library(colors3d)
options(digits=10)
filename=list.files("data/xcorr_results", full.names=T)
load(filename[3])
result_control1=result.df

load(filename[4])
result_control2=result.df

results_list=list(result_control1, result_control2)


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


#hist(sapply(result.df, function(x) min(x$peak.score)), xlim=c(0,1))

#drop channel with lowest peak score
# for(i in 1:length(result.df)){
#   result.df[[i]]$peak.time[which(result.df[[i]]$peak.score==min(result.df[[i]]$peak.score))]=NA
#   result.df[[i]]$peak.score[which(result.df[[i]]$peak.score==min(result.df[[i]]$peak.score))]=NA
# }
# hist(sapply(result.df, function(x) min(x$peak.score, na.rm=T)), xlim=c(0,1))

## remove the first row because in some records this is a negative number for some reason...
loc.results_list=list()
for (i in 1:length(results_list)){
  result.df=results_list[[i]]
  sound.type=sapply(result.df, function(x) x$call_type[1])[-1]
  sound.name=sapply(result.df, function(x) x$call_name[1])[-1]
  species=sapply(result.df, function(x) x$spp[1])[-1]
  sound.results=as.data.frame(t(sapply(result.df, function(x) x$peak.time))) %>% tibble() %>% rename(t1=V1, t2=V2, t3=V3, t4=V4, t5=V5) %>% filter(t1>0)
  sound.results$temp=rep(21.5, nrow(sound.results))
  
  loc.result=localize(mics=coords.xy, sounds=sound.results)
  loc.result$sound.type=sound.type
  loc.result$sound.name=sound.name
  loc.result$species=species
  loc.result$low.peak.score=sapply(result.df, function(x) min(x$peak.score, na.rm=T))[-1]
  
  loc.results_list[[i]]=loc.result
}
##save data frame in folder
write.csv(loc.results_list[[1]],  "data/localization_results/control1_20250804.csv")
write.csv(loc.results_list[[2]], "data/localization_results/control2_20250804.csv")

unique(loc.results_list[[1]]$sound.name)
unique(loc.results_list[[2]]$sound.name)

unique(loc.results_list[[2]]$species)


loc.results.trim=list()
for(i in 1:length(loc.results_list)){
  loc.results.trim[[i]]=loc.results_list[[i]] %>%
    filter(north<max(coords.xy$north) & east<max(coords.xy$east) & north>min(coords.xy$north) & east>min(coords.xy$east)) 
}

loc.results.trim[[2]] %>% filter(species=="RWBL") %>% filter(sound.name!="") %>% group_by(sound.type) %>% summarize(mean.err=mean(err.metres), median_err=median(err.metres), n_calls=n())



color.code=data.frame(type=c("cheer",  "check", "chit", "oakalee", "song"), color=c("#E41A1C", "#e34a33","#fec44f", "#5e3c99","#2b83ba"))


par(mfrow=c(1,2))
for(i in 1:length(loc.results.trim)){
  loc.result.trim=loc.results.trim[[i]]
  plot(loc.result.trim$east, loc.result.trim$north, pch=21, xlim=c(min(loc.result.trim$east-20), max(loc.result.trim$east+20)), ylim=c(min(loc.result.trim$north-20), max(loc.result.trim$north+20)), bg=color.code[match(loc.result.trim$sound.name, color.code$type), "color"], las=1, xlab="Easting", ylab="Northing")
  #points(coords.xy, pch="x", col="black", cex=2)
  text(coords.xy$east, coords.xy$north,pch="x", col="black", cex=2, labels=rownames(coords.xy))
  legend("bottomleft", legend=color.code$type, pch=21, pt.bg=color.code$color, bty="n")
}

treatment=c("control 1", "control 2")
for(i in 1:length(loc.results.trim)){
  loc.results.trim[[i]]$treatment=treatment[i]
}

plot_data=bind_rows(loc.results.trim) %>% left_join(., color.code, by=join_by("sound.name"=="type")) %>% select(north, east, time, treatment, sound.type, sound.name, species, color)
plot_data=plot_data %>% mutate(sec=floor(time)) 

xlims=c(min(loc.result.trim$east-5), max(loc.result.trim$east+5))
ylims=c(min(loc.result.trim$north-5), max(loc.result.trim$north+5))

ggplot(plot_data, aes(x=east, y=north, fill=sound.type))+
  geom_point(pch=21, size=3) +
  scale_fill_viridis_d() +
  facet_wrap(~treatment) +
  xlim(xlims) +
  ylim(ylims) +
  theme_bw() +
  theme(panel.background = element_rect(fill="transparent"),
        plot.background=element_rect(fill="transparent"),
        legend.background=element_rect(fill="transparent"),
        legend.box.background=element_rect(fill="transparent"))+
  annotate("text", x=coords.xy$east, y=coords.xy$north, label="X")


ggplot(plot_data %>% filter(sound.type=="song") %>% filter(treatment=="control 2") %>% filter(species=="DICK") , aes(x=east, y=north))+
  geom_hdr(probs=c(0.99, 0.95, 0.90, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2), aes(fill = after_stat(probs)), alpha=1, show.legend=T) +
  geom_point()

###
d <- expand_grid(x = floor(min(coords.xy$east)):ceiling(max(coords.xy$east)), y =floor(min(coords.xy$north)):ceiling(max(coords.xy$north)))
d$colors2d <- colors2d(d[, 1:2])



ggplot(d, aes(x, y, fill = colors2d)) +
  geom_raster() +
  scale_fill_identity() + 
  theme_classic() +
  annotate("text", x=coords.xy$east, y=coords.xy$north, label=rep("x", 5), cex=8)

i=1
loc.result_2calls=loc.results.trim[[i]] %>% mutate(east_round=round(east), north_round=round(north)) %>% left_join(., d, by=join_by(east_round==x, north_round==y)) %>% 
  mutate(sound.name=str_replace_all(sound.name, "chit\\?", "chit")) %>%
  mutate(sound.name=str_replace_all(sound.name, "cheer\\?", "cheer")) %>%
  filter(sound.name!="")

types=unique(loc.result_2calls$sound.name)

#png(filename="TimelinePlot_control1.png", width=8, height=6, units="in", res=300)
par(mar=c(4, 7, 2,2))
plot(seq(0, max(loc.result_2calls$time), length=4), 1:4, type="n", xlab="Time", ylab="", yaxt="n", main=treatment[i])
axis(2, at=1:length(types), labels=types, las=1)
for(j in 1:nrow(loc.result_2calls)){
  points(loc.result_2calls$time[j], match(loc.result_2calls$sound.name[j], types), pch="|", cex=2, col=loc.result_2calls$colors2d[j])
}
#dev.off()

## or plot both on one line
i=1
loc.result_2calls=loc.results.trim[[i]] %>% mutate(east_round=round(east), north_round=round(north)) %>% left_join(., d, by=join_by(east_round==x, north_round==y)) %>% filter(sound.type=="song"&species=="RWBL")


types=unique(loc.result_2calls$sound.type)

pdf(file="TimelinePlot_Control1_song.pdf", width=12, height=2)
plot(seq(0, max(loc.result_2calls$time), length=2), 1:2, type="n", xlab="Time", ylab="", yaxt="n", main=treatment[i], ylim=c(0.9,1.1))
for(j in 1:nrow(loc.result_2calls)){
  points(loc.result_2calls$time[j],1, pch="|", cex=2, col=loc.result_2calls$colors2d[j])
}
dev.off()

###
