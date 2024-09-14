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

color.code=data.frame(type=c("cheer", "cheer var", "check", "distress", "chonk", "chit","tsew",  "oakalee", "dickcissel", "oriole?", "yellowthroat"), categories=c("'cheer' call", "'cheer' call", "RW other alarm", "RW other alarm", "RW other alarm", "RW other alarm","Other spp alarm",  "RW song", "Other spp song", "Other spp song", "Other spp song"), color=c("#E41A1C","#E41A1C", "#4DAF4A", "#4DAF4A", "#4DAF4A", "#4DAF4A","#fec44f", "#5e3c99", "#2b83ba", "#2b83ba", "#2b83ba"), color2=c("#b30000", "#b30000", "#e34a33", "#fc8d59", "#fc8d59", "#fc8d59", "#fdcc8a", "#253494","#41b6c4", "#41b6c4", "#41b6c4"))



par(mfrow=c(1,2))
for(i in 1:length(loc.results.trim)){
  loc.result.trim=loc.results.trim[[i]]
plot(loc.result.trim$east, loc.result.trim$north, pch=21, xlim=c(min(loc.result.trim$east-20), max(loc.result.trim$east+20)), ylim=c(min(loc.result.trim$north-20), max(loc.result.trim$north+20)), bg=color.code[match(loc.result.trim$sound.type, color.code$type), "color"], las=1, xlab="Easting", ylab="Northing", main=treatment[i])
#points(coords.xy, pch="x", col="black", cex=2)
text(coords.xy$east, coords.xy$north,pch="x", col="black", cex=2, labels=rownames(coords.xy))
legend("bottomleft", legend=color.code$categories, pch=21, pt.bg=color.code$color, bty="n")
}

##ggplot
# 
#prepare and make a global data frame
for(i in 1:length(loc.results.trim)){
  loc.results.trim[[i]]$treatment=treatment[i]
}
plot_data=bind_rows(loc.results.trim) %>% left_join(., color.code, by=join_by("sound.type"=="type")) %>%
  mutate(categories=factor(categories, level=c("'cheer' call", "RW other alarm", "Other spp alarm", "RW song", "Other spp song"))) %>% select(north, east, time, sound.type, treatment, categories, color, color2)


#set color palette
colors2=color.code[match(sort(unique(plot_data$categories)), color.code[,2]),4]
xlims=c(min(loc.result.trim$east-5), max(loc.result.trim$east+5))
ylims=c(min(loc.result.trim$north-5), max(loc.result.trim$north+5))

ggplot(plot_data, aes(x=east, y=north, fill=categories))+
   geom_point(pch=21, size=3) +
  scale_fill_manual(values=colors2) +
  facet_wrap(~treatment) +
  xlim(xlims) +
  ylim(ylims) +
  theme_bw() +
  theme(panel.background = element_rect(fill="transparent"),
        plot.background=element_rect(fill="transparent"),
        legend.background=element_rect(fill="transparent"),
        legend.box.background=element_rect(fill="transparent"))+
  annotate("text", x=coords.xy$east, y=coords.xy$north, label="X")

ggsave("plots/microphonearray_plot_bothtreatment_altcolor.png", bg="transparent")

#cheer only
ggplot(plot_data%>%filter(categories=="'cheer' call"), aes(x=east, y=north, fill=categories))+
  geom_point(pch=21, size=3, fill=color.code[which(color.code$categories=="'cheer' call"),4][1]) +
  facet_wrap(~treatment) +
  xlim(xlims) +
  ylim(ylims) +
  theme_bw() +
  theme(panel.background = element_rect(fill="transparent"),
        plot.background=element_rect(fill="transparent"),
        legend.background=element_rect(fill="transparent"),
        legend.box.background=element_rect(fill="transparent"))+
  annotate("text", x=coords.xy$east, y=coords.xy$north, label="X")

ggsave("plots/microphonearray_plot_cheeronly_altcolor.png", bg="transparent", width=10, height=5, units="in")

#songs only
ggplot(plot_data%>%filter(categories=="RW song"|categories=="Other spp song"), aes(x=east, y=north, fill=categories))+
  geom_point(pch=21, size=3) +
  scale_fill_manual(values=c("#253494","#41b6c4"), guide="none") +
  facet_wrap(~treatment) +
  xlim(xlims) +
  ylim(ylims) +
  theme_bw() +
  theme(panel.background = element_rect(fill="transparent"),
        plot.background=element_rect(fill="transparent"),
        legend.background=element_rect(fill="transparent"),
        legend.box.background=element_rect(fill="transparent"))+
  annotate("text", x=coords.xy$east, y=coords.xy$north, label="X")

ggsave("plots/microphonearray_plot_songsonly_altcolor.png", bg="transparent", width=10, height=5, units="in")

#other spp alarms only
ggplot(plot_data%>%filter(categories=="Other spp alarm"), aes(x=east, y=north, fill=categories))+
  geom_point(pch=21, size=3) +
  scale_fill_manual(values="#fdcc8a", guide="none") +
  facet_wrap(~treatment) +
  xlim(xlims) +
  ylim(ylims) +
  theme_bw() +
  theme(panel.background = element_rect(fill="transparent"),
        plot.background=element_rect(fill="transparent"),
        legend.background=element_rect(fill="transparent"),
        legend.box.background=element_rect(fill="transparent"))+
  annotate("text", x=coords.xy$east, y=coords.xy$north, label="X")

ggsave("plots/microphonearray_plot_othersppalarms_altcolor.png", bg="transparent", width=10, height=5, units="in")

#other redwing alarms only
ggplot(plot_data%>%filter(categories=="RW other alarm"), aes(x=east, y=north, fill=categories))+
  geom_point(pch=21, size=3) +
  scale_fill_manual(values="#fc8d59", guide="none") +
  facet_wrap(~treatment) +
  xlim(xlims) +
  ylim(ylims) +
  theme_bw() +
  theme(panel.background = element_rect(fill="transparent"),
        plot.background=element_rect(fill="transparent"),
        legend.background=element_rect(fill="transparent"),
        legend.box.background=element_rect(fill="transparent"))+
  annotate("text", x=coords.xy$east, y=coords.xy$north, label="X")

ggsave("plots/microphonearray_plot_RWotheralarms_altcolor.png", bg="transparent", width=10, height=5, units="in")

### let's add kernel density


ggplot(plot_data %>% filter(time>200&time<300)%>% filter(treatment=="control") , aes(x=east, y=north))+
  geom_point() +
  geom_density_2d_filled(contour_var="count", bins=10) +
  xlim(xlims) +
  ylim(ylims) +
  annotate("text", x=coords.xy$east, y=coords.xy$north, label="X")

### animate kernel density
#set up transition states
range(plot_data$time)
time_breaks=seq(0,300, length=11)
trans_state=vector(length=length(plot_data$time))
for(i in 1:length(time_breaks)){
trans_state[plot_data$time>time_breaks[i]&plot_data$time<time_breaks[i+1]]=i
}
trans_state
plot_data$trans_state=trans_state

#manually produce pngs
for(i in 1:10){
p=ggplot(plot_data  %>% filter(trans_state==i), aes(x=east, y=north))+
  geom_point() +
  geom_density_2d_filled(contour_var="ndensity", bins=10) +
  xlim(xlims) +
  ylim(ylims) +
  theme(legend.position="none")+
  facet_wrap(~treatment)
p
ggsave(paste("density_plot-", i, ".png", sep=""), bg="transparent", width=10, height=5, units="in")
}

q=ggplot(plot_data %>% filter(treatment=="control"), aes(x=east, y=north))+
  geom_point() +
  geom_density_2d_filled(contour_var="ndensity", bins=10) +
  xlim(xlims) +
  ylim(ylims) +
  theme(legend.position="none")+
  ggtitle("control")

anim=q+transition_states(trans_state, transition_length=2)
mygif=animate(anim, nframe=10, fps=2, renderer=gifski_renderer(loop=FALSE))
mygif
#anim_save(filename="control.gif", mygif)


plot_data=plot_data %>% mutate(sec=floor(time)) %>% select(-time)
q2=ggplot(plot_data %>% filter(treatment=="alarm"), aes(x=east, y=north, fill=categories))+
  geom_point(aes(group=sec), pch=21, size=5, alpha=0.5) +
  scale_fill_manual(values=colors2, guide="none") +
  xlim(xlims) +
  ylim(ylims) +
  theme_bw() +
  annotate("text", x=coords.xy$east, y=coords.xy$north, label="X")
q2
anim2=q2+transition_reveal(sec) + labs(title="{frame_along}")
anim3=q2+transition_manual(sec, cumulative=T) + labs(title="{frame_along}")
animate(anim3, fps=2)
#animate(anim2, fps=2, renderer=gifski_renderer(loop=FALSE))
