###Cross-correlations

library(seewave)
library(lubridate)
library(warbleR)
library(stringr)
library(tidyverse)
library(magrittr)
options(digits=10)

folders.full=list.files(pattern="MC", full.names=T)
folders.short=list.files(pattern="MC", full.names=F)
use.folder=folders.full[1]
use.folder.short=folders.short[1]

annot=read.table(list.files(use.folder, pattern=".txt", full.names = T), header=T, sep="\t")

names(annot)

seltab=annot %>% mutate(sound.files=list.files(use.folder, pattern=".wav")) %>% select(sound.files, channel=Channel, start=Begin.Time..s., end=End.Time..s., bottom.freq=Low.Freq..Hz., top.freq=High.Freq..Hz., type=Label)

seltab$selec=seq(1:nrow(seltab))
seltab$channel=as.numeric(seltab$channel)

#this creates the "template" sounds and saves them into a folder
dir.create(paste(use.folder.short, "templates", sep="/")) #creates a new folder called "templates" within the focal folder.
cut_sels(seltab, path=use.folder.short, dest.path=paste(use.folder.short, "templates", sep="/"))

#set up cross correlations

#first, we need to make a selection table for each template. This will include the template track and the test track clips that will be compared

temp_names=list.files(paste(use.folder, "templates", sep="/"))
file_order=as.numeric(str_sub(temp_names, start=str_locate(temp_names, "-")[,1]+1, end=str_locate(temp_names, "\\.")[,1]-1))

result.df=list()
for(k in 1:nrow(annot)){
  temp.df=data.frame(sound.files=temp_names[k], channel=1, selec=1, start=0, end=duration_wavs(paste(use.folder, "templates", temp_names[k], sep="/"))$duration)
  
  track.df=data.frame(sound.files=list.files(use.folder, pattern=".wav"), channel=1:5, selec=1:5, start=seltab$start[file_order[k]]-0.5, end=seltab$end[file_order[k]]+0.5)
  
  set=rbind(temp.df, track.df)
  
  #copy template file over from template subfolder to main focal folder 
  file.copy(from=paste(use.folder, "templates", set[1,"sound.files"], sep="/"), to=use.folder)
  
  #run cross-correlation
  xc=list()
  for(j in 1:(nrow(set)-1)){
    xc[[j]]=cross_correlation(set[c(1,1+j),], bp=c(1,10), output="list", ovlp=90, path=use.folder)
  }
  #xc
  #remove the template file
  file.remove(paste(use.folder, set[1,"sound.files"], sep="/"))
  
  times=sapply(xc, function(x) x$scores$time[which.max(x$scores$score)])
  #times
  peak.score=sapply(xc, function(x) max(x$scores$score))
  #peak.score
  
  
  result.df[[k]]=data.frame(trial=annot$Selection[file_order[k]], call_type=annot$Label[file_order[k]], annotated_mic=annot$Channel[file_order[k]], template=temp.df$sound.file, channel=set[2:6,"channel"], peak.time=times, peak.score=peak.score)
  
  print(k)
}

save(result.df, file=paste(use.folder, "xcorr.results.rdata", sep="/"))
