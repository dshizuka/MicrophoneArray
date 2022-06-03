###Cross-correlations

library(seewave)
library(lubridate)
library(warbleR)
library(stringr)
library(tidyverse)
library(magrittr)
library(foreach)
library(doParallel)
options(digits=10)

n.cores=detectCores()
registerDoParallel(n.cores-2)

folders.full=list.files(pattern="MC", full.names=T)
folders.short=list.files(pattern="MC", full.names=F)
use.folder=folders.full[2]
use.folder.short=folders.short[2]
mc_track_full=list.files(use.folder, pattern=".wav", full.names = T)
mc_track_short=list.files(use.folder, pattern=".wav", full.names = F)

annot=read.table(list.files(use.folder, pattern=".txt", full.names = T), header=T, sep="\t")

names(annot)

seltab=annot %>% mutate(sound.files=mc_track_short) %>% select(sound.files, channel=Channel, start=Begin.Time..s., end=End.Time..s., bottom.freq=Low.Freq..Hz., top.freq=High.Freq..Hz., type=Label)

seltab$selec=seq(1:nrow(seltab))
seltab$channel=as.numeric(seltab$channel)

#this creates the "template" sounds and saves them into a folder
dir.create(paste(use.folder.short, "templates", sep="/")) #creates a new folder called "templates" within the focal folder.
cut_sels(seltab, path=use.folder.short, dest.path=paste(use.folder.short, "templates", sep="/"))

#set up cross correlations

#first, we need to make a selection table for each template. This will include the template track and the test track clips that will be compared
#The original code creates a temporary file extracted from the templates folder, then runs the cross correlation, then deletes it. However, when running this in parallel, we end up with multiple template files ported at once, so it gets confused. Need to fix this by...

temp_names=list.files(paste(use.folder, "templates", sep="/"))
if(is.na(match(mc_track_short, temp_names))==F) temp_names=temp_names[-match(mc_track_short, temp_names)]

file_order=as.numeric(str_sub(temp_names, start=str_locate(temp_names, "-")[,1]+1, end=str_locate(temp_names, "\\.")[,1]-1))

#result.df=list()
#copy full multichannel file over to template subfolder (we need all sound files to be in same folder to run the cross correlation). 

if(is.na(match(mc_track_short, list.files(paste(use.folder, "templates", sep="/"))))==T) file.copy(from=list.files(use.folder, pattern=".wav", full.names = T), to=paste(use.folder, "templates", sep="/"))

ptm=proc.time()

result.df=foreach(k = 1:nrow(annot), .inorder=TRUE) %dopar% {
  temp.df=data.frame(sound.files=temp_names[order(file_order)][k], channel=1, selec=1, start=0, end=duration_wavs(paste(use.folder, "templates", temp_names[order(file_order)][k], sep="/"))$duration)
  
  track.df=data.frame(sound.files=mc_track_short, channel=1:5, selec=1:5, start=seltab$start[k]-0.5, end=seltab$end[k]+0.5)
  
  bp_filter=c(seltab$bottom.freq[k]/1000-0.1, seltab$top.freq[k]/1000+0.1)
  
  set=rbind(temp.df, track.df)
  
  
  
  #run cross-correlation
  xc=list()
  for(j in 1:(nrow(set)-1)){
    xc[[j]]=cross_correlation(set[c(1,1+j),], bp=bp_filter, output="list", ovlp=99, path=paste(use.folder,"templates",sep="/"))
  }
  #xc
  
  times=sapply(xc, function(x) x$scores$time[which.max(x$scores$score)])
  #times
  peak.score=sapply(xc, function(x) max(x$scores$score))
  #peak.score
  
  
  data.frame(trial=k, call_type=annot$Label[k], annotated_mic=annot$Channel[k], template=temp.df$sound.file, channel=set[2:6,"channel"], peak.time=times, peak.score=peak.score)
}

proc.time()-ptm
stopImplicitCluster()
###

save(result.df, file=paste(use.folder, "xcorr.results_99_bpfilter.rdata", sep="/"))
