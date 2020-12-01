#week3 Kloss
#1
dmsfunction<-function(d,m,s){
  rad<-pi*(d+m/60+s/3600)/180
  return (rad)}
heightmin<-tan(dmsfunction(1,21,-1))*(2550-25)
heightmax<-tan(dmsfunction(1,21,1))*(2550+25)
paste(heightmin)                  #heigtmin
paste(heightmax)                  #heightmax
paste((heightmax+heightmin)/2)    #heightmean
#uncertainty?

#2
paste(durationmax<-(29.66+0.2)-(25.53-0.1)) #max duration in ma
paste(durationmin<-(29.66-0.2)-(25.53+0.1)) #min duration in ma
#duration was between 3.83 ma and 3.43 ma

#3
eqscals<-read.table("C:/Users/CKloss/Desktop/R course/text.txt")
colnames(eqscals)<-c("X","r","Mo")
#a
mean(eqscals$r)
median(eqscals$r)
sd(eqscals$r)
mad(eqscals$r)
mean(eqscals$Mo)
median(eqscals$Mo)
sd(eqscals$Mo)
mad(eqscals$Mo)
#b
boxplot(eqscals$r)
boxplot(eqscals$Mo)
#no outliers
#c
abc<-3*mad(eqscals$Mo)
trimmed_eqscals<-data.frame(eqscals[eqscals$Mo<abc,])
mean(trimmed_eqscals$Mo)
median(trimmed_eqscals$Mo)
sd(trimmed_eqscals$Mo)
boxplot(trimmed_eqscals$Mo)
#no outliers
bestMo<-median(trimmed_eqscals$Mo)
paste(bestMo)
#uncertainty?
#d
Mw_Mo<-(log(median(trimmed_eqscals$Mo)))/1.5-6.0
paste(Mw_Mo)
#uncertainty?

