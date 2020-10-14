
setwd("taxonomy-working/working-entry1/Description")
dfiles=list.files(path="Description") #List of all description files

#ISO code of country to search for
country="NGA" #Ex: Nigeria

#get country for each UID
uid=rep(NA,length(dfiles))
ccode=uid
for (i in 1:length(dfiles)){
  dat=read.csv(file=dfiles[i],header=F,row.names=1,as.is=T)
  uid[i]=dat["uid",1]
  ccode[i]=dat["ISO_A1",1]
}
ctry.df<-data.frame(uid,ccode)

##Search for unique country (or list of countries)
ctry.df$uid[which(ctry.df$ccode %in% country)]