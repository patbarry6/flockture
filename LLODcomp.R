# read in data
dat<-read.table('./data/small_data.txt',head=F)
# what did flock end up with for final ref pops
Flock.results<-read.table('./data/InitPart=1212.csv',sep=",",header=T)
dat$FLref<-Flock.results$Ref.alloc
#make a list with all possible alleles for each locus
Allele.index<-list()
for (l in 1:15){ 
  Allele.index[[l]]<-sort(as.numeric(union(unique(dat[,2*l]),unique(dat[,2*l+1]))))
}

#Make some lists for allele frequencies for each of the two references
Allele.freqC1<-Allele.index
Allele.freqC2<-Allele.index
#lets make a list of output to compare with flock - obviously LLOD and the likelihood is important
SumStats.names<-c('LLOD','L1','L2')
SumStats <- sapply(SumStats.names,function(x) NULL)

#now lets calculate the likelihood for each individual into either of the two reference populations
    # first we will remove the individual that we are considering
    # second we split the data into the two reference populations
    # third we will calculate the allele frequencies in each reference populaiton
    # remembering to add in 1/2n+1 for alleles that do not occur in a ref 
    # then using the allele frequencies we can calculate the likelihood 
    # as a simple product of the genotype freq at each locus
    # then take let log10 difference of each
for (i in 1:nrow(dat)){
  
  dat.temp<-dat[-i,]# drop the indiviual being assessed
  dat.c1<-dat.temp[dat.temp$FLref=='ref1',] # split the references 
  dat.c2<-dat.temp[dat.temp$FLref=='ref2',] # split the references 
  for (L in 1:15){
    for (a in 1:length(Allele.index[[L]])){ #calculate the allele frequencies for each ref using the allele index - ensures all alleles present are accounted for
      Allele.freqC1[[L]][a]<-sum(dat.c1[,((L*2):(L*2+1))]==Allele.index[[L]][a])/(nrow(dat.c1[,((L*2):(L*2+1))])*2)
      Allele.freqC2[[L]][a]<-sum(dat.c2[,((L*2):(L*2+1))]==Allele.index[[L]][a])/(nrow(dat.c2[,((L*2):(L*2+1))])*2)
  }#over a
}#over L
  
# Some alleles aren't in both populations, sampling error potentially, need to replace 0s when calculating the LLOD
# replace with 1/(2n+1) where n is the size of the ref at current state. 
dat.c1.n<-nrow(dat.c1)
dat.c2.n<-nrow(dat.c2)
# I get to use rapply! how often does that happen?
  Allele.freqC1<-rapply(Allele.freqC1, f=function(x) ifelse(x==0,1/(2*dat.c1.n+1),x), how="replace" ) # if Allele.freqC1 has a 0, replace it
  Allele.freqC2<-rapply(Allele.freqC2, f=function(x) ifelse(x==0,1/(2*dat.c2.n+1),x), how="replace" )
  
#Now lets calculate the likelihood of an individual (the one we removed) being from either reference group
ind.temp<-dat[i,]#whos genotype are we looking at?
  #calcualte likelihood of coming from clust1
  #probably a better way to do this, but this allows us to see exactly where the likelihood comes
  #from and troubleshoot differences, which there are many
L1<-Allele.freqC1[[1]][which(ind.temp[,2]==Allele.index[[1]])]*Allele.freqC1[[1]][which(ind.temp[,3]==Allele.index[[1]])]*
    Allele.freqC1[[2]][which(ind.temp[,4]==Allele.index[[2]])]*Allele.freqC1[[2]][which(ind.temp[,5]==Allele.index[[2]])]*
    Allele.freqC1[[3]][which(ind.temp[,6]==Allele.index[[3]])]*Allele.freqC1[[3]][which(ind.temp[,7]==Allele.index[[3]])]*
    Allele.freqC1[[4]][which(ind.temp[,8]==Allele.index[[4]])]*Allele.freqC1[[4]][which(ind.temp[,9]==Allele.index[[4]])]*
    Allele.freqC1[[5]][which(ind.temp[,10]==Allele.index[[5]])]*Allele.freqC1[[5]][which(ind.temp[,11]==Allele.index[[5]])]*
    Allele.freqC1[[6]][which(ind.temp[,12]==Allele.index[[6]])]*Allele.freqC1[[6]][which(ind.temp[,13]==Allele.index[[6]])]*
    Allele.freqC1[[7]][which(ind.temp[,14]==Allele.index[[7]])]*Allele.freqC1[[7]][which(ind.temp[,15]==Allele.index[[7]])]*
    Allele.freqC1[[8]][which(ind.temp[,16]==Allele.index[[8]])]*Allele.freqC1[[8]][which(ind.temp[,17]==Allele.index[[8]])]*
    Allele.freqC1[[9]][which(ind.temp[,18]==Allele.index[[9]])]*Allele.freqC1[[9]][which(ind.temp[,19]==Allele.index[[9]])]*
    Allele.freqC1[[10]][which(ind.temp[,20]==Allele.index[[10]])]*Allele.freqC1[[10]][which(ind.temp[,21]==Allele.index[[10]])]*
    Allele.freqC1[[11]][which(ind.temp[,22]==Allele.index[[11]])]*Allele.freqC1[[11]][which(ind.temp[,23]==Allele.index[[11]])]*
    Allele.freqC1[[12]][which(ind.temp[,24]==Allele.index[[12]])]*Allele.freqC1[[12]][which(ind.temp[,25]==Allele.index[[12]])]*
    Allele.freqC1[[13]][which(ind.temp[,26]==Allele.index[[13]])]*Allele.freqC1[[13]][which(ind.temp[,27]==Allele.index[[13]])]*
    Allele.freqC1[[14]][which(ind.temp[,28]==Allele.index[[14]])]*Allele.freqC1[[14]][which(ind.temp[,29]==Allele.index[[14]])]*
    Allele.freqC1[[15]][which(ind.temp[,30]==Allele.index[[15]])]*Allele.freqC1[[15]][which(ind.temp[,31]==Allele.index[[15]])]
SumStats[[2]][i]<-L1  
  #calculate likelihood of coming from clust2
  L2<-Allele.freqC2[[1]][which(ind.temp[,2]==Allele.index[[1]])]*Allele.freqC2[[1]][which(ind.temp[,3]==Allele.index[[1]])]*
    Allele.freqC2[[2]][which(ind.temp[,4]==Allele.index[[2]])]*Allele.freqC2[[2]][which(ind.temp[,5]==Allele.index[[2]])]*
    Allele.freqC2[[3]][which(ind.temp[,6]==Allele.index[[3]])]*Allele.freqC2[[3]][which(ind.temp[,7]==Allele.index[[3]])]*
    Allele.freqC2[[4]][which(ind.temp[,8]==Allele.index[[4]])]*Allele.freqC2[[4]][which(ind.temp[,9]==Allele.index[[4]])]*
    Allele.freqC2[[5]][which(ind.temp[,10]==Allele.index[[5]])]*Allele.freqC2[[5]][which(ind.temp[,11]==Allele.index[[5]])]*
    Allele.freqC2[[6]][which(ind.temp[,12]==Allele.index[[6]])]*Allele.freqC2[[6]][which(ind.temp[,13]==Allele.index[[6]])]*
    Allele.freqC2[[7]][which(ind.temp[,14]==Allele.index[[7]])]*Allele.freqC2[[7]][which(ind.temp[,15]==Allele.index[[7]])]*
    Allele.freqC2[[8]][which(ind.temp[,16]==Allele.index[[8]])]*Allele.freqC2[[8]][which(ind.temp[,17]==Allele.index[[8]])]*
    Allele.freqC2[[9]][which(ind.temp[,18]==Allele.index[[9]])]*Allele.freqC2[[9]][which(ind.temp[,19]==Allele.index[[9]])]*
    Allele.freqC2[[10]][which(ind.temp[,20]==Allele.index[[10]])]*Allele.freqC2[[10]][which(ind.temp[,21]==Allele.index[[10]])]*
    Allele.freqC2[[11]][which(ind.temp[,22]==Allele.index[[11]])]*Allele.freqC2[[11]][which(ind.temp[,23]==Allele.index[[11]])]*
    Allele.freqC2[[12]][which(ind.temp[,24]==Allele.index[[12]])]*Allele.freqC2[[12]][which(ind.temp[,25]==Allele.index[[12]])]*
    Allele.freqC2[[13]][which(ind.temp[,26]==Allele.index[[13]])]*Allele.freqC2[[13]][which(ind.temp[,27]==Allele.index[[13]])]*
    Allele.freqC2[[14]][which(ind.temp[,28]==Allele.index[[14]])]*Allele.freqC2[[14]][which(ind.temp[,29]==Allele.index[[14]])]*
    Allele.freqC2[[15]][which(ind.temp[,30]==Allele.index[[15]])]*Allele.freqC2[[15]][which(ind.temp[,31]==Allele.index[[15]])]
  SumStats[[3]][i]<-L2
  SumStats[[1]][i]<-abs(log10(L1)-log10(L2))
}
SumStats$LLOD==Flock.results$Unsigned.LLOD #Are all the LLOD values the same?
plot(SumStats$LLOD,Flock.results$Unsigned.LLOD,xlab='MyCalc',ylab='Flock',xlim=c(0,10),ylim=c(0,10))
abline(c(0:1))