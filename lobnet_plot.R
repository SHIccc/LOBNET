rm(list=ls(all=T))
graphics.off()
require(igraph)
require(zoo)

#setwd("~/Dropbox/Memeda/hfhd/pplot")
setwd("~/Dropbox/hfhd/pplot")
load("Brexit_Size_SPM_NL.RData")#sparse estimation using glasso

#pm: the estimated the precision matrix
pm1=matrix(list2[[1]]$icov[7][[1]], 629,629)

#f1<-NULL
f1<-vector("list",length=17)
pm<-vector("list",length=length(list2))
for (i in 1:length(list2)){
  pm[[i]] = matrix(list2[[i]]$icov[7][[1]], 629, 629)
  for (j in 1:17){
    #f1[j] <- sum(abs(pm[[i]][(37*j-36),]))
    #f2[j] = c(f2[[j]], f1[j])
    w <- sum(abs(pm[[i]][(37*j-36),]))
    f1[[j]] = c(f1[[j]], w)
  }
}
names(pm)=names(list2)
#Technology: T, IBM, MSFT, CSCO
#Health Care: JNJ, PFE, MRK, MDT
#Finance: JPM, WFC, BAC, C
#Consumer Good: AAPL, PEP, KO
#Industrial Good: GE
#Basic Material: XOM
names(f1) = c("AAPL","BAC", "C","CSCO", "GE","IBM", "JNJ", #7
              "JPM","KO","MDT","MRK","MSFT","PEP","PFE",  #7
              "T","WFC","XOM")                          #3

#plot of "importantce"
par(mfrow = c(1,1))
#technology
t.atnt = ts(f1[[15]],start=c(2016,6,1),frequency=365.25)
t.ibm = ts(f1[[6]],start=c(2016,6,1),frequency=365.25)
t.msft = ts(f1[[12]],start=c(2016,6,1),frequency=365.25)
t.csco = ts(f1[[4]],start=c(2016,6,1),frequency=365.25)
#healthcare
t.jnj = ts(f1[[7]],start=c(2016,6,1),frequency=365.25)
t.pfe = ts(f1[[14]],start=c(2016,6,1),frequency=365.25)
t.mrk = ts(f1[[11]],start=c(2016,6,1),frequency=365.25)
t.mdt = ts(f1[[10]],start=c(2016,6,1),frequency=365.25)
#finance
t.jpm = ts(f1[[8]],start=c(2016,6,1),frequency=365.25)
t.wfc = ts(f1[[16]],start=c(2016,6,1),frequency=365.25)
t.bac = ts(f1[[2]],start=c(2016,6,1),frequency=365.25)
t.c = ts(f1[[3]],start=c(2016,6,1),frequency=365.25)
#Consumer Goods
t.aapl = ts(f1[[1]],start=c(2016,6,1),frequency=365.25)
t.pep = ts(f1[[13]],start=c(2016,6,1),frequency=365.25)
t.ko = ts(f1[[9]],start=c(2016,6,1),frequency=365.25)
#Industrial Goods
t.ge = ts(f1[[5]],start=c(2016,6,1),frequency=365.25)
#Basic Material
t.xom = ts(f1[[17]],start=c(2016,6,1),frequency=365.25)

par(mfrow = c(3,2))
plot.ts(t.atnt, ylim=c(1,4.5), ylab="Technology")
lines(t.ibm, lty=2)
lines(t.msft, lty=3)
lines(t.csco, lty=4)
plot(t.jnj, ylim=c(1,4.5), ylab="Health Care")
lines(t.pfe, lty=2)
lines(t.mrk, lty=2)
lines(t.mdt, lty=2)
plot(t.jpm, ylim=c(1,4.5), ylab="Finance")
lines(t.wfc, lty=2)
lines(t.bac, lty=2)
lines(t.c, lty=2)
plot(t.aapl, ylim=c(1,4.5), ylab = "Consumer Goods")
lines(t.pep, lty=2)
lines(t.ko, lty=2)
plot(t.ge, ylim=c(1,4.5), ylab="Industrial Goods")
plot(t.xom, ylim=c(1,4.5), ylab="Basic Material")

#plot
str(f1)
ff1 = lapply(f1, function(x) mean(x)) #WFC and JPM

nnzero(pm[[1]][1:37,])

#calculate the average
myavg = lapply(f1, function(x) mean(x))

#find nonzero
f3<-vector("list",length=17)
for (i in 1:length(pm)){
  for (j in 1:17){
    ww <- nnzero(pm[[i]][(37*j-36):(37*j),])
    #ww <- nnzero(pm[[i]][(37*j-36):(37*j),-((37*j-36):(37*j))])
    f3[[j]] = c(f3[[j]], ww)
  }
}
names(f3) = c("AAPL","BAC", "C","CSCO", "GE","IBM", "JNJ", #7
              "JPM","KO","MDT","MRK","MSFT","PEP","PFE",  #7
              "T","WFC","XOM") 

ff3 =lapply(f3, function(x) mean(x))#select JPM BAC

par(mfrow = c(1,1))
plot(t.wfc, ylim=c(1,4.5))
lines(t.pfe, col="red")
lines(t.jpm, col="blue")
lines(t.c, col="green")

