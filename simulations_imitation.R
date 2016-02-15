# DISTRIBUTION FUNCTION; FOR DRAW OF RANDOM PRICES

library(distr)
library(ggplot2)

pricesA<-seq(from=0, to=5,by=0.1)
pricesBC<-c(pricesA,pricesA+5) # in second place, the CS prices --> identified as CS by adding 5

#   to draw starting points
distA <-DiscreteDistribution(supp = pricesA, prob = rep(1/length(pricesA),length(pricesA)))
distBC <-DiscreteDistribution(supp = pricesBC, prob = rep(1/length(pricesBC),length(pricesBC)))
#     
rdistA <- r(distA)                 # function to create random variates from p
rdistBC <- r(distBC)

pA<-c(rdistA(1))
pB<-c(rdistBC(1))
pC<-c(rdistBC(1))


# PROFIT FUNCTION

profit_ftn<-function(pA,pB,pC){

  stA<-(pA<=5)
  stB<-(pB>5)
  stC<-(pC>5)
  
  p<-cbind(pA,pB-5*stB,pC-5*stC,stA,stB,stC)
  p<-matrix(p,ncol = 6)
  
  st<-p[4:6]
  p<-p[1:3]
  v_naiveA<-c(v+e,v,v)-p # value for naive consumer of type A
  v_naiveA
  v_naiveB<-c(v,v+e,v)-p
  v_naiveC<-c(v,v,v+e)-p
  v_n_A<-as.numeric(v_naiveA==max(v_naiveA)) # which firm does naive of type A buy from (a,b,c)
  v_n_B<-as.numeric(v_naiveB==max(v_naiveB)) 
  v_n_C<-as.numeric(v_naiveC==max(v_naiveC)) 
  sales_n_A<-v_n_A/sum(v_n_A) # to take account of ties
  sales_n_B<-v_n_B/sum(v_n_B)
  sales_n_C<-v_n_C/sum(v_n_C)
  
  
  v_savvyA<-c(v,v,v)+c(e,0,0)*(1-st)-p*(1+(1-st)*lambda)
  v_savvyB<-c(v,v,v)+c(0,e,0)*(1-st)-p*(1+(1-st)*lambda)
  v_savvyC<-c(v,v,v)+c(0,0,e)*(1-st)-p*(1+(1-st)*lambda)
  v_s_A<-as.numeric(v_savvyA==max(v_savvyA)) 
  v_s_B<-as.numeric(v_savvyB==max(v_savvyB)) 
  v_s_C<-as.numeric(v_savvyC==max(v_savvyC)) 
  sales_s_A<-v_s_A/sum(v_s_A)
  sales_s_B<-v_s_B/sum(v_s_B)
  sales_s_C<-v_s_C/sum(v_s_C)
  profits<-c(sum(st)==1)*(sales_n_A+sales_n_B+sales_n_C)*p+(sum(st)!=1)*((1-mu)*(sales_n_A+sales_n_B+sales_n_C)*p+mu*(sales_s_A+sales_s_B+sales_s_C)*p)
  profits<-profits*(p<5)
  profits<-profits*(p>0)
}

fbb<-function(x) profit_ftn(x[1],x[2],x[3]) #to compute max


# Best Imitation Functions

Best<-function(pA,pB,pC){
  p<-c(pA,pB,pC)
  st<-(p>5)
  profit<-profit_ftn(pA,pB,pC)
  profit
  p_best<-p[which.max(profit)]
  which.max(profit)
  st_best<-(p_best>5)
  pA<-(which.max(profit)==1)*(p_best)+(which.max(profit)>1)*(p_best-5*st_best) # to translate CS price into NCS price
  pB<-(which.max(profit)==1)*(p_best+5*st[2])+(which.max(profit)>1)*(p_best)
  pC<-(which.max(profit)==1)*(p_best+5*st[3])+(which.max(profit)>1)*(p_best)
  p_best<-c(pA,pB,pC)
  
  }

#test

mu=0.1
lambda=0.3
pA=4.9
pB=5.1
pC=5

print(Best(pA,pB,pC))


# RUNNING SIMULATION OF BEST RESPONSE DYNAMICS FOR GRAPHICAL REPRESENTATION

# INITIALIZATION

v<-5
e<-1

#start

pA<-3
pB<-5
pC<-6

P<-c(0,pA,pB,pC)
mu=0.3
lambda=0.2
jig<-0.015 #size of price experimentation
jig_st<-0.002 #probability of random format change


T<-5000
i<-1
while(i<=T){
  pA<-Best(pA,pB,pC)[1]
  pB<-Best(pA,pB,pC)[2]
  pC<-Best(pA,pB,pC)[3]
  stB<-(pB>5)
  stC<-(pC>5)
  pA<-pA+jig*runif(1,-1,1)
  pA<-(pA>5)*5+(pA<=5)*pA
  pA<-(pA<0)*0+(pA>=0)*pA
  rand<-runif(1,0,1)
  pB<-pB-5*stB+jig*runif(1,-1,1)
  pB<-(pB>5)*5+(pB<=5)*pB
  pB<-(pB<0)*0+(pB>=0)*pB
  stB<-(rand>jig_st)*stB+(rand<=jig_st)*(1-stB)
  pB<-pB+5*stB
  
  rand2<-runif(1,0,1)
  pC<-pC-5*stC+jig*runif(1,-1,1)
  pC<-(pC>5)*5+(pC<=5)*pC
  pC<-(pC<0)*0+(pC>=0)*pC
  stC<-(rand2>jig_st)*stC+(rand2<=jig_st)*(1-stC)
  pC<-pC+5*stC
  P<-rbind(P,c(i,pA,pB,pC))
  i<-i+1
}

P2<-matrix(P,ncol = 4)
P2<-P2[2:(T+1),] # get rid of initial values
nr_formatA<-(P2[,3]>5)+(P2[,4]>5)+1 # number of firms with format A
nr_formatA <- as.factor(nr_formatA)
#names(nr_formatA)<-c("firms with format A")
levels(nr_formatA) <- c("1", "2","3")
P3<-cbind(P2,nr_formatA)

Pf<-data.frame(P3)

names(Pf) <- c("time","prices","priceb","pricec","firmswithformatA")
Pf$firmswithformatA <- as.factor(Pf$firmswithformatA)
levels(Pf$firmswithformatA) <- c("1", "2","3")

# graph in paper


ggplot(Pf, aes(x=time, y=prices, group=1, colour=firmswithformatA)) +
  geom_point(size=2) + 
  geom_line(color='steelblue',size=1, alpha=0.3)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("period")

ggsave("Figure_mu_0.3.pdf", width = 12, height = 6, units = "in", dpi = 400)


# best imitation graph in the triopoly (given price by A and price by B, what is the best choice by C)

mu<-0.5
lambda<-0.5

pA<-1
pC<-5 #we do not care about this number since we compute what is the best to imitate of A and B for firm C
pB<-seq(from=0, to=10,by=0.01)
p<-rbind(pA,pB,pC)
p<-matrix(p,nrow=3)
p<-t(p)

imit_best<-function(x) Best(x[1],x[2],x[3]) #to compute as function of vector

tC<-apply(X=p,MARGIN=1,FUN=imit_best)
st<-(tC>5) #idneitfy standard offers
st
tC
p
st
as.numeric(tC[2,])
as.numeric(st[2,])
p2<-cbind(p,st[2,],as.numeric(tC[2,]-5*as.numeric(st[2,])))
p2<-matrix(p2,ncol = 5)
P2<-cbind(as.numeric(seq(1,1001,1)),p2)
P3<-matrix(P2,ncol = 6)
P<-data.frame(P3)
P$X1 <- factor(P$X1, levels = P$X1)

library(ggplot2)

pB<-P[,3]
pC<-P[,6]
format<-factor(P[,5])

ggplot(P, aes(x=pB, y=pC, group=1, color=format)) + geom_point(size=4) + geom_point(color='steelblue',size=1, alpha=0.3)