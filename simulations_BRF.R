# DISTRIBUTION FUNCTION; FOR DRAW OF RANDOM PRICES

library(distr)

pricesA<-seq(from=0, to=5,by=0.1) # careful with choosing step, in simulations, step=0.1. step=0.01 to get more precision in graph
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

# defining function to compare vectors with a bit of leeway 
#(since R makes things difficult and like to torture people with nitty gritty)

elementwise.all.equal <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))})


# PROFIT FUNCTION

v<-5
e<-1
pA<-2
pB<-2.9
pC<-6.9

profit_ftn<-function(pA,pB,pC){
 
  stA<-(pA<=5)
  stB<-(pB>5)
  stC<-(pC>5)
  st<-c(stA,stB,stC)
  p<-c(pA,pB-5*stB,pC-5*stC)

  v_naiveA<-round(c(v+e,v,v)-p,digits=2) # value for naive consumer of type A
  v_naiveB<-round(c(v,v+e,v)-p,digits=2)
  v_naiveC<-round(c(v,v,v+e)-p,digits=2)
  v_n_A<-(v_naiveA==max(v_naiveA)) # which firm does naive of type A buy from (a,b,c)
  v_n_B<-(v_naiveB==max(v_naiveB)) 
  v_n_C<-(v_naiveC==max(v_naiveC)) 
  sales_n_A<-v_n_A/sum(v_n_A) # to take account of ties
  sales_n_B<-v_n_B/sum(v_n_B)
  sales_n_C<-v_n_C/sum(v_n_C)
  
  
  v_savvyA<-c(v,v,v)+c(e,0,0)*(1-st)-p*(1+(1-st)*lambda)
  v_savvyB<-c(v,v,v)+c(0,e,0)*(1-st)-p*(1+(1-st)*lambda)
  v_savvyC<-c(v,v,v)+c(0,0,e)*(1-st)-p*(1+(1-st)*lambda)
  v_s_A<-round(v_savvyA==max(v_savvyA),digits=2) 
  v_s_B<-round(v_savvyB==max(v_savvyB),digits=2) 
  v_s_C<-round(v_savvyC==max(v_savvyC),digits=2) 
  sales_s_A<-v_s_A/sum(v_s_A)
  sales_s_B<-v_s_B/sum(v_s_B)
  sales_s_C<-v_s_C/sum(v_s_C)
  profits<-c(sum(st)==1)*(sales_n_A+sales_n_B+sales_n_C)*p+(sum(st)!=1)*((1-mu)*(sales_n_A+sales_n_B+sales_n_C)*p+mu*(sales_s_A+sales_s_B+sales_s_C)*p)
}

fbb<-function(x) profit_ftn(x[1],x[2],x[3]) #to compute max

print(profit_ftn(2,2.9,6.9))

# Best Response Functions

BRC<-function(pA,pB,pC){
  #vector of prices for given pA, pB
  pA<-rep(pA,length(pricesBC))
  pB<-rep(pB,length(pricesBC))
  P_C<-cbind(pA,pB,pricesBC)
  P_C<-matrix(P_C,ncol = 3)
  tC<-apply(X=P_C,MARGIN=1,FUN=fbb) # vector of profits
  tC<-cbind(c(pricesBC),t(tC))
  BR_C<-tC[which.max(tC[,4]),1] # note, could take two values --> make choice, at the moment, random.
}

BRB<-function(pA,pB,pC){
  #vector of prices for given pA, pC
  
  pA<-rep(pA,length(pricesBC))
  pC<-rep(pC,length(pricesBC))
  P_B<-cbind(pA,pricesBC,pC)
  P_B<-matrix(P_B,ncol = 3)
  tB<-apply(X=P_B,MARGIN=1,FUN=fbb) # vector of profits
  tB<-cbind(c(pricesBC),t(tB))
  BR_B<-tB[which.max(tB[,3]),1] # note, could take two values --> make choice
}

BRA<-function(pA,pB,pC){
  #vector of prices for given pB, pC
  
  pB<-rep(pB,length(pricesA))
  pC<-rep(pC,length(pricesA))
  P_A<-cbind(pricesA,pB,pC)
  P_A<-matrix(P_A,ncol = 3)
  tA<-apply(X=P_A,MARGIN=1,FUN=fbb) # vector of profits
  tA<-cbind(c(pricesA),t(tA))
  BR_A<-tA[which.max(tA[,2]),1] # note, could take two values --> make choice
}


# answers to scenarios to be played by participants in advance of experiment
# set prices to be computed with 1cent precision

vmu<-c(0,0.1,0.2)
vlambda<-c(0.1,0.2)

results<-c("mu","lambda","s1","s2","s3","s4")
for(mu in unique(vmu)){
  for(lambda in unique(vlambda)){
s1<-BRA(0,7.7,9)
s2<-BRA(0,1.5,1.6)
s3<-BRB(2,0,6.9)
s4<-BRC(4.1,4.5,0)
results<-cbind(results, c(mu,lambda,s1,s2,s3,s4))
}
}
results

# BRF graph in the triopoly

mu<-0.2
lambda<-0.2


pA<-pC<-5
pB<-seq(from=0, to=10,by=0.1)
p<-rbind(pA,pB,pC)
p<-matrix(p,nrow=3)
p<-t(p)

fBRC<-function(x) BRC(x[1],x[2],x[3]) #to compute as function of vector

tC<-apply(X=p,MARGIN=1,FUN=fBRC)
st<-(tC>5) #idneitfy standard offers
p2<-cbind(p,st,as.numeric(tC)-5*st)
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


# RUNNING SIMULATION OF BEST RESPONSE DYNAMICS FOR GRAPHICAL REPRESENTATION

# INITIALIZATION

v<-5
e<-1

#start

pA<-c(rdistA(1))
pB<-c(rdistBC(1))
pC<-c(rdistBC(1))

P<-c(pA,"A")
mu=0.7
lambda=0.3


T<-180
i<-1
while(i<=T){
  rnd<-sample(1:3,1,replace=T)
  if(rnd==2){
    pB<-BRB(pA,pB,pC)
    P<-rbind(P,c(pB,"B"))
  }
  if(rnd==3){
    pC<-BRC(pA,pB,pC)
    P<-rbind(P,c(pC,"C"))
  }
  if(rnd==1){
    pA<-BRA(pA,pB,pC)
    P<-rbind(P,c(pA,"A"))
  }
i<-i+1
}

P2<-cbind(as.numeric(as.character(P[,1])),P[,2])
st<-(P[,1]>5)

P2<-matrix(P2,ncol=2)
p<-cbind(P2,st,as.numeric(P2[,1])-5*st)
p<-matrix(p,ncol = 4)

P2<-cbind(as.numeric(seq(1,T+1,1)),p)
P3<-matrix(P2,ncol = 5)
P<-data.frame(P3)
P$X1 <- factor(P$X1, levels = P$X1)
P$X5 <- as.numeric(as.character(P$X5))


library(ggplot2)
qplot(P[,1], P[,5], data=P, colour=P[,3])

P30<-P[(T/2):T,]
P30[,4]<-as.numeric(P30[,4])-1
P30[,4]<-P30[,4]+as.numeric(P30[,3]=="A")

time<-P30[,1]
prices<-P30[,5]
format<-factor(P30[,4])
firm<-P30[,3]

# graph in paper (depends on random draw of turns to best reply)
ggplot(P30, aes(x=time, y=prices,group=1, colour=format,shape=firm)) + geom_point(size=4) + geom_line(color='steelblue',size=1, alpha=0.3)


# RUNNING SIMULATIONS FOR COMPUTATION OF AVERAGE PROFIT BY CONDITION; WITH RANDOM SEQUENCE OF BR AND LATENCY


table_param=1

if(table_param==1){
vmu<-c(0,0.3,0.7,1)
vlambda<-c(0,0.3,100)
}else{
vmu<-c(0,0.1,0.2)
vlambda<-c(0.1,0.2)
}

profit<-c(rep(0,7))
lat<-0 # latency
jig<-0 # if want to add random noise to BR, in order not to have stuck prices when latency very high

vmu
vlambda
# LOOPING

for(mu in unique(vmu)){
  for(lambda in unique(vlambda)){

    print(mu)
    print(lambda)
    
    J<-1 # number of different starting prices to go through
    T<-20000 # number of periods after given starting price
    
    P<-matrix(-1,ncol=3,nrow=J*T+1) #matrix to record prices over time
    P[1,]<-c(0,0,0)
    j<-1
    while(j<=J){ 
      
#      pA<-c(rdistA(1)) #random starting prices
#      pB<-c(rdistBC(1))
#      pC<-c(rdistBC(1))
      pA<-2 # arbitary starting prices
      pB<-2
      pC<-2
      
      P[(j-1)*T+1,]<-c(pA,pB,pC)
      i<-2      
      while(i<=T){
        rnd<-sample(1:3,1,replace=T)
        if(rnd==2){
          
          pB<-BRB(pA,pB,pC)
          pB<-(lat*(P[(i-1),2]-5*as.numeric(P[(i-1),2]>5))+(1-lat)*(pB-5*as.numeric(pB>5)))+5*as.numeric(pB>5)+jig*runif(1,-1,1) # adopts the best standard but adjusts price only slowly
          P[(j-1)*T+i,]<-c(pA,pB,pC)
          P
        }
        if(rnd==3){
          pC<-BRC(pA,pB,pC)
          pC<-(lat*(P[(i-1),3]-5*as.numeric(P[(i-1),3]>5))+(1-lat)*(pC-5*as.numeric(pC>5)))+5*as.numeric(pC>5)+jig*runif(1,-1,1) # adopts the best standard but adjusts price only slowly
          P[(j-1)*T+i,]<-c(pA,pB,pC)
        }
        if(rnd==1){
          pA<-BRA(pA,pB,pC)
          pA<-(lat*(P[(i-1),1]-5*as.numeric(P[(i-1),1]>5))+(1-lat)*(pA-5*as.numeric(pA>5)))+5*as.numeric(pA>5)+jig*runif(1,-1,1) # adopts the best standard but adjusts price only slowly
          P[(j-1)*T+i,]<-c(pA,pB,pC)
        }
        i<-i+1
        }
      j<-j+1
    }
    
    t<-apply(X=P,MARGIN=1,FUN=fbb)
    profitA<-mean(t[1,2:(J*T+1)])
    profitBC<-mean(t[2:3,2:(J*T+1)])
    format<-mean(P[2:(J*T+1),2:3]>5)
    formatB<-mean(P[2:(J*T+1),2]>5)
    formatC<-mean(P[2:(J*T+1),3]>5)
    
    profit<-rbind(profit,c(mu,lambda,profitA,profitBC,format,formatB,formatC))
  }}

profitABC<-cbind(profit,(profit[,3]+2*profit[,4])/3)
profitABC

if(table_param==1){
write.csv(profitABC, file="profitABC_table_param.csv")
  }else{
  write.csv(profitABC, file="profitABC_experiment_param.csv")}

Pf<-data.frame(P)
qplot(c(seq(1,T*J+1,1)), Pf[,3]-5*as.numeric(Pf[,3]>5), data=Pf)
qplot(c(seq(1,T*J+1,1)), Pf[,2]-5*as.numeric(Pf[,2]>5), data=Pf)
qplot(c(seq(1,T*J+1,1)), Pf[,1]-5*as.numeric(Pf[,1]>5), data=Pf)


