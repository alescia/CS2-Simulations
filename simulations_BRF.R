library(distr)
library(nnet)
library(ggplot2)

##################
# PROFIT FUNCTION
##################

v<-5
e<-1
mu<-0.2
lambda<-0.2
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

##########################
# Best Response Functions
##########################

BRC<-function(pA,pB,pC){
  #vector of prices for given pA, pB
  pA<-rep(pA,length(pricesBC))
  pB<-rep(pB,length(pricesBC))
  P_C<-cbind(pA,pB,pricesBC)
  P_C<-matrix(P_C,ncol = 3)
  tC<-apply(X=P_C,MARGIN=1,FUN=fbb) # vector of profits
  tC<-cbind(c(pricesBC),round(t(tC),digits=2))
  BR_C<-tC[which.is.max(tC[,4]),1] # choose at random if tie
}

pA<-2
pB<-5
pC<-6.9
mu<-0
lambda<-0


BRB<-function(pA,pB,pC){
  #vector of prices for given pA, pC
  
  pA<-rep(pA,length(pricesBC))
  pC<-rep(pC,length(pricesBC))
  P_B<-cbind(pA,pricesBC,pC)
  P_B<-matrix(P_B,ncol = 3)
  tB<-apply(X=P_B,MARGIN=1,FUN=fbb) # vector of profits
  tB<-cbind(c(pricesBC),round(t(tB),digits=2))
  tB
  BR_B<-tB[which.is.max(tB[,3]),1] # note, choose at random when can take two values
}


BRA<-function(pA,pB,pC){
  #vector of prices for given pB, pC
  
  pB<-rep(pB,length(pricesA))
  pC<-rep(pC,length(pricesA))
  P_A<-cbind(pricesA,pB,pC)
  P_A<-matrix(P_A,ncol = 3)
  tA<-apply(X=P_A,MARGIN=1,FUN=fbb) # vector of profits
  tA<-cbind(c(pricesA),round(t(tA),digits=2))
  BR_A<-tA[which.is.max(tA[,2]),1]
}


fBRA<-function(x) BRA(x[1],x[2],x[3]) #to compute as function of vector
fBRB<-function(x) BRB(x[1],x[2],x[3]) #to compute as function of vector
fBRC<-function(x) BRC(x[1],x[2],x[3]) #to compute as function of vector

############################################################################
# answers to scenarios to be played by participants in advance of experiment
############################################################################

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

#######################################################################
# duopoly BRF graph, with data from excel simulations, for paper
#######################################################################

pA<-seq(from=0, to=5,by=0.1)
stB<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.5,1,1,1,1,1,1,1,1,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
format<-as.factor(stB)
levels(format) <- c("B", "X", "A")
format <- factor(format,levels(format)[c(3,1,2)])
pB<-c(0.90,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2,2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,3,3.1,3.2,3.3,3.4,3.5,2.6,2.7,2.8,2.9,3,3.1,3.2,3.3,2.4,2.5,2.6,2.7,2.8,2.9,3,3.1,3.2,3.3,3.4,3.5,3.6,3.7,3.8,3.9)
P<-data.frame(pA,pB,format)

lab <- c("A","B","X")
initialShapes <- unlist(lapply(lab, utf8ToInt))

ggplot(data=P, aes(x=pA, y=pB, shape = format, label=format)) + 
  geom_point(size=4) + 
  # geom_text()+
  #geom_point(color='steelblue',size=1, alpha=0.3)+ 
  expand_limits(x = 0, y = 0)+
  theme_light()+
  xlab("price by firm A")+ylab("price by firm B")+
  scale_shape_manual(values = initialShapes,
                     labels=c("Format A", "Format B", "Indifferent"))+
  theme(legend.position = "bottom", legend.title = element_blank())
ggsave("Figure_BRF_Duopoly.pdf", width = 12, height = 6, units = "in", dpi = 400)

theme_light()
ggplot(data = iris,
       aes(x = Sepal.Length, y = Sepal.Width, colour = Species, shape = Species, label = Species)) + 
  geom_point() +
  geom_text(show.legend = FALSE)

#############################
# BRF graph in the triopoly
#############################


pA<-pC<-5
pB<-seq(from=0, to=10,by=0.1)
p<-rbind(pA,pB,pC)
p<-matrix(p,nrow=3)
p<-t(p)

tC<-apply(X=p,MARGIN=1,FUN=fBRC)
st<-(tC>5) #idneitfy standard offers
p2<-cbind(p,st,as.numeric(tC)-5*st)
p2<-matrix(p2,ncol = 5)
P2<-cbind(as.numeric(seq(1,101,1)),p2)
P3<-matrix(P2,ncol = 6)
P<-data.frame(P3)
P$X1 <- factor(P$X1, levels = P$X1)

pB<-P[,3]
pC<-P[,6]
format<-factor(P[,5])

ggplot(P, aes(x=pB, y=pC, group=1, color=format)) + geom_point(size=4) + geom_point(color='steelblue',size=1, alpha=0.3)

############################################################################
# RUNNING SIMULATION OF BEST RESPONSE DYNAMICS FOR GRAPHICAL REPRESENTATION
############################################################################

# INITIALIZATION

pricesA<-seq(from=0, to=5,by=0.05) # careful with choosing step, in simulations, step=0.1. step=0.01 to get more precision in graph, but can lead to dynamics that are too slow
pricesBC<-c(pricesA,pricesA+5) # in second place, the CS prices --> identified as CS by adding 5


v<-5
e<-1

#start

pA<-2
pB<-2
pC<-2

P<-c(pA,"A")
mu=0.5
lambda=0.2

# SIMULATION

T<-200
i<-1
while(i<=T){
    pB<-BRB(pA,pB,pC)
    P<-rbind(P,c(pB,"B"))
    pC<-BRC(pA,pB,pC)
    P<-rbind(P,c(pC,"C"))
    pA<-BRA(pA,pB,pC)
    P<-rbind(P,c(pA,"A"))
i<-i+1
}

P2<-cbind(as.numeric(as.character(P[,1])),P[,2])
st<-(P[,1]>5)

P2<-matrix(P2,ncol=2)
p<-cbind(P2,st,as.numeric(P2[,1])-5*st)
p<-matrix(p,ncol = 4)

P2<-cbind(as.numeric(seq(1,T+1,1)),p)
P3<-matrix(P2,ncol = 5)
P3<-data.frame(P3)
P3$X5 <- as.numeric(as.character(P3$X5))

P<-data.frame(P3)

qplot(P[,1], P[,5], data=P, colour=P[,3])

P30<-P[50:200,]
P30[,4]<-as.numeric(P30[,4])-1
P30[,4]<-P30[,4]+as.numeric(P30[,3]=="A")

P30<-data.frame(P30)

names(P30) <- c("time","p","firm","format","prices")

P30$format <- as.factor(P30$format)
levels(P30$format) <- c("own", "format A")

#P30$time is a factor -> BAD
P30$time <- as.numeric(as.character(P30$time)) # -> now it is numeric

# graph in paper (depends on random draw of turns to best reply)
ggplot(P30, aes(x=time, y=prices,group=1, colour=format,shape=firm)) + 
  geom_point(size=4) + 
  geom_line(color='steelblue',size=1, alpha=0.3) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")+
  xlab("period")+
  scale_x_continuous(breaks = seq(50,200,10))+
  scale_shape_manual(values=LETTERS, guide=FALSE)
  
ggsave("Figure_BRF.pdf", width = 12, height = 6, units = "in", dpi = 400)


###############################################################################################################
# RUNNING SIMULATIONS FOR COMPUTATION OF AVERAGE PROFIT BY CONDITION; WITH RANDOM SEQUENCE OF BR AND LATENCY
###############################################################################################################

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
    
    J<-100 # number of different starting prices to go through
    T<-200 # number of periods after given starting price
    
    P<-matrix(-1,ncol=3,nrow=J*T+1) #matrix to record prices over time
    P[1,]<-c(0,0,0)
    j<-1
    while(j<=J){ 
      
      pA<-c(rdistA(1)) #random starting prices
      pB<-c(rdistBC(1))
      pC<-c(rdistBC(1))

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

    P    
    P2<-matrix(-1,ncol=3,nrow=J*T/2) #matrix without half (starting) periods
    P2
    j<-1
    while(j<=J){
      i<-T/2+1
      while(i<=T){
        P2[(j-1)*T/2+i-T/2,]<-P[(j-1)*T+i,]
        i<-i+1
        }
      j<-j+1
      }
    j 
    i
    P2   
    P  
    t<-apply(X=P2,MARGIN=1,FUN=fbb)
    
    profitA<-mean(t[1,])
    profitBC<-mean(t[2:3,])
    format<-mean(P2[,2:3]>5)
    formatB<-mean(P2[,2]>5)
    formatC<-mean(P2[,3]>5)
    
    profit<-rbind(profit,c(mu,lambda,profitA,profitBC,format,formatB,formatC))
  }}

profitABC<-cbind(profit,(profit[,3]+2*profit[,4])/3)
profitABC

if(table_param==1){
write.csv(profitABC, file="profitABC_table_param.csv")
  }else{
  write.csv(profitABC, file="profitABC_experiment_param.csv")}
