#profit function

profit_ftn<-function(p){
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
}

# INITIALIZATION

pricesA<-seq(from=0.1, to=5,by=0.1)
pricesBC<-c(pricesA,pricesA+5) # in second place, the CS prices --> identified as CS by more than 5

library(distr)

v<-5
e<-1
vmu<-c(0,0.3,0.7,1)
#vmu<-c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
vlambda<-c(0,0.3,100)

# LOOPING

for(mu in unique(vmu)){
  for(lambda in unique(vlambda)){
    print(mu)
    print(lambda)
# initialize distributions!
  distA <-DiscreteDistribution(supp = pricesA, prob = rep(1/length(pricesA),length(pricesA)))
  distBC <-DiscreteDistribution(supp = pricesBC, prob = rep(1/length(pricesBC),length(pricesBC)))

# if no CS at start
# distBC <-DiscreteDistribution(supp = pricesBC, prob = c(rep(1/length(pricesA),length(pricesA)),rep(0,length(pricesA))) ) 
# if start from point distribution
#distA <-DiscreteDistribution(supp = pricesA, prob = c(1,rep(0,length(pricesA)-1)))
#distBC <-DiscreteDistribution(supp = pricesBC, prob = c(1,rep(0,length(pricesBC)-1)))

  rdistA <- r(distA)                 # function to create random variates from p
  rdistBC <- r(distBC)

# initialize storage of results
  gA<-pricesA
  gBC<-pricesBC
  profitA<-0
  profitBC<-0
  formatA_BC<-0
  

# rounds of learning iterations
  i<-1
  N<-10000
  while(i<=100){
    set.seed(1)                      # for reproduceable example
    #print(i)

    pA<-c(rdistA(N))
    pA<-c(pA,pricesA,pricesA) # guarantee full support
    pB<-c(rdistBC(N))
    pB<-c(pB,pricesBC)
    pC<-c(rdistBC(N))
    pC<-c(pC,pricesBC)

    stA<-(pA<=5)
    stB<-(pB>5)
    stC<-(pC>5)

    p<-cbind(pA,pB-5*stB,pC-5*stC,stA,stB,stC)
    p<-matrix(p,ncol = 6)

    t<-apply(X=p,MARGIN=1,FUN=profit_ftn) # computing  profit by level of prices

    fA<-cbind(c(t(p[,1])),c(t(p[,4])),c(t[1,]))
    fBC<-cbind(c(t(p[,2:3])),c(t(p[,5:6])),c(t[2:3,]))
    fBC<-cbind(fBC[,1]+5*fBC[,2],fBC)

    mean_profitA<-tapply(X=fA[,3],INDEX=fA[,1],FUN=mean) # mean profit by price
    mean_profitBC<-tapply(X=fBC[,4],INDEX=fBC[,1],FUN=mean)

    p_adjustA<-mean_profitA/sum(mean_profitA)
    p_adjustBC<-mean_profitBC/sum(mean_profitBC)

    # giving results as they go
    profit_A<-sum(p_adjustA*mean_profitA) # expected profit A
    profit_BC<-sum(p_adjustBC*mean_profitBC)
    format_A_BC<-sum(p_adjustBC[51:100]) # percentage CS

    gA<-cbind(gA,p_adjustA)
    gBC<-cbind(gBC,p_adjustBC)
    profitA<-cbind(profitA,profit_A)
    profitBC<-cbind(profitBC,profit_BC)
    formatA_BC<-cbind(formatA_BC,format_A_BC)

    distA <-DiscreteDistribution(supp = pricesA, prob = c(p_adjustA))
    distBC <-DiscreteDistribution(supp = pricesBC, prob = c(p_adjustBC))

    rdistA<- r(distA)                 
    rdistBC <- r(distBC)              

    i<-i+1
    }

#storage
  nam <- paste("resultsA_", mu,"_", lambda, sep = "")
  assign(nam,cbind(gA))
  nam <- paste("resultsBC_", mu,"_", lambda, sep = "")
  assign(nam,cbind(gBC))
  nam <- paste("profitA_", mu,"_", lambda, sep = "")
  assign(nam,cbind(profitA))
  nam <- paste("profitBC_", mu,"_", lambda, sep = "")
  assign(nam,cbind(profitBC))
  nam <- paste("formatA_BC_", mu,"_", lambda, sep = "")
  assign(nam,cbind(formatA_BC))
  
  }
}

# make table of results
profits_name<-NA
tab_profitsA<-NA
tab_profitsBC<-NA
tab_formats<-NA

for(mu in unique(vmu)){
  for(lambda in unique(vlambda)){
    profits_A<-paste("profitA",mu,lambda,sep="_")
    profits_BC<-paste("profitBC",mu,lambda,sep="_")
    format_name<-paste("formatA_BC",mu,lambda,sep="_")
    vecA<-get(profits_A)
    vecBC<-get(profits_BC)
    vec_f<-get(format_name)
    profitsA<-mean(vecA[,95:100])
    profitsBC<-mean(vecBC[,95:100])
    formats<-mean(vec_f[,95:100])
    tab_profitsA<-rbind(tab_profitsA,c(mu,lambda,profitsA))
    tab_profitsBC<-rbind(tab_profitsBC,c(mu,lambda,profitsBC))
    tab_formats<-rbind(tab_formats,c(mu,lambda,formats))
  }}

tab_profitsA
tab_profitsBC    
tab_formats


# giving out results, by stage in evolution of probabilities of each prices

f = function(expr) eval(parse(text=expr))
for(mu in unique(vmu)){
  for(lambda in unique(vlambda)){
    print("mu is");print(mu)
    print("lambda is");print(lambda)
    j<-paste("results_", mu,"_", lambda, sep = "")
    print(sum(f(j)[51:100,1]))
    print(sum(f(j)[51:100,2]))
    print(sum(f(j)[51:100,10]))
    print(sum(f(j)[51:100,15]))
  }
}

# graphical representation of distributions and their evolution over time

library(ggplot2)
a<-resultsBC_0.7_100
dim(a)
price=rep(a[1:50,1],6)
#init_proba<- rep(1/length(pricesBC),length(pricesBC))
cdf = c(cumsum(a[,2]),cumsum(a[,5]),cumsum(a[,31]))
label=c(rep("NCS t=02",50),rep("CS t=02",50),rep("NCS t=04",50),rep("CS t=04",50),rep("NCS t=30",50),rep("CS t=30",50))

prices<-c(as.numeric(a[1:50,1]))
proba1<-c(as.numeric(a[1:50,31]))
sum(proba1*prices)/sum(proba1) # average price when own format

proba2<-c(as.numeric(a[51:100,31]))
sum(proba2*prices)/sum(proba2) # average price when format A
df <- data.frame(x=price,y = cdf,z=label)

qplot(price, cdf, data=df, group=label, geom="line") +  geom_point(size = 5, aes(shape=label, colour = label)) 
