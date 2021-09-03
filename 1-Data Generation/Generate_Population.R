### Generate Populations for Simulation Study (based on plan from 03/23/17) ###

####### generate population (design variables) #######

rm(list=ls())
set.seed(22000)

hisp.prop=rep(c(.2,.2125,.2975,.2975),times=2)  # proportion of HHs within each stratum with Hispanic surname & in target population
other.prop=rep(c(.15,.225,.26,.2925),times=2)   # proportion of HHs within each stratum with other surname & in target population
Nbg.strat=rep(c(58,21,130,167),times=2)         # number of BGs within each stratum
# Number of BGs in each stratum: 1: 58, 2: 21, 3: 130, 4: 167, 5: 58, 6: 21, 7: 130, 8: 167

Nbg=sum(Nbg.strat)
bg.size.temp=1+round(rexp(Nbg,1/450))    # at least 1 HH per BG, mean number of HHs/BG=451 -- this is number of ALL HHs in each BG (including non-eligible HHs)
num.hisp.strat=round(c(bg.size.temp[1:Nbg.strat[1]]*hisp.prop[1],bg.size.temp[(Nbg.strat[1]+1):sum(Nbg.strat[1:2])]*hisp.prop[2],bg.size.temp[(sum(Nbg.strat[1:2])+1):sum(Nbg.strat[1:3])]*hisp.prop[3],bg.size.temp[(sum(Nbg.strat[1:3])+1):sum(Nbg.strat[1:4])]*hisp.prop[4],
                       bg.size.temp[(sum(Nbg.strat[1:4])+1):sum(Nbg.strat[1:5])]*hisp.prop[5],bg.size.temp[(sum(Nbg.strat[1:5])+1):sum(Nbg.strat[1:6])]*hisp.prop[6],bg.size.temp[(sum(Nbg.strat[1:6])+1):sum(Nbg.strat[1:7])]*hisp.prop[7],bg.size.temp[(sum(Nbg.strat[1:7])+1):sum(Nbg.strat[1:8])]*hisp.prop[8])) # number of eligible HHs with Hispanic surname in each BG
num.other.strat=round(c(bg.size.temp[1:Nbg.strat[1]]*other.prop[1],bg.size.temp[(Nbg.strat[1]+1):sum(Nbg.strat[1:2])]*other.prop[2],bg.size.temp[(sum(Nbg.strat[1:2])+1):sum(Nbg.strat[1:3])]*other.prop[3],bg.size.temp[(sum(Nbg.strat[1:3])+1):sum(Nbg.strat[1:4])]*other.prop[4],
                       bg.size.temp[(sum(Nbg.strat[1:4])+1):sum(Nbg.strat[1:5])]*other.prop[5],bg.size.temp[(sum(Nbg.strat[1:5])+1):sum(Nbg.strat[1:6])]*other.prop[6],bg.size.temp[(sum(Nbg.strat[1:6])+1):sum(Nbg.strat[1:7])]*other.prop[7],bg.size.temp[(sum(Nbg.strat[1:7])+1):sum(Nbg.strat[1:8])]*other.prop[8])) # number of eligible HHs with other surname in each BG
bg.size=num.hisp.strat+num.other.strat  # number of eligible HHs in each BG
Nhh=sum(bg.size)                        # number of HHs in target population
hh.size=1+rpois(Nhh,1)                  # at least 1 subject per HH, mean number of subjects/HH=2
N=sum(hh.size)                          # number of subjects in target population

A=matrix(rep(NA,times=max(bg.size)*length(bg.size)),nrow=max(bg.size),ncol=length(bg.size))
for (i in 1:length(bg.size)){
  A[,i]=c(rep(TRUE,times=num.hisp.strat[i]),rep(FALSE,times=num.other.strat[i]),rep(NA,times=max(bg.size)-bg.size[i]))    # create matrix A with each column corresponding to a BG (containing 1's for each Hispanic HH followed by 0's for each other HH)
}
hisp.strat.hh=na.omit(c(A))   #indicator for Hispanic surname (one entry per HH)

BGid=rep(rep(rep(1:Nbg, times=bg.size), times=hh.size), times=2)  # all ID's unique (e.g., subid=k only for one subject within one HH)
hhid=rep(rep(1:Nhh, times=hh.size), times=2)
subid=rep(1:N, times=2)
v.num=rep(c(1,2),each=N)
strat=1+(BGid>Nbg.strat[1])+(BGid>sum(Nbg.strat[1:2]))+(BGid>sum(Nbg.strat[1:3]))+(BGid>sum(Nbg.strat[1:4]))+(BGid>sum(Nbg.strat[1:5]))+(BGid>sum(Nbg.strat[1:6]))+(BGid>sum(Nbg.strat[1:7]))
hisp.strat=hisp.strat.hh[hhid]

age.inrange=FALSE
age=rep(0,N)            #create age vector with all 0's (so all values will be replaced in first iteration of while loop)
while(!age.inrange){
  age[age<18|age>74]=rnorm(length(age[age<18|age>74]),40,15)    #only generate new values for age to replace out of range values
  #check for success
  age.inrange=(sum(age>=18 & age<=74)==N)   #age.inrange=TRUE if all N subjects have 18<=age<=74 (which would break the loop)
}
age.base=rep(age,times=2)   #create baseline age (which is the same at V1 and V2)
age.strat=(age.base>=45)    #indicator for older (45-74 years) stratum


######### generate population outcome and covariates #########

age.strat.unq=age.strat[v.num==1]
hisp.strat.unq=hisp.strat[v.num==1]
strat.unq=strat[v.num==1]

### generate covariates ###
x0=rep(1,N)
x1=rbinom(N,1,.5)
x2=rbinom(N,1,.67)
x3=rbinom(N,1,.2)
x4=rbinom(N,1,.2)

x6.inrange=FALSE
x6=rep(-1,N)            #create x6 vector with all -1's (so all values will be replaced in first iteration of while loop)
while(!x6.inrange){
  x6[x6<3|x6>9]=rnorm(length(x6[x6<3|x6>9]),6,.5)    #only generate new values for x6 to replace out of range values
  #check for success
  x6.inrange=(sum(x6>=3 & x6<=9)==N)   #x6.inrange=TRUE if all N subjects have 3<=x6<=9 (which would break the loop)
}

x8.inrange=FALSE
x8=rep(-1,N)            #create x8 vector with all -1's (so all values will be replaced in first iteration of while loop)
while(!x8.inrange){
  x8[x8<0]=rnorm(length(x8[x8<0]),4.5,sqrt(.17))    #only generate new values for x8 to replace out of range values
  #check for success
  x8.inrange=(sum(x8>=0)==N)   #x8.inrange=TRUE if all N subjects have x6>=0 (which would break the loop)
}

x12=rbinom(N,1,.5)
x13=rbinom(N,1,.3)
x14=rbinom(N,1,.25)

x15.inrange=FALSE
x15=rep(25,N)            #create x15 vector with all 25's (so all values will be replaced in first iteration of while loop)
while(!x15.inrange){
  x15[x15<0|x15>24]=rnorm(length(x15[x15<0|x15>24]),2,sqrt(6.5))    #only generate new values for x15 to replace out of range values
  #check for success
  x15.inrange=(sum(x15>=0 & x15<=24)==N)   #x15.inrange=TRUE if all N subjects have 0<=x5<=24 (which would break the loop)
}

x17=rnorm(N,4.5+1*age.strat.unq+.1*(strat.unq %in% c(1,5))+.2*(strat.unq %in% c(2,6))+.3*(strat.unq %in% c(3,7))-1*hisp.strat.unq,.1)
x18=rbinom(N,1,exp(1.5-.06*age+1*(strat.unq %in% c(1,5))+.5*(strat.unq %in% c(2,6))+1*(strat.unq %in% c(3,7))-1*hisp.strat.unq)/(1+exp(1.5-.06*age+1*(strat.unq %in% c(1,5))+.5*(strat.unq %in% c(2,6))+1*(strat.unq %in% c(3,7))-1*hisp.strat.unq)))

x.v1=cbind(x0,x1,x2,x3,x4,rep(0,N),x8,x12,x13,x14,x15,x17,x18)
x.v2=cbind(x0,x1,x2,x3,x4,x6,x8,x12,x13,x14,x15,x17,x18)
x=rbind(x.v1,x.v2)                    #all observations from V1 are stacked on top of all observations from V2
colnames(x)=c("x0","x1","x2","x3","x4","x6","x8","x12","x13","x14","x15","x17","x18")

### generate continuous outcomes ###
gen.y=function(beta.v1,bgvar.v1,hhvar.v1,evar.v1,xmat.v1,beta.diff,bgvar.diff,hhvar.diff,evar.diff,xmat.diff){
  # IMPORTANT NOTE: the coefficient for baseline Y should be at the END of beta.diff
  ### generate baseline continuous y ###
  e.con1=rnorm(N,0,sqrt(evar.v1))  # generate visit 2 error term
  b1bg.unique.con1=rnorm(Nbg,0,sqrt(bgvar.v1))    # generate random intercept for BG cluster
  b1bg.con1=rep(rep(b1bg.unique.con1, times=bg.size), times=hh.size)    # replicate b1 so that all subjects from the same HH have the same b1bg
  b1hh.unique.con1=rnorm(Nhh,0,sqrt(hhvar.v1))   # generate random intercept for HH cluster
  b1hh.con1=rep(b1hh.unique.con1, times=hh.size)  # replicate b1 so that all subjects from the same HH have the same b1hh
  y.con1=xmat.v1%*%beta.v1+b1bg.con1+b1hh.con1+e.con1   # baseline continuous response y
  
  ### generate continuous y difference ###
  e.diff=rnorm(N,0,sqrt(evar.diff))  # generate visit 2 error term
  b1bg.unique.diff=rnorm(Nbg,0,sqrt(bgvar.diff))    # generate random intercept for BG cluster
  b1bg.diff=rep(rep(b1bg.unique.con1, times=bg.size), times=hh.size)    # replicate b1 so that all subjects from the same HH have the same b1bg
  b1hh.unique.diff=rnorm(Nhh,0,sqrt(hhvar.diff))   # generate random intercept for HH cluster
  b1hh.diff=rep(b1hh.unique.diff, times=hh.size)  # replicate b1 so that all subjects from the same HH have the same b1hh
  y.diff=cbind(xmat.diff,y.con1)%*%beta.diff+b1bg.diff+b1hh.diff+e.diff   # continuous y difference
  return(c(y.con1,y.diff))  # continuous y: visit 1 value is baseline y, visit 2 value is y difference between visits
}
y.con.bmi.v4=gen.y(beta.v1=c(30,-1,-.7,-1,2,-.8,.03,.4,-.04,-1.5,1.7,.7,-.6,1.8),
                   bgvar.v1=0,hhvar.v1=7,evar.v1=32,
                   xmat.v1=cbind(x0,x1,x2,x3,x4,x15,age,(strat.unq %in% c(1,5)),(strat.unq %in% c(2,6)),(strat.unq %in% c(3,7)),x2*(strat.unq %in% c(1,5)),x2*(strat.unq %in% c(2,6)),x2*(strat.unq %in% c(3,7)),x15*age.strat.unq),
                   beta.diff=c(8,4,.1,-7.5,-6,-.9,-.1,7,-.02,1,-.05,-.7,-.02,1,1,.1,.01,-1,.1,-.1,1,0,-.3,1.3,-.2),
                   bgvar.diff=.2,hhvar.diff=.3,evar.diff=9,
                   xmat.diff=cbind(x0,x1,x2,x3,x4,x15,age,(strat.unq %in% c(1,5)),(strat.unq %in% c(2,6)),(strat.unq %in% c(3,7)),x6,x6*x1,x6*x2,x6*x3,x6*x4,x6*x15,x6*age,x6*(strat.unq %in% c(1,5)),x6*(strat.unq %in% c(2,6)),x6*(strat.unq %in% c(3,7)),x2*(strat.unq %in% c(1,5)),x2*(strat.unq %in% c(2,6)),x2*(strat.unq %in% c(3,7)),x15*age.strat.unq))
y.con.gfr.v3=gen.y(beta.v1=c(150, -0.17, -2.95, 4.87, -4, -0.85, 2.37, -6.38, -2.58,-1,1.2,.2,2.5),
                   bgvar.v1=3,hhvar.v1=30,evar.v1=120,
                   xmat.v1=cbind(x0,x12,x13,x14,x8,age,(strat.unq %in% c(1,5)),(strat.unq %in% c(2,6)),(strat.unq %in% c(3,7)),x12*(strat.unq %in% c(1,5)),x12*(strat.unq %in% c(2,6)),x12*(strat.unq %in% c(3,7)),x8*age.strat.unq),
                   beta.diff=c(19, 9.30, -7.53, 9.46, 2.6, 0.04, -6.87, 10.50, -3.19, 7.00,  -1.67, 0.95, -2.05, -0.26, -0.07, 1.36, -1.98, 0.68,-3.1,1.7,.7,-1.6, -0.40),
                   bgvar.diff=3,hhvar.diff=30,evar.diff=90,
                   xmat.diff=cbind(x0,x12,x13,x14,x8,age,(strat.unq %in% c(1,5)),(strat.unq %in% c(2,6)),(strat.unq %in% c(3,7)),x6,x6*x12,x6*x13,x6*x14,x6*x8,x6*age,x6*(strat.unq %in% c(1,5)),x6*(strat.unq %in% c(2,6)),x6*(strat.unq %in% c(3,7)),x12*(strat.unq %in% c(1,5)),x12*(strat.unq %in% c(2,6)),x12*(strat.unq %in% c(3,7)),x8*age.strat.unq))
y.con.gfr.v4=gen.y(beta.v1=c(150, -0.17, -2.95, 4.87, -4, -0.85, 2.37, -6.38, -2.58,-1,1.2,.2,2.5),
                   bgvar.v1=3,hhvar.v1=30,evar.v1=120,
                   xmat.v1=cbind(x0,x12,x18,x14,x17,age,(strat.unq %in% c(1,5)),(strat.unq %in% c(2,6)),(strat.unq %in% c(3,7)),x12*(strat.unq %in% c(1,5)),x12*(strat.unq %in% c(2,6)),x12*(strat.unq %in% c(3,7)),x17*age.strat.unq),
                   beta.diff=c(19, 9.30, -7.53, 9.46, 2.6, 0.04, -6.87, 10.50, -3.19, 7.00,  -1.67, 0.95, -2.05, -0.26, -0.07, 1.36, -1.98, 0.68,-3.1,1.7,.7,-1.6, -0.40),
                   bgvar.diff=3,hhvar.diff=30,evar.diff=90,
                   xmat.diff=cbind(x0,x12,x18,x14,x17,age,(strat.unq %in% c(1,5)),(strat.unq %in% c(2,6)),(strat.unq %in% c(3,7)),x6,x6*x12,x6*x18,x6*x14,x6*x17,x6*age,x6*(strat.unq %in% c(1,5)),x6*(strat.unq %in% c(2,6)),x6*(strat.unq %in% c(3,7)),x12*(strat.unq %in% c(1,5)),x12*(strat.unq %in% c(2,6)),x12*(strat.unq %in% c(3,7)),x17*age.strat.unq))

### generate binary outcomes ###
gen.bin=function(y.con,k){
  y.bin=rep(NA,N)
  y.bin[v.num==1]=1*(y.con[v.num==1]<k)
  y.bin[v.num==2]=1*(y.con[v.num==1]+y.con[v.num==2]<k)
  return(y.bin)
}
y.bin.gfr.low.v3=gen.bin(y.con.gfr.v3,90); y.bin.gfr.med.v3=gen.bin(y.con.gfr.v3,95); y.bin.gfr.hi.v3=gen.bin(y.con.gfr.v3,100);
y.bin.gfr.low.v4=gen.bin(y.con.gfr.v4,90); y.bin.gfr.med.v4=gen.bin(y.con.gfr.v4,95); y.bin.gfr.hi.v4=gen.bin(y.con.gfr.v4,100);

### generate baseline binary Y (based on logistic mixed model, V5) ###
x1.bin=cbind(x0,x12,x18,x14,x17,age,(strat.unq %in% c(1,5)),(strat.unq %in% c(2,6)),(strat.unq %in% c(3,7)),(strat.unq %in% c(1,5))*x12,(strat.unq %in% c(2,6))*x12,(strat.unq %in% c(3,7))*x12,age.strat.unq*x17)
beta1.bin=c(-7.74, 0.01, 0.45, -0.76, 0.62, 0.13, -0.34, 1, 0.39, 0.16, -0.12, -0.04, -0.39)
b1bg.unique.bin=rnorm(Nbg,0,sqrt(1))   # generate random intercept for BG cluster
b1bg.bin=rep(rep(b1bg.unique.bin, times=bg.size), times=hh.size)    # replicate b1 so that all data records from the same BG have the same b1bg
b1hh.unique.bin=rnorm(Nhh,0,sqrt(4))   # generate random intercept for HH cluster
b1hh.bin=rep(b1hh.unique.bin, times=hh.size)  # replicate b1 so that all data records from the same HH have the same b1hh
p1.bin=c(1/(1+exp(-c(x1.bin%*%beta1.bin)-b1bg.bin-b1hh.bin)))
y1.bin.v5=rbinom(N,1,p1.bin)

### generate V2 binary Y (based on logistic mixed model, V5) ###
x2.bin=cbind(x0,x12,x18,x14,x17,x6,age,(strat.unq %in% c(1,5)),(strat.unq %in% c(2,6)),(strat.unq %in% c(3,7)),(strat.unq %in% c(1,5))*x12,(strat.unq %in% c(2,6))*x12,(strat.unq %in% c(3,7))*x12,age.strat.unq*x17,
             y1.bin.v5,y1.bin.v5*cbind(x12,x18,x14,x17,x6,age,(strat.unq %in% c(1,5)),(strat.unq %in% c(2,6)),(strat.unq %in% c(3,7)),(strat.unq %in% c(1,5))*x12,(strat.unq %in% c(2,6))*x12,(strat.unq %in% c(3,7))*x12,age.strat.unq*x17))
beta2.bin=c(-4.56, 0.14, 0.44, 0.32, -0.07, -0.3, 0.1, -0.35, 0.22, -0.08, 0.7, -0.15, -0.15, 0.2, 2.1, -0.04, -0.04, -0.04, 0.05, -0.02, 0, 0.1, 0.25, 0.05, -0.1, -0.15, 0, -0.03)
b2bg.unique.bin=rnorm(Nbg,0,sqrt(1))   # generate random intercept for BG cluster
b2bg.bin=rep(rep(b2bg.unique.bin, times=bg.size), times=hh.size)    # replicate b1 so that all data records from the same BG have the same b1bg
b2hh.unique.bin=rnorm(Nhh,0,sqrt(4))   # generate random intercept for HH cluster
b2hh.bin=rep(b2hh.unique.bin, times=hh.size)  # replicate b1 so that all data records from the same HH have the same b1hh
p2.bin=c(1/(1+exp(-c(x2.bin%*%beta2.bin)-b2bg.bin-b2hh.bin)))
y.bin.v5=rbinom(N,1,p2.bin)
y.bin.gfr.hi.v5=c(y1.bin.v5,y.bin.v5)

### generate V2 Poisson Y (based on Poisson mixed model, V6) ###
x2.pois=cbind(x0,x12,x18,x14,x17,age,(strat.unq %in% c(1,5)),(strat.unq %in% c(2,6)),(strat.unq %in% c(3,7)),(strat.unq %in% c(1,5))*x12,(strat.unq %in% c(2,6))*x12,(strat.unq %in% c(3,7))*x12,age.strat.unq*x17,
             y1.bin.v5,y1.bin.v5*cbind(x12,x18,x14,x17,age,(strat.unq %in% c(1,5)),(strat.unq %in% c(2,6)),(strat.unq %in% c(3,7)),(strat.unq %in% c(1,5))*x12,(strat.unq %in% c(2,6))*x12,(strat.unq %in% c(3,7))*x12,age.strat.unq*x17))
beta2.pois=c(-6.08, 0.08, 0.25, 0.18, -0.1, 0.05, -0.2, 0.1, -0.05, 0.4, -0.07, -0.07, 0.2, 2.6, -0.05, -0.15, -0.11, 0.05, -0.03, 0.13, 0.01, 0.04, -0.26, 0, 0.03, -0.11)
b2bg.unique.pois=rnorm(Nbg,0,sqrt(1))   # generate random intercept for BG cluster
b2bg.pois=rep(rep(b2bg.unique.pois, times=bg.size), times=hh.size)    # replicate b1 so that all data records from the same BG have the same b1bg
b2hh.unique.pois=rnorm(Nhh,0,sqrt(4))   # generate random intercept for HH cluster
b2hh.pois=rep(b2hh.unique.pois, times=hh.size)  # replicate b1 so that all data records from the same HH have the same b1hh
exp2.pois=c(exp(c(x2.pois%*%beta2.pois)+log(x6)+b2bg.pois+b2hh.pois))
y.bin.v6=rpois(N,exp2.pois)
y.bin.gfr.hi.v6=c(y1.bin.v5,y.bin.v6)

pop=data.frame(strat,BGid,hhid,subid,v.num,hisp.strat,age.base,age.strat,x,y.con.bmi.v4,y.con.gfr.v3,y.bin.gfr.low.v3,y.bin.gfr.med.v3,y.bin.gfr.hi.v3,y.con.gfr.v4,y.bin.gfr.low.v4,y.bin.gfr.med.v4,y.bin.gfr.hi.v4,y.bin.gfr.hi.v5,y.bin.gfr.hi.v6)
pop=pop[order(pop$subid,pop$v.num),]

### save data ##
write.csv(pop,"D:/hard drive material/CSCC research/Population migration project/04_Simulated_data/newdat/population.csv", row.names=FALSE)
#write.csv(pop,"C:/Users/haoli/Desktop/CSCC research/04_Simulated_data/population.csv", row.names=FALSE)