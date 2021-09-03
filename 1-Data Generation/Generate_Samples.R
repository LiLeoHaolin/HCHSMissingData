### Generate Samples for Simulation Study (based on plan from 10/14/16) ###

####### function to generate sample #######
samp.gen = function(pop,prob.bg,num.bg,prob.hh.hisp,prob.hh.other,prob.age){
  
  pop.unq=pop[pop$v.num==2,]   #this is the population only including visit 2 records (to simplify sampling)  
  pop.v1=pop[pop$v.num==1,]
  
  ### re-create hh.size ###
  hh.list=unique(pop.unq[,c("BGid","hhid")])  # list of BGid & hhid for each unique hhid
  bg.size=table(hh.list$BGid)   # number of HHs per BG, 
  hh.size=table(pop.unq$hhid)   # number of subjects per HH
  hisp.strat.unq=aggregate(hisp.strat~hhid,pop.unq,mean)$hisp.strat   # hisp.strat with 1 record per HH
  
  ### generate raw weights (these do not depend on the sample (only depend on sampling probabilities)) ###
  pop.unq$W_bg=1/prob.bg[pop.unq$strat]   # BG stage raw weight (based on BG sampling fraction; 1/sampling fraction for that stratum)
  #pop.unq$W_hh=ifelse(pop.unq$hhid %in% hh.list$hhid[hisp.strat.unq],1/prob.hh.hisp[pop.unq$strat],1/prob.hh.other[pop.unq$strat])   #HH stage raw weight
  pop.unq$W_hh=ifelse(pop.unq$hisp.strat,1/prob.hh.hisp[pop.unq$strat],1/prob.hh.other[pop.unq$strat])   #HH stage raw weight
  pop.unq$W_sub=ifelse(pop.unq$age.strat,1/prob.age[2],1/prob.age[1])    # subject stage raw weight
  pop.unq$W_bghhsub=pop.unq$W_bg*pop.unq$W_hh*pop.unq$W_sub     # raw combined weights
  
  ### select random sample from population ###
  #select stratified random sample of BGs from pop & save list of BGs
  bg.select=c(sample(1:58,round(prob.bg[1]*num.bg[1])),sample(59:79,round(prob.bg[2]*num.bg[2])),sample(80:209,round(prob.bg[3]*num.bg[3])),sample(210:376,round(prob.bg[4]*num.bg[4])),sample(377:434,round(prob.bg[5]*num.bg[5])),sample(435:455,round(prob.bg[6]*num.bg[6])),sample(456:585,round(prob.bg[7]*num.bg[7])),sample(586:752,round(prob.bg[8]*num.bg[8])))
  bg.select.s=1*(bg.select<=58)+2*(bg.select>=59 & bg.select<=79)+3*(bg.select>=80 & bg.select<=209)+4*(bg.select>=210 & bg.select<=376)+5*(bg.select>=377 & bg.select<=434)+6*(bg.select>=435 & bg.select<=455)+7*(bg.select>=456 & bg.select<=585)+8*(bg.select>=586 & bg.select<=752)  #stratum for each BG
  
  #select stratified random sample of HHs from each selected BG & save indicator of HH selection for all subjects within selected HHs
  samp.hh.bystrat=function(s){
    hh.select.s=rep(FALSE,dim(pop.unq[pop.unq$strat==s,])[1])         #initially set hh.select.s=FALSE for all subjects in stratum s, so that unselected BG's will be set to FALSE
    for (j in bg.select[bg.select.s==s]){
      hh.list.hisp=hh.list$hhid[hh.list$BGid==j & hisp.strat.unq==TRUE]   #list of unique HHs in BGid j, w/ Hisp surname
      hh.list.other=hh.list$hhid[hh.list$BGid==j & hisp.strat.unq==FALSE] #list of unique HHs in BGid j, w/ other surname
      hh.select.hisp=sample(hh.list.hisp,round(prob.hh.hisp[s]*length(hh.list.hisp)))      #list of randomly sampled HHs in BGid w/ Hispanic surname
      hh.select.other=sample(hh.list.other,round(prob.hh.other[s]*length(hh.list.other)))  #list of randomly sampled HHs in BGid w/ other surname
      hh.select.s[pop.unq$hhid[pop.unq$strat==s] %in% c(hh.select.hisp,hh.select.other)]=TRUE  #select subjects from randomly sampled HHs in BGid j (one entry per subject in BGid j)
    }
    return(hh.select.s)
  }
  hh.select=c(samp.hh.bystrat(1),samp.hh.bystrat(2),samp.hh.bystrat(3),samp.hh.bystrat(4),samp.hh.bystrat(5),samp.hh.bystrat(6),samp.hh.bystrat(7),samp.hh.bystrat(8))  #indicator of HH selection
  
  #select random sample of subjects from each selected HH & save indicator of subject selection
  sub.select=rep(FALSE,dim(pop.unq)[1])           # initially set sub.select=FALSE for all subjects, so that unselected HH's will be set to FALSE
  sub.select[pop.unq$subid %in% sample(pop.unq$subid[!pop.unq$age.strat & hh.select],round(prob.age[1]*dim(pop.unq[!pop.unq$age.strat & hh.select,])[1]))]=TRUE   # randomly sample younger subjects among sampled HH's
  sub.select[pop.unq$subid %in% sample(pop.unq$subid[pop.unq$age.strat & hh.select],round(prob.age[2]*dim(pop.unq[pop.unq$age.strat & hh.select,])[1]))]=TRUE     # randomly sample older subjects among sampled HH's
  
  samp=pop.unq[sub.select,]   # create sample by restricting pop to selected subjects
  samp$y1_con_bmi_v4=pop.v1$y.con.bmi.v4[sub.select]; samp$y1_con_gfr_v3=pop.v1$y.con.gfr.v3[sub.select]; samp$y1_bin_gfr_low_v3=pop.v1$y.bin.gfr.low.v3[sub.select]; samp$y1_bin_gfr_med_v3=pop.v1$y.bin.gfr.med.v3[sub.select]; samp$y1_bin_gfr_hi_v3=pop.v1$y.bin.gfr.hi.v3[sub.select];
  samp$y1_con_gfr_v4=pop.v1$y.con.gfr.v4[sub.select]; samp$y1_bin_gfr_low_v4=pop.v1$y.bin.gfr.low.v4[sub.select]; samp$y1_bin_gfr_med_v4=pop.v1$y.bin.gfr.med.v4[sub.select]; samp$y1_bin_gfr_hi_v4=pop.v1$y.bin.gfr.hi.v4[sub.select];
  samp$y1_bin_gfr_hi_v5=pop.v1$y.bin.gfr.hi.v5[sub.select];  samp$y1_bin_gfr_hi_v6=pop.v1$y.bin.gfr.hi.v6[sub.select];
  names(samp)[names(samp) %in% c('v.num','hisp.strat','age.base','age.strat','y.con.bmi.v4','y.con.gfr.v3','y.bin.gfr.low.v3','y.bin.gfr.med.v3','y.bin.gfr.hi.v3','y.con.gfr.v4','y.bin.gfr.low.v4','y.bin.gfr.med.v4','y.bin.gfr.hi.v4','y.bin.gfr.hi.v5','y.bin.gfr.hi.v6')]=c('v_num','hisp_strat','age_base','age_strat','y_con_bmi_v4','y_con_gfr_v3','y_bin_gfr_low_v3','y_bin_gfr_med_v3','y_bin_gfr_hi_v3','y_con_gfr_v4','y_bin_gfr_low_v4','y_bin_gfr_med_v4','y_bin_gfr_hi_v4','y_bin_gfr_hi_v5','y_bin_gfr_hi_v6')
  
  # generate normalized weight
  samp$bghhsub_s2=samp$W_bghhsub/mean(samp$W_bghhsub)    # normalized weight (raw combined weight/mean combined weight)
  
  # scaled subject weights
  sumw.sub=aggregate(W_sub ~ BGid + hhid, dat=samp, FUN=sum); sumw.sub=sumw.sub[order(sumw.sub$BGid,sumw.sub$hhid),]
  sumw.sub.2=aggregate(W_sub^2 ~ BGid + hhid, dat=samp, FUN=sum); sumw.sub.2=sumw.sub.2[order(sumw.sub.2$BGid,sumw.sub.2$hhid),]
  nw.sub=aggregate(W_sub ~ BGid + hhid, dat=samp, FUN=length); nw.sub=nw.sub[order(nw.sub$BGid,nw.sub$hhid),]
  
  samp$sub_s1=samp$W_sub*rep(sumw.sub$W_sub, times=nw.sub$W_sub)/rep(sumw.sub.2$"W_sub^2", times=nw.sub$W_sub)
  samp$sub_s2=samp$W_sub*rep(nw.sub$W_sub, times=nw.sub$W_sub)/rep(sumw.sub$W_sub, times=nw.sub$W_sub)
  
  # scaled HH weights
  samp.unqhh=samp[!duplicated(samp$hhid),]    #keep one row per HH
  sumw.hh=aggregate(W_hh ~ BGid, dat=samp.unqhh, FUN=sum)
  sumw.hh.2=aggregate(W_hh^2 ~ BGid, dat=samp.unqhh, FUN=sum)
  nw.hh=aggregate(W_hh ~ BGid, dat=samp.unqhh, FUN=length)
  
  samp$hh_s1=samp$W_hh*merge(samp[,c('BGid','hhid','subid')],sumw.hh,by='BGid',all=TRUE)$W_hh/merge(samp[,c('BGid','hhid','subid')],sumw.hh.2,by='BGid',all=TRUE)$'W_hh^2'
  samp$hh_s2=samp$W_hh*merge(samp[,c('BGid','hhid','subid')],nw.hh,by='BGid',all=TRUE)$W_hh/merge(samp[,c('BGid','hhid','subid')],sumw.hh,by='BGid',all=TRUE)$W_hh
  
  
  ### generate missing outcome indicators ###
  miss_rate=as.matrix(c(0.02,0.05,0.1,0.2,0.3))
  y_mcar=apply(miss_rate,1,function(x) rbinom(dim(samp)[1],1,x))
  
  gen.miss.y=function(y1,y2,theta0,thetax,thetay){
    theta=as.matrix(rbind(theta0,matrix(thetax,nrow=6,ncol=5,byrow=FALSE),rep(thetay,5)))
    xb=as.matrix(cbind(samp$x0,samp$age_base,(samp$strat %in% c(1,5)),(samp$strat %in% c(2,6)),(samp$strat %in% c(3,7)),samp$x2,samp$x13,(y1+y2)))%*%theta
    p_miss=exp(xb)/(1+exp(xb))
    y_miss=apply(p_miss,2,function(x) rbinom(dim(samp)[1],1,x))
    return(y_miss)
  }
  y_mar_v5=gen.miss.y(y1=samp$y1_con_bmi_v4,y2=samp$y_con_bmi_v4,theta0=c(-.8,.3,1.1,2.1,2.9),thetax=c(-.1,.25,-.5,1,.2,-.5),thetay=0)
  y_mnar_bmi_low_v5=gen.miss.y(y1=samp$y1_con_bmi_v4,y2=samp$y_con_bmi_v4,theta0=c(-1.7,-.7,.1,1,1.7),thetax=c(-.1,.1,-1,.2,.1,-.25),thetay=.05)
  y_mnar_bmi_v5=gen.miss.y(y1=samp$y1_con_bmi_v4,y2=samp$y_con_bmi_v4,theta0=c(-3.3,-2.3,-1.5,-.5,.2),thetax=c(-.1,.1,-1,.2,.1,-.25),thetay=.1)
  y_mnar_bmi_hi_v5=gen.miss.y(y1=samp$y1_con_bmi_v4,y2=samp$y_con_bmi_v4,theta0=c(-5,-4,-3.1,-2.2,-1.4),thetax=c(-.1,.1,-1,.2,.1,-.25),thetay=.15)
  y_mnar_gfr_low_v5=gen.miss.y(y1=samp$y1_con_gfr_v3,y2=samp$y_con_gfr_v3,theta0=c(-5.1,-4,-3,-1.8,-1),thetax=c(.1,.8,-.1,1,.1,-.25),thetay=-.05)
  y_mnar_gfr_v5=gen.miss.y(y1=samp$y1_con_gfr_v3,y2=samp$y_con_gfr_v3,theta0=c(-1,.3,1.5,2.9,4),thetax=c(.1,.8,-.1,1,.1,-.25),thetay=-.1)
  y_mnar_gfr_hi_v5=gen.miss.y(y1=samp$y1_con_gfr_v3,y2=samp$y_con_gfr_v3,theta0=c(2.9,4.4,5.8,7.5,8.8),thetax=c(.1,.8,-.1,1,.1,-.25),thetay=-.15)
  
  # add missing outcome data indicators to dataset
  y_miss.mat=cbind(y_mcar,y_mar_v5,y_mnar_bmi_low_v5,y_mnar_bmi_v5,y_mnar_bmi_hi_v5,y_mnar_gfr_low_v5,y_mnar_gfr_v5,y_mnar_gfr_hi_v5)
  colnames(y_miss.mat)=c("y_mcar_2","y_mcar_5","y_mcar_10","y_mcar_20","y_mcar_30",
                         "y_mar_v5_2","y_mar_v5_5","y_mar_v5_10","y_mar_v5_20","y_mar_v5_30",
                         "y_mnar_bmi_low_v5_2","y_mnar_bmi_low_v5_5","y_mnar_bmi_low_v5_10","y_mnar_bmi_low_v5_20","y_mnar_bmi_low_v5_30",
                         "y_mnar_bmi_v5_2","y_mnar_bmi_v5_5","y_mnar_bmi_v5_10","y_mnar_bmi_v5_20","y_mnar_bmi_v5_30",
                         "y_mnar_bmi_hi_v5_2","y_mnar_bmi_hi_v5_5","y_mnar_bmi_hi_v5_10","y_mnar_bmi_hi_v5_20","y_mnar_bmi_hi_v5_30",
                         "y_mnar_gfr_low_v5_2","y_mnar_gfr_low_v5_5","y_mnar_gfr_low_v5_10","y_mnar_gfr_low_v5_20","y_mnar_gfr_low_v5_30",
                         "y_mnar_gfr_v5_2","y_mnar_gfr_v5_5","y_mnar_gfr_v5_10","y_mnar_gfr_v5_20","y_mnar_gfr_v5_30",
                         "y_mnar_gfr_hi_v5_2","y_mnar_gfr_hi_v5_5","y_mnar_gfr_hi_v5_10","y_mnar_gfr_hi_v5_20","y_mnar_gfr_hi_v5_30")
  samp=cbind(samp,y_miss.mat)
  
  # create "fine" non-response weights, where finer stratification is allowed to differ by dataset
  age.cat=factor(cut(samp$age_base,c(18,25,35,45,55,65,75),include.lowest=TRUE,right=FALSE),labels=1:6)
  hisp.strat=ifelse(samp$hisp_strat,'T','F')
  
  age.cat.new=age.cat
  age.cat.new[age.cat %in% c(1,2) & ((samp$strat %in% c(1,5) & samp$hisp_strat==FALSE) | samp$strat %in% c(2,6))]=1
  age.cat.new[age.cat %in% c(5,6) & ((samp$strat %in% c(1,5) & samp$hisp_strat==FALSE) | samp$strat %in% c(2,6))]=5
  
  hisp.strat.new=ifelse(samp$strat %in% c(2,6) & age.cat %in% c(1,2,5,6),'F',hisp.strat)
  x2.new=samp$x2
  x13.new=samp$x13
  
  test.strat=factor(interaction(samp$strat,hisp.strat.new,age.cat.new,x2.new,x13.new))
  
  test.strat.new=test.strat
  
  cell.size=c(table(test.strat.new[samp$y_mcar_30==0]),table(test.strat.new[samp$y_mar_v5_30==0]),
              table(test.strat.new[samp$y_mnar_bmi_low_v5_30==0]),table(test.strat.new[samp$y_mnar_bmi_v5_30==0]),table(test.strat.new[samp$y_mnar_bmi_hi_v5_30==0]),
              table(test.strat.new[samp$y_mnar_gfr_low_v5_30==0]),table(test.strat.new[samp$y_mnar_gfr_v5_30==0]),table(test.strat.new[samp$y_mnar_gfr_hi_v5_30==0]),
              table(test.strat.new[samp$y_mcar_20==0]),table(test.strat.new[samp$y_mar_v5_20==0]),
              table(test.strat.new[samp$y_mnar_bmi_low_v5_20==0]),table(test.strat.new[samp$y_mnar_bmi_v5_20==0]),table(test.strat.new[samp$y_mnar_bmi_hi_v5_20==0]),
              table(test.strat.new[samp$y_mnar_gfr_low_v5_20==0]),table(test.strat.new[samp$y_mnar_gfr_v5_20==0]),table(test.strat.new[samp$y_mnar_gfr_hi_v5_20==0]),
              table(test.strat.new[samp$y_mcar_10==0]),table(test.strat.new[samp$y_mar_v5_10==0]),
              table(test.strat.new[samp$y_mnar_bmi_low_v5_10==0]),table(test.strat.new[samp$y_mnar_bmi_v5_10==0]),table(test.strat.new[samp$y_mnar_bmi_hi_v5_10==0]),
              table(test.strat.new[samp$y_mnar_gfr_low_v5_10==0]),table(test.strat.new[samp$y_mnar_gfr_v5_10==0]),table(test.strat.new[samp$y_mnar_gfr_hi_v5_10==0]))
  if(min(cell.size)==0){
    for(j in names(cell.size)[cell.size==0]){
      hisp.strat.new=ifelse(samp$strat==substr(j,1,1) & age.cat.new==substr(j,5,5) & x2.new==substr(j,7,7) & x13.new==substr(j,9,9),'F',hisp.strat.new)
    }
    test.strat.new=factor(interaction(samp$strat,hisp.strat.new,age.cat.new,x2.new,x13.new))
    cell.size=c(table(test.strat.new[samp$y_mcar_30==0]),table(test.strat.new[samp$y_mar_v5_30==0]),
                table(test.strat.new[samp$y_mnar_bmi_low_v5_30==0]),table(test.strat.new[samp$y_mnar_bmi_v5_30==0]),table(test.strat.new[samp$y_mnar_bmi_hi_v5_30==0]),
                table(test.strat.new[samp$y_mnar_gfr_low_v5_30==0]),table(test.strat.new[samp$y_mnar_gfr_v5_30==0]),table(test.strat.new[samp$y_mnar_gfr_hi_v5_30==0]),
                table(test.strat.new[samp$y_mcar_20==0]),table(test.strat.new[samp$y_mar_v5_20==0]),
                table(test.strat.new[samp$y_mnar_bmi_low_v5_20==0]),table(test.strat.new[samp$y_mnar_bmi_v5_20==0]),table(test.strat.new[samp$y_mnar_bmi_hi_v5_20==0]),
                table(test.strat.new[samp$y_mnar_gfr_low_v5_20==0]),table(test.strat.new[samp$y_mnar_gfr_v5_20==0]),table(test.strat.new[samp$y_mnar_gfr_hi_v5_20==0]),
                table(test.strat.new[samp$y_mcar_10==0]),table(test.strat.new[samp$y_mar_v5_10==0]),
                table(test.strat.new[samp$y_mnar_bmi_low_v5_10==0]),table(test.strat.new[samp$y_mnar_bmi_v5_10==0]),table(test.strat.new[samp$y_mnar_bmi_hi_v5_10==0]),
                table(test.strat.new[samp$y_mnar_gfr_low_v5_10==0]),table(test.strat.new[samp$y_mnar_gfr_v5_10==0]),table(test.strat.new[samp$y_mnar_gfr_hi_v5_10==0]))
  }
  if(min(cell.size)==0){
    for(j in names(cell.size)[cell.size==0]){
      if(substr(j,5,5) %in% c(1,3,5)){
        age.cat.new=ifelse(samp$strat==substr(j,1,1) & hisp.strat.new==substr(j,3,3) & (age.cat.new %in% c(substr(j,5,5),as.numeric(substr(j,5,5))+1)) & x2.new==substr(j,7,7) & x13.new==substr(j,9,9),as.numeric(substr(j,5,5)),age.cat.new)
      }else if(substr(j,5,5) %in% c(2,4,6)){
        age.cat.new=ifelse(samp$strat==substr(j,1,1) & hisp.strat.new==substr(j,3,3) & (age.cat.new %in% c(as.numeric(substr(j,5,5))-1,substr(j,5,5))) & x2.new==substr(j,7,7) & x13.new==substr(j,9,9),as.numeric(substr(j,5,5))-1,age.cat.new)
      }
    }
    test.strat.new=factor(interaction(samp$strat,hisp.strat.new,age.cat.new,x2.new,x13.new))
    cell.size=c(table(test.strat.new[samp$y_mcar_30==0]),table(test.strat.new[samp$y_mar_v5_30==0]),
                table(test.strat.new[samp$y_mnar_bmi_low_v5_30==0]),table(test.strat.new[samp$y_mnar_bmi_v5_30==0]),table(test.strat.new[samp$y_mnar_bmi_hi_v5_30==0]),
                table(test.strat.new[samp$y_mnar_gfr_low_v5_30==0]),table(test.strat.new[samp$y_mnar_gfr_v5_30==0]),table(test.strat.new[samp$y_mnar_gfr_hi_v5_30==0]),
                table(test.strat.new[samp$y_mcar_20==0]),table(test.strat.new[samp$y_mar_v5_20==0]),
                table(test.strat.new[samp$y_mnar_bmi_low_v5_20==0]),table(test.strat.new[samp$y_mnar_bmi_v5_20==0]),table(test.strat.new[samp$y_mnar_bmi_hi_v5_20==0]),
                table(test.strat.new[samp$y_mnar_gfr_low_v5_20==0]),table(test.strat.new[samp$y_mnar_gfr_v5_20==0]),table(test.strat.new[samp$y_mnar_gfr_hi_v5_20==0]),
                table(test.strat.new[samp$y_mcar_10==0]),table(test.strat.new[samp$y_mar_v5_10==0]),
                table(test.strat.new[samp$y_mnar_bmi_low_v5_10==0]),table(test.strat.new[samp$y_mnar_bmi_v5_10==0]),table(test.strat.new[samp$y_mnar_bmi_hi_v5_10==0]),
                table(test.strat.new[samp$y_mnar_gfr_low_v5_10==0]),table(test.strat.new[samp$y_mnar_gfr_v5_10==0]),table(test.strat.new[samp$y_mnar_gfr_hi_v5_10==0]))
  }
  if(min(cell.size)==0){
    for(j in names(cell.size)[cell.size==0]){
      x2.new=ifelse(samp$strat==substr(j,1,1) & hisp.strat.new==substr(j,3,3) & age.cat.new==substr(j,5,5) & x13.new==substr(j,9,9),0,x2.new)
    }
    test.strat.new=factor(interaction(samp$strat,hisp.strat.new,age.cat.new,x2.new,x13.new))
    cell.size=c(table(test.strat.new[samp$y_mcar_30==0]),table(test.strat.new[samp$y_mar_v5_30==0]),
                table(test.strat.new[samp$y_mnar_bmi_low_v5_30==0]),table(test.strat.new[samp$y_mnar_bmi_v5_30==0]),table(test.strat.new[samp$y_mnar_bmi_hi_v5_30==0]),
                table(test.strat.new[samp$y_mnar_gfr_low_v5_30==0]),table(test.strat.new[samp$y_mnar_gfr_v5_30==0]),table(test.strat.new[samp$y_mnar_gfr_hi_v5_30==0]),
                table(test.strat.new[samp$y_mcar_20==0]),table(test.strat.new[samp$y_mar_v5_20==0]),
                table(test.strat.new[samp$y_mnar_bmi_low_v5_20==0]),table(test.strat.new[samp$y_mnar_bmi_v5_20==0]),table(test.strat.new[samp$y_mnar_bmi_hi_v5_20==0]),
                table(test.strat.new[samp$y_mnar_gfr_low_v5_20==0]),table(test.strat.new[samp$y_mnar_gfr_v5_20==0]),table(test.strat.new[samp$y_mnar_gfr_hi_v5_20==0]),
                table(test.strat.new[samp$y_mcar_10==0]),table(test.strat.new[samp$y_mar_v5_10==0]),
                table(test.strat.new[samp$y_mnar_bmi_low_v5_10==0]),table(test.strat.new[samp$y_mnar_bmi_v5_10==0]),table(test.strat.new[samp$y_mnar_bmi_hi_v5_10==0]),
                table(test.strat.new[samp$y_mnar_gfr_low_v5_10==0]),table(test.strat.new[samp$y_mnar_gfr_v5_10==0]),table(test.strat.new[samp$y_mnar_gfr_hi_v5_10==0]))
  }
  if(min(cell.size)==0){
    for(j in names(cell.size)[cell.size==0]){
      x13.new=ifelse(samp$strat==substr(j,1,1) & hisp.strat.new==substr(j,3,3) & age.cat.new==substr(j,5,5) & x2.new==substr(j,7,7),0,x13.new)
    }
    test.strat.new=factor(interaction(samp$strat,hisp.strat.new,age.cat.new,x2.new,x13.new))
    cell.size=c(table(test.strat.new[samp$y_mcar_30==0]),table(test.strat.new[samp$y_mar_v5_30==0]),
                table(test.strat.new[samp$y_mnar_bmi_low_v5_30==0]),table(test.strat.new[samp$y_mnar_bmi_v5_30==0]),table(test.strat.new[samp$y_mnar_bmi_hi_v5_30==0]),
                table(test.strat.new[samp$y_mnar_gfr_low_v5_30==0]),table(test.strat.new[samp$y_mnar_gfr_v5_30==0]),table(test.strat.new[samp$y_mnar_gfr_hi_v5_30==0]),
                table(test.strat.new[samp$y_mcar_20==0]),table(test.strat.new[samp$y_mar_v5_20==0]),
                table(test.strat.new[samp$y_mnar_bmi_low_v5_20==0]),table(test.strat.new[samp$y_mnar_bmi_v5_20==0]),table(test.strat.new[samp$y_mnar_bmi_hi_v5_20==0]),
                table(test.strat.new[samp$y_mnar_gfr_low_v5_20==0]),table(test.strat.new[samp$y_mnar_gfr_v5_20==0]),table(test.strat.new[samp$y_mnar_gfr_hi_v5_20==0]),
                table(test.strat.new[samp$y_mcar_10==0]),table(test.strat.new[samp$y_mar_v5_10==0]),
                table(test.strat.new[samp$y_mnar_bmi_low_v5_10==0]),table(test.strat.new[samp$y_mnar_bmi_v5_10==0]),table(test.strat.new[samp$y_mnar_bmi_hi_v5_10==0]),
                table(test.strat.new[samp$y_mnar_gfr_low_v5_10==0]),table(test.strat.new[samp$y_mnar_gfr_v5_10==0]),table(test.strat.new[samp$y_mnar_gfr_hi_v5_10==0]))
  }
  if(min(cell.size)==0){
    for(j in names(cell.size)[cell.size==0]){
      age.cat.new=ifelse(samp$strat==substr(j,1,1) & hisp.strat.new==substr(j,3,3) & x2.new==substr(j,7,7) & x13.new==substr(j,9,9),1,age.cat.new)
    }
    test.strat.new=factor(interaction(samp$strat,hisp.strat.new,age.cat.new,x2.new,x13.new))
    cell.size=c(table(test.strat.new[samp$y_mcar_30==0]),table(test.strat.new[samp$y_mar_v5_30==0]),
                table(test.strat.new[samp$y_mnar_bmi_low_v5_30==0]),table(test.strat.new[samp$y_mnar_bmi_v5_30==0]),table(test.strat.new[samp$y_mnar_bmi_hi_v5_30==0]),
                table(test.strat.new[samp$y_mnar_gfr_low_v5_30==0]),table(test.strat.new[samp$y_mnar_gfr_v5_30==0]),table(test.strat.new[samp$y_mnar_gfr_hi_v5_30==0]),
                table(test.strat.new[samp$y_mcar_20==0]),table(test.strat.new[samp$y_mar_v5_20==0]),
                table(test.strat.new[samp$y_mnar_bmi_low_v5_20==0]),table(test.strat.new[samp$y_mnar_bmi_v5_20==0]),table(test.strat.new[samp$y_mnar_bmi_hi_v5_20==0]),
                table(test.strat.new[samp$y_mnar_gfr_low_v5_20==0]),table(test.strat.new[samp$y_mnar_gfr_v5_20==0]),table(test.strat.new[samp$y_mnar_gfr_hi_v5_20==0]),
                table(test.strat.new[samp$y_mcar_10==0]),table(test.strat.new[samp$y_mar_v5_10==0]),
                table(test.strat.new[samp$y_mnar_bmi_low_v5_10==0]),table(test.strat.new[samp$y_mnar_bmi_v5_10==0]),table(test.strat.new[samp$y_mnar_bmi_hi_v5_10==0]),
                table(test.strat.new[samp$y_mnar_gfr_low_v5_10==0]),table(test.strat.new[samp$y_mnar_gfr_v5_10==0]),table(test.strat.new[samp$y_mnar_gfr_hi_v5_10==0]))
  }
  
  samp$strat_nr_v5_fine=test.strat.new
  
  # obtain estimated P(observed y2|strat_nr_v5_fine) for each missing data scenario
  samp$nrprob_mcar_v5_fine_2=(1-aggregate(y_mcar_2 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mcar_2)[samp$strat_nr_v5_fine]
  samp$nrprob_mcar_v5_fine_5=(1-aggregate(y_mcar_5 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mcar_5)[samp$strat_nr_v5_fine]
  samp$nrprob_mcar_v5_fine_10=(1-aggregate(y_mcar_10 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mcar_10)[samp$strat_nr_v5_fine]
  samp$nrprob_mcar_v5_fine_20=(1-aggregate(y_mcar_20 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mcar_20)[samp$strat_nr_v5_fine]
  samp$nrprob_mcar_v5_fine_30=(1-aggregate(y_mcar_30 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mcar_30)[samp$strat_nr_v5_fine]
  samp$nrprob_mar_v5_fine_2=(1-aggregate(y_mar_v5_2 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mar_v5_2)[samp$strat_nr_v5_fine]
  samp$nrprob_mar_v5_fine_5=(1-aggregate(y_mar_v5_5 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mar_v5_5)[samp$strat_nr_v5_fine]
  samp$nrprob_mar_v5_fine_10=(1-aggregate(y_mar_v5_10 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mar_v5_10)[samp$strat_nr_v5_fine]
  samp$nrprob_mar_v5_fine_20=(1-aggregate(y_mar_v5_20 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mar_v5_20)[samp$strat_nr_v5_fine]
  samp$nrprob_mar_v5_fine_30=(1-aggregate(y_mar_v5_30 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mar_v5_30)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_bmi_low_v5_fine_2=(1-aggregate(y_mnar_bmi_low_v5_2 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_bmi_low_v5_2)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_bmi_low_v5_fine_5=(1-aggregate(y_mnar_bmi_low_v5_5 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_bmi_low_v5_5)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_bmi_low_v5_fine_10=(1-aggregate(y_mnar_bmi_low_v5_10 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_bmi_low_v5_10)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_bmi_low_v5_fine_20=(1-aggregate(y_mnar_bmi_low_v5_20 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_bmi_low_v5_20)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_bmi_low_v5_fine_30=(1-aggregate(y_mnar_bmi_low_v5_30 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_bmi_low_v5_30)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_bmi_v5_fine_2=(1-aggregate(y_mnar_bmi_v5_2 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_bmi_v5_2)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_bmi_v5_fine_5=(1-aggregate(y_mnar_bmi_v5_5 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_bmi_v5_5)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_bmi_v5_fine_10=(1-aggregate(y_mnar_bmi_v5_10 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_bmi_v5_10)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_bmi_v5_fine_20=(1-aggregate(y_mnar_bmi_v5_20 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_bmi_v5_20)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_bmi_v5_fine_30=(1-aggregate(y_mnar_bmi_v5_30 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_bmi_v5_30)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_bmi_hi_v5_fine_2=(1-aggregate(y_mnar_bmi_hi_v5_2 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_bmi_hi_v5_2)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_bmi_hi_v5_fine_5=(1-aggregate(y_mnar_bmi_hi_v5_5 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_bmi_hi_v5_5)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_bmi_hi_v5_fine_10=(1-aggregate(y_mnar_bmi_hi_v5_10 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_bmi_hi_v5_10)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_bmi_hi_v5_fine_20=(1-aggregate(y_mnar_bmi_hi_v5_20 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_bmi_hi_v5_20)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_bmi_hi_v5_fine_30=(1-aggregate(y_mnar_bmi_hi_v5_30 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_bmi_hi_v5_30)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_gfr_low_v5_fine_2=(1-aggregate(y_mnar_gfr_low_v5_2 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_gfr_low_v5_2)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_gfr_low_v5_fine_5=(1-aggregate(y_mnar_gfr_low_v5_5 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_gfr_low_v5_5)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_gfr_low_v5_fine_10=(1-aggregate(y_mnar_gfr_low_v5_10 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_gfr_low_v5_10)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_gfr_low_v5_fine_20=(1-aggregate(y_mnar_gfr_low_v5_20 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_gfr_low_v5_20)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_gfr_low_v5_fine_30=(1-aggregate(y_mnar_gfr_low_v5_30 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_gfr_low_v5_30)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_gfr_v5_fine_2=(1-aggregate(y_mnar_gfr_v5_2 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_gfr_v5_2)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_gfr_v5_fine_5=(1-aggregate(y_mnar_gfr_v5_5 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_gfr_v5_5)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_gfr_v5_fine_10=(1-aggregate(y_mnar_gfr_v5_10 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_gfr_v5_10)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_gfr_v5_fine_20=(1-aggregate(y_mnar_gfr_v5_20 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_gfr_v5_20)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_gfr_v5_fine_30=(1-aggregate(y_mnar_gfr_v5_30 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_gfr_v5_30)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_gfr_hi_v5_fine_2=(1-aggregate(y_mnar_gfr_hi_v5_2 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_gfr_hi_v5_2)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_gfr_hi_v5_fine_5=(1-aggregate(y_mnar_gfr_hi_v5_5 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_gfr_hi_v5_5)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_gfr_hi_v5_fine_10=(1-aggregate(y_mnar_gfr_hi_v5_10 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_gfr_hi_v5_10)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_gfr_hi_v5_fine_20=(1-aggregate(y_mnar_gfr_hi_v5_20 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_gfr_hi_v5_20)[samp$strat_nr_v5_fine]
  samp$nrprob_mnar_gfr_hi_v5_fine_30=(1-aggregate(y_mnar_gfr_hi_v5_30 ~ strat_nr_v5_fine, dat=samp, FUN=mean)$y_mnar_gfr_hi_v5_30)[samp$strat_nr_v5_fine]
  
  # calculate non-response weights = bghhsub_s2/P(observed|strat_nr_v5_fine)
  samp$W_nr_mcar_v5_fine_2=samp$bghhsub_s2/samp$nrprob_mcar_v5_fine_2
  samp$W_nr_mcar_v5_fine_5=samp$bghhsub_s2/samp$nrprob_mcar_v5_fine_5
  samp$W_nr_mcar_v5_fine_10=samp$bghhsub_s2/samp$nrprob_mcar_v5_fine_10
  samp$W_nr_mcar_v5_fine_20=samp$bghhsub_s2/samp$nrprob_mcar_v5_fine_20
  samp$W_nr_mcar_v5_fine_30=samp$bghhsub_s2/samp$nrprob_mcar_v5_fine_30
  samp$W_nr_mar_v5_fine_2=samp$bghhsub_s2/samp$nrprob_mar_v5_fine_2
  samp$W_nr_mar_v5_fine_5=samp$bghhsub_s2/samp$nrprob_mar_v5_fine_5
  samp$W_nr_mar_v5_fine_10=samp$bghhsub_s2/samp$nrprob_mar_v5_fine_10
  samp$W_nr_mar_v5_fine_20=samp$bghhsub_s2/samp$nrprob_mar_v5_fine_20
  samp$W_nr_mar_v5_fine_30=samp$bghhsub_s2/samp$nrprob_mar_v5_fine_30
  samp$W_nr_mnar_bmi_low_v5_fine_2=samp$bghhsub_s2/samp$nrprob_mnar_bmi_low_v5_fine_2
  samp$W_nr_mnar_bmi_low_v5_fine_5=samp$bghhsub_s2/samp$nrprob_mnar_bmi_low_v5_fine_5
  samp$W_nr_mnar_bmi_low_v5_fine_10=samp$bghhsub_s2/samp$nrprob_mnar_bmi_low_v5_fine_10
  samp$W_nr_mnar_bmi_low_v5_fine_20=samp$bghhsub_s2/samp$nrprob_mnar_bmi_low_v5_fine_20
  samp$W_nr_mnar_bmi_low_v5_fine_30=samp$bghhsub_s2/samp$nrprob_mnar_bmi_low_v5_fine_30
  samp$W_nr_mnar_bmi_v5_fine_2=samp$bghhsub_s2/samp$nrprob_mnar_bmi_v5_fine_2
  samp$W_nr_mnar_bmi_v5_fine_5=samp$bghhsub_s2/samp$nrprob_mnar_bmi_v5_fine_5
  samp$W_nr_mnar_bmi_v5_fine_10=samp$bghhsub_s2/samp$nrprob_mnar_bmi_v5_fine_10
  samp$W_nr_mnar_bmi_v5_fine_20=samp$bghhsub_s2/samp$nrprob_mnar_bmi_v5_fine_20
  samp$W_nr_mnar_bmi_v5_fine_30=samp$bghhsub_s2/samp$nrprob_mnar_bmi_v5_fine_30
  samp$W_nr_mnar_bmi_hi_v5_fine_2=samp$bghhsub_s2/samp$nrprob_mnar_bmi_hi_v5_fine_2
  samp$W_nr_mnar_bmi_hi_v5_fine_5=samp$bghhsub_s2/samp$nrprob_mnar_bmi_hi_v5_fine_5
  samp$W_nr_mnar_bmi_hi_v5_fine_10=samp$bghhsub_s2/samp$nrprob_mnar_bmi_hi_v5_fine_10
  samp$W_nr_mnar_bmi_hi_v5_fine_20=samp$bghhsub_s2/samp$nrprob_mnar_bmi_hi_v5_fine_20
  samp$W_nr_mnar_bmi_hi_v5_fine_30=samp$bghhsub_s2/samp$nrprob_mnar_bmi_hi_v5_fine_30
  samp$W_nr_mnar_gfr_low_v5_fine_2=samp$bghhsub_s2/samp$nrprob_mnar_gfr_low_v5_fine_2
  samp$W_nr_mnar_gfr_low_v5_fine_5=samp$bghhsub_s2/samp$nrprob_mnar_gfr_low_v5_fine_5
  samp$W_nr_mnar_gfr_low_v5_fine_10=samp$bghhsub_s2/samp$nrprob_mnar_gfr_low_v5_fine_10
  samp$W_nr_mnar_gfr_low_v5_fine_20=samp$bghhsub_s2/samp$nrprob_mnar_gfr_low_v5_fine_20
  samp$W_nr_mnar_gfr_low_v5_fine_30=samp$bghhsub_s2/samp$nrprob_mnar_gfr_low_v5_fine_30
  samp$W_nr_mnar_gfr_v5_fine_2=samp$bghhsub_s2/samp$nrprob_mnar_gfr_v5_fine_2
  samp$W_nr_mnar_gfr_v5_fine_5=samp$bghhsub_s2/samp$nrprob_mnar_gfr_v5_fine_5
  samp$W_nr_mnar_gfr_v5_fine_10=samp$bghhsub_s2/samp$nrprob_mnar_gfr_v5_fine_10
  samp$W_nr_mnar_gfr_v5_fine_20=samp$bghhsub_s2/samp$nrprob_mnar_gfr_v5_fine_20
  samp$W_nr_mnar_gfr_v5_fine_30=samp$bghhsub_s2/samp$nrprob_mnar_gfr_v5_fine_30
  samp$W_nr_mnar_gfr_hi_v5_fine_2=samp$bghhsub_s2/samp$nrprob_mnar_gfr_hi_v5_fine_2
  samp$W_nr_mnar_gfr_hi_v5_fine_5=samp$bghhsub_s2/samp$nrprob_mnar_gfr_hi_v5_fine_5
  samp$W_nr_mnar_gfr_hi_v5_fine_10=samp$bghhsub_s2/samp$nrprob_mnar_gfr_hi_v5_fine_10
  samp$W_nr_mnar_gfr_hi_v5_fine_20=samp$bghhsub_s2/samp$nrprob_mnar_gfr_hi_v5_fine_20
  samp$W_nr_mnar_gfr_hi_v5_fine_30=samp$bghhsub_s2/samp$nrprob_mnar_gfr_hi_v5_fine_30
  
  return(samp)
}

####### read-in population datasets #######
#pop=read.csv(file="G:/everyone/butera/pop_oct2016_large.csv",header=TRUE)
pop=read.csv(file="D:/hard drive material/CSCC research/Population migration project/12_Clustering/data/population.csv",header=TRUE)


####### generate S samples from same population for each scenario #######
S=8200

allsamp=function(k){
  samp=samp.gen(pop,prob.bg=rep(c(.25,.25,.6,.6),times=2),num.bg=rep(c(58,21,130,167),times=2),prob.hh.hisp=rep(c(.18,.225,.14,.14),times=2),prob.hh.other=rep(c(.025,.0175,.035,.04),times=2),prob.age=c(.3575,.55))
  samp$dat_num=rep(k,times=dim(samp)[1])
  return(samp)
}
set.seed(S)
list.all=lapply(7901:S,allsamp)
samp.all=do.call(rbind,list.all)

####### save sample datasets for each scenario #######
for(i in 7901:S){
  cat(i)
  samp.comb <- samp.all[samp.all$dat_num==i,]
  write.csv(samp.comb,paste0("widewt_samp_mar2017_large_",i,".csv"),row.names=FALSE)
}
