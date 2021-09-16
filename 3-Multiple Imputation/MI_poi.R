library(survey)
library(data.table)
library(mice)
library(mitools)

### specify simulation setting
miss='mcar'
misspct = 30
nimp = 5
seed = 100217
cut = 'low'
flag <- c('x2','x13',paste0('strat',1:3),'age_base','hisp_strat') # auxiliary variables and covariates

### creating variables
ymiss = paste0('y_',miss,'_',misspct)
yimp = paste0('y_bin_',cut,'_',miss,'_',misspct,'_imp')
timp = paste0('x6_',miss,'_',misspct,'_imp')
allcomp = paste0('allcomp_',misspct)
dat$strat1 = 1*(dat$strat==1)
dat$strat2 = 1*(dat$strat==2)
dat$strat3 = 1*(dat$strat==3)
dat$strat4 = 1*(dat$strat==4)
dat.merge = dat
subdat.merge = data.frame(subid = rep(dat$subid,nimp),.imp = rep(1:nimp,each=nrow(dat)))
dat.anal = dat
dat.anal[[yimp]] = ifelse(dat.anal[[ymiss]]==0,dat.anal[[paste0('y_bin_gfr_',cut,'_v3')]],NA)
dat.anal[[timp]] = ifelse(dat.anal[[ymiss]]==0,dat.anal[['x6']],NA)
dat.anal[[allcomp]] = ifelse(dat.anal[[ymiss]]==0,1,0)

### running multiple imputation
subvar = c('BGid','strat','bghhsub_s2','subid','hisp_strat','age_base', #Design variables
           'strat1','strat2','strat3','x13','x15','x2', #Variables that Poulami is using in her 'Under'-specified models
           timp,yimp, #Missing variables
           paste0('y1_bin_gfr_',cut,'_v3'))
subdat = subset(dat.anal,select=subvar)
subdat0 = subdat[which(subdat[[paste0('y1_bin_gfr_',cut,'_v3')]]==0),]
subdat1 = subdat[which(subdat[[paste0('y1_bin_gfr_',cut,'_v3')]]==1),]
subdat0.imp = mice(cbind(subdat0[,unique(c('subid',yimp,timp,flag,c('x13','x15','bghhsub_s2'))),with=FALSE]),seed=seed,m=nimp)
subdat.com = list()
for(k in 1:nimp){
  subdat.com.k = rbind(merge(x=subset(complete(subdat0.imp,k,include=F),select = c('subid',timp,yimp)),
                             y=subset(subdat0,select=c('subid','strat','BGid','bghhsub_s2','x13','x15',paste0('y1_bin_gfr_',cut,'_v3'))),by='subid',all.x=T),
                       subset(subdat1,select = c('subid',timp,yimp,'strat','BGid','bghhsub_s2','x13','x15',paste0('y1_bin_gfr_',cut,'_v3'))))
  subdat.com.k$.imp = k
  subdat.com[[k]] = as.data.frame(subdat.com.k)
}
subdat.com <- rbindlist(subdat.com)
subdat.com$response = subdat.com[[yimp]]
subdat.com$baseline = subdat.com[[paste0('y1_bin_gfr_',cut,'_v3')]]
subdat.com$x6imp = subdat.com[[timp]]
subdat.list = imputationList(split(subdat.com,subdat.com$.imp))

### Analyzing data
design = svydesign(id=~BGid, strata=~strat, weights=~bghhsub_s2, data=subdat.list)
model = with(design,svyglm(response~x13+x15+offset(log(x6imp)),subset=(baseline==0),family=quasipoisson(link = "log")))
df <- data.frame(par=c('Int','x13','x15'),summary(MIcombine(model)))












