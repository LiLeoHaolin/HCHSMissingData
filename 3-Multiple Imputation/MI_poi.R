miss='mcar'
vers=''
misspct = c(2,5,10,20,30)
nimp = 5
seed = 100217
namefile = 'mcar.bin.pois.mi.over.RData'
nsim = 1000
cts <- 1

if(length(grep('under',namefile))==1){
    # flag <- c(paste0('strat',1:3),'age_strat','x15') # Deleted on 07/21/20
    flag <- c('x2','x13')
} else{
    if(length(grep('exact',namefile))==1){
        # flag <- c(paste0('strat',1:3),'age_strat','x15','x2') # Deleted on 07/21/20
        flag <- c('x2','x13',paste0('strat',1:3),'age_base')
    } else{
        if(length(grep('over',namefile))==1){
            # flag <- c(paste0('strat',1:3),'age_strat','x15','x2','hisp_strat') # Deleted on 07/21/20
            flag <- c('x2','x13',paste0('strat',1:3),'age_base','hisp_strat')
        } else{
            stop('check code')
        }
    }
}

library(survey)
library(data.table)
library(mice)
library(mitools)

dirdata = '/pine/scr/b/a/baldoni/Cai/Visit2/Manuscript_MissingData/Data/NRW_Apr2020/'
dirwork = '/pine/scr/b/a/baldoni/Cai/Visit2/Manuscript_MissingData/Codes/'
diroutp = '/pine/scr/b/a/baldoni/Cai/Visit2/Manuscript_MissingData/Output/'

setwd(dirwork)

files = paste0(dirdata,'widewt_samp_mar2017_',1:nsim,'.csv')

foo = function(miss,vers,cut,misspct,nimp,seed){
    label = paste0('bin_pois_',cut,'_',miss)
    df = list()

    ymiss = paste0('y_',miss,vers,'_',misspct)
    yimp = paste0('y_bin_',cut,'_',miss,'_',misspct,'_imp')
    timp = paste0('x6_',miss,'_',misspct,'_imp')
    allcomp = paste0('allcomp_',misspct)

    for(i in 1:length(files)){
        dat = fread(file=files[i],header=T)

        simnum = i
        cat(simnum)
        ###################s
        #Creating variables
        dat$strat1 = 1*(dat$strat==1)
        dat$strat2 = 1*(dat$strat==2)
        dat$strat3 = 1*(dat$strat==3)
        dat$strat4 = 1*(dat$strat==4)

        dat.merge = dat
        subdat.merge = data.frame(subid = rep(dat$subid,nimp),.imp = rep(1:nimp,each=nrow(dat)))

        for(j in 1:length(ymiss)){
            dat.anal = dat
            dat.anal[[yimp[j]]] = ifelse(dat.anal[[ymiss[j]]]==0,dat.anal[[paste0('y_bin_gfr_',cut,'_v3')]],NA)
            dat.anal[[timp[j]]] = ifelse(dat.anal[[ymiss[j]]]==0,dat.anal[['x6']],NA)
            dat.anal[[allcomp[j]]] = ifelse(dat.anal[[ymiss[j]]]==0,1,0)

            ### Running Multiple Imputation
            subvar = c('BGid','strat','bghhsub_s2','subid','hisp_strat','age_base', #Design variables
                       'strat1','strat2','strat3','x13','x15','x2', #Variables that Poulami is using in her 'Under'-specified models
                       timp[j],yimp[j], #Missing variables
                       paste0('y1_bin_gfr_',cut,'_v3'))
            
            subdat = subset(dat.anal,select=subvar)
            
            ### ### Subseting incident cases. Multiple Imputation is run only for non-cases at baseline.
            subdat0 = subdat[which(subdat[[paste0('y1_bin_gfr_',cut,'_v3')]]==0),]
            subdat1 = subdat[which(subdat[[paste0('y1_bin_gfr_',cut,'_v3')]]==1),]
            
            ### Creating pairwise interactions and running mice
            subdat0.imp = mice(cbind(subdat0[,unique(c('subid',yimp[j],timp[j],flag,c('x13','x15','bghhsub_s2'))),with=FALSE]),seed=seed,m=nimp)
            
            subdat.com = list()
            for(k in 1:nimp){
                # STOPPED HERE
                subdat.com.k = rbind(merge(x=subset(complete(subdat0.imp,k,include=F),select = c('subid',timp[j],yimp[j])),
                                           y=subset(subdat0,select=c('subid','strat','BGid','bghhsub_s2','x13','x15',paste0('y1_bin_gfr_',cut,'_v3'))),by='subid',all.x=T),
                                     subset(subdat1,select = c('subid',timp[j],yimp[j],'strat','BGid','bghhsub_s2','x13','x15',paste0('y1_bin_gfr_',cut,'_v3'))))
                subdat.com.k$.imp = k
                subdat.com[[k]] = as.data.frame(subdat.com.k)
            }
            subdat.com <- rbindlist(subdat.com)
            
            subdat.com$response = subdat.com[[yimp[j]]]
            subdat.com$baseline = subdat.com[[paste0('y1_bin_gfr_',cut,'_v3')]]
            subdat.com$x6imp = subdat.com[[timp[j]]]
            
            subdat.list = imputationList(split(subdat.com,subdat.com$.imp))
            
            ### Analyzing data ###
            design = svydesign(id=~BGid, strata=~strat, weights=~bghhsub_s2, data=subdat.list)
            model = with(design,svyglm(response~x13+x15+offset(log(x6imp)),subset=(baseline==0),
                                       family=quasipoisson(link = "log")))
            str(summary(MIcombine(model)))
            df[[cts]] <- data.frame(sim=simnum,missing=misspct[j],par=c('Int','x13','x15'),summary(MIcombine(model)))
            cts <- cts + 1

            ### Merging data
            subdat.merge = merge(subdat.merge,subset(subdat.com,select=c('subid','.imp',yimp[j],timp[j])),by=c('subid','.imp'))
        }

        dat.merge = merge(dat.merge,subdat.merge,by='subid',all.y=T)
        ###################
    }
    df <- rbindlist(df)

    colnames(df) = c('sim','misspct','par','beta','se','lb','ub','missinfo')
    rownames(df) = NULL
    return(df)
}

df.low = foo(miss=miss,vers=vers,cut='low',misspct=misspct,nimp=nimp,seed=seed)
df.hi = foo(miss=miss,vers=vers,cut='hi',misspct=misspct,nimp=nimp,seed=seed)
save(df.low,df.hi,file=paste0(diroutp,namefile))

