## @ by Youyi Fong
#
## formula: e.g. y1~PCA.d14
## trt.grp: 0 for placebo, 1 for vaccine, 2 for both


tps.rsv=function (formula, trt.grp) {
    tmp = factor(sub("_case", "", sub("_ctrl", 
        "", dat.wide$stratum)))
    dat.wide$group = as.integer(tmp)
    group.names = names(table(tmp))
    dat.wide$group.name = group.names[dat.wide$group]
    dat.wide.v$group = dat.wide$group[match(dat.wide.v$pair.id, 
        dat.wide$pair.id)]
    strata.size = table(dat.wide$stratum)
    nn0_both = strata.size[sub("30d", "30d_ctrl", 
        group.names)]
    nn1_both = strata.size[sub("30d", "30d_case", 
        group.names)]
    nn1_both[is.na(nn1_both)] = 0
    names(nn1_both) = sub("ctrl", "case", names(nn0_both))
    if (trt.grp == 0) {
        data = subset(dat.wide.v, trt == trt.grp)
        data$group = (data$group + 1)/2
        nn0 = nn0_both[seq(1, length(nn0_both), by = 2)]
        nn1 = nn1_both[seq(1, length(nn1_both), by = 2)]
        stopifnot(nn1[9] == 0 & length(nn1) == 10)
        nn1 = nn1[-9]
        nn0 = nn0[-9]
        data = data[data$group != 9, ]
        data$group[data$group == 10] = 9
    }
    else if (trt.grp == 1) {
        data = subset(dat.wide.v, trt == trt.grp)
        data$group = data$group/2
        nn0 = nn0_both[seq(2, length(nn0_both), by = 2)]
        nn1 = nn1_both[seq(2, length(nn1_both), by = 2)]
        stopifnot(nn1[9] == 0 & length(nn1) == 10)
        nn1 = nn1[-9]
        nn0 = nn0[-9]
        data = data[data$group != 9, ]
        data$group[data$group == 10] = 9
    }
    else if (trt.grp == 2) {
        data = dat.wide.v
        nn0 = nn0_both
        nn1 = nn1_both
        stopifnot(nn1[17] == 0 & nn1[18] == 0 & length(nn1) == 
            20)
        nn1 = nn1[-18]
        nn0 = nn0[-18]
        data = data[data$group != 18, ]
        nn1 = nn1[-17]
        nn0 = nn0[-17]
        data = data[data$group != 17, ]
        data$group[data$group == 19] = 17
        data$group[data$group == 20] = 18
    }
    else stop("trt.grp: 0 for placebo, 1 for vaccine, 2 for both")
    fit = tps(formula, data, nn0 = nn0, nn1 = nn1, group = data$group) #        method = "WL"# to be more robust
    summary(fit)
    res = summary(fit)$coef
    robust = FALSE
    idx = ifelse(robust, "Emp ", "Mod ")
    res = cbind(res[, c("Value", idx %.% "p")], `lower bound` = res[, 
        "Value"] - 1.96 * res[, idx %.% "SE"], `upper bound` = res[, 
        1] + 1.96 * res[, idx %.% "SE"])
    res[, c(1, 3, 4)] = exp(res[, c(1, 3, 4)])
    colnames(res) = c("OR", "p.value", "(lower", 
        "upper)")
    fit$coefficients = res
    fit
}


#tps.rsv=function(formula, trt.grp, population=c("i","m","b")){
#    
#    population=match.arg(population)    
#    if (population=="i") {
#        subset.ph1=  dat.wide$ppimifl=="Y"
#        subset.ph2=dat.wide.v$ppimifl=="Y"
#    } else if (population=="m") {
#        subset.ph1=  dat.wide$ppimmfl=="Y"
#        subset.ph2=dat.wide.v$ppimmfl=="Y"
#    } else if (population=="b") {
#        subset.ph1=  dat.wide$ppintersectfl=="Y"
#        subset.ph2=dat.wide.v$ppintersectfl=="Y"
#    } 
#    
#    # group index
#    tmp=factor(sub("_case","", sub("_ctrl","",dat.wide$stratum)))
#    dat.wide$group = as.integer(tmp)
#    group.names=names(table(tmp))
#    dat.wide$group.name = group.names[dat.wide$group]
#    dat.wide.v$group = dat.wide$group[match(dat.wide.v$pair.id, dat.wide$pair.id)]
#    
#    # compute nn0 and nn1
#    strata.size=table(dat.wide$stratum[subset.ph1])
#    nn0_both=strata.size[sub("30d","30d_ctrl", group.names)]    
#    nn1_both=strata.size[sub("30d","30d_case", group.names)]
#    # there are some NA's in nn1_both
#    nn1_both[is.na(nn1_both)]=0
#    names(nn1_both)=sub("ctrl","case",names(nn0_both))
#    
#    if(trt.grp==0) {
#        data=subset(dat.wide.v, trt==trt.grp & subset.ph2)    
#        data$group = (data$group+1)/2
#        nn0=nn0_both[seq(1,length(nn0_both),by=2)]
#        nn1=nn1_both[seq(1,length(nn1_both),by=2)]
#        # 10 strata, 9th nn1 is 0
#        stopifnot(nn1[9]==0 & length(nn1)==10)
#        # remove 9th stratum
#        nn1=nn1[-9]; nn0=nn0[-9]; data=data[data$group!=9,]
#        # make 10th stratum the 9th
#        data$group[data$group==10]=9
#        
#    } else if(trt.grp==1) {
#        data=subset(dat.wide.v, trt==trt.grp & subset.ph2)    
#        data$group = data$group/2
#        nn0=nn0_both[seq(2,length(nn0_both),by=2)]
#        nn1=nn1_both[seq(2,length(nn1_both),by=2)]
#        # 10 strata, 9th nn1 is 0
#        stopifnot(nn1[9]==0 & length(nn1)==10)
#        # remove 9th stratum
#        nn1=nn1[-9]; nn0=nn0[-9]; data=data[data$group!=9,]
#        # make 10th stratum the 9th
#        data$group[data$group==10]=9
#        
#    } else if(trt.grp==2) {
#        data=dat.wide.v[subset.ph2,]
#        nn0=nn0_both
#        nn1=nn1_both
#        # 10 strata, 9th nn1 is 0
#        stopifnot(nn1[17]==0 & nn1[18]==0 & length(nn1)==20)
#        # remove 17th and 18th stratum
#        nn1=nn1[-18]; nn0=nn0[-18]; data=data[data$group!=18,]
#        nn1=nn1[-17]; nn0=nn0[-17]; data=data[data$group!=17,]
#        # make 19/20 stratum 17/18
#        data$group[data$group==19]=17
#        data$group[data$group==20]=18
#        
#    } else stop("trt.grp: 0 for placebo, 1 for vaccine, 2 for both")
#    
##    for (a in attr(terms(formula),"term.labels")){
##        data[[a]]=scale(data[[a]])
##    }    
#
#    fit=tps(formula, data, nn0=nn0, nn1=nn1, group=data$group, method="WL")    
#    summary(fit)
#    
#    res = summary(fit)$coef
#    # PL
#    #robust=FALSE # robust may not be very stable when sample size is small
#    #idx=ifelse(robust,"Emp ","Mod ")
#    #res = cbind(res[,c("Value",idx%.%"p")], "lower bound"=res[,"Value"]-1.96*res[,idx%.%"SE"], "upper bound"=res[,1]+1.96*res[,idx%.%"SE"])
#    #res[,c(1,3,4)] = exp(res[,c(1,3,4)])
#    #colnames(res)=c("OR", "p.value", "(lower", "upper)")
#    # WL
#    fit$coefficients=res    
#    
#    fit
#    
#}
