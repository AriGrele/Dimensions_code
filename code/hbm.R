progress=function(percent,len,char='='){                                        #print progress bar to console, takes arguments of percent progress, nchar to print at 100%, character to form the bar from
  k=round(percent*len)                                                          #nchar to print at current percent
  cat('|',                                                                      #print
      rep(char,k),
      rep(' ',len-k),'| ',percent*100,'%                  \r',sep='')
  if(percent==1){cat('\n')}}                                                    #new line at 100%

pd=function(x){                                                                 #calculates percent of vector with same sign as vector median
  side=sign(median(x))                                                          #sign of median
  return(sum(side*x>0)/length(x))}                                              #return probability of direction

chunk=function(ar,len){                                                         #breaks array into equal chunks, takes arguments of array, chunk length
  o=list();c=0                                                                  #define vars
  for(i in len*0:(length(ar)/len-1)){i=i+1;c=c+1;o[[c]]=ar[(i):(i+len-1)]}      #split into groups of set length, append to list
  return(o)}

flip=function(x){                                                               #takes a named vector and returns vector of names, now named for the values they represent in original vector
  if(is.list(x)){x=x[[1]]}                                                      #edge use case
  o=data.frame('i'=NA,'name'=NA)                                                #empty dataframe
  for(i in 1:length(x)){o=rbind(o,data.frame('i'=x[i],'name'=names(x[i])))}     #iterate through items and append names to frame
  return(with(unique(na.omit(o)),setNames(name,i)))}                            #return renamed names

items=function(data,v,x=1){                                                     #recursive function for generating named list from dataframe columns, take argument of dataframe, vector of col names
  o=c()                                                                         #define var
  d=as.numeric(as.factor(data[,v[x]]))                                          #characters to numbers
  d=setNames(d,data[,v[x]])                                                     #rename based on original values
  n=setNames(unique(data[,v[x]]),unique(d))                                     
  if(x==length(v)){return(d)}                                                   #return on final value
  else{for(i in unique(d)){o=c(o,items(data[d==i,],v,x+1))};return(o)}}         #otherwise repeat for sub-hierarchies 

ilen=function(data,v,x=1){                                                      #returns counts of factors in each level of hierarchy, take argument of dataframe, vector of col names
  o=c()                                                                         #define var
  d=as.numeric(as.factor(data[,v[x]]))                                          #characters to numbers
  if(x==length(v)){return(length(unique(d)))}                                   #return on final value
  else{for(i in unique(d)){o[i]=(ilen(data[d==i,],v,x+1))};return(o)}}          #otherwise repeat for sub-hierarchies 

interact=function(data,name){                                                   #format data for interaction summaries, takes input of data.frame, interaction term       
  o=data[[5]][data[[5]][name]!='all',]                                          #subset data
  d=setNames(as.data.frame(matrix(ncol=length(names(o)))),names(o))             #empty output data.frame
  if(length(data[[4]][[2]])-1==1){                                              #this chunk no use at moment
    for(u in unique(o$upper)){
      i_s=c()
      for(i in unique(o[o$upper==u,][,name])){
        i_s=c(i_s,i)
        for(j in unique(o[o$upper==u,][,name])){
          if(!(j %in% i_s)){
            cat(u,' ',i,' ',j,'\n')}}}}}
  else{                                                                         #standard use
    other=c(2,1)[match(name,names(o))]                                          #find interaction col
    count=0                                                                     #var for progress bar
    for(q in unique(o[,other])){                                                #for each grouping in data, count iterations
      for(u in unique(o$upper[o[other]==q])){
        i_s=c()
        for(i in unique(o[o$upper==u&o[other]==q,][,name])){
          i_s=c(i_s,i)
          for(j in unique(o[o$upper==u&o[other]==q,][,name])){
            if(!(j %in% i_s)){count=count+1}}}}}
    cur=0                                                                       #var for progress bar
    for(q in unique(o[,other])){                                                #for each grouping in data
      for(u in unique(o$upper[o[other]==q])){
        i_s=c()
        for(i in unique(o[o$upper==u&o[other]==q,][,name])){
          i_s=c(i_s,i)
          d=rbind(d,o[o[other]==q&o$upper==u&o[,name]!='all',])
          for(j in unique(o[o$upper==u&o[other]==q,][,name])){
            if(!(j %in% i_s)){
              cur=cur+1
              rd=o[o[other]==q&o$upper==u&o[,name]==j,]                         #select each item of interaction col
              rd[name]='Difference'                                             #default values
              rd$upper='all'
              rd$response=o$response[o[other]==q&o$upper==u&o[,name]==i]-       #calculate difference for each group
                o$response[o[other]==q&o$upper==u&o[,name]==j]
              d=rbind(d,rd)                                                     #bind to output
              progress(cur/count,50)                                            #progress
              }}}}}}
  return(list(data[[1]],data[[2]],data[[3]],data[[4]],na.omit(d)))}             #return list of data outputs for summary function

traces=function(x,dir='traces.csv',cull=0){                                     #create traceplots from hbm output, takes arguments of model, directory to save to, amount to cull from output
  samples=x$samples                                                             #select samples and columns
  cols=colnames(x$samples[[1]])
  allucols=unique(gsub('\\[.+]$','',cols));ucols=allucols[1]                    #correct colnames
  for(u in allucols){                                                           #for each col:
    stop=0
    for(q in ucols){if(regexpr(u,q)>0|regexpr(q,u)>0){stop=1}}                  #   stop when non-unique column reached
    if(stop==0){ucols=c(ucols,u)}}                                              #   append col after stop
  
  nchains=x$mcmc.info$n.chains                                                  #extract mcmc info from model
  xmax=x$mcmc.info$n.samples/nchains
  plots=list();k=0                                                              #empty vars for output
  for(u in ucols){                                                              #for each column:
    out=list()
    for(i in cols[regexpr(u,cols)>0]){                                          
      k=k+1
      e=stringr::str_extract_all(i,'(\\[\\d+,\\d+\\])|(\\[\\d+\\])')
      pos=eval(parse(text=gsub('\\]','\\)',gsub('\\[','c\\(',e))))
      rhat=ifelse(length(pos)==1,x$Rhat[[gsub('\\[.+]$','',i)]][pos[1]],
                  ifelse(length(pos)==0,x$Rhat[[gsub('\\[.+]$','',i)]],
                         x$Rhat[[gsub('\\[.+]$','',i)]][pos[1],pos[2]]))
      m=suppressMessages(reshape2::melt(as.data.frame(
          coda::as.array.mcmc.list(samples[,i])))[c(T,rep(F,cull)),])
      out[[i]]=ggplot(m)+
        geom_line(aes(x=rep(1:xmax,nchains)[c(T,rep(F,cull))],
                      y=value,color=variable),alpha=1/nchains*2,size=1)+
        theme_classic()+
        theme(text=element_text(size=65),
              panel.border=element_rect(fill=F,size=5),
              axis.text = element_blank())+
        ggtitle(i,subtitle =paste('Rhat =',round(rhat,4)))+
        guides(color='none')+
        xlab('')+ylab('')+
        scale_x_continuous(expand = c(0,0))+
        scale_y_continuous(expand=c(0,0))
      if(rhat<1.1){out[[i]]=out[[i]]+
        scale_color_manual(values=pals::ocean.ice(nchains))}
      else{out[[i]]=out[[i]]+
        scale_color_manual(values=pals::ocean.matter(nchains))}
      progress(k/length(cols),50)
    }
    n=floor(sqrt(length(out)))
    png(paste(dir,u,'.png',sep=''),1000*n,1000*length(out)/n)
    do.call(gridExtra::grid.arrange,c(out,ncol=n))
    dev.off()}}

wave=function(data,var,s){                                                      #create single distribution plots, takes argument of model output, focal variables, scale
  g=ggplot(data)+
    geom_vline(xintercept = 0,size=2*2)+
    geom_hline(yintercept=0,size=2*s)+
    facet_grid(reformulate(var$var[1]))+
    geom_density(aes_string(x='response',fill=var$var[1]),size=s)
  return(list(g))}
  
wave2=function(data,var,s){                                                     #create multi-distribution plots, takes argument of model output, focal variables, scale
  data=subset(data,!(data[,var$var[1]]=='all'&data[,var$var[2]]=='all'))
  out=list()
  for(g in unique(data[,var$var[2]])){
    out[[g]]=ggplot()+
      geom_vline(xintercept = 0,size=2*s,linetype='dashed')+
      geom_density(data=data[data[,var$var[1]]=='all'&data[,var$var[2]]==g,],
                   aes_string(x='response',group=var$var[2]),size=1.5*s,
                   fill='black',color='black',alpha=.5)+
      geom_density(data=data[data[,var$var[1]]!='all'&data[,var$var[2]]==g,],
                   aes_string(x='response',color=var$var[1]),size=1.5*s)}
  return(out)}

ocean=function(data,groups,fill='lower',s=1){                                   #create groups of waveplots, takes arguments of model output, focal groups, fill groups, scale
  out=list()
  for(g in groups){
    h=data[regexpr(g,data$lower)>0,]
    v=hbmvar(h,c(fill))
    if(length(v$var)==1){out[[g]]=wave(h,v,s)}
    else{if(v$var[2]=='upper'){out[[g]]=wave(h,v,s)}
      else{out[[g]]=wave2(h,v,s)}}}
  return(unlist(out, recursive = FALSE))}

dotplot=function(data,var,s){                                                   #create single dotplot, takes argument of model output, focal variables, scale
  df=setNames(as.data.frame(matrix(ncol=4)),c('variable','y2.5','y97.5','y50'))
  for(i in unique(data[,var$var[1]])){
    q=data$response[data[,var$var[1]]==i]
    df=rbind(df,data.frame('variable'=i,'y2.5'=quantile(q,0.05),
                           'y97.5'=quantile(q,0.95),'y50'=quantile(q,.5)))}
  g=ggplot(data)+
    geom_hline(yintercept=0,size=2*s)+
    geom_boxplot(data=na.omit(df),width=.05*s,lwd=s,
                 aes(x=as.factor(variable),ymin = y2.5, lower = y2.5, 
                     middle = y50, upper = y97.5, ymax = y97.5),
                 stat = "identity",fill='black')+
    geom_point(data=na.omit(df),aes(x=as.factor(variable),y=y50),size=5*s)
  
  return(g)}

dotplot2=function(data,var,s){                                                  #create multi-dotplots (old version), takes argument of model output, focal variables, scale
  df=setNames(as.data.frame(matrix(ncol=5)),
              c(var$var[1],'v2','y2.5','y97.5','y50'))
  for(i in unique(data[,var$var[1]])){
    for(j in unique(data[,var$var[2]])){
      q=data$response[data[,var$var[1]]==i&data[,var$var[2]]==j]
      df=rbind(df,setNames(data.frame('v'=i,'v2'=j,
                                      'y2.5'=quantile(q,0.05),
                                      'y97.5'=quantile(q,0.95),
                                      'y50'=quantile(q,.5)),
                           c(var$var[1],'v2','y2.5','y97.5','y50')))}}
  g=ggplot(data)+
    geom_hline(yintercept=0,size=2*s)+
    facet_grid(reformulate(var$var[1]))+
    geom_boxplot(data=na.omit(df),width=.05*s,lwd=s,position = position_dodge(.9),
                 aes(x=as.factor(v2),group=as.factor(v2),ymin = y2.5, lower = y2.5, middle = y50, upper = y97.5, ymax = y97.5),stat = "identity",fill='black')+
    geom_point(data=na.omit(df),aes(x=as.factor(v2),group=as.factor(v2),y=y50),size=5*s)
  return(g)}

polka=function(data,groups,fill='lower',s=1){                                   #create groups of dotplots, takes arguments of model output, focal groups, fill group, scale
  out=list()
  for(g in groups){
    h=data[regexpr(g,data$lower)>0,]
    v=hbmvar(h,c(fill))
    if(length(v$var)==1){out[[g]]=dotplot(h,v,s)}
    else{if(v$var[2]=='upper'){out[[g]]=dotplot(h,v,s)}
      else{out[[g]]=dotplot2(h,v,s)}}}
  return(out)}

dotplot2=function(data,var,s){                                                  #create multi-dotplots (new version), takes argument of model output, focal variables, scale
  df=setNames(as.data.frame(matrix(ncol=5)),
              c(var$var[1],'v2','y2.5','y97.5','y50'))
  for(i in unique(data[,var$var[1]])){
    for(j in unique(data[,var$var[2]])){
      q=data$response[data[,var$var[1]]==i&data[,var$var[2]]==j]
      df=rbind(df,setNames(data.frame('v'=i,'v2'=j,
                                      'y2.5'=quantile(q,0.05),
                                      'y97.5'=quantile(q,0.95),
                                      'y50'=quantile(q,.5)),
                           c(var$var[1],'v2','y2.5','y97.5','y50')))}}
  g=ggplot(data)+
    geom_hline(yintercept=0,size=2*s)+
    facet_grid(reformulate(var$var[1]))+
    geom_boxplot(data=na.omit(df),
                 width=.05*s,lwd=s,position = position_dodge(.9),
                 aes(x=as.factor(v2),group=as.factor(v2),
                     ymin = y2.5, lower = y2.5, middle = y50, 
                     upper = y97.5, ymax = y97.5),
                 stat = "identity",fill='black')+
    geom_point(data=na.omit(df),aes(x=as.factor(v2),
                                    group=as.factor(v2),y=y50),size=5*s)
  return(g)}

hbmgroup=function(data,groups){                                                 #Selects specific subsets of data based on focal group, takes arguments of model output, focal groups
  v=data[[4]][[2]]
  out=setNames(as.data.frame(matrix(ncol=length(data[[5]]))),names(data[[5]]))
  for(g in groups){
    if(!(g %in% v)){out=rbind(out,data[[5]][regexpr(g,data[[5]]$lower)>0,])}
    else{out=rbind(out,data[[5]][regexpr('all',data[[5]][,g])>0,])}}
  for(g in groups){
    if(!(g %in% v)){out=out[regexpr(g,out$lower)>0,]}
    else{out=out[regexpr('all',out[,g])>0,]}}
  return(na.omit(out))}

hbmvar=function(data,groups){                                                   #selects specific variable names based on focal groups, takes arguments of model output, focal groups
  n=names(data)
  filter=c()
  for(g in groups){
    if(g %in% n){
      if(match(g,n)==1){Var=c(g)}
      else{Var=c(g,n[match(g,n)-1])}}
    else{filter=c(filter,g)}}
  return(list('var'=Var,'filter'=filter))}


sumbayes=function(data){                                                        #creates summary aggregation of model output
  l=1+length(data[[4]][[2]])
  p=list()
  for(i in 1:l){p[[i]]=data[[5]][,i]}
  a=with(data[[5]],aggregate(response,p,pd))
  a$mean=with(data[[5]],aggregate(response,p,mean))$x
  a$CIl=with(data[[5]],aggregate(response,p,function(x) quantile(x,0.025)))$x
  a$CIu=with(data[[5]],aggregate(response,p,function(x) quantile(x,0.975)))$x
  a=setNames(a,c(names(data[[5]])[-c(length(data[[5]]))],
                 'PD','mean','CI_lower','CI_upper'))
  return(a)}

intbayes=function(data,name){                                                   #creates summary aggregation of interaction output
  l=1+length(data[[4]][[2]])
  p=list()
  d=data[[5]][data[[5]][,name]=='Difference',]
  for(i in 1:l){p[[i]]=d[,i]}
  a=with(d,aggregate(response,p,pd))
  a$mean=with(d,aggregate(response,p,mean))$x
  a$CIl=with(d,aggregate(response,p,function(x) quantile(x,0.025)))$x
  a$CIu=with(d,aggregate(response,p,function(x) quantile(x,0.975)))$x
  return(a)}

cello=function(data,var,s,label='none',lsize=1){                                          #create single violin plot with CI bars, takes argument of model output, focal variables, scale
  df=setNames(as.data.frame(matrix(ncol=5)),c('variable','y2.5','y97.5','y50','lab'))
  for(i in unique(data[,var$var[1]])){
    q=data$response[data[,var$var[1]]==i]
    df=rbind(df,data.frame('variable'=i,'y2.5'=quantile(q,0.05),
                           'y97.5'=quantile(q,0.95),'y50'=quantile(q,.5),
                           'lab'=paste0(sign(median(q))*round(100*pd(q),1),'%')))}
  g=ggplot(data)+
    geom_hline(yintercept=0,size=2*s)+
    geom_violin(aes_string(x=var$var[1],y='response',
                           fill=var$var[1]),size=s,scale='width')+
    geom_boxplot(data=na.omit(df),width=.05*s,lwd=s,
                 aes(x=as.factor(variable),
                     ymin = y2.5, lower = y2.5, middle = y50, 
                     upper = y97.5, ymax = y97.5),stat = "identity")
  if(label!='none'){g=g+geom_text(data=na.omit(df),aes(label=lab,
                                                     x=as.factor(variable),
                                                     y=label),size=lsize)}
    
  return(g)}
  
cello2=function(data,var,s){                                                    #create multi-violin plots with CI bars, takes argument of model output, focal variables, scale
  df=setNames(as.data.frame(matrix(ncol=5)),
              c(var$var[1],'v2','y2.5','y97.5','y50'))
  for(i in unique(data[,var$var[1]])){
    for(j in unique(data[,var$var[2]])){
      q=data$response[data[,var$var[1]]==i&data[,var$var[2]]==j]
      df=rbind(df,setNames(data.frame('v'=i,'v2'=j,'y2.5'=quantile(q,0.05),
                                      'y97.5'=quantile(q,0.95),
                                      'y50'=quantile(q,.5)),
                           c(var$var[1],'v2','y2.5','y97.5','y50')))}}
  g=ggplot(data)+
    geom_hline(yintercept=0,size=2*s)+
    facet_grid(reformulate(var$var[1]))+
    geom_violin(aes_string(x=var$var[2],y='response',
                           fill=var$var[2]),size=s,scale='width')+
    geom_boxplot(data=na.omit(df),width=.05*s,lwd=s,position=position_dodge(.9),
                 aes(x=as.factor(v2),group=as.factor(v2),
                     ymin = y2.5, lower = y2.5, middle = y50, 
                     upper = y97.5, ymax = y97.5),stat = "identity")
  return(g)}

mgroup=function(data,groups){                                                   #creates data groupings based on focal group names, takes arguments of model output, focal groups
  k=0
  for(g in groups){
    if(k==0){h=hbmgroup(data,c(g));k=1}
    else{h=rbind(h,hbmgroup(data,c(g)))}}
  return(h)}

bass=function(data,groups,fill='lower',s=1,label='none',lsize=1){               #create groups cello plots, takes arguments of model output, focal groups, fill group, scale
  out=list()
  for(g in groups){
    h=data[regexpr(g,data$lower)>0,]
    v=hbmvar(h,c(fill))
    if(length(v$var)==1){out[[g]]=cello(h,v,s,label=label,lsize=lsize)}
    else{if(v$var[2]=='upper'){out[[g]]=cello(h,v,s,label=label,lsize=lsize)}
         else{out[[g]]=cello2(h,v,s)}}}
  return(out)}

semb=function(model,data,dir='model.jags'){                                     #write and compute Bayesian structural equation models, takes arguments of model string, data, directory to save to
  mc=match.call(expand.dots=T)
  data=as.data.frame(data)
  rows=str_split(model,'\n')[[1]]
  row=c();lefts=c();rights=c();old=c()
  for(r in 1:length(rows)){
    item=c(LETTERS,letters)[r]
    rights=c(rights,paste(item,'[1]',sep=''))
    groups=str_split(rows[r],'~')[[1]]
    left=paste(groups[1],'[i]',sep='')
    lefts=c(lefts,left)
    old=c(old,groups[1])
    Right=str_split(groups[2],'\\+')[[1]]
    new=c(paste(item,'[1]',sep=''))
    
    for(i in 1:length(Right)){
      old=c(old,Right[i])
      new=c(new,paste(item,'[',i+1,']*y.',Right[i],'[i]',sep=''))
      rights=c(rights,paste(item,'[',i+1,']',sep=''))}
    right=paste(new,collapse=' + ')
    row=c(row,paste(left,right,sep=' = '))}
  
  row=paste(row,collapse='\n')
  upper=paste('model {\nfor (i in 1:N){',row,'}',sep='\n')
  new=c()
  for(l in 1:length(lefts)){
    base=gsub('\\[i\\]','',lefts[l])
    new=c(new,paste('y.',lefts[l],'~dnorm(',lefts[l],',tau[',l,'])',sep=''),
          paste('y.',base,'.sim[i]~dnorm(',lefts[l],',tau[',l,'])',sep=''),
          paste('y.',base,'.res[i]=y.',lefts[l],'-',lefts[l],sep=''),
          paste('y.',base,'.sres[i]=y.',base,'.sim[i]-',lefts[l],sep=''))}
  
  row=paste(new,collapse='\n')
  middle=paste('for (i in 1:N){',row,'}',sep='\n')
  new=c()
  for(r in rights){new=c(new,paste(r,'dnorm(0.0,0.01)',sep=' ~ '))}
  
  lower=paste(new,collapse='\n')
  bottom=paste('for (j in 1:',
               length(lefts),
               '){\nsigma[j] ~ dgamma(1,1)\ntau[j] <- pow(sigma[j], -2)\n}\n}',
               sep='')
  new=c();parameters=c()
  for(l in 1:length(lefts)){
    base=gsub('\\[i\\]','',lefts[l])
    new=c(new,paste('y.',
                    base,'.fit=sum(pow(y.',base,'.res[],2)/tau[',l,'])',sep=''),
          paste('y.',
                base,'.sfit=sum(pow(y.',base,'.sres[],2)/tau[',l,'])',sep=''))
    parameters=c(parameters,
                 paste('y.',base,'.sfit',sep=''),
                 paste('y.',base,'.fit',sep=''))}
  tail=paste(new,collapse='\n')
  formula=paste(upper,middle,lower,tail,bottom,sep='\n')
  
  sink(dir)
  cat(formula,fill=T)
  sink()
  
  model_data=list('N'=nrow(data))
  for(o in unique(old)){model_data=c(model_data,
                                     setNames(list(data[,o]),
                                              paste('y.',o,sep='')))}
  
  inits <- function(){list(sigma = rep(1,length(lefts)))}  
  parameters=c(parameters,c(LETTERS,letters)[1:length(lefts)])
  
  mod=jags(model_data, inits, parameters, "model.jags", 
           n.chains = 3, n.thin = 5, n.iter = 10000, n.burnin = 2000, 
           parallel = F)
  cat('\nMaking trace plots\n')
  traces(mod,paste(as.character(mc),'.csv',sep=''))
  return(mod)}

fits=function(mod,...){                                                         #summaries fit measures for Bayesian models, takes arguments of one or more model outputs
  mc=match.call(expand.dots=T)
  mods=c(list(mod),list(...))
  out=data.frame('response'=NA,'ppp'=NA,'DIC'=NA,
                 'names'=NA,'intercept'=NA,'slope'=NA,'r'=NA)
  if(length(mods)>1){
    for(m in 1:length(mods)){
      mc=gsub('^.+ = ','c(',mc)                                                 #bizarre functionality, if this function breaks it's probably this line
      mc=mc[mc!='fits']
      new=fits(mods[[m]])
      new$names=rep(mc[m],nrow(new))
      out=rbind(out,new)}
    out=na.omit(out)}
  else{
    sims=mod$sims.list
    groups=c()
    for(n in names(sims)){
      if(regexpr('\\.s?fit$',n)<=0){sims[[n]]=NULL}
      else{groups[n]=gsub('\\.s?fit$','',n)}}
    DIC=ifelse(mod$calc.DIC,mod$DIC,'NA')
    n=names(groups)
    par(mfrow=c(1,ceiling(length(n)/2)))
    g=1;for(i in 1:(length(n)/2)){
      sims=mod$sims.list
      x=sims[[n[g+1]]];y=sims[[n[g]]]
      l=lm(y~x)
      out=rbind(out,data.frame('response'=groups[n[g]],
                               'ppp'=pp.check(mod,n[g+1],n[g]),
                               'DIC'=DIC,'names'='model','intercept'=coef(l)[1],
                               'slope'=coef(l)[2],r=summary(l)$r.squared))
      g=g+2}
    par(mfrow=c(1,1))}
  return(na.omit(out))}

ddic=function(mod){                                                             #compares model fits, DIC for bsems, takes argument of frame of fit outputs
  if(class(mod)!='data.frame'){cat('Not output of fits()\n')}
  else{
    if(length(unique(mod$names))<2){cat('Only one model input\n')}
    dics=unique(mod$DIC)
    n=unique(mod$names)
    out=data.frame('name'=NA,'delta_DIC'=NA)
    used=c()
    for(i in 1:length(n)){
      used=c(used,i)
      for(j in 1:length(dics)){
        if(i!=j&!(j %in% used)){
        out=rbind(out,data.frame('name'=paste(n[i],n[j],sep=' - '),
                                 'delta_DIC'=dics[i]-dics[j]))}}}
    return(na.omit(out))}}

expand=function(x){o=c();for(i in x){o=c(o,1:i)};return(o)}                     #returns merged vector of 1:x for each x in a vector

gap=function(l,u,count){                                                        #returns missing values, takes argument of lower range vector, upper value, count
  o=c(0);n=u[1];i=1
  for(q in 2:length(l)){
    if(l[q]==0&(l[q-1]==count[i]|l[q-1]==0)){i=i+1;n=u[i];o=c(o,0)}
    else{o=c(o,n)}}
  return(o)}

repframe=function(d,n){                                                         #acts like rep() for data.frames, takes arguments of data.frame, count
  out=setNames(as.data.frame(matrix(nrow=n,ncol=length(d))),names(d))
  for(i in 1:length(d)){out[,i]=rep(d[,i],n)}
  return(out)}

format=function(sims,scales,model,filter,vars){                                 #format hbm output when modeled as differences from mean, takes arguments of model sims list, scales , model, and name filter
  name=names(sims)
  A=name[regexec('[aA]lpha',name)>0]
  B=name[regexec('beta',name)>0]
  l=list()
  for(n in vars){l[[n]]=(model[paste('N',n,sep='')])[[1]]}

  left=setNames(as.data.frame(matrix(nrow=sum(l[[length(l)]]),ncol=length(l))),
                vars)
  
  if(length(l)>1){
    for(i in length(l):2){
      if(sum(!is.na(left[[i]]))==0){
        left[i]=expand(l[[i]])
        left[i][left[i]==1]=0}
      left[i-1]=gap(left[,i],expand(l[[i-1]]),as.numeric(l[i][[1]]))}
    left[length(left)][left[length(left)]==0]=1
    for(i in 1:(length(l)-1)){
      left[,i][left[,i]==0]=expand(l[[i]])}
    for(i in 1:(length(left)-1)){
      first=unique(left[1:i])
      for(q in 1:(length(left)-i)){first=cbind(first,rep('all',length(first)))}
      left=rbind(setNames(first,names(left)),left)}
    left=rbind(setNames(rep('all',length(left)),names(left)),unique(left))
    for(i in names(left)){
      left=rbind(left[left[,i]=='all',],
                 left[left[,i]!='all',][order(
                   as.numeric(left[,i][left[,i]!='all'])),])}}
  if(length(l)==1){
    left[1]=expand(l[[1]])
    left=rbind(setNames(data.frame('all'),names(left)),left)}
  p=list()

  out=setNames(as.data.frame(matrix(ncol=3+length(left))),
               c(names(left),c('upper','lower','response')))
  counts=list();medians=list()
  betas=B[regexpr(paste(paste('_',names(left),'$',sep=''),collapse='|'),B)<0]
  cat('\nformating slopes\n')
  for(b in betas){
    Bs=B[regexpr(b,B)>0]
    for(i in 1:nrow(left)){
      col=names(left)[regexpr('all',left[i,])<0]
      if(length(col)>0){
        lup=tryCatch(left[i,col[length(col)-1]],error=function(e) return('all'))
        if(length(lup)==0){lup='all'}
        llo=tryCatch(left[i,col[length(col)]],error=function(e) return('all'))
        upp=tryCatch(Bs[regexpr(col[length(col)-1],Bs)>0],
                     error=function(e) return('beta_all'))
        col=paste('_',col,'$',sep='')
        lab=Bs[regexpr(col[length(col)],Bs)>0]

        for(slab in lab){
          if(!(slab %in% names(counts))){counts[[slab]]=0}
          counts[[slab]]=counts[[slab]]+1
          right=data.frame('upper'=upp,
                      'lower'=lab,
                      'response'=medians[[lup]]+
                      as.numeric(as.data.frame(sims[slab])[counts[[slab]]][,1]))
          if(length(col)<length(left)){
            medians[[llo]]=right$response}
          if(length(left)>1){out=rbind(out,
                              cbind(repframe(left[i,],nrow(right)),right))}
          else{out=rbind(out,cbind(
            setNames(as.data.frame(rep(left[i,],nrow(right))),
                     names(left)),right))}}
      }
      else{
        right=data.frame('upper'='beta_all','lower'=Bs[1],'response'=sims[[Bs[[1]]]])
        if(length(left)>1){out=rbind(out,cbind(repframe(left[i,],nrow(right)),right))}
        else{out=rbind(out,cbind(setNames(as.data.frame(rep(left[i,],nrow(right))),names(left)),right))}
        medians[['all']]=right$response[right$upper=='beta_all']}}
    progress(match(b,betas)/length(betas),50)}
  counts=list();medians=list()
  alphas=A[regexpr(paste(paste('_',names(left),'$',sep=''),collapse='|'),A)<0]
  cat('\nformating intercepts\n')
  for(a in alphas){
    Bs=A[regexpr(a,A)>0]
    for(i in 1:nrow(left)){
      col=names(left)[regexpr('all',left[i,])<0]
      if(length(col)>0){
        lup=tryCatch(left[i,col[length(col)-1]],error=function(e) return('all'))
        if(length(lup)==0){lup='all'}
        llo=tryCatch(left[i,col[length(col)]],error=function(e) return('all'))
        upp=tryCatch(Bs[regexpr(col[length(col)-1],Bs)>0],error=function(e) return('alpha_all'))
        col=paste('_',col,'$',sep='')
        lab=Bs[regexpr(col[length(col)],Bs)>0]
        for(slab in lab){

          if(!(slab %in% names(counts))){counts[[slab]]=0}
          counts[[slab]]=counts[[slab]]+1

          right=data.frame('upper'=upp,
                           'lower'=lab,
                           'response'=medians[[lup]]+
                             as.numeric(as.data.frame(sims[slab])[counts[[slab]]][,1]))
          if(length(col)<length(left)){
            medians[[llo]]=right$response}
          if(length(left)>1){out=rbind(out,cbind(repframe(left[i,],nrow(right)),right))}
          else{out=rbind(out,cbind(setNames(as.data.frame(rep(left[i,],nrow(right))),names(left)),right))}}
      }
      else{
        right=data.frame('upper'='alpha_all','lower'=Bs[1],'response'=sims[[Bs[[1]]]])
        if(length(left)>1){out=rbind(out,cbind(repframe(left[i,],nrow(right)),right))}
        else{out=rbind(out,cbind(setNames(as.data.frame(rep(left[i,],nrow(right))),names(left)),right))}
        medians[['all']]=right$response[right$upper=='alpha_all']}}
    progress(match(a,alphas)/length(alphas),50)}
  out=na.omit(out)#[c(T,rep(F,99)),] #culling
  out$response[out$lower %in% A]=out$response[out$lower %in% A]*as.numeric(scales[1])
  cat('\nbacktransforming data\n')
  for(sb in names(scales[-1])){
    out$response[regexpr(sb,out$lower)>0]=out$response[regexpr(sb,out$lower)>0]*as.numeric(scales[1])/as.numeric(scales[sb])
    progress(match(sb,names(scales[-1]))/length(names(scales[-1])),50)}
  filtered=out
  for(i in 1:nrow(filter)){
    filtered[,1][out[,1]==filter[,1][i]]=filter[,3][i]
  }
  return(filtered)}

hbm2=function(data,model,params=list('dist'='dnorm','model'=''),...){           #writes and computes hbms, takes arguments of data, model, parameters for jags
  if(class(data)!="data.frame"){cat('Improper dataframe');return(NULL)}
  if(class(model)!="formula"){cat('Improper model');return(NULL)}
  cat('\nwriting model\n')
  mc=match.call(expand.dots=T)

  dist=ifelse('dist'%in%names(params),params$dist,'dnorm')
  Model=ifelse('model'%in%names(params),params$model,'')
  dir=ifelse('dir'%in%names(params),params$dir,paste((paste(mc,collapse = '_')),'.csv'))
  model_dir=ifelse('model_dir'%in%names(params),params$model_dir,paste((paste(mc,collapse = '_')),'.txt',sep=''))
  l=list(...)
  if(is.null(l$n.adapt)){l$n.adapt=2000}
  if(is.null(l$n.burnin)){l$n.burnin=1000}
  if(is.null(l$n.iter)){l$n.iter=10000}
  if(is.null(l$n.chains)){l$n.chains=4}
  
  names=c(as.character(mc$model)[2],strsplit(gsub('\\((.+)\\)','\\1',as.character(mc$model)[3]),' \\+ ')[[1]])
  new_i='i'
  while(T){                                                                     #if i in variable names generate replacement variable for iterations in model
    if(new_i%in%names){new_i=paste(new_i,sample(letters,1),sep='')}
    else{break}}
  new=c(stringr::str_interp('${as.character(mc$model)[2]}[i]~${dist}(mu[i],tau)'),
        stringr::str_interp('${as.character(mc$model)[2]}.sim[i]~${dist}(mu[i],tau)'),
        stringr::str_interp('${as.character(mc$model)[2]}.res[i]=${as.character(mc$model)[2]}[i]-mu[i]'),
        stringr::str_interp('${as.character(mc$model)[2]}.sres[i]=${as.character(mc$model)[2]}.sim[i]-mu[i]'))
  
  left=paste(new,collapse='\n\t')
  vars=strsplit(gsub(',$','',gsub(' \\+ ',',',gsub(')','',strsplit(as.character(mc$model)[3],'\\(')[[1]]))),',')
  var1_terms=c();int_terms=list()
  for(q in vars[[1]]){
    if(regexpr(':',q)>0){
      split=strsplit(q,':')[[1]]
      var1_terms=c(var1_terms,split[1])
      if(split[1]%in%names(int_terms)){int_terms[[split[1]]]=c(int_terms[[split[1]]],split[-1])}
      else{int_terms=c(int_terms,setNames(list(split[-1]),split[1]))}
    }
    else{var1_terms=c(var1_terms,q)}
  }
  vars[[1]]=var1_terms

  for(q in vars[[2]]){
    if(q!=''){data[q]=factor(data[,q],levels=unique(data[,q]))}}
  
  data=data[order(data[vars[[2]][1]]),] ####this one probably gives the xtfrm warning
  rv=is=c();c=0
  vars[[2]]=tryCatch(c('',vars[[2]]),error=function(e){c('')})
  x=paste(paste(paste(c('',rep('[',length(vars[[2]])-1)),vars[[2]],sep=''),'[i]',sep=''),c('',rep(']',length(vars[[2]])-1)),sep='')
  for(q in vars[[2]]){
    c=c+1;is=c(is,paste(c('',x[-1])[1:c],collapse=''))
    rv=c(rv,paste('alpha',q,sep='_'))}
  for(w in vars[[1]]){for(q in vars[[2]]){rv=c(rv,paste('beta',w,q,sep='_'))}}
  rv=chunk(gsub('_$','[i]',paste(rv,is,sep='')),length(vars[[2]]))
  right=c()
  rv=unique(rv)
  c=0;for(q in rv[-1]){

    term=gsub('beta_','',gsub('\\[.+\\]','',q))[1]

    if(term%in%names(int_terms)){
      ints=c()
      for(item in int_terms[[term]]){
        int_q=gsub('beta_',paste('beta_int_',item,sep=''),q)

        rv=c(rv,list(int_q))
        ints=c(ints,stringr::str_interp("(${paste(int_q,collapse='+')})*${item}[i][i]"))}
      c=c+1;right=c(right,stringr::str_interp("(${paste(paste(q,collapse='+'),paste(ints,collapse='+'),sep='+')})*${vars[[1]][c]}[i]"))
    }
    else{
    c=c+1;right=c(right,stringr::str_interp("(${paste(q,collapse='+')})*${vars[[1]][c]}[i]"))}
  }
  rv=unique(rv)
  right=gsub('\\[i\\]\\)',')',gsub('\\[i\\]\\+','+',right))
  rn=gsub('\\[i\\]$','',rv[[1]])
  rn_ints=list()
  simple=rn
  for(term in int_terms){
    for(item in unique(term)){
      int=gsub('^alpha',paste('Alpha_int_',item,sep=''),simple)
      rn_ints=c(rn_ints,list(int))
      int=paste('(',paste(int,collapse='+'),')*',item,'[i]',sep='')
      rn=c(rn,paste(int))}}
  top=stringr::str_interp("for(i in 1:N){\n\t${left}\n\tmu[i]=${paste(rn,collapse='+')}+\n\t${paste(right,collapse='+\n\t')}}")
  s=t=c()
  rv=c(rv,rn_ints)

  for(q in vars[[2]]){
    snam='sigma';tnam='tau'
    s=c(s,paste(snam,q,sep='_'))
    t=c(t,paste(tnam,q,sep='_'))}
  s=gsub('_$','',s);t=gsub('_$','',t)
  bottom=paste(paste(s,'~dunif(0,100)\n',t,'=1/(',s,'*',s,')',sep=''),collapse='\n')
  if(q!=''){bottom=paste(bottom,paste(paste('a',s,'~dunif(0,100)\n','a',t,'=1/(',s,'*',s,')',sep=''),collapse='\n'),sep='\n')}
  bottom=stringr::str_interp("${bottom}\n${paste(paste(gsub('\\\\[.+\\\\]','',as.data.frame(rv)[1,]),'~dnorm(0,0.01)',sep=''),collapse='\n')}")
  middle=c()
  if(length(vars[[2]])>1){for(q in 1:(length(vars[[2]])-1)){
    loop=stringr::str_interp('for(${letters[q]} in 1:N${vars[[2]][-1][q]}${c("",paste("[",letters,"]",sep=""))[q]}){')
    types=c()
    for(r in rv){
      if(regexpr('^[aA]lpha',r[1])>0){types=c(types,stringr::str_interp('~dnorm(0,a${t[q+1]})'))}
      else{types=c(types,stringr::str_interp('~dnorm(0,${t[q+1]})'))}
    }
    lines=gsub(is[q+1],paste(paste('[',letters[1:q],']',sep=''),collapse=''),
               paste(
                 paste(as.data.frame(rv)[q+1,],types,sep=''),
                 collapse='\n\t'),
               fixed=T)
    middle=c(middle,paste(loop,lines,sep='\n\t'))}}
  middle=paste(c(paste(middle,collapse='\n'),rep('}',length(vars[[2]])-1)),collapse='')
  #### derived terms ####
  tail=paste(c(stringr::str_interp('${as.character(mc$model)[2]}.fit=sum(pow(${as.character(mc$model)[2]}.res[],2)/tau)'),
         stringr::str_interp('${as.character(mc$model)[2]}.sfit=sum(pow(${as.character(mc$model)[2]}.sres[],2)/tau)')),collapse='\n')
  
  formula=gsub('\\]\\[',',',stringr::str_interp("model{\n${paste(top,middle,bottom,tail,sep='\n')}\n}"))
  #### model data ####
  model_data=list('N'=nrow(data))
  model_data[as.character(mc$model)[2]]=list(data[,as.character(mc$model)[2]])
  full_names=c()
  for(n in names){full_names=c(full_names,strsplit(n,':')[[1]])}
  names=unique(full_names)
  for(n in unique(c(vars[[1]],names))){
    if(!is.numeric(data[,n])){model_data[n]=list(as.numeric(as.factor(data[,n])))}
    else{model_data[n]=list(data[,n])}}
  convert=list()
  if(length(vars[[2]])>1){for(n in 2:length(vars[[2]])){
    model_data[vars[[2]][n]]=list(items(data,vars[[2]][2:n]))
    convert[[vars[[2]][n]]]=flip(model_data[[vars[[2]][n]]])
    model_data[paste('N',vars[[2]][n],sep='')]=list(ilen(data,vars[[2]][2:n]))}}
  scales=list()
  for(m in names){if(length(model_data[[m]])>1&!(m %in% vars[[2]])){
    if(m!='error'){
      scales[[m]]=attributes(scale(model_data[[m]],center=F))$scale
      model_data[[m]]=as.numeric(scale(model_data[[m]],center=F))}
    else{
      scales[[m]]=1
      model_data[[m]]=as.numeric(model_data[[m]])
    }}}
  
  formula=gsub('\\[i\\]',paste(c('\\[',new_i,'\\]'),collapse=''),formula)
  formula=gsub('for\\(i',paste(c('for\\(',new_i),collapse=''),formula)
  if(Model==''){writeLines(formula,model_dir);M=model_dir}
  else{M=Model}
  derived=c(stringr::str_interp('${as.character(mc$model)[2]}.sfit'),stringr::str_interp('${as.character(mc$model)[2]}.fit'))
  save=c(derived);for(i in rv){save=c(save,i)}
  save=gsub('\\[.+\\]','',save)

  mod=jagsUI::jags(data = model_data,
                   n.adapt=l$n.adapt,
                   n.burnin=l$n.burnin,
                   n.iter=l$n.iter,
                   n.chains=l$n.chains,
                   modules = "glm",
                   model.file = M,
                   parameters.to.save = save,
                   verbose = TRUE,
                   DIC=F)
  cat('\nmaking trace plots\n')
  traces(mod,gsub('\\.csv','_',dir),cull=9)
  cat('\nFormatting data\n')
  v=vars[[2]][-1]
  o=setNames(as.data.frame(matrix(nrow=model_data$N,ncol=2*length(v))),rep(v,2))
  for(i in 1:length(v)){
    o[i]=model_data[[v[i]]]
    o[i+2]=names(model_data[[v[i]]])}
  filter=unique(o)
  output=setNames(as.data.frame(matrix(ncol=3,nrow=0)),c('lower','upper','response'))
  sims=mod$sims.list
  for(n in names(sims)){if(regexpr('\\.s?fit$',n)>0){sims[[n]]=NULL}}
  output=format(sims,scales,model_data,filter,vars[[2]][-1])
  final=list(mod,model_data,mc,vars,output,scales,filter)
  write.csv(sumbayes(final),dir,row.names = F)
  for(n in names(mod$Rhat)){if(max(mod$Rhat[[n]],na.rm=T)>1.1){cat(rep('#',50),'\nRhat Greater than 1.1 in\n',paste(mc,collapse = ' '),'\n',rep('#',50),'\n',sep='');break}}
  return(final)}
