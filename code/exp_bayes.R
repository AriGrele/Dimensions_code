opath='./hbm_output'
mpath='./models'
if(!(mpath%in%list.dirs())){dir.create(mpath)}                        #create model output folder, if needed
if(!(opath%in%list.dirs())){dir.create(opath)}                #create data output folder, if needed

set.seed(8139)                                                                  #seed chosen using runif
out=hbm(herb3,                                                                 #herbivory and flood
        wilmer_herb_total~pdiv+clonen+floodn+(site),
        name='herb3',
        dir=opath,
        model_dir=mpath)
herb_sdata=hbmgroup(out,c('floodn'))                                            #relevant site level output
f1=fits(out)                                                               #fit measures

out=hbm(herb5,                                                                 #herbivory and plant div
        wilmer_herb_total~pdiv+clonen+(site),
        name='herb5',
        dir=opath,
        model_dir=mpath)
herb_sdata=rbind(herb_sdata,mgroup(out,c('pdiv','clonen')))                     #relevant site level output
f2=fits(out)                                                               #fit measures

out=hbm(div5,                                                                  #herbivory and insect div
        herbivory~a_div+pdiv+clonen+(site),
        name='herbrich',
        dir=opath,
        model_dir=mpath)           
herb_sdata=rbind(herb_sdata,hbmgroup(out,c('a_div')))                           #relevant site level output
f3=fits(out)                                                               #fit measures

out=hbm(herb3,                                                                 #presence and flood
        pres~pdiv+clonen+floodn+(site),
        name='pres3',
        dir=opath,
        model_dir=mpath) 
pres_sdata=hbmgroup(out,c('floodn'))                                            #relevant site level output
f4=fits(out)                                                               #fit measures

out=hbm(herb5,                                                                 #presence and plant div
        pres~pdiv+clonen+(site),
        name='pres5',
        dir=opath,
        model_dir=mpath)
pres_sdata=rbind(pres_sdata,mgroup(out,c('pdiv','clonen')))
f5=fits(out)

out=hbm(div5,                                                                  #presence and insect div
        pres~a_div+pdiv+clonen+(site),
        name='presrich',
        dir=opath,
        model_dir=mpath)
pres_sdata=rbind(pres_sdata,hbmgroup(out,c('a_div')))
f6=fits(out)

out=hbm(var3,                                                                  #variance and flood
        vari~pdiv+clonen+floodn+(site),
        name='var3',
        dir=opath,
        model_dir=mpath) 
var_sdata=hbmgroup(out,c('floodn'))
f7=fits(out)
 
out=hbm(var5,                                                                  #variance and plant div
        vari~pdiv+clonen+(site),
        name='var5',
        dir=opath,
        model_dir=mpath)
var_sdata=rbind(var_sdata,mgroup(out,c('pdiv','clonen')))
f8=fits(out)

out=hbm(subset(div5,!is.na(div5$vari)),                                        #variance and insect div
        vari~a_div+pdiv+clonen+(site),
        name='varrich',
        dir=opath,
        model_dir=mpath)
var_sdata=rbind(var_sdata,hbmgroup(out,c('a_div')))
f9=fits(out)

mort5$fdiv=mort5$div;mort3$fdiv=mort3$div;plotresp$fdiv=plotresp$div            #fix mort div
out=hbm(mort3,                                                                 #mort and flood
        alive~pdiv+c_i+flood+(site),
        name='mort3',
        dir=opath,
        model_dir=mpath) 
mort_sdata=hbmgroup(out,c('flood'))
f10=fits(out)

out=hbm(mort5,                                                                 #mort and plant div
        alive~pdiv+c_i+(site),
        name='mort5',
        dir=opath,
        model_dir=mpath)
mort_sdata=rbind(mort_sdata,mgroup(out,c('c_i','pdiv')))
f11=fits(out)

out=hbm(plotresp,                                                              #site level mort and insect div
        alive~a_div+pdiv+clone+(site),
        name='mortrich',
        dir=opath,
        model_dir=mpath)
mort_sdata=rbind(mort_sdata,hbmgroup(out,c('a_div')))
f12=fits(out)

out=hbm(div3,                                                                  #insects and flood
        a_div~pdiv+clonen+floodn+(site),
        name='rich3',
        dir=opath,
        model_dir=mpath) 
div_sdata=hbmgroup(out,c('floodn'))
f13=fits(out)

out=hbm(div5,                                                                  #insects and plant div
        a_div~pdiv+clonen+(site),
        name='rich5',
        dir=opath,
        model_dir=mpath)
div_sdata=rbind(div_sdata,mgroup(out,c('pdiv','clonen')))
f14=fits(out)

#### interactions  ####
herb2=subset(herb3,!(herb3$site!='peru'))                                       #herbivory water interactions
out=hbm(herb3,
        wilmer_herb_total~floodn:pdiv+floodn:clonen+(site),
        n.iter=20000,
        n.burnin=5000,
        name='herbint',
        dir=opath,
        model_dir=mpath)
herbint=mgroup(out,c('clonen','pdiv'))
f16=fits(out)

out=hbm(herb3,                                                                 #pres water interactions
        pres~floodn:pdiv+floodn:clonen+(site),
        n.iter=20000,
        n.burnin=5000,
        name='presint',
        dir=opath,
        model_dir=mpath) 
presint=mgroup(out,c('clonen','pdiv'))
f17=fits(out)

var2=subset(var3,var3$site!='peru')                                             #variance water interactions
out=hbm(var3,
        vari~floodn:pdiv+floodn:clonen+(site),
        n.iter=20000,
        n.burnin=5000,
        name='varint',
        dir=opath,
        model_dir=mpath)  
varint=mgroup(out,c('clonen','pdiv'))
f18=fits(out)

out=hbm(mort3,                                                                 #survival water interactions
        alive~flood:pdiv+flood:c_i+(site),n.iter=20000,n.burnin=5000,
        name='mortint',
        dir=opath,
        model_dir=mpath)    
mortint=mgroup(out,c('c_i','pdiv'))
f19=fits(out)

div2=subset(div3,div3$site!='peru')                                             #insect water interactions
out=hbm(div3,
        a_div~floodn:pdiv+floodn:clonen+(site),
        n.iter=20000,
        n.burnin=5000,
        name='richint',
        dir=opath,
        model_dir=mpath)
divint=mgroup(out,c('clonen','pdiv'))
f20=fits(out)

files=list.files('./hbm_output')                                                #collect output files
files=files[regexpr('\\.csv$',files)>0]                                         #subset csvs
fgroup=list()
c=0
for(f in files){                                                                #merge files based on similar column names
  r=read.csv(paste('./hbm_output/',f,sep=''))
  r$filename=f
  stop=0
  for(g in names(fgroup)){
    if(min((names(r)==names(fgroup[[g]]))+0)==1){
      fgroup[[g]]=rbind(fgroup[[g]],r);stop=1}}
  if(stop==0){c=c+1;fgroup[[as.character(c)]]=r}}
for(g in names(fgroup)){write.csv(fgroup[[g]],
                                  paste('./hbm_output/merged',g,'.csv',sep=''),
                                  row.names = F)}

rbind(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f16,f17,f18,f19,f20)   #list all fit measures




herbout=with(herb_sdata,aggregate(response,list(site,lower),mean))
herbout$pd=with(herb_sdata,aggregate(response,list(site,lower),pd))$x
presout=with(pres_sdata,aggregate(response,list(site,lower),mean))
presout$pd=with(pres_sdata,aggregate(response,list(site,lower),pd))$x
varout=with(var_sdata,aggregate(response,list(site,lower),mean))
varout$pd=with(var_sdata,aggregate(response,list(site,lower),pd))$x
richout=with(div_sdata,aggregate(response,list(site,lower),mean))
richout$pd=with(div_sdata,aggregate(response,list(site,lower),pd))$x
mortout=with(mort_sdata,aggregate(response,list(site,lower),mean))
mortout$pd=with(mort_sdata,aggregate(response,list(site,lower),pd))$x

herbout$resp='Percent herbivory'
presout$resp='Presence of damage'
varout$resp ='Variance in herbivory'
richout$resp='Insect richness'
mortout$resp='Plant mortality'

write.csv(rbind(herbout,
                presout,
                varout,
                richout,
                mortout),'herb.csv',row.names = F);system2('open','herb.csv')
