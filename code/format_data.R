total=read.csv('total.csv')                                                     #load in herb data
divp=read.csv('divp.csv')                                                       #load in diversity data

herb3=subset(total,!is.na(total$c_i))                                           #format herbivory data for flooded sites
herb3=subset(herb3,!is.na(herb3$flood))
herb3=subset(herb3,site!='mogi'&site!='Brazil')
herb3=herb3[!(herb3$site=='peru' & herb3$c_i=='clone'),]
herb3$clonen[herb3$c_i=='clone']=0
herb3$clonen[herb3$c_i=='ind']=1
herb3$floodn[herb3$flood=='no']=0
herb3$floodn[herb3$flood=='flood']=1
herb3=subset(herb3,!is.na(herb3$clonen))

herb5=subset(total,!is.na(total$c_i))                                           #format herbivory data for all sites
herb5=subset(herb5,!is.na(herb5$flood))
herb5=herb5[!(herb5$site=='peru' & herb5$flood=='no'),]
herb5$clonen[herb5$c_i=='clone']=0
herb5$clonen[herb5$c_i=='ind']=1
herb5$floodn[herb5$flood=='no']=0
herb5$floodn[herb5$flood=='flood']=1
herb5=subset(herb5,!is.na(herb5$clonen))

div3=subset(divp,!is.na(divp$c_i))                                              #format div data for flooded sites
div3=subset(div3,!is.na(div3$flood))
div3=subset(div3,site!='mogi'&site!='Brazil')
div3=div3[!(div3$site=='peru' & div3$c_i=='clone'),]
div3$clonen[div3$c_i=='clone']=0
div3$clonen[div3$c_i=='ind']=1
div3$floodn[div3$flood=='no']=0
div3$floodn[div3$flood=='flood']=1
div3=subset(div3,!is.na(div3$clonen))

div5=subset(divp,!is.na(divp$c_i))                                              #format div data for all sites
div5=div5[!(div5$site=='peru' & div5$flood=='no'),]
div5$clonen[div5$c_i=='clone']=0
div5$clonen[div5$c_i=='ind']=1
div5$floodn[div5$flood=='no']=0
div5$floodn[div5$flood=='flood']=1
div5=subset(div5,!is.na(div5$clonen))

mort5=read.csv('Mortality_full.csv')                                            #load in mortality data
mort5$flood[mort5$flood=='flood']=1;mort5$flood[mort5$flood=='no']=0            #format mortality data for all sites
mort5$alive=100*mort5$alive
mort5$pdiv=mort5%>%group_by(site)%>%mutate(div/max(div,na.rm=T))%>%pull()
mort3=subset(mort5,mort5$site!='mogi'&mort5$site!='Brazil')                     #create subset mortality data for flooded sites

var3=setNames(with(herb3,aggregate(wilmer_herb_total,list(site,                 #variance in herb for flooded sites
                                                          final_div,
                                                          pdiv,clonen,
                                                          floodn,plant,
                                                          species),var)),
        c('site','final_div','pdiv','clonen','floodn','plant','species','vari'))

var5=setNames(with(herb5,aggregate(wilmer_herb_total,list(site,                 #variance in herb for all sites
                                                          final_div,
                                                          pdiv,clonen,
                                                          plant,
                                                          species),var)),
              c('site','final_div','pdiv','clonen','plant','species','vari'))

herb3$site=factor(herb3$site,                                                   #proper site factors for all data.frames
                  levels=c('Costa Rica','Ecuador','peru'))
herb5$site=factor(herb5$site,
                  levels=c('Costa Rica','Ecuador','peru','mogi','Brazil'))

div3$site=factor(div3$site,
                 levels=c('Costa Rica','Ecuador','peru'))
div5$site=factor(div5$site,
                 levels=c('Costa Rica','Ecuador','peru','mogi','Brazil'))

var3$site=factor(var3$site,
                 levels=c('Costa Rica','Ecuador','peru'))
var5$site=factor(var5$site,
                 levels=c('Costa Rica','Ecuador','peru','mogi','Brazil'))

mort3$site=factor(mort3$site,
                  levels=c('Costa Rica','Ecuador','peru'))
mort5$site=factor(mort5$site,
                  levels=c('Costa Rica','Ecuador','peru','mogi','Brazil'))

mort5$block[is.na(mort5$block)]='none'                                          #NA blocks to 'none'
total$block[is.na(total$block)]='none'
divp$block[is.na(divp$block)]='none'
divp$flood[is.na(divp$flood)]='no'                                              #NA flood to 'no'
                                                                                #mean plot level mortality
mortp=setNames(with(mort5,aggregate(alive,list(site,block,plot,flood,c_i,div,pdiv),mean)),c('site','block','plot','flood','clone','div','pdiv','alive'))
                                                                                #median plot level herbivory
temp=setNames(with(total,aggregate(wilmer_herb_total,list(site,block,plot,flood,c_i,div,pdiv,plant_code),median)),c('site','block','plot','flood','clone','div','pdiv','plant','wilmer_herb_total'))
                                                                                #mean plot level herbivory
herbp=setNames(with(temp,aggregate(wilmer_herb_total,list(site,block,plot,flood,clone,div,pdiv),mean)),c('site','block','plot','flood','clone','div','pdiv','herbivory'))
                                                                                #count of groups in each plot
temp=with(total,aggregate(wilmer_herb_total,list(site,block,plot,flood,c_i,div,pdiv,plant_code),length))
                                                                                #mean leaf count 
herbp$nlvs=with(temp,aggregate(x,list(Group.1,Group.2,Group.3,Group.4,Group.5,Group.6,Group.7),mean))$x
                                                                                #total leaf count
herbp$nlvs=with(total,aggregate(wilmer_herb_total,list(site,block,plot,flood,c_i,div,pdiv),length))$x
                                                                                #herbivory measurement error for each plot
herbp$error=with(total,aggregate(wilmer_herb_total,list(site,block,plot,flood,c_i,div,pdiv),sd))$x
herbp$error[is.na(herbp$error)]=mean(herbp$error,na.rm=T)                       #mean error
herbp$flood[herbp$flood=='flood']=1;herbp$flood[herbp$flood=='no']=0            #flood proxy variables
div=setNames(with(divp,                                                         #plot level insect diversity
                  aggregate(a_div,list(site,block,plot,flood,c_i,div,pdiv),mean)),
             c('site','block','plot','flood','clone','div','pdiv','a_div'))
herbp$plot=gsub('Plot ','',herbp$plot)                                          #fix plot names
div$plot=gsub('Plot ','',div$plot)                                              
div$flood[div$flood=='flood']=1;div$flood[div$flood=='no']=0                    #more flood proxy variables

herbp$site=factor(herbp$site,levels=levels(mortp$site))                         #proper site factor levels
div$site=factor(div$site,levels=levels(mortp$site))                             
plotresp=merge(herbp,mortp,                                                     #merge herb, mort, div by plot into single data.frame
               by=c('site','block','plot','flood','clone','div','pdiv'))
plotresp=merge(plotresp,div,
               by=c('site','block','plot','flood','clone','div','pdiv'))
plotresp$flood=as.numeric(plotresp$flood)                                       #flood to numeric
plotresp$clone[plotresp$clone=='clone']=0                                       #clone proxy variables
plotresp$clone[plotresp$clone=='ind']=1
plotresp$clone=as.numeric(plotresp$clone)
plotresp=na.omit(plotresp)                                                      #remove NAs
plotresp$norm=plotresp%>%group_by(site)%>%mutate(alive-min(alive))%>%pull()     #nomralized variables
plotresp$norm=plotresp%>%group_by(site)%>%mutate(norm/max(norm))%>%pull()
plotresp$hnorm=plotresp%>%group_by(site)%>%mutate(herbivory-min(herbivory))%>%pull()
plotresp$hnorm=plotresp%>%group_by(site)%>%mutate(hnorm/max(hnorm))%>%pull()
mort5$c_i[mort5$c_i=='clone']=0;mort5$c_i[mort5$c_i=='ind']=1                   #mortality clone proxy variables
mort3$c_i[mort3$c_i=='clone']=0;mort3$c_i[mort3$c_i=='ind']=1

family$family=factor(family$family,                                             #proper family level site factors
                     levels=unique(family$family[order(family$order)]))
family=family[!is.na(family$family),]                                           #remove NAs
famsite=setNames(with(family,aggregate(site,list(family,                        #families by site
                                                 order,
                                                 site),length)),
                 c('Family','order','Site','count'))
famsite$percent=famsite%>%group_by(Site)%>%mutate(count/sum(count))%>%pull()    #percent presence of families
famsite$Site=factor(famsite$Site,                                               #site level factors
                    levels=c('Costa Rica','Ecuador','peru','Brazil','mogi')[5:1])
famsite=subset(famsite,!is.na(famsite$Family))                                  #remove NAs

a22=plotresp                                                                    #throwaway plotresp frame for testing
a22$flood=sapply(a22$flood,function(x) c('0'='No flood','1'='Flood')[as.character(x)])
a22$site=sapply(a22$site,function(x) c('Ecuador'='Ecuador','Costa Rica'='Costa Rica','peru'='Peru','Brazil'='Uaimii','mogi'='Mogi')[as.character(x)])
a22$site=factor(a22$site,levels=c('Ecuador','Costa Rica','Peru','Mogi','Uaimii'))

mortcurve=read.csv('Mortality_full.csv')                                        #load in mortality over time data
mortcurve$divf='High'                                                           #proper diversity names and factor levels
mortcurve$divf[mortcurve$div==1]='One'
mortcurve$divf[mortcurve$div==2]='Two'
mortcurve$divf=factor(mortcurve$divf,levels = c('One','Two','High'))
mortcurve$site=factor(mortcurve$site,                                           #proper site levels
                      levels=c('Costa Rica','Ecuador','peru','Brazil','mogi'))

morttime=read.csv('./files/updated mortalities.csv')                            #load in update mortality data
                                                                                #BSEM subseted and scaled data.frmaes
som1=as.data.frame(scale(morttime[morttime$site=='Costa Rica',][,c('div','clone','flood','herb','rich','surv_next')]))
som4=as.data.frame(scale(morttime[morttime$site=='mogi',][,c('div','clone','herb','rich','surv_next')]))
som5=as.data.frame(scale(morttime[morttime$site=='Brazil',][,c('div','clone','herb','rich','surv_next')]))

