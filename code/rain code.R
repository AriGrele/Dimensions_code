xe=c(-77.88,-0.60)                                                              #coordinates for ec, pe, ui, mg
xp=c(-75.09,-11.35)
xu=c(-43.57,-20.295)
xm=c(-47.16,-22.25)
                                                                                #URL for data api
baseurlagg="http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_ppt_1958_CurrentYear_GLOBE.nc"

nc=nc_open(baseurlagg)                                                          #grab url
lon=ncvar_get(nc, "lon")                                                        #get lat, long
lat=ncvar_get(nc, "lat")
flat = match(abs(lat - xe[2]) < 1/48, 1)                                        #set ec lat
latindex = which(flat %in% 1)
flon = match(abs(lon - xe[1]) < 1/48, 1)                                        #set ec long
lonindex = which(flon %in% 1)
start=c(lonindex, latindex, 1)                                                  #ec position
count=c(1, 1, -1)
var='ppt'
data=as.numeric(ncvar_get(nc, varid = var,start = start, count))                #data column
dataf=data.frame(year=rep(1:63,each=12)+1957,month = rep(1:12, 63), data)       #new data.frame of year, month, precip
dataf=subset(dataf,year<2017)                                                   #subset by year
dataf$group='all'
dataf$group[(dataf$year==2015|dataf$year==2016)&!                               #define experimental period
              (dataf$year==2015&dataf$month<7|
                 dataf$year==2016&dataf$month==12)]='exp'

nc=nc_open(baseurlagg)                                                          #grab url
lon=ncvar_get(nc, "lon")                                                        #get lat, long
lat=ncvar_get(nc, "lat")
flat = match(abs(lat - xp[2]) < 1/48, 1)                                        #set pe lat
latindex = which(flat %in% 1)
flon = match(abs(lon - xp[1]) < 1/48, 1)                                        #set pe long
lonindex = which(flon %in% 1)
start=c(lonindex, latindex, 1)                                                  #pe position
count=c(1, 1, -1)
p=as.numeric(ncvar_get(nc, varid = var,start = start, count))                   #data column
pe=data.frame(year=rep(1:63,each=12)+1957,month = rep(1:12, 63), p)             #new data.frame of year, month, precip
pe$group='all'
pe$group[(pe$year==2015|pe$year==2016|pe$year==2017)&!                          #define experimental period
           (pe$year==2015&pe$month<10)&!(pe$year==2017&pe$month>7)]='exp'

cr=subset(read.csv("./files/cr_rain.csv"),variable!='total')                    #load CR data
cr$variable=as.numeric(cr$variable)                                             #year as numeric
cr$group='all'
cr$group[(cr$year==2015|cr$year==2016|cr$year==2017)&!(cr$year==2015&cr$variable<3|cr$year==2017&cr$variable>1)]='exp'    #define experimental period

dataf$site='ec'                                                                 #set site names
cr$site='cr'
pe$site='pe'
#ui$site='ui'
#mg$site='mg'
dataf=setNames(dataf,c('year','month','precip','group','site'))                 #output data.frame
cr=setNames(cr,c('year','month','precip','group','site'))                       #make names consistent
pe=setNames(pe,c('year','month','precip','group','site'))

pe$precip[pe$year==2016&pe$month==2]=217                                        #correcting extreme precipitation outlier using data from nearby weather stations

data=rbind(dataf,cr,pe)                                                         #merge to single data.frame
data$m=NA
data[data$group=='exp',]$m=data[data$group=='exp',]%>%                          #final sampling date for experiment
  group_by(site)%>%mutate(max(year+month/12,na.rm=T))%>%pull()
data$m=data%>%group_by(site)%>%mutate(max(m,na.rm=T))%>%pull()                  #add to all rows
data=subset(data,year+month/12<=m)                                              #remove rows from after experiment end
data$group[data$group!='exp']='moose'                                           #throwaway grouping
data$group[data$year<1998]='all'                                                #define period for calculating norm

for(s in unique(data$site)){                                                    #monthly precip averages
  a=with(data[data$site==s&data$group=='all',],
         aggregate(precip,list(month),mean))
  for(i in 1:12){                                                               #monthly precip anomaly
    data$dif[data$site==s&data$month==i]=
      data$precip[data$site==s&data$month==i]-a$x[i]}}
data$dif=as.numeric(data$dif)                                                   #anomaly as numeric

rain=plot_grid(ggplot(data[data$group=='exp',],aes(x=site,y=precip,fill=site))+ #plot monthly precip in experimental period, monthly anomaly
                    geom_boxplot(outlier.alpha=0)+
                    guides(color='none',fill='none')+
                     theme_classic()+
                     theme(text=element_text(size=40))+
                     xlab('Study site')+
                     ylab('Monthly precipitation (mm)')+
                     scale_x_discrete(labels=c('CR','EC','PE'))+
                     scale_fill_manual(values=pals::ocean.speed(7)[6:2])+
                      ggtitle('A')+
                      geom_point(stat='summary',size=5),
                   ggplot(data[data$group=='exp',],aes(x=site,y=dif,fill=site))+
                     geom_boxplot(outlier.alpha=0)+
                     geom_hline(yintercept = 0)+
                     guides(color='none',fill='none')+
                     theme_classic()+
                     theme(text=element_text(size=40))+
                     xlab('Study site')+
                     ylab('Precipitation anomaly (mm)')+
                     scale_x_discrete(labels=c('CR','EC','PE'))+
                     scale_y_continuous(breaks = c(-200,0,200,400,600))+
                     coord_cartesian(ylim=c(-250,250))+
                     scale_fill_manual(values=pals::ocean.speed(7)[6:2])+
                     ggtitle('B')+
                     geom_point(stat='summary',size=5))

data[data$group=='exp',]=                                                       #order data by date
  data[data$group=='exp',][with(data[data$group=='exp',],
                                order(site,(month-1)/12+year)),]
data$total=NA                                                                   #empty column
data[data$group=='exp',]$total=data[data$group=='exp',]%>%                      #populate with cumulative precip
  group_by(site)%>%mutate(cumsum(precip))%>%pull()

data$av=NA                                                                      #empty column
data$av[data$group=='all']=data[data$group=='all',]%>%                          #populate with average precip by site, month, for experimental period
  group_by(site,month)%>%mutate(mean(precip,na.rm=T))%>%pull()
data$av=data%>%group_by(site,month)%>%mutate(max(av,na.rm=T))%>%pull()          #populate non-experimental period
data[data$group=='exp',]$av=data[data$group=='exp',]%>%                         #convert to cumulative average
  group_by(site)%>%mutate(cumsum(av))%>%pull()

data$sd=data$sdl=data$sdu=NA                                                    #three empty columns
data$sd[data$group=='all']=data[data$group=='all',]%>%                          #populate with 95% quantile for non-experimental
  group_by(site)%>%
  mutate((quantile(precip,.975,na.rm=T)-quantile(precip,.025,na.rm=T))/2)%>%
  pull()
data$sd=data%>%group_by(site)%>%mutate(max(sd,na.rm=T))%>%pull()                #populate experimental period

data[data$month==data$month[data$group=='exp'&data$site=='cr'][1],]             
data=data[order(with(data,year+(month-1)/12)),]                                 #order by date

av=data.frame("Group.1"=NA,'x'=NA,'av'=NA,'site'=NA,'eight'=NA,'five'=NA)       #empty frame for cumulative averages
for(site in c('cr','ec','pe')){                                                 #for each site:
  out=data.frame('group'=NA,'i'=NA,'sum'=NA)                                    #   define output data.frame
  for(i in 1:nrow(data[data$group=='all'&data$site==site,])){                   #   for each row in non-experimental period:
    if(data$month[data$group=='all'&data$site==site][i]==                       #   if experimental month == non-experimental month
       data$month[data$group=='exp'&data$site==site][1]){
      sub=data[data$group=='all'&                                               #       subset to equivalent of experimental period in non-experimental
               data$site==site,][i:(i+(nrow(
                 data[data$group=='exp'&data$site==site,])-1)),]
      sub$sum=cumsum(sub$precip)                                                #       cumulative precipitation
      if(sum(is.na(sub$precip)>0)){}                                            #       if precipitation exists: empty for time being
      else{out=rbind(out,data.frame('group'=i,                                  #       else: append data to frame
                                    'i'=1:nrow(data[data$group=='exp'&
                                                    data$site==site,]),
                                    'sum'=sub$sum))}}}
  
  out$i=((out$i-1+data$month[data$group=='exp'&data$site==site][1])-1)/12+      #   correct date in output
    data$year[data$group=='exp'&data$site==site][1]                             
  out$q=out%>%group_by(i)%>%                                                    #   95% quantile
    mutate((quantile(sum,.975,na.rm=T)-quantile(sum,.025,na.rm=T))/2)%>%pull()
  cra=with(out,aggregate(sum,list(i),                                           
               \(x)(quantile(x,.975,na.rm=T)-quantile(x,.025,na.rm=T))/2))      #   95% quantile by month
  cra$eight=with(out,aggregate(sum,list(i),                                     #   80% quantile by month
               \(x)(quantile(x,.9,na.rm=T)-quantile(x,.1,na.rm=T))/2))$x
  cra$five=with(out,aggregate(sum,list(i),                                      #   50% quantile by month
               \(x)(quantile(x,.75,na.rm=T)-quantile(x,.25,na.rm=T))/2))$x
  cra$av=with(out,aggregate(sum,list(i),                                        #   average precip by month
               \(x) mean(x,na.rm=T)))$x
  cra$site=site                                                                 #   set site name
  av=rbind(av,cra)}                                                             #   bind to average data.frame
pal=pals::ocean.speed(7)[6:2]|>col2rgb()                                        #set colors
colors=c()                                                                      #empty vector
for(i in 1:ncol(pal)){                                                          #for each color, calculate 95%,80%,50% quantile colors as overlapping regions of .25 alpha
  pal2=pal
  pal2[,1]=(pal[,i]-255)*.25+255
  pal2[,2]=(pal[,i]-pal2[,1])*.25+pal2[,1]
  pal2[,3]=(pal[,i]-pal2[,2])*.25+pal2[,2]
  colors=c(colors,
           rgb(pal2[1,1],pal2[2,1],pal2[3,1],maxColorValue=255),
           rgb(pal2[1,2],pal2[2,2],pal2[3,2],maxColorValue=255),
           rgb(pal2[1,3],pal2[2,3],pal2[3,3],maxColorValue=255))}

colors=c(colors,pals::ocean.speed(7)[6:2])                                      #append to color vector
colors=setNames(colors,c(paste(rep(1:3,3),                                      #set names to match factors in ggplot
                        rep(c('cr','ec','pe','ui','mg'),each=3),sep=''),
                        c('cr','ec','pe','ui','mg')))


cumm=ggplot()+                                                                  #plot of cumulative precipitations
  geom_ribbon(data=av,
              aes(ymin=av-x,ymax=av+x,x=Group.1,fill=paste(1,site,sep=''),
                  color=paste(1,site,sep='')))+
  geom_ribbon(data=av,
              aes(ymin=av-eight,ymax=av+eight,x=Group.1,fill=paste(2,site,sep=''),
                  color=paste(2,site,sep='')))+
  geom_ribbon(data=av,
              aes(ymin=av-five,ymax=av+five,x=Group.1,fill=paste(3,site,sep=''),
                  color=paste(3,site,sep='')))+
  geom_line(data=av,aes(x=Group.1,y=av,color=site),linetype='dashed',size=2)+
  geom_line(data=data[data$group=='exp',],
            aes(x=year+(month-1)/12,y=total,color=site),size=3)+
  theme_classic()+
  scale_color_manual(values=colors)+
  scale_fill_manual(values=colors,
                    breaks=c('cr','ec','pe'),
                    labels=c('CR','EC','PE'))+
  xlab('Year')+
  ylab('Cumulative precipitation (mm)')+
  guides(color='none')+
  labs(fill='Study site')+
  theme(text=element_text(size=35),
        legend.position="bottom")+
  ggtitle('C')
  

png('./figures/rain.png',1000,1000,type='cairo');rain;dev.off()                 #save images
png('./figures/cummulative.png',1000,1000,type='cairo');cumm;dev.off()
png('./figures/precipitation.png',2000,1000,type='cairo');plot_grid(rain,cumm,ncol=2);dev.off()
