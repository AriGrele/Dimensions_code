mortcurve=read.csv('Mortality_full.csv')                                        #load mort data
mortcurve$divf='High'                                                           #rename tax div levles
mortcurve$divf[mortcurve$div==1]='One'
mortcurve$divf[mortcurve$div==2]='Two'
mortcurve$divf=factor(mortcurve$divf,levels = c('One','Two','High'))            #proper tax div order
mortcurve$site=factor(mortcurve$site,
                      levels=c('Costa Rica','Ecuador','peru','Brazil','mogi'))  #proper site order

mortime=read.csv('mort_time.csv')                                               #load mort over time data
mortcurve$alive=1-mortcurve$alive                                               #survival -> death
mortcurve$full_mort=1-mortcurve$full_mort                                       
mortcurve$dif=0+(mortcurve$full_mort!=mortcurve$alive)

####costa Rica####
mortcr=mortcurve[mortcurve$site=='Costa Rica',]                                 #subset to site
mortcr$days=(as.numeric(as.POSIXct(as.Date(mortcr$date,format='%m/%d/%y')))-1420290061)/3600/24
#mortcr$days=as.numeric(mortcr$date)                                             #days as numeric of date
range(mortcr$days,na.rm=T) 
mortcr$days[is.na(mortcr$days)]=729                                             #final day value
mortcr=mortcr[order(mortcr$days),]                                              #order by day
mortcr=mortcr[order(mortcr$c_i,mortcr$flood),]                                  #order by variables
mortcr$cumort=mortcr%>%group_by(c_i,flood)%>%                                   #cumulative mortality
  mutate(cumsum(alive)/length(alive)*100)%>%pull()
mortcr$cudmort=mortcr%>%group_by(c_i,flood)%>%                                  
  mutate(cumsum(dif)/length(dif)*100)%>%pull()
mortecsub=rbind(mortcr[c('days','cumort','c_i','flood')],                       #new cumulative data.frame
                data.frame('days'=0,'cumort'=0,'c_i'=rep(c('clone','ind'),each=2),'flood'=rep(c('flood','no'),2)))
cr1=ggplot(mortecsub,aes(x=days,y=100-cumort,color=paste(c_i,flood)))+          #plot survival curve
  geom_step(size=6)+ylim(0,100)+
  theme_classic()+
  xlab('Days since experiment start')+
  ylab('% survival')+
  labs(color='Intraspecific diversity')+
  scale_color_manual(values=pals::ocean.matter(4))+
  ggtitle('% Survival over time')

mortcr=mortcr[order(mortcr$days),]                                              #repeat for div+flood
mortcr=mortcr[order(mortcr$div,mortcr$flood),]
mortcr$cumort=mortcr%>%group_by(div,flood)%>%mutate(cumsum(alive)/length(alive)*100)%>%pull()
mortcr$cudmort=mortcr%>%group_by(div,flood)%>%mutate(cumsum(dif)/length(dif)*100)%>%pull()
mortecsub=rbind(mortcr[c('days','cumort','div','flood')],
                data.frame('days'=0,'cumort'=0,'div'=rep(unique(mortcr$div),each=2),'flood'=rep(c('flood','no'),length(unique(mortcr$div)))))
cr2=ggplot(mortecsub,aes(x=days,y=100-cumort,color=paste(div,flood)))+
  geom_step(size=6)+ylim(0,100)+
  theme_classic()+
  xlab('Days since experiment start')+
  ylab('% survival')+
  labs(color='Intraspecific diversity')+
  scale_color_manual(values=pals::ocean.matter(6))+
  ggtitle('% Survival over time')

####ecuador####
mortec=mortcurve[mortcurve$site=='Ecuador',]
mortec$days=(as.numeric(as.POSIXct(as.Date(mortec$date,format='%m/%d/%y')))-1436502748)/3600/24
range(mortec$days,na.rm=T)
mortec$days[is.na(mortec$days)]=520
mortec=mortec[order(mortec$days),]
mortec=mortec[order(mortec$c_i,mortec$flood),]
mortec$cumort=mortec%>%group_by(c_i,flood)%>%mutate(cumsum(alive)/length(alive)*100)%>%pull()
mortec$cudmort=mortec%>%group_by(c_i,flood)%>%mutate(cumsum(dif)/length(dif)*100)%>%pull()
mortecsub=rbind(mortec[c('days','cumort','c_i','flood')],data.frame('days'=0,'cumort'=0,'c_i'=rep(c('clone','ind'),each=2),'flood'=rep(c('flood','no'),2)))
ec1=ggplot(mortecsub,aes(x=days,y=100-cumort,color=paste(c_i,flood)))+
  geom_step(size=6)+ylim(0,100)+
  theme_classic()+
  xlab('Days since experiment start')+
  ylab('% survival')+
  labs(color='Treatment pair')+
  scale_color_manual(values=pals::ocean.matter(4))+
  ggtitle('% Survival over time')

mortec=mortec[order(mortec$days),]
mortec=mortec[order(mortec$div,mortec$flood),]
mortec$cumort=mortec%>%group_by(div,flood)%>%mutate(cumsum(alive)/length(alive)*100)%>%pull()
mortec$cudmort=mortec%>%group_by(div,flood)%>%mutate(cumsum(dif)/length(dif)*100)%>%pull()
mortecsub=rbind(mortec[c('days','cumort','div','flood')],
                data.frame('days'=0,'cumort'=0,'div'=rep(unique(mortec$div),each=2),'flood'=rep(c('flood','no'),length(unique(mortec$div)))))
ec2=ggplot(mortecsub,aes(x=days,y=100-cumort,color=paste(div,flood)))+
  geom_step(size=6)+ylim(0,100)+
  theme_classic()+
  xlab('Days since experiment start')+
  ylab('% survival')+
  labs(color='Treatment pair')+
  scale_color_manual(values=pals::ocean.matter(6))+
  ggtitle('% Survival over time')

###Peru####
mortpe=mortcurve[mortcurve$site=='peru',]

mortpe$days=(as.numeric(as.POSIXct(as.Date(mortpe$date,format='%m/%d/%y')))-1443659857)/3600/24
range(mortpe$days,na.rm=T)
mortpe$days[is.na(mortpe$days)]=639
mortpe=mortpe[order(mortpe$days),]
mortpe=mortpe[order(mortpe$c_i,mortpe$flood),]
mortpe$cumort=mortpe%>%group_by(c_i,flood)%>%mutate(cumsum(alive)/length(alive)*100)%>%pull()
mortpe$cudmort=mortpe%>%group_by(c_i,flood)%>%mutate(cumsum(dif)/length(dif)*100)%>%pull()
mortecsub=rbind(mortpe[c('days','cumort','c_i','flood')],data.frame('days'=0,'cumort'=0,'c_i'=rep(c('clone','ind'),each=2),'flood'=rep(c('flood','no'),2)))
pe1=ggplot(mortecsub,aes(x=days,y=100-cumort,color=paste(c_i,flood)))+
  geom_step(size=6)+ylim(0,100)+
  theme_classic()+
  xlab('Days since experiment start')+
  ylab('% survival')+
  labs(color='Intraspecific diversity')+
  scale_color_manual(values=pals::ocean.matter(4))+
  ggtitle('% Survival over time')

mortpe=mortpe[order(mortpe$days),]
mortpe=mortpe[order(mortpe$div,mortpe$flood),]
mortpe$cumort=mortpe%>%group_by(div,flood)%>%mutate(cumsum(alive)/length(alive)*100)%>%pull()
mortpe$cudmort=mortpe%>%group_by(div,flood)%>%mutate(cumsum(dif)/length(dif)*100)%>%pull()
mortecsub=rbind(mortpe[c('days','cumort','div','flood')],
                data.frame('days'=0,'cumort'=0,'div'=rep(unique(mortpe$div),each=2),'flood'=rep(c('flood','no'),length(unique(mortpe$div)))))
pe2=ggplot(mortecsub,aes(x=days,y=100-cumort,color=paste(div,flood)))+
  geom_step(size=6)+ylim(0,100)+
  theme_classic()+
  xlab('Days since experiment start')+
  ylab('% survival')+
  labs(color='Interspecific diversity')+
  scale_color_manual(values=pals::ocean.matter(6))+
  ggtitle('% Survival over time')

###Uaimii####
mortui=mortcurve[mortcurve$site=='Brazil',]
mortui$days=(as.numeric(as.POSIXct(as.Date(mortui$date,format='%m/%d/%y')))-1519165404)/3600/24
range(mortui$days,na.rm=T)
mortui$days[is.na(mortui$days)]=668
mortui=mortui[order(mortui$days),]
mortui=mortui[order(mortui$c_i),]
mortui$cumort=mortui%>%group_by(c_i)%>%mutate(cumsum(alive)/length(alive)*100)%>%pull()
mortui$cudmort=mortui%>%group_by(c_i)%>%mutate(cumsum(dif)/length(dif)*100)%>%pull()
mortecsub=rbind(mortui[c('days','cumort','c_i')],data.frame('days'=0,'cumort'=0,'c_i'=c('clone','ind')))
ui1=ggplot(mortecsub,aes(x=days,y=100-cumort,color=c_i))+
  geom_step(size=6)+ylim(0,100)+
  theme_classic()+
  xlab('Days since experiment start')+
  ylab('% survival')+
  labs(color='Intraspecific diversity')+
  scale_color_manual(values=pals::ocean.matter(2))+
  ggtitle('% Survival over time')

mortui=mortui[order(mortui$days),]
mortui=mortui[order(mortui$div),]
mortui$cumort=mortui%>%group_by(div)%>%mutate(cumsum(alive)/length(alive)*100)%>%pull()
mortui$cudmort=mortui%>%group_by(div)%>%mutate(cumsum(dif)/length(dif)*100)%>%pull()
mortecsub=rbind(mortui[c('days','cumort','div')],data.frame('days'=0,'cumort'=0,'div'=unique(mortui$div)))
ui2=ggplot(mortecsub,aes(x=days,y=100-cumort,color=as.factor(div)))+
  geom_step(size=6)+ylim(0,100)+
  theme_classic()+
  xlab('Days since experiment start')+
  ylab('% survival')+
  labs(color='Interspecific diversity')+
  scale_color_manual(values=pals::ocean.matter(3))+
  ggtitle('% Survival over time')

####mogi####
mortmg=mortcurve[mortcurve$site=='mogi',]
mortmg$days=sapply(mortmg$date,function(x) c('one'=1,'two'=2,'three'=3,'four'=4,'final'=5)[x])*148
range(mortmg$days,na.rm=T)
mortmg$days[is.na(mortmg$days)]=740
mortmg=mortmg[order(mortmg$days),]
mortmg=mortmg[order(mortmg$c_i),]
mortmg$cumort=mortmg%>%group_by(c_i)%>%mutate(cumsum(alive)/length(alive)*100)%>%pull()
mortmgsub=rbind(mortmg[c('days','cumort','c_i')],data.frame('days'=0,'cumort'=0,'c_i'=c('clone','ind')))
mg1=ggplot(mortmgsub,aes(x=days,y=100-cumort,color=c_i))+
  geom_step(size=6)+ylim(0,100)+
  theme_classic()+
  xlab('Days since experiment start')+
  ylab('% survival')+
  labs(color='Intraspecific diversity')+
  scale_color_manual(values=pals::ocean.matter(2))+
  ggtitle('% Survival')

mortmg=mortmg[order(mortmg$days),]
mortmg=mortmg[order(mortmg$div),]
mortmg$cumort=mortmg%>%group_by(div)%>%mutate(cumsum(alive)/length(alive)*100)%>%pull()
mortmgsub=rbind(mortmg[c('days','cumort','div')],data.frame('days'=0,'cumort'=0,'div'=unique(mortmg$div)))
mg2=ggplot(mortmgsub,aes(x=days,y=100-cumort,color=as.factor(div)))+
  geom_step(size=6)+ylim(0,100)+
  theme_classic()+
  xlab('Days since experiment start')+
  ylab('% survival')+
  labs(color='Interspecific diversity')+
  scale_color_manual(values=pals::ocean.matter(3))+
  ggtitle('% Survival')

################################################################################
#### apply themes to plots ####
apal=pals::ocean.matter(5)[-5]
pelab=pe1+theme(text=element_text(size=40))+
  ggtitle('Peru % survival over time')+
  labs(color='Intraspecific diversity &\nwater regime combination')+
  scale_color_manual(values=c('clone flood'=apal[1],'clone no'=apal[2],'ind flood'=apal[3],'ind no'=apal[4]),
                     labels=c('Low diversity, water added','Low diversity, no water','High diversity, water added', 'High diversity, no water'),
                     guide=guide_legend(override.aes=list(shape=15,size=50)))
apal=pals::ocean.deep(7)[-7]
pelab2=pe2+theme(text=element_text(size=40))+
  ggtitle('Peru % survival over time')+
  labs(color='Interspecific diversity &\nwater regime combination')+
  scale_color_manual(values=c('1 flood'=apal[1],'1 no'=apal[2],'2 flood'=apal[3],'2 no'=apal[4],'6 flood'=apal[5],'6 no'=apal[6]),
                     labels=c('Low diversity, water added','Low diversity, no water','Medium  diversity, water added', 'Medium diversity, no water','High diversity, water added', 'High diversity, no water'),
                     guide=guide_legend(override.aes=list(shape=15,size=50)))

labels=plot_grid(pelab,pelab2,ncol=1)
png('./figures/mortlabs.png',2000,2000,type='cairo');labels;dev.off();system2('open','./figures/mortlabs.png')

apal=pals::ocean.matter(5)[-5]
cr1=cr1+
  theme(text=element_text(size=40))+
  ggtitle('Costa Rica % survival over time\n')+
  labs(color='Intraspecific diversity\nwater regime combination')+
  scale_color_manual(values=c('clone flood'=apal[1],'clone no'=apal[2],'ind flood'=apal[3],'ind no'=apal[4]),
                     labels=c('Low diversity, water added','Low diversity, no water','High diversity, water added', 'High diversity, no water'))+
  guides(color='none')
ec1=ec1+
  theme(text=element_text(size=40))+
  ggtitle('Ecuador % survival over time\n')+
  labs(color='Intraspecific diversity\nwater regime combination')+
  scale_color_manual(values=c('clone flood'=apal[1],'clone no'=apal[2],'ind flood'=apal[3],'ind no'=apal[4]),
                     labels=c('Low diversity, water added','Low diversity, no water','High diversity, water added', 'High diversity, no water'))+
  guides(color='none')
pe1=pe1+theme(text=element_text(size=40))+
  ggtitle('Peru % survival over time')+
  labs(color='Intraspecific diversity\nwater regime combination')+
  scale_color_manual(values=c('clone flood'=apal[1],'clone no'=apal[2],'ind flood'=apal[3],'ind no'=apal[4]),
                     labels=c('Low diversity, water added','Low diversity, no water','High diversity, water added', 'High diversity, no water'))+
  guides(color='none')
mg1=mg1+theme(text=element_text(size=40))+ggtitle('Mogi % survival over time')+
  labs(color='Intraspecific diversity')+
  scale_color_manual(values=c('clone'=apal[2],'ind'=apal[4]),
                     labels=c('Low diversity','High diversity'))+
  guides(color='none')
ui1=ui1+theme(text=element_text(size=40))+ggtitle('Uaimii % survival over time')+
  labs(color='Intraspecific diversity')+
  scale_color_manual(values=c('clone'=apal[2],'ind'=apal[4]),
                     labels=c('Low diversity','High diversity'))+
  guides(color='none')

apal=pals::ocean.deep(7)[-7]
cr2=cr2+
  theme(text=element_text(size=40))+
  ggtitle('Costa Rica % survival over time\n')+
  labs(color='Interspecific diversity\nwater regime combination')+
  scale_color_manual(values=c('1 no'=apal[2],'2 flood'=apal[3],'2 no'=apal[4],'12 flood'=apal[5],'12 no'=apal[6]),
                     labels=c('Low diversity, no water','Medium  diversity, water added', 'Medium diversity, no water','High diversity, water added', 'High diversity, no water'))+
  guides(color='none')
ec2=ec2+
  theme(text=element_text(size=40))+
  ggtitle('Ecuador % survival over time\n')+
  labs(color='Interspecific diversity\nwater regime combination')+
  scale_color_manual(values=c('1 flood','1 no'=apal[2],'2 flood'=apal[3],'2 no'=apal[4],'10 flood'=apal[5],'10 no'=apal[6]),
                     labels=c('Low diversity, water added','Low diversity, no water','Medium  diversity, water added', 'Medium diversity, no water','High diversity, water added', 'High diversity, no water'))+
  guides(color='none')
pe2=pe2+theme(text=element_text(size=40))+
  ggtitle('Peru % survival over time')+
  labs(color='Interspecific diversity\nwater regime combination')+
  scale_color_manual(values=c('1 flood'=apal[2],'1 no'=apal[2],'2 flood'=apal[3],'2 no'=apal[4],'6 flood'=apal[5],'6 no'=apal[6]),
                     labels=c('Low diversity, water added','Low diversity, no water','Medium  diversity, water added', 'Medium diversity, no water','High diversity, water added', 'High diversity, no water'))+
  guides(color='none')
mg2=mg2+theme(text=element_text(size=40))+ggtitle('Mogi % survival over time')+
  labs(color='Interspecific diversity\nwater regime combination')+
  scale_color_manual(values=c('1'=apal[2],'2'=apal[4],'4'=apal[6]),
                     labels=c('Low','Medium','High diversity'))+
  guides(color='none')
ui2=ui2+theme(text=element_text(size=40))+ggtitle('Uaimii % survival over time')+
  labs(color='Interspecific diversity\nwater regime combination')+
  scale_color_manual(values=c('1'=apal[2],'3'=apal[6]),
                     labels=c('Low','High diversity'))+
  guides(color='none')


blank=ggplot()+theme_void()
five_gen=cowplot::plot_grid(plot_grid(cr1,ec1,pe1,ncol=3),
                            blank,
                            plot_grid(blank,mg1,ui1,blank,nrow=1,rel_widths = c(.5,1,1,.5)),
                            blank,
                            nrow=2,rel_heights=c(1,.1,1,.1))
five_tax=cowplot::plot_grid(plot_grid(cr2,ec2,pe2,ncol=3),plot_grid(blank,mg2,ui2,blank,nrow=1,rel_widths = c(.5,1,1,.5)),nrow=2)

png('./figures/mort_curve_gen.png',3000,2000,type='cairo');five_gen;dev.off()   #save plots
png('./figures/mort_curve_tax.png',3000,2000,type='cairo');five_tax;dev.off()
png('./figures/allcuves.png',3000,4000,type='cairo');plot_grid(plot_grid(cr1,ec1,pe1,blank,blank,blank,cr2,ec2,pe2,blank,blank,blank,nrow=4,
                                                                         rel_widths=c(1,1,1),rel_heights=c(1,.1,1,.1)),
                                                               plot_grid(ui1,mg1,blank,blank,blank,blank,ui2,mg2,blank,blank,blank,blank,nrow=4,
                                                                         rel_widths=c(1,1,1),rel_heights=c(1,.1,1,.1)),
                                                               nrow=2);dev.off()

md=list(mortcr,mortec,mortpe,mortui,mortmg)[[1]]                                #subset to site
summary(coxph(Surv(days,alive)~c_i*flood+flood*div,data=md))                    #run cox models
summary(coxph(Surv(days,alive)~c_i*flood+div,data=md))
summary(coxph(Surv(days,alive)~c_i+flood*div,data=md))
summary(coxph(Surv(days,alive)~c_i+flood+div,data=md))
summary(coxph(Surv(days,alive)~flood*c_i,data=md))
cox=coxph(Surv(days,alive)~c_i*flood,data=md)
fit=survfit(cox, newdata = data.frame('c_i'=c('ind','clone','ind','clone'),'flood'=c('no','flood')))
ggsurvplot(fit,data=md)


ma=with(mortcurve,aggregate(site,list(site,flood,c_i,div),function(x) 1))       #aggregate by factors
ma$Group.2[ma$Group.2=='flood']='Water added'
ma$Group.2[ma$Group.2=='no']='No water added'
ma$Group.2=factor(ma$Group.2,levels=c('Water added','No water added'))
ma$col=sapply(ma$Group.1,function(x) c('Costa Rica'=pal[1],'Ecuador'=pal[2],'peru'=pal[3],'mogi'=pal[5],'Brazil'=pal[4])[as.character(x)])
ma$count=with(mortcurve,aggregate(plot,list(site,flood,c_i,div),\(x) length(unique(x))))$x
A1=ggplot(ma,aes(x=Group.4,y=Group.3,fill=x))+                                  #create before and after factor plots
  geom_tile(fill=ma$col)+
  geom_text(aes(label=count),size=15,color='white')+
  facet_grid(Group.1~Group.2)+
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),
        strip.text.x = element_text(size=60),
        panel.border = element_rect(fill=F,size=2*scal),
        panel.spacing=unit(0, "lines"),
        text=element_text(size=60),
        axis.text.y = element_text(angle=90,vjust=0))+
  scale_x_continuous(expand=c(0,0),breaks=c(1,2,4,6,8,10,12))+
  scale_y_discrete(expand=c(0,0),position='left',labels=c('   Low','   High'))+
  xlab('*Piper* interspecific richness')+
  ylab('*Piper* intraspecific diversity')+
  theme(axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown())+
  ggtitle('A) Planted treatments')

ma=with(total,aggregate(site,list(site,flood,c_i,final_div),function(x) 1))
ma$Group.2[ma$Group.2=='flood']='Water added'
ma$Group.2[ma$Group.2=='no']='No water added'
ma$Group.2=factor(ma$Group.2,levels=c('Water added','No water added'))
ma$Group.1=factor(ma$Group.1,levels=c('Costa Rica','Ecuador','peru','Brazil','mogi'))
ma$col=sapply(ma$Group.1,function(x) c('Costa Rica'=pal[1],'Ecuador'=pal[2],'peru'=pal[3],'mogi'=pal[5],'Brazil'=pal[4])[as.character(x)])
ma$count=with(total,aggregate(plot,list(site,flood,c_i,final_div),\(x) length(unique(x))))$x
A2=ggplot(ma,aes(x=as.numeric(Group.4),y=Group.3,fill=x))+
  geom_tile(fill=ma$col)+
  geom_text(aes(label=count),size=15,color='white')+
  facet_grid(Group.1~Group.2)+
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),
        strip.text.x = element_text(size=60),
        panel.border = element_rect(fill=F,size=2*scal),
        panel.spacing=unit(0, "lines"),
        text=element_text(size=60),
        axis.text.y = element_text(angle=90,vjust=0))+
  scale_x_continuous(expand=c(0,0),breaks=c(1,2,4,6,8,10,12))+
  scale_y_discrete(expand=c(0,0),position='left',labels=c('',''))+
  xlab('*Piper* interspecific richness')+
  ylab('')+
  theme(axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown())+
  ggtitle('B) Final treatments')

tf2=plot_grid(text,A1,A2,rel_widths = c(.26,.74,.75),nrow=1)
png('./figures/before and after.png',3600,2400,type='cairo');tf2;dev.off()      #save plots

