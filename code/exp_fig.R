light=function(col,f){c=col2rgb(col)[,1]*f/255;return(rgb(c[1],c[2],c[3]))}     #lighten rgb color

if(!('./figures'%in%list.dirs())){dir.create('./figures')}                      #create figures folder if needs be

wat=c("deepskyblue4","#84BD00FF")

theme_set(theme_classic())                                                      #ggplot theme
world=ne_countries(scale="medium",                                              #download medium res worldmap
                   continent=c('South America','North America'),
                   returnclass = "sf")
pal=ocean.speed(7)[6:2]                                                         #set site colors
noc=data.frame('names'=c('La Selva',                                            #data.frame for site locations
          'Biological Station',
          'Yanayacu',
          'Biological Station',
          'El Fundo Génova',
          'Uaimii State Forest',
          'Mogi-Guaçu',
          'Biological Reserve'),
          'country'=c('Costa Rica','','Ecuador','','Peru','Brazil','Brazil',''),
          'x'=c(-84.04,-84.04,-77.89,-77.89,-75.35,-43.57,-47.16,-47.16),
          'y'=c(9.94,9.94,-.6,-.6,-11.1,-20.3,-22.25,-22.25))

loc=data.frame('x'=c(-84.04,-77.89,-75.35,-43.57,-47.16),                       #reduced data.frame
               'y'=c(9.94,-.6,-11.1,-20.3,-22.25),
               'names2'=c(rep(' ',5)),
               'country2'=c(rep(' ',5)),
               'col'=pal)
mx=95                                                                           #variable longitude limits
tri=data.frame('x'=c(0,2,7,7,2,0)-mx,'y'=c(0,0,-7,-8,-15,-15)+19)               #create arrows
tip=data.frame('x'=7-mx,
               'y'=c(12.5,12.5-15,12.5-15*2,12.5-15*3,12.5-15*4)-1,
               'col'=loc$col)

apal=ocean.ice(6)[2:5]                                                          #cross-site colors
pal.safe(c(pal[-6],apal))           
blank=ggplot()+theme_void()                                                     #blank plot for formating

#### map of sites ####
scal=3
heights=seq(15,-43,length.out = 5)-5
heights=c(heights[1],heights[1]-2.25,
          heights[2],heights[2]-2.25,
          heights[3:5],heights[5]-2.25)
map=ggplot(data = world)+                                                    
  geom_sf(size=.75*scal)+
  coord_sf(xlim=c(-mx,-35),ylim=c(-55,20)-1,expand=F)+
  geom_rect(aes(xmin=-mx,xmax=1-mx,ymin=-56,ymax=19),fill='white')+
  annotation_scale(location = "br",
                   width_hint = 0.3,
                   text_cex=1.5*scal,
                   pad_y = unit(.25*scal,'cm'),
                   pad_x = unit(.25*scal,'cm'),
                   height = unit(.25*scal,'cm'))+
  geom_label_repel(data=loc,aes(x=x,y=y,label=names2,segment.color=col),
                  fontface = "bold",
                  nudge_x = -mx+5.75-loc$x,
                  nudge_y = tip$y-loc$y,
                  hjust='left',
                  vjust=.5,
                  box.padding =0,
                  fill=NA,
                  color='black',
                  label.size=NA,
                  segment.curvature = -1e-20,
                  segment.size=7*scal,
                  size=7.5*scal,
                  arrow = arrow(length = unit(0.00, "npc")))+
  geom_point(data=loc,aes(x=x,y=y),color=loc$col,size=10*scal)+
  guides(color='none')+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0,2,0,-2), "cm"))+
  xlab('')+ylab('')+theme(panel.border = element_blank(),
                          panel.background = element_rect(fill='white',color='white'),
                          axis.line = element_blank())+
  geom_polygon(data=tri,aes(x=x,y=y),fill=pal[1])+
  geom_polygon(data=tri,aes(x=x,y=y-15),fill=pal[2])+
  geom_polygon(data=tri,aes(x=x,y=y-15*2),fill=pal[3])+
  geom_polygon(data=tri,aes(x=x,y=y-15*3),fill=pal[4])+
  geom_polygon(data=tri,aes(x=x,y=y-15*4),fill=pal[5])+
  annotate('segment',x=-Inf,xend=Inf,y=-Inf,yend=-Inf,color='black',size=2*scal)+
  annotate('segment',x=-Inf,xend=Inf,y=Inf,yend=Inf,color='black',size=2*scal)+
  annotate('segment',x=Inf,xend=Inf,y=-Inf,yend=Inf,color='black',size=2*scal)+
  annotate('segment',x=-mx,xend=2-mx,y=4,yend=4,color='black',size=2*scal)+
  annotate('segment',x=-mx,xend=2-mx,y=4-15,yend=4-15,color='black',size=2*scal)+
  annotate('segment',x=-mx,xend=2-mx,y=4-15*2,yend=4-15*2,color='black',size=2*scal)+
  annotate('segment',x=-mx,xend=2-mx,y=4-15*3,yend=4-15*3,color='black',size=2*scal)+
  annotate('segment',x=-Inf,xend=-Inf,y=-Inf,yend=Inf,color='black',size=2*scal)

png('./figures/map.png',2000,2000/70*90,type='cairo');map;dev.off()

tip=data.frame('x'=c(-94,-94,-94,30,30),
               'y'=c(-60+75,-60+45,-60+15,-60+45,-60+15),
               'col'=loc$col)
map2=ggplot(data = world)+                                                    
  geom_sf(size=.75*scal)+
  coord_sf(xlim=c(-90,-30)-4,ylim=c(-60,30)-5,expand=F)+
  geom_rect(aes(xmin=-mx,xmax=1-mx,ymin=-56,ymax=19),fill='white')+
  annotation_scale(location = "tr",
                   width_hint = 0.3,
                   text_cex=1.5*scal,
                   pad_y = unit(.25*scal,'cm'),
                   pad_x = unit(.25*scal,'cm'),
                   height = unit(.25*scal,'cm'))+
  geom_point(data=loc,aes(x=x,y=y),color='black',size=5*scal)+
  guides(color='none')+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0,2,0,-2), "cm"))+
  xlab('')+ylab('')+theme(panel.border = element_blank(),
                          panel.background = element_rect(fill='white',color='white'),
                          axis.line = element_blank())+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

png('./figures/map2.png',2000,2500,type='cairo');map2;dev.off();system2('open','./figures/map2.png')

#### factor plot ####
tri=data.frame('x'=c(0,2,7,7,2,0)-mx,'y'=c(0,0,-7,-8,-15,-15)+19)               #create arrows
tip=data.frame('x'=7-mx,
               'y'=c(12.5,12.5-15,12.5-15*2,12.5-15*3,12.5-15*4)-1,
               'col'=loc$col)
ma=with(mortcurve,aggregate(site,list(site,flood,c_i,divf),function(x) 1))
ma$Group.2[ma$Group.2=='flood']='Water added'
ma$Group.2[ma$Group.2=='no']='No water added'
ma$Group.2=factor(ma$Group.2,levels=c('Water added','No water added'))
ma$col=sapply(ma$Group.1,function(x) c('Costa Rica'=pal[1],
                                       'Ecuador'=pal[2],
                                       'peru'=pal[3],
                                       'mogi'=pal[5],
                                       'Brazil'=pal[4])[as.character(x)])
factors=ggplot(ma,aes(x=Group.4,y=Group.3,fill=x))+
  geom_tile(fill=ma$col)+
  facet_grid(Group.1~Group.2)+
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),
        strip.text.x = element_text(size=60),
        panel.border = element_rect(fill=F,size=2*scal),
        panel.spacing=unit(0, "lines"),
        text=element_text(size=60),
        axis.text.y = element_text(angle=90,vjust=.5),
        plot.margin = unit(c(0,-2,0,2), "cm"))+
  scale_x_discrete(expand=c(0,0))+
  scale_y_discrete(expand=c(0,0),position='left',labels=c('Low','High'))+
  xlab('*Piper* interspecific richness')+
  ylab('*Piper* intraspecific diversity')+
  theme(axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown())

scal2=2.5
text=ggplot()+
  geom_label_repel(data=noc,aes(x=x,y=y,label=names),
                   fontface = "bold",
                   nudge_x = -mx*2-noc$x,
                   nudge_y = c(tip$y[1],tip$y[1]-2.1,tip$y[2],tip$y[2]-2.1,tip$y[3:4],tip$y[5],tip$y[5]-2.1)-noc$y,
                   hjust='left',
                   vjust=.5,
                   box.padding=0,
                   fill='white',
                   color='black',
                   segment.color=NA,
                   size=7.5*scal2,
                   label.size=NA)+
  geom_label_repel(data=noc,aes(x=x,y=y,label=country),
                   fontface = "bold",
                   nudge_x = -mx*2-noc$x,
                   nudge_y = c(tip$y[1],tip$y[1]-2.1,tip$y[2],tip$y[2]-2.1,tip$y[3:4],tip$y[5],tip$y[5]-2.1)+2.1-noc$y,
                   hjust='left',
                   vjust=.5,
                   box.padding=0,
                   fill='white',
                   color='darkgrey',
                   segment.color=NA,
                   size=12.5*scal2,
                   label.size=NA)+
  xlim(-mx,-20)+
  ylim(-56,19)+
  theme_void()
tf=plot_grid(text,factors,rel_widths = c(.26,.74))
smallm=plot_grid(blank,map,blank,rel_heights = c(0.0214,.8,0.0305),ncol=1)

#### insect families by site ####
insectmask=c('Geometridae/eois'='Geometridae - Eois','minero'='Leaf miners')
for(m in names(insectmask)){family$family=gsub(m,insectmask[m],family$family)}

fa=with(family,aggregate(family,list(site,order,family),length))
fa$percent=fa%>%group_by(Group.1)%>%mutate(x/sum(x))%>%pull()
fa$herb=with(family,aggregate(herbivory,list(site,order,family),function(x) sum(x,na.rm=T)))$x
fa$herbpercent=fa%>%group_by(Group.1)%>%mutate(herb/sum(herb))%>%pull()
fl=with(fa,aggregate(x,list(Group.2,Group.3),length))
fa$Group.3=factor(fa$Group.3,levels=as.character(fl$Group.2)[order(fl$Group.1,-fl$x)[length(fl$x):1]])

A=ggplot(fa,aes(y=Group.3,x=Group.1,fill=percent))+
  theme_classic()+
  geom_tile()+xlab('Study site')+ylab('')+
  scale_fill_gradientn(colors=ocean.ice(100)[100:1],
                       limits=c(0,.63),breaks=c(0,.2,.4,.6,.8,1)/2,labels=c('0%','10%','20%','30%','40%','50%'))+
  theme(text = element_text(size = 50),
        axis.text.x = element_text(size = 45),
        panel.spacing = unit(0,'lines'))+
  scale_y_discrete(expand = c(0,0))+
  scale_x_discrete(limits=c(c('Costa Rica','Ecuador','peru','Brazil','mogi')),
                   labels=c('CR','EC','PE','UI','MG'),
                   expand=c(0,0))+
  ggtitle('B    Percentage of taxa observed')+
  theme(legend.position = 'none',
        panel.border = element_rect(fill=F,size=2))

B=ggplot(fa,aes(y=Group.3,x=Group.1,fill=herbpercent))+
  theme_classic()+
  geom_tile()+xlab('Study site')+ylab('')+
  scale_fill_gradientn(colors=ocean.ice(100)[100:1],
                       limits=c(0,.63),breaks=c(0,.2,.4,.6,.8,1,1.2)/2,labels=c('0%','10%','20%','30%','40%','50%','60%'))+
  theme(text = element_text(size = 50),axis.text.y = element_blank(),
        axis.text.x = element_text(size = 45),
        panel.spacing = unit(0,'lines'))+
  scale_y_discrete(expand = c(0,0))+
  scale_x_discrete(limits=c(c('Costa Rica','Ecuador','peru','Brazil','mogi')),
                   labels=c(c('CR','EC','PE','UI','MG')),
                   expand=c(0,0))+
  ggtitle('Mean percent herbivory')+
  labs(fill='')+
  theme(legend.key.height = unit(8,'cm'),
        legend.key.width  = unit(3,'cm'),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(fill=F,size=2))

#### spec gen by site ####
insectmask=c('Geometridae/eois'='Geometridae - Eois','minero'='Leaf miners')
for(m in names(insectmask)){family$family=gsub(m,insectmask[m],family$family)}
families
family$spec=(c('s','s','g','g','g','s','g','g','g','g','g','g','g','g','g')|>
  setNames(unique(family$family)))[family$family]

fa=with(family,aggregate(family,list(site,spec),length))
fa$Group.1=(c('Ecuador','Costa Rica','Peru','Mogi','Uaimii')|>
              setNames(unique(fa$Group.1)))[fa$Group.1]|>
              factor(levels=c('Costa Rica','Ecuador','Peru','Uaimii','Mogi'))
fa$Group.2=c('g'='Generalist taxa','s'='Specialist taxa')[fa$Group.2]
fa$percent=fa%>%group_by(Group.1)%>%mutate(x/sum(x))%>%pull()
fa$herb=with(family,aggregate(herbivory,list(site,spec),function(x) mean(x,na.rm=T)))$x
fa$herb2=with(family,aggregate(herbivory,list(site,spec),function(x) sum(x,na.rm=T)))$x
fa$herbpercent=fa%>%group_by(Group.1)%>%mutate(herb2/sum(herb2))%>%pull()
#fl=with(fa,aggregate(x,list(Group.2),length))
#fa$Group.3=factor(fa$Group.2,levels=c('g','s'))

A2=ggplot(fa,aes(x=Group.1,y=percent*100,fill=Group.2))+
  geom_bar(stat='summary',position='dodge')+
  theme_classic()+
  ylab('Proportion of taxa observed')+
  xlab('')+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),
        strip.text.x = element_text(size=60),
        panel.border = element_rect(fill=F,size=2),
        panel.spacing=unit(0, "lines"),
        text=element_text(size=40),
        axis.text.y = element_text(angle=90,vjust=.5),
        axis.text.x=element_text(angle=45,hjust=1))+
  labs(fill='Diet breadth')+
  scale_fill_manual(values=wat)+
  guides(fill='none')+
  ylim(0,100)

B2=ggplot(fa,aes(x=Group.1,y=herb,fill=Group.2))+
  geom_bar(stat='summary',position='dodge')+
  theme_classic()+
  ylab('Mean percent herbivory when present')+
  xlab('')+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),
        strip.text.x = element_text(size=60),
        panel.border = element_rect(fill=F,size=2),
        panel.spacing=unit(0, "lines"),
        text=element_text(size=40),
        axis.text.y = element_text(angle=90,vjust=.5),
        axis.text.x=element_text(angle=45,hjust=1))+
  labs(fill='Diet breadth')+
  scale_fill_manual(values=wat)+
  guides(fill='none')+
  ylim(0,100)

C2=ggplot(fa,aes(x=Group.1,y=herbpercent*100,fill=Group.2))+
  geom_bar(stat='summary',position='dodge')+
  theme_classic()+
  ylab('Porportion of herbivory performed')+
  xlab('')+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),
        strip.text.x = element_text(size=60),
        panel.border = element_rect(fill=F,size=2),
        panel.spacing=unit(0, "lines"),
        text=element_text(size=40),
        axis.text.y = element_text(angle=90,vjust=.5),
        axis.text.x=element_text(angle=45,hjust=1))+
  labs(fill='Diet breadth')+
  scale_fill_manual(values=wat)+
  ylim(0,100)

fa=with(family,aggregate(family,list(site,order,spec),length))
fa$sd=with(family,aggregate(herbivory,list(site,order,spec),\(x)sd(x,na.rm=T)))$x
fa$percent=fa%>%group_by(Group.1)%>%mutate(x/sum(x))%>%pull()
fa$herb=with(family,aggregate(herbivory,list(site,order,spec),function(x) mean(x,na.rm=T)))$x
fa$herb2=with(family,aggregate(herbivory,list(site,order,spec),function(x) sum(x,na.rm=T)))$x
fa$herbpercent=fa%>%group_by(Group.1)%>%mutate(herb2/sum(herb2))%>%pull()
#fa$Group.2[fa$Group.3=='s'&fa$Group.2=='other']='lm'
fa=subset(fa,Group.2!='other')
fa$Group.2=c('coleoptera'='Coleoptera','lepidoptera'='Lepidoptera','lm'='Leaf miners')[fa$Group.2]
fa$Group.3=c('g'='Generalist taxa','s'='Specialist taxa')[fa$Group.3]
fa$Group.1=(c('Ecuador','Costa Rica','Peru','Mogi','Uaimii')|>
  setNames(unique(fa$Group.1)))[fa$Group.1]|>
  factor(levels=c('Costa Rica','Ecuador','Peru','Uaimii','Mogi'))
A3=ggplot(fa,aes(x=Group.1,y=percent*100,fill=Group.3))+
  geom_bar(stat='summary',position='dodge')+
  facet_grid(cols=vars(Group.2))+
  theme_classic()+
  ylab('Percent herbivory')+
  xlab('')+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),
        strip.text.x = element_text(size=60),
        panel.border = element_rect(fill=F,size=2),
        panel.spacing=unit(0, "lines"),
        text=element_text(size=40),
        axis.text.y = element_text(angle=90,vjust=.5),
        axis.text.x=element_text(angle=45,hjust=1))+
  labs(fill='Diet breadth')+
  scale_fill_manual(values=wat)
B3=ggplot(fa,aes(x=Group.1,y=herb,fill=Group.3))+
  geom_bar(stat='summary',position='dodge')+
  facet_grid(cols=vars(Group.2))+
  theme_classic()+
  ylab('Percent herbivory')+
  xlab('')+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),
        strip.text.x = element_text(size=60),
        panel.border = element_rect(fill=F,size=2),
        panel.spacing=unit(0, "lines"),
        text=element_text(size=40),
        axis.text.y = element_text(angle=90,vjust=.5),
        axis.text.x=element_text(angle=45,hjust=1))+
  labs(fill='Diet breadth')+
  scale_fill_manual(values=wat)
C3=ggplot(fa,aes(x=Group.1,y=herbpercent*100,fill=Group.3))+
  geom_bar(stat='summary',position='dodge')+
  facet_grid(cols=vars(Group.2))+
  theme_classic()+
  ylab('Percent herbivory')+
  xlab('')+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),
        strip.text.x = element_text(size=60),
        panel.border = element_rect(fill=F,size=2),
        panel.spacing=unit(0, "lines"),
        text=element_text(size=40),
        axis.text.y = element_text(angle=90,vjust=.5),
        axis.text.x=element_text(angle=45,hjust=1))+
  labs(fill='Diet breadth')+
  scale_fill_manual(values=wat)
#png('./figures/order_level_specgen.png',2000,1000,type='cairo');C2;dev.off();system2('open','./figures/order_level_specgen.png')
#png('./figures/allspecgen1.png',3000,1000,type='cairo');plot_grid(A3,B3,C3,nrow=1);dev.off();system2('open','./figures/allspecgen1.png')
png('./figures/allspecgen2.png',2000,1000,type='cairo');plot_grid(A2,C2,nrow=1,rel_widths=c(.8,1.05));dev.off();system2('open','./figures/allspecgen2.png')
#png('./figures/allspecgen3.png',3000,2000,type='cairo');plot_grid(A2,B2,C2,A3,B3,C3,nrow=2);dev.off();system2('open','./figures/allspecgen3.png')

#### herb and mort ####
C=plot_grid(ggplot(total,aes(x=site,y=wilmer_herb_total,fill=site))+geom_boxplot(notch=F,outlier.alpha = 0,color='black')+
                       scale_x_discrete(limits=c(c('Costa Rica','Ecuador','peru','Brazil','mogi')),
                                        labels=c('','','','',''))+
                       scale_fill_manual(values=c('Costa Rica'=pal[1],'Ecuador'=pal[2],'peru'=pal[3],'mogi'=pal[5],'Brazil'=pal[4]))+
                       guides(fill='none')+
                       theme(text = element_text(size = 50),axis.text.x = element_text(size=45,angle=45,hjust=1))+
                       xlab('')+ylab('Percent herbivory')+ggtitle('A'),
                     ggplot(total,aes(x=site,y=pres,fill=site))+geom_bar(stat='summary')+
                       scale_x_discrete(limits=c(c('Costa Rica','Ecuador','peru','Brazil','mogi')),
                                        labels=c(c('','','','','')))+
                       scale_fill_manual(values=c('Costa Rica'=pal[1],'Ecuador'=pal[2],'peru'=pal[3],'mogi'=pal[5],'Brazil'=pal[4]))+
                       guides(fill='none')+
                       theme(text = element_text(size = 50),axis.text.x = element_text(size=45,angle=45,hjust=1))+
                       xlab('')+ylab('Percent of leaves with damage')+ggtitle(' '),
                     ggplot(plotresp,aes(x=site,y=a_div,fill=site))+geom_boxplot(notch=F,outlier.alpha = 0,color='black')+
                       scale_x_discrete(limits=c(c('Costa Rica','Ecuador','peru','Brazil','mogi')),
                                        labels=c(c('CR','EC','PE','UI','MG')))+
                       scale_fill_manual(values=c('Costa Rica'=pal[1],'Ecuador'=pal[2],'peru'=pal[3],'mogi'=pal[5],'Brazil'=pal[4]))+
                       guides(fill='none')+
                       theme(text = element_text(size = 50),axis.text.x = element_text(size=45))+
                       xlab('Study site')+ylab('Mean leaf-level insect richness'),
                     ggplot(mort5,aes(x=site,y=100-alive,fill=site))+geom_bar(stat='summary')+
                       scale_x_discrete(limits=c(c('Costa Rica','Ecuador','peru','Brazil','mogi')),
                                        labels=c(c('CR','EC','PE','UI','MG')))+
                       scale_fill_manual(values=c('Costa Rica'=pal[1],'Ecuador'=pal[2],'peru'=pal[3],'mogi'=pal[5],'Brazil'=pal[4]))+
                       guides(fill='none')+
                       ylim(0,100)+
                       theme(text = element_text(size = 50),axis.text.x = element_text(size=45))+
                       xlab('Study site')+ylab('Percent mortality'),
                     ncol=2)

tiles=plot_grid(A,B,nrow=1,rel_widths=c(1,.9))
C_=plot_grid(blank,C,blank,rel_widths = c(.125,.9,.1),nrow=1)
png('./figures/herb_mort_summary_wide.png',4000,1500,type='cairo');plot_grid(C,A,B,nrow=1,rel_widths=c(1.5,1.2,1));dev.off()
png('./figures/herb_mort_summary_tall.png',2600,3000,type='cairo');plot_grid(C_,blank,tiles,nrow=3,rel_heights = c(1.2,.05,1));dev.off()
png('./figures/herb_genspec.png',2000,900,type='cairo');plot_grid(A2,B2,nrow=1);dev.off();system2('open','./figures/herb_genspec.png')

#### plot diagnostics ###
C=plot_grid(ggplot(a22,aes(x=alive,y=nlvs,color=site))+
                       geom_point(size=6)+
                       geom_smooth(size=5,method=lm,alpha=0)+
                       scale_color_manual(values=pal)+
                       xlab('Percent survival')+
                       ylab('Number of leaves measured')+
                       theme(text = element_text(size = 40),axis.text.x = element_text(size=30)),
                     ggplot(a22,aes(x=nlvs,y=herbivory,color=site))+
                       geom_point(size=6)+
                       geom_smooth(size=5,method=lm,alpha=0)+
                       scale_color_manual(values=pal)+
                       ylab('Mean herbivory on surviving plants')+
                       xlab('Number of leaves measured')+
                       theme(text = element_text(size = 40),axis.text.x = element_text(size=30)))

D=plot_grid(ggplot(a22,aes(x=nlvs,y=error,color=site))+
                       geom_point(size=6)+
                       geom_smooth(size=5,method=lm,alpha=0)+
                       scale_color_manual(values=pal)+
                       ylab('Stdev of herbivory')+
                       xlab('Number of leaves measured')+
                       theme(text = element_text(size = 40),axis.text.x = element_text(size=30)),
                     ggplot(a22,aes(y=(error),x=herbivory,color=site))+
                       geom_point(size=6)+
                       geom_smooth(size=5,method=lm,alpha=0)+
                       scale_color_manual(values=pal)+
                       xlab('Mean herbivory on surviving plants')+
                       ylab('Stdev of herbivory')+
                       theme(text = element_text(size = 40),axis.text.x = element_text(size=30)))

E=ggplot(a22,aes(y=alive,x=a_div,color=site))+
  geom_point(size=6)+
  geom_smooth(size=5,method=lm,alpha=0)+
  scale_color_manual(values=pal)+
  xlab('Mean insect richness per plot')+
  ylab('percent survival')+
  theme(text = element_text(size = 40),axis.text.x = element_text(size=30))

png('./figures/nlvs.png',2000,1000,type='cairo');C;dev.off()
png('./figures/error.png',2000,1000,type='cairo');D;dev.off()
png('./figures/rich.png',1000,1000,type='cairo');E;dev.off()

#### bayesian plots ####
herb_site=bass(herb_sdata,c('floodn','pdiv','clonen','a_div'),'site',1.4,label=24,lsize=12.5)
pres_site=bass(pres_sdata,c('floodn','pdiv','clonen','a_div'),'site',1.4,label=48,lsize=12.5)
var_site=bass(var_sdata,c('floodn','pdiv','clonen','a_div'),'site',1.4,label=590,lsize=12.5)
mort_site=bass(mort_sdata,c('flood','pdiv','c_i','a_div'),'site',1.4,label=48,lsize=12.5)
siteplots=c(herb_site,pres_site,var_site,mort_site)
for(i in 1:length(siteplots)){siteplots[[i]]=siteplots[[i]]+
  scale_x_discrete(limits=c('all','Costa Rica','Ecuador','peru','Brazil','mogi'),
                   labels=list(c('','','','','',''),c('All sites','CR','EC','PE','UI','MG'))[[(i>12)+1]])+
  guides(fill='none')+
  theme_classic()+
  ggtitle(list(c('','','',''),c('Water addition','Interspecific diversity','Intraspecific diversity','Insect richness')[i])[[(i<5)+1]])+
  xlab(c('','Study site')[(i>12)+1])+
  ylab(c('',c('Percent herbivory beta','Percent leaves with damage beta','Variance in herbivory beta','Survivorship beta')[ceiling(i/4)])[(i%in%c(1,5,9,13))+1])+
  theme(text=element_text(size=60),axis.line = element_line(size=2))+
  scale_fill_manual(values=c('white',ocean.speed(6)[6:2]))+
  ylim(list(c(-25,25),c(-50,50),c(-600,600),c(-50,50))[[ceiling(i/4)]])+
  scale_fill_manual(values=c('all'='white','Costa Rica'=pal[1],'Ecuador'=pal[2],'peru'=pal[3],'mogi'=pal[5],'Brazil'=pal[4]))}
png('./figures/siteplots.png',4000,4000,type='cairo');do.call(grid.arrange,c(siteplots,nrow=4));dev.off()

s=15
herb_all=bass(herb_sdata,s=1.1,label=30,lsize=s)
pres_all=bass(pres_sdata,s=1.1,label=50,lsize=s)
var_all=bass(var_sdata,s=1.1,label=1000,lsize=s)
mort_all=bass(mort_sdata,s=1.1,label=75,lsize=s)
div_all=bass(div_sdata,s=1.1,label=1.5,lsize=s)
allplots=list(herb_all,pres_all,var_all,mort_all,div_all)
for(i in 1:length(allplots)){allplots[[i]]=allplots[[i]][[1]]+
  theme_classic()+
  theme(aspect.ratio = 1,text = element_text(size=40),axis.text.x = element_text(angle=30,hjust=1),axis.line = element_line(size=1.1))+
  guides(fill='none')+
  scale_fill_manual(values=list(c('beta_floodn'=apal[1],'beta_pdiv'=apal[2],'beta_clonen'=apal[3],'beta_a_div'=apal[4]),
                                c('beta_floodn'=apal[1],'beta_pdiv'=apal[2],'beta_clonen'=apal[3],'beta_a_div'=apal[4]),
                                c('beta_floodn'=apal[1],'beta_pdiv'=apal[2],'beta_clonen'=apal[3],'beta_a_div'=apal[4]),
                                c('beta_flood'=apal[1], 'beta_pdiv'=apal[2],'beta_c_i'= apal[3] , 'beta_a_div'=apal[4]),
                                c('beta_floodn'=apal[1], 'beta_pdiv'=apal[2],'beta_clonen'= apal[3]))[[i]])+
  scale_x_discrete(limits= list(c('beta_floodn','beta_pdiv','beta_clonen','beta_a_div'),
                                c('beta_floodn','beta_pdiv','beta_clonen','beta_a_div'),
                                c('beta_floodn','beta_pdiv','beta_clonen','beta_a_div'),
                                c('beta_flood', 'beta_pdiv','beta_c_i' ,  'beta_a_div'),
                                c('beta_floodn', 'beta_pdiv','beta_clonen'))[[i]],
                   labels=c('Water addition','Interspecific diversity','Intraspecific diversity','Insect richness'))+
  xlab('')+ylab(c('Percent herbivory beta','Percent leaves with damage beta','Variance in herbivory beta','Survivorship beta','Insect richness beta')[i])+
  ylim(list(c(-30,30),c(-50,50),c(-1000,1000),c(-75,75),c(-1.5,1.5))[[i]])+
  ggtitle(c('A','B','C','D','E')[i])#+
  #annotate('text',x='beta_a_div',y=c(-30,-50,-1000,-75,-1.5)[i],label='PPC',size=s)
}
png('./figures/4plots.png',2000,3000,type='cairo');do.call(grid.arrange,c(allplots,ncol=2));dev.off()

div_site=bass(div_sdata,c('floodn','pdiv','clonen'),'site',1.1,label=.9,lsize=12)
for(i in 1:length(div_site)){div_site[[i]]=div_site[[i]]+
  scale_x_discrete(limits=c('all','Costa Rica','Ecuador','peru','Brazil','mogi'),
                   labels=c('All sites','CR','EC','PE','UI','MG'))+
  guides(fill='none')+
  theme_classic()+
  xlab('Study site')+
  ylab(c('','Insect richness response')[(i==1)+1])+
  ggtitle(c('Water addition','Interspecific diversity','Intraspecific diversity')[i])+
  theme(text=element_text(size=40),axis.line = element_line(size=1.1))+
  ylim(-1,1.1)+
  scale_fill_manual(values=c('all'='white','Costa Rica'=pal[1],'Ecuador'=pal[2],'peru'=pal[3],'mogi'=pal[5],'Brazil'=pal[4]))}
png('./figures/divsite2.png',3000,1000,type='cairo');do.call(grid.arrange,c(div_site,nrow=1));dev.off()

#sem plot formating
m1='y.pdiv [pos=c(1,5),size=c(3.5,1.5),lab="Interspecific \\ndiversity"]
y.clonen [pos=c(10,5),size=c(3.5,1.5),lab="Intraspecific \\ndiversity"]
y.a_div [pos=c(1,0),size=c(3.5,1.5),lab="Insect \\nrichness"]
y.herbivory [pos=c(10,0),size=c(3.5,1.5),lab="Percent \\nherbivory"]
y.floodn [pos=c(5.5,-5),size=c(3.5,1.5),lab="Water \\naddition"]
y.pdiv>y.a_div [nudge=c(0,0,0,0),txtnudge=c(-.75,0),txt="A"]
y.pdiv>y.herbivory [nudge=c(0,.5,-.33,.5),txtnudge=c(0,0.75),txt="B"]
y.clonen>y.a_div [nudge=c(0,-.5,.33,-.5),txtnudge=c(0,-.75),txt="C"]
y.clonen>y.herbivory [nudge=c(0,0,0,0),txtnudge=c(.75,0),txt="D"]
y.floodn>y.herbivory [nudge=c(0,0,0,0),txtnudge=c(0,0.5),txt="F"]
y.a_div>y.herbivory [nudge=c(0,0,0,0),txtnudge=c(0,0.5),txt="E"]
y.floodn>y.a_div [nudge=c(0,0,0,0),txtnudge=c(0,0.5),txt="G"]'

m2='y.pdiv [pos=c(1,5),size=c(3.5,1.5),lab="Interspecific \\ndiversity"]
y.clonen [pos=c(10,5),size=c(3.5,1.5),lab="Intraspecific \\ndiversity"]
y.a_div [pos=c(1,0),size=c(3.5,1.5),lab="Insect \\nrichness"]
y.herbivory [pos=c(10,0),size=c(3.5,1.5),lab="Percent \\nherbivory"]
y.floodn [pos=c(5.5,-5),size=c(3.5,1.5),lab="Water \\naddition"]
y.pdiv>y.a_div [nudge=c(0,0,0,0),txtnudge=c(-1,0),txt="A"]
y.pdiv>y.herbivory [nudge=c(0,.5,-.33,.5),txtnudge=c(0,1),txt="B"]
y.clonen>y.a_div [nudge=c(0,-.5,.33,-.5),txtnudge=c(0,-1),txt="C"]
y.clonen>y.herbivory [nudge=c(0,0,0,0),txtnudge=c(1,0),txt="D"]
y.floodn>y.herbivory [nudge=c(0,0,0,0),txtnudge=c(0,0.5),txt="F"]
y.a_div>y.herbivory [nudge=c(0,0,0,0),txtnudge=c(0,0.75),txt="E"]'

m3='y.pdiv [pos=c(1,5),size=c(3.5,1.5),lab="Interspecific \\ndiversity"]
y.clonen [pos=c(10,5),size=c(3.5,1.5),lab="Intraspecific \\ndiversity"]
y.a_div [pos=c(1,0),size=c(3.5,1.5),lab="Insect \\nrichness"]
y.herbivory [pos=c(10,0),size=c(3.5,1.5),lab="Percent \\nherbivory"]
y.pdiv>y.a_div [nudge=c(0,0,0,0),txtnudge=c(-1,0),txt="A"]
y.pdiv>y.herbivory [nudge=c(0,.5,-.33,.5),txtnudge=c(0,0.75),txt="B"]
y.clonen>y.a_div [nudge=c(0,-.5,.33,-.5),txtnudge=c(0,-.75),txt="C"]
y.clonen>y.herbivory [nudge=c(0,0,0,0),txtnudge=c(1,0),txt="D"]
y.a_div>y.herbivory [nudge=c(0,0,0,0),txtnudge=c(0,0.5),txt="E"]'

m4='y.div [pos=c(0,6),size=c(3.5,1.5),lab="Interspecific \\ndiversity"]
y.clone [pos=c(11,6),size=c(3.5,1.5),lab="Intraspecific \\ndiversity"]
y.a_div [pos=c(0,0),size=c(3.5,1.5),lab="Insect \\nrichness"]
y.herbivory [pos=c(5.5,0),size=c(3.5,1.5),lab="Percent \\nherbivory"]
y.flood [pos=c(5.5,-5),size=c(3.5,1.5),lab="Water \\naddition"]
y.alive [pos=c(11,0),size=c(3.5,1.5),lab="Survival"]
y.div>y.a_div [nudge=c(0,0,0,0),txtnudge=c(-1,0),txt="A"]
y.div>y.herbivory [nudge=c(0,-1,-.33,-1),txtnudge=c(0,-1.25),txt="B"]
y.clone>y.a_div [nudge=c(0,1,.33,1),txtnudge=c(0,1.25),txt="C"]
y.clone>y.herbivory [nudge=c(0,-1,.33,-1),txtnudge=c(0,-1.25),txt="D"]
y.flood>y.herbivory [nudge=c(0,0,0,0),txtnudge=c(0.75,0.5),txt="F"]
y.a_div>y.herbivory [nudge=c(0,0,0,0),txtnudge=c(0,0.5),txt="E"]
y.div>y.alive [nudge=c(0,.1,-.33,.1),txtnudge=c(0,-.5),txt="H"]
y.clone>y.alive [nudge=c(0,0,0,0),txtnudge=c(1,0),txt="I"]
y.herbivory>y.alive [nudge=c(0,0,0,0),txtnudge=c(0,0.5),txt="J"]
y.flood>y.alive [nudge=c(0,0,0,0),txtnudge=c(0.25,0.5),txt="K"]
y.flood>y.a_div [nudge=c(0,0,0,0),txtnudge=c(-0.25,0.5),txt="G"]'

m5='y.div [pos=c(0,6),size=c(3.5,1.5),lab="Interspecific \\ndiversity"]
y.clone [pos=c(11,6),size=c(3.5,1.5),lab="Intraspecific \\ndiversity"]
y.a_div [pos=c(0,0),size=c(3.5,1.5),lab="Insect \\nrichness"]
y.herbivory [pos=c(5.5,0),size=c(3.5,1.5),lab="Percent \\nherbivory"]
y.flood [pos=c(5.5,-5),size=c(3.5,1.5),lab="Water \\naddition"]
y.alive [pos=c(11,0),size=c(3.5,1.5),lab="Survival"]
y.div>y.a_div [nudge=c(0,0,0,0),txtnudge=c(-1,0),txt="A"]
y.div>y.herbivory [nudge=c(0,-1,-.33,-1),txtnudge=c(0,-1.25),txt="B"]
y.clone>y.a_div [nudge=c(0,1,.33,1),txtnudge=c(0,1.25),txt="C"]
y.clone>y.herbivory [nudge=c(0,-1,.33,-1),txtnudge=c(0,-1.25),txt="D"]
y.flood>y.herbivory [nudge=c(0,0,0,0),txtnudge=c(0.75,0.5),txt="F"]
y.a_div>y.herbivory [nudge=c(0,0,0,0),txtnudge=c(0,0.5),txt="E"]
y.div>y.alive [nudge=c(0,.1,-.33,.1),txtnudge=c(0,-.5),txt="H"]
y.clone>y.alive [nudge=c(0,0,0,0),txtnudge=c(1,0),txt="I"]
y.herbivory>y.alive [nudge=c(0,0,0,0),txtnudge=c(0,0.5),txt="J"]
y.flood>y.alive [nudge=c(0,0,0,0),txtnudge=c(0.25,0.5),txt="K"]'

m55='y.div [pos=c(0,6),size=c(3.5,1.5),lab="Interspecific \\ndiversity"]
y.clone [pos=c(11,6),size=c(3.5,1.5),lab="Intraspecific \\ndiversity"]
y.rich [pos=c(0,0),size=c(3.5,1.5),lab="Insect \\nrichness"]
y.herb [pos=c(5.5,0),size=c(3.5,1.5),lab="Percent \\nherbivory"]
y.flood [pos=c(5.5,-5),size=c(3.5,1.5),lab="Water \\naddition"]
y.surv_next [pos=c(11,0),size=c(3.5,1.5),lab="Survival"]
y.div>y.rich [nudge=c(0,0,0,0),txtnudge=c(-1,0),txt="A"]
y.div>y.herb [nudge=c(0,-1,-.33,-1),txtnudge=c(0,-1.25),txt="B"]
y.clone>y.rich [nudge=c(0,1,.33,1),txtnudge=c(0,1.25),txt="C"]
y.clone>y.herb [nudge=c(0,-1,.33,-1),txtnudge=c(0,-1.25),txt="D"]
y.flood>y.herb [nudge=c(0,0,0,0),txtnudge=c(0.75,0.5),txt="F"]
y.rich>y.herb [nudge=c(0,0,0,0),txtnudge=c(0,0.5),txt="E"]
y.div>y.surv_next [nudge=c(0,.1,-.33,.1),txtnudge=c(0,-.5),txt="H"]
y.clone>y.surv_next [nudge=c(0,0,0,0),txtnudge=c(1,0),txt="I"]
y.herb>y.surv_next [nudge=c(0,0,0,0),txtnudge=c(0,0.5),txt="J"]
y.flood>y.surv_next [nudge=c(0,0,0,0),txtnudge=c(0.25,0.5),txt="K"]'

m6='y.div [pos=c(0,6),size=c(3.5,1.5),lab="Interspecific \\ndiversity"]
y.clone [pos=c(11,6),size=c(3.5,1.5),lab="Intraspecific \\ndiversity"]
y.rich [pos=c(0,0),size=c(3.5,1.5),lab="Insect \\nrichness"]
y.herb [pos=c(5.5,0),size=c(3.5,1.5),lab="Percent \\nherbivory"]
y.surv_next [pos=c(11,0),size=c(3.5,1.5),lab="Survival"]
y.div>y.rich [nudge=c(0,0,0,0),txtnudge=c(-1,0),txt="A"]
y.div>y.herb [nudge=c(0,-1,-.33,-1),txtnudge=c(0,-1.25),txt="B"]
y.clone>y.rich [nudge=c(0,1,.33,1),txtnudge=c(0,1.25),txt="C"]
y.clone>y.herb [nudge=c(0,-1,.33,-1),txtnudge=c(0,-1.25),txt="D"]
y.rich>y.herb [nudge=c(0,0,0,0),txtnudge=c(0,0.5),txt="E"]
y.div>y.surv_next [nudge=c(0,.1,-.33,.1),txtnudge=c(0,-.5),txt="H"]
y.clone>y.surv_next [nudge=c(0,0,0,0),txtnudge=c(1,0),txt="I"]
y.herb>y.surv_next [nudge=c(0,0,0,0),txtnudge=c(0,0.5),txt="J"]'


mi='y.pdiv [pos=c(0,6),size=c(3.5,1.5),lab="Interspecific \\ndiversity"]
y.clonen [pos=c(11,6),size=c(3.5,1.5),lab="Intraspecific \\ndiversity"]
y.a_div [pos=c(2,0),size=c(3.5,1.5),lab="Insect \\nrichness"]
y.herbivory [pos=c(9,0),size=c(3.5,1.5),lab="Percent \\nherbivory"]
y.floodn [pos=c(5.5,-5),size=c(3.5,1.5),lab="Water \\naddition"]
y.divint [pos=c(0,-5),size=c(3.5,1.5),lab="Interspecific x\\nWater"]
y.cloneint [pos=c(11,-5),size=c(3.5,1.5),lab="Intraspecific x\\nWater"]
y.pdiv>y.a_div [nudge=c(0,0,-.25,0),txtnudge=c(-.25,-.5),txt="A"]
y.pdiv>y.herbivory [nudge=c(0,0,-.25,0),txtnudge=c(.25,-.5),txt="B"]
y.clonen>y.a_div [nudge=c(0,1,.25,1),txtnudge=c(0,1.25),txt="C"]
y.cloneint>y.a_div [nudge=c(0,-1,.25,-1),txtnudge=c(2.5,-.25),txt="C"]
y.divint>y.a_div [nudge=c(0,0,-.25,0),txtnudge=c(-.25,.5),txt="C"]
y.clonen>y.herbivory [nudge=c(0,0,.25,0),txtnudge=c(.25,-.5),txt="D"]
y.floodn>y.herbivory [nudge=c(0,-.5,0,-.5),txtnudge=c(0,0),txt="F"]
y.a_div>y.herbivory [nudge=c(0,0,0,0),txtnudge=c(0,.5),txt="E"]
y.pdiv>y.divint [nudge=c(-.33,0,-.33,0),txtnudge=c(.75,0),txt="H"]
y.clonen>y.cloneint [nudge=c(.33,0,.33,0),txtnudge=c(-.75,0),txt="I"]
y.floodn>y.cloneint [nudge=c(0,0,0,0),txtnudge=c(0,.75),txt="K"]
y.floodn>y.divint [nudge=c(0,0,0,0),txtnudge=c(0,.75),txt="K"]
y.divint>y.herbivory [nudge=c(0,0,-.33,0),txtnudge=c(.5,.5),txt="D"]
y.cloneint>y.herbivory [nudge=c(0,0,.33,0),txtnudge=c(.25,.5),txt="D"]'

mi2='y.pdiv [pos=c(0,6),size=c(3.5,1.5),lab="Interspecific \\ndiversity"]
y.clonen [pos=c(11,6),size=c(3.5,1.5),lab="Intraspecific \\ndiversity"]
y.a_div [pos=c(2,0),size=c(3.5,1.5),lab="Insect \\nrichness"]
y.herbivory [pos=c(9,0),size=c(3.5,1.5),lab="Percent \\nherbivory"]
y.floodn [pos=c(5.5,-5),size=c(3.5,1.5),lab="Water \\naddition"]
y.divint [pos=c(0,-5),size=c(3.5,1.5),lab="Interspecific x\\nWater"]
y.pdiv>y.a_div [nudge=c(0,0,-.25,0),txtnudge=c(-.25,-.5),txt="A"]
y.pdiv>y.herbivory [nudge=c(0,0,-.25,0),txtnudge=c(.25,-.5),txt="B"]
y.clonen>y.a_div [nudge=c(0,1,.25,1),txtnudge=c(0,1.25),txt="C"]
y.divint>y.a_div [nudge=c(0,0,-.25,0),txtnudge=c(-.25,.5),txt="C"]
y.clonen>y.herbivory [nudge=c(0,0,.25,0),txtnudge=c(.25,-.5),txt="D"]
y.floodn>y.herbivory [nudge=c(0,-.5,0,-.5),txtnudge=c(0,0),txt="F"]
y.a_div>y.herbivory [nudge=c(0,0,0,0),txtnudge=c(0,.5),txt="E"]
y.pdiv>y.divint [nudge=c(-.33,0,-.33,0),txtnudge=c(.75,0),txt="H"]
y.floodn>y.divint [nudge=c(0,0,0,0),txtnudge=c(0,.75),txt="K"]
y.divint>y.herbivory [nudge=c(0,0,-.33,0),txtnudge=c(.5,.5),txt="D"]'

bmaski=c('B3'='Interspecific\ndiversity on\n insect richness',
         'B4'='Interspecific X\n water on\n insect richness',
         'B2'='Intraspecific\ndiversity on\n insect richness',
         'B5'='Intraspecific X\n water on\n insect richness',
         'A5'='Interspecific\ndiversity on\n herbivory',
         'A4'='Intraspecific\ndiversity on\n herbivory',
         'A7'='Intraspecific X\n water on\n herbivory',
         'A6'='Interspecific X\n water on\n herbivory',
         'A3'='Water addition\non herbivory',
         'A2'='Insect richness\non herbivory',
         'C3'='Water addition on\n Interspecific X water',
         'D2'='Intraspecific\ndiversity on\n Intraspecific X water',
         'C2'='Interspecific\ndiversity on\n Interspecific X water',
         'D3'='Water addition on\n Intraspecific X water')
bmaski2=c('B3'='Interspecific\ndiversity on\n insect richness',
          'B4'='Interspecific X\n water on\n insect richness',
          'B2'='Intraspecific\ndiversity on\n insect richness',
          'B5'='Intraspecific X water on\n insect richness',
          'A5'='Interspecific\ndiversity on\n herbivory',
          'A4'='Intraspecific\ndiversity on\n herbivory',
          'A6'='Interspecific X\n water on\n herbivory',
          'A3'='Water addition\non herbivory',
          'A2'='Insect richness\non herbivory',
          'C3'='Water addition on\n Interspecific X water',
          'C2'='Interspecific\ndiversity on\n Interspecific X water')
bname=c('CR','EC','PE','UI','MG')   
s=4                                                                             #sem plots
criplot=path(mi,crsemi@jags_model,mask=maski,size=11,scal=s,outline=F,txtoff=T,autonudge=F,thresh=.005)+
  ggtitle('Costa Rica (CR)')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=50))
eciplot=path(mi,ecsemi@jags_model,mask=maski,size=11,scal=s,outline=F,txtoff=T,autonudge=F,thresh=.005)+
  ggtitle('Ecuador (EC)')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=50))
peiplot=path(mi2,pesemi@jags_model,mask=maski2,size=11,scal=s,outline=F,txtoff=T,autonudge=F,thresh=.005)+
  ggtitle('Peru (PE)')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=50))
intbars=sembars(semmerge(crsemi,ecsemi,pesemi,
                         names=bname[1:3],
                         mask=list(bmaski,bmaski,bmaski2),
                         filter=c('C2','C3','D2','D3')),
                s=10,flip=T,group='groups',label=.5,labsize=10*3)+
  theme(text=element_text(size=50),
        panel.border=element_rect(size=2))+
  xlab('')+
  ylab('Parameter estimate')

png('./figures/criplot.png',1980,3200,type='cairo');plot_grid(plot_grid(criplot,eciplot,peiplot,ncol=1),
                                                              intbars,rel_widths=c(1,.8),
                                                              ncol=2);dev.off();system2('open','./figures/criplot.png')

crplot=path(m2,crsem@jags_model,scal=s,size=15,alpha=10,outline=F,mask=mask,txtoff=T,autonudge=F,thresh=.005)+
  ggtitle('Costa Rica (CR)')+
  theme(text=element_text(size=75))+
  ylim(-6.5,6.5)+
  theme(plot.title = element_text(hjust = 0.5))
ecplot=path(m2,ecsem,scal=s,size=15,alpha=10,outline=F,mask=mask,txtoff=T,autonudge=F,thresh=.005)+
  ggtitle('Ecuador (EC)')+
  theme(text=element_text(size=75))+
  ylim(-6.5,6.5)+
  theme(plot.title = element_text(hjust = 0.5))
peplot=path(m2,pesem,scal=s,size=15,alpha=10,outline=F,mask=mask,txtoff=T,autonudge=F,thresh=.005)+
  ggtitle('Peru (PE)')+
  theme(text=element_text(size=75))+
  ylim(-6.5,6.5)+
  theme(plot.title = element_text(hjust = 0.5))
mgplot=path(m3,mgsemb,scal=s,size=15,alpha=10,outline=F,mask=mask2,txtoff=T,autonudge=F,thresh=.005)+
  ggtitle('Mogi Guaçu (MG)')+
  theme(text=element_text(size=75))+
  ylim(-1.5,6.5)+
  theme(plot.title = element_text(hjust = 0.5))
uiplot=path(m3,uisemb,scal=s,size=15,alpha=10,outline=F,mask=mask2,txtoff=T,autonudge=F,thresh=.005)+
  ggtitle('Uaimii (UI)')+
  theme(text=element_text(size=75))+
  ylim(-1.5,6.5)+
  theme(plot.title = element_text(hjust = 0.5))

s2=2.5                                                                          #sem mort plots
crplot2=path(m55,crsem21,scal=s2,size=12,alpha=10,outline=F,mask=mask3,txtoff=T,autonudge=F,thresh=.001)+
  ggtitle('Costa Rica (CR)')+
  theme(text=element_text(size=60))+
  ylim(-6.5,7.5)+
  theme(plot.title = element_text(hjust = 0.5))
ecplot2=path(m5,ecsem21,scal=s2,size=12,alpha=10,outline=F,mask=mask35,txtoff=T,autonudge=F,thresh=.001)+
  ggtitle('Ecuador (EC)')+
  theme(text=element_text(size=60))+
  ylim(-6.5,7.5)+
  theme(plot.title = element_text(hjust = 0.5))
peplot2=path(m5,pesem21,scal=s2,size=12,alpha=10,outline=F,mask=mask35,txtoff=T,autonudge=F,thresh=.001)+
  ggtitle('Peru (PE)')+
  theme(text=element_text(size=60))+
  ylim(-6.5,7.5)+
  theme(plot.title = element_text(hjust = 0.5))
mgplot2=path(m6,mgsem20,scal=s2,size=12,alpha=10,outline=F,mask=mask4,txtoff=T,autonudge=F,thresh=.001)+
  ggtitle('Mogi (MG)')+
  theme(text=element_text(size=60))+
  ylim(-1.5,7.5)+
  theme(plot.title = element_text(hjust = 0.5))
uiplot2=path(m6,uisem20,scal=s2,size=12,alpha=10,outline=F,mask=mask4,txtoff=T,autonudge=F,thresh=.001)+
  ggtitle('Uaimii (UI)')+
  theme(text=element_text(size=60))+
  ylim(-1.5,7.5)+
  theme(plot.title = element_text(hjust = 0.5))

blank=ggplot()+theme_void()                                                     #blank plot for formatting 
bname=c('CR','EC','PE','UI','MG')                                               #create masks for dot plots
bmask=c('B3'='Interspecific diversity \non insect richness',
        'B2'='Intraspecific diversity \non insect richness',
        'A5'='Interspecific diversity \non herbivory',
        'A4'='Intraspecific diversity \non herbivory',
        'A3'='Water addition \non herbivory',
        'A2'='Insect richness \non herbivory')
bmask2=c('B3'='Interspecific diversity \non insect richness',
         'B2'='Intraspecific diversity \non insect richness',
         'A4'='Interspecific diversity \non herbivory',
         'A3'='Intraspecific diversity \non herbivory',
         'A2'='Insect richness \non herbivory')
bmask3=c('B3'='Interspecific diversity \non insect richness',
         'B2'='Intraspecific  diversity \non insect richness',
         'A4'='Interspecific diversity \non herbivory',
         'A3'='Intraspecific diversity \non herbivory',
         'A5'='Water addition \non herbivory',
         'A2'='Insect richness \non herbivory',
         'C2'='Water addition \non survival',
         'C3'='Intraspecific diversity \non survival',
         'C4'='Interspecific diversity \non survival',
         'C5'='Herbivory \non survival')
bmask35=c('B3'='Interspecific diversity \non insect richness',
          'B2'='Intraspecific  diversity \non insect richness',
          'A4'='Interspecific diversity \non herbivory',
          'A3'='Intraspecific diversity \non herbivory',
          'A5'='Water addition \non herbivory',
          'A2'='Insect richness \non herbivory',
          'C5'='Water addition \non survival',
          'C2'='Intraspecific diversity \non survival',
          'C3'='Interspecific diversity \non survival',
          'C4'='Herbivory \non survival')
bmask4=c('B3'='Interspecific diversity \non insect richness',
         'B2'='Intraspecific  diversity \non insect richness',
         'A4'='Interspecific diversity \non herbivory',
         'A3'='Intraspecific diversity \non herbivory',
         'A2'='Insect richness \non herbivory',
         'C2'='Intraspecific diversity \non survival',
         'C3'='Interspecific diversity \non survival',
         'C4'='Herbivory \non survival')

five=plot_grid(plot_grid(crplot,ecplot,peplot,nrow=1),                          #merge sem plots and create dotplots
               plot_grid(blank,uiplot,mgplot,blank,nrow=1,rel_widths = c(.5,1,1,.5)),nrow=2,rel_heights = c(13,8))
fivebars=sembars(semmerge(crsem,ecsem,pesem,uisemb,mgsemb,names=bname,mask=list(bmask,bmask,bmask,bmask2,bmask2)),
                 s=15,flip=F,group='groups',label=.45,labsize=15*3)+
  theme(text=element_text(size=48),
        panel.border=element_rect(size=2))+
  coord_cartesian(ylim=c(-.2,.5))+
  xlab('')


fivemort=plot_grid(plot_grid(crplot2,ecplot2,peplot2,nrow=1),
                   plot_grid(blank,uiplot2,mgplot2,blank,nrow=1,rel_widths = c(.5,1,1,.5)),nrow=2,rel_heights = c(13,8))
mortbars=sembars(semmerge(crsem21,ecsem21,pesem21,uisem20,mgsem20,names=bname,mask=list(bmask35,bmask3,bmask3,bmask4,bmask4)),
                 s=11,flip=F,group='groups',label=1,labsize=45)+
  theme(text=element_text(size=50))+
  coord_cartesian(ylim=c(-1.1,1.1))+
  xlab('')+ylab('Standardized path estimate')


f2=fivebars+scale_x_discrete(breaks=bname,               
                             labels=bname)+xlab('')+ylab('Parameter estimate')
png('./figures/fivemodels.png',1000*3,1500+1500*8/13,type='cairo');five;dev.off() #save figures
fiveplus=plot_grid(
  five,f2,rel_heights=c(1500+1500*8/13,1000),nrow=2,
  labels=c("A",'B'),label_size=80)
png('./figures/fiveplus.png',
    1000*3,1000+1500+1500*8/13,type='cairo');plot_grid(
      five,f2,rel_heights=c(1500+1500*8/13,1000),nrow=2,
      labels=c("A",'B'),label_size=80);dev.off()
png('./figures/fivemort.png',1000*3,1500+1500*8/13,type='cairo');fivemort;dev.off()
png('./figures/mortplus.png',
    1000*3,1500+1500*8/13+1000,type='cairo');plot_grid(
      fivemort,mortbars,rel_heights=c(1500+1500*8/13,1000),nrow=2,
      labels=c('A','B'),label_size=80);dev.off();system2('open','./figures/mortplus.png')

#### meta path models ####
s=7
meta1=path(m1,scal=s,size=15,alpha=10,outline=F,txtoff=F,autonudge=F,thresh=.005)+
  ggtitle('Model I')+
  theme(text=element_text(size=80))+
  ylim(-6.5,6.5)+
  theme(plot.title = element_text(hjust = 0.5))
meta2=path(m2,scal=s,size=15,alpha=10,outline=F,txtoff=F,autonudge=F,thresh=.005)+
  ggtitle('Model II')+
  theme(text=element_text(size=80))+
  ylim(-6.5,6.5)+
  theme(plot.title = element_text(hjust = 0.5))
meta3=path(m3,scal=s,size=15,alpha=10,outline=F,txtoff=F,autonudge=F,thresh=.005)+
  ggtitle('Model III')+
  theme(text=element_text(size=80))+
  ylim(-6.5,6.5)+
  theme(plot.title = element_text(hjust = 0.5))

left=plot_grid(meta1,meta2,meta3,nrow=3)

s2=7                                                  
meta4=path(m4,scal=s2,size=15,alpha=10,outline=F,txtoff=F,autonudge=F,thresh=.001)+
  ggtitle('Model IV')+
  theme(text=element_text(size=80))+
  ylim(-6.5,7.5)+
  theme(plot.title = element_text(hjust = 0.5))
meta5=path(m5,scal=s2,size=15,alpha=10,outline=F,txtoff=F,autonudge=F,thresh=.001)+
  ggtitle('Model V')+
  theme(text=element_text(size=80))+
  ylim(-6.5,7.5)+
  theme(plot.title = element_text(hjust = 0.5))
meta6=path(m6,scal=s2,size=15,alpha=10,outline=F,txtoff=F,autonudge=F,thresh=.001)+
  ggtitle('Model VI')+
  theme(text=element_text(size=80))+
  ylim(-6.5,7.5)+
  theme(plot.title = element_text(hjust = 0.5))

right=plot_grid(meta4,meta5,meta6,nrow=3)
png('./figures/meta.png',2300,4500,type='cairo');plot_grid(left,right,rel_widths=c(1,1.1));dev.off();system2('open','./figures/meta.png')

hyp=read.csv('./files/hypotheses.csv')
gt(hyp)|>gtsave('table.png','./figures/',vwidth=500,vheight=1000);system2('open','./figures/table.png')

#### temp interaction plots ####

labs=data.frame('x'=.5,'y'=30,'text'=c('50.4%','92%',''),'site'=c('Costa Rica','Ecuador','peru'),'c_i'=NA)
labs2=data.frame('x'=.5,'y'=30,'text'=c('81.5%','54.9%','51.5%'),'site'=c('Costa Rica','Ecuador','peru'),'c_i'=NA)

s=5

herbint_plot=plot_grid(
  ggplot(herb3,aes(x=(flood=='flood')+0,y=wilmer_herb_total,color=c_i))+
    geom_point(stat='summary')+
    geom_errorbar(stat='summary',size=s/2,width=.3)+
    geom_smooth(method=lm,alpha=0,size=s)+
    facet_grid(cols=vars(site),labeller=labeller(.cols=setNames(c('Costa Rica','Ecuador','Perú'),c('Costa Rica','Ecuador','peru'))))+
    theme_classic()+
    theme(panel.border=element_rect(fill=F),panel.spacing=unit(0,'lines'))+
    theme(text=element_text(size=50))+
    scale_color_manual(values=wat,labels=c('Low','High'),
                       guide=guide_legend(
                         direction = "horizontal",
                         title.position = "top",
                         override.aes=list(shape=15,linetype=0,size=20)))+
    ylab('Percent herbivory\n')+
    xlab('Water addition\n')+
    labs(color='Intraspecific diversity')+
    scale_x_continuous(breaks=0:1,labels=c('No','Yes'))+
    theme(legend.position="top")+
    coord_cartesian(ylim=c(5,30))+
    theme(legend.key.size = unit(2, 'cm'))+
    geom_text(data=labs,aes(x=x,y=y,label=text),size=15,color='black'),
  ggplot(herb3,aes(x=(flood=='flood')+0,y=wilmer_herb_total,color=as.factor(div>2)))+
    geom_point(stat='summary')+
    geom_errorbar(stat='summary',size=s/2,width=.3)+
    geom_smooth(method=lm,alpha=0,size=s)+
    facet_grid(cols=vars(site),labeller=labeller(.cols=setNames(c('Costa Rica','Ecuador','Perú'),c('Costa Rica','Ecuador','peru'))))+
    theme_classic()+
    theme(panel.border=element_rect(fill=F),panel.spacing=unit(0,'lines'))+
    theme(text=element_text(size=50))+
    scale_color_manual(values=wat,labels=c('<=2 species','>2 species'),
                       guide=guide_legend(
                         direction = "horizontal",
                         title.position = "top",
                         override.aes=list(shape=15,linetype=0,size=20)))+
    ylab('\n')+
    xlab('Water addition\n')+
    labs(color='Interspecific richness')+
    scale_x_continuous(breaks=0:1,labels=c('No','Yes'))+
    theme(legend.position="top")+
    coord_cartesian(ylim=c(5,30))+
    theme(legend.key.size = unit(2, 'cm'))+
    geom_text(data=labs2,aes(x=x,y=y,label=text),size=15,color='black'))

presint_plot=plot_grid(
  ggplot(herb3,aes(x=(flood=='flood')+0,y=pres,color=c_i))+
    geom_point(stat='summary')+
    geom_errorbar(stat='summary')+
    geom_smooth(method=lm,alpha=0,size=s)+
    facet_grid(cols=vars(site),labeller=labeller(.cols=setNames(c('Costa Rica','Ecuador','Perú'),c('Costa Rica','Ecuador','peru'))))+
    theme_classic()+
    theme(panel.border=element_rect(fill=F),panel.spacing=unit(0,'lines')),
  ggplot(herb3,aes(x=(flood=='flood')+0,y=pres,color=as.factor(div>2)))+
    geom_point(stat='summary')+
    geom_errorbar(stat='summary')+
    geom_smooth(method=lm,alpha=0)+
    facet_grid(cols=vars(site),labeller=labeller(.cols=setNames(c('Costa Rica','Ecuador','Perú'),c('Costa Rica','Ecuador','peru'))))+
    theme_classic()+
    theme(panel.border=element_rect(fill=F),panel.spacing=unit(0,'lines')))

varint_plot=plot_grid(
  ggplot(var3,aes(x=floodn+0,y=vari,color=as.factor(clonen)))+
    geom_point(stat='summary')+
    geom_errorbar(stat='summary')+
    geom_smooth(method=lm,alpha=0,size=s)+
    facet_grid(cols=vars(site),labeller=labeller(.cols=setNames(c('Costa Rica','Ecuador','Perú'),c('Costa Rica','Ecuador','peru'))))+
    theme_classic()+
    theme(panel.border=element_rect(fill=F),panel.spacing=unit(0,'lines')),
  ggplot(var3,aes(x=floodn+0,y=vari,color=as.factor(final_div>2)))+
    geom_point(stat='summary')+
    geom_errorbar(stat='summary')+
    geom_smooth(method=lm,alpha=0)+
    facet_grid(cols=vars(site),labeller=labeller(.cols=setNames(c('Costa Rica','Ecuador','Perú'),c('Costa Rica','Ecuador','peru'))))+
    theme_classic()+
    theme(panel.border=element_rect(fill=F),panel.spacing=unit(0,'lines')))

labs=data.frame('x'=.5,'y'=2,'text'=c('50.4%','99.5%',''),'site'=c('Costa Rica','Ecuador','peru'),'c_i'=NA)
labs2=data.frame('x'=.5,'y'=2,'text'=c('98.7%','99.7%','95.1%'),'site'=c('Costa Rica','Ecuador','peru'),'c_i'=NA)

divint_plot=plot_grid(
  ggplot(div3,aes(x=(flood=='flood')+0,y=a_div,color=c_i))+
    geom_point(stat='summary')+
    geom_errorbar(stat='summary',size=s/2,width=.3)+
    geom_smooth(method=lm,alpha=0,size=s)+
    facet_grid(cols=vars(site),labeller=labeller(.cols=setNames(c('Costa Rica','Ecuador','Perú'),c('Costa Rica','Ecuador','peru'))))+
    theme_classic()+
    theme(panel.border=element_rect(fill=F),panel.spacing=unit(0,'lines'))+
    theme(text=element_text(size=50))+
    scale_color_manual(values=wat)+
    ylab('Insect richness')+
    xlab('Water addition\n')+
    labs(color='Intraspecific\ndiversity')+
    scale_x_continuous(breaks=0:1,labels=c('No','Yes'))+
    guides(color='none')+
    theme(legend.position="top")+
    coord_cartesian(ylim=c(1,2))+
    geom_text(data=labs,aes(x=x,y=y,label=text),size=15,color='black'),
  ggplot(div3,aes(x=(flood=='flood')+0,y=a_div,color=as.factor(div>2)))+
    geom_point(stat='summary')+
    geom_errorbar(stat='summary',size=s/2,width=.3)+
    geom_smooth(method=lm,alpha=0,size=s)+
    facet_grid(cols=vars(site),labeller=labeller(.cols=setNames(c('Costa Rica','Ecuador','Perú'),c('Costa Rica','Ecuador','peru'))))+
    theme_classic()+
    theme(panel.border=element_rect(fill=F),panel.spacing=unit(0,'lines'))+
    theme(text=element_text(size=50))+
    scale_color_manual(values=wat)+
    ylab('')+
    xlab('Water addition\n')+
    labs(color='Interspecific\ndiversity')+
    scale_x_continuous(breaks=0:1,labels=c('No','Yes'))+
    guides(color='none')+
    theme(legend.position="top")+
    coord_cartesian(ylim=c(1,2))+
    geom_text(data=labs2,aes(x=x,y=y,label=text),size=15,color='black'))

labs=data.frame('x'=.5,'y'=100,'text'=c('98%','90.5%','84.1%'),'site'=c('Costa Rica','Ecuador','peru'),'c_i'=NA)
labs2=data.frame('x'=.5,'y'=100,'text'=c('66.8%','85.4%','69.4%'),'site'=c('Costa Rica','Ecuador','peru'),'c_i'=NA)

mortint_plot=plot_grid(
  ggplot(mort3,aes(x=as.numeric(flood),y=alive,color=c_i))+
    geom_point(stat='summary')+
    geom_errorbar(stat='summary',size=s/2,width=.3)+
    geom_smooth(method=lm,alpha=0,size=s)+
    facet_grid(cols=vars(site),labeller=labeller(.cols=setNames(c('Costa Rica','Ecuador','Perú'),c('Costa Rica','Ecuador','peru'))))+
    theme_classic()+
    theme(panel.border=element_rect(fill=F),panel.spacing=unit(0,'lines'))+
    theme(text=element_text(size=50))+
    scale_color_manual(values=wat)+
    ylab('Percent survival')+
    xlab('Water addition\n')+
    labs(color='Intraspecific\ndiversity')+
    scale_x_continuous(breaks=0:1,labels=c('No','Yes'))+
    guides(color='none')+
    theme(legend.position="top")+
    coord_cartesian(ylim=c(0,100))+
    geom_text(data=labs,aes(x=x,y=y,label=text),size=15,color='black'),
  ggplot(mort3,aes(x=as.numeric(flood),y=alive,color=as.factor(div>2)))+
    geom_point(stat='summary')+
    geom_errorbar(stat='summary',size=s/2,width=.3)+
    geom_smooth(method=lm,alpha=0,size=s)+
    facet_grid(cols=vars(site),labeller=labeller(.cols=setNames(c('Costa Rica','Ecuador','Perú'),c('Costa Rica','Ecuador','peru'))))+
    theme_classic()+
    theme(panel.border=element_rect(fill=F),panel.spacing=unit(0,'lines'))+
    theme(text=element_text(size=50))+
    scale_color_manual(values=wat)+
    ylab('')+
    xlab('Water addition\n')+
    labs(color='Interspecific\ndiversity')+
    scale_x_continuous(breaks=0:1,labels=c('No','Yes'))+
    guides(color='none')+
    theme(legend.position="top")+
    coord_cartesian(ylim=c(0,100))+
    geom_text(data=labs2,aes(x=x,y=y,label=text),size=15,color='black'))

intplots=list(herbint_plot,presint_plot,varint_plot,divint_plot,mortint_plot)
for(i in intplots){i=i+
  theme(panel.border=element_rect(fill=NA,size=2))}
png('./figures/interactions.png',2000,5000,type='cairo');do.call(grid.arrange,c(intplots,ncol=1));dev.off()
system2('open','./figures/interactions.png')
intplots=list(herbint_plot,divint_plot,mortint_plot)
for(i in intplots){i=i+
  theme(panel.border=element_rect(fill=NA,size=2))}
png('./figures/interactions2.png',2000,3000,type='cairo');do.call(grid.arrange,c(intplots,ncol=1));dev.off()
system2('open','./figures/interactions2.png')

#### sems in map####

# t1=plot_grid(crplot,ecplot,peplot,ncol=1)
# t2=plot_grid(uiplot,map2,mgplot,ncol=1)
# png('./figures/map2.png',2200,4500,type='cairo');plot_grid(
#   plot_grid(t1,blank,t2,ncol=3,rel_widths=c(1000,200,1000)),
#   fivebars+ylab('Standardized path estimate'),ncol=1,rel_heights=c(3700,800));dev.off();system2('open','./figures/map2.png')

t1=plot_grid(crplot,ecplot,peplot,ncol=1)
t2=plot_grid(uiplot,map2,mgplot,ncol=1)
fivebars=sembars(semmerge(crsem,ecsem,pesem,uisemb,mgsemb,names=bname,mask=list(bmask,bmask,bmask,bmask2,bmask2)),
                 s=15,flip=T,group='groups',label=.45,labsize=15*3)+
  theme(text=element_text(size=70),
        panel.border=element_rect(size=2))+
  xlab('')

png('./figures/map2.png',3200,3700,type='cairo');plot_grid(
  plot_grid(t1,blank,t2,ncol=3,rel_widths=c(1000,200,1000)),
  fivebars+ylab('Standardized path estimate'),ncol=2,rel_widths=c(2200,1000));dev.off();system2('open','./figures/map2.png')



png('./figures/map_factor.png',4000,2000/70*90,type='cairo');grid.arrange(tf,smallm,ncol=2);dev.off()
png('./figures/map sem.png',4000,6671.429,type='cairo');plot_grid(grid.arrange(tf,smallm,ncol=2),
                                                                  plot_grid(blank,
                                                                            plot_grid(
                                                                              plot_grid(crplot,ecplot,peplot,nrow=1),
                                                                              plot_grid(
                                                                                plot_grid(uiplot,ncol=1,rel_heights=c(1,.5)),
                                                                                plot_grid(mgplot,ncol=1,rel_heights=c(1,.5)),
                                                                                blank,nrow=1),rel_heights=c(1500,1000),
                                                                              nrow=2),
                                                                            blank,
                                                                            fivebars,labels=c('','B','','C'),label_size=100,
                                                                            ncol=1,rel_heights=c(50,3000,50,1000)),labels=c('A',''),label_size=100,
                                                                  rel_heights=c(2571.429,4100),
                                                                  ncol=1);dev.off()
