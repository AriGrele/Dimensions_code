evenness=function(x){return(diversity(x)/log(specnumber(x)))}
evenness2=function(x){return(max(x,na.rm=T)/sum(x,na.rm=T))}
framesum=function(x){o=list()                                                   #sum function across rows of dataframe
for(i in 1:length(x[,1])){o[i]=sum(x[i,1:length(x)],na.rm=T)}
o[o==0]=NA;return(as.numeric(o))}

lats=list('Ecuador'=-1.43,'Costa Rica'=10.43,'peru'=-12,'mogi'=-23.55,'Brazil'=-20.3)

d=read.csv('./files/herbivory_master_file_with_mogi2.csv',                      #load data
           stringsAsFactors = FALSE,na.strings=c(""," ","NA"))
d=subset(d,d$experiment_observation=='experiment')                              #subset to just experimental data

d$experiment_final_intermediate[is.na(d$experiment_final_intermediate)]='final'
d$experiment_final_intermediate[d$site=='Brazil'&
                                  d$experiment_final_intermediate==3]='final'
d=subset(d,d$experiment_final_intermediate=='final'|
           d$experiment_final_intermediate=='final')                            #only look at final data
d$nlvs_per_ind=as.numeric(as.character(d$nlvs_per_ind))                         #leaves to numeric

d$wilmer_herb_total[d$wilmer_herb_total=='.,']=NA                               #herbivory to numeric
d$wilmer_herb_total=as.numeric(d$wilmer_herb_total)
d$wilmer_herb_total[is.na(d$wilmer_herb_total)]=                                #fix rows with missing total herbivory if possible
  framesum(d[21:31][c(T,F)][is.na(d$wilmer_herb_total),])
d=d[!is.na(d$wilmer_herb_total),]                                               #otherwise remove them, 105 rows
d$nlvs_per_ind=as.numeric(d$nlvs_per_ind)                                       #leaf count to numeric
d$div=as.numeric(match(gsub('^m|.$','',d$div),                                  #convert plant diversity from characters to ints
                       c('on','tw','thre','fou','','si','','','','te','','twelv')))
d$notes=NA

d=subset(d,is.na(d$notes))                                                      #remove rows that have issue notes, e.g. dead leaves, incorrect codes. 158 rows
d$c_i=gsub('s','',d$c_i)                                                        #clones->clone
d=subset(d,regexec('.+ .+ ',d$species)<0)                                       #remove notes from piper species. 23 rows
d$species[d$species=='Piper sp']=d$species2[d$species=='Piper sp']              #for unknown species, use name from species 2 column
d$species=gsub('.+ ','',d$species)                                              #remove piper from species name
d$species=gsub('seod','seud',d$species)                                         #merge pseudobumbratum, pseodobumbratum
d=d[,colSums(is.na(d))!=nrow(d)]                                                #remove 14 empty columns.
d=d[-c(1,11,12,15,29,30)]                                                       #remove 5 unnecessary columns
d$plant=as.character(paste(d$site,
                           d$plant_code,
                           d$plot,
                           d$experiment_final_intermediate,sep='_'))            #unique plant identifier, check final w/ tara
d$canopy_openness=d%>%group_by(plant)%>%                                        #fix plants with two openness measures, or some measures NA
  mutate(min(canopy_openness,na.rm=T))%>%pull()
d$canopy_openness[is.infinite(d$canopy_openness)]=NA                            #replace infinite openness with NA
d$site=as.factor(d$site)
d$site=factor(d$site,levels=names(lats))
d$flood[d$site=='Ecuador' & d$div==1]='no'

for(i in c(13:23)[c(T,F)]){d[,i][grep('\\d',d[,i])]=NA;d[,i][d[,i]=='']=NA}     
#fix order names
colnames(d)[13:23][c(T,F)]=c('lepidoptera1','lepidoptera2','coleoptera1','coleoptera2','other1','other2')

d$latitude=NA;d$long=NA;d$alt=NA
d=d[!is.na(d$wilmer_herb_total),]                                               #remove row added without herbivory measurements
d$latitude[d$site=='mogi']=-23.55
d$latitude[d$site=='Costa Rica' & is.na(d$alt)]=10.43
d$latitude[d$site=='peru' & is.na(d$alt)]=11.09
d$alt[d$site=='mogi']=745
d$alt[d$site=='Costa Rica' & is.na(d$alt)]=65
d$alt[d$site=='peru' & is.na(d$alt)]=1155.55
d$alat=abs(d$latitude)

dm=data.frame(row.names=c(colnames(d)[c(1:12,28:length(d))],                    #empty data.frame for first set
                          'order','family','herbivory')) 
for(i in c(13:23)[c(T,F)]){                                                     #for each order, append herbivory/family to end of dataframe
  dm=rbind(dm,cbind(d[c(1:12,28:length(d))],
                    data.frame('order'=colnames(d)[i],
                               'family'=d[,i],
                               'herbivory'=d[,i+1])))}
families=as.character(na.omit(unique(dm$family)))                               #all family names
f=length(families)                                                              #number of families

dl=d[c(1:12,28:length(d))]                                                      #subset of all data for new frame
dl$plant=paste(dl$plant,1:nrow(dl),sep='_')                                     #leaf level diversity
#dl$plant=paste(dl$site,dl$block,dl$plot)                                       #plot level diversity

for(i in families){                                                             #for each family name, add column of herbivory for each leaf
  x=numeric(length(d$site))
  h=subset(dm,dm$order==na.omit(dm$order[dm$family==i])[1])
  c=h$family==i;c[is.na(c)]=F
  x[c]=h$herbivory[c]
  dl=cbind(dl,data.frame(x))}
colnames(dl)[16:(15+f)]=paste(families,'herbivory',sep='_')                     #names for herbivory columns ####18!!!!!!
for(i in 1:f){dl=cbind(dl,as.logical((dl[,i+15])>0)+0)}                         #for each herbivory column, add column of presence/absence of that family on each leaf

colnames(dl)[(16+f):(15+f+f)]=paste(families,'abundance',sep='_')               #names for abundance columns
# for(i in names(dl)){                                                          #filter specific taxa
#   for(rf in c("Geometridae/eois","Chrysomelidae","minero")){
#     if(regexpr(rf,i)>0){dl[i]=0}}}

a=aggregate(dl[,12],list(dl$plant),                                             #set up aggregation
            function(x){return(mean(x,na.rm=T))})
for(i in 1:f){                                                                  #for each herbivory column, add mean aggregate column by plant
  a=cbind(a,aggregate(dl[,15+i],list(dl$plant),
                      function(x){return(mean(x,na.rm=T))})$x)}
for(i in 1:f){                                                                  #for each abundance column, add sum aggregate column by plant
  a=cbind(a,aggregate(dl[,15+i+f],list(dl$plant),function(x) sum(x,na.rm=T))$x)}
colnames(a)=c('plant','herbivory',
              paste(families,'herbivory',sep='_'),
              paste(families,'abundance',sep='_'))

total=d[-c(13:28)]                                                              #dataset of cleaned, total herbivory data
family=dm[!is.na(dm$family),][-12]                                              #dataset of cleaned, family level herbivory data
family$order=gsub('\\d','',family$order)                                        #remove numbers from order names
divp=unique(merge(dl[1:17][-c(11,12)],                                          #dataset of cleaned, plant level diversity data
                  data.frame('plant'=a$plant,
                             'h_div'=(evenness(a[regexpr('_herbivory',colnames(a))>0])),
                             'a_div'=exp(diversity(a[regexpr('_abundance',colnames(a))>0])),
                             'herbivory'=a$herbivory)))

a2=merge(divp[c('site','block','plot','plant')],a,by='plant')
a2=cbind(a2[1:4],a2[regexpr('_abundance',colnames(a2))>0])
a2$block
aout=setNames(as.data.frame(matrix(ncol=4)),c('site','block','plot','richness'))
for(q in unique(a2$site)){
  for(w in unique(a2$block[a2$site==q])){
    for(e in unique(a2$plot[a2$site==q&a2$block==w])){
      cs=colSums(a2[a2$site==q&a2$block==w&a2$plot==e,][-4:0])
      aout=rbind(aout,data.frame('site'=q,'block'=w,'plot'=e,'richness'=sum(cs>0,na.rm=T)))
    }
  }
}
aout$site=factor(aout$site,levels=levels(divp$site))

dl$plant=gsub('_\\d+$','',dl$plant)                                             #remove numerals from plant
divp$plant=gsub('_\\d+$','',divp$plant)                                       
nlvs=aggregate(dl$nlvs_per_ind,list(dl$plant),length)                           #number of leaves
colnames(nlvs)=c('plant','nlvs_per_ind')                                        #correct nlvs names
divp=merge(divp,nlvs,by='plant')                                                #nlvs in divp frame

crr=read_excel("./files/cr_exp_plot_richness.xlsx")                             #cr plot level richness
crr$block=NA;crr$site='Costa Rica'
crr$plot=paste('Plot',crr$plot,sep=' ')
colnames(crr)=c('plot','plot_richness','block','site')
mr=read_excel("./files/mogi_exp_plot_richness.xlsx")[1:3]                       #mg plot level richness
mr$site='mogi'
cmr=rbind(crr,mr)                                                               #bind cr and mg plot level richness
total=merge(total,cmr,by=c('site','block','plot'),all=T)                        #merge into total
family=merge(family,cmr,by=c('site','block','plot'),all=T)                      #merge into family
divp=merge(divp,cmr,by=c('site','block','plot'),all=T)                          #merge into divp
total$dif=total$plot_richness-total$div                                         #change in plot level richness
family$dif=family$plot_richness-family$div
divp$dif=divp$plot_richness-divp$div

total$site=factor(total$site,levels=names(lats))                                #site factor levels
#proper tax div names
total$divf[total$div>2]='high';total$divf[total$div==2]='medium';total$divf[total$div==1]='low'
divp$divf[divp$div>2]='high';divp$divf[divp$div==2]='medium';divp$divf[divp$div==1]='low'
total$plant=with(total,paste(site,plot,plant_code,sep='_'))                     #fix plant code and plant names
divp$plant=with(divp,paste(site,plot,plant_code,sep='_'))
total$nlvs_per_ind=total%>%group_by(plant)%>%mutate(n())%>%pull()               #fix nlvs
total$divf=factor(total$divf,levels=c('low','medium','high'))                   #set proper factors
divp$divf=factor(divp$divf,levels=c('low','medium','high'))
total=total[!is.na(total$wilmer_herb_total),]                                   #remove NAs
#calcualte final plot diveristy
total$final_div=total%>%group_by(paste(site,block,plot))%>%mutate(length(unique(species)))%>%pull()
divp$final_div=divp%>%group_by(paste(site,block,plot))%>%mutate(length(unique(species)))%>%pull()
total$pdiv=total%>%group_by(paste(site))%>%mutate(div/max(div,na.rm=T))%>%pull()
divp$pdiv=divp%>%group_by(paste(site))%>%mutate(div/max(div,na.rm=T))%>%pull()

precip=c('Costa Rica'=4495,'Ecuador'=2900,'peru'=1767,'mogi'=1271)              #average climate variables
temp=c('Costa Rica'=25,'Ecuador'=16,'peru'=23,'mogi'=26)
total$precip=sapply(total$site,function(x) precip[x])                           #merge into data.frames
total$temp=sapply(total$site,function(x) temp[x])
divp$precip=sapply(divp$site,function(x) precip[x])
divp$temp=sapply(divp$site,function(x) temp[x])
total$pres=as.numeric(total$wilmer_herb_total>0)*100                            #pres as numeric
total$finalf=total$final_div
total$finalf[total$finalf>2]='high'                                             #div level names
total$finalf[total$finalf=='2']='medium'
total$finalf[total$finalf=='1']='low'

divp$finalf=divp$final_div                                                      
divp$finalf[divp$finalf>2]='high'
divp$finalf[divp$finalf=='2']='medium'
divp$finalf[divp$finalf=='1']='low'
divp$finalf=factor(divp$finalf,levels=c('low','medium','high'))
family$spec='generalist';family$spec[family$family=='Geometridae/eois'|         #fix insect names
                                       family$order=='coleoptera'|
                                       family$family=='minero']='specialist'
total$flood[is.na(total$flood)]='no'                                            #NA flood to 'no'

total$block[is.na(total$block)]='none'                                          #missing blocks as 'none'
divp$block[is.na(divp$block)]='none'
pt=setNames(with(total,aggregate(pres,list(site,block,plot,plant_code),mean)),  #aggregated presence
            c('site','block','plot','plant_code','pres'))
divp=merge(divp,pt,by=c('site','block','plot','plant_code'))                    #merge with divp
vt=setNames(with(total,                                                         #aggregated variance
                 aggregate(wilmer_herb_total,list(site,block,plot,plant_code),var)),
            c('site','block','plot','plant_code','vari'))
divp=merge(divp,vt,by=c('site','block','plot','plant_code'))                    #merge with divp

write.csv(total,'total.csv',row.names=F)                                        #save total and divp files
write.csv(divp,'divp.csv',row.names=F)