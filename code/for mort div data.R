evenness=function(x){return(diversity(x)/log(specnumber(x)))}
evenness2=function(x){return(max(x,na.rm=T)/sum(x,na.rm=T))}
framesum=function(x){o=list()                                                   #sum function across rows of dataframe
for(i in 1:length(x[,1])){o[i]=sum(x[i,1:length(x)],na.rm=T)}
o[o==0]=NA;return(as.numeric(o))}

d=read.csv('./files/data2.csv',                                                 #load data
           stringsAsFactors = FALSE,na.strings=c(""," ","NA"))
d=subset(d,d$experiment_observation=='experiment')                              #subset to just experimental data

d$experiment_final_intermediate[is.na(d$experiment_final_intermediate)]='final'
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
d$flood[d$site=='Ecuador' & d$div==1]='no'

d2=subset(d,d$site=='mogi'|d$site=='Brazil'|                                    #subset to crc, mg, ui
            d$site=="Costa Rica")[c(1,4,5,3,2,10,12,(12:25)[c(F,T)])]
d2$block[d2$site=='Costa Rica']=1                                               #fill empty block col in cr

aout=setNames(as.data.frame(matrix(ncol=7)),                                    #define output data.frame
              c('site','block','plot','plant_code','date','herb','rich'))
for(q in unique(d2$site)){                                                      #terrible nested loop to extract relevant data for each sampling period in cr, mg, ui
  for(w in unique(d2$block[d2$site==q])){
    for(e in unique(d2$plot[d2$site==q&d2$block==w])){
      for(r in unique(d2$experiment_final_intermediate[d2$site==q&d2$block==w&d2$plot==e])){
        for(t in unique(d2$plant_code[d2$site==q&d2$block==w&d2$plot==e&d2$experiment_final_intermediate==r])){
          temp=d2[d2$site==q&d2$block==w&d2$plot==e&d2$experiment_final_intermediate==r&d2$plant_code==t,]
          u=c()
          for(i in 8:13){
            u=c(u,unique(temp[,i]))}
          aout=rbind(aout,data.frame('site'=q,'block'=w,'plot'=e,'plant_code'=t,'date'=r,'herb'=mean(temp$wilmer_herb_total,na.rm=T),'rich'=length(na.omit(u))))}}}}}

mt=read.csv('./files/mort_time.csv')                                            #load in mort_time data
morttime=merge(mt,aout[-6],                                                     #merge with extracted data
               by=c('site','block','plot','plant_code','date'),all=T)

morttime$rich[is.na(morttime$rich)]=0                                           #NAs to 0
morttime$clone[morttime$clone=='clone']=0                                       #clone and flood to proxy variables
morttime$clone[morttime$clone=='ind']=1
morttime$flood[morttime$flood=='no']=0
morttime$flood[morttime$flood=='flood']=1
morttime$clone=as.numeric(morttime$clone)
morttime$flood=as.numeric(morttime$flood)
morttime=subset(morttime,!is.na(herb)&!is.na(surv_next))                        #subset to rows with herbivory and survival measures
