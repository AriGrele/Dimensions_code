mpath='./models'
if(!(mpath%in%list.dirs())){dir.create(mpath)}                        #create model output folder, if needed

divp$floodn[divp$flood=='flood']=1                                              #flood and clone proxy variables
divp$floodn[divp$flood=='no']=0
divp$clonen[divp$c_i=='ind']=1
divp$clonen[divp$c_i=='clone']=0
divp$divint=divp$pdiv*divp$floodn
divp$cloneint=divp$clonen*divp$floodn

                                                                                #subset and scale data for each site
data1=as.data.frame(scale(divp[divp$site=='Costa Rica',][,c('pdiv','clonen','floodn','herbivory','a_div','divint','cloneint')]))
data2=as.data.frame(scale(divp[divp$site=='Ecuador',][,c('pdiv','clonen','floodn','herbivory','a_div','divint','cloneint')]))
data3=as.data.frame(scale(divp[divp$site=='peru',][,c('pdiv','clonen','floodn','herbivory','a_div','divint','cloneint')]))
data4=as.data.frame(scale(divp[divp$site=='mogi',][,c('pdiv','clonen','herbivory','a_div','divint','cloneint')]))
data5=as.data.frame(scale(divp[divp$site=='Brazil',][,c('pdiv','clonen','herbivory','a_div','divint','cloneint')]))

set.seed(1498)                                                                  #seed chosen with runif
                                                                                #define models to run
model0='herbivory~a_div+floodn+clonen+pdiv
a_div~clonen+pdiv+floodn'
model='herbivory~a_div+floodn+clonen+pdiv
a_div~clonen+pdiv'
model2='herbivory~a_div+clonen+pdiv
a_div~clonen+pdiv'
model5='herbivory~a_div+clone+div+flood
a_div~clone+div+flood
alive~flood+clone+div+herbivory'
model52='herbivory~a_div+clone+div+flood
a_div~clone+div
alive~flood+clone+div+herbivory'
model53='herbivory~a_div+clone+div
a_div~clone+div
alive~flood+clone+div+herbivory'
model71='herb~rich+clone+div+flood
rich~clone+div+flood
surv_next~clone+div+herb+flood'
model72='herb~rich+clone+div+flood
rich~clone+div
surv_next~clone+div+herb+flood'
model73='herb~rich+clone+div
rich~clone+div
surv_next~clone+div+herb'

crsem0=bsem(model0,data1,model_name='crsem0',model_dir=mpath)                                                       #run cr models
crsem=bsem(model,data1,model_name='crsem',model_dir=mpath)
mask=guess_mask(paste0(mpath,'/crsem.txt'))                                                   #define mask for cr, ec, pe
crsem2=bsem(model2,data1,model_name='crsem2',model_dir=mpath)
(crf=fits(crsem0,crsem,crsem2))                                                 #fits of models
ddic(crf)                                                                       #model comparison

ecsem0=bsem(model0,data2,model_name='ecsem0',model_dir=mpath)                                                       #run ec models
ecsem=bsem(model,data2,model_name='ecsem',model_dir=mpath)
ecsem2=bsem(model2,data2,model_name='ecsem2',model_dir=mpath)
(ecf=fits(ecsem0,ecsem,ecsem2))
ddic(ecf)

pesem0=bsem(model0,data3,model_name='pesem0',model_dir=mpath)                                                       #run pe models
pesem=bsem(model,data3,model_name='pesem',model_dir=mpath)
pesem2=bsem(model2,data3,model_name='pesem2',model_dir=mpath)
(pef=fits(pesem0,pesem,pesem2))
ddic(pef)

mgsemb=bsem(model2,data4,model_name='mgsem',model_dir=mpath)                                                       #mg model
mask2=guess_mask(paste0(mpath,'/mgsem.txt'))                                                  #define mask for mg, ui
uisemb=bsem(model2,data5,model_name='uisem',model_dir=mpath)                                                       #ui model
fits(mgsemb,uisemb)                                                             #model fits
                                                                                #subset and scale data for mortality bsems
som1=as.data.frame(scale(morttime[morttime$site=='Costa Rica',][,c('div','clone','herb','rich','surv_next','flood')]))
som2=as.data.frame(scale(plotresp[plotresp$site=='Ecuador',][,c('div','flood','clone','herbivory','a_div','alive')]))
som3=as.data.frame(scale(plotresp[plotresp$site=='peru',][,c('div','flood','clone','herbivory','a_div','alive')]))
som4=as.data.frame(scale(morttime[morttime$site=='mogi',][,c('div','clone','herb','rich','surv_next')]))
som5=as.data.frame(scale(morttime[morttime$site=='Brazil',][,c('div','clone','herb','rich','surv_next')]))

crsem20=bsem(model71,som1,model_name='crsem20',model_dir=mpath)                                                      #cr mort models
crsem21=bsem(model72,som1,model_name='crsem21',model_dir=mpath)
mask3=guess_mask(paste0(mpath,'/crsem21.txt'))                                                  #define cr mort mask
crsem22=bsem(model73,som1,model_name='crsem22',model_dir=mpath)
(crf2=fits(crsem20,crsem21,crsem22))
ddic(crf2)
with(crf2,tapply(ppp,list(names),mean))

ecsem20=bsem(model5,som2,model_name='ecsem20',model_dir=mpath)                                                       #ec mort models
ecsem21=bsem(model52,som2,model_name='ecsem21',model_dir=mpath)
mask35=guess_mask(paste0(mpath,'/ecsem21.txt'))                                                 #define ec, pe mask
ecsem22=bsem(model53,som2,model_name='ecsem22',model_dir=mpath)
(ecf2=fits(ecsem20,ecsem21,ecsem22))
ddic(ecf2)
with(ecf2,tapply(ppp,list(names),mean))

pesem20=bsem(model5,som3,model_name='pesem20',model_dir=mpath)                                                       #pe mort models
pesem21=bsem(model52,som3,model_name='pesem21',model_dir=mpath)
pesem22=bsem(model53,som3,model_name='pesem22',model_dir=mpath)
(pef2=fits(pesem20,pesem21,pesem22))
ddic(pef2)
with(pef2,tapply(ppp,list(names),mean))

mgsem20=bsem(model73,som4,model_name='mgsem2',model_dir=mpath)                                                      #mg mort models
mask4=guess_mask(paste0(mpath,'/mgsem2.txt'))                                                  #define mg, ui mort mask
uisem20=bsem(model73,som5,model_name='uisem2',model_dir=mpath)                                                      #ui mort models
(uimg=fits(mgsem20,uisem20))
with(uimg,tapply(ppp,list(names),mean))
                
#### interactions ####
modeli='herbivory~a_div+floodn+clonen+pdiv+divint+cloneint
a_div~clonen+pdiv+divint+cloneint
divint~pdiv+floodn
cloneint~clonen+floodn'
modeli2='herbivory~a_div+floodn+clonen+pdiv+divint
a_div~clonen+pdiv+divint
divint~pdiv+floodn'

crsemi=bsem(modeli,data1,model_name='crsemi',model_dir=mpath)
ecsemi=bsem(modeli,data2,model_name='ecsemi',model_dir=mpath)
pesemi=bsem(modeli2,data3,model_name='pesemi',model_dir=mpath)
maski=guess_mask(paste0(mpath,'/crsemi.txt'))
maski2=guess_mask(paste0(mpath,'/pesemi.txt'))  

