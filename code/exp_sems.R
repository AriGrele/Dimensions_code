                                                                                #define sem models
model1='herbivory~a_div+floodn+clonen+pdiv                                      
a_div~clonen+pdiv+floodn'
model2='herbivory~a_div+floodn+clonen+pdiv
a_div~clonen+pdiv'
model3='herbivory~a_div+clonen+pdiv
a_div~clonen+pdiv'
model4='herbivory~a_div+clone+pdiv+flood
a_div~clone+pdiv+flood
alive~flood+clone+pdiv+herbivory'
model5='herbivory~a_div+clone+pdiv+flood
a_div~clone+pdiv
alive~flood+clone+pdiv+herbivory'
model6='last_herb~a_div+clone+pdiv
a_div~clone+pdiv
alive~clone+pdiv+last_herb'
model11='herbivory~a_div+floodn
a_div~clonen+pdiv+floodn'
model12='herbivory~a_div+floodn
a_div~clonen+pdiv'
model13='herbivory~a_div
a_div~clonen+pdiv'
                                                                                #subset and scale data for each site
som1=scale(divp[divp$site=='Costa Rica',][,c('pdiv','clonen','floodn','herbivory','a_div','h_div','nlvs_per_ind')])
som2=scale(divp[divp$site=='Ecuador',][,c('pdiv','clonen','floodn','herbivory','a_div','h_div','nlvs_per_ind')])
som3=scale(divp[divp$site=='peru',][,c('pdiv','clonen','floodn','herbivory','a_div','h_div','nlvs_per_ind')])
som4=scale(divp[divp$site=='Brazil',][,c('pdiv','clonen','herbivory','a_div','h_div','nlvs_per_ind')])
som5=scale(divp[divp$site=='mogi',][,c('pdiv','clonen','herbivory','a_div','h_div','nlvs_per_ind')])
crsem=list();ecsem=list();pesem=list();mgsem=list();uisem=list()                #empty output lists
for(M in 1:3){                                                                  #for each model and site, run sem
  crsem[[M]]=sem(c(model1,model2,model3)[M],som1)
  ecsem[[M]]=sem(c(model1,model2,model3)[M],som2)
  pesem[[M]]=sem(c(model1,model2,model3)[M],som3)}
mgsem=sem(model3,som5)
uisem=sem(model3,som4)

lavaanPlot::lavaanPlot(model=mgsem,coefs=T)                                     #output plot
                                                                                #subset and scale data for each site for mortality models
som1=scale(plotresp[plotresp$site=='Costa Rica',][,c('div','flood','clone','herbivory','a_div','alive')])
som2=scale(plotresp[plotresp$site=='Ecuador',][,c('div','flood','clone','herbivory','a_div','alive')])
som3=scale(plotresp[plotresp$site=='peru',][,c('div','flood','clone','herbivory','a_div','alive')])
som4=scale(mortcurve[mortcurve$site=='mogi',][,c('div','flood','clone','herbivory','a_div','alive')])
som5=scale(plotresp[plotresp$site=='Brazil',][,c('div','flood','clone','herbivory','a_div','alive')])
crsem2=list();ecsem2=list();pesem2=list();mgsem2=list();uisem2=list()           #empty output lists
for(M in 1:3){                                                                  #for each model and site, run sem
  crsem2[[M]]=sem(c(model4,model5,model5)[M],som1)
  ecsem2[[M]]=sem(c(model4,model5,model5)[M],som2)
  pesem2[[M]]=sem(c(model4,model5,model5)[M],som3)}

source('"for mort dive data".R')                                                #load in second data frame and format
                                                                                #mortality model with corrected var names
model4='herb~rich+clone+div                                                     
rich~clone+div
surv_next~clone+div+herb'
                                                                                #ui and mg data and models
som4=scale(morttime[morttime$site=='mogi',][,c('div','clone','herb','rich','surv_next')])
som5=scale(morttime[morttime$site=='Brazil',][,c('div','clone','herb','rich','surv_next')])
mgsem2=sem(model4,som4)
uisem2=sem(model4,som5)

