options(scipen=999)                                                             #increase decimal readout
Sys.setenv(OMP_NUM_THREADS="12")                                                #increase number of threads for JAGS 
 
source('./code/exp_packages.R')                                                 #load packages
source('./code/exp_gen.R')                                                      #collate, clean and format data
source('./code/format_data.R')                                                  #specific dataframes for model input

source('./code/mort curves.R')                                                  #mortality data

source('./code/exp_bayes.R')                                                    #HBMs
source('./code/BSEMs.R')                                                        #BSEMs

source('./code/rain code.R')                                                    #precipitation data
source('./code/exp_fig.R')                                                      #create figures
