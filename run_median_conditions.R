#########################################################################################################
#########################################################################################################
#                                                                                                       #
#                                          DESCRIPTION                                                  #
#   run_new.R  creates a dataframe and xlsx sheet of ARC, Normal Cost, AAL, Expected value of annuity,  #
#   Retirement Annuity, Replacement Rate for an random combination of  Entry Age, Retirement Age,       #
#   Current Age, Funding Level, Discount Rate, Salary growth rate, Payroll, Growth Rate(%), COLA, AFC,  #
#   Benefit Factor, Cost Method, Mortality Table,Amortization Period in the current working directory   #                                                                 #
#                                                                                                       #
#########################################################################################################
#########################################################################################################

############################# R COMMAND ##################################
# rm() removes a variable from the workspace. A list of all the variables 
# in the workspace is passed as a parameter to rm to clear all variables 
# in the workspace to ensure that the model uses only the updated values
# sent by the user for computations
##########################################################################

rm(list=ls())

############################ LIBRARY IMPORT ##############################
# xlsx Provides R functions to read/write/format Excel 2007 and Excel 97/2000/XP/2003 file formats
# REFERENCE: https://cran.r-project.org/web/packages/xlsx/xlsx.pdf
##########################################################################

library(openxlsx)

############################ LIBRARY IMPORT ##############################
# dplyr is a package for data manipulation, written and maintained by Hadley Wickham.
# It provides some great, easy-to-use functions that are very handy when performing 
# exploratory data analysis and manipulation.
##########################################################################

library(dplyr)

############################# R COMMAND ##################################
# source() notifies R that the variables and functions present in the
# file passed as an argument to it should be loaded to the r environment   
# and is avaiable for use in this file
##########################################################################
# we source our model logic present in functions.R


library(BayesBridge)
source("functions.R")

print(paste0("Prep Start Time ",Sys.Date()," ",Sys.time()))

min_age<-30
max_age<-100
# Cost method vector
c_m<-c('EAN','PUC')

# entry age vector
e_a<-30

# retirement age vector
r_a<-65

# inflation rate
inflation<-0.02

# salary growth ratevector
act_sgr<-sgr<-0.0568

# discount rate vector
i_r<-0.08

# current age vector
ca<<-seq(30,85,1)

# Mortality table numbers
mort_num<-c(2,4,6)

# starting salary
sal<-1

# Benefit factor vector
bf_p<-0.02

# afc vector
afc_p<-c(1,5,10)

# cola vector
cola_r<-seq(0,0.04,0.01)

# vesting period
vesting<-5

# run number
run<-0

# funding level
median_p<-0.854

# amortization period
amortization<-29

# Max age limit of the model
max_age<-100

# A mortality table code conversion function.
mort_name <- function(mort_number){
  if(mort_number=='2')
    m <-"RP2014_Employee_total"
  else if(mort_number=='4')
    m <-"RP2000_Employee_total"
  else if(mort_number=='6')
    m <-"RP2010_Employee_total"
  else
    m<-""
  return(m)
}

# person per each grp for active employees
perAgeGroup <- 3

# median age
medianAge <- 45

# grid of every possible combination of current age, entry age, retirement age, discount rate,
# salary growth rate, cola, afc, benefit factor, cost method, mortality tables 
f<-expand.grid(ca, e_a , r_a, i_r, sgr, median_p,cola_r, afc_p, bf_p, c_m, mort_num)

# filter out entires where current age is less than entry age
f<-filter(f,Var1>=Var2)

names(f)<-c('Current Age' ,'Entry Age','Retirement Age' ,'Discount Rate' ,
            'Salary Growth Rate','Funding Ratio','COLA' ,'AFC' ,
            'Benefit Factor','Cost Method' ,'Mortality Table')

f<-mutate(f, 'id' = paste(`Entry Age`,`Retirement Age` ,`Discount Rate` ,
                          `Salary Growth Rate` ,`COLA` ,`AFC` ,
                          `Benefit Factor`,`Cost Method` ,`Mortality Table` ,sep=''),
          'index' = `Current Age` - `Entry Age` + 1,
          'IndexId'=paste(id,row_number(),sep=''))

# find unique combinations on entry age, retirement age, discount rate, salary growth rate,
# cola, afc, benefit factor, cost method, mortality table, id, index, indexid
a<-f[!duplicated(f[,c('id')]),]

# create list for storing outputs nc,actual salary, aal, adc, rpvfbx, retirement annuity and replacement rate
nc_list<-list()
act_sal_list<-list()
aal_list<-list()
adc_list<-list()
rpvfbx_list<-list()
ret_annuity_list<-list()
repl_rate_list<-list()


print(paste0("Prep End Time ",Sys.Date()," ",Sys.time()))

print(paste0("Data generation START Time ",Sys.Date()," ",Sys.time()))

# Start calculations for each entry in a
for(j in seq(nrow(a))){
  pop<-generate_pop(a$'Entry Age'[j],a$'Retirement Age'[j],'Uniform',(a$'Retirement Age'[j]-a$'Entry Age'[j]+1)*perAgeGroup,medianAge)
  pop_ret<-population_retirees(a$'Entry Age'[j],a$'Retirement Age'[j],a$'Mortality Table'[j],pop)
  pop<- table(c(pop,pop_ret))

  ind<-a$id[j]
  nc_list[[ind]]<-(pop[1:(a$'Retirement Age'[j]-a$'Entry Age'[j]+1)]*get_NC(a$'Entry Age'[j],a$'Retirement Age'[j],a$'Discount Rate'[j],a$'Salary Growth Rate'[j],
                                            a$'Salary Growth Rate'[j],a$'COLA'[j],a$'AFC'[j],a$'Benefit Factor'[j],a$'Cost Method'[j],
                                            a$'Mortality Table'[j],vesting))[subset(ca,ca>=a$'Entry Age'[j])-a$'Entry Age'[j]+1] # nc value for random actuarial inputs
  act_sal_list[[ind]]<-(pop[1:(a$'Retirement Age'[j]-a$'Entry Age'[j]+1)]*get_act_sal(a$'Entry Age'[j],a$'Retirement Age'[j],a$'Salary Growth Rate'[j]))[subset(ca,ca>=a$'Entry Age'[j])-a$'Entry Age'[j]+1] # actual salary for random actuarial inputs
  aal_list[[ind]]<-(pop*get_AAL(a$'Entry Age'[j],a$'Retirement Age'[j],a$'Discount Rate'[j],a$'Salary Growth Rate'[j],
                               a$'Salary Growth Rate'[j],a$'COLA'[j],a$'AFC'[j],a$'Benefit Factor'[j],a$'Cost Method'[j],
                               a$'Mortality Table'[j],vesting))[subset(ca,ca>=a$'Entry Age'[j])-a$'Entry Age'[j]+1] # AAL for random actuarial inputs
  adc_list[[ind]]<-get_ARC(pop,a$'Entry Age'[j],a$'Retirement Age'[j],median_p,a$'Discount Rate'[j],a$'Salary Growth Rate'[j],a$'Discount Rate'[j],
                           a$'Salary Growth Rate'[j],a$'Salary Growth Rate'[j], a$'Salary Growth Rate'[j]-inflation,
                           a$'COLA'[j],a$'AFC'[j],a$'Benefit Factor'[j],a$'Cost Method'[j],a$'Mortality Table'[j],
                           vesting,amortization)[subset(ca,ca>=a$'Entry Age'[j])-a$'Entry Age'[j]+1] # ARC for random actuarial inputs
  rpvfbx_list[[ind]]<-(pop*c(get_rPVFBx(a$'Entry Age'[j],a$'Retirement Age'[j],a$'Discount Rate'[j],a$'Salary Growth Rate'[j],
                                         a$'Salary Growth Rate'[j],a$'COLA'[j],a$'AFC'[j],a$'Benefit Factor'[j],a$'Mortality Table'[j]),
                      get_rPVFBx_after_r(a$'Entry Age'[j],a$'Retirement Age'[j],a$'Discount Rate'[j],a$'Salary Growth Rate'[j],
                                         a$'Salary Growth Rate'[j],a$'COLA'[j],a$'AFC'[j],a$'Benefit Factor'[j],a$'Mortality Table'[j])))[subset(ca,ca>=a$'Entry Age'[j])-a$'Entry Age'[j]+1]
                                          # Expected value of annuity for random actuarial inputs
  ret_annuity_list[[ind]]<-get_retirement_annuity(a$'Entry Age'[j],a$'Retirement Age'[j],a$'Salary Growth Rate'[j],
                                                  a$'Salary Growth Rate'[j],a$'AFC'[j],a$'Benefit Factor'[j]) # Retirement Annuity for random actuarial inputs
  repl_rate_list[[ind]]<-get_replacement_rate(a$'Entry Age'[j],a$'Retirement Age'[j],a$'Salary Growth Rate'[j],
                                              a$'Salary Growth Rate'[j],a$'AFC'[j],a$'Benefit Factor'[j]) # Replacement rate for random actuarial inputs
  
}    

f<-filter(f, f$'id' %in% a$'id')

#unlist all outputs
nc_list<-unlist(nc_list)
nc_list[is.na(nc_list)]<-0
act_sal_list<-unlist(act_sal_list)
act_sal_list[is.na(act_sal_list)]<-0
aal_list<-unlist(aal_list)
adc_list<-unlist(adc_list)
rpvfbx_list<-unlist(rpvfbx_list)
repl_rate_list<-unlist(repl_rate_list)
ret_annuity_list<-unlist(ret_annuity_list)

#create vectors
rpvfbx<-numeric()
ret_annuity<-numeric()
repl_rate<-numeric()
run<-numeric()

save(nc_list,file='nclist.RData')  # saving the Rdata file
save(act_sal_list,file='actual_sal_list.RData')  # saving the Rdata file
save(aal_list,file='aal_list.RData')  # saving the Rdata file
save(adc_list,file='adc_list.RData')  # saving the Rdata file
save(rpvfbx_list,file='rpvfbx_list.RData')  # saving the Rdata file
save(repl_rate_list,file='repl_rate_list.RData')  # saving the Rdata file
save(ret_annuity_list,file='ret_annuity_list.RData')  # saving the Rdata file

assets<-median_p*aal_list
c<-data.frame('Normal Cost' = nc_list, 'NC/Payroll' = nc_list/act_sal_list , 'Salary' = act_sal_list,'AAL' = aal_list, 'ADC' = adc_list,'rPVFBx'= rpvfbx_list, 'Assets' = assets)

# fetch appropriate normal cost, salary, aal, adc, rpvfbx, retirement annuity and replacement rate
run<- seq(1,nrow(f))

for(i in f['id']){
  ret_annuity<-c(ret_annuity,ret_annuity_list[i])
  repl_rate<-c(repl_rate,repl_rate_list[i])
}

f$`Mortality Table`<-sapply(unlist(f$'Mortality Table'),mort_name)
f<-cbind(data.frame('Run' = run),c,data.frame('Retirement Annuity factor' = ret_annuity, 'Replacement Rate' = repl_rate),select(f,-c(id,index,IndexId)),row.names=NULL)
f$NC.Payroll[is.nan(f$NC.Payroll)]<-0 #replacing NaNs with zeros

save(f,file='Simulate_run_age.RData')  # saving the Rdata file

print(paste0("Data generation END Time ",Sys.Date()," ",Sys.time()))


