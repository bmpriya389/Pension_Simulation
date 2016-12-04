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

library(xlsx)

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

source("functions.R")

print(paste0("Prep Start Time ",Sys.Date()," ",Sys.time()))

min_age<-25

# Cost method vector
c_m<-c('EAN','PUC')

# entry age vector
e_a<-c(25,30,35)

# retirement age vector
r_a<-seq(55,65,5)

# inflation rate
inflation<-0.02

# salary growth ratevector
act_sgr<-sgr<-seq(0.03,0.06,0.01)+inflation

# discount rate vector
i_r<-seq(0.03,0.09,0.01)+inflation

# current age vector
ca<-seq(25,85,10)

# Mortality table numbers
mort_num<-c(2,4,6) 

# starting salary
sal<-1

# Benefit factor vector
bf_p<-seq(0.02,0.06,0.01)

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
amortization<-30

# Max age limit of the model
max_age<-100

# A mortality table code conversion function.
mort_name <- function(mort_number){
  m<-switch(mort_number,'2'="RP2014_Employee_total",'4'="RP2000_Employee_total",'6'="RP2010_Employee_total")
  return(m)
}

# grid of every possible combination of current age, entry age, retirement age, discount rate,
# salary growth rate, cola, afc, benefit factor, cost method, mortality tables

f<-expand.grid(ca, e_a , r_a, i_r, sgr, cola_r, afc_p, bf_p, c_m, mort_num)

# filter out entires where current age is less than entry age
f<-filter(f,Var1>=Var2)

names(f)<-c('Current Age' ,'Entry Age','Retirement Age' ,'Discount Rate' ,
            'Salary Growth Rate','COLA' ,'AFC' ,
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


# Start calculations for each entry in a

print(paste0("Start Time ",Sys.Date()," ",Sys.time()))

for(j in seq(nrow(a))){
  ind<-a$id[j]
  nc_list[[ind]]<-get_NC(a$'Entry Age'[j],a$'Retirement Age'[j],a$'Discount Rate'[j],a$'Salary Growth Rate'[j],
                         a$'Salary Growth Rate'[j],a$'COLA'[j],a$'AFC'[j],a$'Benefit Factor'[j],a$'Cost Method'[j],
                         a$'Mortality Table'[j],vesting)[subset(ca,ca>=a$'Entry Age'[j])-a$'Entry Age'[j]+1] # nc value for random actuarial inputs
  act_sal_list[[ind]]<-get_act_sal(a$'Entry Age'[j],a$'Retirement Age'[j],a$'Salary Growth Rate'[j])[subset(ca,ca>=a$'Entry Age'[j])-a$'Entry Age'[j]+1] # actual salary for random actuarial inputs
  aal_list[[ind]]<-get_AAL(a$'Entry Age'[j],a$'Retirement Age'[j],a$'Discount Rate'[j],a$'Salary Growth Rate'[j],
                           a$'Salary Growth Rate'[j],a$'COLA'[j],a$'AFC'[j],a$'Benefit Factor'[j],a$'Cost Method'[j],
                           a$'Mortality Table'[j],vesting)[subset(ca,ca>=a$'Entry Age'[j])-a$'Entry Age'[j]+1] # AAL for random actuarial inputs
  adc_list[[ind]]<-get_ARC(rep(1,(100-a$'Entry Age'[j])+1),a$'Entry Age'[j],a$'Retirement Age'[j],
                           median_p,a$'Discount Rate'[j],a$'Salary Growth Rate'[j],a$'Discount Rate'[j],
                           a$'Salary Growth Rate'[j],a$'Salary Growth Rate'[j], a$'Salary Growth Rate'[j]-inflation,
                           a$'COLA'[j],a$'AFC'[j],a$'Benefit Factor'[j],a$'Cost Method'[j],a$'Mortality Table'[j],
                           vesting,amortization)[subset(ca,ca>=a$'Entry Age'[j])-a$'Entry Age'[j]+1] # ARC for random actuarial inputs
  rpvfbx_list[[ind]]<-get_rPVFBx_after_r(a$'Entry Age'[j],a$'Retirement Age'[j],a$'Discount Rate'[j],
                                         a$'Salary Growth Rate'[j],a$'Salary Growth Rate'[j],
                                         a$'COLA'[j],a$'AFC'[j],a$'Benefit Factor'[j],a$'Mortality Table'[j])[2] # Expected value of annuity for random actuarial inputs
  ret_annuity_list[[ind]]<-get_retirement_annuity(a$'Entry Age'[j],a$'Retirement Age'[j],a$'Salary Growth Rate'[j],
                                                  a$'Salary Growth Rate'[j],a$'AFC'[j],a$'Benefit Factor'[j]) # Retirement Annuity for random actuarial inputs
  repl_rate_list[[ind]]<-get_replacement_rate(a$'Entry Age'[j],a$'Retirement Age'[j],a$'Salary Growth Rate'[j],
                                              a$'Salary Growth Rate'[j],a$'AFC'[j],a$'Benefit Factor'[j]) # Replacement rate for random actuarial inputs
}


f<-filter(f, f$'id' %in% a$'id')

# unlist all outputs
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
nc<-numeric()
sal<-numeric()
aal<-numeric()
adc<-numeric()
rpvfbx<-numeric()
ret_annuity<-numeric()
repl_rate<-numeric()
run<-numeric()

# fetch appropriate normal cost, salary, aal, adc, rpvfbx, retirement annuity and replacement rate
for(i in seq(1,nrow(f))){
  rpvfbx<-c(rpvfbx,rpvfbx_list[f[i,'id']])
  ret_annuity<-c(ret_annuity,ret_annuity_list[f[i,'id']])
  repl_rate<-c(repl_rate,repl_rate_list[f[i,'id']])
  run<-c(run,i)
}

f$`Mortality Table`<-sapply(f$`Mortality Table`, mort_name)
f<-cbind(data.frame('Run' = run, 'Normal Cost' = nc_list, 'NC/Payroll' = nc_list/act_sal_list , 'Salary' = act_sal_list,'AAL' = aal_list, 'ADC' = adc_list, 'RPVFBX' = rpvfbx, 'Retirement Annuity' = ret_annuity, 'Replacement Rate' = repl_rate),select(f,-c(id,index,IndexId)))
f$NC.Payroll[is.nan(f$NC.Payroll)]<-0

runWorkBook<- createWorkbook();

for(i in ca){
  a<-createSheet(runWorkBook,paste("Current Age",i))
  addDataFrame(subset(f,f$`Current Age`==i),col.names=TRUE,sheet = a)
}

save(f,file='Simulate_run_age.RData')  # saving the Rdata file
saveWorkbook(runWorkBook,"run_Age.xlsx") # Saving Workbook

print(paste0("END Time ",Sys.Date()," ",Sys.time()))

