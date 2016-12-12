library(openxlsx)

load("Simulate_run_age.Rdata")

ca<<-seq(25,85,1)

#function to write xlsx
writeToxlsx<-function(data,wb,name){
  k<-0
  for(i in ca){
    k <- k+1
    addWorksheet(wb,sheetName = paste("Current Age",i))
    d<-subset(data,data$`Current Age`==i)
    writeData(wb,k,x = d)
  }
  saveWorkbook(wb,file=paste(name,'.xlsx'),overwrite = TRUE) # Saving Workbook
  
}


print(paste0("Subsetting and wrtin xlsx START Time ",Sys.Date()," ",Sys.time()))

EANrp2014<- subset(f,(f$`Mortality Table`=="RP2014_Employee_total" & f$`Cost Method`=='EAN'));
EANrp2010<- subset(f,(f$`Mortality Table`=="RP2010_Employee_total" & f$`Cost Method`=='EAN'));
EANrp2000<- subset(f,(f$`Mortality Table`=="RP2000_Employee_total" & f$`Cost Method`=='EAN'));
PUCrp2014<- subset(f,(f$`Mortality Table`=="RP2014_Employee_total" & f$`Cost Method`=='PUC'));
PUCrp2010<- subset(f,(f$`Mortality Table`=="RP2010_Employee_total" & f$`Cost Method`=='PUC'));
PUCrp2000<- subset(f,(f$`Mortality Table`=="RP2000_Employee_total" & f$`Cost Method`=='PUC'));

EANrp2014wb<- createWorkbook();
EANrp2010wb<- createWorkbook();
EANrp2000wb<- createWorkbook();
PUCrp2014wb<- createWorkbook();
PUCrp2010wb<- createWorkbook();
PUCrp2000wb<- createWorkbook();

writeToxlsx(EANrp2000,EANrp2000wb,'EANrp2000wb')
writeToxlsx(EANrp2010,EANrp2010wb,'EANrp2010wb')
writeToxlsx(EANrp2014,EANrp2014wb,'EANrp2014wb')
writeToxlsx(PUCrp2000,PUCrp2000wb,'PUCrp2000wb')
writeToxlsx(PUCrp2010,PUCrp2010wb,'PUCrp2010wb')
writeToxlsx(PUCrp2014,PUCrp2014wb,'PUCrp2014wb')

print(paste0("Subsetting and wrtin xlsx END Time ",Sys.Date()," ",Sys.time()))

