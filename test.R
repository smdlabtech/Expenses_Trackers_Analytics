

input_data = "C:/Users/DASY/Downloads/web_devs/expenses_tracker_shiny/_data/18-07-2021 Expense_Data.csv"
df1 <- read.csv(input_data,header = T,sep = ',', stringsAsFactors=FALSE, fileEncoding="latin1")
colnames(df1)<-c("Designation","Costs","Date","Purchase")  #Renommer les Champs de la table de donnees ("Designation","Prix","Date","Achat")
df1$Date<-convertToDate(df1$Date)  
df1


## Sauvegarder des données (et lecture)
saveRDS(d, file = "df1.rds")
df2 <- readRDS("df1.rds")
df2
