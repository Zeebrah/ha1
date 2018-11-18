
setwd("/Users/darafilyaskina/Desktop/notifications")
#setwd("/Users/ivanloktaev/Yandex.Disk.localized/Uni/4\ Курс/Data\ Science/ha1\ materials/hw1p5/notifications")
library(XML)
library(stringr)
xml_file<-xmlParse(unzip("notification__Adygeja_Resp_inc_20110101_000000_20110201_000000_2.xml.zip"))

root <- xmlRoot(xml_file)
names(root[[1]][[2]])

#create a dataframe of all nodes

x<-xmlToDataFrame(root, stringsAsFactors = FALSE)

## extract only those nodes that are needed
xmlChildren(root)
# <<<<<<< HEAD
# =======
library(data.table)
x<-as.data.table(x)
x[,placingWay.name:=lapply("placingWay",substr(as.character(x[1,5]),1,2))]
  
# >>>>>>> 3429b8f5e5961c72412cecb92c868f3ffd9fb702
