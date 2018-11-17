#setwd("/Users/darafilyaskina/Desktop/notifications")
setwd("/Users/ivanloktaev/Yandex.Disk.localized/Uni/4\ Курс/Data\ Science/ha1\ materials/hw1p5/notifications")
library(XML)
library(stringr)
huinia<-htmlTreeParse(unzip("notification__Adygeja_Resp_inc_20110101_000000_20110201_000000_2.xml.zip"))
huinia$children$html[1]
