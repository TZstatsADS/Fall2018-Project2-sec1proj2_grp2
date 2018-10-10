
#setwd("../")

##Install and load needed libraries#########################################

packages.used=c("rgeos", "sp", "rgdal", "plotly",
                "leaflet", "htmlwidgets", "shiny",
                "ggplot2", "dplyr", "data.table","DT", "leaflet.extras")

packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1],packages.used))

if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)}

library(plotly)
library(rgeos)
library(sp)
library(rgdal)
library(leaflet)
library(htmlwidgets)
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(data.table)
library(leaflet.extras)

##Set Variables##############################################################

group1 = "<span style='color: #7f0000; font-size: 11pt'><strong>count</strong></span>"
group2 = "<span style='color: #7f0000; font-size: 11pt'><strong>FPD</strong></span>"
group3 = "<span style='color: #7f0000; font-size: 11pt'><strong>Percentage Cash Paying:</strong></span>"

color = list(color1 = c('#F2D7D5','#D98880', '#CD6155', '#C0392B', '#922B21','#641E16'),
             color2 = c('#e6f5ff','#abdcff', '#70c4ff', '#0087e6', '#005998','#00365d','#1B4F72'),
             color3 = c("#F7FCF5","#74C476", "#005A32"))

bin = list(bin1 = c(0,100,1000,10000,100000,1000000,10000000), bin2 = c(0,1,2,3,4,5,6,7))

label = list(label1 = c("<100","100-1000","1000~10,000","10,000~100,000","100,000~1,000,000","1,000,000~10,000,000"),
             label2 = c("0-1","2-3","3-4","4-5","5-6","6","7+"),
             label3 = c("<0.4","0.4~0.6",">0.6"))

title = list(t1 = "Pick Up Frequency", t2 = "Fare by Distance",t3  = "Percent Cash Payment")

vars <- c("Business Day" = 1,"Not Business Day" = 2)

##Load data##############################################################

load('../output/myShape1.RData')
subdat<-spTransform(myShape1, CRS("+init=epsg:4326"))
dynamicdata = fread("../data/pickupDropoff date_hour.csv", header = TRUE, stringsAsFactors=F)
dataa<-fread("../data/rawdata.csv",header = T)

load('../output/count_seperated.RData')
load('../output/FPD_seperated.RData')
rownames(count_result) = subdat@data$NTACode

subway1 = read.csv("../data/sub.csv", header = TRUE, stringsAsFactors = F)
subway2 = read.csv("../data/sub2.csv", header = TRUE, stringsAsFactors = F)
payper = read.csv("../data/Data_frame_of_summary.csv")

count_result1 <- as.data.frame(count_result[,,1])

sum <- apply(count_result1, 1, sum)
as.matrix(count_result1)
count_result1 <- count_result1/sum 
as.data.frame(count_result1)
count_result1[is.na(count_result1)] <- 0

fit <- kmeans(count_result1, 9)
aggregate(count_result1,by=list(fit$cluster),FUN=mean)
count_result1 <- data.frame(count_result1, fit$cluster)