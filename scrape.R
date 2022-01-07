#This file is for scraping the data 

rm(list=ls())
library(tidyverse)
library(rvest)
library(stringr)
library('xml2')
library(httr)  
library(rvest) 
library(tidyr) 
library(reshape2)
library("plyr")

getLinks = function(){
  subunits <- read_html("https://katalogus.hasznaltauto.hu/abarth")
  CList = subunits %>%
    html_nodes("ul")%>%
    html_text()
  
  CList[2]
  
  temp =  str_replace_all(CList[2], "[\r\n\t]" , " ")
  result <- strsplit(temp,"[[:space:]]")
  result = unlist(result)
  
  tempList = list()
  index = 1
  flag = 0
  for (i in 3:length(result)) {
    if(flag == 0) { 
      if(nchar(result[i]) > 1 & nchar(result[i+1]) > 1){
        tempList[index] = paste0(result[i],"_",result[i+1])
        index = index + 1
        flag = 1
      }else if(nchar(result[i]) > 1 ){
        tempList[index] = result[i]
        index = index + 1
      }
    }else{
      flag = 0
    }
  }
  
  tempList = unlist(tempList)
  tempList = tolower(tempList)
  # tempList = paste0("https://katalogus.hasznaltauto.hu/",tempList)
  tempList = data.frame(Name = tempList,
                        CLink = paste0("https://katalogus.hasznaltauto.hu/",tempList))
  tempList
}
getCompanyCars = function(link,company){
  url = link
  print(url)
  page <- read_html(url) #just read the html once
  web <- page %>%html_nodes("a") %>%
    html_attr("href")
  web
  web = unlist(web)
  
  pagelist = web[grepl(paste0(company,"/page"), web)]
  pagelist = unique(pagelist)  
  pagelist = pagelist[2:length(pagelist)]
  
  list1 = web[grepl(url, web)]
  list1 = unique(list1)
  
  pagelist = na.omit(pagelist)
  
  if(length(pagelist)>0){
    pagelist = paste0("https://katalogus.hasznaltauto.hu",pagelist)
    for(p in 1:length(pagelist)){
      page <- read_html(pagelist[p]) #just read the html once
      web <- page %>%html_nodes("a") %>%
        html_attr("href")
      web
      web = unlist(web)
      list2 = web[grepl(url, web)]
      list2 = unique(list2)
      list1 = c(list1,list2)
    }
  }
  
  list1 = unique(list1)
  list1
}
getAllCompaniesCars = function(tempLinks){
  Data = data.frame()
  
  for (i in 1:nrow(tempLinks)) {
    
    link = Links$CLink[i]
    company = Links$Name[i]
    
    ListofCarsLink = getCompanyCars(link,company)
    ListCompany = ListofCarsLink
    ListCompany[1:length(ListofCarsLink)] = company
    ListCLink = ListofCarsLink
    ListCLink[1:length(ListofCarsLink)] = link
    df = data.frame(Company = ListCompany,
                    CLink = ListCLink,
                    CarLink = ListofCarsLink )
    df = unique(df)
    Data = rbind(Data,df)
  }
  Data = unique(Data)
  Data
}
getCarsTablesData = function(Data){
  MainTableDF  = data.frame()
  for (d in 1:nrow(Data)) {
    u = Data$CarLink[d]
    
    company = Data$Company[d]
    cLink = Data$CLink[d]
    carLink = Data$CarLink[d]
    
    Read = read_html(u)
    tables_list1 = Read %>%
      html_nodes("table")%>%
      html_table(fill= TRUE)
    
    tableDF = data.frame()
    for (t in 1:length(tables_list1)) {
      tt = tables_list1[[t]]
      tableDF1 = data.frame(Col = tt$X1,
                            Row = tt$X2)
      tableDF = rbind(tableDF,tableDF1)
      
    }
    
    datadf = dcast(tableDF, Row ~ Col)
    datadf = datadf[1,c(1:nrow(tableDF))]
    names(datadf) = tableDF$Col
    datadf[1,] = tableDF$Row
    datadf$Company = company
    datadf$CompanyLink = cLink
    datadf$CompanyCarLink = carLink
    datadf$CarModel = gsub(paste0(cLink,"/"),"", u)
    MainTableDF = rbind.fill(MainTableDF, datadf)
  }
  MainTableDF = unique(MainTableDF)
  MainTableDF
}

Links = getLinks()

Data = getAllCompaniesCars(Links) 

MainData = getCarsTablesData(Data[1:500,]) 

#getwd()
#setwd("C:\\Users\\Rauhan Nazir\\Desktop\\Web scraping Mishi Feedback")
#path <- getwd()

#saveRDS(MainData, paste0(path, "/MainData.rds")) 
#write.csv(MainData,file = "MainData.csv")

