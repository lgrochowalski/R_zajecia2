library(dplyr)
library(stringr)
library(gtools)

library(xml2)
library(rvest)
#### lab2 2:46

wektorLinkow<-c()
for(i in 1:5){
  print(i)
  newUrl<- paste0("https://www.otomoto.pl/osobowe/toyota/prius/?search%5Border%5D=created_at%3Adesc&page=",i)
  page<-read_html(newUrl)
  result<-page%>%html_nodes(xpath='  /html/body/div[4]/div[2]/section/div[2]/div[1]/div/div[1]/div[6]/article[*]/div[2]/div[1]/div[1]/h2/a')
  wektorLinkow<-c(wektorLinkow,xml_attr(result,"href"))
}
wektorLinkowU<-wektorLinkow%>%unique()


zrobWierszRvest<-function(w,wektorLinkow){
  newUrl<-wektorLinkow[w]
  page<-read_html(newUrl)

  cena <- html_node(page,".offer-price") %>% html_attr("data-price")
  
  offer_labels <- html_nodes(page,".offer-params__label") %>% html_text()
  offer_values <- html_nodes(page,".offer-params__value") %>% html_text() %>% str_trim()
  
  df1<- data.frame( matrix(offer_values,nrow=1,ncol=length(offer_values)))
  names(df1)<-offer_labels
  df1<-cbind(cena,df1)
  df1
}


samochody<-NULL
for(w in 1: length(wektorLinkowU)){
  skip<-FALSE
  tryCatch(
    df1<-zrobWierszRvest(w,wektorLinkowU),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(samochody)){
    samochody<-df1
  }else{
    samochody<-smartbind(samochody,df1)
  }
}

View(samochody)