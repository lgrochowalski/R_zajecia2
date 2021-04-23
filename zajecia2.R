library(dplyr)
library(stringr)
library(gtools)

library(xml2)
library(rvest)
#### lab2 2:46

wektorLinkow<-c()
for(i in 1:2){
  print(i)
  newUrl<- paste0("https://www.otodom.pl/sprzedaz/mieszkanie/?page=",i)
  page<-read_html(newUrl)
  result<-page%>%html_nodes(xpath='/html/body/div[3]/main/section[2]/div/div/div[1]/div/article[*]/div[1]/header/h3/a')
  wektorLinkow<-c(wektorLinkow,xml_attr(result,"href"))
}
wektorLinkowU<-wektorLinkow%>%unique()


zrobWierszRvest<-function(w,wektorLinkow){
  newUrl<-wektorLinkow[w]
  page<-read_html(newUrl)
  cena<-html_node(page,".css-srd1q3")%>%html_text()
  v<-page %>% xml_find_all('/html/body/div[1]/main/div/div[3]/div[1]/*/*')%>%html_attr("title")%>%na.omit()
  print(v)
  indexy<-seq(1,length(v),1)
  nazwyKolumn<-v[indexy%%2==1]
  wartosci<-v[indexy%%2==0]
  df1<- data.frame( matrix(wartosci,nrow=1,ncol=length(wartosci)))
  names(df1)<-nazwyKolumn
  df1<-cbind(cena,df1)
  # df1
}


mieszkania<-NULL
for(w in 1: length(wektorLinkowU)){
  skip<-FALSE
  tryCatch(
    df1<-zrobWierszRvest(w,wektorLinkowU),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(mieszkania)){
    mieszkania<-df1
  }else{
    mieszkania<-smartbind(mieszkania,df1)
    # View(mieszkania)
  }
}
