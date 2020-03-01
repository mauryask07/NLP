
install.packages("xml2")
install.packages("rvest")
install.packages("stringr")
install.packages("jsonlite")


library(xml2)
library(rvest)
library(stringr)

url<- "https://www.lalpathlabs.com/doctors.aspx"
webpage<- read_html(url)

#scrape title of the product
title_html <- html_nodes(webpage, "ul.dr-menu-option")
  title <- html_text(title_html)
head(title)

#
# remove all space and new lines
a<-str_replace_all(title, "[\r\n]","")[1]

#str_extract_all(a, "\\b[A]\\w+")[[1]]

str(a)

class(a)
as.matrix(a)
a[[1]]

library(tm)

library(NLP)
#write.csv(a,"new.csv")
countWhite

countSpaces <- function(x){
  counter <- 0
  coll <- numeric()
  vec <- strsplit(x," ")[[1]]
  for(i in 1:length(vec)){
    if (vec[i]==""){
      counter <- counter+1
    }
    else{
      if (counter!=0) coll <- c(coll,counter)
      counter <- 1
    }
  }
  coll
}
countSpaces(a)

length(a)
nchar(a)
library(stringi)
library(stringi)
stri_stats_latex(a)

b<-head(strsplit(a, split = "\  "), 10)

str_replace_all(b, "\r\n","")[1]
paste(b,collapse="")


str_extract_all(a, "\\b[A]\\w+")[[1]]

temp <- stringr::str_replace_all(a,"[\\s]+", " ")



gsub("^((\\w+\\W+){230}\\w+).*$","\\1",a)
gsub("^((\\w+\\W+){299}).*$","\\1",a)


head(unlist(strsplit(a, split = "\\s+")),200)



a
length(a)
a[[1]]


x<- list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
               123, NULL, 456)


df <- data.frame(matrix(unlist(b), nrow=length(b), byrow=T))
do.call(rbind.data.frame, df)

sub("^x...+;", "", df)     ## [1] "@34skirt"

q<-data.frame(Reduce(rbind, b))
unique(q)
