library(KoNLP)
library(httr)
library(rvest)
library(readr)
library(xml2)
library(stringr)
library(dplyr)
library(tidytext)
library(lubridate)
library(RMariaDB)
library(DBI)
library(googledrive)
library(googlesheets4)

### 주가는 관련 뉴스와 많은 관계가 있습니다. 정확한 상관관계나 선후관계는 알 수 없지만, 많은 영향을 주고 받습니다. 기존에 주가를 가져오는 함수들은 많은 분들에 의해 작성됐고, 뉴스를 크롤링 해오는 것 또한 많은 분들에 의해 작성됐지만, 하나로 결합한 함수는 찾아보기 어려웠습니다. 그래서 직접 제작해봤습니다.

### 네이버 금융 안의 기사를 참고하면 주가와 관련된 뉴스만을 정렬해서 볼 수 있습니다. 원하는 시작일과 종료일을 지정하시고, 종목코드와 종목명을 넣으시면 그 기간만큼의 주식과 관련된 것들이 자동으로 데이터프레임으로 정리되도록 작성했습니다. 

### 만들어지는 데이터의 내용들은 다음과 같습니다.
### stockname, stockcode, 날짜, 시가, 고가, 저가, 종가, 거래량, 작성된 뉴스 수, 등락, 등락률, 등락률 절댓값, 대표 키워드 1~5


### 참고사항
### 대표키워드는 형태소 분석 후 명사 중 상위 단어를 선택했으며, 주식종목명과 같은 키워드들은 당연히 많이 나오기에 제거합니다.
### 기간을 하루로 지정 시, 주식 장이 열리지 않는 날이 있기에 그런 경우는 알아서 조정합니다.
### 네이버 금융 사이트를 이용했습니다.
### 주식코드와 종목명은 네이버에서 검색하면 찾아볼 수 있으며, R에서 여러 패키지로도 존재합니다.
make_df_of_stock_ver3 <- function(code, startDate, endDate, name) { 
  
  url <- "https://fchart.stock.naver.com/siseJson.nhn"
  
  if (as.character(wday(ymd(startDate))) == "2") {
    startDate <- (ymd(startDate)-3) %>% as.character() %>% str_remove_all(.,"-")
  } else if (as.character(wday(ymd(startDate))) == "1") {
    startDate <- (ymd(startDate)-2) %>% as.character() %>% str_remove_all(.,"-")
  } else {
    startDate <- (ymd(startDate)-1) %>% as.character() %>% str_remove_all(.,"-")
  }
  
  
  data <- POST(url, query =
                 list(
                   symbol = code,
                   requestType = '1',
                   startTime =  startDate,
                   endTime =  endDate,
                   timeframe =  'day'
                 ))
  
  stock_info <- content(data, "text") 
  stock_info <- suppressWarnings(read_csv(stock_info)) 
  stock_info <- stock_info[,1:6] 
  stock_info <- na.omit(stock_info) 
  colnames(stock_info) <- gsub('[[:punct:]]','',colnames(stock_info)) 
  
  for(i in 1:nrow(stock_info)) {
    stock_info[i,1] <- gsub('[[:punct:]]','',stock_info[i,1]) 
  } 
  
  stock_name <- data.frame(stockname = rep(name, nrow(stock_info))) 
  stock_code <- data.frame(stockcode = rep(code, nrow(stock_info)))
  stock_info <- cbind(stock_name,stock_code,stock_info) 
  
  QUERY <- URLencode(name) 
  QUERY <- str_replace(QUERY, "&","%26")
  QUERY <- str_replace_all(QUERY, "\\(","%28")
  QUERY <- str_replace_all(QUERY, "\\)","%29") 
  number_of_news <- matrix(ncol=1, nrow=nrow(stock_info)) %>% as.data.frame() 
  finance_naver_url_1 <- "https://finance.naver.com/news/news_search.nhn?rcdate=&q=" 
  finance_naver_url_2 <- "&x=0&y=0&sm=all.basic&pd=4&stDateStart="
  finance_naver_url_3 <- "&stDateEnd="
  finance_naver_url_4 <- '&page=' 
  
  for(i in 1:nrow(stock_info)) {
    finance_naver_url <- paste0(finance_naver_url_1,QUERY,finance_naver_url_2,as.Date(stock_info[i,3], tryFormats = "%Y%m%d"),finance_naver_url_3,as.Date(stock_info[i,3], tryFormats = "%Y%m%d"))
    the_number_of_news <- NULL
    html <- read_html(finance_naver_url, encoding = "CP949")
    the_number_of_news <- (html_nodes(html,'p strong') %>% html_text())[2] %>% str_remove(',') %>% as.numeric()
    number_of_news[i,1] <- the_number_of_news
  }
  
  stock_info <- cbind(stock_info,number_of_news)
  colnames(stock_info)[9] <- "numberofnews" 
  
  fluctuation <- matrix(ncol=1, nrow=nrow(stock_info)) %>% as.data.frame() 
  
  for (i in 2:nrow(stock_info)) {
    fluctuation[i,1] <- (stock_info[i,7] - stock_info[i-1,7])
  } 
  
  
  changepct <- matrix(ncol=1, nrow=nrow(stock_info)) %>% as.data.frame() 
  
  for (i in 2:nrow(stock_info)) {
    changepct[i,1] <- round((stock_info[i,7] - stock_info[i-1,7])/stock_info[i-1,7]*100,2)
  } 
  
  stock_info <- cbind(stock_info,fluctuation,changepct,abs(changepct))
  colnames(stock_info)[10:12] <- c("fluc","changepct","abschangepct") 
  stock_info <- na.omit(stock_info) 
  
  
  page <- as.data.frame(matrix(ncol=1, nrow=nrow(stock_info))) 
  keywords <- matrix(ncol=5, nrow=nrow(stock_info)) %>% as.data.frame() 
  
  for(i in 1:nrow(stock_info)) {
    page[i,1] <- ceiling(stock_info[i,9]/20)
  } 
  
  for(i in 1:nrow(stock_info)) {
    if (page[i,1] ==0) next 
    PAGE <- seq(from=1, to=page[i,1], by=1) 
    news_titles <- NULL
    
    
    for(x in PAGE) {
      finance_naver_url <-  paste0(finance_naver_url_1,QUERY,finance_naver_url_2,as.Date(stock_info[i,3], tryFormats = "%Y%m%d"),finance_naver_url_3,as.Date(stock_info[i,3], tryFormats = "%Y%m%d"),finance_naver_url_4,x)
      
      
      
      
      for(url in finance_naver_url) {
        html <- read_html(url,encoding="CP949")
        news_titles <- c(news_titles,html %>% 
                           html_nodes(css='.articleSubject') %>%   
                           html_nodes('a') %>%
                           html_text()
        )
      }
    }
    
    news_titles_pre <- gsub("[\n]",'', news_titles)
    news_titles_pre <- gsub("[[:punct:]]",' ', news_titles_pre)
    news_titles_pre <- gsub("[[:cntrl:]]",' ',news_titles_pre)
    
    news_df <- news_titles_pre %>% as.data.frame()
    
    title <- 1:nrow(news_df)
    
    ntb<- cbind(title,news_titles_pre) %>% as_tibble(encoding="UTF-8")
    
    x<-c(NA,NA) 
    
    if(nrow(ntb)==1) {
      ntb <- rbind(ntb,x)
    } 
    
    ntb %>% unnest_tokens(pos, news_titles_pre, token=SimplePos09) %>%
      mutate(pos_order = 1:n()) %>%
      filter(str_detect(pos, "/n")) %>%
      mutate(pos_done = str_remove(pos, "/.*$")) %>%
      arrange(pos_order) %>%
      filter(nchar(pos_done) > 1) %>%
      select(pos_done) %>% count(pos_done, sort=T) -> wordfrequency
    
    wordfrequency <- subset(wordfrequency, (as.vector(wordfrequency$pos_done) %in% name) == F)
    
    
    
    
    keywords[i,1:5] <- wordfrequency$pos_done[1:5]
    
    
  }
  colnames(keywords)[1:5] <- c("keyword1", "keyword2", "keyword3", "keyword4", "keyword5")
  stock_info <- cbind(stock_info, keywords)
  
  
  
  
  
  return(stock_info)
  
}

# 예시
make_df_of_stock_ver3("128940","20210101","20210201","한미약품")

