주가와 뉴스 동시 크롤링
================
김민우
2021 06 20



### 주가는 관련 뉴스와 많은 관계가 있습니다. 정확한 상관관계나 선후관계는 알 수 없지만, 많은 영향을 주고 받습니다. 기존에 주가를 가져오는 함수들은 많은 분들에 의해 작성됐고, 뉴스를 크롤링 해오는 것 또한 많은 분들에 의해 작성됐지만, 하나로 결합한 함수는 찾아보기 어려웠습니다. 그래서 직접 제작해봤습니다.

### 네이버 금융 안의 기사를 참고하면 주가와 관련된 뉴스만을 정렬해서 볼 수 있습니다. 원하는 시작일과 종료일을 지정하시고, 종목코드와 종목명을 넣으시면 그 기간만큼의 주식과 관련된 것들이 자동으로 데이터프레임으로 정리되도록 작성했습니다.

### 만들어지는 데이터의 내용들은 다음과 같습니다.

### stockname, stockcode, 날짜, 시가, 고가, 저가, 종가, 거래량, 작성된 뉴스 수, 등락, 등락률, 등락률 절댓값, 대표 키워드 1\~5

### 참고사항

### 대표키워드는 형태소 분석 후 명사 중 상위 단어를 선택했으며, 주식종목명과 같은 키워드들은 당연히 많이 나오기에 제거합니다.

### 기간을 하루로 지정 시, 주식 장이 열리지 않는 날이 있기에 그런 경우는 알아서 조정합니다.

### 네이버 금융 사이트를 이용했습니다.

### 주식코드와 종목명은 네이버에서 검색하면 찾아볼 수 있으며, R에서 여러 패키지로도 존재합니다.

``` r
# Package loading
library(KoNLP)
library(httr)
library(rvest)
library(readr)
library(xml2)
library(stringr)
library(dplyr)
library(tidytext)
library(lubridate)
```

``` r
# 함수

stock <- function(code, startDate, endDate, name) { 
  
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
```

### 사용 예시

``` r
stock("128940","20210401","20210601","한미약품")
```

    ##    stockname stockcode     날짜   시가   고가   저가   종가 거래량 numberofnews
    ## 2   한미약품    128940 20210401 316500 320000 315500 320000  24920            5
    ## 3   한미약품    128940 20210402 319500 319500 315500 317000  22372            2
    ## 4   한미약품    128940 20210405 316500 317000 311500 313000  30661            2
    ## 5   한미약품    128940 20210406 313000 316000 312000 313000  25810           17
    ## 6   한미약품    128940 20210407 314500 331000 312000 329000  60348           17
    ## 7   한미약품    128940 20210408 329000 329500 321500 327500  30769           16
    ## 8   한미약품    128940 20210409 327500 337000 326000 329500  37470            4
    ## 9   한미약품    128940 20210412 330000 342000 326500 342000  79134            1
    ## 10  한미약품    128940 20210413 342500 351000 340000 346500  87895           13
    ## 11  한미약품    128940 20210414 346500 347000 338000 342000  51665           11
    ## 12  한미약품    128940 20210415 342000 368000 335500 358000 264673           15
    ## 13  한미약품    128940 20210416 361000 361500 342500 353500 142759            4
    ## 14  한미약품    128940 20210419 356000 362000 347500 356000 118624            2
    ## 15  한미약품    128940 20210420 353000 358000 350500 356000  55397           22
    ## 16  한미약품    128940 20210421 355000 368000 353500 358500 141842            6
    ## 17  한미약품    128940 20210422 361500 362000 347000 347000  83249            2
    ## 18  한미약품    128940 20210423 344000 378000 340500 367500 243269            4
    ## 19  한미약품    128940 20210426 365000 365500 357000 358000 104758            1
    ## 20  한미약품    128940 20210427 360000 362000 347500 347500  65335           21
    ## 21  한미약품    128940 20210428 352000 355500 337500 338500  74767           11
    ## 22  한미약품    128940 20210429 345000 351500 340000 341000  68330           25
    ## 23  한미약품    128940 20210430 344500 350500 338000 338000  53946            5
    ## 24  한미약품    128940 20210503 335500 337500 325000 325500  45358            5
    ## 25  한미약품    128940 20210504 322000 334500 320000 332500  44791            3
    ## 26  한미약품    128940 20210506 335000 341500 333500 339000  52767            4
    ## 27  한미약품    128940 20210507 342000 349000 337000 340500  52255            2
    ## 28  한미약품    128940 20210510 342500 353500 339500 351000  65954           33
    ## 29  한미약품    128940 20210511 351500 358500 346500 346500  63038            8
    ## 30  한미약품    128940 20210512 346000 351000 338000 343000  56109           22
    ## 31  한미약품    128940 20210513 342000 350000 336000 347500  68611            4
    ## 32  한미약품    128940 20210514 341500 342500 328000 329000 130105            6
    ## 33  한미약품    128940 20210517 330500 340000 328000 340000  72760            3
    ## 34  한미약품    128940 20210518 339500 363500 339500 353500 228585           37
    ## 35  한미약품    128940 20210520 357000 359000 351000 352500  59735            8
    ## 36  한미약품    128940 20210521 354500 355000 345000 345500  41563            2
    ## 37  한미약품    128940 20210524 341500 342000 332500 333500  65944            4
    ## 38  한미약품    128940 20210525 334000 336000 329500 331500  62169            5
    ## 39  한미약품    128940 20210526 331000 334000 329000 334000  38838            4
    ## 40  한미약품    128940 20210527 335000 354500 333000 353000 127862            7
    ## 41  한미약품    128940 20210528 352000 356500 347000 352500  58663            6
    ## 42  한미약품    128940 20210531 356500 362000 350500 355500  69163           15
    ## 43  한미약품    128940 20210601 356000 360000 352000 352000  42425           16
    ##      fluc changepct abschangepct         keyword1       keyword2     keyword3
    ## 2    3500      1.11         1.11             국내       1천400조       35만원
    ## 3   -3000     -0.94         0.94             10조 공정거래위원장     기술수출
    ## 4   -4000     -1.26         1.26             19종     메가비타민     비엘비정
    ## 5       0      0.00         0.00         롤론티스           임상         10대
    ## 6   16000      5.11         5.11       패스트트랙     단장증후군         신약
    ## 7   -1500     -0.46         0.46         롤론티스         한국인         효과
    ## 8    2000      0.61         0.61           가속화           강화         개발
    ## 9   12500      3.79         3.79             감소     연구개발비         이익
    ## 10   4500      1.32         1.32              5종           항암     혁신신약
    ## 11  -4500     -1.30         1.30       포지오티닙         안전성          2회
    ## 12  16000      4.68         4.68             백신           국내          8월
    ## 13  -4500     -1.26         1.26             정부          1분기          8월
    ## 14   2500      0.71         0.71              1위            2위         30개
    ## 15      0      0.00         0.00           러시아     아모잘탄큐       고혈압
    ## 16   2500      0.70         0.70   11조2천917억원             lg       lg엔솔
    ## 17 -11500     -3.21         3.21            1분기           개선         뉴욕
    ## 18  20500      5.91         5.91             백신       위탁생산       특징주
    ## 19  -9500     -2.59         2.59            1분기          k제약         실적
    ## 20 -10500     -2.93         2.93            1분기       영업이익        299억
    ## 21  -9000     -2.59         2.59            1분기             27     롤론티스
    ## 22   2500      0.74         0.74     자가검사키트       코로나19         약국
    ## 23  -3000     -0.88         0.88          2분기에     sk바이오팜      강남3구
    ## 24 -12500     -3.70         3.70           17억원             1q          7일
    ## 25   7000      2.15         2.15             국내         가능성       날아오
    ## 26   6500      1.95         1.95         1700억원          1분기       sk바사
    ## 27   1500      0.44         0.44             3200      6거래일째         관심
    ## 28  10500      3.08         3.08     자가검사키트       코로나19       온라인
    ## 29  -4500     -1.28         1.28           국산화           아트 자가검사키트
    ## 30  -3500     -1.01         1.01       희귀의약품           추가   바이오신약
    ## 31   4500      1.31         1.31             백신             21 83만5000회분
    ## 32 -18500     -5.32         5.32 삼성바이오로직스       3659가구          5곳
    ## 33  11000      3.34         3.34           22계단         녹십자   두산\u91cd
    ## 34  13500      3.97         3.97             백신         제넥신     코로나19
    ## 35  -1000     -0.28         0.28             백신           공개         전략
    ## 36  -7000     -1.99         1.99           제넥신          1분기          1호
    ## 37 -12000     -3.47         3.47             뷰티         sk바사         가능
    ## 38  -2000     -0.60         0.60             민관           유예       지재권
    ## 39   2500      0.75         0.75           30조원           국내         규모
    ## 40  19000      5.69         5.69             백신         글로벌         기업
    ## 41   -500     -0.14         0.14             개최         온라인           10
    ## 42   3000      0.85         0.85           기념식           백신         대표
    ## 43  -3500     -0.98         0.98         제약업계           백신         유한
    ##        keyword4     keyword5
    ## 2         664건           cp
    ## 3      문화정착         세계
    ## 4          성분       타짐주
    ## 5  미국암학회서     스펙트럼
    ## 6        치료제     혁신신약
    ## 7           3상         대상
    ## 8          도약     바이오주
    ## 9        정상화     주가수준
    ## 10 미국암학회서         연구
    ## 11       내약성         복용
    ## 12         생산       제약사
    ## 13       광고법     남양유업
    ## 14           gc         관심
    ## 15         백신       치료제
    ## 16         개선 건강기능식품
    ## 17         동탄     동탄에서
    ## 18       가능성     가상화폐
    ## 19         장밋         <NA>
    ## 20       영업익       전년비
    ## 21         승인          101
    ## 22         전국         판매
    ## 23       거리두         누구
    ## 24           vc         구매
    ## 25         다운     리베이트
    ## 26         가동         공장
    ## 27         기관         누구
    ## 28       상반기 유가증권시장
    ## 29         14년         34호
    ## 30     폐섬유증         백신
    ## 31    890만회분         계약
    ## 32         건설   굴뚝\u682a
    ## 33         성적         수소
    ## 34         개발       위탁생
    ## 35       제넥신         초록
    ## 36       sk바사         백신
    ## 37         개발       녹십자
    ## 38         관련         간담
    ## 39         기업         뉴스
    ## 40       김태억         달째
    ## 41          6월         70개
    ## 42         발명       대규모
    ## 43         휴가     기술수출
