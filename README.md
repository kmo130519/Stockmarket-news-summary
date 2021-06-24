# Stockmarket-news-summary
주가를 자동으로 크롤링 해주는 함수는 많습니다. 기존의 주가 크롤링 함수에 네이버 뉴스를 결합하여 기사의 메인 단어들을 한꺼번에 볼 수 있는 함수를 제작했습니다.

# 활용방안
DB와 연결하여 전 종목의 주가를 관리할 수 있습니다. 자동으로 쿼리를 날리는 함수를 제작하셔서 등을 하시면 됩니다.
R과 DB의 연동은 RMariaDB [(해당패키지확인)](https://cran.r-project.org/web/packages/RMariaDB/index.html)등의 패키지를 참고하시고, 원하는 시간 대마다 자동 업데이트를 원하시면 taskscheduleR  [(해당패키지확인)](https://cran.r-project.org/web/packages/taskscheduleR/taskscheduleR.pdf)패키지를 알아보시면 좋습니다.
태블로 같은 시각화 툴을 이용하시면 기사 수와 주가와의 상관관계를 보실 수 있습니다. 단순 그래프의 닮음성으로는 등락률의 절댓값과 기사량이 제법 닮음을 확인할 수 있습니다.



![주식](https://user-images.githubusercontent.com/70559817/123257903-b9a86e80-d52d-11eb-8493-dac34a3c81fe.png)

혹은 개인의 주식 포트폴리오를 만들 수도 있습니다.    
[개인 포트폴리오 예시 Using Tableu](https://public.tableau.com/app/profile/.57331214/viz/Juice_PortfolioInterest_/portfolio)

그 외 활용 예시사진
![주식1](https://user-images.githubusercontent.com/70559817/123258048-e492c280-d52d-11eb-89ef-aed462a18161.png)
![주식2](https://user-images.githubusercontent.com/70559817/123258061-e65c8600-d52d-11eb-9c43-cff852839e35.png)






