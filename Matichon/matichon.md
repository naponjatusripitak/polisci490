# วิเคราะห์ข่าวการเมืองจากมติชนออนไลน์ด้วย Latent Dirichlet Allocation

### ก่อนอื่นต้องโหลด package ที่ต้องใช้ในการเก็บข้อมูลและการวิเคราะห์
```R
packages <- c("xml2","rvest", "dplyr", "tm", "tidytext", "ggplot2", "stopwords", "SnowballC", "tidyverse", "lubridate", "stringr", "httr", "SnowballC", "wdman", "RSelenium", "tcltk", "XML", "topicmodels", "stringi", "LDAvis", "slam", "ldatuning", "kableExtra", "widyr", "igraph", "ggraph", "fmsb", "pander")

load.packages <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}

lapply(packages, load.packages)
```
### เริ่ม scrape ข้อมูลข่าวด้วย ```RSelenium```
```R
rD <- rsDriver()
remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4567L
                      , browserName = "chrome"
)

remDr$open()

matichon <- data.frame(title=NULL, text=NULL, date=NULL, link=NULL)

remDr$setTimeout(type = "page load", milliseconds = 100000)
remDr$setImplicitWaitTimeout(milliseconds = 100000)

lastpage <- 2249

for(i in 1:lastpage) {
  tryCatch({
    site <- paste0("https://www.matichon.co.th/category/politics/page/",i) # create URL for each page to scrape
    remDr$navigate(site) # navigates to webpage
    elem <- remDr$findElements(using="class", value="item-details")
    url_find <- remDr$findElements(using="xpath", "//*[@id='td-outer-wrap']/div[2]/div/div[4]/div[1]/div/div/div[2]/h3/a")
    
    url <- unlist(sapply(url_find, function(x) {x$getElementAttribute("href")}))
    title <- unlist(sapply(url_find, function(x) {x$getElementAttribute("title")}))
    meta_find <- remDr$findElements(using="xpath", "//*[@id='td-outer-wrap']/div[2]/div/div[4]/div[1]/div/div/div[2]/div[1]/span[2]/time")
    date <- unlist(sapply(meta_find, function(x) {x$getElementAttribute("datetime")}))
    
    article_body <- NULL
    for(j in 1:length(url)){
      article_html <- url[j] %>%
        read_html() 
      article_body[j] <- article_html %>%
        html_nodes(xpath="//div[@class='td-post-content']/p") %>%
        html_text() %>%
        paste(collapse = '\n\n')
    }
    
    matichon_page <- data.frame(title=title, text=article_body, date=date, link=url)
    matichon <- rbind(matichon, matichon_page)
    
  }, error=function(e){
    Sys.sleep(sample(1:10, 1))
    remDr$navigate(site) # navigates to webpage
    elem <- remDr$findElements(using="class", value="item-details")
    url_find <- remDr$findElements(using="xpath", "//*[@id='td-outer-wrap']/div[2]/div/div[4]/div[1]/div/div/div[2]/h3/a")
    
    url <- unlist(sapply(url_find, function(x) {x$getElementAttribute("href")}))
    title <- unlist(sapply(url_find, function(x) {x$getElementAttribute("title")}))
    meta_find <- remDr$findElements(using="xpath", "//*[@id='td-outer-wrap']/div[2]/div/div[4]/div[1]/div/div/div[2]/div[1]/span[2]/time")
    date <- unlist(sapply(meta_find, function(x) {x$getElementAttribute("datetime")}))
    
    article_body <- NULL
    for(j in 1:length(url)){
      article_html <- url[j] %>%
        read_html() 
      article_body[j] <- article_html %>%
        html_nodes(xpath="//div[@class='td-post-content']/p") %>%
        html_text() %>%
        paste(collapse = '\n\n')
    }
    
    matichon_page <- data.frame(title=title, text=article_body, date=date, link=url)
    matichon <- rbind(matichon, matichon_page)})
}
```

