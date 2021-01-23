library(RSelenium)
library(rvest)
library(dplyr)


remDr <- remoteDriver(remoteServerAddr="localhost", port=4445L, browserName = "chrome",)
remDr$open()
remDr$navigate("https://www.instagram.com/explore/tags/%EC%A7%9C%EC%9E%A5%EB%A9%B4/")


posting_j <- remDr$findElement(using = "xpath", 
                               value = "//*[@id="react-root"]/section/main/article/div[2]/div/div[1]/div[1]/a/div[1]/div[2]")

posting_j$clickElement()

url_j <- remDr$getPageSource()[[1]]
url_j <- read_html(url_j)

links <- html %>% html_nodes(".c-Yi7") %>% html_nodes("time") %>% html_attr("title")