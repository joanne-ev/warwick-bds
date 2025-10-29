
# Load the necessary libraries 
library(rvest)
library(httr2)
library(tidyverse)

base_website <- 'https://books.toscrape.com/'

og_req <- request(website)

og_resp <- og_req %>%
  req_perform()

categories <- resp %>%
  resp_body_html() %>%
  html_elements(".nav-list ul a") %>% 
  html_text2() 

n <- length(categories)

links <- resp %>%
  resp_body_html() %>%
  html_elements(".nav-list ul a") %>% 
  html_attr("href")


for (i in seq(0, n)) {
  
  cat = categories[i]
  
  cat = "Fiction"
  
  if (cat == "Fiction") {
    
    cat_website <- links[i]
    website <- paste0(base_website, cat_website)
    
    req <- request(website)
    
    resp <- req %>% 
      req_perform()
    
    pages <- resp %>% 
      resp_body_html() %>%
      html_elements(".current") %>%
      html_text2()
    
    page <- str_extract(pages, "\\d+$") %>% as.numeric()
    
    for (j in seq(0, page)) {
      
      # Loop through pages by cutting the below (e.g., https://books.toscrape.com/catalogue/category/books/fiction_10/page-1.html)
        
      website <- paste0(base_website, cat_website, "page-", j, ".html")
      
      prices <- resp %>%
        resp_body_html() %>%
        html_elements(".price_color") %>%
        html_text2()
      
      titles <- resp %>% 
        resp_body_html() %>%
        html_elements("h3 a") %>%
        html_attr("title")
      
      ratings <- resp %>% 
        resp_body_html() %>%
        html_elements(".star-rating") %>% 
        html_attr("class")
      
      ratings <- ratings %>% gsub("star-rating ", "", .) %>% paste(., "stars")
      
      data.frame(
        Title = titles, 
        Price = prices,
        Rating = ratings
      )
    }
  }
  
}

# 
# 
# website_2 <- "https://books.toscrape.com/catalogue/category/books/fiction_10/index.html"
# 
# 
# testreq <- request(website_2)
# 
# testresp <- testreq %>%
#   req_perform()
# 
# pages <- testresp %>% 
#   resp_body_html() %>%
#   html_elements(".current") %>%
#   html_text2()
# 
# number <- str_extract(pages, "\\d+$") %>% as.numeric()
# 
# number
# 
# 
# cat_website <- "catalogue/category/books/fiction_10/"
# 
# website <- paste0(base_website, cat_website, "page-", 4, ".html")
# 
# website




