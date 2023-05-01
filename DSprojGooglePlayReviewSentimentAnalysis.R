# Install and load required packages
#install.packages(c("Rselenium", "rvest", "httr"))
library(RSelenium)
library(rvest)
library(httr)
library(netstat)
library(stringr)
library(lubridate)
library(dplyr)
library(tidytext)
library(scales)
library(ggplot2)

results = data.frame()

# Start a Selenium server and open a browser window
rD <- rsDriver(browser = "firefox", port=free_port())
remDr <- rD[["client"]]

remDr$navigate("https://play.google.com/store/apps")
html_obj <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()
remDr$close()
rD$server$stop()

img_elements <- html_obj %>% html_nodes("div.ULeU3b.neq64b div div a")
img_hrefs <- img_elements %>% html_attr("href") %>% head(100)

for (url in img_hrefs){
  rD <- rsDriver(browser = "firefox", port=free_port(), check = FALSE, verbose = FALSE)
  remDr <- rD[["client"]]
  url <- paste("https://play.google.com", url, sep="")
  remDr$navigate(url)
  
  div_element <- remDr$findElement(using = "css", value = "div.TT9eCd") 
  div_text <- div_element$getElementText()
  rating <- as.numeric(gsub("[^[:digit:].]+", "", div_text))
  
  buttonElem <- remDr$findElement(using = "xpath", value = "//span[contains(text(), 'See all reviews')]")

  buttonElem$clickElement()
  
  html_obj <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()
  remDr$close()
  rD$server$stop()
  
  reviews <- html_obj %>% html_elements(".h3YV2d") %>% html_text() %>% head(20)
  
  review_data <- data.frame(url = url, reviews=reviews, rating = rating)
  
  results <- rbind(results, review_data)
}



# Load the AFINN sentiment lexicon
sentiments <- get_sentiments("afinn")

# Split the reviews into individual words and join the sentiment scores
reviews_sentiments <- results %>%
  unnest_tokens(word, reviews) %>%
  inner_join(sentiments, by = "word")

# Group the dataframe by app and calculate the average sentiment score
avg_sentiments <- reviews_sentiments %>%
  group_by(url) %>%
  summarize(avg_sentiment = mean(value))

# Rescale the sentiment scores to be between 0 and 5
avg_sentiments <- avg_sentiments %>%
  mutate(avg_sentiment = rescale(avg_sentiment, to = c(1, 5)))

# find ratings
app_ratings <- results %>%
  group_by(url) %>%
  slice(1) %>%
  select("url", "rating")

# Join the ratings and sentiment dataframes
avg_sentiments <- avg_sentiments %>%
  left_join(app_ratings, by="url")

# View the final output
avg_sentiments$url


correlation <- cor(avg_sentiments$avg_sentiment, avg_sentiments$rating)
correlation

ggplot(avg_sentiments, aes(x = avg_sentiment, y = rating)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  annotate("text", x = max(avg_sentiments$avg_sentiment), y = max(avg_sentiments$rating), 
           label = paste("Correlation =", correlation)) +
  labs(x = "review_sentiment", y = "Overall App Rating", title = "Scatter Plot of App Review Sentiment vs App Rating")
