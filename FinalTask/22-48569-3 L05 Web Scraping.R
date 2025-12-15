library(rvest)
library(stringr)
# Website URL

url <- "https://www.thedailystar.net/"
webpage <- read_html(url)
# -------------------------------
# Extract Titles
titles_raw <- webpage %>%
  html_elements("h3") %>%
  html_text2()

# Extract Links
links_raw <- webpage %>%
  html_elements("h3 a") %>%
  html_attr("href")

# Initialize vectors
article_times <- c()
all_paragraphs <- c()
#Extract Article Time
for (link in links_raw) {
  
  # Fix relative links
  if (!str_detect(link, "^http")) {
    link <- paste0("https://www.thedailystar.net", link)
  }
  
  article_page <- tryCatch(read_html(link), error = function(e) NULL)
  
  if (!is.null(article_page)) {
    time_text <- article_page %>%
      html_element("time") %>%
      html_text2()
    
    article_times <- c(article_times, time_text)
  }
}
#Extract Article Content
for (link in links_raw) {
  
  if (!str_detect(link, "^http")) {
    link <- paste0("https://www.thedailystar.net", link)
  }
  
  article_page <- tryCatch(read_html(link), error = function(e) NULL)
  
  if (!is.null(article_page)) {
    p_text <- article_page %>%
      html_elements("p") %>%
      html_text2()
    
    all_paragraphs <- c(all_paragraphs, paste(p_text, collapse = " "))
  }
}
#Create Data Frame
articles_df <- data.frame(
  title = titles_raw,
  link = links_raw,
  time = article_times,
  content = all_paragraphs,
  stringsAsFactors = FALSE
)
#Output
print(articles_df)

write.csv(articles_df, "dailystar_articles.csv", row.names = FALSE)
