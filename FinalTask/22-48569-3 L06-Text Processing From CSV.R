# Required Packages
packages <- c("tidyverse", "readr", "stringr", "janitor",
              "tidytext", "SnowballC", "wordcloud", "topicmodels")

installed_packages <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

csv_path <- "C:\\Users\\User\\Downloads\\dailystar_articles.csv"

TEXT_COL <- "content"
# Read CSV
raw <- read_csv(
  csv_path,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

# Detect Text Column if Missing
if (!TEXT_COL %in% names(raw)) {
  candidates <- names(raw)[map_lgl(raw, ~ is.character(.x) || is.factor(.x))]
  TEXT_COL <- candidates[
    which.max(
      sapply(raw[candidates],
             function(x) mean(nchar(as.character(x)), na.rm = TRUE))
    )
  ]
  message("Guessed text column: ", TEXT_COL)
}

# Prepare DataFrame
df <- raw %>%
  mutate(
    doc_id = row_number(),
    text   = as.character(.data[[TEXT_COL]])
  ) %>%
  select(doc_id, everything())
# Normalize encoding
df$text <- iconv(df$text, from = "", to = "UTF-8")


cat("Rows:", nrow(df), "\n")
cat("NAs in text:", sum(is.na(df$text)), "\n")

# Remove NA, Blank & Duplicate Texts
df <- df %>%
  filter(!is.na(text)) %>%
  mutate(text = str_squish(text)) %>%
  filter(text != "") %>%
  distinct(text, .keep_all = TRUE)

# Sample output
df %>% select(doc_id, text) %>% head(5)
# Text Cleaning Function

clean_text <- function(x) {
  x %>%
    str_replace_all("https?://\\S+|www\\.[^\\s]+", " ") %>%  # URLs
    str_replace_all("@\\w+|#\\w+", " ") %>%                 # mentions / hashtags
    str_replace_all("[^\\p{L}\\p{N}\\s']", " ") %>%         # letters & numbers
    str_to_lower() %>%
    str_squish()
}

df <- df %>% mutate(text_clean = clean_text(text))

# Cleaned sample
df %>% select(doc_id, text_clean) %>% head(5)

# Tokenization & Stopword Removal
# tidytext English stopwords
data(stop_words) 

tokens <- df %>%
  unnest_tokens(word, text_clean) %>%
  anti_join(stop_words, by = "word") %>%
  filter(nchar(word) >= 3)

tokens %>% head(10)

# Stemming
tokens <- tokens %>%
  mutate(stem = SnowballC::wordStem(word, language = "en"))

tokens %>% select(doc_id, word, stem) %>% head(10)


# Word Frequency
word_freq <- tokens %>%
  count(stem, sort = TRUE)

head(word_freq, 20)

# Top Words Bar Chart
top_n <- 20

ggplot(
  word_freq %>% slice_max(n, n = top_n),
  aes(x = reorder(stem, n), y = n)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste("Top", top_n, "Words by Frequency"),
    x = "Word (Stem)",
    y = "Count"
  ) +
  theme_minimal()


# Word Cloud-
library(wordcloud)
wordcloud::wordcloud(
  words = wc_df$stem,
  freq  = wc_df$n,
  max.words = 200,
  random.order = FALSE
)
# Save Wordcloud
png("wordcloud.png", width = 1200, height = 900, res = 150)
par(mar = c(1,1,1,1))
wordcloud::wordcloud(
  words = wc_df$stem,
  freq  = wc_df$n,
  max.words = 200,
  random.order = FALSE
)
dev.off()

# TF-IDF Calculation
tfidf <- tokens %>%
  count(doc_id, stem, sort = FALSE) %>%
  bind_tf_idf(term = stem, document = doc_id, n = n) %>%
  arrange(desc(tf_idf))

head(tfidf, 10)

# Document-Term Matrix (TF-IDF)
dtm_tfidf <- tfidf %>%
  cast_dtm(document = doc_id, term = stem, value = tf_idf)

dtm_tfidf

# Save Outputs
write_csv(df,        "cleaned_text.csv")
write_csv(word_freq, "top_words.csv")
write_csv(tfidf,     "tfidf_by_doc.csv")

cat("Saved files:\n",
    "- cleaned_text.csv\n",
    "- top_words.csv\n",
    "- tfidf_by_doc.csv\n",
    "- wordcloud.png\n")



