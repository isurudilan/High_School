# word clouds

library(dplyr)
library(wordcloud2)
library(tidyr)
library(tidytext)


df2<-df

# Remove "(Taylor's Version)" from track names
df2$track_name <- gsub("\\(Taylor's Version\\)|Taylor’s Version|\\(From The Vault\\)", "", df2$track_name)

# Print cleaned track names
head(df2)

# Filter data for Taylor Swift and Ed Sheeran
taylor_swift_data <- df2 %>% 
  filter(artist_name == "Taylor Swift")

ed_data <- df2 %>% 
  filter(artist_name == "Ed Sheeran")

# Tokenize track names and count word frequencies for Taylor Swift
taylor_swift_word_freq <- taylor_swift_data %>%
  unnest_tokens(word, track_name) %>%
  count(word) %>%
  arrange(desc(n))

# Tokenize track names and count word frequencies for Ed Sheeran
ed_word_freq <- ed_data %>%
  unnest_tokens(word, track_name) %>%
  count(word) %>%
  arrange(desc(n))

#Taylor cloud

# Create word clouds for Taylor Swift and Ed Sheeran
# Words to be removed
words_to_remove <- c("the", "this", "to", "is", "are")

# Filter out the words to be removed from your word frequency data
taylor_swift_word_freq_filtered <- taylor_swift_word_freq[!taylor_swift_word_freq[,1] %in% words_to_remove, ]

# Set up your word cloud parameters
ts_size <- 1
ts_bg_color <- "white"
ts_color <- ifelse(taylor_swift_word_freq_filtered[, 2] > 8, 'red', 'skyblue')

# Convert your filtered data to a data frame
ts_data <- as.data.frame(taylor_swift_word_freq_filtered)
ts_dataOut <- ts_data[,1:2]
names(ts_dataOut) <- c("name", "freq")

# Calculate weight factor
weightFactor <- ts_size * 180 / max(ts_dataOut$freq)

# Define hover function and widget size
hoverFunction <- NULL
widgetsize <- NULL

# Set up word cloud settings
ts_settings <- list(
  word = ts_dataOut$name,
  freq = ts_dataOut$freq,
  fontFamily = 'Segoe UI',
  fontWeight = 'bold',
  color =  ts_color,
  minSize =  0,
  weightFactor = weightFactor,
  backgroundColor = ts_bg_color,
  gridSize =  0,
  minRotation = -pi/4,
  maxRotation = pi/4,
  shuffle = TRUE,
  rotateRatio = 0.4,
  shape = 'circle',
  ellipticity = 0.65,
  figBase64 = NULL,
  hover = htmlwidgets::JS(hoverFunction)
)

# Create the word cloud
htmlwidgets::createWidget("wordcloud2", ts_settings,
                          width = widgetsize[1],
                          height = widgetsize[2],
                          sizingPolicy = htmlwidgets::sizingPolicy(
                            viewer.padding = 0,
                            browser.padding = 0,
                            browser.fill = TRUE
                          ))

### Ed's cloud

# Words to be removed
words_to_remove <- c("on", "the", "in")

# Filter out the words to be removed from your word frequency data
ed_word_freq_filtered <- ed_word_freq[!ed_word_freq[,1] %in% words_to_remove, ]

# Set up your word cloud parameters
b_size <- 1
b_bg_color <- "white"
ts_color <- ifelse(ed_word_freq_filtered[, 2] > 8, 'purple', 'green')

# Convert your filtered data to a data frame
b_data <- as.data.frame(ed_word_freq_filtered)
b_dataOut <- b_data[,1:2]
names(b_dataOut) <- c("name", "freq")

# Calculate weight factor
b_weightFactor <- b_size * 180 / max(b_dataOut$freq)

# Define hover function and widget size
hoverFunction <- NULL
widgetsize <- NULL

# Set up word cloud settings
b_settings <- list(
  word = b_dataOut$name,
  freq = b_dataOut$freq,
  fontFamily = 'Segoe UI',
  fontWeight = 'bold',
  color =  ts_color,
  minSize =  0,
  weightFactor = b_weightFactor,
  backgroundColor = b_bg_color,
  gridSize =  0,
  minRotation = -pi/4,
  maxRotation = pi/4,
  shuffle = TRUE,
  rotateRatio = 0.4,
  shape = 'circle',
  ellipticity = 0.65,
  figBase64 = NULL,
  hover = htmlwidgets::JS(hoverFunction)
)

# Create the word cloud
htmlwidgets::createWidget("wordcloud2", b_settings,
                          width = widgetsize[1],
                          height = widgetsize[2],
                          sizingPolicy = htmlwidgets::sizingPolicy(
                            viewer.padding = 0,
                            browser.padding = 0,
                            browser.fill = TRUE
                          ))


