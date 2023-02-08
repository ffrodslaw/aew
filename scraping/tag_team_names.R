library(rvest)
library(stringr)

tag_teams_url <- "http://www.cagematch.net/?id=28"

tag_teams_page <- read_html(url(tag_teams_url)) 

last_page <- tag_teams_page |> 
  html_element("#TableHeader") |> 
  html_text() |> 
  str_extract(pattern = "(?<=total ).*(?= items)") |> 
  as.numeric()

pages <- seq(0, last_page, by = 100)

tag_teams_table <- list()

for(i in pages){
  
  i.url <- paste("http://www.cagematch.net/?id=28&view=teams&s=", i, sep = "")
  tag_teams_table[[i/100+1]] <- read_html(url(i.url)) |> 
    html_element("table") |> 
    html_table(header = T)
  
    print(i)
  
}

tag_teams_df <- bind_rows(tag_teams_table)
colnames(tag_teams_df) <- c("rank", "name", "members", "ratings", "votes")

#####################

# word cloud

library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)

text <- tag_teams_df$name
docs <- Corpus(VectorSource(text))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# remove "and"
df <- df |> 
  dplyr::filter(word != "and" & word != "the" & word != "los")

set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1,           
          max.words=200, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))

# save as html

# Make the graph
my_graph <- wordcloud2(data=df, size=1.6, color='random-light', backgroundColor = "black")

# save it in html
library("htmlwidgets")
saveWidget(my_graph,"~/Documents/projects/aew/tag_team_wordcloud.html",selfcontained = F)
