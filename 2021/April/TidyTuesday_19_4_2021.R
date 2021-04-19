
# Raw data ----------------------------------------------------------------

netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')


# Upload the packages -----------------------------------------------------

library(tm)
library(SnowballC)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm)


# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Quicksand")

font_add_google("Work Sans")

font_labels <- "Quicksand"

font_labels2 <- "Work Sans"

showtext_auto()



# Corpus

netflix_titles<-select(netflix_titles,12)

netflix_titles <- iconv(netflix_titles$description, to = "UTF-8")

netflix_titles_title<- Corpus(VectorSource(netflix_titles))


# Next, we will convert the corpus to a plain text document.

netflix_titles_title<- tm_map(netflix_titles_title, PlainTextDocument)

###############################  Step 2: Text transformation

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
netflix_titles_title<- tm_map(netflix_titles_title, toSpace, "/")
netflix_titles_title<- tm_map(netflix_titles_title, toSpace, "@")
netflix_titles_title<- tm_map(netflix_titles_title, toSpace, "\\|")


###############################  Step 3: Cleaning the text
# Convert the text to lower case
netflix_titles_title<- tm_map(netflix_titles_title, content_transformer(tolower))


# Remove numbers
netflix_titles_title<- tm_map(netflix_titles_title, removeNumbers)

# Remove english common stopwords
netflix_titles_title<- tm_map(netflix_titles_title, removeWords, stopwords("english"))

# Remove your own stop word
# specify your stopwords as a character vector
netflix_titles_title<- tm_map(netflix_titles_title, removeWords, c("blabla1", "blabla2")) 
netflix_titles_title<- tm_map(netflix_titles_title, removeWords, c('â???"', 'â???')) 
netflix_titles_title<- netflix_titles_title  %>% filter (word != "â???"")

# Remove punctuations
netflix_titles_title<- tm_map(netflix_titles_title, removePunctuation)

# Eliminate extra white spaces
netflix_titles_title<- tm_map(netflix_titles_title, stripWhitespace)

# Eliminate extra the, this
netflix_titles_title<- tm_map(netflix_titles_title, removeWords, c('the', 'this', 'applause', stopwords('english')))

############################### Step 4 : Build a term-document matrix

dtnetflix_titles_title <- TermDocumentMatrix(netflix_titles_title)
m <- as.matrix(dtnetflix_titles_title)
v <- sort(rowSums(m),decreasing=TRUE)
dtnetflix_titles_title<- data.frame(word = names(v),freq=v)


dtnetflix_titles_title = dtnetflix_titles_title[-c(4),]

dtnetflix_titles_title<-dtnetflix_titles_title%>% filter(freq > 50)

letterCloud(dtnetflix_titles_title,word = "N", size = 1.5,letterFont = font_labels, color = "#B81D24")