"Task 2"

###################################################################
######### Movie Reviews Extraction (IMDB) ###########
install.packages("rvest")
install.packages("XML")
install.packages("magrittr")

library(rvest)
library(XML)
library(magrittr)

aurl <- "https://www.imdb.com/title/tt6077448/reviews?ref_=tt_urv"

imdbmovie_reviews <- NULL


#809 global ratings divided by 25 we get 32 pages
# for only 15 pages- each page 25 reviews = 375 reviews we get
for (i in 1:15){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>% html_nodes(".title") %>% html_text()
  imdbmovie_reviews <- c(imdbmovie_reviews,rev)
}

write.table(imdbmovie_reviews,"imdbmovies.txt")
getwd()

##################################
#### Sentiment Analysis ####
txt <- imdbmovie_reviews

str(txt)
length(txt)
View(txt)

# install.packages("tm")
library(tm)

# Convert the character data to corpus type
x <- Corpus(VectorSource(txt))

inspect(x[1])

x <- tm_map(x, function(x) iconv(enc2utf8(x), sub='byte'))

# Data Cleansing
x1 <- tm_map(x, tolower)
inspect(x1[1])

x1 <- tm_map(x1, removePunctuation)
inspect(x1[1])

x1 <- tm_map(x1, removeNumbers)
inspect(x1[1])

x1 <- tm_map(x1, removeWords, stopwords('english'))
inspect(x1[1])

# striping white spaces 
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])

# Term document matrix 
# converting unstructured data to structured format using TDM
#Non-/sparse entries: 1710/33540
#Sparsity           : 95%
#Maximal term length: 14
tdm <- TermDocumentMatrix(x1)
tdm
dtm <- t(tdm) # transpose
dtm <- DocumentTermMatrix(x1)

# To remove sparse entries upon a specific value
corpus.dtm.frequent <- removeSparseTerms(tdm, 0.99) 

tdm <- as.matrix(tdm)
dim(tdm)

tdm[1:20, 1:20]

inspect(x[1])

# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 30) # repeat atlest 30 times
w_sub

barplot(w_sub, las=2, col = rainbow(30))

# Term phone season maximum number of times
x1 <- tm_map(x1, removeWords, c('dont','watch','season')) # remove very popular terms and those which is not required
x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x1)
tdm

tdm <- as.matrix(tdm)
tdm[10:82,1:40]

# Bar plot after removal of the term 'phone'
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 20)
w_sub

barplot(w_sub, las=2, col = rainbow(30))

##### Word cloud #####
install.packages("wordcloud")
library(wordcloud)

wordcloud(words = names(w_sub), freq = w_sub)

w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
head(w_sub1)

wordcloud(words = names(w_sub1), freq = w_sub1) # all words are considered

# better visualization
wordcloud(words = names(w_sub1), freq = w_sub1, random.order=F, colors=rainbow(30), scale = c(2,0.5), rot.per = 0.4)
windows()
wordcloud(words = names(w_sub1), freq = w_sub1, random.order=F, colors= rainbow(30),scale=c(3,0.5),rot.per=0.3)

windowsFonts(JP1 = windowsFont("MS Gothic"))
par(family = "JP1")
wordcloud(x1, scale= c(2,0.5))

############# Wordcloud2 ###############

install.packages("wordcloud2")
library(wordcloud2)

w1 <- data.frame(names(w_sub), w_sub)
colnames(w1) <- c('word', 'freq')

wordcloud2(w1, size=0.3, shape='circle')

wordcloud2(w1, size=0.3, shape = 'triangle')
wordcloud2(w1, size=0.3, shape = 'star')


#### Bigram ####
library(rJava) # Install java software then on;y Rweka Works, java, visualstudio for compatibility
library(RWeka)
library(wordcloud)

minfreq_bigram <- 2
bitoken <- NGramTokenizer(x1, Weka_control(min = 2, max = 2))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq, decreasing = TRUE), ]

wordcloud(sort_two$bitoken, sort_two$Freq, random.order = F, scale = c(2, 0.35), min.freq = minfreq_bigram, colors = brewer.pal(8, "Dark2"), max.words = 150)

# lOADING Positive and Negative words  
pos.words <- readLines(file.choose())	# read-in positive-words.txt
neg.words <- readLines(file.choose()) 	# read-in negative-words.txt

stopwdrds <-  readLines(file.choose())

### Positive word cloud ###
pos.matches <- match(names(w_sub1), pos.words)
pos.matches <- !is.na(pos.matches)
freq_pos <- w_sub1[pos.matches]
names <- names(freq_pos)
windows()
wordcloud(names, freq_pos, scale=c(4,1), colors = brewer.pal(8,"Dark2"))


### Matching Negative words ###
neg.matches <- match(names(w_sub1), neg.words)
neg.matches <- !is.na(neg.matches)
freq_neg <- w_sub1[neg.matches]
names <- names(freq_neg)
windows()
wordcloud(names, freq_neg, scale=c(4,.5), colors = brewer.pal(8, "Dark2"))
