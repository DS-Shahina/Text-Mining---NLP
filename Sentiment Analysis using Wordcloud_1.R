# Social Media Extraction and Wordcloud

install.packages("twitteR")
library(twitteR)

install.packages("ROAuth")
library(ROAuth)

install.packages("base64enc")
library(base64enc)

install.packages("httpuv")
library(httpuv)

cred <- OAuthFactory$new(consumerKey='nmGVSdznbiWBTpXlMI',
                         consumerSecret='uEG4QRk4xMmtPgr76ptEX4EXb7ZqdrsUcsLdjdMTKhB7dovyr5',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")

setup_twitter_oauth("nmGVSdznbiW5TpXlMI", 
                    "uEG4QRk4xMmtPqdrsUcsLdjdMTKhB7dovyr5",
                    "2410408194-w8x1LgT3Bsh0MFMKGw585RNyoBsw",
                    "5J1TL7Mjgw2a5BIHCRB7i7YQHvHGvy0QgOX")

Tweets <- userTimeline('TeamMessi', n = 1000)

TweetsDF <- twListToDF(Tweets)

Tweetsraw <- TweetsDF$text 

write.csv(Tweetsraw, "Tweets.csv")
getwd() #get working directory

x = readLines(file.choose()) 

install.packages("stringr")
library(stringr)

# build a corpus using the text mining (tm) package
install.packages("tm")
library(tm)

x = str_replace_all(x,"[^[:graph:]]", " ")
x1 = Corpus(VectorSource(x))  	# Constructs a source for a vector as input
x1 = tm_map(x1, tolower)		# converts to lower case
x1 = tm_map(x1, removePunctuation)	# removes punctuatuion marks
x1 = tm_map(x1, removeNumbers)		# removes numbers in the documents
x1 = tm_map(x1, removeWords, c(stopwords('english')))
x1 = tm_map(x1, stripWhitespace) 	# removes white space

install.packages("wordcloud")
library(wordcloud)
wordcloud(x1, min.freq = 10, random.order = FALSE, colors="blue")

###################################################################
######### Amazon Reviews Extraction ###########
install.packages("rvest")
install.packages("XML")
install.packages("magrittr")

library(rvest)
library(XML)
library(magrittr)

######### Amazon URL ###########
aurl <- "https://www.amazon.in/OnePlus-Midnight-Black-128GB-Storage/product-reviews/B07DJHY82F/ref=cm_cr_getr_d_paging_btm_prev_1?showViewpoints=1&pageNumber"
# without page number
amazon_reviews <- NULL

# for only 20 pages
for (i in 1:20){
     murl <- read_html(as.character(paste(aurl,i,sep="=")))
     rev <- murl %>% html_nodes(".review-text") %>% html_text()
     amazon_reviews <- c(amazon_reviews,rev)
}

?html_nodes
?html_text

write.table(amazon_reviews,"Oneplus6T.txt")
getwd()

##################################
#### Sentiment Analysis ####
txt <- amazon_reviews

str(txt)
length(txt)
View(txt)

# install.packages("tm")
library(tm)

# Convert the character data to corpus type
x <- Corpus(VectorSource(txt))

inspect(x[1])

x <- tm_map(x, function(x) iconv(enc2utf8(x), sub='byte'))
?tm_map

# Data Cleansing
x1 <- tm_map(x, tolower)
inspect(x1[1])

x1 <- tm_map(x1, removePunctuation)
inspect(x1[1])

inspect(x1[5])
x1 <- tm_map(x1, removeNumbers)
inspect(x1[1])

x1 <- tm_map(x1, removeWords, stopwords('english'))
inspect(x1[1])

# striping white spaces 
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])

# Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x1)
tdm
dtm <- t(tdm) # transpose
dtm <- DocumentTermMatrix(x1)

# To remove sparse entries upon a specific value
corpus.dtm.frequent <- removeSparseTerms(tdm, 0.99) 
?removeSparseTerms

tdm <- as.matrix(tdm)
dim(tdm)

tdm[1:20, 1:20]

inspect(x[1])

# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 65)
w_sub

barplot(w_sub, las=2, col = rainbow(30))

# Term phone repeats maximum number of times
x1 <- tm_map(x1, removeWords, c('phone','oneplus','will'))
x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x1)
tdm

tdm <- as.matrix(tdm)
tdm[100:109, 1:20]

# Bar plot after removal of the term 'phone'
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 50)
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
?wordcloud

windowsFonts(JP1 = windowsFont("MS Gothic"))
par(family = "JP1")
wordcloud(x1, scale= c(2,0.5))
?windowsFonts

############# Wordcloud2 ###############

installed.packages("wordcloud2")
library(wordcloud2)

w1 <- data.frame(names(w_sub), w_sub)
colnames(w1) <- c('word', 'freq')

wordcloud2(w1, size=0.3, shape='circle')
?wordcloud2

wordcloud2(w1, size=0.3, shape = 'triangle')
wordcloud2(w1, size=0.3, shape = 'star')


#### Bigram ####
library(RWeka)
library(wordcloud)

minfreq_bigram <- 2
bitoken <- NGramTokenizer(x1, Weka_control(min = 2, max = 2))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq, decreasing = TRUE), ]

wordcloud(sort_two$bitoken, sort_two$Freq, random.order = F, scale = c(2, 0.35), min.freq = minfreq_bigram, colors = brewer.pal(8, "Dark2"), max.words = 150)

#####################################
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

###################################
### Comparing text using wordclouds

# files <- DirSource("C:/data/speeches/")
data <- Corpus(DirSource("C:/Datasets_BA/360DigiTMG/DS_India/360DigiTMG DS India Module wise PPTs/Module 18 Text Mining_Natural Language Processing (NLP)/R Code/speeches/"))

data <- tm_map(data, content_transformer(tolower))
data <- tm_map(data, removePunctuation)
data <- tm_map(data, removeNumbers)
data <- tm_map(data, removeWords, stopwords("english"))

data <- tm_map(data, removeWords,c("applause","Applause","APPLAUSE",
                               "And","But","will","must"))

data <- TermDocumentMatrix(data)
data <- as.matrix(data)

colnames(data) <- c("bush","obama")

windows()
comparison.cloud(data, max.words = 250, title.size = 2,
                 colors = brewer.pal(3,"Set1"))
