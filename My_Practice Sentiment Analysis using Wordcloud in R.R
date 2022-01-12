# Social Media Extraction and Wordcloud

install.packages("twitteR")
library(twitteR)

install.packages("ROAuth") # Authentication to login twitter, third party application
library(ROAuth)

install.packages("base64enc") # R can read those web pages
library(base64enc)

install.packages("httpuv") # https
library(httpuv)

#I need credentials to login the twitter

#Developer account credentials
cred <- OAuthFactory$new(consumerKey='nmGVSdznbiWBTpXlMI',
                         consumerSecret='uEG4QRk4xMmtPgr76ptEX4EXb7ZqdrsUcsLdjdMTKhB7dovyr5',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

#using this credential to login the twitter account
save(cred, file="twitter authentication.Rdata")
#once you saved this credentials, next time you login the twitter you can use same credentials

load("twitter authentication.Rdata")

setup_twitter_oauth("nmGVSdznbiW5TpXlMI", 
                    "uEG4QRk4xMmtPqdrsUcsLdjdMTKhB7dovyr5",
                    "2410408194-w8x1LgT3Bsh0MFMKGw585RNyoBsw",
                    "5J1TL7Mjgw2a5BIHCRB7i7YQHvHGvy0QgOX")
# enter 1 in console next time it will not ask
# Stablish a conection from R to Twitter
#1st line is consumerKey, 2nd is consumerSecret key, 3rd is Access token key, 4th is Access token secret key.

#now extract the data from twitter
Tweets <- userTimeline('sachin_rt', n = 1000) # "TeamMessi" twitter handler name
# n = 1000, 1000 tweets to extract

TweetsDF <- twListToDF(Tweets)

Tweetsraw <- TweetsDF$text 


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
# Web page to extract the data - review page
# without page number

# to store that review we have a variable, initially it is null
amazon_reviews <- NULL

#5,666 global ratings divided by 10 we get 566 pages
# for only 20 pages- each page 10 reviews = 200 reviews we get
for (i in 1:20){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>% html_nodes(".review-text") %>% html_text() # that extract content from webpage (html source page)
  amazon_reviews <- c(amazon_reviews,rev)
}

# %>% follow left to right - pipe operator or pipe function
?html_nodes
?html_text

write.table(amazon_reviews,"Oneplus6T.txt")
getwd()


##################################
#### Sentiment Analysis ####
txt <- amazon_reviews # 200 reviews extracted, store in txt variable

str(txt)
length(txt)
View(txt)

# install.packages("tm")
library(tm)

typeof(txt)
# Convert the character data to corpus type
x <- Corpus(VectorSource(txt))

inspect(x[1]) #each row is 1 column

x <- tm_map(x, function(x) iconv(enc2utf8(x), sub='byte')) # inline function (single line)
?tm_map # Transformations on Corpus data, tm didn't understand all character set so it converts into specific charater sets (enc2utf8)
#Unicode (or Universal Coded Character Set) Transformation Format - 8-bit 

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

tdm <- as.matrix(tdm) # convert tdm into matrix so that we can see
dim(tdm) #3010 terms out of 200 reviews # it's a huge matrix

tdm[1:20, 1:20] # 20 rows, 20 columns, you can not see tdm it will crash out R So take a sample

inspect(x[1]) # if you want yo see what is this words

# Bar plot
w <- rowSums(tdm)
w
w[1:10]
w_sub <- subset(w, w >= 65) # repeat atlest 65 times, we can do randomly
w_sub

barplot(w_sub, las=2, col = rainbow(30))

# Term phone repeats maximum number of times
x1 <- tm_map(x1, removeWords, c('phone','oneplus','also')) # very popular terms and those which is not required
x1 <- tm_map(x1, stripWhitespace) # after removing words empty space is created

#again generate termdocument matrix
tdm <- TermDocumentMatrix(x1) # previously it was 3010, now 3007
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

w_sub1 <- sort(rowSums(tdm), decreasing = TRUE) # taking all
head(w_sub1)

wordcloud(words = names(w_sub1), freq = w_sub1) # all words are considered

# better visualization
wordcloud(words = names(w_sub1), freq = w_sub1, random.order=F, colors=rainbow(30), scale = c(2,0.5), rot.per = 0.4) # based on frequency color is display-30 different shades, scale is range of font size, rotation percentage - horizontal and vertical words written, 40% vertically 60% bydefault horizontally 
windows() # for better resolution we create window
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
library(rJava) # Install java software then on;y Rweka Works, java, visualstudio for compatibility
library(RWeka)
library(wordcloud)

minfreq_bigram <- 2 # 2 words
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
pos.matches <- !is.na(pos.matches) #capture only not na
freq_pos <- w_sub1[pos.matches]# it consider only true value
names <- names(freq_pos) # name in another variable
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
data <- Corpus(DirSource("D:/C DRIVE-SSD DATA backup 15-12-2020/Desktop/360digitmg material/Text Mining - NLP/speeches/"))

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



