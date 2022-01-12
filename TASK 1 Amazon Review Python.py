"Task 1"

#pip install bs4
import requests   # Importing requests to extract content from a url
from bs4 import BeautifulSoup as bs # Beautifulsoup is for web scrapping...used to scrap specific content 
import re # regular expression - it is used for string manipulation

import matplotlib.pyplot as plt
from wordcloud import WordCloud # pip install wordcloud

# try to extract data from amazon
# creating empty reviews list 
oneplus_reviews=[]


for i in range(1,21): # 20 pages #url for 20 pages
  ip=[]  # empty list
  url="https://www.amazon.in/OnePlus-Display-Storage-4000mAH-Battery/product-reviews/B07HGJK535/ref=cm_cr_getr_d_paging_btm_next_3?ie=UTF8&reviewerType=all_reviews&pageNumber="+str(i) # loading reviews which we extracted - str(i) convert i into string
  response = requests.get(url) #requests to extract content from a url
  soup = bs(response.content,"html.parser")# creating soup object to iterate over the extracted content 
  reviews = soup.find_all("span",attrs={"class","a-size-base review-text review-text-content"})# Extracting the content under specific tags # span- entire span of website (entire content of htlm web page) capture the attribute -(class","a-size-base review-text review-text-content) and all the reviews are store in the reviews variable
  for i in range(len(reviews)): # 10 reviews for 1 page, till  20 pages
    ip.append(reviews[i].text)  
 
  oneplus_reviews=oneplus_reviews+ip  # adding the reviews of one page to empty list which in future contains all the reviews
#it should 200 reviews but it shows 150

# writng reviews in a text file 
with open("oneplus.txt","w",encoding='utf8') as output: #oneplus.txt is not in structured format so i can not load the data as dataframe , it saved in c drive in users, Admin
    output.write(str(oneplus_reviews))
  

with open("oneplus.txt","r",encoding='utf8') as sw:
    ip_rev_string = sw.read()
    
# Joinining all the reviews into single paragraph 
ip_rev_string = " ".join(oneplus_reviews)

import nltk
# from nltk.corpus import stopwords

# Removing unwanted symbols incase if exists
ip_rev_string = re.sub("[^A-Za-z" "]+"," ", ip_rev_string).lower() #in this special symbol replace with space after that convert everything into lower space
ip_rev_string = re.sub("[0-9" "]+"," ", ip_rev_string)

# words that contained in iphone XR reviews
ip_reviews_words = ip_rev_string.split(" ") # take each word separately , it is also called as Tokenization
# 24437 words are extracted

#TFIDF
from sklearn.feature_extraction.text import TfidfVectorizer # to normalize
vectorizer = TfidfVectorizer(ip_reviews_words, use_idf=True,ngram_range=(1, 3)) #unigram,bigram trigram
X = vectorizer.fit_transform(ip_reviews_words) #sparse matrix

with open("D:/C DRIVE-SSD DATA backup 15-12-2020/Desktop/360DigiTmg Assignment/Text Mining - NLP/stop.txt","r") as sw:
    stop_words = sw.read() #it's in a single string
    
stop_words = stop_words.split("\n") #571 stop words, "\n"- new line charater to do each word separately, and split converts string into list format also

stop_words.extend(["oneplus","mobile","time","android","phone","device","screen","battery","product","good","day","price"]) #cleaning unnecessary words
# now it's 583 words
ip_reviews_words = [w for w in ip_reviews_words if not w in stop_words] #take that word which is not stop word
#now 10464 words is there out of 24437 words

# Joinining all the reviews into single paragraph 
ip_rev_string = " ".join(ip_reviews_words)

# WordCloud can be performed on the string inputs.
# Corpus level word cloud

wordcloud_ip = WordCloud(
                      background_color='White',
                      width=1800,
                      height=1400
                     ).generate(ip_rev_string)

plt.imshow(wordcloud_ip)

# positive words # Choose the path for +ve words stored in system
with open("D:/C DRIVE-SSD DATA backup 15-12-2020/Desktop/360DigiTmg Assignment/Text Mining - NLP/positive-words.txt","r") as pos:
  poswords = pos.read().split("\n")

# Positive word cloud
# Choosing the only words which are present in positive words
ip_pos_in_pos = " ".join ([w for w in ip_reviews_words if w in poswords])

wordcloud_pos_in_pos = WordCloud(
                      background_color='White',
                      width=1800,
                      height=1400
                     ).generate(ip_pos_in_pos)
plt.figure(2)
plt.imshow(wordcloud_pos_in_pos)

# negative words Choose path for -ve words stored in system
with open("D:/C DRIVE-SSD DATA backup 15-12-2020/Desktop/360DigiTmg Assignment/Text Mining - NLP/negative-words.txt", "r") as neg:
  negwords = neg.read().split("\n")

# negative word cloud
# Choosing the only words which are present in negwords
ip_neg_in_neg = " ".join ([w for w in ip_reviews_words if w in negwords])

wordcloud_neg_in_neg = WordCloud(
                      background_color='black',
                      width=1800,
                      height=1400
                     ).generate(ip_neg_in_neg)
plt.figure(3)
plt.imshow(wordcloud_neg_in_neg)


# wordcloud with bigram
nltk.download('punkt')
from wordcloud import WordCloud, STOPWORDS

WNL = nltk.WordNetLemmatizer()

# Lowercase and tokenize
text = ip_rev_string.lower()

# Remove single quote early since it causes problems with the tokenizer.
text = text.replace("'", "")

tokens = nltk.word_tokenize(text) # this will give me words -  tokens
text1 = nltk.Text(tokens)

# Remove extra chars and remove stop words.
text_content = [''.join(re.split("[ .,;:!?‘’``''@#$%^_&*()<>{}~\n\t\\\-]", word)) for word in text1]

# Create a set of stopwords (set is no duplication)
stopwords_wc = set(STOPWORDS) # it's a predefined stop words that we upload through library wordcloud - 192 stopwords
customised_words = ['price', 'great'] # If you want to remove any particular word form text which does not contribute much in meaning

new_stopwords = stopwords_wc.union(customised_words) #set don't follow any order so we don't use append or extend.now it's 194

# Remove stop words
text_content = [word for word in text_content if word not in new_stopwords]

# Take only non-empty entries - remove empty entries
text_content = [s for s in text_content if len(s) != 0]

# Best to get the lemmas of each word to reduce the number of similar words
text_content = [WNL.lemmatize(t) for t in text_content]

nltk_tokens = nltk.word_tokenize(text)  
bigrams_list = list(nltk.bigrams(text_content)) # 8091 words
print(bigrams_list)


dictionary2 = [' '.join(tup) for tup in bigrams_list] # we convert it into dictionary, the tuple is converted into string you can say
print (dictionary2)

# Using count vectoriser to view the frequency of bigrams, How many times each word is repeated, or you can apply TfidfVectorizer in place of CountVectorizer
from sklearn.feature_extraction.text import CountVectorizer
vectorizer = CountVectorizer(ngram_range=(2, 2)) # range is 2, Only bigram
bag_of_words = vectorizer.fit_transform(dictionary2) #TDM OR DTM , word and the frequency
vectorizer.vocabulary_

sum_words = bag_of_words.sum(axis=0) # row wise , 7134 count
words_freq = [(word, sum_words[0, idx]) for word, idx in vectorizer.vocabulary_.items()]
words_freq =sorted(words_freq, key = lambda x: x[1], reverse=True)
print(words_freq[:100])

# Generating wordcloud
words_dict = dict(words_freq)
WC_height = 1000
WC_width = 1500
WC_max_words = 200 # maximum word is 200
wordCloud = WordCloud(max_words=WC_max_words, height=WC_height, width=WC_width, stopwords=new_stopwords)
wordCloud.generate_from_frequencies(words_dict)

plt.figure(4)
plt.title('Most frequently occurring bigrams connected by same colour and font size')
plt.imshow(wordCloud, interpolation='bilinear')
plt.axis("off")
plt.show()

