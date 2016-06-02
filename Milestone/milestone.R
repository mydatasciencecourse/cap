library(tm)
library(stringi)
library(slam)

df<-data.frame(text=readLines("final/en_US/en_US.news.txt"))

corpus<-VCorpus(DataframeSource(df), readerControl = list(language="english"))

#corpusCleaned<-tm_map(corpus, stemDocument)
#corpusCleaned<-tm_map(corpusCleaned, removeWords, stopwords("english"))
#corpusWithoutPunctuation<-tm_map(corpus, removePunctuation)

createNGram<-function(corpus, n){
  regex<-paste("\\b", paste(Map(function(arg){"\\w+"}, seq(1:n)), collapse = " "), sep = "")
  print(regex)
  
  DocumentTermMatrix(corpus, control = list(tokenize=function(x){
    x<-gsub(" {2,}", " ", str_trim(x))
    
    stri_match_all_regex(x, paste("(?=(", regex, "))", sep=""))[[1]][,2]
  }))
}

freq1 <- col_sums(createNGram(corpus, 1), na.rm=T)
s1<-sum(freq1)
freq1<-freq1[order(freq1, decreasing = T)]

data<-head(freq1, n=700)
qplot(names(data), data/s1) + geom_bar(position = "dodge", stat="identity")+coord_flip() + scale_x_discrete(limits=names(data)[order(data, decreasing=F)])

s<-0;col<-c();index<-1;while(s<0.5){col<-c(col, names(freq1)[index]);s<-s+freq1[index]/s1;index<-index+1}

head(goodTuringProportions(freq1))