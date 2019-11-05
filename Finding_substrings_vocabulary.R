library(plyr)
library(microbenchmark)
library(dplyr)

# Besede s sodim številom èrk
# besede z lihim številom èrk

# razliène zaèetne in konène pozicije
# kos -> ko, os. in ne samo kot: kos -> ko ?

#èe je iskalni niz daljši od besede:
# beseda je: os -> iskalni niz: "kos"

# shranjevanje rezultatov v df
# branje iz df-ja

setwd("C:\\Users\\Tomaz\\Documents\\06-SQL\\5-BlogPosts\\72")
#besede
datoteka = 'Slovenian_Vocabulary_Short.txt'

#x3 <- read.csv(file=datoteka, sep=',', header=FALSE)
x3 <- readLines(datoteka)



str_len2 <- function(x) {
  laply(seq(1,nchar(x),1), function(i) substr(x, i, i+1)) #i+1 = dolžina 2; i+2 =  dolžina 3;
}

str_len3 <- function(x) {
  laply(seq(1,nchar(x),1), function(i) substr(x, i, i+2)) #i+1 = dolžina 2; i+2 =  dolžina 3;
}

str_len4 <- function(x) {
  laply(seq(1,nchar(x),1), function(i) substr(x, i, i+3)) #i+1 = dolžina 2; i+2 =  dolžina 3;
}

str_len5 <- function(x) {
  laply(seq(1,nchar(x),1), function(i) substr(x, i, i+4)) #i+1 = dolžina 2; i+2 =  dolžina 3;
}

#prazni vektorji
vektor_len2 <- character(0)
vektor_len3 <- character(0)
vektor_len4 <- character(0)
vektor_len5 <- character(0)



for (i in 1:length(x3)) {
  beseda = x3[i]  
  vektor_len2 <- c(vektor_len2, str_len2(beseda))
  vektor_len3 <- c(vektor_len3, str_len3(beseda))
  vektor_len4 <- c(vektor_len4, str_len4(beseda))
  vektor_len5 <- c(vektor_len5, str_len5(beseda))
  print(i)
}


#clean
# èe se odloèimo za pucanje besed krajše od iskalnega niza

res_len2 <- as.data.frame(table(vektor_len2))
res_len3 <- as.data.frame(table(vektor_len3))
res_len4 <- as.data.frame(table(vektor_len4))
res_len5 <- as.data.frame(table(vektor_len5))


#changing types
res_len2$vektor_len2 <- as.character(res_len2$vektor_len2)
res_len3$vektor_len3 <- as.character(res_len3$vektor_len3)
res_len4$vektor_len4 <- as.character(res_len4$vektor_len4)
res_len5$vektor_len5 <- as.character(res_len5$vektor_len5)


# getting the lenghts
res_len2 %>%
  filter(nchar(res_len2$vektor_len2) == 2) -> res_len2A 

res_len3 %>%
  filter(nchar(res_len3$vektor_len3) == 3) -> res_len3A 

res_len4 %>%
  filter(nchar(res_len4$vektor_len4) == 4) -> res_len4A 

res_len5 %>%
  filter(nchar(res_len5$vektor_len5) == 5) -> res_len5A 


# Removing special chars. e.g.: , . ( ) / \ etc.
## ToDo: ... 
# ending here...

head(res_len2A[order(-res_len2A$Freq),],15)
head(res_len3A[order(-res_len3A$Freq),],15)
head(res_len4A[order(-res_len4A$Freq),],15)
s <- head(res_len5A[order(-res_len5A$Freq),],15)



#finding words from substrings

s$vektor_len5 <- as.factor(s$vektor_len5)
s$Freq <- as.numeric(s$Freq)
library(ggplot2)

ggplot(s, aes(x=Freq)) + geom_histogram()
