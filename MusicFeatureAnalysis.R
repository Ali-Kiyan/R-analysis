#installing packages
#for string manipulation
install.packages("stringr")
#for k-means clustering algorithm
install.packages("cluster")
#for filetring based on string in datasets 
install.packages("dplyr")
#for plotting 
install.packages("psych")
install.packages("ggplot2")
#using respective packages
library(stringr)  
library(cluster)
library(dplyr)
library(psych)
library(ggplot2)
#reading data set 
music <- read.csv("music.csv", header=T) 
#selecting musiclly significant features for analysis
music_cleaned <- music[,c(1, 7, 9, 10, 11, 18, 24,26, 28, 29, 30, 32)]
#check if there is any NA in the dataset 
any(is.na(music_cleaned))
#checking the structure and dimention of the dataset 
dim(music_cleaned)
str(music_cleaned)

#getting the number  of NAs 
sum(is.na(music_cleaned$song.hotttnesss))

#checking the number of missing values for each column
sum(is.na(music_cleaned$artist.hotttnesss))
sum(is.na(music_cleaned$bars_start))
sum(is.na(music_cleaned$beats_start))
sum(is.na(music_cleaned$duration))
sum(is.na(music_cleaned$end_of_fade_in))
sum(is.na(music_cleaned$loudness))
sum(is.na(music_cleaned$song.hotttnesss))
sum(is.na(music_cleaned$start_of_fade_out))
sum(is.na(music_cleaned$tatums_start))
sum(is.na(music_cleaned$tempo))
sum(is.na(music_cleaned$time_signature))
sum(is.na(music_cleaned$terms))


#using stringr and dplyr retrun a separate dataset if the track has one of the 5 main genre type in its terms column 
#Rock dataset
contains_rock <- music_cleaned %>% 
  filter(str_detect(music_cleaned$terms, "rock"))
nrow(contains_rock)
#Jazz dataset
contains_Jazz <- music_cleaned %>% 
  filter(str_detect(music_cleaned$terms, "jazz"))
nrow(contains_Jazz)
#Pop dataset 
contains_Pop <- music_cleaned %>% 
filter(str_detect(music_cleaned$terms, "pop"))
nrow(contains_Pop)
#classic dataset
contains_Classic <- music_cleaned %>% 
  filter(str_detect(music_cleaned$terms, "classic"))
nrow(contains_Classic)
#country dataset
contains_Country <- music_cleaned %>% 
  filter(str_detect(music_cleaned$terms, "country"))
nrow(contains_Country)


#calculate the mean for each main genres 


Rock_Song_hotness_mean <- mean(contains_rock$song.hotttnesss, na.rm = TRUE)


Jazz_Song_hotness_mean <- mean(contains_Jazz$song.hotttnesss, na.rm = TRUE)


Pop_Song_hotness_mean <- mean(contains_Pop$song.hotttnesss, na.rm = TRUE)


Classic_Song_hotness_mean <- mean(contains_Classic$song.hotttnesss, na.rm = TRUE)


Country_Song_hotness_mean <- mean(contains_Country$song.hotttnesss, na.rm = TRUE)


#3.impute the mean to every NA row
length <- nrow(music_cleaned)


#for Rock Tracks 
for(i in 1:length) 
{
  if(is.na(music_cleaned$song.hotttnesss[i]))
  {
    if(grepl("rock", music_cleaned$terms[i]))
      {
         music_cleaned$song.hotttnesss[i] <- Rock_Song_hotness_mean
      }
  }
}
#remaining NA rows 
#sum(is.na(music_cleaned))
#for Jazz Tracks
for(i in 1:length) 
{
  if(is.na(music_cleaned$song.hotttnesss[i]))
  {
    if(grepl("jazz", music_cleaned$terms[i]))
    {
      music_cleaned$song.hotttnesss[i] <- Jazz_Song_hotness_mean
    }
  }
}
#remaining NA rows 
sum(is.na(music_cleaned))
#for POP Tracks

#for(i in 1:length) 
#{
#  if(is.na(music_cleaned$song.hotttnesss[i]))
#  {
#    if(grepl("pop", music_cleaned$terms[i]))
#    {
#     music_cleaned$song.hotttnesss[i] <- Pop_Song_hotness_mean
#    }
#  }
#}
#remaining NA rows 
#sum(is.na(music_cleaned))
#for Classical Tracks
for(i in 1:length) 
{
  if(is.na(music_cleaned$song.hotttnesss[i]))
  {
    if(grepl("classic", music_cleaned$terms[i]))
    {
      music_cleaned$song.hotttnesss[i] <- Classic_Song_hotness_mean
    }
  }
}
#remaining NA rows 
sum(is.na(music_cleaned))
#for country Tracks
for(i in 1:length) 
{
  if(is.na(music_cleaned$song.hotttnesss[i]))
  {
    if(grepl("country", music_cleaned$terms[i]))
    {
      music_cleaned$song.hotttnesss[i] <- Country_Song_hotness_mean
    }
  }
}
#remaining NA rows 
#sum(is.na(music_cleaned))
#getting the number  of NAs 
sum(is.na(music_cleaned$song.hotttnesss))


#eliminating NA left in  the dataset
music_cleaned <- music_cleaned[complete.cases(music_cleaned),]
#checking the number of zero values for the column when zero value is meaningless
sum(music_cleaned$loudness == 0)
sum(music_cleaned$tempo ==0)
#removing zeros in the rows
#music_cleaned <- music_cleaned[-which(music_cleaned$song.hotttnesss == 0),] 
#using subset to eliminate zeros (alternative to perivious line)
#music_cleaned <- subset(music_cleaned,song.hotttnesss!=0)
music_cleaned <- subset(music_cleaned,tempo!=0)
music_cleaned <- subset(music_cleaned,loudness!=0)
#num of complete case (without NA)
sum(as.numeric(complete.cases(music_cleaned)))

head(music_cleaned)
#getting the pair of clusters
pairs(music_cleaned)
#finding correlation in pairs of cluster with psych package as a better alternative to previous line
pairs.panels(music_cleaned)
#scatter plot for a 3 most influentional attributes for song popularity (checking data)
qplot(music_cleaned$artist.hotttnesss, music_cleaned$song.hotttnesss)
qplot(music_cleaned$tempo, music_cleaned$song.hotttnesss)
qplot(music_cleaned$loudness, music_cleaned$song.hotttnesss)

#backing up the dataset 
music_cleaned_backup <- music_cleaned
music_cleaned_backup$terms <- as.factor(music_cleaned_backup$terms) 
str(music_cleaned_backup)
#forming mainGenre dataset containing 5 main genre
mainGenre <- rbind(contains_rock, contains_Jazz, contains_Classic, contains_Country)
dim(mainGenre)

#song popularity histogram in different genre
qplot(song.hotttnesss, data = mainGenre, fill = terms)
#Drawing plot for highly related attributes to song popularity in 5 main genre
qplot(song.hotttnesss, loudness, data = mainGenre , color = mainGenre$terms)
qplot(song.hotttnesss, tempo, data = mainGenre , color = mainGenre$terms)

#Drawing plot for highly related attributes to song popularity in each genre
qplot(song.hotttnesss, loudness, data = contains_rock , color = contains_rock$terms)
qplot(song.hotttnesss, loudness, data = contains_Jazz , color = contains_Jazz$terms)
qplot(song.hotttnesss, loudness, data = contains_Classic , color = contains_Classic$terms)
qplot(song.hotttnesss, loudness, data = contains_Country , color = contains_Country$terms)
qplot(song.hotttnesss, loudness, data = contains_Pop , color = contains_Pop$terms)

qplot(song.hotttnesss, tempo, data = contains_rock , color = contains_rock$terms)
qplot(song.hotttnesss, tempo, data = contains_Jazz , color = contains_Jazz$terms)
qplot(song.hotttnesss, tempo, data = contains_Classic , color = contains_Classic$terms)
qplot(song.hotttnesss, tempo, data = contains_Country , color = contains_Country$terms)
qplot(song.hotttnesss, loudness, data = contains_Pop , color = contains_Pop$terms)


#removing the nominal value
music_cleaned <- music_cleaned[,-c(11)]

#export fully cleaned dataset to a csv file 
write.csv(music_cleaned, file='musicCleaned.csv')

#normalizing with range between -1 and 1 (for more accurate)
col <- ncol(music_cleaned)
newmin = -1
newmax= 1
#initializing
nrml_music <- 0
#normalizing
for(j in 1:col){
  oldmin <- min(music_cleaned[j])
  oldmax <- max(music_cleaned[j])
  current <- music_cleaned[j]
  temp <- ((current-oldmin)/(oldmax-oldmin))*(newmax-newmin) + newmin
  temp <- as.data.frame(temp)
  nrml_music <- cbind(nrml_music, data.frame(temp))
}
#removing extra columns
nrml_music <- nrml_music[,-c(1)]
write.csv(nrml_music, file='normalizedMusic.csv')
#check if the range is correct
min(nrml_music$song.hotttnesss)
max(nrml_music$song.hotttnesss)
#standardisation with substraction of mean and division by standard deviation with scale function (as an alternative to normalization)
#less accurate in this case
#means = apply(music_cleaned, 2, mean)
#sds = apply(music_cleaned, 2, sd)
#nrml_music = scale(music_cleaned, center = means, scale=sds)
#calculating the distance matrix
distance = dist(nrml_music)

#clustring the normalized dataset into three cluster with k-means package
kc <- kmeans(nrml_music,4)
#checking the optimal number of clusters with using within sum of square
mydata <- nrml_music[1:3]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)


#center of each cluster
kc$centers

#distribution of terms(genre in clusters)
table(music_cleaned$tempo, kc$cluster)


#results
plot(kc$centers)
# 4 clusters for the relation between artist popularity and song popularity 
plot(music_cleaned$song.hotttnesss ~ music_cleaned$artist.hotttnesss, data = music_cleaned,col=kc$cluster)
# 7 clusters for the relationship between popularity of the song and its tempo 
plot(music_cleaned$song.hotttnesss ~ music_cleaned$tempo, data = music_cleaned,col=kc$cluster)
# 6 clusters for the relationship between popularity of the song and its loudness
plot(music_cleaned$song.hotttnesss ~ music_cleaned$loudness, data = music_cleaned,col=kc$cluster)
#adding each song cluster in song popularity/artist papularity analysis
music_cleaned$cluster <- kc$cluster
#finding cluster with highest song popularity  
plot(music_cleaned$song.hotttnesss ~ music_cleaned$artist.hotttnesss, data = music_cleaned,col=music_cleaned$cluster==3)
#songs which are in most popular cluster and have more than 0.5 song popularity will 1 value in popularity col and 
#songs which have less than 0.5 and are not belong to overlapping cluster will get 0 in popularity col (less popular songs)
music_cleaned$popularity <- ifelse(music_cleaned$cluster == 4 , ifelse(music_cleaned$song.hotttnesss > 0.5, 1, 3),ifelse(music_cleaned$cluster!=3,0,3))
#deleting the overlapping cluster
music_cleaned <- subset(music_cleaned,popularity!=3)
#exporting
write.csv(music_cleaned, file='musicCleanedWithPopularity.csv')

plot(music_cleaned$artist.hotttnesss, music_cleaned$song.hotttnesss,col=music_cleaned$popularity==1)
plot(music_cleaned$artist.hotttnesss, music_cleaned$song.hotttnesss,col=music_cleaned$popularity==0)
#package for using decision tree
library(party) 
#changing to factor
music_cleaned$popularity <-as.factor(music_cleaned$popularity)
#drawing the decision tree
popularity_tree <- ctree(popularity ~ tempo + loudness + artist.hotttnesss ,music_cleaned)
plot(popularity_tree) 
str(music_cleaned)

tab<-table(predict(popularity_tree), music_cleaned$popularity)
tab
sum(diag(tab))/sum(tab)
