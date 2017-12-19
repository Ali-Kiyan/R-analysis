music <- read.csv("music.csv", header=T)
music_cleaned <- music[,c(7, 9, 11, 18, 24,26, 28, 29, 32)]
dim(music_cleaned)
music_cleaned <- music_cleaned[complete.cases(music_cleaned),]
dim(music_cleaned)
library(cluster)
head(music_cleaned)
pairs(music_cleaned)
plot(music_cleaned$song.hotttnesss, music_cleaned$loudness, data= music_cleaned)
dim(music_cleaned)

beats_start <- music_cleaned$beats_start
hotness <- music_cleaned$song.hotttnesss
oldmin <- min(beats_start)
oldmax <- max(beats_start)
newbeats_start <- ((beats_start-oldmin)/(oldmax-oldmin))*(newmax-newmin) + newmin
newbeats_start <- as.data.frame(newbeats_start)
newbeats_start <- cbind(newbeats_start,data.frame(hotness))
nrml_music <- newbeats_start[complete.cases(newbeats_start),]
#means = apply(music_cleaned, 2, mean)
#sds = apply(music_cleaned, 2, sd)
#nrml_music <- scale(music_cleaned, center = means, scale=sds)
#nrml_music 
#distance = dist(nrml_music)
#distance
kc <- kmeans(nrml_music,10)
kc
kc$centers 
plot(music_cleaned$beats_start ~ music_cleaned$song.hotttnesss, data = music_cleaned,col=kc$cluster)

