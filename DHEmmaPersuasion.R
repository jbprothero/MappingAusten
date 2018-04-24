
# Load in libraries for European Map Plotting
library(ggmap)
library(mapproj)

# Read in data
lang = read.csv("Language.csv", header=T)
set = read.csv("Settlement.csv", header=T)
nlang = length(lang[1,]) - 4
nset = length(set[1,]) - 4

# Plot a summary of etymology info for locations in emma & persuasion
plot(c(1:nlang, 1:nlang), c(rep(1.2, nlang), rep(0.8, nlang)), 
     ylim=c(.6, 1.4), axes=F, xlab='', ylab='',
     cex = c(apply(lang[lang$Emma==1,5:(nlang+4)], 2, sum), 
             apply(lang[lang$Persuasion==1,5:(nlang+4)], 2, sum))*.4,
     main='Location Etymology in Emma & Persuasion')
axis(1, 1:nlang, colnames(lang)[5:(nlang+4)], las=3, cex.axis=0.7)
axis(2, c(1.2, .8), colnames(lang)[2:3], las=2, cex.axis=0.7)

# Plot a summary of settlement info for locations in emma & persuasion
plot(c(1:nset, 1:nset), c(rep(1.2, nset), rep(0.8, nset)), 
     ylim=c(.6, 1.4), axes=F, xlab='', ylab='',
     cex = c(apply(set[set$Emma==1,5:(nset+4)], 2, sum), 
             apply(set[set$Persuasion==1,5:(nset+4)], 2, sum))*.5,
     main='Settlement History in Emma & Persuasion')
axis(1, 1:nset, colnames(set)[5:(nset+4)], las=3, cex.axis=0.7)
axis(2, c(1.2, .8), colnames(set)[2:3], las=2, cex.axis=0.7)

# Extract Geographical info (longitude/latitude geocodes) from Google Maps for map plotting
map=get_map(location="Europe", zoom=3)

#langlatlon <- geocode(c('Rome, Italy', 'Wales, UK', 'Cumbria, UK', 'Berlin, Germany', 'Norwich, UK', 'Berkhamsted, UK', 
#                        'Copenhagen, Denmark','Madrid, Spain', 'Dublin, Ireland', 'Oslo, Norway', 'Paris, France', 
#                        'Athens, Greece','Lisbon, Portugal', 'Istanbul, Turkey', 'Nassau, Bahamas'))
#langlatlon[nlang,] = c(0, 64) #correction to include Bahamas in a European Map
#
#setlatlon <- geocode(c('Rome, Italy', 'Normandy, France', 'Cumbria, UK', 'Brussels, Belgium', 'Brittany, France', 
#                       'Paris, France', 'Copenhagen, Denmark', 'Istanbul, Turkey', 'Rabat, Morocco', 
#                       'Beirut, Lebanon', 'Jerusalem, Israel','Tehran, Iran', 'Warsaw, Poland', 'Madrid, Spain', 
#                       'Oslo, Norway', 'London, England'))

# The latitude/longidute information is stored in spreadsheets to minimize querying the Google API
langlatlon = read.csv("langlatlon.csv", header=T)
setlatlon = read.csv("setlatlon.csv", header=T)

# Calculate pertinenet summations of the data
emmalang = apply(lang[lang$Emma==1,5:(nlang+4)], 2, sum)
perslang = apply(lang[lang$Persuasion==1,5:(nlang+4)], 2, sum)
totlang = apply(lang[,5:(nlang+4)], 2, sum)
emmaset = apply(set[set$Emma==1,5:(nset+4)], 2, sum)
persset = apply(set[set$Persuasion==1,5:(nset+4)], 2, sum)
totset = apply(set[,5:(nset+4)], 2, sum)


# Plot each subset on the European Map
ggmap(map) + 
  scale_x_continuous(limits = c(-15, 55), expand = c(0,0))+
  scale_y_continuous(limits = c(30, 65), expand = c(0,0))+
  scale_size_continuous(range = c(min(emmalang)*2, max(emmalang)), 
                        breaks=sort(unique(emmalang)), name="Frequency")+
  geom_point(aes(x = lon, y=lat, size=emmalang), color='red', data=langlatlon, alpha=0.7)+
  ggtitle("Location Etymology in Emma")
  
ggmap(map) + 
  scale_x_continuous(limits = c(-15, 55), expand = c(0,0))+
  scale_y_continuous(limits = c(30, 65), expand = c(0,0))+
  scale_size_continuous(range = c(min(perslang)*2, max(perslang)), 
                        breaks=sort(unique(perslang)), name="Frequency")+
  geom_point(aes(x = lon, y=lat, size=perslang), color='blue', data=langlatlon, alpha=0.7)+
  ggtitle("Location Etymology in Persuasion")

ggmap(map) + 
  scale_x_continuous(limits = c(-15, 55), expand = c(0,0))+
  scale_y_continuous(limits = c(30, 65), expand = c(0,0))+
  scale_size_continuous(range = c(min(totlang)*2, max(totlang)), 
                        breaks=sort(unique(totlang)), name="Frequency")+
  geom_point(aes(x = lon, y=lat, size=totlang), color='purple', data=langlatlon, alpha=0.7)+
  ggtitle("Location Etymology in Emma & Persuasion")

ggmap(map) + 
  scale_x_continuous(limits = c(-15, 55), expand = c(0,0))+
  scale_y_continuous(limits = c(30, 65), expand = c(0,0))+
  scale_size_continuous(range = c(min(emmaset)*2, max(emmaset)), 
                        breaks=sort(unique(emmaset)), name="Frequency")+
  geom_point(aes(x = lon, y=lat, size=emmaset), color='red', data=setlatlon, alpha=0.7)+
  ggtitle("Settlement History in Emma")

ggmap(map) + 
  scale_x_continuous(limits = c(-15, 55), expand = c(0,0))+
  scale_y_continuous(limits = c(30, 65), expand = c(0,0))+
  scale_size_continuous(range = c(min(persset)*2, max(persset)), 
                        breaks=sort(unique(persset)), name="Frequency")+
  geom_point(aes(x = lon, y=lat, size=persset), color='blue', data=setlatlon, alpha=0.7)+
  ggtitle("Settlement History in Persuasion")

ggmap(map) + 
  scale_x_continuous(limits = c(-15, 55), expand = c(0,0))+
  scale_y_continuous(limits = c(30, 65), expand = c(0,0))+
  scale_size_continuous(range = c(min(totset)*2, max(totset)), 
                        breaks=sort(unique(totset)), name="Frequency")+
  geom_point(aes(x = lon, y=lat, size=totset), color='purple', data=setlatlon, alpha=0.7)+
  ggtitle("Settlement History in Emma & Persuasion")
