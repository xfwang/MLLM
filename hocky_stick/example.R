lilac.phenology <- read.table("http://faculty.washington.edu/eliezg/teaching/DataScience/Lecture9/LilacAnalysis/LilacPhenology.txt", header=TRUE)
lilac.phenology <- subset(lilac.phenology, YEAR < 2000)
lilac.locations <- read.csv("http://faculty.washington.edu/eliezg/teaching/DataScience/Lecture9/LilacAnalysis/LilacLocations.csv")
lp <- lilac.phenology
lp[lp == 999] <- NA
ll <- lilac.locations

ll$Nobs <- table(lp$STATION)[match(ll$ID, as.numeric(names(table(lp$STATION))))]

require(ggmap)
basemap1 <- get_map(location = c(-112, 38), maptype = "terrain", zoom=4)
ggmap(basemap1) + geom_point(data = ll, mapping = aes(x = Long, y = Lat, colour = Elev, size = Nobs))

Stations <- c(456624, 426357, 354147, 456974)
lilacs <- subset(lp, STATION %in% Stations)
lilacs <- lilacs[!is.na(lilacs$BLOOM),]

require(rstan)
require(coda)
require(plyr)

lm_stan <- stan_model(file = "/Users/Zhenyu/Desktop/hockystickmulti.stan")


l1 <- subset(lilacs, STATION == Stations[1] | STATION == Stations[2] | STATION == Stations[3] | STATION == Stations[4])
Year <- l1$YEAR
Bloom <- l1$BLOOM
STATION<-l1$STATION

STATION[STATION==354147]<-1
STATION[STATION==426357]<-2
STATION[STATION==456624]<-3
STATION[STATION==456974]<-4

Data <-  list(n=length(Year), x=Year, y=Bloom,nst=4,nsta=STATION)
lm.fit <- sampling(lm_stan, Data,control = list(adapt_delta = 0.99))

pairs(lm.fit)
