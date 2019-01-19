
##############################
##  pres21.R: Code for Week 2, Day #1
##  Note: Corresponds to pres21.pdf.
##        See pres21.pdf for references.
##  Author: Marshall A. Taylor
##############################

### BEGIN ###

#Set working directory
setwd("set_working_directory")


#Install and load packages
install.packages("rtweet")
install.packages("ggplot2")
install.packages("maps")
library(rtweet)
library(ggplot2)
library(maps)


#Authenticate yourself
create_token(app = "app_name_here",
             consumer_key = "con_key_here",
             consumer_secret = "con_secret_here",
             access_token = "access_token_here",
             access_secret = "access_secret_here")


#Browse the package
browseVignettes("rtweet")


#Scrape some data
irish.data <- search_tweets("#GoIrish", n=2000, 
                            include_rts = F, lang = "en")


#What data are collected?
names(irish.data)


#First 20 tweets
head(irish.data$text, 20)


#Frequency of #GoIrish over time
ts_plot(irish.data, "5 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets using #GoIrish",
    subtitle = "Tweets aggregated using five-hour intervals",
    caption = "\nSource: Plot code from Kearney 2019."
  )


#Get data with geocode tags limited tothe U.S.
irish.data2 <- search_tweets("#GoIrish", n=2000, 
                            include_rts = F, lang = "en",
                           geocode = lookup_coords("usa"))
irish.geo <- lat_lng(irish.data2)


#Plot geocoded data on a U.S. map
maps::map("state", lwd = .9, fill=T, col="#0C2340")
with(irish.geo, points(lng, lat, pch = 20, cex = .75, col = "#C99700"))


#Get some tweets from a particular profile
nd.women <- get_timeline("ndwbb", n = 200)


#How tweets are constructed/posted
ggplot(nd.women, aes(x=source, fill=source)) +
  geom_bar() +
  ylab("Frequency") + xlab("") +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        legend.title=element_text(face="bold")) +
  scale_fill_discrete(name="Source")


#Regional trends
chi.trends <- get_trends("Chicago")
head(chi.trends$trend, 4)


#Save your R objects
save.image("rtweet_objects.RData")

### END ###
