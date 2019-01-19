
##############################
##  pres22.R: Code for Week 2, Day #2
##  Note: Corresponds to pres22.pdf.
##        See pres22.pdf for references.
##  Author: Marshall A. Taylor
##############################

### BEGIN ###

#Set working directory
setwd("set_working_directory")


#Install and load packages
install.packages("rtimes")
install.packages("rtweet")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("reshape2")
install.packages("rvest")
install.packages("purrr")
install.packages("stringr")
install.packages("lubridate")
library(rtimes)
library(rtweet)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(rvest)
library(purrr)
library(stringr)
library(lubridate)


#Authenticate yourself
Sys.setenv(NYTIMES_AS_KEY = "api_key_here")


#Browse the package
browseVignettes("rtimes")


#Scrape some NYT article data
ny.articles <- as_search(q = '"Unite the Right"', begin_date = "20170513",
                         end_date = "20171231", all_results = T, sleep = 10)
nyarticle.data <- ny.articles$data


#What data are collected?
names(nyarticle.data)


#Take a look at some of the article data
head(nyarticle.data$headline.main, 10)


#Frequency of the phrase over time
nyarticle.data$time <- as.Date(nyarticle.data$pub_date) + 1
nyarticle.data$datetime <- as.POSIXct(nyarticle.data$time)

ts_plot(nyarticle.data, "1 day") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = expression(bold(paste("Frequency of ", bolditalic("NYT"), 
                                  ' Articles Mentioning "Unite the Right"'))),
    subtitle = "Tweets aggregated by week",
    caption = "\nSource: Plot code adapted from Kearney 2019."
  ) +
  xlab("August 11, 2017 - December 15, 2017")


#What news desk?
plot1 <- ggplot(nyarticle.data, aes(x=news_desk, fill=news_desk)) +
  geom_bar() +
  ylab("Frequency") + xlab("") +
  ylim(0, 25) +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        legend.title=element_text(face="bold", size = 6),
        legend.text = element_text(size = 6),
        legend.position = "bottom") +
  scale_fill_discrete(name="News Desk")


#What type of material?
plot2 <- ggplot(nyarticle.data, aes(x=type_of_material, fill=type_of_material)) +
  geom_bar() +
  ylab("Frequency") + xlab("") +
  ylim(0, 25) +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        legend.title=element_text(face="bold", size = 6),
        legend.text = element_text(size = 6),
        legend.position = "bottom") +
  scale_fill_discrete(name="Type of\nMaterial")


#Put the plots together
ggarrange(plot1, plot2, align = "h")


#What did the articles that come from the business news desk look like?
nyarticle.data$headline.main[nyarticle.data$news_desk=="Business"]


#Word count over time
nyarticle.data <- nyarticle.data[order(nyarticle.data$datetime),]

ggplot(nyarticle.data, aes(x=datetime, y=word_count)) +
  geom_line() +
  ylab("Word Count") + xlab("August 11, 2017 - December 15, 2017") +
  theme_bw()

ggplot(nyarticle.data, aes(x=datetime, y=word_count, label=headline.main)) +
  geom_line() +
  ylab("Word Count") + xlab("August 11, 2017 - December 15, 2017") +
  theme_bw() +
  geom_label_repel(aes(label = ifelse(word_count == 410 | 
                                        word_count == 1839 |
                                        word_count == 715, headline.main, "")),
                   nudge_y = 200, segment.alpha = .5,
                   arrow = arrow(length = unit(0.01, "npc"), 
                                 type = "closed", ends = "first"),
                   segment.color = "red")


#Prevalence of keywords (some code adapted from:
  #https://stackoverflow.com/questions/37731324/split-or-separate-uneven-unequal-strings-with-no-delimiter)
keyword.data <- data.frame(matrix(ncol = 2, nrow = 32))
colnames(keyword.data) <- c("web_url", "keywords2")

for (i in 1:32) {
  if (is.null(ny.articles$data$keywords[[i]]$value)) next
  extract1 <- as.data.frame(t(ny.articles$data$keywords[[i]]$value))
  extract2 <- ny.articles$data$web_url[[i]]
  united <- unite(extract1, col=keywords2)
  keyword.data[i,1] <- extract2
  keyword.data[i,2] <- united
}
rm(extract1, extract2, united)

nyarticle.data <- merge(nyarticle.data, keyword.data, by = "web_url", all.y = T)

test <- strsplit(nyarticle.data$keywords2, split = "_")
maxL <- max(sapply(test, length))
test <- data.frame(do.call(rbind, lapply(test, 
                                         function(i) c(i, rep(NA, maxL-length(i))))))
nyarticle.data2 <- cbind(x=nyarticle.data[, -2], test)

keywords <- melt(nyarticle.data2, id.vars="x.web_url", measure.vars=c("X1","X2","X3","X4","X5",
                                                               "X6","X7","X8","X9","X10",
                                                               "X11","X12","X13","X14"))
keyword.counts <- as.data.frame(table(keywords$value))
order <- levels(with(keyword.counts, reorder(Var1, -Freq)))

ggplot(keyword.counts[which(keyword.counts$Freq >= 5),], aes(x = reorder(Var1, -Freq), y = Freq, 
                                                             fill = Var1)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  xlab("") + ylab("Frequency") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        legend.title=element_text(face="bold")) +
  scale_fill_discrete(name="Keyword", breaks = order)


#Personalized scraper with rvest (influenced by this page: 
    #https://stackoverflow.com/questions/36683510/r-web-scraping-across-multiple-pages)
review.text <- lapply(paste0('https://www.amazon.com/COSORI-CP018-PC-Electric-Stainless-17-Program/product-reviews/B07415MDCH/ref=cm_cr_arp_d_paging_btm_2?ie=UTF8&reviewerType=all_reviews&pageNumber=', 1:50),
                      function(url){
                        url %>% read_html() %>% 
                          html_nodes(".review-text") %>% 
                          html_text()
                })

review.title <- lapply(paste0('https://www.amazon.com/COSORI-CP018-PC-Electric-Stainless-17-Program/product-reviews/B07415MDCH/ref=cm_cr_arp_d_paging_btm_2?ie=UTF8&reviewerType=all_reviews&pageNumber=', 1:50),
                      function(url){
                        url %>% read_html() %>% 
                          html_nodes("#cm_cr-review_list .a-color-base") %>% 
                          html_text()
                      })

review.rating <- lapply(paste0('https://www.amazon.com/COSORI-CP018-PC-Electric-Stainless-17-Program/product-reviews/B07415MDCH/ref=cm_cr_arp_d_paging_btm_2?ie=UTF8&reviewerType=all_reviews&pageNumber=', 1:50),
                       function(url){
                         url %>% read_html() %>% 
                           html_nodes("#cm_cr-review_list .review-rating") %>% 
                           html_text()
                       })

review.name <- lapply(paste0('https://www.amazon.com/COSORI-CP018-PC-Electric-Stainless-17-Program/product-reviews/B07415MDCH/ref=cm_cr_arp_d_paging_btm_2?ie=UTF8&reviewerType=all_reviews&pageNumber=', 1:50),
                        function(url){
                          url %>% read_html() %>% 
                            html_nodes("#cm_cr-review_list .a-profile-name") %>% 
                            html_text()
                        })

review.date <- lapply(paste0('https://www.amazon.com/COSORI-CP018-PC-Electric-Stainless-17-Program/product-reviews/B07415MDCH/ref=cm_cr_arp_d_paging_btm_2?ie=UTF8&reviewerType=all_reviews&pageNumber=', 1:50),
                      function(url){
                        url %>% read_html() %>% 
                          html_nodes("#cm_cr-review_list .review-date") %>% 
                          html_text()
                      })

ipot.reviews <- cbind(unlist(review.name), unlist(review.rating),
                      unlist(review.title),
                      unlist(review.text), unlist(review.date))
colnames(ipot.reviews) <- c("name", "rating", "title", "review", "date")
ipot.reviews <- as.data.frame(ipot.reviews)


#Check out the data
head(ipot.reviews, 10)


#Check the percentage distribution of ratings--it matches what's on Amazon (as of Jan 17)
prop.table(table(ipot.reviews$rating))


#Distribution of the ratings
ggplot(ipot.reviews, aes(x = as.factor(rating))) +
  geom_bar() +
  theme_bw() +
  ylab("Frequency") + xlab("Ratings")


#Ratings over time
ipot.reviews$date2 <- parse_date_time(ipot.reviews$date, orders = c("mdy"))
ipot.reviews <- ipot.reviews[order(ipot.reviews$date2),]
ipot.reviews$date2 <- as.numeric(cut(ipot.reviews$date2, 3)) #Cut variable into
                                                        #3 equal-size groups

df <- as.data.frame(with(ipot.reviews, prop.table(table(rating,date2),1)))
ggplot(df, aes(x=factor(date2), y=Freq, fill=rating)) + 
  geom_bar(position="dodge", color="black", stat="identity") +
  xlab("Date Range") + ylab("Proportion of Ratings") +
  theme_bw() +
  theme(legend.title=element_text(face="bold")) +
  scale_fill_discrete(name="Rating") +
  scale_x_discrete(limits=c("1","2","3"),
                   labels=c("09/17/17 - 02/25/18","02/28/18 - 08/06/18",
                            "08/07/18 - 01/15/19")) 


#Save your R objects
save.image("rtimes_objects.RData")

### END ###


