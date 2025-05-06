library(stm)
library(quanteda)
library(stringr) # Package provide a cohesive set of functions designed to make working with strings as easy as possible
library(igraph) # Package for network analysis and visualization
library(stmCorrViz) # Package for hierarchical correlation view of STMs

###########################################
## Reading the data from the spreadsheet ##
###########################################

setwd("C:/Users/maksy/Desktop/SEMESTER 5/Business Data Analytics/Project 2")
data <- read.csv(file = "project2_reviews.csv", header = TRUE, sep = ',')

#######################
## Cleaning the data ##
#######################

data <- subset(data, select = -c(X, X.1, X.2, X.3, X.4, X.5))
columns <- c("rating", "text", "title", "user.userLocation.name", "user.userLocation.shortName", "user.username", "publishedDate", "sentiment", "year", "nationality")

# data<-data[!is.na(data$documents),] # stm doesn't work with missing data
data <- data[complete.cases(data[, columns]), ]

data
data$text = str_replace_all(data$text, "/"," ")# substitute "/" with a space
data$text = str_replace_all(data$text, "&#x27;|&quot;|&#x2F;", "'") ## links andother eventual html encoding (adapted from https://juliasilge.com/blog/evaluating-stm/)
data$text = str_replace_all(data$text, "<a(.*?)>", " ")
data$text = str_replace_all(data$text, "&gt;|&lt;|&amp;", " ")
data$text = str_replace_all(data$text, "&#[:digit:]+;", " ")
data$text = str_remove_all(data$text, "<[^>]*>")
docss<-data$text
docss
remove.words <- c("dont","will","can","one","also", "get", "-")
length(docss)

##################################
### The textProcessor function ###
##################################

processed <- textProcessor(docss, metadata=data, lowercase=TRUE, removestopwords=TRUE,
                           removenumbers=TRUE, removepunctuation=TRUE, stem=TRUE, wordLengths=c(3, Inf),
                           sparselevel=1, language="en", verbose=TRUE, onlycharacter= FALSE,
                           striphtml=FALSE, customstopwords=remove.words)

meta<-processed$meta
vocab<-processed$vocab
docs<-processed$document
docs
length(docs)
rr<-processed$docs.removed
rr
length(rr)
# building the list of documents correspondent to docss
if (identical(rr, integer(0))){
 z<-docss
} else {
 z<-docss[-rr]
}

##################################
### The prepDocuments function ###
##################################

plotRemoved(processed$documents, lower.thresh = seq(1, 25, by = 1))

out <- prepDocuments(processed$documents, processed$vocab, processed$meta, verbose=FALSE,
                     lower.thresh = 2)
out
docs <- out$documents
docs
vocab <- out$vocab
vocab
meta <-out$meta
meta
out$words.removed
out$docs.removed
out$tokens.removed
out$wordcounts
# building the list of documents correspondent to docs
rr1<-out$docs.removed
rr1
rr1 <- as.numeric(rr1)
rr1
if (identical(rr1, numeric(0))){
  z
} else {
  z<-z[-rr1]
}
z
length(z)


###########################
### Assesing the models ###
###########################

library(ggplot2)
library(plotly)
###______ASSESSING THE MODELS (10, 15, 20, 25, 30 topics)___________________________###

model_NP_10Prrateby<-stm(docs, vocab, K=10, data=meta, init.type = "Spectral")
model_NP_10Prrateby
labelTopics(model_NP_10Prrateby,n=15) #show top 15 words
plot.STM(model_NP_10Prrateby,main="Top Terms for 10 topics model", n=7) #show top topics

model_NP_15Prrateby<-stm(docs, vocab, K=15, data=meta, init.type = "Spectral")
model_NP_15Prrateby
labelTopics(model_NP_15Prrateby,n=15) #show top 15 words
plot.STM(model_NP_15Prrateby,main="Top Terms for 15 topics model", n=7) #show top topics

model_NP_20Prrateby<-stm(docs,vocab, K=20, data=meta, init.type = "Spectral", verbose=FALSE)
labelTopics(model_NP_20Prrateby,n=15)
model_NP_20Prrateby
plot.STM(model_NP_20Prrateby,main="Top Terms for 20 topics model",n=7)

model_NP_25Prrateby<-stm(docs,vocab, K=25, data=meta, init.type = "Spectral", verbose=FALSE)
labelTopics(model_NP_25Prrateby,n=15)
model_NP_25Prrateby
plot.STM(model_NP_25Prrateby, main = "Top Terms for Topics 1-12", n = 7, topics = 1:12)
plot.STM(model_NP_25Prrateby, main = "Top Terms for Topics 13-25", n = 7, topics = 13:25)


model_NP_30Prrateby<-stm(docs,vocab, K=30, data=meta, init.type = "Spectral", verbose=FALSE)
labelTopics(model_NP_30Prrateby,n=15)
model_NP_30Prrateby
plot.STM(model_NP_30Prrateby,main="Top Terms for Topics 1-15",n=7, topics = 1:15)
plot.STM(model_NP_30Prrateby,main="Top Terms for Topics 16-30",n=7, topics = 16:30)

###_______________________Coherence-Exclusivity CHECKING________________________###

M10ExSem<-as.data.frame(cbind(c(1:10),exclusivity(model_NP_10Prrateby), semanticCoherence(model=model_NP_10Prrateby, docs), "Mod10"))
M15ExSem<-as.data.frame(cbind(c(1:15),exclusivity(model_NP_15Prrateby), semanticCoherence(model=model_NP_15Prrateby, docs), "Mod15"))
M20ExSem<-as.data.frame(cbind(c(1:20),exclusivity(model_NP_20Prrateby), semanticCoherence(model=model_NP_20Prrateby, docs), "Mod20"))
M25ExSem<-as.data.frame(cbind(c(1:25),exclusivity(model_NP_25Prrateby), semanticCoherence(model=model_NP_25Prrateby, docs), "Mod25"))
M30ExSem<-as.data.frame(cbind(c(1:30),exclusivity(model_NP_30Prrateby), semanticCoherence(model=model_NP_30Prrateby, docs), "Mod30"))


mean_Exc10<-mean(exclusivity(model_NP_10Prrateby))
mean_Coh10<-mean(semanticCoherence(model=model_NP_10Prrateby, docs))
mean_Exc15<-mean(exclusivity(model_NP_15Prrateby))
mean_Coh15<-mean(semanticCoherence(model=model_NP_15Prrateby, docs))
mean_Exc20<-mean(exclusivity(model_NP_20Prrateby))
mean_Coh20<-mean(semanticCoherence(model=model_NP_20Prrateby, docs))
mean_Exc25<-mean(exclusivity(model_NP_25Prrateby))
mean_Coh25<-mean(semanticCoherence(model=model_NP_25Prrateby, docs))
mean_Exc30<-mean(exclusivity(model_NP_30Prrateby))
mean_Coh30<-mean(semanticCoherence(model=model_NP_30Prrateby, docs))
mean_Exc10
mean_Coh10
mean_Exc15
mean_Coh15
mean_Exc20
mean_Coh20
mean_Exc25
mean_Coh25
mean_Exc30
mean_Coh30


ModsExSem<-rbind(M10ExSem, M15ExSem, M20ExSem, M25ExSem, M30ExSem)
colnames(ModsExSem)<-c("K","Exclusivity", "SemanticCoherence", "Model")
ModsExSem$Exclusivity<-as.numeric(as.character(ModsExSem$Exclusivity))
ModsExSem$SemanticCoherence<-as.numeric(as.character(ModsExSem$SemanticCoherence))
options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)
plotexcoer<-ggplot(ModsExSem, aes(SemanticCoherence, Exclusivity, color = Model)) +geom_point(size = 2, alpha = 0.7) + geom_text(aes(label=K), nudge_x=.05, nudge_y=.05) + labs(x = "Semantic coherence", y= "Exclusivity", title = "Comparing exclusivity and semantic coherence")
plotexcoer


###########################################
### 1.4 ESTIMATING THE STRUCTURAL MODEL ###
###########################################

##################################
### Model selection and search ###
##################################

model_NP_15Prrateby<-stm(docs, vocab, K=15, data=meta, init.type = "Spectral", verbose=FALSE)
model_NP_15Prrateby
labelTopics(model_NP_15Prrateby)
capture.output(labelTopics(model_NP_15Prrateby), file = "Topics KeyWords.txt")

#############################
### B. Explore the topics ###
#############################

### The most common words for a give topic. Make a label plot using FREX labels for topics and one using PROB labels. 
par(mfrow = c(1,2))
plot(model_NP_15Prrateby, type = 'labels', labeltype = 'frex', main = 'FREX',text.cex=0.5, n=8)
plot(model_NP_15Prrateby, type = 'labels', labeltype = 'prob', main = 'PROB',text.cex=0.5, n=8)

cloud(model_NP_15Prrateby, topic = 5)
cloud(model_NP_15Prrateby, topic = 8)
cloud(model_NP_15Prrateby, topic = 12)
cloud(model_NP_15Prrateby, topic = 15)

############################
### c) Total topic share ###
############################
par(mfrow = c(1,1))
plot(model_NP_15Prrateby, type="summary", xlim=c(0,.4))

##################################
### d) Contrast between topics ###
##################################
dev.off()
plot(model_NP_15Prrateby, type = "perspectives", topics = c(5,10), main = "Topic contrasts") # Topics #10 and #5
plot(model_NP_15Prrateby, type = "perspectives", topics = c(3,7), main = "Topic contrasts") # Topics #7 and #3
plot(model_NP_15Prrateby, type = "perspectives", topics = c(2,13), main = "Topic contrasts") # Topics #13 and #2
plot(model_NP_15Prrateby, type = "perspectives", topics = c(1,9), main = "Topic contrasts") # Topics #9 and #1

##############################################
### e) Topic proportions within documents: ###
##############################################

library(tidytext)
td_theta <- tidytext::tidy(model_NP_15Prrateby, matrix = "theta")
selectiontdthteta<-td_theta[td_theta$document%in%c(1:15),]#select the first 15 documents
thetaplot1<-ggplot(selectiontdthteta, aes(y=gamma, x=as.factor(topic), fill = as.factor(topic))) + geom_bar(stat="identity",alpha = 0.8, show.legend = FALSE) + facet_wrap(~ document, ncol = 3) + labs(title = "Theta values per document", y = expression(theta), x = "Topic")
thetaplot1

#####################################
### f) Word frequencies per topic ###
#####################################

library(dplyr)
if (!require(devtools)) install.packages("devtools")
# Loading required package: devtools
#install.packages("drlib", repos = "http://cran.us.r-project.org")
# require(devtools)
library(devtools)
devtools::install_github("dgrtwo/drlib")

library(drlib) #drlib is available on github and needs devtools to be installed
td_beta <- tidytext::tidy(model_NP_15Prrateby)
options(repr.plot.width=7, repr.plot.height=8, repr.plot.res=100)
td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")

##############################################
### g) Word distribution within each topic ###
##############################################

## Topic 1
betaT1<-td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%filter(topic=="Topic 1")
#beta values for topic 1
betaplotT1<-ggplot(betaT1[betaT1$beta>0.005,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms",
                                                                                  y = expression(beta), title = "Word probabilities for Topic 1")
#plot word probabilities higher than 0.005 for topic 1
betaplotT1

## Topic 2
betaT2<-td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%filter(topic=="Topic 2")
#beta values for topic 1
betaplotT2<-ggplot(betaT2[betaT2$beta>0.005,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms",
                                                                                  y = expression(beta), title = "Word probabilities for Topic 2")
#plot word probabilities higher than 0.005 for topic 2
betaplotT2

## Topic 3
betaT3<-td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%filter(topic=="Topic 3")
#beta values for topic 3
betaplotT3<-ggplot(betaT3[betaT3$beta>0.005,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms",
                                                                                  y = expression(beta), title = "Word probabilities for Topic 3")
#plot word probabilities higher than 0.005 for topic 3
betaplotT3

## Topic 4
betaT4<-td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%filter(topic=="Topic 4")
#beta values for topic 4
betaplotT4<-ggplot(betaT4[betaT4$beta>0.005,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms",
                                                                                  y = expression(beta), title = "Word probabilities for Topic 4")
#plot word probabilities higher than 0.005 for topic 4
betaplotT4

## Topic 5
betaT5<-td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%filter(topic=="Topic 5")
#beta values for topic 5
betaplotT5<-ggplot(betaT5[betaT5$beta>0.005,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms",
                                                                                  y = expression(beta), title = "Word probabilities for Topic 5")
#plot word probabilities higher than 0.005 for topic 5
betaplotT5

## Topic 6
betaT6<-td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%filter(topic=="Topic 6")
#beta values for topic 6
betaplotT6<-ggplot(betaT6[betaT6$beta>0.005,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms",
                                                                                  y = expression(beta), title = "Word probabilities for Topic 6")
#plot word probabilities higher than 0.005 for topic 6
betaplotT6

## Topic 7
betaT7<-td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%filter(topic=="Topic 7")
#beta values for topic 7
betaplotT7<-ggplot(betaT7[betaT7$beta>0.005,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms",
                                                                                  y = expression(beta), title = "Word probabilities for Topic 7")
#plot word probabilities higher than 0.005 for topic 7
betaplotT7

# Topic 8
betaT8 <- td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic == "Topic 8")

betaplotT8 <- ggplot(betaT8[betaT8$beta > 0.005,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "identity") +
  coord_flip() +
  labs(x = "Terms", y = expression(beta), title = "Word probabilities for Topic 8")
betaplotT8

# Topic 9
betaT9 <- td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic == "Topic 9")

betaplotT9 <- ggplot(betaT9[betaT9$beta > 0.005,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "identity") +
  coord_flip() +
  labs(x = "Terms", y = expression(beta), title = "Word probabilities for Topic 9")
betaplotT9

# Topic 10
betaT10 <- td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic == "Topic 10")

betaplotT10 <- ggplot(betaT10[betaT10$beta > 0.005,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "identity") +
  coord_flip() +
  labs(x = "Terms", y = expression(beta), title = "Word probabilities for Topic 10")
betaplotT10

# Topic 11
betaT11 <- td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic == "Topic 11")

betaplotT11 <- ggplot(betaT11[betaT11$beta > 0.005,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "identity") +
  coord_flip() +
  labs(x = "Terms", y = expression(beta), title = "Word probabilities for Topic 11")
betaplotT11

# Topic 12
betaT12 <- td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic == "Topic 12")

betaplotT12 <- ggplot(betaT12[betaT12$beta > 0.005,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "identity") +
  coord_flip() +
  labs(x = "Terms", y = expression(beta), title = "Word probabilities for Topic 12")
betaplotT12

# Topic 13
betaT13 <- td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic == "Topic 13")

betaplotT13 <- ggplot(betaT13[betaT13$beta > 0.005,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "identity") +
  coord_flip() +
  labs(x = "Terms", y = expression(beta), title = "Word probabilities for Topic 13")
betaplotT13

# Topic 14
betaT14 <- td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic == "Topic 14")

betaplotT14 <- ggplot(betaT14[betaT14$beta > 0.005,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "identity") +
  coord_flip() +
  labs(x = "Terms", y = expression(beta), title = "Word probabilities for Topic 14")
betaplotT14

# Topic 15
betaT15 <- td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic == "Topic 15")

betaplotT15 <- ggplot(betaT15[betaT15$beta > 0.005,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "identity") +
  coord_flip() +
  labs(x = "Terms", y = expression(beta), title = "Word probabilities for Topic 15")
betaplotT15


####################################################
### h) Documents associated with specific topics ###
####################################################

# Number of topics (15 in this case)
N <- 15

# Loop through all topics from 1 to N
for (i in seq(1, N)) {
  
  # Extract the thoughts for each topic
  thoughts <- findThoughts(model_NP_30Prrateby, texts = z, n = 50, topics = i)$docs[[1]]
  
  # Save thoughts to CSV file for each topic
  file_name <- paste("Comments", toString(i), ".csv")
  write.csv(thoughts, file = file_name)
  
  # Plot topic labels and quotes for each topic
  options(repr.plot.width=10, repr.plot.height=12, repr.plot.res=100)
  par(mfrow=c(1,2))
  
  # Plot topic labels
  plot.STM(model_NP_30Prrateby, "labels", text.cex=0.5)
  
  # Extract the quotes for the current topic and plot them
  hh <- findThoughts(model_NP_30Prrateby, texts = z, n = 5, topics = i)$docs[[1]]
  plotQuote(hh, width=100, maxwidth=1000, text.cex=0.5, main=paste("Topic", i))
}

######################################
### i) Correlations between topics ###
######################################

library(stmCorrViz)
mod.out.corr <- topicCorr(model_NP_15Prrateby)
plot(mod.out.corr)

#######################################
### j) Topic hierarchy/correlations ### 
#######################################

stmCorrViz(model_NP_15Prrateby, "stm-interactive-correlation.html",
           documents_raw=data$documents, documents_matrix=out$documents)



#########################
### Topic proportions ###
#########################

# Step 1: Extract the topic proportions (theta) from the STM model
td_theta <- tidy(model_NP_15Prrateby, matrix = "theta")

# Step 2: Calculate the mean proportion for each topic across all documents
topic_proportions <- td_theta %>%
  group_by(topic) %>%
  summarise(mean_proportion = mean(gamma)) %>%
  mutate(percentage = mean_proportion * 100)  # Convert to percentage

# Step 3: Sort the topics by their proportions (in descending order)
topic_proportions_sorted <- topic_proportions %>%
  arrange(desc(percentage))

###################################################################################
### 1.5 ESTIMATING THE STRUCTURAL TOPIC MODEL WITH TOPICAL PREVALENCE PARAMETER ###
###################################################################################


model_NP_15Prrateby_Sent<-stm(docs, vocab, prevalence=~sentiment, K=15, data=meta,
                              init.type = "Spectral", verbose=FALSE)

model_NP_15Prrateby_Sent
labelTopics(model_NP_15Prrateby_Sent)

predict_topics_NP<-estimateEffect(formula =~ sentiment, stmobj = model_NP_15Prrateby_Sent,
                                  metadata = out$meta, uncertainty = "Global")

head(summary(predict_topics_NP))

# Create the plot and save it as a PNG file
png("structural_topic_model.png", width = 1200, height = 800)

# Plotting the estimateEffect results
plot.estimateEffect(
  predict_topics_NP, 
  model = model_NP_15Prrateby_Sent, 
  cov.value1 = "pos",
  cov.value2 = "neg", 
  covariate = "sentiment",
  method = "difference",
  nsims = 100,
  xlab = "More Negative ..................................................................................................................................... More Positive", 
  labeltype = "custom", 
  custom.labels = c(1:30), 
  ci.level = 0.99
)

dev.off()  # Close the graphics device


#######

#> Loading required package: devtools
devtools::install_github("mikaelpoul/tidystm", dependencies = TRUE)
library(tidystm)
effect00 <- extract.estimateEffect(x = predict_topics_NP,
                                   covariate = "sentiment",
                                   model = model_NP_15Prrateby_Sent,
                                   method = "pointestimate",
)
knitr::kable(effect00)
write.csv(effect00, file = "effect15.csv")
#______________________Charts__________________________
install.packages("stminsights")
library(stminsights)
effects <- get_effects(estimates = predict_topics_NP,
                       variable = "sentiment",
                       type = "pointestimate")
effects

par(mfrow = c(1,1))

# plot effects for topics 6 and 11
effects %>% filter(topic == 6) %>%
  ggplot(aes(x = value, y = proportion)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, size = 1) +
  coord_flip() + theme_light() + labs(x = 'Topic 6', y = 'Topic Proportion')
effects %>% filter(topic == 11) %>%
  ggplot(aes(x = value, y = proportion)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, size = 1) +
  coord_flip() + theme_light() + labs(x = 'Topic 11', y = 'Topic Proportion')


##########################################
### 1.6 IDENTIFICATION THE TIME EFFECT ###
##########################################
meta$sentiment <- as.factor(meta$sentiment)
meta$year <- as.factor(meta$year)   # If year should be categorical

model_NP_15_1Prrateby_year<-stm(docs, vocab, prevalence=~sentiment+year, K=15, data=meta, init.type = "Spectral", verbose=FALSE)
labelTopics(model_NP_15_1Prrateby_year)
model_NP_15_1Prrateby_year
#Topics prevalence table
topicprop15_1<- model_NP_15_1Prrateby_year$theta
topicprop15_1
write.csv(topicprop15_1, file = "topicprop15_1.csv")


predict_topics_1_NP<-estimateEffect(formula = ~ sentiment + year, stmobj = model_NP_15_1Prrateby_year, metadata = out$meta, uncertainty = "Global")
predict_topics_1_NP
effect15_1 <- extract.estimateEffect(x = predict_topics_1_NP,
                                     covariate = "year",
                                     moderator = "sentiment",
                                     model = model_NP_15_1Prrateby_year,
                                     method = "pointestimate",
                                     moderator.value ="pos" )
effect15_1
knitr::kable(effect15_1)
write.csv(effect15_1, file = "effect15_1.csv")
effect15_2 <- extract.estimateEffect(x = predict_topics_1_NP,
                                     covariate = "year",
                                     moderator = "sentiment",
                                     model = model_NP_15_1Prrateby_year,
                                     method = "pointestimate",
                                     moderator.value ="neg")
knitr::kable(effect15_2)
write.csv(effect15_2, file = "effect15_2.csv")


###################################################################
### c) Build the plots the relationship between Time and Topics ###
###################################################################


## Estimate Effect visualization
library(ggplot2)
effect <- lapply(c("neg", "pos"), function(i) {
  extract.estimateEffect(x = predict_topics_1_NP,
                         covariate = "year",
                         method = "continuous",
                         model = model_NP_15_1Prrateby_year,
                         labeltype = "prob",
                         topic=15,
                         n = 7,
                         ci.level = 0.95,
                         moderator = "sentiment",
                         moderator.value = i)
})
head(effect)
effect <- do.call("rbind", effect)
effect
## Plot it with ggplot2 and facet by topic instead.
library(ggplot2)
ggplot(effect, aes(x = covariate.value, y = estimate,
                   ymin = ci.lower, ymax = ci.upper,
                   group = moderator.value,
                   fill = factor(moderator.value))) +
  facet_wrap(~ label, nrow = 2) +
  geom_ribbon(alpha = .5) +
  geom_line() +
  scale_x_continuous(labels = function(x) ifelse(x == 1, "1\nREP", ifelse(x == 0, "0\nDEM",
                                                                          x))) + labs(x = "year", y = "Expected Topic Proportion", fill = "sentiment") +
  theme(legend.position = "bottom")


##########################################
### 1.7 IDENTIFICATION LOCATION EFFECT ###
##########################################

### a) Create the model model_NP_15_2Prrateby uses “Sentiment” and “nationality” variables

meta$sentiment <- as.factor(meta$sentiment)
meta$nationality <- as.factor(meta$nationality)

meta <- na.omit(meta)

model_NP_15_2Prrateby<-stm(docs,vocab, prevalence=~sentiment+nationality, K=15, data=meta, init.type = "Spectral", verbose=FALSE)


labelTopics(model_NP_15_2Prrateby)
model_NP_15_2Prrateby
topicprop15_2<-model_NP_15_2Prrateby$theta
topicprop15_2
write.csv(topicprop15_2, file = "topicprop15_2.csv")

### b) Estimate the relationship between user.userLocation.shortName and Topics 
predict_topics_2_NP<-estimateEffect(formula = ~ sentiment + nationality, stmobj = model_NP_15_2Prrateby, metadata = out$meta, uncertainty = "Global")
predict_topics_2_NP

# Verify and preprocess the covariate
meta$nationality <- as.factor(meta$nationality)

# Ensure levels match
meta$nationality <- factor(
  meta$nationality,
  levels = c("Poland", "Foreigner")
)

# Plot the effect
plot.estimateEffect(
  predict_topics_2_NP,
  model = model_NP_15_2Prrateby,
  cov.value1 = "Poland",
  cov.value2 = "Foreigner",
  covariate = "nationality",
  method = "difference",
  nsims = 100,
  xlab = "Poland ............................................................... Foreigner",
  labeltype = "custom",
  custom.labels = c(1:15),
  ci.level = 0.99
)

### c) Plot the influence of covariates included in as a topical content covariate 

model_NP_15_3Prrateby<-stm(docs,vocab, prevalence=~sentiment+nationality, K=15, data=meta
                           , init.type = "Spectral", content=~nationality, verbose=FALSE)
labelTopics(model_NP_15_3Prrateby)
model_NP_15_3Prrateby


plot(model_NP_15_3Prrateby, type="perspectives", topics=15)



### d) Build the plots of the relationship between Nationality and Topics 

#____nationality 0 - Poland, 1 - Foreigner________________________
effect15_3 <- extract.estimateEffect(x = predict_topics_2_NP,
                                     covariate = "nationality",
                                     moderator = "sentiment",
                                     model = model_NP_15_2Prrateby,
                                     method = "pointestimate",
                                     moderator.value ="pos" )
effect15_3
knitr::kable(effect15_3)
write.csv(effect15_3, file = "effect15_3.csv")
effect15_4 <- extract.estimateEffect(x = predict_topics_2_NP,
                                     covariate = "nationality", 
                                     moderator = "sentiment",
                                     model = model_NP_15_2Prrateby,
                                     method = "pointestimate",
                                     moderator.value ="neg")
knitr::kable(effect15_4)
write.csv(effect15_4, file = "effect15_4.csv")

### Plot


meta$nationality_numeric <- ifelse(meta$nationality == "Poland", 0, 1)

model_NP_15_3Prrateby <- stm(
  docs, vocab, 
  prevalence = ~ sentiment + nationality_numeric, 
  K = 15, 
  data = meta, 
  init.type = "Spectral", 
  verbose = FALSE
)

predict_topics_2_NP <- estimateEffect(
  1:15 ~ sentiment + nationality_numeric, 
  stmobj = model_NP_15_3Prrateby, 
  metadata = meta, 
  uncertainty = "Global"
)

effect <- lapply(c("neg", "pos"), function(i) {
  extract.estimateEffect(x = predict_topics_2_NP,
                         covariate = "nationality_numeric",
                         method = "continuous",
                         model = model_NP_15_3Prrateby,
                         labeltype = "prob",
                         topic=15,
                         n = 7,
                         ci.level = 0.95,
                         moderator = "sentiment",
                         moderator.value = i)
})
effect <- do.call("rbind", effect)
ggplot(effect, aes(x = covariate.value, y = estimate,
                   ymin = ci.lower, ymax = ci.upper,
                   group = moderator.value,
                   fill = factor(moderator.value))) +
  facet_wrap(~ label, nrow = 2) +
  geom_ribbon(alpha = .5) +
  geom_line() +
  scale_x_continuous(labels = function(x) ifelse(x == 1, "1\nPoland", ifelse(x == 0, "0\nForeigner",
                                                                          x))) + labs(x = "nationality", y = "Expected Topic Proportion", fill = "sentiment") +
  theme(legend.position = "bottom")
