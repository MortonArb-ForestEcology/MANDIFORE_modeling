library(ggplot2)

test <- read.csv("/Volumes/GoogleDrive/My Drive/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/Drought and heat analysis/Resilience_dataframe.csv")


summary(test)

summary(test[test$flag.4sig & test$year>2025 & test$year<2099,])

test.clean <- test[test$flag.4sig & test$year>2025 & test$year<2099,]

ggplot(test.clean) +
  geom_histogram(aes(x=year, fill=GCM))