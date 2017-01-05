setwd("C:/Users/Cindy/Documents/Davis/Summer")
load("house-training-test.Rdata")


rem = c("county", "zip", "street", "long", "lat", "date")
predtraining = housetraining[,-match(rem, names(housetraining))]
predtraining = predtraining[,-6]
predtraining = na.exclude(predtraining)
require("randomForest")


marintraining = housetraining[which(housetraining$county == "Marin County"),] # only marin county
marintraining = marintraining[,-match(rem, names(marintraining))] # disclude some variables

marintraining = na.exclude(marintraining) # remove NA's
set.seed(1256)
marintraining$city = as.factor(as.character(marintraining$city))
fit.rf = randomForest(city ~ ., data = marintraining) # fitting the random forest

print(fit.rf)

importance(fit.rf) # br isn't that important
plot(fit.rf)




### KNN ###
install.packages("caret")
library(caret)
?knnreg

# removing non-numeric
numtrain = housetraining[,sapply(housetraining, is.numeric)]
numtest = housetest[,sapply(housetest, is.numeric)]
numtrain = na.exclude(numtrain)
numtest = na.exclude(numtest)

set.seed(300)
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
} # normalize function

normtrain = as.data.frame(lapply(numtrain, normalize)) # normalized
normtest = as.data.frame(lapply(numtest, normalize))

knnResult = knnregTrain(train = normtrain, test = normtest, y = normtrain$price, k = 5)
library(ggplot2)

par(mfrow=c(1,1))
smoothScatter(knnResult, normtest$price, main="Scatterplot Colored by Smoothed Densities")

errorRate = sqrt(1/length(knnResult)*sum(normtest$price - knnResult)^2)
errorRate = sqrt(1/length(knnResult)*sum(normtest$price - knnResult)^2)

patricksDataFrame = data.frame(Predicted = knnResult, Actual = normtest$price)
