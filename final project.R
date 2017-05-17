#setwd("/Users/siyuan/Documents/Spring 2017/Orie 4740 - Data Mining/Seattle_Airbnb")

#calendar = read.csv("data/calendar.csv", header = TRUE)
listings = read.csv("data/listings.csv", header = TRUE)
#reviews = read.csv("data/reviews.csv", header = TRUE)

# Remove arbitrary features
listings[c("thumbnail_url", "medium_url", "picture_url", "xl_picture_url", 
           "host_url", "host_thumbnail_url", "host_picture_url", "host_neighbourhood", 
           "listing_url", "experiences_offered", "id", "scrape_id", "last_scraped", "name", "host_id",
           "host_name", "host_since", "host_location", "host_acceptance_rate", "host_verifications",
           "host_has_profile_pic", "host_identity_verified", "street", "neighbourhood",
           "city", "state", "market", "zipcode", "smart_location", "country_code", "country",
           "is_location_exact", "square_feet", "weekly_price", "monthly_price", "maximum_nights",
           "calendar_updated", "has_availability", "calendar_last_scraped", "first_review", "last_review",
           "requires_license", "jurisdiction_names", "license", "neighbourhood_cleansed")] = NULL

# Remove dollar sign and percentage in price-related features and convert them to number
listings$security_deposit = as.numeric(gsub("[\\$,]", "", listings$security_deposit))
listings$price = as.numeric(gsub("[\\$,]", "", listings$price))
listings$cleaning_fee = as.numeric(gsub("[\\$,]", "", listings$cleaning_fee))
listings$extra_people = as.numeric(gsub("[\\$,]", "", listings$extra_people))
listings$host_response_rate = as.numeric(gsub("[,\\%]", "", listings$host_response_rate))

# Replace missing value in features with 0
listings$security_deposit[is.na(listings$security_deposit)] = 0
listings$cleaning_fee[is.na(listings$cleaning_fee)] = 0
listings$reviews_per_month[is.na(listings$reviews_per_month)] = 0
listings$bathrooms[is.na(listings$bathrooms)] = 0
listings$bedrooms[is.na(listings$bedrooms)] = 0
listings$beds[is.na(listings$beds)] = 0

# Replace missing values in feature with average of the feature
listings$review_scores_rating[is.na(listings$review_scores_rating)] = mean(listings$review_scores_rating, na.rm = TRUE)
listings$review_scores_accuracy[is.na(listings$review_scores_accuracy)] = mean(listings$review_scores_accuracy, na.rm = TRUE)
listings$review_scores_cleanliness[is.na(listings$review_scores_cleanliness)] = mean(listings$review_scores_cleanliness, na.rm = TRUE) 
listings$review_scores_checkin[is.na(listings$review_scores_checkin)] = mean(listings$review_scores_checkin, na.rm = TRUE)
listings$review_scores_communication[is.na(listings$review_scores_communication)] = mean(listings$review_scores_communication, na.rm = TRUE)
listings$review_scores_location[is.na(listings$review_scores_location)] = mean(listings$review_scores_location, na.rm = TRUE)
listings$review_scores_value[is.na(listings$review_scores_value)] = mean(listings$review_scores_value, na.rm = TRUE)
listings$host_response_rate[is.na(listings$host_response_rate)] = mean(listings$host_response_rate, na.rm = TRUE)


# Sum number of words in text-related features
listings$count_summary = sapply(gregexpr("[[:alpha:]]+", listings$summary), function(x) sum(x > 0))
listings$count_summary = listings$count_summary + sapply(gregexpr("[[:digit:]]+\\.*[[:digit:]]*", listings$summary), function(x) sum(x > 0))

listings$count_description = sapply(gregexpr("[[:alpha:]]+", listings$description), function(x) sum(x > 0))
listings$count_description = listings$count_description + sapply(gregexpr("[[:digit:]]+\\.*[[:digit:]]*", listings$description), function(x) sum(x > 0))

listings$count_space = sapply(gregexpr("[[:alpha:]]+", listings$space), function(x) sum(x > 0))
listings$count_space = listings$count_space + sapply(gregexpr("[[:digit:]]+\\.*[[:digit:]]*", listings$space), function(x) sum(x > 0))

listings$count_neighborhood = sapply(gregexpr("[[:alpha:]]+", listings$neighborhood_overview), function(x) sum(x > 0))
listings$count_neighborhood = listings$count_neighborhood + sapply(gregexpr("[[:digit:]]+\\.*[[:digit:]]*", listings$neighborhood_overview), function(x) sum(x > 0))

listings$count_notes = sapply(gregexpr("[[:alpha:]]+", listings$notes), function(x) sum(x > 0))
listings$count_notes = listings$count_notes + sapply(gregexpr("[[:digit:]]+\\.*[[:digit:]]*", listings$notes), function(x) sum(x > 0))

listings$count_transit = sapply(gregexpr("[[:alpha:]]+", listings$transit), function(x) sum(x > 0))
listings$count_transit = listings$count_transit + sapply(gregexpr("[[:digit:]]+\\.*[[:digit:]]*", listings$transit), function(x) sum(x > 0))

listings$count_host = sapply(gregexpr("[[:alpha:]]+", listings$host_about), function(x) sum(x > 0))
listings$count_host = listings$count_host + sapply(gregexpr("[[:digit:]]+\\.*[[:digit:]]*", listings$host_about), function(x) sum(x > 0))

listings$count_amenities = sapply(gregexpr("[^,\\s][^\\,]*[^,\\s]*", listings$amenities), function(x) sum(x > 0))

# Replace missing value cannot be selected by regular expression with 0
listings$count_amenities[which(listings$amenities == "{}")] = 0
listings$count_notes[which(is.na(listings$notes))] = 0

# Calculate the total number of words across text-related features
listings$count_total = listings$count_summary + listings$count_description + listings$count_space +
  listings$count_neighborhood + listings$count_notes + listings$count_transit + listings$count_host 

# Remove redundant feautures
listings[c("summary", "description", "space", "amenities", "neighborhood_overview",
           "notes", "transit", "host_about")] = NULL

# check for missing values in listings
unique (unlist(lapply (listings, function (x) which (is.na (x)))))

# Construct a new data frame for modeling
new_listings = listings
new_listings[c("count_notes","count_description","count_space","count_transit",
               "count_host","count_neighborhood","host_listings_count"
               ,"count_summary","count_amenities")] = NULL

sapply(new_listings,class)

#new_listings = read.csv("listings_cleansed.csv", header = TRUE)

# Construct models with different approach
# Linear regression model
# Divide into training set and testing set
data.lm = new_listings
set.seed(2)
train_ind <- sample(1:nrow(data.lm), 2/3*nrow(data.lm))
listings.train <- data.lm[train_ind, ]
listings.test <- data.lm[-train_ind, ]

listingsLM = lm( formula = price ~ ., data = listings.train )
summary(listingsLM)

#detecting outliers
#1
#plot(hatvalues (listingsLM))
#lev = hat(model.matrix(listingsLM))
#plot(lev)
#data.lm[lev >0.9,]
#cook = cooks.distance(listingsLM)
#plot(cook,ylab="Cooks distances")
#points(32,lev[32],col='red')
#points(1045,lev[1045],col='red')
#points(2576,lev[2576],col='red')
#points(3588,lev[3588],col='red')
#which.max(hatvalues (listingsLM))
#data.lm <- data.lm[-c(829,1553,2057,3423), ]


# Residual plot
plot(predict(listingsLM), residuals(listingsLM))


# Backward subset selection
library(leaps)
regfit.bwd=regsubsets(price~.,data = data.lm, nvmax = 39, method = "backward")
reg.summary=summary(regfit.bwd)
# adjusted R^2
par(mfrow=c(2,2))
plot(reg.summary$adjr2, xlab="Number of Variables ",
     ylab="Adjusted RSq",type="l")
points(25,reg.summary$adjr2[25], col="red",cex=2,pch=20)
# RSS
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",
     type="l")
points(25,reg.summary$rss[25], col="red",cex=2,pch=20)
# Cp
plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
points(25,reg.summary$cp[25], col="red",cex=2,pch=20)
# BIC
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",
     type='l')
points(25,reg.summary$bic [25],col="red",cex=2,pch=20)
# Select variables based on BIC
par(mar=c(1,1,1,1))
plot(regfit.bwd,scale="bic")
plot(regfit.bwd,scale="adjr2")

# Select
#write.csv(data.lm, file = "listings_selected.csv",row.names=FALSE)
data.selected = read.csv("listings_selected.csv", header = TRUE)
# for MSE data.selected = read.csv("data/listings_selected_old.csv", header = TRUE)
set.seed(2)
train_i <- sample(1:nrow(data.selected), 2/3*nrow(data.selected))
listings.train.new <- data.selected[train_i, ]
listings.test.new <- data.selected[-train_i, ]

listingsLM.new = lm( formula = price ~ ., data = listings.train.new )
summary(listingsLM.new)
plot(predict(listingsLM.new), residuals(listingsLM.new))
regfit.bwd=regsubsets(price~.,data = data.lm, nvmax = 34, method = "backward")
reg.summary=summary(regfit.bwd)
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",
     type='l')
which.min(reg.summary$bic)
plot(regfit.bwd,scale="bic")

# poly, log
listingsLM.new = lm( formula = price ~ . +poly(guests_included ,3)
                     +sqrt(cleaning_fee)+ log(bedrooms+1)
                     +poly(bathrooms,3)+poly(accommodates,5),
                     data = listings.train.new )
summary(listingsLM.new)
plot(predict(listingsLM.new), residuals(listingsLM.new))
lev = hat(model.matrix(listingsLM.new))
plot(lev)
listings.train.new<-listings.train.new[lev <0.6,]
listingsLM.new = lm( formula = price ~ . +poly(guests_included ,3)
                     +sqrt(cleaning_fee)+ log(bedrooms+1)
                     +poly(bathrooms,3)+poly(accommodates,5),
                     data = listings.train.new )
summary(listingsLM.new)
# MSE original
pred <- predict(listingsLM,data.lm[-train_ind, ],type = 'response')
attach(data.lm)
mean((price-predict(listingsLM, data.lm))[-train_ind]^2)
# MSE
pred <- predict(listingsLM.new,data.selected[-train_i, ],type = 'response')
attach(data.selected)
mean((price-predict(listingsLM.new, data.selected))[-train_i]^2)
# MSE2
listingsLM.new2 = lm( formula = price ~ .,
                     data = listings.train.new )
pred2 <- predict(listingsLM.new2,data.selected[-train_i, ],type = 'response')
attach(data.selected)
mean((price-predict(listingsLM.new2, data.selected))[-train_i]^2)

# LASSO
library(glmnet)
x = model.matrix(price~., listings.train)
x.test = model.matrix(price~., listings.test)
lasso.fit = glmnet(x, listings.train$price, alpha = 1, lambda = 2)

cv.out=cv.glmnet(x, listings.train$price, alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.fit,s=bestlam ,newx=x.test)
mean((lasso.pred-listings.test$price)^2)

original = model.matrix(price~., listings)
out = glmnet(original, listings$price, alpha = 1, lambda = 2)
lasso.coef=predict(out,type="coefficients",s=bestlam)
lasso.coef
lasso.coef[lasso.coef!=0]

# Growing random forest for regression
library(MASS)
library(randomForest)

bag.listings=randomForest(price~.,data = listings.train, mtry=14,
                          importance=TRUE)
bag.listings
yhat.bag = predict(bag.listings,newdata=listings.test)
price.test=listings.test[, "price"]
plot(yhat.bag, price.test)
abline(0,1)

mean((yhat.bag - price.test)^2)
importance(bag.listings)
varImpPlot(bag.listings)