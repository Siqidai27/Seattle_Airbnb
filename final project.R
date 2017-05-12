#setwd("/Users/siyuan/Documents/Spring 2017/Orie 4740 - Data Mining/seattle-airbnb-open-data")

calendar = read.csv("data/calendar.csv", header = TRUE)
listings = read.csv("data/listings.csv", header = TRUE)
reviews = read.csv("data/reviews.csv", header = TRUE)

listings[c("thumbnail_url", "medium_url", "picture_url", "xl_picture_url", 
           "host_url", "host_thumbnail_url", "host_picture_url", "host_neighbourhood", 
           "listing_url", "experiences_offered", "id", "scrape_id", "last_scraped", "name", "host_id",
           "host_name", "host_since", "host_location", "host_acceptance_rate", "host_verifications",
           "host_has_profile_pic", "host_identity_verified", "street", "neighbourhood",
           "city", "state", "market", "zipcode", "smart_location", "country_code", "country",
           "is_location_exact", "square_feet", "weekly_price", "monthly_price", "maximum_nights",
           "calendar_updated", "has_availability", "calendar_last_scraped", "first_review", "last_review",
           "requires_license", "jurisdiction_names", "license")] = NULL

listings$security_deposit = as.numeric(gsub("[\\$,]", "", listings$security_deposit))
listings$price = as.numeric(gsub("[\\$,]", "", listings$price))
listings$cleaning_fee = as.numeric(gsub("[\\$,]", "", listings$cleaning_fee))
listings$extra_people = as.numeric(gsub("[\\$,]", "", listings$extra_people))

listings$security_deposit[is.na(listings$security_deposit)] = 0
listings$cleaning_fee[is.na(listings$cleaning_fee)] = 0
listings$reviews_per_month[is.na(listings$reviews_per_month)] = 0

listings$review_scores_rating[is.na(listings$review_scores_rating)] = mean(listings$review_scores_rating, na.rm = TRUE)
listings$review_scores_accuracy[is.na(listings$review_scores_accuracy)] = mean(listings$review_scores_accuracy, na.rm = TRUE)
listings$review_scores_cleanliness[is.na(listings$review_scores_cleanliness)] = mean(listings$review_scores_cleanliness, na.rm = TRUE) 
listings$review_scores_checkin[is.na(listings$review_scores_checkin)] = mean(listings$review_scores_checkin, na.rm = TRUE)
listings$review_scores_communication[is.na(listings$review_scores_communication)] = mean(listings$review_scores_communication, na.rm = TRUE)
listings$review_scores_location[is.na(listings$review_scores_location)] = mean(listings$review_scores_location, na.rm = TRUE)
listings$review_scores_value[is.na(listings$review_scores_value)] = mean(listings$review_scores_value, na.rm = TRUE)

#listings$host_response_time[where(listings$host_response_time == "N/A")]
listings$host_response_rate = as.numeric(gsub("[,\\%]", "", listings$host_response_rate))
listings$host_response_rate[is.na(listings$host_response_rate)] = mean(listings$host_response_rate, na.rm = TRUE)

# sum number of words in each feature
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
listings$count_amenities[which(listings$amenities == "{}")] = 0

listings$count_notes[which(is.na(listings$notes))] = 0
listings$count_total = listings$count_summary + listings$count_description + listings$count_space +
  listings$count_neighborhood + listings$count_notes + listings$count_transit + 
  listings$count_host 

listings[c("summary", "description", "space", "amenities", "neighborhood_overview",
           "notes", "transit", "host_about")] = NULL

#sapply(listings,class)

new_listings = listings
new_listings[c("count_notes","count_description","count_space","count_transit",
               "count_host","count_neighborhood")] = NULL

sapply(new_listings,class)

#new_listings = read.csv("listings_cleansed.csv", header = TRUE)

# divide into training set and testing set
train_ind <- sample(1:nrow(new_listings), 2/3*nrow(new_listings))
listings.train <- new_listings[train_ind, ]
listings.test <- new_listings[-train_ind, ]

listingsLM = lm( formula = price ~ ., data = listings.train )
summary(listingsLM)

#write.csv(new_listings, file = "temp_table.csv",row.names=FALSE)

write.csv(listings, file = "listings_cleansed.csv", row.names = FALSE)

