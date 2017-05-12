#setwd("/Users/siyuan/Documents/Spring 2017/Orie 4740 - Data Mining/seattle-airbnb-open-data")

calendar = read.csv("calendar.csv", header = TRUE)
listings = read.csv("listings.csv", header = TRUE)
reviews = read.csv("reviews.csv", header = TRUE)

listings[c("thumbnail_url", "medium_url", "picture_url", "xl_picture_url", 
           "host_url", "host_thumbnail_url", "host_picture_url", "host_neighbourhood", 
           "listing_url", "experiences_offered", "id", "scrape_id", "last_scraped", "name", "host_id",
           "host_name", "host_since", "host_location", "host_acceptance_rate", "host_verifications",
           "host_has_profile_pic", "host_identity_verified", "street", "neighbourhood",
           "city", "state", "market", "zipcode", "smart_location", "country_code", "country",
           "is_location_exact", "square_feet", "weekly_price", "monthly_price", "maximum_nights",
           "calendar_updated", "has_availability", "calendar_last_scraped", "first_review", "last_review",
           "requires_license", "juridication_names")] = NULL

sum(is.na(listings$price))


#listing_clean = subset(listings, select = c("type", "state", "opensecrets_id"))