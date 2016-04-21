#Data Wrangling Assignment 1

#0:Load the data in RStudio
library(dplyr)
library(tidyr)
products <- read.csv("refine_original.csv", stringsAsFactors = FALSE)
inventory <- tbl_df(products)

#1:Clean up company names

#Replace company names with correct spelling by checking first letter
inventory$company <- sub("^(p|f).*","philips",inventory$company, ignore.case = TRUE)
inventory$company <- sub("^a.*","akzo",inventory$company, ignore.case = TRUE)
inventory$company <- sub("^v.*","van houten",inventory$company, ignore.case = TRUE)
inventory$company <- sub("^u.*","unilever",inventory$company, ignore.case = TRUE)

#2:Separate product code and number
inventory <- separate(inventory, Product.code...number,c("product_code", "product_number"), sep="-")

#3:Add product categories by a left_join
product_code <- c("p","v","x","q")
product_category <- c("Smartphone","TV","Laptop","Tablet")
product_key <- data.frame(product_code,product_category)
inventory <- left_join(inventory, product_key, by="product_code")

#Rearrange columns to have product category as second column
inventory <- inventory[c("company","product_category","product_code","product_number","address","city","country","name")]

#4:Concatenate the 3 address fields for geocoding
inventory <- unite(inventory,"full_address",address,city,country, sep=",")

#5:Add dummy variables for company and product category

#Add columns for dummy company variables, initialised to 0
companies <- c("company_philips", "company_akzo", "company_van_houten", "company_unilever")
inventory[c(companies)] <- 0

#Change value to 1 if company name matches column
for (i in 1:length(inventory$company)) {
  if (inventory$company[i] == "philips")  {
    inventory$company_philips[i] <- 1 
    
  } else if ((inventory$company[i]) == "akzo")  {
    inventory$company_akzo[i] <- 1
    
  } else if ((inventory$company[i]) == "van houten") {
    inventory$company_van_houten[i] <- 1
    
  } else if ((inventory$company[i]) == "unilever"){
    inventory$company_unilever[i] <- 1
  }
}

#Add columns for dummy product variables, initialised to 0 
products_list <- c("product_smartphone", "product_tv", "product_laptop", "product_tablet")
inventory[c(products_list)] <- 0

#Change value to 1 if product name matches column
for (i in 1:length(inventory$product_category)) {
  if (inventory$product_category[i] == "Smartphone")  {
    inventory$product_smartphone[i] <- 1  
    
  } else if ((inventory$product_category[i]) == "TV")  {
    inventory$product_tv[i] <- 1
    
  } else if ((inventory$product_category[i]) == "Laptop") {
    inventory$product_laptop[i] <- 1
    
  } else if ((inventory$product_category[i]) == "Tablet"){
    inventory$product_tablet[i] <- 1
  }
}

#6:Output to csv file for upload on github
write.csv(inventory, file = "refine_clean.csv")

