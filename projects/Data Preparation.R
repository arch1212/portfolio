library(dplyr)

# Importing the data
credit.card <- read.csv("Linear Regression Case.csv", na.strings = "#NULL!")

VarsSelected <- c("custid", "region", "townsize", "gender", "agecat", "birthmonth", "edcat", "jobcat",
                  "union", "empcat", "retire", "inccat", "debtinc", "creddebt", "othdebt", "default",
                  "jobsat", "marital", "spousedcat", "reside", "pets", "pets_cats", "pets_dogs", "pets_birds",
                  "pets_reptiles", "pets_small", "pets_saltfish", "pets_freshfish", "homeown", "hometype",
                  "addresscat", "cars", "carown", "cartype", "carcatvalue", "carbought", "carbuy", "commute",
                  "commutecat", "commutetime", "reason", "polview", "polparty", "polcontrib", "vote", "card", "cardtype",
                  "cardbenefit", "cardfee",  "cardtenurecat", "card2", "card2type", "card2benefit", "card2fee",
                  "card2tenurecat", "carditems", "card2items", "active", "bfast", "tenure", "churn", "longmon",
                  "longten", "tollfree", "tollmon", "tollten", "equip", "equipmon", "equipten", "callcard",
                  "cardmon", "cardten", "wireless", "wiremon", "wireten", "multline", "voice", "pager",
                  "internet", "callid", "callwait", "forward", "confer", "ebill", "owntv", "hourstv",
                  "ownvcr", "owndvd", "owncd", "ownpda", "ownpc", "ownipod", "owngame", "ownfax",
                  "news", "response_01", "response_02", "response_03", "cardspent", "card2spent")

# Select variables to keep
credit.card <- credit.card[,VarsSelected]

# Remove the column with Customer ID values
credit.card <- credit.card[,-1]

# Converting Categorical Features to Factors
# labeling no response as NAs
# keeping N/A as it is

credit.card$region <- factor(credit.card$region, levels = c(1,2,3,4,5), labels = c("Zone 1","Zone 2","Zone 3","Zone 4","Zone 5"))
credit.card$townsize <- factor(credit.card$townsize, levels = c(1,2,3,4,5), labels = c("> 250,000", "50,000-249,999",
                                                                                       "10,000-49,999",
                                                                                       "2,500-9,999",
                                                                                       "< 2,500")) 
credit.card$gender <- factor(credit.card$gender, levels = c(0,1), labels = c("Male", "Female"))
credit.card$agecat <- factor(credit.card$agecat, levels = c(1,2,3,4,5,6,9), labels = c("<18", "18-24",
                                                                                       "25-34",
                                                                                       "35-49",
                                                                                       "50-64",
                                                                                       ">65", "No Response"))
credit.card$edcat <- factor(credit.card$edcat, levels = c(1,2,3,4,5), labels = c("Did not complete high school","High school degree",
                                                                                 "Some college",
                                                                                 "College degree",
                                                                                 "Post-undergraduate degree"))
credit.card$jobcat <- factor(credit.card$jobcat, levels = c(1,2,3,4,5,6), labels = c("Managerial and Professional","Sales and Office",
                                                                                     "Service",
                                                                                     "Agricultural and Natural Resources",
                                                                                     "Precision Production, Craft, Repair",
                                                                                     "Operation, Fabrication, General Labor"))
credit.card$union <- factor(credit.card$union, levels = c(0,1), labels = c("No", "Yes"))
credit.card$empcat <- factor(credit.card$empcat, levels =c(1,2,3,4,5), labels = c("Less than 2",
                                                                                  "2 to 5",
                                                                                  "6 to 10",
                                                                                  "11 to 15",
                                                                                  "More than 15"))
credit.card$retire <- factor(credit.card$retire, levels = c(0,1), labels = c("No", "Yes"))
credit.card$inccat <- factor(credit.card$inccat, levels = c(1,2,3,4,5), labels = c("Under $25",
                                                                                   "$25 - $49",
                                                                                   "$50 - $74",
                                                                                   "$75 - $124",
                                                                                   "$125+"))
credit.card$default <- factor(credit.card$default, levels = c(0,1), labels = c("No", "Yes"))
credit.card$jobsat <- factor(credit.card$jobsat, levels = c(1,2,3,4,5), labels = c("Highly dissatisfied",
                                                                                   "Somewhat dissatisfied",
                                                                                   "Neutral",
                                                                                   "Somewhat satisfied",
                                                                                   "Highly satisfied"))
credit.card$marital <- factor(credit.card$marital, levels = c(0,1), labels=c("Unmarried",
                                                                             "Married"))
credit.card$spousedcat <- factor(credit.card$spousedcat, levels = c(-1,1,2,3,4,5), labels = c("Not married",
                                                                                              "Did not complete high school",
                                                                                              "High school degree",
                                                                                              "Some college",
                                                                                              "College degree",
                                                                                              "Post-undergraduate degree"))
credit.card$homeown <- factor(credit.card$homeown, levels = c(0,1), labels = c("Rent", "Own"))
credit.card$hometype <- factor(credit.card$hometype, levels = c(1,2,3,4), labels = c("Single-family",
                                                                                     "Multiple-Family",
                                                                                     "Condominium/Townhouse",
                                                                                     "Mobile Home"))
credit.card$addresscat <- factor(credit.card$addresscat, levels = c(1,2,3,4,5), labels = c("Less than 3",
                                                                                           "4 to 7",
                                                                                           "8 to 15",
                                                                                           "16 to 25",
                                                                                           "More than 25"))

credit.card$carown <- factor(credit.card$carown, levels =c(-1,0,1), labels = c("N/A", "Lease","Own"))
credit.card$cartype <- factor(credit.card$cartype, levels = c(-1,0,1), labels = c("N/A","Domestic", "Import"))
credit.card$carcatvalue <- factor(credit.card$carcatvalue, levels = c(-1,1,2,3), labels = c("N/A",
                                                                                            "Economy",
                                                                                            "Standard",
                                                                                            "Luxury"))
credit.card$carbought <- factor(credit.card$carbought, levels = c(-1,0,1), labels = c("N/A",
                                                                                      "No",
                                                                                      "Yes"))
credit.card$carbuy <- factor(credit.card$carbuy, levels = c(0,1), labels= c("No","Yes"))
credit.card$commute <- factor(credit.card$commute, levels = c(1,2,3,4,5,6,7,8,9,10), labels = c("Car",
                                                                                                "Motorcycle",
                                                                                                "Carpool",
                                                                                                "Bus",
                                                                                                "Train/Subway",
                                                                                                "Other public transit",
                                                                                                "Bicycle",
                                                                                                "Walk",
                                                                                                "Other non-motorized transit",
                                                                                                "Telecommute"))
credit.card$commutecat <- factor(credit.card$commutecat, levels = c(1,2,3,4,5), labels = c("Single occupancy",
                                                                                           "Multiple occupancy",
                                                                                           "Public transportation",
                                                                                           "Non-motorized",
                                                                                           "Telecommute"))

credit.card$reason <- factor(credit.card$reason, levels = c(1,2,3,4,8,9), labels = c("Prices",
                                                                                     "Convenience",
                                                                                     "Service",
                                                                                     "Other",
                                                                                     "N/A",
                                                                                     "Missing"))
credit.card$polview <- factor(credit.card$polview, levels = c(1,2,3,4,5,6,7), labels = c("Extremely liberal",
                                                                                         "Liberal",
                                                                                         "Slightly liberal",
                                                                                         "Moderate",
                                                                                         "Slightly conservative",
                                                                                         "Conservative",
                                                                                         "Extremely conservative"))
credit.card$polparty <- factor(credit.card$polparty, levels = c(0,1), labels = c("No", "Yes")) 
credit.card$polcontrib <- factor(credit.card$polcontrib, levels = c(0,1), labels = c("No", "Yes")) 
credit.card$vote <- factor(credit.card$vote, levels = c(0,1), labels = c("No", "Yes"))

credit.card$card <- factor(credit.card$card, levels = c(1,2,3,4,5), labels = c("American Express",
                                                                               "Visa",
                                                                               "Mastercard",
                                                                               "Discover",
                                                                               "Other"))
credit.card$cardtype <- factor(credit.card$cardtype, levels = c(1,2,3,4), labels = c("None",
                                                                                     "Gold",
                                                                                     "Platinum",
                                                                                     "Other")) 
credit.card$cardbenefit <- factor(credit.card$cardbenefit, levels = c(1,2,3,4), labels = c("None",
                                                                                           "Cash back",
                                                                                           "Airline miles",
                                                                                           "Other"))
credit.card$cardfee <- factor(credit.card$cardfee, levels = c(0,1), labels = c("No", "Yes"))
credit.card$cardtenurecat <- factor(credit.card$cardtenurecat, levels = c(1,2,3,4,5), labels = c("Less than 2",
                                                                                                 "2 to 5",
                                                                                                 "6 to 10",
                                                                                                 "11 to 15",
                                                                                                 "More than 15"))
credit.card$card2 <- factor(credit.card$card2, levels = c(1,2,3,4,5), labels = c("American Express",
                                                                                 "Visa",
                                                                                 "Mastercard",
                                                                                 "Discover",
                                                                                 "Other"))
credit.card$card2type <- factor(credit.card$card2type, levels = c(1,2,3,4), labels = c("None",
                                                                                       "Gold",
                                                                                       "Platinum",
                                                                                       "Other"))
credit.card$card2benefit <- factor(credit.card$card2benefit, levels = c(1,2,3,4), labels = c("None",
                                                                                             "Cash back",
                                                                                             "Airline miles",
                                                                                             "Other"))

credit.card$card2fee <- factor(credit.card$card2fee, levels = c(0,1), labels = c("No", "Yes"))
credit.card$card2tenurecat <-  factor(credit.card$card2tenurecat, levels = c(1,2,3,4,5), labels = c("Less than 2",
                                                                                                    "2 to 5",
                                                                                                    "6 to 10",
                                                                                                    "11 to 15",
                                                                                                    "More than 15"))
credit.card$active <- factor(credit.card$active, levels = c(0,1), labels = c("No", "Yes"))
credit.card$bfast <- factor(credit.card$bfast, levels = c(1,2,3), labels = c("Energy bar",
                                                                             "Oatmeal",
                                                                             "Cereal"))
credit.card$churn <- factor(credit.card$churn, levels = c(0,1), labels = c("No", "Yes"))
credit.card$tollfree <- factor(credit.card$tollfree, levels = c(0,1), labels = c("No", "Yes"))
credit.card$equip <- factor(credit.card$equip, levels = c(0,1), labels = c("No", "Yes"))
credit.card$callcard <- factor(credit.card$callcard, levels = c(0,1), labels = c("No", "Yes"))
credit.card$wireless <- factor(credit.card$wireless, levels = c(0,1), labels = c("No", "Yes"))
credit.card$multline <- factor(credit.card$multline, levels = c(0,1), labels = c("No", "Yes"))
credit.card$voice <- factor(credit.card$voice, levels = c(0,1), labels = c("No", "Yes"))
credit.card$pager <- factor(credit.card$pager, levels = c(0,1), labels = c("No", "Yes"))

credit.card$internet <- factor(credit.card$internet, levels = c(0,1,2,3,4), labels = c("None",
                                                                                       "Dial-up",
                                                                                       "DSL",
                                                                                       "Cable modem",
                                                                                       "Other"))

credit.card$callid <- factor(credit.card$callid, levels = c(0,1), labels = c("No", "Yes"))
credit.card$callwait <- factor(credit.card$callwait, levels = c(0,1), labels = c("No", "Yes"))
credit.card$forward <- factor(credit.card$forward, levels = c(0,1), labels = c("No", "Yes"))
credit.card$confer <- factor(credit.card$confer, levels = c(0,1), labels = c("No", "Yes"))
credit.card$ebill <- factor(credit.card$ebill, levels = c(0,1), labels = c("No", "Yes"))
credit.card$owntv <- factor(credit.card$owntv, levels = c(0,1), labels = c("No", "Yes"))
credit.card$ownvcr <- factor(credit.card$ownvcr, levels = c(0,1), labels = c("No", "Yes"))
credit.card$owndvd <- factor(credit.card$owndvd, levels = c(0,1), labels = c("No", "Yes"))
credit.card$owncd <- factor(credit.card$owncd, levels = c(0,1), labels = c("No", "Yes"))
credit.card$ownpda <- factor(credit.card$ownpda, levels = c(0,1), labels = c("No", "Yes"))
credit.card$ownpc <- factor(credit.card$ownpc, levels = c(0,1), labels = c("No", "Yes"))
credit.card$ownipod <- factor(credit.card$ownipod, levels = c(0,1), labels = c("No", "Yes"))
credit.card$owngame <- factor(credit.card$owngame, levels = c(0,1), labels = c("No", "Yes"))
credit.card$ownfax <- factor(credit.card$ownfax, levels = c(0,1), labels = c("No", "Yes"))
credit.card$news <- factor(credit.card$news, levels = c(0,1), labels = c("No", "Yes"))
credit.card$response_01 <- factor(credit.card$response_01, levels = c(0,1), labels = c("No", "Yes"))
credit.card$response_02 <- factor(credit.card$response_02, levels = c(0,1), labels = c("No", "Yes"))
credit.card$response_03 <- factor(credit.card$response_03, levels = c(0,1), labels = c("No", "Yes"))

# Creating Outcome Variable
credit.card <- credit.card %>% mutate(total.card.spend = cardspent + card2spent) %>% 
  select(-cardspent, -card2spent)

summary(credit.card)

# Writing the data to a csv file
write.csv(credit.card, "creditcard.csv", row.names = FALSE)
