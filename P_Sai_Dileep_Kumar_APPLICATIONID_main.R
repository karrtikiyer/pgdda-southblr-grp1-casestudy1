#required for converting char columns to dates in data frame
if (!"lubridate" %in% rownames(installed.packages()))
  install.packages("lubridate")
#After installing any package, we have to load it into the R environment.
library(lubridate)

#required for finding and replacing strings
if (!"stringr" %in% rownames(installed.packages()))
  install.packages("stringr")
library(stringr)

#required for aggregation, selection filtering of data sets
if (!"dplyr" %in% rownames(installed.packages()))
  install.packages("dplyr")
library(dplyr)

#required for plotting
if (!"ggplot2" %in% rownames(installed.packages()))
  install.packages("ggplot2")
library(ggplot2)

#required for plotting
if (!"readr" %in% rownames(installed.packages()))
  install.packages("readr")
library(readr)


#Assumption: The files to be read are in the current working directory,
#else use getwd and form file path as shown below
company.filename <-
  file.path(getwd(), "companies.txt")
rounds2.filename <-
  file.path(getwd(), "rounds2.csv")

df.companies <-  read_tsv(company.filename)

df.rounds2 <-read_csv(rounds2.filename,col_types = "cccccn")

#Step3.1:For better and more accurate matching while merge:Remove from permalink special characters and
#keep only alpha numeric,slash, apostrophe, comma and hyphen
df.companies$permalink <- sapply(df.companies$permalink,
                                 gsub,
                                 pattern = "[^[:alnum:]///'-]",
                                 replacement = "")
df.rounds2$company_permalink <- sapply(df.rounds2$company_permalink,
                                       gsub,
                                       pattern = "[^[:alnum:]///'-]",
                                       replacement = "")

#Step3:convert all character columns to lower case
df.companies[, sapply(df.companies, is.character)] <-
  sapply(df.companies[, sapply(df.companies, is.character)],
         tolower)
df.rounds2[, sapply(df.rounds2, is.character)] <-
  sapply(df.rounds2[, sapply(df.rounds2, is.character)],
         tolower)
#only keep country code in upper case
df.companies$country_code <- toupper(df.companies$country_code )
#Step4:convert columns to factor whichever applicable
#founding_round_type, founding_round_code, status, country code, state code, region, city
df.companies$status <- as.factor(df.companies$status)
df.rounds2$funding_round_type <-
  as.factor(df.rounds2$funding_round_type)
df.rounds2$funding_round_code <-
  as.factor(df.rounds2$funding_round_code)
df.companies$country_code <- as.factor(df.companies$country_code)
df.companies$state_code <- as.factor(df.companies$state_code)
df.companies$region <- as.factor(df.companies$region)
df.companies$city <- as.factor(df.companies$city)

#Step5:convert funded_at and founded_at columns to date
df.companies$founded_at <- parse_date_time(df.companies$founded_at, 
                                           orders=c("d-m-y","y-m-d"))
df.rounds2$funded_at <- dmy(df.rounds2$funded_at)

#Check point 1
#Q1-->How many unique companies are present in rounds2?
chkp1.q1.answer <- length(unique(df.rounds2$company_permalink))

#Q2-->How many unique companies are present in companies ?
chkp1.q2.answer <- length(unique(df.companies$permalink))


#Q3--> In the companies data frame, which column can be used as the  unique key for each company? Write the name of the column.
chkp1.q3.answer <- "permalink variable in companies df"


#Q4--> Are there any companies in the rounds2 file which are not  present in companies ? Answer Y/N.
chkp1.q4.answer.1 <-
  nrow(subset(
    df.rounds2,!(df.rounds2$company_permalink %in% df.companies$permalink)
  ))
chkp1.q4.answer <- (chkp1.q4.answer.1 > 0)

#Q5--> Merge the two data frames so that all  variables (columns)  in the companies frame are added to the rounds2 data frame. Name the merged frame master_frame. How many observations are present in master_frame ?
master_frame <-
  merge(
    df.rounds2,
    df.companies,
    by.x = "company_permalink",
    by.y = "permalink",
    incomparables = NA
  )

chkp1.q5.answer <- nrow(master_frame)

#convert data frame to table df for use in dplyr
master_frame <- tbl_df(master_frame)

#remove /organization/ from permalink - Constant across all values, not needed
master_frame$company_permalink <-
  str_replace(master_frame$company_permalink,
              pattern = "/organization/",
              replacement = "")

#remove /funding-round/ from funding_round_permalink - Constant across all values, not needed
master_frame$funding_round_permalink <-
  str_replace(master_frame$funding_round_permalink,
              pattern = "/funding-round/",
              replacement = "")


#Check point 2
chkp2.q1.answer <-
  nrow(subset(master_frame, is.na(master_frame$raised_amount_usd), select = raised_amount_usd))
chkp2.total0s <-
  nrow(subset(master_frame, (master_frame$raised_amount_usd == 0)))

#Questions to ask ourselves and discuss:
#Should we be and when to remove outliers
#Should we remove companies that are in closed state? May be not since
#that is not a criteria, we are only looking at average fundings in each type
#master_frame <- subset(master_frame,!(master_frame$status=="closed"))

#NA and O removal strategies
strategies <- factor(c("Omit", "Mean", "MeanRespFT"))
#"MeanRespFT" explanation "Mean of Respective Funding Types"
#Using MeanRespFT as the adopted one for the rest of calculations
adopted_strategy <- strategies[3]

if (adopted_strategy == strategies[1]) {
  #strategy1: Remove observations that have NA's and 0 value in raised_amount_usd
  print("Omit Strategy Used")
  chkp2.q2.answer <- "Remove all NA's and 0's"
  master_frame <-
    subset(master_frame,!is.na(master_frame$raised_amount_usd))
  #Remove raised amount that are 0
  master_frame <-
    subset(master_frame, !(master_frame$raised_amount_usd == 0))
} else if (adopted_strategy == strategies[2]) {
  #strategy2: Use mean for NA's and 0 values in raised_amount_usd
  print("Mean Strategy Used")
  chkp2.q2.answer <- mean(master_frame$raised_amount_usd, na.rm = T)
  master_frame[which(is.na(master_frame$raised_amount_usd)), "raised_amount_usd"] <-
    mean(master_frame$raised_amount_usd, na.rm = T)
  master_frame[which(master_frame$raised_amount_usd == 0), "raised_amount_usd"] <-
    mean(master_frame$raised_amount_usd, na.rm = T)
} else if (adopted_strategy == strategies[3]) {
  #strategy3: Use mean per round of its respective founding round type for NA's and O values
  print("MeanRespFT Strategy Used")
  chkp2.q2.answer <- master_frame %>%
    group_by(frt = funding_round_type) %>%
    summarise(
      avg_usd = round(mean(raised_amount_usd, na.rm = T) / n(), 2),
      avg_usd_ft = mean(raised_amount_usd, na.rm = T)
    )
  
  master_frame <- master_frame %>%
    inner_join(chkp2.q2.answer, by = c("funding_round_type" =
                                         "frt"))
  master_frame[which(is.na(master_frame$raised_amount_usd)),
               "raised_amount_usd"] <-
    master_frame[which(is.na(master_frame$raised_amount_usd)), ] %>%
    select(avg_usd)
  
  master_frame[which(master_frame$raised_amount_usd == 0),
               "raised_amount_usd"] <-
    master_frame[which(master_frame$raised_amount_usd == 0), ] %>%
    select(avg_usd)
  #Remove the avg_usd column as it is no more needed
  master_frame <- master_frame %>% select(-avg_usd,-avg_usd_ft)
}

#Add a new column:convert USD to millions of USD and add a new column since raised_amount_usd when plotted gives
#exponential valus such as 2e6 which is not very understandble.
master_frame <- master_frame %>%
  mutate(raised_amount_m_usd = round(master_frame$raised_amount_usd / 1000000, digits =
                                       2))

#Checkpoint 3
#step 1: group by found round type and calculate average raising fund in USD and Millions USD per type
chkp3 <- master_frame %>%
  group_by(funding_round_type) %>%
  summarise(
    avg_usd = mean(raised_amount_usd, na.rm = T),
    avg_m_usd = mean(raised_amount_m_usd, na.rm = T),
    avg_per_round = round(avg_m_usd/n_distinct(funding_round_code),2)
  )

#step 2: find average for venture, angel, seed and private equity type
chkp3.q1toq4 <- chkp3 %>%
  filter(funding_round_type %in% c("venture", "angel", "seed", "private_equity")) %>%
  select(funding_round_type, avg_usd, avg_m_usd, avg_per_round)
#step 3: Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round,
#which investment type is the most suitable for them?
#define funding range in millions of USD
funding_range <- range(5, 15)
chkp3.q5 <- chkp3.q1toq4 %>%
  filter(floor(avg_per_round) >= funding_range[1], floor(avg_per_round) <= funding_range[2]) %>%
  arrange(desc(avg_m_usd)) %>%
  summarise(FT = first(funding_round_type),
            m_usd = first(avg_m_usd),
            m_usd_per_round = first(avg_per_round)) %>%
  summarise(FT)

#Checkpoint 4: English speaking countries
#in-memory vector of english speaking countries from the pdf file shared by UPGRAD
eng_spkng_countries <-
  as.factor(toupper(
    c(
      "BWA",
      "CMR",
      "ETH",
      "ERI",
      "GMB",
      "GHA",
      "KEN",
      "LSO",
      "LBR",
      "MWI",
      "MUS",
      "NAM",
      "NGA",
      "RWA",
      "SYC",
      "SLE",
      "ZAF",
      "SSD",
      "SDN",
      "SWZ",
      "TZA",
      "UGA",
      "ZMB",
      "ZWE",
      "ATG",
      "BHS",
      "BRB",
      "BLZ",
      "CAN",
      "DMA",
      "GRD",
      "GUY",
      "JAM",
      "KNA",
      "LCA",
      "VCT",
      "TTO",
      "USA",
      "IRL",
      "MLT",
      "GBR",
      "AUS",
      "FJI",
      "KIR",
      "MHL",
      "FSM",
      "NRU",
      "NZL",
      "PLW",
      "PNG",
      "WSM",
      "SLB",
      "TON",
      "TUV",
      "VUT",
      "IND",
      "PAK",
      "PHL",
      "SGP"
    )
  ))
#For FT type of investment which is the suggested investment type, find top 9 english speaking countries
top9 <- master_frame %>%
  filter(funding_round_type %in% chkp3.q5$FT,
         country_code %in% eng_spkng_countries) %>%
  group_by(country_code) %>%
  summarise(
    count = n(),
    total_inv = sum(raised_amount_usd, na.rm = T),
    total_m_inv = sum(raised_amount_m_usd, na.rm = T)
  ) %>%
  arrange(desc(total_inv)) %>%
  top_n(n = 9, wt = total_inv)
#top 3 countries: Analysing top 3 English speaking countries
chkp4.q1 <- top9 %>%
  top_n(n = 3, wt = total_inv) %>%
  select(country_code)

#Checkpoint 5: Sector analysis
#read mappings file
mappings.filename <-
  file.path(getwd(), "mapping_file.csv")
df.mappings <-
  read.csv(mappings.filename,
           stringsAsFactors = F)
#Trim white spaces from all character columns
df.mappings[, sapply(df.mappings, is.character)] <-
  sapply(df.mappings[, sapply(df.mappings, is.character)],
         trimws)
#convert all character columns to lower case
df.mappings[, sapply(df.mappings, is.character)] <-
  sapply(df.mappings[, sapply(df.mappings, is.character)],
         tolower)
#convert columns to factor whichever applicable
df.mappings$category_list <- as.factor(df.mappings$category_list)
df.mappings$main_sector <- as.factor(df.mappings$main_sector)
#extract primary sector from category list
master_frame$primary_sector <- master_frame$category_list %>%
  sapply(function(x) {
    splitvec <- unlist(strsplit(x, split = '\\|'))
    ifelse(length(splitvec) >= 1, tolower(trimws(splitvec[1])), "")
  })
#convert primary sector to factors
master_frame$primary_sector <-
  as.factor(master_frame$primary_sector)
#merge master_frame and mappings csv using primary_sector and category_list as common keys
master_frame <-
  merge(
    master_frame,
    df.mappings,
    by.x = "primary_sector",
    by.y = "category_list",
    incomparables = NA
  )

#Checkpoint 6
#Filter on preferred funding type, top 3 english speaking countries, investment in range of 5 to 15 million USD
final_frame <- master_frame %>%
  filter(
    funding_round_type %in% chkp3.q5$FT,
    country_code %in% chkp4.q1$country_code,
    raised_amount_m_usd >= funding_range[1] &
      raised_amount_m_usd <= funding_range[2]
  )
#Below actually not required since we already have final_frame, but since it is asked in case study
#explicity it is done
DF1 <- final_frame %>%
  filter(country_code == chkp4.q1$country_code[1])
DF2 <- final_frame %>%
  filter(country_code == chkp4.q1$country_code[2])
DF3 <- final_frame %>%
  filter(country_code == chkp4.q1$country_code[3])


chkp6.q1q2 <- final_frame %>%
  group_by(country_code) %>%
  summarise(num_inv = n(),
            total_inv = sum(raised_amount_usd)) %>%
  arrange(desc(total_inv), num_inv)

# chkp6.q3toq8 <- final_frame %>%
#   group_by(country_code, main_sector) %>%
#   summarise(num_inv = n(),
#             total_inv = sum(raised_amount_usd)) %>%
#   top_n(3, wt = num_inv) %>% 
#   mutate(inv_rank = dense_rank(desc(num_inv))) %>% 
#   arrange(inv_rank, desc(num_inv), desc(total_inv))

chkp6.q3toq8 <- final_frame %>%
  group_by(country_code, main_sector) %>%
  summarise(num_inv = n(),
            total_inv = sum(raised_amount_usd)) %>% 
  arrange(country_code, desc(num_inv), desc(total_inv)) %>% 
  mutate(inv_rank = dense_rank(desc(num_inv))) %>% 
  top_n(3, wt = total_inv)

chkp6.q9q10 <- final_frame %>%
  inner_join(chkp6.q3toq8,
             by = c("country_code" = "country_code",
                    "main_sector" = "main_sector")) %>%
  #filter(funding_round_type %in% chkp3.q5$FT) %>%
  group_by(country_code, main_sector, company_permalink) %>%
  summarise(max_inv = sum(raised_amount_m_usd, na.rm = T)) %>%
  top_n(1, wt = max_inv)



#Checkpoint 7: Plots
if (!"scales" %in% rownames(installed.packages()))
  install.packages("scales")
#   After installing any package, we have to load it into the R environment.
library(scales)
#7.1 fraction of total investments (globally) in venture, seed and
#private equity and the average amount of investment in each funding type
g1_data <-
  chkp3.q1toq4 %>% filter(funding_round_type %in% c("venture", "seed", "private_equity"))
g1 <-
  ggplot(g1_data, aes(x = "", y = avg_per_round, fill = funding_round_type)) +
  geom_bar(width = 1, stat = "identity")
g1 <- g1 + scale_y_continuous(labels = comma)
g1 <- g1 + coord_polar("y", start = 0)
g1 <- g1 + labs(x = "", y = "% Investments USD")
g1 <- g1 + scale_fill_discrete(name = "Funding Type")
g1  + geom_text(aes(y = avg_per_round / 3 + c(0, cumsum(avg_per_round)[-length(avg_per_round)]),
                    label = str_c(round(
                      avg_per_round / sum(avg_per_round) * 100, 2
                    ), "%")), size = 2.5)
#7.2 countries vs total investments
g2 <- ggplot(top9, aes(x = reorder(country_code, -total_m_inv)))
g2 <- g2 + geom_bar(aes(weight = total_m_inv), fill = "orange")
g2 <-
  g2 + labs(title = "Countries vs Total Investments", x = "Country", y =
              "Investments(million USD)")
g2 + scale_y_continuous(labels = comma)
#7.3: show the number of investments in the top 3 sectors of
#the top 3 countries on one chart (for the chosen investment type FT)
g3 <- ggplot(chkp6.q3toq8, aes(x = reorder(country_code, -num_inv)))
g3 <-
  g3 + geom_bar(aes(weight = num_inv, fill = factor(main_sector)), position = "dodge")
g3 <- g3 + labs(x = "Country", y = " Number of Investments")
g3 <- g3 + scale_y_continuous(labels = comma)
g3 + scale_fill_discrete(name = "Sectors")
