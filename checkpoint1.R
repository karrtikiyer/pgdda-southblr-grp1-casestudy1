#required for converting char columns to dates in data frame
if(!"lubridate" %in% rownames(installed.packages()))   
  install.packages("lubridate")

#   After installing any package, we have to load it into the R environment.
library(lubridate)

#Assumption: The files to be read are in the current working directory,else use getwd and form file path as shown below
company.filename <- file.path(getwd(),"UPGRAD","casestudy1","casestudy","companies.txt")
rounds2.filename <- file.path(getwd(),"UPGRAD","casestudy1","casestudy","rounds2.csv")
#detected possible/probable file encoding using:
#f <- rawToChar(readBin(company.filename, "raw", 100000))
#stri_enc_detect(f)
df.companies <- read.delim(company.filename, header = T, sep = "\t", stringsAsFactors = F, encoding = "ISO-8859-1")
df.rounds2 <- read.csv(rounds2.filename, stringsAsFactors = F,encoding = "ISO-8859-1")

#Step1:convert to utf-8 all the character columns, this is required on Mac Machies very tolower or trimws fails without
#this conversion
df.companies[,sapply(df.companies,is.character)] <- sapply(
  df.companies[,sapply(df.companies,is.character)],
  iconv,"ISO-8859-1","UTF-8")
df.rounds2[,sapply(df.rounds2,is.character)] <- sapply(
  df.rounds2[,sapply(df.rounds2,is.character)],
  iconv,"ISO-8859-1","UTF-8")

#Step2:Trim white spaces from all character columns
df.companies[,sapply(df.companies,is.character)]<-sapply(
  df.companies[,sapply(df.companies,is.character)],
  trimws)
df.rounds2[,sapply(df.rounds2,is.character)]<-sapply(
  df.rounds2[,sapply(df.rounds2,is.character)],
  trimws)

#Step3:convert all character columns to lower case
df.companies[,sapply(df.companies,is.character)]<-sapply(
  df.companies[,sapply(df.companies,is.character)],
  tolower)
df.rounds2[,sapply(df.rounds2,is.character)]<-sapply(
  df.rounds2[,sapply(df.rounds2,is.character)],
  tolower)
#TODO: For comparison purposes below two variables, can be removed later
df.companies.1 <- df.companies
df.rounds2.1 <- df.rounds2

#Step3.1:Remove special characters and keep only alpha numeric
df.companies[,sapply(df.companies,is.character)]<-sapply(
  df.companies[,sapply(df.companies,is.character)],
  gsub,pattern="[^[:alnum:]///' ]",replacement="")

df.rounds2[,sapply(df.rounds2,is.character)]<-sapply(
  df.rounds2[,sapply(df.rounds2,is.character)],
  gsub,pattern="[^[:alnum:]///' ]",replacement="")

#Step4:convert columns to factor whichever applicable
#founding_round_type, founding_round_code, status, country code, state code, region, city
df.companies$status <- as.factor(df.companies$status)
df.rounds2$funding_round_type <- as.factor(df.rounds2$funding_round_type)
df.rounds2$funding_round_code <- as.factor(df.rounds2$funding_round_code)
df.companies$country_code <- as.factor(df.companies$country_code)
df.companies$state_code <- as.factor(df.companies$state_code)
df.companies$region <- as.factor(df.companies$region)
df.companies$city <- as.factor(df.companies$city)

#Step5:convert funded_at and founded_at columns to date
df.companies$founded_at <- dmy(df.companies$founded_at)
df.rounds2$funded_at <- dmy(df.rounds2$funded_at)

#Check point 1
#Q1-->How many unique companies are present in rounds2?
chkp1.q1.answer <-length(unique(df.rounds2$company_permalink))
#qs.answer1 <-length(unique(df.rounds2$company_permalink))
#Q2-->How many unique companies are present in companies ?
chkp1.q2.answer<-length(unique(df.companies$permalink))
#qs.answer2<-length(unique(df.companies$permalink))

#Q3--> In the companies data frame, which column can be used as the  unique key for each company? Write the name of the column.
chkp1.q3.answer<-"permalink variable in companies df"
#nrow(unique(df.companies[c("name","status")])) TODO: Composite key which can be used to eliminate closed companies

#Q4--> Are there any companies in the rounds2 file which are not  present in companies ? Answer Y/N.
chkp1.q4.answer.1 <- nrow(subset(df.rounds2,!(df.rounds2$company_permalink %in% df.companies$permalink)))
chkp1.q4.answer <- (chkp1.q4.answer.1>0)

#Q5--> Merge the two data frames so that all  variables (columns)  in the companies frame are added to the rounds2 data frame. Name the merged frame master_frame. How many observations are present in master_frame ?
chkp1.q5.answer <- merge(df.rounds2, df.companies, by.x = "company_permalink", by.y = "permalink", incomparables = NA)

master_frame <- chkp1.q5.answer

#Check point 2
chkp2.q1.answer <- nrow(subset(master_frame, is.na(master_frame$raised_amount_usd),select = raised_amount_usd))
#nrow(subset(master_frame, is.na(master_frame$raised_amount_usd) & tolower(master_frame$status)!="closed",select = raised_amount_usd))
master_frame <- subset(master_frame, !is.na(master_frame$raised_amount_usd))
#TODO: Percentange of NA's = 20028/115196 = 17.38%, need to decide if we want to remove the NA's or replace it with mean
chkp2.q2.answer <- "Remove the entries with NA in raised_amound_usd"
chkp2.q2.answer.1 <- mean(master_frame$raised_amount_usd, na.rm = T)
#convert USD to millions of USD
#TODO: how to decide the decimal point precision after division
master_frame$raised_amount_m_usd <- master_frame$raised_amount_usd/1000000
#Remove raised amount that are 0
master_frame <- subset(master_frame,!(master_frame$raised_amount_m_usd==0))
#remove companies that are in closed state
master_frame <- subset(master_frame,!(master_frame$status=="closed"))

str(master_frame)
summary(master_frame$raised_amount_m_usd)
