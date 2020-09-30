library(tidyverse)
df <- read.csv("D:/Data Analyst Technical Test/NH_Technical_Test.csv", header = TRUE)
View(df)
glimpse(df)
summary(df)

#no.4(a)
library(dplyr)
df %>%
  group_split(Duplicated = (add_count(.,invoice_id, invoice_created_at, invoice_paid_at) %>% pull(n)) > 1)

df1<-df %>% distinct(invoice_id, invoice_created_at, invoice_paid_at, .keep_all = TRUE)

library(plyr)

arrange(df1, desc(service_period), service_status, invoice_status)

head(arrange(df1, desc(service_period), service_status, invoice_status), 100)

df %>%
  group_split(Duplicated = (add_count(.,client_id) %>% pull(n)) > 1)

#no-4(b)
total<-aggregate(df1$total_price, by=list(product_id=df1$product_id), sum)
total_sales<-df1 %>% count(product_id)
colnames(total) = c("product_id", "total_revenue") 
colnames(total_sales) = c("product_id", "total_sales")
df.merge = merge(total, total_sales, all = TRUE)
arrange(df.merge, desc(total_revenue))

#no-4(c)
df1
df.subcol <- select(filter(df1, invoice_status == "Paid", service_status == "Active"), c(client_id, invoice_status, service_status, monthly_period))
df.subcol
df.monthlytotal<-aggregate(df.subcol$monthly_period, by=list(df.subcol$client_id, df.subcol$invoice_status, df.subcol$service_status), sum)
colnames(df.monthlytotal) = c("client_id", "invoice_status", "service_status", "monthly_total")
head(arrange(df.monthlytotal, desc(monthly_total)), 100)

#no.4(d)
library(lubridate)

rawdata <- df1 %>% 
  mutate_at(c("invoice_created_at", "invoice_paid_at"), as.POSIXct, format = "%Y-%m-%dT%H:%M:%SZ")



rawdata$seconds <- as.double(rawdata$invoice_paid_at - rawdata$invoice_created_at, units = "secs")


total_seconds <- sum(rawdata$seconds,na.rm=TRUE)
total_invoice <- nrow(df1)
avg_seconds <- total_seconds/total_invoice
convert_avg <- seconds_to_period(avg_seconds)

#no.4(e)
df.segment <-  select(filter(df1, client_task == "Activate"), c(client_id, client_task, monthly_period))
df.activate <- aggregate(df.segment$monthly_period, by=list(df.segment$client_id, df.segment$client_task), sum)      
colnames(df.activate) = c("client_id", "client_task", "monthly_period")

df.segment2 <- select(filter(df1, client_task == "Renew"), c(client_id, client_task, monthly_period))
df.renew <- aggregate(df.segment2$monthly_period, by=list(df.segment2$client_id, df.segment2$client_task), sum)
colnames(df.renew) = c("client_id", "client_task", "monthly_period")

#no.4(f)
summary(df)
