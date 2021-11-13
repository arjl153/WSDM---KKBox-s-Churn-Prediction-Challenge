library(tibble) #for as.tibble
library(data.table) #for fread()
library(magrittr)
library(dplyr)
library(tidyr)
library(forcats)
library(gridExtra)

#for date
library(lubridate)

# general visualisation
library('ggplot2') # visualisation
library('scales') # visualisation
library('grid') # visualisation
library('gridExtra') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation

library('ggforce') # visualisation
library('ggridges') # visualisation


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# function to extract binomial confidence levels
get_binCI <- function(x,n) as.list(setNames(binom.test(x,n)$conf.int, c("lwr", "upr")))

train = as.tibble(fread('train.csv', sep=',', header=T, stringsAsFactors = T, colClasses = c("is_churn"="factor")))
memb = as.tibble(fread('members_v3.csv', sep=',', header=T, stringsAsFactors = T, colClasses = c("city"="factor", "gender"="factor", "registered_via"="factor"), nrows=1e6))
trans = as.tibble(fread("transactions.csv", sep=',', header=T, stringsAsFactors = T, colClasses = c("payment_method_id"="factor", "is_auto_renew"="factor", "is_cancel"="factor"), nrows=1e6))
userl = as.tibble(fread("user_logs_v2.csv", sep=',', header=T, stringsAsFactors = T, nrows=1e6))

grid.table(head(train))
grid.table(head(memb))
grid.table(head(trans))
grid.table(head(userl))

#init merge data-set
merge1 = merge(train, memb, by="msno")
merge2 = merge(merge1, trans, by="msno")
t_merge = merge(merge2, userl, by="msno")
length(t_merge$msno)

#hypothesis1: what are the type of ppl who churned
churned = t_merge[t_merge$is_churn == 1, ]
not_churned = t_merge[t_merge$is_churn == 0, ]
length(churned$msno)
length(not_churned$msno)

View(churned)
set.seed(123)
not_churned.msno = sample(not_churned$msno, size=1500, replace=FALSE)
length(unique(not_churned.msno))

not_churned = not_churned[not_churned$msno %in% not_churned.msno, ]
not_churned = not_churned[!duplicated(not_churned$msno), ]
length(unique(not_churned$msno))

table(churned$is_auto_renew)
520/(945+520) #0.3549

table(not_churned$is_auto_renew)
1337/(130+1337) #0.911


churned.regvia = as.data.frame(table(churned$registered_via))
not_churned.regvia = as.data.frame(table(not_churned$registered_via))
table(churned$registered_via)

p3 <- churned %>%
  ggplot(aes(registered_via, fill = registered_via)) +
  geom_bar() +
  theme(legend.position = "none") +
  scale_y_sqrt() +
  theme(axis.title=element_text(size=20, face="bold")) +
  theme(axis.text=element_text(size=15, face="bold"))

print(p3)

p3 <- not_churned %>%
  ggplot(aes(registered_via, fill = registered_via)) +
  geom_bar() +
  theme(legend.position = "none") +
  scale_y_sqrt() +
  theme(axis.title=element_text(size=20, face="bold")) +
  theme(axis.text=element_text(size=15, face="bold"))

print(p3)

table(churned$actual_amount_paid)
table(not_churned$actual_amount_paid)

churned$discount = churned$plan_list_price - churned$actual_amount_paid
churned$if_discount = if_else(churned$discount > 0, 1, 0) %>% as.integer()
churned_if_discount = as.data.frame(table(churned$if_discount))

churned_if_discount$pct = churned_if_discount$Freq / sum(churned_if_discount$Freq)
head(churned_if_discount)

not_churned$discount = not_churned$plan_list_price - not_churned$actual_amount_paid
not_churned$if_discount = if_else(not_churned$discount > 0, 1, 0) %>% as.integer()
not_churned_if_discount = as.data.frame(table(not_churned$if_discount))

not_churned_if_discount$pct = not_churned_if_discount$Freq / sum(not_churned_if_discount$Freq)
head(not_churned_if_discount)


mean(churned$bd) #median age was 23
mean(not_churned$bd) #also 23

mean(churned$num_unq)
mean(not_churned$num_unq)

mean(churned$num_25)
mean(not_churned$num_25)

mean(churned$num_50)
mean(not_churned$num_50)

mean(churned$num_75)
mean(not_churned$num_75)

mean(churned$num_985)
mean(not_churned$num_985)
mean(churned$num_100)
mean(not_churned$num_100)

mean(churned$total_secs)
mean(not_churned$total_secs)

#feature visualizations
p1 <- train %>%
  ggplot(aes(is_churn, fill = is_churn)) +
  geom_bar() +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=20, face="bold")) +
  theme(axis.text=element_text(size=15, face="bold"))

#print(p1)






p2 <- memb %>%
  ggplot(aes(gender, fill = gender)) +
  geom_bar() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size=22)) +
  theme(axis.title=element_text(size=20, face="bold")) +
  theme(axis.text=element_text(size=15, face="bold"))

#print(p2)



p3 <- memb %>%
  ggplot(aes(registered_via, fill = registered_via)) +
  geom_bar() +
  theme(legend.position = "none") +
  scale_y_sqrt() +
  theme(axis.title=element_text(size=20, face="bold")) +
  theme(axis.text=element_text(size=15, face="bold"))

#print(p3)


p4 <- memb %>%
  ggplot(aes(city, fill = city)) +
  geom_bar() +
  theme(legend.position = "none") +
  scale_y_sqrt() +
  theme(axis.title=element_text(size=20, face="bold")) +
  theme(axis.text=element_text(size=15, face="bold"))

#print(p4)


p5 <- memb %>%
  filter(bd > 0 & bd < 100) %>%
  ggplot(aes(bd)) +
  geom_density(fill = "red", bw = 1) +
  theme(axis.title=element_text(size=20, face="bold")) +
  theme(axis.text=element_text(size=15, face="bold"))

#print(p5)

layout <- matrix(c(1,1,2,2,3,3,4,4,4,5,5,5),2,6,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, layout=layout)

#a vast majority did not churn
train %>%
  group_by(is_churn) %>%
  summarise(percentage = n()/nrow(train)*100)


#registration_init_time
tmp_memb <- memb %>%
  mutate(registration_init_time = ymd(registration_init_time))

p6 <- tmp_memb %>%
  ggplot(aes(registration_init_time)) +
  geom_freqpoly(color = "dark green", binwidth = 1)

#print(p6)


p7 <- tmp_memb %>%
  mutate(wday = wday(registration_init_time, label = TRUE)) %>%
  ggplot(aes(wday, fill = wday)) +
  geom_bar() +
  theme(legend.position = "none") +
  labs(x = "Day of the week")

#print(p7)


p8 <- tmp_memb %>%
  filter(registration_init_time > ymd("20041231") & registration_init_time < ymd("20170101")) %>%
  mutate(month = month(registration_init_time, label = TRUE)) %>%
  ggplot(aes(month, fill = month)) +
  geom_bar() +
  theme(legend.position = "none") +
  labs(x = "Month of the year")

#print(p8)

layout <- matrix(c(1,1,2,3),2,2,byrow=TRUE)
multiplot(p6, p7, p8, layout=layout)

#user-logs
u1 <- userl %>%
  count(msno) %>%
  ggplot(aes(n)) +
  geom_bar(fill = "blue") +
  labs(x = "Entries per user") +
  theme(axis.title=element_text(size=20, face="bold")) +
  theme(axis.text=element_text(size=15, face="bold"))

#print(u1)


u2 <- userl %>%
  filter(abs(total_secs)<1e5) %>%
  ggplot(aes(total_secs)) +
  geom_vline(xintercept = median(userl$total_secs), linetype = 2) +
  geom_density(fill = "blue", alpha = 0.5) +
  scale_x_log10() +
  theme(axis.title=element_text(size=20, face="bold")) +
  theme(axis.text=element_text(size=15, face="bold"))

print(median(userl$total_secs))
#print(u2)


u3 <- userl %>%
  ggplot(aes(num_unq)) +
  geom_vline(xintercept = median(userl$num_unq), linetype = 2) +
  geom_histogram(binwidth = .05, fill = "red", alpha = 0.7) +
  scale_x_log10() +
  theme(axis.title=element_text(size=20, face="bold")) +
  theme(axis.text=element_text(size=15, face="bold"))

print(median(userl$num_unq))
#print(u3)

u4 <- userl %>%
  gather(num_25, num_50, num_75, num_985, num_100, key = "slen", value = "cases") %>%
  mutate(slen = fct_relevel(factor(slen),"num_100", after = Inf)) %>%
  ggplot(aes(cases, fill = slen)) +
  geom_density(position = "stack", bw = .1) +
  scale_x_log10(lim = c(1,800)) +
  labs(x = "Number of songs", fill = "% played") +
  theme(axis.title=element_text(size=20, face="bold")) +
  theme(axis.text=element_text(size=15, face="bold"))

print(u4)

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)

tmp_userl <- userl %>%
  mutate(date = ymd(date))

u5 <- tmp_userl %>%
  count(date) %>%
  ggplot(aes(date,n)) +
  geom_line(color = "blue") +
  theme(axis.title=element_text(size=20, face="bold")) +
  theme(axis.text=element_text(size=15, face="bold"))

#print(u5)


u6 <- tmp_userl %>%
  mutate(wday = wday(date, label = TRUE)) %>%
  ggplot(aes(wday, fill = wday)) +
  geom_bar() +
  theme(legend.position = "none") +
  labs(x = "Day of the week") +
  theme(axis.title=element_text(size=20, face="bold")) +
  theme(axis.text=element_text(size=15, face="bold"))

#print(u6)


layout <- matrix(c(1,1,2,3),2,2,byrow=TRUE)
multiplot(p5, p6, layout=layout)


#predictive analysis
sample_ids1 = select(train, msno) %>% distinct(msno)
#summary(sample_ids1) glimpse(sample_ids1)
#length(sample_ids1$msno) length(unique(sample_ids1$msno))

#keep only those rows with non-empty gender (392,056/1,000,000) => drop gender??
#memb1 = memb[which(as.character(memb$gender)== "female" | as.character(memb$gender)== "male"), ]
#glimpse(memb1)

sample_ids2 = memb %>% filter(msno %in% sample_ids1$msno ) %>% group_by(msno) %>% distinct(msno)
#summary(sample_ids2) glimpse(sample_ids2)
#length(sample_ids2$msno) length(unique(sample_ids2$msno))

sample_ids3 = trans %>% filter(msno %in% sample_ids2$msno) %>% group_by(msno) %>% distinct(msno)
#summary(sample_ids3) glimpse(sample_ids3)
#length(sample_ids3$msno) length(unique(sample_ids3$msno))

sample_ids4 = userl %>% filter(msno %in% sample_ids3$msno) %>% group_by(msno) %>% distinct(msno)
#summary(sample_ids4) glimpse(sample_ids4)
length(sample_ids4$msno) 
length(unique(sample_ids4$msno))


#sampling, set random seed to generate similar sample each time
set.seed(123)
sample_msno = sample(sample_ids4$msno, size=30000, replace=FALSE)
#length(sample_msno) length(unique(sample_msno))


train_s = train[train$msno %in% sample_msno, ]
memb_s = memb[memb$msno %in% sample_msno, ]
trans_s = trans[trans$msno %in% sample_msno, ]
userl_s = userl[userl$msno %in% sample_msno, ]

trans_s = trans_s[!duplicated(trans_s$msno), ]
length(trans_s$msno)

userl_s = userl_s[!duplicated(userl_s$msno), ]
length(userl_s$msno)

length(unique(train_s$msno))
length(unique(memb_s$msno))
length(unique(trans_s$msno))
length(unique(userl_s$msno))


fwrite(train_s, file='data/train_s.csv', append=FALSE, quote="auto")
fwrite(memb_s, file='data/memb_s.csv', append=FALSE, quote="auto")
fwrite(trans_s, file='data/trans_s.csv', append=FALSE, quote="auto")
fwrite(userl_s, file='data/userl_s.csv', append=FALSE, quote="auto")


#data cleaning
#train_s
train_s_c = train_s
#train_s_c <- train_s %>%
#  mutate(is_churn = factor(is_churn))

for (i in names(train_s_c)){
  print(sum(is.na(train_s_c$i)))
} #no NA values

#memb_s
memb_s_c = memb_s %>% 
  select( -c(gender) ) #dropping gender as too many empty values
memb_s_c = memb_s_c %>%
  mutate(bd = if_else(memb_s_c$bd < 0 | memb_s_c$bd > 100, as.integer(median(memb_s_c$bd, na.rm=T)), memb_s_c$bd))
summary(memb_s_c)
memb_s_c = memb_s_c %>% transform(registration_init_time = as.Date(as.character(memb_s_c$registration_init_time), "%Y%m%d"))


#trans_s
length(trans_s$msno)
trans_s_c = trans_s
trans_s_c = trans_s_c %>% transform(membership_expire_date = as.Date(as.character(trans_s_c$membership_expire_date), "%Y%m%d"))
trans_s_c = trans_s_c %>% transform(transaction_date = as.Date(as.character(trans_s_c$transaction_date), "%Y%m%d"))


#userl_s
#if < 0, replace w/ 0.000001 elif > 24hours/day replace w/ 24hrs
userl_s_c = userl_s %>% 
  mutate(total_secs = if_else(userl_s$total_secs <0, 0.000001, if_else(userl_s$total_secs > 86400, 86400, userl_s$total_secs)))

userl_s_c = userl_s_c %>% transform(date = as.Date(as.character(userl_s_c$date), "%Y%m%d"))


#creating feature-set
#trans_s
trans_s_c$discount = trans_s_c$plan_list_price - trans_s_c$actual_amount_paid
trans_s_c$if_discount = if_else(trans_s_c$discount > 0, 1, 0) %>% as.integer()

trans_s_c$amount_paid_per_day = trans_s_c$actual_amount_paid / trans_s_c$payment_plan_days
trans_s_c$membership_duration = trans_s_c$membership_expire_date - trans_s_c$transaction_date

sum(is.infinite(trans_s_c$amount_paid_per_day)) #has inf values

trans_s_c=trans_s_c[!is.infinite((trans_s_c$amount_paid_per_day)),]
sum(is.infinite(trans_s_c$amount_paid_per_day))

trans_s_c=trans_s_c[!is.na((trans_s_c$amount_paid_per_day)),]
sum(is.na(trans_s_c$amount_paid_per_day))

#glimpse(trans_s_c) summary(trans_s_c)


glimpse(train_s_c)
summary(train_s_c)
glimpse(memb_s_c)
summary(memb_s_c)
glimpse(trans_s_c)
summary(trans_s_c)
glimpse(userl_s_c)
summary(userl_s_c)


#left join the data-sets
joint1 <- train_s_c %>% 
  left_join(memb_s_c, by='msno') %>%
  left_join(trans_s_c, by='msno') %>%
  left_join(userl_s_c, by='msno') 

#for some reason, we've to do this
joint1=joint1[!is.na((joint1$amount_paid_per_day)),]
sum(is.na(joint1$amount_paid_per_day))

length(joint1$msno)

#splitting into train/test
set.seed(321)
test_msno = sample(joint1$msno, size=0.2*nrow(joint1))
length(test_msno)
testing_set = joint1[joint1$msno %in% test_msno, 2:ncol(joint1)] 
length(testing_set$is_churn)

'%!in%' = function(x, y)!('%in%'(x,y)) #make a new 'not in' operator

training_set = joint1[joint1$msno %!in% test_msno, 2:ncol(joint1)]
length(training_set$is_churn)
View(training_set)

summary(training_set)
glimpse(training_set)


model = glm(is_churn ~ ., data=training_set, family=binomial(link="logit"), na.action = na.exclude)
summary(model)
p=predict(model, newdata = testing_set, type="response")
results = ifelse(p>0.5,1,0)
mce = mean(results!=testing_set$is_churn)
print(1-mce)


if(FALSE){
  library(tree)
  model = tree(is_churn~., data=training_set)
  summary(model)
  plot(model)
  text(model, pretty=0)
  training_set$is_churn = as.factor(training_set$is_churn)
}