#This code is for exploratory analysis of data from Ponpare coupon website.
#"Load and transform data.R" must be run before to load and transform data

######################### coupon_visit_train #######################

counts<-table(coupon_visit_train$PURCHASE_FLG) # Buil and save contigency table

#Create barplot to visualise numbers of viewed and purchased coupons 
barplot(counts, names.arg=c("Viewed coupons", "Purchased coupons"), 
        main= "Browsed and purchased coupons during training time period", 
        ylab="Browsed coupons, train set, *10^5", ylim=c(0,3000000), yaxt="n")
axis(side=2, at=c(0, 1000000, 2000000,3000000),  labels=c(0, 10, 20,30))
text(x=0.7, y=2000000, "95.7%", col="blue", cex=2)
text(1.9,1000000, "4.3%", col="red", cex=2)

#Create barplot to visualise time of the day for coupons browsing
counts_Hour<-table(coupon_visit_train$Hour)#build and save contigency table)
barplot(counts_Hour, 
        main="Time of coupons browsing",
        ylab="Number of coupons, *10^3", ylim=c(0, 300000),
        xlab="Hour of the day", 
        cex.names=0.52, axes=FALSE)
axis(side=2, at=c(0, 50000, 100000, 150000, 200000,250000, 300000), 
     labels=c(0, 50, 100, 150, 200,250, 300))



######################## coupon_detail_train ########################

#Create barplot to visualize number of coupons purchased by individual customer
#for the sake of visuaization ITEM_COUNTS 1-7 is displayed
counts_ITEM_COUNT<-table(coupon_detail_train$ITEM_COUNT)
barplot(counts_ITEM_COUNT[c(1:7)], main="Number of coupons purchsed by individual user",
        ylab="Purchased coupons, *10^3", ylim=c(0, 150000), yaxt="n",
        xlab="Number of coupons")
axis(side=2, at=c(0, 30000, 60000, 90000, 120000, 150000), labels=c(0, 30, 60, 90, 120, 150))


######################### user_list##############################

#Creat barplot to visualise sex distribution among users using SEX_ID
counts_SEX_ID<-table(user_list$SEX_ID) # Buil and save contigency table
barplot(counts_SEX_ID, names.arg=c("Females", "Males"), 
        main= "Sex distribution among website users ", 
        ylab="Website users", ylim=c(0,20000))
axis(side=2, at=c(0, 5000, 10000, 15000,20000))
text(x=0.7, y=7000, "48%", col="pink", cex=2)
text(1.9,7000, "52%", col="blue", cex=2)

#Create barplot to visualise age group distribution among users using AGE_GROUPS
counts_AGE_GROUPS<-table(user_list$AGE_GROUPS) # Buil and save contigency table
barplot(counts_AGE_GROUPS, names.arg=c("14-23", "24-33","34-43", "44-53", 
                                       "54-63", "64-73", "74-83"), 
        main= "Age distribution among website users ", 
        ylab="Website users", ylim=c(0,10000))
axis(side=2, at=c(0, 2000, 4000, 6000,8000, 10000))
#add percentages to each age group on the plot: (counts_AGE_GROUPS/22873)*100
text(x=0.8, y=600, "4.0%", col="blue", cex=1)
text(x=2, y=3000, "24.0%", col="blue", cex=1)
text(x=3.1, y=4000, "31.3%", col="blue", cex=1)
text(x=4.2, y=3000, "23.8%", col="blue", cex=1)
text(x=5.5, y=2000, "12.8%", col="blue", cex=1)
text(x=6.8, y=600, "3.6%", col="blue", cex=1)
text(x=8, y=600, "0.5%", col="blue", cex=1)

# PREF_NAME_en is residential prefecture of the users. 
# 48 unique values are available for this variable
#To see 10 residential prefectures with bigger number of the customers 
sort(table (user_list$PREF_NAME_en), decreasing=TRUE)[1:10]


############################## train ################################

# Creat barplot to visualise distribution of PRICE_RATE interval groups
#Create interval groups for train$PRICE_RATE:
train$PRICE_RATE_GROUPS<-cut(train$PRICE_RATE, 
                             breaks=c(0,20,40,60, 80,100), 
                             labels=c("0-19","20-39", "40-59", "60-79", "80-100"))

counts_PRICE_RATE_GROUPS<-table(train$PRICE_RATE_GROUPS) # Buil and save contigency table
barplot(counts_PRICE_RATE_GROUPS, 
        names.arg=c("0-19","20-39", "40-59", "60-79", "80-100"),
        main= "Discount rate of coupons in a training set", 
        ylab="Number of coupons* 10^3", 
        xlab="Discount rate in %",
        ylim=c(0, 14000))
axis(side=2, at=c(0,4000, 8000, 12000))
#add percentages to each discount group on the plot: 
#(counts_PRICE_RATE_GROUPS/19415)*100
text(x=0.8, y=2000, "0.03%", col="blue", cex=1)
text(x=2, y=2000, "0.41%", col="blue", cex=1)
text(x=3.1, y=6000, "66.80%", col="blue", cex=1)
text(x=4.2, y=4000, "27.00%", col="blue", cex=1)
text(x=5.5, y=3000, "5.76%", col="blue", cex=1)


#Create plot to visualise DISPPERIOD (Time in days when coupon is available for sale)
summary(train$DISPPERIOD)
plot(table(train$DISPPERIOD),  ylab="Number of coupons",
     xlab="Display period in days",
     main="Coupons display period")

#Create plot to visualise VALIDPERIOD (Time in days whan coupon is valid for use)
summary(train$VALIDPERIOD)
plot(table(train$VALIDPERIOD),  ylab="Number of coupons",
     xlab="Validity period in days",
     main="Coupons validity period")

#Create barplot to visualise large_area_name (large area name of shop location)
counts_large_area_name<-sort(table(train$large_area_name_en), decreasing=TRUE)
# Buil and save sorted contigency table
barplot(counts_large_area_name, main="Location of the shops providing coupons",
        ylab="Number of coupons",
        xlab="Area name of shop location",
        ylim=c(0,10000),
        cex.names=0.52)

#Create barplot to visualise GENRE_NAME (coupon category)
counts_GENRE_NAME<-sort(table(train$GENRE_NAME_en), decreasing=TRUE)

# par("mar") original margin parameters
#[1] 5.1 4.1 4.1 2.1
# margin parameters should be changed so all labels appear on the plot
par(mar=c(8.5, 4.1, 4.1,2.1))
barplot(counts_GENRE_NAME, main="Coupon categories in a training set",
        ylab="Number of coupons", ylim=c(0, 6000), 
        cex.names=0.8, las=2)
par(mar=c(5.1,4.1,4.1,2.1)) # reverce changes in margin parameters