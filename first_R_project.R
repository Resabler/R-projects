#loading and inspecting data
htd <- read.csv(file.choose())
head(htd)
dim(htd)
str(htd)
names(htd)
summary(htd)
dim(htd)



# counting the sum colwise

 isa <- colSums(is.na(htd))

#col with max null values
 sum(isa)

max(isa)
names(which.max(isa))


print(isa)
null_counts <- colSums(is.na( htd ))
sum(null_counts)

missing_percent <- round(null_counts / nrow(htd) *100, 1) 
print(missing_percent)

is.na(htd)

which.max(isa)
names(which.min(isa))
which.min(isa)
htd[1,3]
isa[which.min(isa)]

sum(isa.na(htd))
#find col in which have max no of na
issa <- is.na(htd)
summary(htd$systolic_bp)


which(htd < 0)
which(1599)

htd$systolic_bp[which(htd$systolic_bp <0)]
htd[which(htd$systolic_bp<0),]
htd$systolic_bp[htd$systolic_bp == -999 ]  <- NA
print(htd$systolic_bp)
summary(htd$systolic_bp)
table(htd$sex)

htd$sex[htd$sex == 'M' | htd$sex == 'male'] <- "Male"
htd$sex[htd$sex == 'F' | htd$sex == 'femlae'] <- "Female"
table(htd$sex)
htd$sex[htd$sex %in% c('Male')] <- "MALE"
htd$sex[htd$sex %in% c('Female','f',F)] <- "FEMALE"
table(htd$sex)

duplicated(htd)
htd[duplicated(htd),]

duplicated(htd)
htd[duplicated(htd),]
htd[duplicated(htd) | duplicated(htd,fromLast = TRUE),]

htd <- htd[!duplicated(htd),]
sum(duplicated(htd))

boxplot(htd$weight_kg,htd$age ,xlabel='weight',ylabel='height',main="Relation between the Age & Weight ")

htd$bmi <- htd$weight_kg / (htd$height/100)^2
sm <- table(htd$smoker)
tm <- table(htd$condition)
prop.table(sm)
prop.table(tm)

aggregate(htd$systolic_bp ~ htd$sex , FUN = mean)

aggregate(htd$sex ~ htd$smoker,FUN = average)

mean(htd$length_of_stay_days)
median(htd$length_of_stay_days)
boxplot(htd$length_of_stay_days)

boxplot(htd$age)

t2 <- table(htd$condition)

t1 <- table(htd$smoker)

prop.table(t1)
prop.table(t2)

hist(htd$age,breaks=3,main="Histogram of Age")
c <- aggregate( htd$patient_id ~ htd$admission_year + htd$condition, FUN=length)
table(c)
head(c)
names(c[3])

counts <- aggregate( patient_id ~ admission_year + condition ,data = htd , FUN = length)
print(counts)
head(counts)
table(counts)
names(counts)
names(counts[3])
names(counts)[3] <- "admissions"
names(counts)
head(counts)
install.packages("ggplot2")
library(ggplot2)
ggplot(counts,aes(x=admission_year,y=patient_id,colour = condition)) + geom_line() + geom_point()
names(counts)

write.csv(htd, file = "cleaned_csv", row.names = FALSE)
