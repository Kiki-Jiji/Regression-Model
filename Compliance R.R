
dir()

library(readxl)
library(dplyr)
library(stringr)


excel_sheets("Compliance_Costs_17_18.xlsx")

comp = read_excel("Compliance_Costs_17_18.xlsx",
                  sheet= "Analysis")

comp2 = read_excel("Number_of_Questions_per_Survey.xlsx", sheet = 2, skip = 3)

comp3 = comp[, 1:5]

colnames(comp2)[1] = ("Survey Code")  

full = inner_join(comp2, comp3, by = "Survey Code")

full1 <- filter(full, !is.na(full$`Median Completion Time`))
full2 <- full1 %>%
  filter(!str_detect(full1$No_of_Questions, "\\?"))


#filter for just compliance review and time wuestion 

sub <-    full %>%
filter(str_detect(full$`time question`, "yes") | str_detect(full$`compliance review`, "yes")) 

#elimintae NA 

subb <- filter(sub, !is.na(sub$`Median Completion Time`))

#analysis on just complaince review and time question

colnames(subb) = c("Survey_Code", "frequency", "No_Questions", "compliance_review", "time_question", "survey_questionnaire" , "mode", "Frequency", "Sample_Size", "Achieved_Sample", "Median_Time")          

fin1 <- subb %>%
  filter(!str_detect(subb$No_Questions, "\\?"))

fin <- transform(fin1, No_Questions= as.numeric(No_Questions))

rm(fin1)
rm(comp)
rm(comp1)
rm(comp2)


#fin is the final dataset without NA and just compl adn time
#fin1 final dataset but with outliers not corrected
#full2 is full dataset, with NA removed and \\? removered
#use to remove outliers, will then
fin[5,3] = 20
fin[10,11] = 15

#Look at correlation for each dateset

print(paste0("The correlation for the full dataset is ", cor(as.numeric(full2$No_of_Questions), full2$`Median Completion Time`)))

print(paste0("The correlation for the data with outliers is ", cor(as.numeric(fin1$No_Questions), fin1$Median_Time)))

print(paste0("The correlation for the trimmed data is ", cor(fin$No_Questions, fin$Median_Time)))

#regression

model <- lm(Median_Time ~ No_Questions, data = fin)
summary(model)

#combine predection and actual result
new <- data.frame(No_Questions =  fin$No_Questions)

pred = predict(model, newdata = new, interval = "confidence")



#plot scatter of acrtual value and predection 
top <- data.frame(fin$Median_Time, pred)
plot(top$fin.Median_Time, top$pred)

#plot residuals
hist(model$residuals)



#graph it

final = cbind(fin, pred)

library("ggplot2")
names(final)

ggplot(final, aes(Median_Time, No_Questions)) +
  geom_point() +
  stat_smooth(method = lm)

      
