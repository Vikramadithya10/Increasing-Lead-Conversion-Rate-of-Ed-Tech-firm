##BSF EndTerm Project
#Group 1 Team 8
library(corrplot)
library(GGally)
library(dplyr)
library(summarytools) 
library (ggplot2)
library (logiBin)
library(ROCR)
library(car)

#library(data.table)
#library(caret)
#options(digits = 4, scipen = 9)


d <- read.csv(file.choose())
#str(d)

#Data types correction
d$Lead.Number <- as.character(d$Lead.Number)
for (i in c(3:6,11:32,35:37)) { d[,i] <- as.factor(d[,i])}

#str(d)
#view(dfSummary(d))
#colnames(d)
#summary(d)

################################################################################
#Modifying columns

#table(d$Lead.Source)
d$Lead.Source.mod <- as.factor(case_when(d$Lead.Source=="Direct Traffic"~"Direct Traffic",
                               d$Lead.Source=="Facebook"~"Facebook",
                               d$Lead.Source=="Google"~"Google",
                               d$Lead.Source=="google"~"Google",
                               d$Lead.Source=="Olark Chat"~"Olark Chat",
                               d$Lead.Source=="Organic Search"~"Organic Search",
                               d$Lead.Source=="Reference"~"Reference",
                               d$Lead.Source=="Referral Sites"~"Referral Sites",
                               d$Lead.Source=="Welingak Website"~"Welingak Website",
                               TRUE~"Others"))
#table(d$Lead.Source.mod)

#table(d$Last.Activity)
d$Last.Activity.mod <- as.factor(case_when(d$Last.Activity=="Converted to Lead"~"Converted to Lead",
                                 d$Last.Activity=="Email Bounced"~"Email Bounced",
                                 d$Last.Activity=="Email Link Clicked"~"Email Link Clicked",
                                 d$Last.Activity=="Email Opened"~"Email Opened",
                                 d$Last.Activity=="Form Submitted on Website"~"Form Submitted on Website",
                                 d$Last.Activity=="Olark Chat Conversation"~"Olark Chat Conversation",
                                 d$Last.Activity=="Page Visited on Website"~"Page Visited on Website",
                                 d$Last.Activity=="SMS Sent"~"SMS Sent",
                                 d$Last.Activity=="Unreachable"~"Unreachable",
                                 TRUE~"Others"))
#table(d$Last.Activity.mod)

#table(d$Country)
d$Country.mod <- as.factor(case_when(d$Country=="India"~"India",
                           TRUE~"Others"))
#table(d$Country.mod)

#table(d$Specialization)
d$Specialization.mod <- as.factor(case_when(d$Specialization=="Banking, Investment And Insurance"~"Banking, Investment And Insurance",
                                  d$Specialization=="Business Administration"~"Business Administration",
                                  d$Specialization=="Finance Management"~"Finance Management",
                                  d$Specialization=="Human Resource Management"~"Human Resource Management",
                                  d$Specialization=="IT Projects Management"~"IT Projects Management",
                                  d$Specialization=="Marketing Management"~"Marketing Management",
                                  d$Specialization=="Operations Management"~"Operations Management",
                                  d$Specialization=="Supply Chain Management"~"Supply Chain Management",
                                  TRUE~"Others"))
#table(d$Specialization.mod)

#table(d$How.did.you.hear.about.X.Education)
d$How.did.you.hear.about.X.Education.mod <- as.factor(case_when(d$How.did.you.hear.about.X.Education=="Multiple Sources"~"Multiple Sources",
                                                      d$How.did.you.hear.about.X.Education=="Online Search"~"Online Search",
                                                      d$How.did.you.hear.about.X.Education=="Student of SomeSchool"~"Student of SomeSchool",
                                                      d$How.did.you.hear.about.X.Education=="Word Of Mouth"~"Word Of Mouth",
                                                      TRUE~"Others"))
#table(d$How.did.you.hear.about.X.Education.mod, d$Converted)

#table(d$Tags)
d$Tags.mod <- as.factor(case_when(d$Tags=="Already a student"~"Already a student",
                        d$Tags=="Closed by Horizzon"~"Closed by Horizzon",
                        d$Tags=="Ringing"~"Ringing",
                        d$Tags=="Will revert after reading the email"~"Will revert after reading the email",
                        d$Tags=="switched off"~"switched off",
                        d$Tags=="Interested in other courses"~"Interested in other courses",
                        TRUE~"Others"))
#table(d$Tags.mod)

#table(d$Last.Notable.Activity)
d$Last.Notable.Activity.mod <- as.factor(case_when(d$Last.Notable.Activity=="Email Opened"~"Email Opened",
                                         d$Last.Notable.Activity=="Modified"~"Modified",
                                         d$Last.Notable.Activity=="SMS Sent"~"SMS Sent",
                                         d$Last.Notable.Activity=="Olark Chat Conversation"~"Olark Chat Conversation",
                                         d$Last.Notable.Activity=="Email Link Clicked"~"Email Link Clicked",
                                         d$Last.Notable.Activity=="Page Visited on Website"~"Page Visited on Website",
                                         TRUE~"Others"))
#table(d$Last.Notable.Activity.mod)

##MODEL CONSTRUCTION

set.seed(999)
d_split <- sample(1:nrow(d), 0.7*nrow(d))
d_train <- data.frame(d[d_split,])
d_test <- data.frame(d[-d_split,])

##Select columns on which model will be run 
model <- d_train[, c(3:44)] 
cvnames <- colnames(model)

#summary(model)
#cvnames

##Do binning
result <- getBins(model, "Converted", cvnames, minProp = 0.05, minCr = 0.99, nCores = 1)

##Store binning information
bin_mod <- result$bin
#View(bin_mod)

##Summary of all variables 
summary_bin <- result$varSummary
#View(summary_bin)

##error terms which are not binned 
err_terms <- result$err
#View(err_terms)

################################################################################
##write.csv(bin_mod, file.choose(),row.names = FALSE)
##write.csv(summary_bin, file.choose(), row.names = FALSE)
##write.csv(err_terms, file.choose(), row.names = FALSE)
################################################################################
##Calculate WoE 

bin_mod$distribution_of_bad <- bin_mod$bads/sum(model$Converted)
bin_mod$distribution_of_good <- bin_mod$goods/(length(model$Converted) - sum(model$Converted))
bin_mod$woe <- log(bin_mod$distribution_of_good/bin_mod$distribution_of_bad)

#Bads - Converted
#Goods - Not Converted

##write.csv(bin_mod, file.choose(), row.names = FALSE)
##write.csv(summary_bin, file.choose(), row.names = FALSE)

#dictionary <- bin_mod[,c(1,2,12)]
#View(dictionary)

#Column Conversions
d$Tags.mod.woe <- case_when(d$Tags.mod %in% c("Interested in other courses","Ringing","Already a student","switched off")~3.258951,
                            d$Tags.mod %in% c("Will revert after reading the email","Closed by Horizzon")~-4.055081,
                            TRUE~0.587179)

d$Lead.Quality.woe <- case_when(d$Lead.Quality %in% c("Low in Relevance","Might be")~-1.645419176,
                                d$Lead.Quality %in% c("High in Relevance")~-3.355611424,
                                d$Lead.Quality %in% c("Worst")~3.750112038,
                                TRUE~0.806004803
                                )

d$Total.Time.Spent.on.Website.woe <- case_when(d$Total.Time.Spent.on.Website <= 1~-0.105381359630271,
                                               d$Total.Time.Spent.on.Website > 1 & d$Total.Time.Spent.on.Website <= 434~1.35879108285511,
                                               d$Total.Time.Spent.on.Website > 434 & d$Total.Time.Spent.on.Website <= 652~0.485407760751351,
                                               d$Total.Time.Spent.on.Website > 652~-1.27232505379485
                                               )

d$Lead.Origin.woe <- case_when(d$Lead.Origin %in% c("API","Lead Import")~0.354280217140783,
                               d$Lead.Origin %in% c("Landing Page Submission")~0.0853623896804409,
                               d$Lead.Origin %in% c("Lead Add Form")~-2.97521641519508
                               )

d$Do.Not.Email.woe <- case_when(d$Do.Not.Email %in% c("No")~-0.0798885264182611,
                                d$Do.Not.Email %in% c("Yes")~1.16569139686883
                                )

d$What.is.your.current.occupation.woe <-case_when(d$What.is.your.current.occupation %in% c("")~1.3209395773373,
                                              d$What.is.your.current.occupation %in% c("Unemployed","Student","Other")~-0.177430399568483,
                                              d$What.is.your.current.occupation %in% c("Working Professional","Housewife","Businessman")~-2.90140206173066
                                              )

d$What.matters.most.to.you.in.choosing.a.course.woe <- case_when(d$What.matters.most.to.you.in.choosing.a.course %in% c("","Other")~1.32971156341013,
                                                                 d$What.matters.most.to.you.in.choosing.a.course %in% c("Better Career Prospects","Flexibility & Convenience")~-0.400279982990293
                                                                 )


d$Lead.Profile.woe <- case_when(d$Lead.Profile %in% c("","Student of SomeSchool")~1.40708122552883,
                                d$Lead.Profile %in% c("Select","Other Leads")~-0.0487330360690917,
                                d$Lead.Profile %in% c("Potential Lead","Dual Specialization Student","Lateral Student")~-1.77434438854739
                                )

d$City.woe <- case_when(d$City %in% c("")~1.64552119034033,
                        d$City %in% c("Mumbai","Other Metro Cities","Other Cities","Tier II Cities")~-0.0879389243273655,
                        d$City %in% c("Thane & Outskirts","Select","Other Cities of Maharashtra")~-0.34782656651781
                        )

d$Asymmetrique.Activity.Index.woe <- case_when(d$Asymmetrique.Activity.Index %in% c("")~-0.0175949216990546,
                                               d$Asymmetrique.Activity.Index %in% c("02.Medium")~-0.170065144781548,
                                               d$Asymmetrique.Activity.Index %in% c("01.High","03.Low")~0.697473310435157
                                               )

d$Asymmetrique.Profile.Index.woe <- case_when(d$Asymmetrique.Profile.Index %in% c("","03.Low")~-0.0186319479055457,
                                              d$Asymmetrique.Profile.Index %in% c("01.High")~-0.349772704303061,
                                              d$Asymmetrique.Profile.Index %in% c("02.Medium")~0.334791474257522
                                              )

d$Asymmetrique.Activity.Score.woe <- case_when(d$Asymmetrique.Activity.Score <= 13~1.52274482577341,
                                               d$Asymmetrique.Activity.Score > 13 & d$Asymmetrique.Activity.Score <= 14~-0.0691905344855843,
                                               d$Asymmetrique.Activity.Score > 14 & d$Asymmetrique.Activity.Score <= 15~-1.01168363875376,
                                               d$Asymmetrique.Activity.Score > 15~0.37063578975131,
                                               TRUE~-0.0175949216990546)

d$Asymmetrique.Profile.Score.woe <- case_when(d$Asymmetrique.Profile.Score <= 15~0.460701976198818,
                                              d$Asymmetrique.Profile.Score > 15 & d$Asymmetrique.Profile.Score <= 18~-0.0441593775235558,
                                              d$Asymmetrique.Profile.Score > 18~-1.38347807986038,
                                              TRUE~-0.0175949216990546)

d$A.free.copy.of.Mastering.The.Interview.woe <- case_when(d$A.free.copy.of.Mastering.The.Interview %in% c("No")~-0.0522868216026515,
                                                          d$A.free.copy.of.Mastering.The.Interview %in% c("Yes")~0.115023886776623
                                                          )

d$Lead.Source.mod.woe <- case_when(d$Lead.Source.mod %in% c("Direct Traffic")~0.292708194149258,
                                   d$Lead.Source.mod %in% c("Olark Chat","Facebook","Referral Sites")~0.619012488457999,
                                   d$Lead.Source.mod %in% c("Google","Others")~-0.106434707657956,
                                   d$Lead.Source.mod %in% c("Organic Search")~0.0640871066525539,
                                   d$Lead.Source.mod %in% c("Reference","Welingak Website")~-3.09118621372728
                                   )

d$Last.Activity.mod.woe <- case_when(d$Last.Activity.mod %in% c("Email Bounced","Converted to Lead")~1.63577412022341,
                                     d$Last.Activity.mod %in% c("Olark Chat Conversation")~2.04255127750296,
                                     d$Last.Activity.mod %in% c("Page Visited on Website","Email Link Clicked","Form Submitted on Website")~0.617036203229787,
                                     d$Last.Activity.mod %in% c("Email Opened","Unreachable")~0.0679640505083586,
                                     d$Last.Activity.mod %in% c("SMS Sent","Others")~-0.964917479890183
                                     )
d$Country.mod.woe <- case_when(d$Country.mod %in% c("India")~0.063425843175192,
                               d$Country.mod %in% c("Others")~-0.148159481643075
                               )

d$Specialization.mod.woe <- case_when(d$Specialization.mod %in% c("Marketing Management","Human Resource Management","Business Administration","Finance Management","Operations Management","Banking, Investment And Insurance","Supply Chain Management")~-0.323510699880261,
                                      d$Specialization.mod %in% c("Others","IT Projects Management")~0.301761881427991
                                      )

d$How.did.you.hear.about.X.Education.mod.woe <- case_when(d$How.did.you.hear.about.X.Education.mod %in% c("Others","Multiple Sources")~0.0539543520661679,
                                                      d$How.did.you.hear.about.X.Education.mod %in% c("Student of SomeSchool","Online Search","Word Of Mouth")~-0.268246993104248
                                                      )

d$Last.Notable.Activity.mod.woe <- case_when(d$Last.Notable.Activity.mod %in% c("Modified","Olark Chat Conversation","Email Link Clicked")~0.764828338767335,
                                             d$Last.Notable.Activity.mod %in% c("Email Opened","Page Visited on Website","Others")~0.0741803135301533,
                                             d$Last.Notable.Activity.mod %in% c("SMS Sent")~-1.26076518096335
                                             )

#colnames(d)
#summary(d)

##Select y variable and columns replaced by woe values
##This will be used to create the model

d_train <- data.frame(d[d_split,])
d_test <- data.frame(d[-d_split,])

#x_bin <- as.data.frame(d_train[, c(7,45:64)])
#View(x_bin)
#summary(x_bin)
x_bin <- subset(d_train, select = 
                  c(Converted,
                    Tags.mod.woe,
                    #S10#Lead.Quality.woe, #Zero Stars in log_reg
                    Total.Time.Spent.on.Website.woe,
                    #S2#Lead.Origin.woe, #High VIF, Zero Stars in log_reg
                    Do.Not.Email.woe,
                    #S4#What.is.your.current.occupation.woe, #High VIF, Zero Stars in log_reg
                    #S3#What.matters.most.to.you.in.choosing.a.course.woe, #High VIF, Zero Stars in log_reg
                    Lead.Profile.woe,
                    City.woe,
                    Asymmetrique.Activity.Index.woe,
                    #S5#Asymmetrique.Profile.Index.woe,#High VIF, Zero Stars in log_reg, Low IV 
                    Asymmetrique.Activity.Score.woe,
                    Asymmetrique.Profile.Score.woe,
                    #S6#A.free.copy.of.Mastering.The.Interview.woe, #Low IV, Zero Stars in log_reg
                    Lead.Source.mod.woe,
                    Last.Activity.mod.woe,
                    #S7#Country.mod.woe, #Low IV, Zero Stars in log_reg
                    #S9#Specialization.mod.woe, #Zero Stars in log_reg
                    #S8#How.did.you.hear.about.X.Education.mod.woe, #Low IV, Zero Stars in log_reg
                    Last.Notable.Activity.mod.woe
                  ))

x_bin_test <- as.data.frame(d_test[, c(7,45:64)])
x_bin_test <-  subset(d_test, select = 
                        c(Converted,
                          Tags.mod.woe,
                          #S10#Lead.Quality.woe, #Zero Stars in log_reg
                          Total.Time.Spent.on.Website.woe,
                          #S2#Lead.Origin.woe, #High VIF, Zero Stars in log_reg
                          Do.Not.Email.woe,
                          #S4#What.is.your.current.occupation.woe, #High VIF, Zero Stars in log_reg
                          #S3#What.matters.most.to.you.in.choosing.a.course.woe, #High VIF, Zero Stars in log_reg
                          Lead.Profile.woe,
                          City.woe,
                          Asymmetrique.Activity.Index.woe,
                          #S5#Asymmetrique.Profile.Index.woe,#High VIF, Zero Stars in log_reg, Low IV 
                          Asymmetrique.Activity.Score.woe,
                          Asymmetrique.Profile.Score.woe,
                          #S6#A.free.copy.of.Mastering.The.Interview.woe, #Low IV, Zero Stars in log_reg
                          Lead.Source.mod.woe,
                          Last.Activity.mod.woe,
                          #S7#Country.mod.woe, #Low IV, Zero Stars in log_reg
                          #S9#Specialization.mod.woe, #Zero Stars in log_reg
                          #S8#How.did.you.hear.about.X.Education.mod.woe, #Low IV, Zero Stars in log_reg
                          Last.Notable.Activity.mod.woe
                        ))
##Remove rows with woe as infinity

##Check for no missing data
sum(is.na(x_bin)) ## Should be ZERO
sum(is.na(x_bin_test)) ## Should be ZERO

##Removing columns with high correlations

##Removing variables with iv less than 1% 

##Dropping parameters basis Low IV, High VIF (GTE2) and low importance (basis z value) for the model developed

################################################################################
##Constructing Logistic Model
log_reg <- glm(Converted ~ ., data=x_bin, family = binomial)
summary(log_reg)

##Check VIF to remove columns with high correlations 
#library(car)
vif(log_reg)

##Check correlation between variables 
#cor <- cor(x_bin)
#View(cor)
##write.csv(cor,file.choose())

##Calculate AUC and GINI 
#library(ROCR)
x_predtrain <- as.data.frame(predict(log_reg, x_bin, type = "response")) 
x_predtest <- as.data.frame(predict(log_reg, x_bin_test, type = "response")) 

x_predtrain$bad_flag <- x_bin$Converted
x_predtest$bad_flag <- x_bin_test$Converted

x_predtrain$quartile <- ntile(x_predtrain$`predict(log_reg, x_bin, type = "response")`, 10)
x_predtest$quartile <- ntile(x_predtest$`predict(log_reg, x_bin_test, type = "response")`, 10)

x_predtrain_sum <- x_predtrain %>% group_by(quartile) %>% summarise(bad_rate = sum(bad_flag)/n(),
                                                       bads = sum(bad_flag),
                                                       goods = n() - sum(bad_flag),
                                                       count = n(),
                                                       min_prob = min(`predict(log_reg, x_bin, type = "response")`),
                                                       max_prob = max(`predict(log_reg, x_bin, type = "response")`), 
                                                       avg_prob = mean(`predict(log_reg, x_bin, type = "response")`)
                                                       )
x_predtest_sum <- x_predtest %>% group_by(quartile) %>% summarise(bad_rate = sum(bad_flag)/n(),
                                                                    bads = sum(bad_flag),
                                                                    goods = n() - sum(bad_flag),
                                                                    count = n(),
                                                                    min_prob = min(`predict(log_reg, x_bin_test, type = "response")`),
                                                                    max_prob = max(`predict(log_reg, x_bin_test, type = "response")`), 
                                                                    avg_prob = mean(`predict(log_reg, x_bin_test, type = "response")`)
                                                                  )

rocrpredtrain <- prediction(x_predtrain$`predict(log_reg, x_bin, type = "response")`,x_predtrain$bad_flag)
rocrpredtest <- prediction(x_predtest$`predict(log_reg, x_bin_test, type = "response")`,x_predtest$bad_flag)

auctrain <- as.numeric(performance(rocrpredtrain,"auc") @y.values)
auctrain/0.01
auctest <- as.numeric(performance(rocrpredtest,"auc") @y.values)
auctest/0.01

GINItrain <- 2*auctrain - 1
GINItrain/0.01
GINItest <- 2*auctest - 1
GINItest/0.01

#View(x_predtrain_sum)
#View(x_predtest_sum)

write.csv (x_predtrain_sum, file.choose(), row.names = FALSE)
write.csv (x_predtest_sum, file.choose(), row.names = FALSE)
#Bads - Converted
#Goods - Not Converted
#Base Model GINItrain = 94.07563; GINItest = 95.20654
#Step02 GINItrain = 94.08133; GINItest = 95.18902
#Step03 GINItrain = 94.08087; GINItest = 95.19153
#Step04 GINItrain = 94.07755; GINItest = 95.17800
#Step05 GINItrain = 94.07308; GINItest = 95.19153
#Step06 GINItrain = 94.07245; GINItest = 95.17800
#Step07 GINItrain = 94.06630; GINItest = 95.17800
#Step08 GINItrain = 94.06097; GINItest = 95.17800
#Step09 GINItrain = 94.06383; GINItest = 95.17800
#Step10 GINItrain = 94.05890; GINItest = 95.17800