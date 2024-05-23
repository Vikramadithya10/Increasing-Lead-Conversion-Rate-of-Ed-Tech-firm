setwd("D:/IIM Trichy/Term - IV/BSF/Final Project")

library(ggplot2)

# Reading data
df = read.csv("Leads.csv")
View(df)

# Checking types of data
str(df)

# Finding missing values
missing_values_per_column <- sapply(df, function(col) sum(col == ""))
print(missing_values_per_column)

###### Univariate Analysis #####
## Lead Origin
# Calculate frequency counts
freq_counts <- table(df$Lead.Origin)

# Reorder the factor levels based on frequency
Lead.Origin_ordered <- factor(df$Lead.Origin, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(Lead.Origin = Lead.Origin_ordered), aes(x = Lead.Origin, fill = "blue")) +
  geom_bar() +
  labs(title = "Lead Origin",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

## Lead Source
# Calculate frequency counts
freq_counts <- table(df$Lead.Source)

# Reorder the factor levels based on frequency
Lead.Source_ordered <- factor(df$Lead.Source, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(Lead.Source = Lead.Source_ordered), aes(x = Lead.Source, fill = "blue")) +
  geom_bar() +
  labs(title = "Lead Source",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

# Checking no of frequency of each value in Lead Source
sorted_freq_counts <- sort(table(df$Lead.Source), decreasing = TRUE)
print(sorted_freq_counts)

## Do.Not.Email
# Calculate frequency counts
freq_counts <- table(df$Do.Not.Email)

# Reorder the factor levels based on frequency
Do.Not.Email_ordered <- factor(df$Do.Not.Email, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(Do.Not.Email = Do.Not.Email_ordered), aes(x = Do.Not.Email, fill = "blue")) +
  geom_bar() +
  labs(title = "Do Not Email",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

## Do.Not.Call
# Calculate frequency counts
freq_counts <- table(df$Do.Not.Call)

# Reorder the factor levels based on frequency
Do.Not.Call_ordered <- factor(df$Do.Not.Call, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(Do.Not.Call = Do.Not.Call_ordered), aes(x = Do.Not.Call, fill = "blue")) +
  geom_bar() +
  labs(title = "Do Not Call",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

# Checking no of frequency of each value in Lead Source
sorted_freq_counts0 <- sort(table(df$Do.Not.Call), decreasing = TRUE)
print(sorted_freq_counts0)

## Last Activity 
# Calculate frequency counts
freq_counts <- table(df$Last.Activity)

# Reorder the factor levels based on frequency
Last.Activity_ordered <- factor(df$Last.Activity, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(Last.Activity = Last.Activity_ordered), aes(x = Last.Activity, fill = "blue")) +
  geom_bar() +
  labs(title = "Last Activity",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

# Checking no of frequency of each value in Last Activity
sorted_freq_counts1 <- sort(table(df$Last.Activity), decreasing = TRUE)
print(sorted_freq_counts1)

## Country 
# Calculate frequency counts
freq_counts <- table(df$Country)

# Reorder the factor levels based on frequency
Country_ordered <- factor(df$Country, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(Country = Country_ordered), aes(x = Country, fill = "blue")) +
  geom_bar() +
  labs(title = "Country",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

# Checking no of frequency of each value in Country
sorted_freq_counts2 <- sort(table(df$Country), decreasing = TRUE)
print(sorted_freq_counts2)

## Specialization 
# Calculate frequency counts
freq_counts <- table(df$Specialization)

# Reorder the factor levels based on frequency
Specialization_ordered <- factor(df$Specialization, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(Specialization = Specialization_ordered), aes(x = Specialization, fill = "blue")) +
  geom_bar() +
  labs(title = "Specialization",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

# Checking no of frequency of each value in Specialization
sorted_freq_counts3 <- sort(table(df$Specialization), decreasing = TRUE)
print(sorted_freq_counts3)

## How did you hear about X Education  
# Calculate frequency counts
freq_counts <- table(df$How.did.you.hear.about.X.Education)

# Reorder the factor levels based on frequency
How.did.you.hear.about.X.Education_ordered <- factor(df$How.did.you.hear.about.X.Education, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(How.did.you.hear.about.X.Education = How.did.you.hear.about.X.Education_ordered), aes(x = How.did.you.hear.about.X.Education, fill = "blue")) +
  geom_bar() +
  labs(title = "How did you hear about X Education",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

# Checking no of frequency of each value in X Education
sorted_freq_counts4 <- sort(table(df$How.did.you.hear.about.X.Education), decreasing = TRUE)
print(sorted_freq_counts4)

## What is your current occupation  
# Calculate frequency counts
freq_counts <- table(df$What.is.your.current.occupation)

# Reorder the factor levels based on frequency
What.is.your.current.occupation_ordered <- factor(df$What.is.your.current.occupation, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(What.is.your.current.occupation = What.is.your.current.occupation_ordered), aes(x = What.is.your.current.occupation, fill = "blue")) +
  geom_bar() +
  labs(title = "What is your current occupation",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

## What matters most to you in choosing a course  
# Calculate frequency counts
freq_counts <- table(df$What.matters.most.to.you.in.choosing.a.course)

# Reorder the factor levels based on frequency
What.matters.most.to.you.in.choosing.a.course_ordered <- factor(df$What.matters.most.to.you.in.choosing.a.course, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(What.matters.most.to.you.in.choosing.a.course = What.matters.most.to.you.in.choosing.a.course_ordered), aes(x = What.matters.most.to.you.in.choosing.a.course, fill = "blue")) +
  geom_bar() +
  labs(title = "What matters most to you in choosing a course",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

## Search  
# Calculate frequency counts
freq_counts <- table(df$Search)

# Reorder the factor levels based on frequency
Search_ordered <- factor(df$Search, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(Search = Search_ordered), aes(x = Search, fill = "blue")) +
  geom_bar() +
  labs(title = "Search",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

# Checking no of frequency of each value in Search
sorted_freq_counts7 <- sort(table(df$Search), decreasing = TRUE)
print(sorted_freq_counts7)

## Magazine  
# Calculate frequency counts
freq_counts <- table(df$Magazine)

# Reorder the factor levels based on frequency
Magazine_ordered <- factor(df$Magazine, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(Magazine = Magazine_ordered), aes(x = Magazine, fill = "blue")) +
  geom_bar() +
  labs(title = "Magazine",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

## Newspaper Article 
# Calculate frequency counts
freq_counts <- table(df$Newspaper.Article)

# Reorder the factor levels based on frequency
Newspaper.Article_ordered <- factor(df$Newspaper.Article, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(Newspaper.Article = Newspaper.Article_ordered), aes(x = Newspaper.Article, fill = "blue")) +
  geom_bar() +
  labs(title = "Newspaper Article",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

# Checking no of frequency of each value in Newspaper Article
sorted_freq_counts8 <- sort(table(df$Newspaper.Article), decreasing = TRUE)
print(sorted_freq_counts8)

## X Education Forums 
# Calculate frequency counts
freq_counts <- table(df$X.Education.Forums)

# Reorder the factor levels based on frequency
X.Education.Forums_ordered <- factor(df$X.Education.Forums, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(X.Education.Forums = X.Education.Forums_ordered), aes(x = X.Education.Forums, fill = "blue")) +
  geom_bar() +
  labs(title = "X Education Forums",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

# Checking no of frequency of each value in X Education Forums
sorted_freq_counts8 <- sort(table(df$X.Education.Forums), decreasing = TRUE)
print(sorted_freq_counts8)

## Newspaper 
# Calculate frequency counts
freq_counts <- table(df$Newspaper)

# Reorder the factor levels based on frequency
Newspaper_ordered <- factor(df$Newspaper, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(Newspaper = Newspaper_ordered), aes(x = Newspaper, fill = "blue")) +
  geom_bar() +
  labs(title = "Newspaper",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

# Checking no of frequency of each value in Newspaper
sorted_freq_counts8 <- sort(table(df$Newspaper), decreasing = TRUE)
print(sorted_freq_counts8)

## Digital Advertisement 
# Calculate frequency counts
freq_counts <- table(df$Digital.Advertisement)

# Reorder the factor levels based on frequency
Digital.Advertisement_ordered <- factor(df$Digital.Advertisement, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(Digital.Advertisement = Digital.Advertisement_ordered), aes(x = Digital.Advertisement, fill = "blue")) +
  geom_bar() +
  labs(title = "Digital Advertisement",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

# Checking no of frequency of each value in Digital Advertisement
sorted_freq_counts9 <- sort(table(df$Digital.Advertisement), decreasing = TRUE)
print(sorted_freq_counts9)

## Through Recommendations 
# Calculate frequency counts
freq_counts <- table(df$Through.Recommendations)

# Reorder the factor levels based on frequency
Through.Recommendations_ordered <- factor(df$Through.Recommendations, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(Through.Recommendations = Through.Recommendations_ordered), aes(x = Through.Recommendations, fill = "blue")) +
  geom_bar() +
  labs(title = "Through Recommendations",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

# Checking no of frequency of each value in Through Recommendations
sorted_freq_counts10 <- sort(table(df$Through.Recommendations), decreasing = TRUE)
print(sorted_freq_counts10)

## Receive More Updates About Our Courses 
# Calculate frequency counts
freq_counts <- table(df$Receive.More.Updates.About.Our.Courses)

# Reorder the factor levels based on frequency
Receive.More.Updates.About.Our.Courses_ordered <- factor(df$Receive.More.Updates.About.Our.Courses, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(Receive.More.Updates.About.Our.Courses = Receive.More.Updates.About.Our.Courses_ordered), aes(x = Receive.More.Updates.About.Our.Courses, fill = "blue")) +
  geom_bar() +
  labs(title = "Receive More Updates About Our Courses",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

## Tags 
# Calculate frequency counts
freq_counts <- table(df$Tags)

# Reorder the factor levels based on frequency
Tags_ordered <- factor(df$Tags, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(Tags = Tags_ordered), aes(x = Tags, fill = "blue")) +
  geom_bar() +
  labs(title = "Tags",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

# Checking no of frequency of each value in Tags
sorted_freq_counts5 <- sort(table(df$Tags), decreasing = TRUE)
print(sorted_freq_counts5)

## Lead Quality 
# Calculate frequency counts
freq_counts <- table(df$Lead.Quality)

# Reorder the factor levels based on frequency
Lead.Quality_ordered <- factor(df$Lead.Quality, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(Lead.Quality = Lead.Quality_ordered), aes(x = Lead.Quality, fill = "blue")) +
  geom_bar() +
  labs(title = "Lead Quality",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

## Update me on Supply Chain Content 
# Calculate frequency counts
freq_counts <- table(df$Update.me.on.Supply.Chain.Content)

# Reorder the factor levels based on frequency
Update.me.on.Supply.Chain.Content_ordered <- factor(df$Update.me.on.Supply.Chain.Content, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(Update.me.on.Supply.Chain.Content = Update.me.on.Supply.Chain.Content_ordered), aes(x = Update.me.on.Supply.Chain.Content, fill = "blue")) +
  geom_bar() +
  labs(title = "Update me on Supply Chain Content",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

## Get updates on DM Content 
# Calculate frequency counts
freq_counts <- table(df$Get.updates.on.DM.Content)

# Reorder the factor levels based on frequency
Get.updates.on.DM.Content_ordered <- factor(df$Get.updates.on.DM.Content, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(Get.updates.on.DM.Content = Get.updates.on.DM.Content_ordered), aes(x = Get.updates.on.DM.Content, fill = "blue")) +
  geom_bar() +
  labs(title = "Get updates on DM Content",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

## Lead Profile 
# Calculate frequency counts
freq_counts <- table(df$Lead.Profile)

# Reorder the factor levels based on frequency
Lead.Profile_ordered <- factor(df$Lead.Profile, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(Lead.Profile = Lead.Profile_ordered), aes(x = Lead.Profile, fill = "blue")) +
  geom_bar() +
  labs(title = "Lead Profile",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

## City 
# Calculate frequency counts
freq_counts <- table(df$City)

# Reorder the factor levels based on frequency
City_ordered <- factor(df$City, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(City = City_ordered), aes(x = City, fill = "blue")) +
  geom_bar() +
  labs(title = "City",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

## Asymmetrique Activity Index 
# Calculate frequency counts
freq_counts <- table(df$Asymmetrique.Activity.Index)

# Reorder the factor levels based on frequency
Asymmetrique.Activity.Index_ordered <- factor(df$Asymmetrique.Activity.Index, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(Asymmetrique.Activity.Index = Asymmetrique.Activity.Index_ordered), aes(x = Asymmetrique.Activity.Index, fill = "blue")) +
  geom_bar() +
  labs(title = "Asymmetrique Activity Index",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

## I agree to pay the amount through cheque 
# Calculate frequency counts
freq_counts <- table(df$I.agree.to.pay.the.amount.through.cheque)

# Reorder the factor levels based on frequency
I.agree.to.pay.the.amount.through.cheque_ordered <- factor(df$I.agree.to.pay.the.amount.through.cheque, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(I.agree.to.pay.the.amount.through.cheque = I.agree.to.pay.the.amount.through.cheque_ordered), aes(x = I.agree.to.pay.the.amount.through.cheque, fill = "blue")) +
  geom_bar() +
  labs(title = "Asymmetrique Activity Index",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

## A free copy of Mastering The Interview 
# Calculate frequency counts
freq_counts <- table(df$A.free.copy.of.Mastering.The.Interview)

# Reorder the factor levels based on frequency
A.free.copy.of.Mastering.The.Interview_ordered <- factor(df$A.free.copy.of.Mastering.The.Interview, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(A.free.copy.of.Mastering.The.Interview = A.free.copy.of.Mastering.The.Interview_ordered), aes(x = A.free.copy.of.Mastering.The.Interview, fill = "blue")) +
  geom_bar() +
  labs(title = "A free copy of Mastering The Interview",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

## Last Notable Activity 
# Calculate frequency counts
freq_counts <- table(df$Last.Notable.Activity)

# Reorder the factor levels based on frequency
Last.Notable.Activity_ordered <- factor(df$Last.Notable.Activity, levels = names(freq_counts)[order(freq_counts, decreasing = TRUE)])

# Create a bar plot using ggplot2 with reordered levels and blue color
ggplot(data = data.frame(Last.Notable.Activity = Last.Notable.Activity_ordered), aes(x = Last.Notable.Activity, fill = "blue")) +
  geom_bar() +
  labs(title = "Last Notable Activity",
       x = "Character Values", y = "Frequency") +
  scale_fill_identity()

# Checking no of frequency of each value in Last Notable Activity
sorted_freq_counts6 <- sort(table(df$Last.Notable.Activity), decreasing = TRUE)
print(sorted_freq_counts6)

## Total Visits
# Create a histogram plot
ggplot(data = df, aes(x = TotalVisits)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black", na.rm = TRUE) +
  labs(title = "Total Visits",
       y = "Frequency") +
  theme_minimal()

# Create a box plot 
ggplot(data = df, aes(x = 1, y = TotalVisits)) +
  geom_boxplot(fill = "lightblue", color = "black", na.rm = TRUE) +
  labs(title = "Box Plot of Total Visits",
       x = "", y = "Total Visits") +
  theme_minimal()

## Total Time spent on website
# Create a histogram plot
ggplot(data = df, aes(x = Total.Time.Spent.on.Website)) +
  geom_histogram(binwidth = 30, fill = "blue", color = "black", na.rm = TRUE) +
  labs(title = "Total Time Spent on Website",
       y = "Frequency") +
  theme_minimal()

# Create a box plot 
ggplot(data = df, aes(x = 1, y = Total.Time.Spent.on.Website)) +
  geom_boxplot(fill = "lightblue", color = "black", na.rm = TRUE) +
  labs(title = "Box Plot of Total Time Spent on Website",
       x = "", y = "Total Time Spent on Website") +
  theme_minimal()

## Page Views Per Visit
# Create a histogram plot
ggplot(data = df, aes(x = Page.Views.Per.Visit)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", na.rm = TRUE) +
  labs(title = "Page Views Per Visit",
       y = "Frequency") +
  theme_minimal()

# Create a box plot 
ggplot(data = df, aes(x = 1, y = Page.Views.Per.Visit)) +
  geom_boxplot(fill = "lightblue", color = "black", na.rm = TRUE) +
  labs(title = "Page Views Per Visit",
       x = "", y = "Page Views Per Visit") +
  theme_minimal()

## Asymmetrique Activity Score
# Create a histogram plot
ggplot(data = df, aes(x = Asymmetrique.Activity.Score)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", na.rm = TRUE) +
  labs(title = "Asymmetrique Activity Score",
       y = "Frequency") +
  theme_minimal()

# Create a box plot 
ggplot(data = df, aes(x = 1, y = Asymmetrique.Activity.Score)) +
  geom_boxplot(fill = "lightblue", color = "black", na.rm = TRUE) +
  labs(title = "Asymmetrique Activity Score",
       x = "", y = "Asymmetrique Activity Score") +
  theme_minimal()

## Asymmetrique Profile Score
# Create a histogram plot
ggplot(data = df, aes(x = Asymmetrique.Profile.Score)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", na.rm = TRUE) +
  labs(title = "Asymmetrique Profile Score",
       y = "Frequency") +
  theme_minimal()

# Create a box plot 
ggplot(data = df, aes(x = 1, y = Asymmetrique.Profile.Score)) +
  geom_boxplot(fill = "lightblue", color = "black", na.rm = TRUE) +
  labs(title = "Asymmetrique Profile Score",
       x = "", y = "Asymmetrique Profile Score") +
  theme_minimal()
