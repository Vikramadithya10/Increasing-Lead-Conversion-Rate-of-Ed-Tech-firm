library(tidyverse)
library(corrplot)

# Read the Excel data into a dataframe
dataset <- read.csv("Leads.csv")

dataset$Converted <- as.factor(dataset$Converted)

# Calculate the count of each Lead.Source-Converted combination
count_data <- dataset %>%
  group_by(Lead.Source, Converted) %>%
  summarise(count = n())

# Reorder the levels of Lead.Source based on the total count of each Lead.Source
dataset$Lead.Source <- factor(dataset$Lead.Source, levels = count_data %>%
                                group_by(Lead.Source) %>%
                                summarise(total_count = sum(count)) %>%
                                arrange(desc(total_count)) %>%
                                pull(Lead.Source))

# Create a bar plot using ggplot2 for 'Lead.Source' data
my_plot_Lead.Source <- ggplot(data = dataset, aes(x = Lead.Source, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by Lead Source",
       x = "Lead Source",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_Lead.Source)
print(table(dataset$Lead.Source))

# Calculate the count of each Lead.Origin-Converted combination
count_data <- dataset %>%
  group_by(Lead.Origin, Converted) %>%
  summarise(count = n())

# Reorder the levels of Lead.Origin based on the total count of each Lead.Origin
dataset$Lead.Origin <- factor(dataset$Lead.Origin, levels = count_data %>%
                                group_by(Lead.Origin) %>%
                                summarise(total_count = sum(count)) %>%
                                arrange(desc(total_count)) %>%
                                pull(Lead.Origin))

# Create a bar plot using ggplot2 for 'Lead.Origin' data
my_plot_Lead.Origin <- ggplot(data = dataset, aes(x = Lead.Origin, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by Lead Origin",
       x = "Lead.Origin",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_Lead.Origin)
print(table(dataset$Lead.Origin))


# Calculate the count of each Do.Not.Email-Converted combination
count_data <- dataset %>%
  group_by(Do.Not.Email, Converted) %>%
  summarise(count = n())

# Reorder the levels of Do.Not.Email based on the total count of each Do.Not.Email
dataset$Do.Not.Email <- factor(dataset$Do.Not.Email, levels = count_data %>%
                                group_by(Do.Not.Email) %>%
                                summarise(total_count = sum(count)) %>%
                                arrange(desc(total_count)) %>%
                                pull(Do.Not.Email))

# Create a bar plot using ggplot2 for 'Do.Not.Email' data
my_plot_Do.Not.Email <- ggplot(data = dataset, aes(x = Do.Not.Email, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by Email preference",
       x = "Do.Not.Email",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_Do.Not.Email)
print(table(dataset$Do.Not.Email))

# Calculate the count of each Do.Not.Email-Converted combination
count_data <- dataset %>%
  group_by(Do.Not.Email, Converted) %>%
  summarise(count = n())

# Reorder the levels of Do.Not.Email based on the total count of each Do.Not.Email
dataset$Do.Not.Email <- factor(dataset$Do.Not.Email, levels = count_data %>%
                                 group_by(Do.Not.Email) %>%
                                 summarise(total_count = sum(count)) %>%
                                 arrange(desc(total_count)) %>%
                                 pull(Do.Not.Email))

# Create a bar plot using ggplot2 for 'Do.Not.Email' data
my_plot_Do.Not.Email <- ggplot(data = dataset, aes(x = Do.Not.Email, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by Email preference",
       x = "Do.Not.Email",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_Do.Not.Email)
print(table(dataset$Do.Not.Email))

# Calculate the count of each Do.Not.Call-Converted combination
count_data <- dataset %>%
  group_by(Do.Not.Call, Converted) %>%
  summarise(count = n())

# Reorder the levels of Do.Not.Call based on the total count of each Do.Not.Call
dataset$Do.Not.Call <- factor(dataset$Do.Not.Call, levels = count_data %>%
                                 group_by(Do.Not.Call) %>%
                                 summarise(total_count = sum(count)) %>%
                                 arrange(desc(total_count)) %>%
                                 pull(Do.Not.Call))

# Create a bar plot using ggplot2 for 'Do.Not.Call' data
my_plot_Do.Not.Call <- ggplot(data = dataset, aes(x = Do.Not.Call, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by Call preference",
       x = "Do.Not.Call",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_Do.Not.Call)
print(table(dataset$Do.Not.Call))

# Calculate the count of each Last.Activity-Converted combination
count_data <- dataset %>%
  group_by(Last.Activity, Converted) %>%
  summarise(count = n())

# Reorder the levels of Last.Activity based on the total count of each Last.Activity
dataset$Last.Activity <- factor(dataset$Last.Activity, levels = count_data %>%
                                group_by(Last.Activity) %>%
                                summarise(total_count = sum(count)) %>%
                                arrange(desc(total_count)) %>%
                                pull(Last.Activity))

# Create a bar plot using ggplot2 for 'Last.Activity' data
my_plot_Last.Activity <- ggplot(data = dataset, aes(x = Last.Activity, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by Last.Activity",
       x = "Last.Activity",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_Last.Activity)
print(table(dataset$Last.Activity))


# Calculate the count of each Country-Converted combination
count_data <- dataset %>%
  group_by(Country, Converted) %>%
  summarise(count = n())

# Reorder the levels of Country based on the total count of each Country
dataset$Country <- factor(dataset$Country, levels = count_data %>%
                                group_by(Country) %>%
                                summarise(total_count = sum(count)) %>%
                                arrange(desc(total_count)) %>%
                                pull(Country))

# Create a bar plot using ggplot2 for 'Country' data
my_plot_Country <- ggplot(data = dataset, aes(x = Country, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by Country",
       x = "Country",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_Country)
print(table(dataset$Country))

# Calculate the count of each Specialization-Converted combination
count_data <- dataset %>%
  group_by(Specialization, Converted) %>%
  summarise(count = n())

# Reorder the levels of Specialization based on the total count of each Specialization
dataset$Specialization <- factor(dataset$Specialization, levels = count_data %>%
                            group_by(Specialization) %>%
                            summarise(total_count = sum(count)) %>%
                            arrange(desc(total_count)) %>%
                            pull(Specialization))

# Create a bar plot using ggplot2 for 'Specialization' data
my_plot_Specialization <- ggplot(data = dataset, aes(x = Specialization, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by Specialization",
       x = "Specialization",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_Specialization)
print(table(dataset$Specialization))

# Calculate the count of each Specialization-Converted combination
count_data <- dataset %>%
  group_by(Specialization, Converted) %>%
  summarise(count = n())

# Reorder the levels of Specialization based on the total count of each Specialization
dataset$Specialization <- factor(dataset$Specialization, levels = count_data %>%
                                   group_by(Specialization) %>%
                                   summarise(total_count = sum(count)) %>%
                                   arrange(desc(total_count)) %>%
                                   pull(Specialization))

# Create a bar plot using ggplot2 for 'Specialization' data
my_plot_Specialization <- ggplot(data = dataset, aes(x = Specialization, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by Specialization",
       x = "Specialization",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_Specialization)
print(table(dataset$Specialization))

# Calculate the count of each How.did.you.hear.about.X.Education-Converted combination
count_data <- dataset %>%
  group_by(How.did.you.hear.about.X.Education, Converted) %>%
  summarise(count = n())

# Reorder the levels of How.did.you.hear.about.X.Education based on the total count of each How.did.you.hear.about.X.Education
dataset$How.did.you.hear.about.X.Education <- factor(dataset$How.did.you.hear.about.X.Education, levels = count_data %>%
                                   group_by(How.did.you.hear.about.X.Education) %>%
                                   summarise(total_count = sum(count)) %>%
                                   arrange(desc(total_count)) %>%
                                   pull(How.did.you.hear.about.X.Education))

# Create a bar plot using ggplot2 for 'How.did.you.hear.about.X.Education' data
my_plot_How.did.you.hear.about.X.Education <- ggplot(data = dataset, aes(x = How.did.you.hear.about.X.Education, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by DiscoverSource",
       x = "DiscoverSource",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_How.did.you.hear.about.X.Education)
print(table(dataset$How.did.you.hear.about.X.Education))


# Calculate the count of each What.is.your.current.occupation-Converted combination
count_data <- dataset %>%
  group_by(What.is.your.current.occupation, Converted) %>%
  summarise(count = n())

# Reorder the levels of What.is.your.current.occupation based on the total count of each What.is.your.current.occupation
dataset$What.is.your.current.occupation <- factor(dataset$What.is.your.current.occupation, levels = count_data %>%
                                                       group_by(What.is.your.current.occupation) %>%
                                                       summarise(total_count = sum(count)) %>%
                                                       arrange(desc(total_count)) %>%
                                                       pull(What.is.your.current.occupation))

# Create a bar plot using ggplot2 for 'What.is.your.current.occupation' data
my_plot_What.is.your.current.occupation <- ggplot(data = dataset, aes(x = What.is.your.current.occupation, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by Occupation",
       x = "Occupation",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_What.is.your.current.occupation)
print(table(dataset$What.is.your.current.occupation))

# Calculate the count of each What.matters.most.to.you.in.choosing.a.course-Converted combination
count_data <- dataset %>%
  group_by(What.matters.most.to.you.in.choosing.a.course, Converted) %>%
  summarise(count = n())

# Reorder the levels of What.matters.most.to.you.in.choosing.a.course based on the total count of each What.matters.most.to.you.in.choosing.a.course
dataset$What.matters.most.to.you.in.choosing.a.course <- factor(dataset$What.matters.most.to.you.in.choosing.a.course, levels = count_data %>%
                                                    group_by(What.matters.most.to.you.in.choosing.a.course) %>%
                                                    summarise(total_count = sum(count)) %>%
                                                    arrange(desc(total_count)) %>%
                                                    pull(What.matters.most.to.you.in.choosing.a.course))

# Create a bar plot using ggplot2 for 'What.matters.most.to.you.in.choosing.a.course' data
my_plot_What.matters.most.to.you.in.choosing.a.course <- ggplot(data = dataset, aes(x = What.matters.most.to.you.in.choosing.a.course, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by CourseSelectionImportantFactor",
       x = "CourseSelectionImportantFactor",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_What.matters.most.to.you.in.choosing.a.course)
print(table(dataset$What.matters.most.to.you.in.choosing.a.course))


# Calculate the count of each Search ads-Converted combination
count_data <- dataset %>%
  group_by(Search, Converted) %>%
  summarise(count = n())

# Reorder the levels of Search based on the total count of each Search ads
dataset$Search <- factor(dataset$Search, levels = count_data %>%
                                                                  group_by(Search) %>%
                                                                  summarise(total_count = sum(count)) %>%
                                                                  arrange(desc(total_count)) %>%
                                                                  pull(Search))

# Create a bar plot using ggplot2 for 'Search ads' data
my_plot_Search <- ggplot(data = dataset, aes(x = Search, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by SeenSearchAds",
       x = "SeenSearchAds",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_Search)
print(table(dataset$Search))

# Calculate the count of each Magazine ads-Converted combination
count_data <- dataset %>%
  group_by(Magazine, Converted) %>%
  summarise(count = n())

# Reorder the levels of Magazine based on the total count of each Magazine ads outcome
dataset$Magazine <- factor(dataset$Magazine, levels = count_data %>%
                           group_by(Magazine) %>%
                           summarise(total_count = sum(count)) %>%
                           arrange(desc(total_count)) %>%
                           pull(Magazine))

# Create a bar plot using ggplot2 for 'Magazine ads' data
my_plot_Magazine <- ggplot(data = dataset, aes(x = Magazine, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by SeenMagazineAds",
       x = "SeenMagazineAds",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_Magazine)
print(table(dataset$Magazine))

# Calculate the count of each Newspaper.Article ads-Converted combination
count_data <- dataset %>%
  group_by(Newspaper.Article, Converted) %>%
  summarise(count = n())

# Reorder the levels of Newspaper.Article based on the total count of each Newspaper.Article ads outcome
dataset$Newspaper.Article <- factor(dataset$Newspaper.Article, levels = count_data %>%
                             group_by(Newspaper.Article) %>%
                             summarise(total_count = sum(count)) %>%
                             arrange(desc(total_count)) %>%
                             pull(Newspaper.Article))

# Create a bar plot using ggplot2 for 'Newspaper.Article ads' data
my_plot_Newspaper.Article <- ggplot(data = dataset, aes(x = Newspaper.Article, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by SeenNewspaperArticleAds",
       x = "SeenNewspaperArticleAds",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_Newspaper.Article)
print(table(dataset$Newspaper.Article))

# Calculate the count of each X.Education.Forums ads-Converted combination
count_data <- dataset %>%
  group_by(X.Education.Forums, Converted) %>%
  summarise(count = n())

# Reorder the levels of X.Education.Forums based on the total count of each X.Education.Forums ads outcome
dataset$X.Education.Forums <- factor(dataset$X.Education.Forums, levels = count_data %>%
                                      group_by(X.Education.Forums) %>%
                                      summarise(total_count = sum(count)) %>%
                                      arrange(desc(total_count)) %>%
                                      pull(X.Education.Forums))

# Create a bar plot using ggplot2 for 'X.Education.Forums' data
my_plot_X.Education.Forums <- ggplot(data = dataset, aes(x = X.Education.Forums, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by SeenXEducationForums",
       x = "SeenXEducationForums",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_X.Education.Forums)
print(table(dataset$X.Education.Forums))

# Calculate the count of each Newspaper ads-Converted combination
count_data <- dataset %>%
  group_by(Newspaper, Converted) %>%
  summarise(count = n())

# Reorder the levels of Newspaper ads based on the total count of each Newspaper ads outcome
dataset$Newspaper <- factor(dataset$Newspaper, levels = count_data %>%
                                       group_by(Newspaper) %>%
                                       summarise(total_count = sum(count)) %>%
                                       arrange(desc(total_count)) %>%
                                       pull(Newspaper))

# Create a bar plot using ggplot2 for 'Newspaper' data
my_plot_Newspaper <- ggplot(data = dataset, aes(x = Newspaper, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by SeenNewspaperAds",
       x = "SeenNewspaperAds",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_Newspaper)
print(table(dataset$Newspaper))

# Calculate the count of each Digital.Advertisement-Converted combination
count_data <- dataset %>%
  group_by(Digital.Advertisement, Converted) %>%
  summarise(count = n())

# Reorder the levels of Digital.Advertisement based on the total count of each Digital.Advertisement outcome
dataset$Digital.Advertisement <- factor(dataset$Digital.Advertisement, levels = count_data %>%
                              group_by(Digital.Advertisement) %>%
                              summarise(total_count = sum(count)) %>%
                              arrange(desc(total_count)) %>%
                              pull(Digital.Advertisement))

# Create a bar plot using ggplot2 for 'Digital.Advertisement' data
my_plot_Digital.Advertisement <- ggplot(data = dataset, aes(x = Digital.Advertisement, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by SeenDigitalAdvertisement",
       x = "SeenDigitalAdvertisement",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_Digital.Advertisement)
print(table(dataset$Digital.Advertisement))

# Calculate the count of each Through.Recommendations-Converted combination
count_data <- dataset %>%
  group_by(Through.Recommendations, Converted) %>%
  summarise(count = n())

# Reorder the levels of Through.Recommendations based on the total count of each Through.Recommendations outcome
dataset$Through.Recommendations <- factor(dataset$Through.Recommendations, levels = count_data %>%
                                          group_by(Through.Recommendations) %>%
                                          summarise(total_count = sum(count)) %>%
                                          arrange(desc(total_count)) %>%
                                          pull(Through.Recommendations))

# Create a bar plot using ggplot2 for 'Through.Recommendations' data
my_plot_Through.Recommendations <- ggplot(data = dataset, aes(x = Through.Recommendations, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by Through.Recommendations",
       x = "Through.Recommendations",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_Through.Recommendations)
print(table(dataset$Through.Recommendations))

# Calculate the count of each OptedForUpdates-Converted combination
count_data <- dataset %>%
  group_by(Receive.More.Updates.About.Our.Courses, Converted) %>%
  summarise(count = n())

# Reorder the levels of OptedForUpdates based on the total count of each OptedForUpdates outcome
dataset$Receive.More.Updates.About.Our.Courses <- factor(dataset$Receive.More.Updates.About.Our.Courses, levels = count_data %>%
                                            group_by(Receive.More.Updates.About.Our.Courses) %>%
                                            summarise(total_count = sum(count)) %>%
                                            arrange(desc(total_count)) %>%
                                            pull(Receive.More.Updates.About.Our.Courses))

# Create a bar plot using ggplot2 for 'OptedForUpdates' data
my_plot_Updates <- ggplot(data = dataset, aes(x = Receive.More.Updates.About.Our.Courses, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by OptedForUpdates",
       x = "OptedForUpdates",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_Updates)
print(table(dataset$Receive.More.Updates.About.Our.Courses))

# Calculate the count of each Tags-Converted combination
count_data <- dataset %>%
  group_by(Tags, Converted) %>%
  summarise(count = n())

# Reorder the levels of Tags based on the total count of each Tags outcome
dataset$Tags <- factor(dataset$Tags, levels = count_data %>%
                                            group_by(Tags) %>%
                                            summarise(total_count = sum(count)) %>%
                                            arrange(desc(total_count)) %>%
                                            pull(Tags))

# Create a bar plot using ggplot2 for 'Tags' data
my_plot_Tags <- ggplot(data = dataset, aes(x = Tags, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by Tags",
       x = "Tags",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_Tags)
print(table(dataset$Tags))


# Calculate the count of each Lead.Quality-Converted combination
count_data <- dataset %>%
  group_by(Lead.Quality, Converted) %>%
  summarise(count = n())

# Reorder the levels of Lead.Quality based on the total count of each Lead.Quality
dataset$Lead.Quality <- factor(dataset$Lead.Quality, levels = count_data %>%
                         group_by(Lead.Quality) %>%
                         summarise(total_count = sum(count)) %>%
                         arrange(desc(total_count)) %>%
                         pull(Lead.Quality))

# Create a bar plot using ggplot2 for 'Lead.Quality' data
my_plot_Lead.Quality <- ggplot(data = dataset, aes(x = Lead.Quality, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by Lead.Quality",
       x = "Lead.Quality",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_Lead.Quality)
print(table(dataset$Lead.Quality))

# Calculate the count of each SupplyChainContentUpdate-Converted combination
count_data <- dataset %>%
  group_by(Update.me.on.Supply.Chain.Content, Converted) %>%
  summarise(count = n())

# Reorder the levels of SupplyChainContentUpdate based on the total count of each SupplyChainContentUpdate
dataset$Update.me.on.Supply.Chain.Content <- factor(dataset$Update.me.on.Supply.Chain.Content, levels = count_data %>%
                                 group_by(Update.me.on.Supply.Chain.Content) %>%
                                 summarise(total_count = sum(count)) %>%
                                 arrange(desc(total_count)) %>%
                                 pull(Update.me.on.Supply.Chain.Content))

# Create a bar plot using ggplot2 for 'SupplyChainContentUpdate' data
my_plot_SupplyChainContentUpdate <- ggplot(data = dataset, aes(x = Update.me.on.Supply.Chain.Content, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by SupplyChainContentUpdate",
       x = "SupplyChainContentUpdate",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_SupplyChainContentUpdate)
print(table(dataset$Update.me.on.Supply.Chain.Content))

# Calculate the count of each Get.updates.on.DM.Content-Converted combination
count_data <- dataset %>%
  group_by(Get.updates.on.DM.Content, Converted) %>%
  summarise(count = n())

# Reorder the levels of Get.updates.on.DM.Content based on the total count of each Get.updates.on.DM.Content
dataset$Get.updates.on.DM.Content <- factor(dataset$Get.updates.on.DM.Content, levels = count_data %>%
                                                      group_by(Get.updates.on.DM.Content) %>%
                                                      summarise(total_count = sum(count)) %>%
                                                      arrange(desc(total_count)) %>%
                                                      pull(Get.updates.on.DM.Content))

# Create a bar plot using ggplot2 for 'DMContentUpdate' data
my_plot_DMContentUpdate <- ggplot(data = dataset, aes(x = Get.updates.on.DM.Content, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by DMContentUpdate",
       x = "DMContentUpdate",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_DMContentUpdate)
print(table(dataset$Get.updates.on.DM.Content))

# Calculate the count of each Lead.Profile-Converted combination
count_data <- dataset %>%
  group_by(Lead.Profile, Converted) %>%
  summarise(count = n())

# Reorder the levels of Lead.Profile based on the total count of each Lead.Profile
dataset$Lead.Profile <- factor(dataset$Lead.Profile, levels = count_data %>%
                                              group_by(Lead.Profile) %>%
                                              summarise(total_count = sum(count)) %>%
                                              arrange(desc(total_count)) %>%
                                              pull(Lead.Profile))

# Create a bar plot using ggplot2 for 'Lead.Profile' data
my_plot_DMContentUpdate <- ggplot(data = dataset, aes(x = Lead.Profile, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by Lead.Profile",
       x = "Lead.Profile",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_DMContentUpdate)
print(table(dataset$Lead.Profile))

# Calculate the count of each City-Converted combination
count_data <- dataset %>%
  group_by(City, Converted) %>%
  summarise(count = n())

# Reorder the levels of City based on the total count of each City
dataset$City <- factor(dataset$City, levels = count_data %>%
                                 group_by(City) %>%
                                 summarise(total_count = sum(count)) %>%
                                 arrange(desc(total_count)) %>%
                                 pull(City))

# Create a bar plot using ggplot2 for 'City' data
my_plot_City <- ggplot(data = dataset, aes(x = City, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by City",
       x = "City",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_City)
print(table(dataset$City))

# Calculate the count of each Asymmetrique.Activity.Index-Converted combination
count_data <- dataset %>%
  group_by(Asymmetrique.Activity.Index, Converted) %>%
  summarise(count = n())

# Reorder the levels of Asymmetrique.Activity.Index based on the total count of each Asymmetrique.Activity.Index
dataset$Asymmetrique.Activity.Index <- factor(dataset$Asymmetrique.Activity.Index, levels = count_data %>%
                         group_by(Asymmetrique.Activity.Index) %>%
                         summarise(total_count = sum(count)) %>%
                         arrange(desc(total_count)) %>%
                         pull(Asymmetrique.Activity.Index))

# Create a bar plot using ggplot2 for 'Asymmetrique.Activity.Index' data
my_plot_Asymmetrique.Activity.Index <- ggplot(data = dataset, aes(x = Asymmetrique.Activity.Index, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by Asymmetrique Activity Index",
       x = "Asymmetrique Activity Index",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_Asymmetrique.Activity.Index)
print(table(dataset$Asymmetrique.Activity.Index))

# Calculate the count of each Asymmetrique.Profile.Index-Converted combination
count_data <- dataset %>%
  group_by(Asymmetrique.Profile.Index, Converted) %>%
  summarise(count = n())

# Reorder the levels of Asymmetrique.Profile.Index based on the total count of each Asymmetrique.Profile.Index
dataset$Asymmetrique.Profile.Index <- factor(dataset$Asymmetrique.Profile.Index, levels = count_data %>%
                                                group_by(Asymmetrique.Profile.Index) %>%
                                                summarise(total_count = sum(count)) %>%
                                                arrange(desc(total_count)) %>%
                                                pull(Asymmetrique.Profile.Index))

# Create a bar plot using ggplot2 for 'Asymmetrique.Profile.Index' data
my_plot_Asymmetrique.Profile.Index <- ggplot(data = dataset, aes(x = Asymmetrique.Profile.Index, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by Asymmetrique Profile Index",
       x = "Asymmetrique Profile Index",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_Asymmetrique.Profile.Index)
print(table(dataset$Asymmetrique.Profile.Index))

# Calculate the count of each ChequePayment-Converted combination
count_data <- dataset %>%
  group_by(I.agree.to.pay.the.amount.through.cheque, Converted) %>%
  summarise(count = n())

# Reorder the levels of ChequePayment based on the total count of each ChequePayment
dataset$I.agree.to.pay.the.amount.through.cheque <- factor(dataset$I.agree.to.pay.the.amount.through.cheque, levels = count_data %>%
                                               group_by(I.agree.to.pay.the.amount.through.cheque) %>%
                                               summarise(total_count = sum(count)) %>%
                                               arrange(desc(total_count)) %>%
                                               pull(I.agree.to.pay.the.amount.through.cheque))

# Create a bar plot using ggplot2 for 'ChequePayment' data
my_plot_ChequePayment <- ggplot(data = dataset, aes(x = I.agree.to.pay.the.amount.through.cheque, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by ChequePayment",
       x = "ChequePayment",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_ChequePayment)
print(table(dataset$I.agree.to.pay.the.amount.through.cheque))

# Calculate the count of each FreeBook-Converted combination
count_data <- dataset %>%
  group_by(A.free.copy.of.Mastering.The.Interview, Converted) %>%
  summarise(count = n())

# Reorder the levels of FreeBook based on the total count of each FreeBook
dataset$A.free.copy.of.Mastering.The.Interview <- factor(dataset$A.free.copy.of.Mastering.The.Interview, levels = count_data %>%
                                                             group_by(A.free.copy.of.Mastering.The.Interview) %>%
                                                             summarise(total_count = sum(count)) %>%
                                                             arrange(desc(total_count)) %>%
                                                             pull(A.free.copy.of.Mastering.The.Interview))

# Create a bar plot using ggplot2 for 'FreeBook' data
my_plot_FreeBook <- ggplot(data = dataset, aes(x = A.free.copy.of.Mastering.The.Interview, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by FreeBook",
       x = "FreeBook",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_FreeBook)
print(table(dataset$A.free.copy.of.Mastering.The.Interview))


# Calculate the count of each Last.Notable.Activity-Converted combination
count_data <- dataset %>%
  group_by(Last.Notable.Activity, Converted) %>%
  summarise(count = n())

# Reorder the levels of Last.Notable.Activity based on the total count of each Last.Notable.Activity
dataset$Last.Notable.Activity <- factor(dataset$Last.Notable.Activity, levels = count_data %>%
                                                           group_by(Last.Notable.Activity) %>%
                                                           summarise(total_count = sum(count)) %>%
                                                           arrange(desc(total_count)) %>%
                                                           pull(Last.Notable.Activity))

# Create a bar plot using ggplot2 for 'Last.Notable.Activity' data
my_plot_Last.Notable.Activity <- ggplot(data = dataset, aes(x = Last.Notable.Activity, fill = Converted)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot of Conversion Status by Last.Notable.Activity",
       x = "Last.Notable.Activity",
       y = "Conversion status of the leads",
       fill = "Converted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
print(my_plot_Last.Notable.Activity)
print(table(dataset$Last.Notable.Activity))


# List of column names indicating continuous variables
continuous_column_names <- c("Converted", "TotalVisits", "Total.Time.Spent.on.Website", "Page.Views.Per.Visit", "Asymmetrique.Activity.Score", "Asymmetrique.Profile.Score")

# Creating a new dataframe with only continuous variables
continuous_vars <- dataset[, continuous_column_names]
continuous_vars <- apply(continuous_vars, 2, as.numeric)
continuous_vars <- continuous_vars[complete.cases(continuous_vars), ]

str(continuous_vars)
# Calculating the correlation matrix for continuous variables
correlation_matrix <- cor(continuous_vars)
print(correlation_matrix)


corrplot(correlation_matrix)
