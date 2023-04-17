
setwd("E:/Netflix/")

# install.packages("readr")
library(readr)
df <- read_csv("titles.csv")
df1 <- read.csv("credits.csv")

# EDA and data cleaning

# cleaning data

# 1- find data type
# convert need for categorical data , as a factor

df$type <- as.factor(df$type)
df1$role <- as.factor(df1$role)
# df$age_certification <- as.factor(df$age_certification)
# df$genres <- as.factor(df$genres)


library(dplyr)
glimpse(df)
glimpse(df1)

str(df)
head(df)
tail(df)
summary(df)
summary(df1)
str(df1)
unique(df$production_countries)

# 2-find out of range
# 3- find duplicate
sum(duplicated(df))
sum(duplicated(df$genres))
#4- find levels
levels(df$type)
levels(df1$role)
levels(df$age_certification)

# missing value
sum(is.na(df))
sum(is.na(df1))

# see miss value
# install.packages("visdat")

library(visdat)
vis_miss(df)

# fill season with median

df$seasons <- ifelse(is.na(df$seasons), median(df$seasons, na.rm = TRUE), df$seasons)

# drop age_certificate and production_countries is null
# df2 <-  df %>% 
#   filter(!is.na(age_certification))

df2 <- df

library(stringi)

vis_miss(df2)

df1$name <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", df1$name)


# ******************************
#  deal with braket

library(stringr)

# str_detect(df3$production_countries,fixed("["))
df2$production_countries <- str_remove_all(df2$production_countries,fixed("["))
# str_detect(df3$production_countries,fixed("]"))
df2$production_countries <- str_remove_all(df2$production_countries,fixed("]"))
df2$production_countries <- str_remove_all(df2$production_countries,("'"))


df2$genres <- str_remove_all(df2$genres,fixed("["))
df2$genres <- str_remove_all(df2$genres,fixed("]"))
df2$genres <- str_remove_all(df2$genres,("'"))

df2 <-  df2 %>% 
  filter((stri_isempty(production_countries) == FALSE))

#  
#df$n_gender <- str_remove_all(df$genres,fixed("[]"))

# library(magrittr)
# df %<>% dplyr::rename_all(make.names)

# ************************************
# creadit data base
sum(is.na(df1))

#  EDA

# fill null age_certification with max category

df2$age_categories <- ifelse(is.na(df2$age_certification) == TRUE, "Mature_Audience",
                             ifelse(df2$age_certification == "TV-PG" ,"Parental_Guidance" ,
                                    ifelse(df2$age_certification == "TV-MA" , "Mature_Audience", 
                                           ifelse(df2$age_certification == "TV-Y7" , "Teens",
                                                  ifelse(df2$age_certification == "TV-14" , "Teens",
                                                         ifelse(df2$age_certification == "TV-14" , "Teens",
                                                                ifelse(df2$age_certification == "R" , "Mature_Audience",
                                                                       ifelse(df2$age_certification == "TV-Y" , "Teens", 
                                                                              ifelse(df2$age_certification == "PG-13" , "Teens",
                                                                                     ifelse(df2$age_certification == "TV-G" , "General_Audience",
                                                                                            ifelse(df2$age_certification == "PG" , "Teens",
                                                                                                   ifelse(df2$age_certification == "NC-17" , "Mature_Audience",
                                                                                                          ifelse(df2$age_certification == "G" , "General_Audience",df2$age_certification)))))))))))))

# df2$age_categories1 <- switch(
#   df2$age_certification, 
#   "TV-PG" = "Parental_Guidance",
#   "TV-MA" = "Mature_Audience",
#   "TV-Y7" = "Teens",
#   "TV-14" = "Teens",
#   "R" = "Mature_Audience",
#   "TV-Y" = "Teens",
#   "PG-13" = "Teens",
#   "TV-G" = "General_Audience",
#   "PG" = "Teens",
#   "NC-17" = "Mature_Audience",
#   "G" = "General_Audience",
#   df2$age_certification
#   
# )

# visualization

# *******************************************************************************
# correlation
library(dplyr)

df33 <- print(select_if(df2, is.numeric))
corr <- round(cor(df33), 1)

#-------------------------------------------------------------------------------
library(ggcorrplot)
ggcorrplot(corr, tl.cex = 7 , title = "Correlation Plot", method  =  "circle",
           lab = TRUE , lab_size = 3)

# ***************************************************************************
# dev.off()

# ----------------Distribution of the release_year----------------------

ggplot(df2, aes(x = release_year)) +
  geom_histogram(color="#008080", fill="#00FF00") +
  
  labs(subtitle="Number of Movies/Shows released year wise", 
       y="Numbe", 
       x="Years", 
       title="Histogram", 
       caption = "Source: Netflix")



# ----------------------------------------------------------------------
dfy <- df2 %>% 
  group_by(release_year, type)%>%
  summarize(name_rows= n())

ggplot(dfy, aes(x = release_year ,y= name_rows, group=type, colour=type )) +
  geom_line(size=1.5) +
  labs(subtitle="Number of Movies/Shows released by year and type from 1945 to 2022", 
       y="Number", 
       x="Years", 
       title="Line", 
       caption = "Source: Netflix")

# ----------------Number of Films by type----------------------

ggplot(df2, aes(x = type)) +
  geom_bar(color="#9ACD32", fill="#BDB76B" ) +
  labs(subtitle="Category by distribution", 
       y="Number", 
       x="Type", 
       title="Bar chart", 
       caption = "Source: Netflix")

# ----------------films by Audience----------------------

ggplot(df2, aes(x = age_categories)) +
  geom_bar(color="#FF69B4", fill="#FF1493")  +
  labs(subtitle="Distributions of Audience ", 
       y="Number", 
       x="Type", 
       title="Bar chart", 
       caption = "Source: Netflix")

# ----------------Category by type and Audience----------------------

ggplot(df2, aes(x = type , fill = age_categories)) +
  geom_bar() +
  labs(subtitle="Category by type and Audience ", 
       y="Number", 
       x="Type", 
       title="Bar chart", 
       caption = "Source: Netflix")


ggplot(df2, aes(x = age_categories , fill = type )) +
  geom_bar() +
  labs(subtitle="Category by type and Audience ", 
       y="Number", 
       x="Type", 
       title="Bar chart", 
       caption = "Source: Netflix")


# --------------------------Year of release by Audience---------------------------------

ggplot(df2, aes(x = release_year, fill = age_categories)) +
  geom_density()+
  labs(subtitle="Year of release Movie/shows by Audience ", 
       y="Number", 
       x="year", 
       title="Density plot", 
       caption = "Source: Netflix")
# ------------------------Year of release by type-----------------------------------
ggplot(df2, aes(x = release_year, fill = type)) +
  geom_density()+
  labs(subtitle="Year of release Movie/shows by Type ", 
       y="Number of Films by Type", 
       x="year", 
       title="Density plot", 
       caption = "Source: Netflix")
# ----------------------------------top 10 Production Countries------------------------------------
# install.packages("splitstackshape")

# split multi coulmn to seprate (production_countries)

library(splitstackshape)
# df3 <- cSplit(df2,'production_countries' , ',','long')


dd <- df2 %>% 
  group_by(production_countries)%>%
  summarize(name_rows= n())

dd1 <- dd[order(dd$name_rows,decreasing = TRUE),]
dd2 <- top_n(dd1, n=10, name_rows)
dd3 <- transform(dd2,production_countries = reorder(production_countries,order(name_rows, decreasing = TRUE)))

ggplot(dd3) + 
  geom_bar(aes(x = production_countries, y = name_rows), stat = 'identity', color="#40E0D0", fill="#00CED1") +
  labs(subtitle="Top 10 Production Countries ", 
       y="Number", 
       x="country", 
       title="Bar plot", 
       caption = "Source: Netflix")


# ----------------------------------Top 10 popularity------------------------------------ 

library(ggplot2)
library(dplyr)

#   top_n(df, n=3, tmdb_popularity) %>%
#   ggplot(., aes(x=title, y=tmdb_popularity))+
#   geom_bar(stat='identity')

df4 <- df2[order(df2$tmdb_popularity,decreasing = FALSE),]

df5 <- top_n(df4, n=10, tmdb_popularity)
df6 <- transform(df5,title = reorder(title,order(tmdb_popularity, decreasing = FALSE)))
ggplot(df6) + 
  geom_bar(aes(x = title, y = tmdb_popularity), stat = 'identity',color = "#BA55D3" , fill = "#9932CC") +
  coord_flip() + 
  labs(subtitle="Top 10 Movies / show by popularity ", 
       y="popularity", 
       x="Name", 
       title="Bar plot", 
       caption = "Source: Netflix")

# --------------------------Top 10 Actor--------------------------------
da <- filter(df1, role == "ACTOR")
da1 <- da %>% 
  group_by(name)%>%
  summarize(name_rows= n())

da2 <- da1[order(da1$name_rows,decreasing = FALSE),]
da3 <- top_n(da2, n=10, name_rows)
da4 <- transform(da3,name = reorder(name,order(name_rows, decreasing = FALSE)))
ggplot(da4) + 
  geom_bar(aes(x = name, y = name_rows), stat = 'identity', color="#FF69B4", fill="#FF69B4") +
  coord_flip() + 
  labs(subtitle="Actors with the most number of appearence in the Movies/TV Shows ", 
       y="Number of appearence", 
       x="Name ", 
       title="Bar plot", 
       caption = "Source: Netflix")

# 
# --------------------------Top 10 director--------------------------------
dr <- filter(df1, role == "DIRECTOR")
dr1 <- dr %>% 
  group_by(name)%>%
  summarize(name_rows= n())

dr2 <- dr1[order(dr1$name_rows,decreasing = FALSE),]
dr3 <- top_n(dr2, n=10, name_rows)
dr4 <- transform(dr3,name = reorder(name,order(name_rows, decreasing = FALSE)))
ggplot(dr4) + 
  geom_bar(aes(x = name, y = name_rows), stat = 'identity', color="#20B2AA", fill="#20B2AA") +
  coord_flip() + 
  labs(subtitle="Directors with the most number of Movies/TV Shows ", 
       y="Number of Movies / Shows", 
       x="Name", 
       title="Bar plot", 
       caption = "Source: Netflix")
# --------------------------Top 10 genres--------------------------------  

library(splitstackshape)
# dfg <- cSplit(df2,'genres' , ',','long')

dfg1 <- df2 %>% 
  group_by(genres)%>%
  summarize(name_rows= n())

dfg2 <- dfg1[order(dfg1$name_rows,decreasing = FALSE),]
dfg3 <- top_n(dfg2, n=10, name_rows)
dfg4 <- transform(dfg3,genres = reorder(genres,order(name_rows, decreasing = FALSE)))

ggplot(dfg4) + 
  geom_bar(aes(x = genres, y = name_rows), stat = 'identity', color="#40E0D0", fill="#7FFFD4") +
  coord_flip() + 
  labs(subtitle="Top 10 genres ", 
       y="Number", 
       x="genres", 
       title="Bar plot", 
       caption = "Source: Netflix")



