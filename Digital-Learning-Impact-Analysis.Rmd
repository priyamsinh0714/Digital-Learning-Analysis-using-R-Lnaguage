---
title: "Assesssment 1 - COVID-19 impact on digital learning"
author: "Priyam and 47750731"
date: "2024-04-12"
output:
  pdf_document: default
  word_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


``` {r, include=FALSE}
#we will include the libraries, if not available they were installed 
library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(here)
library(janitor)
library(skimr)
library(usmap)
library(viridis)
library(DataExplorer)
library(lubridate)
library(ggthemes)
library(corrplot)
library(ggplot2)


```



# Data Description

Over 56 million students in the US had disruptions in their education because of the COVID-19 pandemic. To stop the virus from spreading, the majority of states and local governments in the United States closed their educational institutions in the spring of 2020. Schools and teachers have responded by attempting to connect with pupils virtually using digital platforms and tools for distance learning. Even now, worries about the widening digital divide and long-term learning loss among the most disadvantaged students in America are becoming more and more pressing.

 
# Data upload
* Now we'll load the entire dataset—distributions, products, and engagements of the districts.

``` {r,echo=FALSE,message=FALSE ,warning=FALSE}


districtsInfo = read_csv("districts_info.csv", na=c("","NA", "NaN"), col_types = cols(.default = "c", district_id ="d"))
productsInfo = read_csv("products_info.csv", na=c("","NA", "NaN"), col_types = cols(.default = "c", `LP ID` ="d"))


d1000 = read_csv("1000.csv", na=c("","NA", "NaN"), col_types = cols(.default = "c", `lp_id` ="d"))
d1039 = read_csv("1039.csv", na=c("","NA", "NaN"), col_types = cols(.default = "c", `lp_id` ="d"))
d1044 = read_csv("1044.csv", na=c("","NA", "NaN"), col_types = cols(.default = "c", `lp_id` ="d"))
d1052 = read_csv("1052.csv", na=c("","NA", "NaN"), col_types = cols(.default = "c", `lp_id` ="d"))
d1131 = read_csv("1131.csv", na=c("","NA", "NaN"), col_types = cols(.default = "c", `lp_id` ="d"))
```



# Districts Dataset
* District-specific data, including information from Edunomics Lab, the FCC (Dec. 2018), and NCES (2018–19), is included in the district file districts_info.csv. 
* We eliminated the school districts' personally identifiable information from this data collection. In order to alter many data fields and lower the possibility of re-identification, we additionally employed the open-source program ARX (Prasser et al. 2020). 
* Certain data points are released with a range that the actual value falls under in order to aid with data generalization. Furthermore, a large number of missing data points are designated as "NaN," suggesting that the data was suppressed to increase the dataset's anonymity.
``` {r,echo=FALSE,message=FALSE}
summary(districtsInfo)
```


* The above shows the dimension of the 'Districts' dataset. 
* We can see the districts file has the column variable district_id as the numeric, and the rest of the variables are characters.
* Let's do data cleaning and pre-processing of Data.districts info 
Summary of the data in terms of data type and summary statistics of the numeric variable Now, we will take each column and do the cleaning and pre processing of the variable. 

## District's State
Let's take the state column first.
``` {r,echo=FALSE,message=FALSE, warning = F}
#Now we will focus on each column wise
print(summary(districtsInfo$state))
``` 


* The state column has 233 rows which has a character type.
``` {r,echo=FALSE,message=FALSE}
#Currently we have the following states list
print(districtsInfo %>%
  group_by(state) %>% summarise(count = n()),n = 35)
```


* As we can see, we don't know, and whereabouts seem vague. 
* We will check on all the districts that are part of the engagement, as they will be crucial. 
* As we can also see, there are 55 NA, and without data cleaning, there are 32 states, but that's incorrect as there are discrepancies like Utah, New York, and Ohio. It will be correct in this.
* Before that, let's see the districts 1000, 1039, 1044, 1052, and 1131. Since these are engagement files, let's see these districts.
``` {r,echo=FALSE,message=FALSE}
districtsInfo %>% filter( district_id %in% c(1000, 1039,1044,1052,1131))
```


* As we can see, 1131 doesn't know the state, which means we don't know the state, but in merging the engagement districts, we cannot keep it as this as we want this row. Similarly, we need 1039 rows as well. Therefore, we will keep 1039 rows by keeping their state name as the district ID.

``` {r,echo=FALSE,message=FALSE}
#since this district 1131 is a part of engagement file 
#we will keep the state name as 1131,1039  to avoid any data redundancy
districtsInfo$state[ districtsInfo$district_id == 1131] <- "1131"
districtsInfo$state[ districtsInfo$district_id == 1039] <- "1039"

#whereabouts seem a vague way so we will replace with NA 

districtsInfo$state[ districtsInfo$state == "Whereabouts"] <- NA 
```



* Correcting the spelling errors by first entering the title in title format and then using the replace function.
``` {r,echo=FALSE,message=FALSE}


districtsInfo <- districtsInfo %>%
       mutate(state = str_to_title(state)) %>% 
  #str_to_title() automatically forces the first letter 
  #to be Upper Case and the other letters to be Lower Case. 
       mutate(state = replace(state, state %in% c("Uttah", "Utaah"), "Utah") ) %>%
       mutate(state = replace(state, state %in% c("New Y0rk","Ny City"), "New York") )%>%
       mutate(state = replace(state, state == "Ohi0","Ohio") ) 
 
print(districtsInfo %>%
        group_by(state) %>% 
        summarise(count = n(), percentage = count/233 * 100),n = 35) %>%
        arrange(desc(percentage))
```
* Removing the NA of the district file because they are 23.17%, which is a huge number, but without knowing the state name of the district, we cannot analyze the further district demographics of it. The government will not be able to identify further over the districts that have na as the district.
``` {r,echo=FALSE,message=FALSE}
#as we can see there's no point of having the district whose state is not decided ??? 
districtsInfo <- districtsInfo %>% filter(!is.na(state))
``` 
## District's Locale


* Let's take the second column of the NCES locale classification that categorizes U.S. territory into four types of areas: city, suburban, town, and rural.
``` {r,echo=FALSE,message=FALSE}
#Districts locale

print(districtsInfo %>%
        group_by(locale) %>% summarise(count = n(), percentage = count / 179 * 100),n = 35)
```
* There are 3 NA's, of which 1.6% we will replace. As there's no point in deleting rows, we will replace them with the most common locale. 
* We can see there are some similarities between cities; there are a few of these; we will replace them. Set them into the required categories.
``` {r,echo=FALSE,message=FALSE}
#as we can see there are multiple names to the same city we will replace it.
districtsInfo <- districtsInfo %>%
  mutate(locale = str_to_title(locale)) %>%
  mutate(locale  = replace(locale, locale %in% c("C1ty", "Cit"),"City") ) %>%
  mutate(locale  = replace(locale , locale  == "Sub", "Suburb") )
print(districtsInfo %>%
        group_by(locale) %>% summarise(count = n(), percentge = n()/ 179 * 100),n = 35)
```


* Now we will replace NA's and see the results.
``` {r,echo=FALSE,message=FALSE}
#ce with most common
which(is.na(districtsInfo$locale)) #index for NA value
districtsInfo$locale[23] <- "Suburb"
districtsInfo$locale[24] <- "Suburb"
districtsInfo$locale[128] <- "Suburb"

print(districtsInfo %>%
        group_by(locale) %>% summarise(count = n()),n = 35)
``` 


## District's Minority Range

* Let's take the next column. Pct black/hispanic Percentage of students in the districts identified as black or Hispanic based on 2018–19 NCES data As it seems like a long variable name, we will cut it to the minority range.
``` {r,echo=FALSE,message=FALSE}
#DistrictsInfo Pct black/hispanic 
#Percentage of students in the districts eligible for free or reduced-price lunch based on 2018-19 NCES data
names(districtsInfo)[4] = "minority_range" #percentage
print(districtsInfo %>%
        group_by(minority_range) %>% summarise(count = n(), percentge = n()/ 179 * 100),n = 35)
```


* "[0, 0.2]" is determined as 0 to 20%. We will replace this with the middle number of 10% for easy understanding. We will convert this to a numeric data type. - We will replace the na values with the median of the entire column.
``` {r,echo=FALSE,message=FALSE}
districtsInfo <- districtsInfo %>% 
  mutate( minority_range = replace(minority_range, minority_range == "[0, 0.2[", "0.1") ) %>%
  mutate(minority_range = replace(minority_range, minority_range == "[0.2, 0.4[", "0.3") ) %>%
  mutate(minority_range = replace(minority_range, minority_range == "[0.4, 0.6[", "0.5") ) %>%
  mutate(minority_range = replace(minority_range, minority_range == "[0.6, 0.8[","0.7") ) %>%
  mutate(minority_range = replace(minority_range, minority_range == "[0.8, 1[", "0.9") ) 

#as percentage is a numeric data type therefore converting it 
districtsInfo$minority_range <- as.numeric(districtsInfo$minority_range) 

#replacing na with median  
vec <- which(is.na(districtsInfo$minority_range))

districtsInfo$minority_range[vec] <- median(districtsInfo$minority_range,na.rm = T) #median value 

print(districtsInfo %>%
        group_by(minority_range) %>% summarise(count = n(), percentge = n()/ 179 * 100),n = 35)
``` 


## District's pct_free/reduced

*Now, we will take the next column, pct_free/reduced. Percentage of students in the districts eligible for free or reduced-price lunch based on 2018-19 NCES data
As it seems like a long variable name, we will cut it to the free range.
``` {r,echo=FALSE,message=FALSE}
#Percentage of students in the districts eligible for free or reduced-price lunch based on 2018-19 NCES data
names(districtsInfo)[5] = "free_range"
print(districtsInfo %>%
        group_by(free_range) %>% summarise(count = n(), percentge = n()/ 179 * 100),n = 35)
```


* "[0, 0.2[" determines as 0 to 20 % we will replace this as the middle number 10% for easy understanding. we will convert this to numeric data type.
* We will replace the na values with the median of the entire column.
``` {r,echo=FALSE,message=FALSE}
districtsInfo <- districtsInfo %>%
  mutate( free_range = replace(free_range, free_range == "[0, 0.2[", "0.1") ) %>%
  mutate(free_range = replace(free_range, free_range == "[0.2, 0.4[", "0.3") ) %>%
  mutate(free_range = replace(free_range, free_range == "[0.4, 0.6[", "0.5") ) %>%
  mutate(free_range = replace(free_range, free_range == "[0.6, 0.8[","0.7") ) %>%
  mutate(free_range = replace(free_range, free_range == "[0.8, 1[", "0.9") ) 

districtsInfo$free_range <- as.numeric(districtsInfo$free_range)  
vec <- which(is.na(districtsInfo$free_range))

districtsInfo$free_range[vec] <- median(districtsInfo$free_range,na.rm = T) #median value 

print(districtsInfo %>%
        group_by(free_range) %>% summarise(count = n() , percentge = n()/ 179 * 100),n = 35)
``` 


## District's Expense per pupil

* pp_total_raw	Per-pupil total expenditure (sum of local and federal expenditure) from Edunomics Lab’s National Education Resource Database on Schools (NERD$) project. The expenditure data are school-by-school, and we use the median value to represent the expenditure of a given school district
* As it seems the long variable name we will cut it to the free range
``` {r,echo=FALSE,message=FALSE}
#PP_TOTAL_RAW
#Per-pupil total expenditure (sum of local and federal expenditure) from Edunomics Lab’s National Education Resource Database on Schools (NERD$) project. 
#The expenditure data are school-by-school, and we use the median value to represent the expenditure of a given school district.
names(districtsInfo)[7] = "Expenseperpupil"
print(districtsInfo %>% group_by(Expenseperpupil) %>% summarise(count = n(),percentge = n()/ 179 * 100),n = 35)
```



* "[10000, 12000[", determines as 10000 to 12000 we will replace this as the mean value number 1 for easy understanding. we will convert this to numeric data type.
* we will replace the na values with the median of the entire column.
``` {r,echo=FALSE,message=FALSE}

districtsInfo <- districtsInfo %>%
  mutate( Expenseperpupil = replace(Expenseperpupil, Expenseperpupil == "[10000, 12000[", "11000") ) %>%
  mutate(Expenseperpupil = replace(Expenseperpupil, Expenseperpupil == "[12000, 14000[", "13000")  ) %>%
  mutate(Expenseperpupil = replace(Expenseperpupil, Expenseperpupil == "[14000, 16000[", "15000") )%>%
  mutate(Expenseperpupil = replace(Expenseperpupil, Expenseperpupil == "[16000, 18000[", "17000") ) %>%
  mutate(Expenseperpupil = replace(Expenseperpupil, Expenseperpupil == "[18000, 20000[", "19000") ) %>%
  mutate(Expenseperpupil = replace(Expenseperpupil, Expenseperpupil == "[20000, 22000[", "21000") ) %>%
  mutate(Expenseperpupil = replace(Expenseperpupil, Expenseperpupil == "[22000, 24000[", "23000") ) %>%
  mutate(Expenseperpupil = replace(Expenseperpupil, Expenseperpupil == "[32000, 34000[", "33000") ) %>%
  mutate(Expenseperpupil = replace(Expenseperpupil, Expenseperpupil == "[4000, 6000[", "5000") ) %>%
  mutate(Expenseperpupil = replace(Expenseperpupil, Expenseperpupil == "[6000, 8000[", "7000") ) %>%
  mutate(Expenseperpupil= replace(Expenseperpupil, Expenseperpupil == "[8000, 10000[", "9000") ) 

#converting into numeric
districtsInfo$Expenseperpupil <- as.numeric(districtsInfo$Expenseperpupil)  


vec <- which(is.na(districtsInfo$Expenseperpupil))
#as there are 25% of na so we can not omit it 

districtsInfo$Expenseperpupil[vec] <- median(districtsInfo$Expenseperpupil,na.rm = T)  #median value 

print(districtsInfo %>% group_by(Expenseperpupil) %>% summarise(count = n(),percentge = n()/ 233 * 100),n = 35)
```



## District's Connection Ratio 
let's take the next column county_connections_ratio	ratio (residential fixed high-speed connections over 200 kbps in at least one direction/households) based on the county level data from FCC From 477 (December 2018 version). 
``` {r,echo=FALSE,message=FALSE}
#County Connection Ratio
names(districtsInfo)[6] = "Ratio"
print(districtsInfo %>% group_by(Ratio) %>% summarise(count = n(), percentge = n()/ 233 * 100),n = 35)
```


* "[0.18, 1[", determines as 18% to 100% we will replace this as the 18% number easy understanding. we will convert this to numeric data type
* We will replace the na values with the median of the entire column.
``` {r,echo=FALSE,message=FALSE}
districtsInfo <- districtsInfo %>%
  mutate( Ratio = replace( Ratio, Ratio == "[0.18, 1[", "0.18") ) %>%
  mutate( Ratio = replace( Ratio, Ratio == "[1, 2[", "0.5")  )

districtsInfo$Ratio <- as.numeric(districtsInfo$Ratio)  
vec <- which(is.na(districtsInfo$Ratio))
districtsInfo$Ratio[vec] <- median(districtsInfo$Ratio,na.rm = T) 
```


#Product Dataset 
*The product file products_info.csv includes information about the characteristics of the top 372 products with most users in 2020. The categories listed in this file are part of LearnPlatform’s product taxonomy. Data were labeled by our team. Some products may not have labels due to being duplicate, lack of accurate url or other reasons

*Lets take each column individually and do the data cleaning and data preprocessing wherever required.

*The first column is the Sector. Sector(s)	 of education where the product is used
## Product's Sector
``` {r,echo=FALSE,message=FALSE}
#products

names(productsInfo)[5] = "Sector"
print(productsInfo %>% group_by(Sector) %>% summarise(count = n(), percentge = n()/ 372 * 100),n = 35)
```



* There are 18 na which is 4.8% of the column. Lets also try to minimise the categories by combining the similar categories name.
* Not sure is replaced by NA, Further NA is replaced by most common category.

``` {r,echo=FALSE,message=FALSE}
#not sure is the na 
productsInfo$Sector[productsInfo$Sector == "not sure"] <- NA

productsInfo <- productsInfo %>%
  mutate(Sector = str_to_title(Sector)) %>%
  mutate( Sector = replace(Sector, Sector %in% c("Pprek-12", "Prek-112", "Prek-122", "Pprek-12", "Prek-12", 
                                                 "Pre Kindergarten To Year 12", "Pre Kindergarten To Yr 12"), "Prek-12") ) %>%
  mutate( Sector = replace(Sector, Sector %in% c("Higher Ed; Corporate", "Prek-12; Higher Ed; Corporate", "Prek-12; Higher; Corporate"), "Corporate") ) %>%
  mutate( Sector = replace(Sector, Sector == "Prek-12; Higher Ed", "Higher Ed") )

  
vec <- which(is.na(productsInfo$Sector))
productsInfo$Sector[vec] <- "Prek-12"
print(productsInfo %>% group_by(Sector) %>% summarise(count = n(), percentage = n()/372 * 100),n = 35)
```

## Product's Primary Function

* lets take the next column 
* Primary Essential Function	The basic function of the product. There are two layers of labels here. Products are first labeled as one of these three categories: LC = Learning & Curriculum, CM = Classroom Management, and SDO = School & District Operations. Each of these categories have multiple sub-categories with which the products were labeled.
``` {r,echo=FALSE,message=FALSE}
head(productsInfo$`Primary Essential Function`)
```


* we can see they have combined the function we will separate into two columns, Primary and secondary for category creation as part of data processing.
``` {r,echo=FALSE,message=FALSE} 

#ProductsInfo Primary essentail

productsInfo <- productsInfo %>% 
  separate(`Primary Essential Function`, c("Primary", "Secondary"), "-")
print(productsInfo %>% group_by(Primary) %>% summarise(count = n(), percentage = n()/372 * 100),n = 35)
```
N
``` {r,echo=FALSE,message=FALSE}
productsInfo <- productsInfo %>%
  mutate( Primary = replace(Primary, Primary == "CL ", "CM ") ) %>%
  mutate( Primary = replace(Primary, Primary ==  "LC/CM/SDO ", "LC ")) %>%
  mutate( Primary = replace(Primary, Primary ==  "LC", "LC ")) 


#since maximum %age of data is LC we can replae few na with LC 
vec <- which(is.na(productsInfo$Primary))
productsInfo$Primary[vec] <- "LC "


print(productsInfo %>% group_by(Primary) %>% summarise(count = n(), percentage = n()/372 * 100),n = 35)
```


* Now we have the 3 category data of CM, LC and SDO as the primary function
  
* Now let's look into the secondary function colum that we have created


## Product's Secondary Function
``` {r,echo=FALSE,message=FALSE}
#Secondary
print(productsInfo %>% group_by(Secondary) %>% 
        summarise(count = n(), percentage = n()/372 * 100) %>%
        arrange(desc(percentage)), n = 35)
```


* There are 23 rows with 20 NA since there are too many secondary function we will replace them as unknown category as  top secondary function is just the 19%. 
* There was the one text error in which extra space was used because of this it converted into two different categories. they are same.
``` {r,echo=FALSE,message=FALSE}

productsInfo <- productsInfo %>%
  mutate( Secondary = replace(Secondary, Secondary == " Sites, Resources & Reference ", " Sites, Resources & Reference") )

vec <- which(is.na(productsInfo$Secondary))
productsInfo$Secondary[vec] <- "Unknown"
print(productsInfo %>% group_by(Secondary) %>% 
        summarise(count = n(), percentage = n()/372 * 100) %>%
        arrange(desc(percentage)), n = 35)
```


* Now, Let's take the next column Provider/Company Name	Name of the product provider

``` {r,echo=FALSE,message=FALSE}

#provider
print(productsInfo %>% group_by(`Provider/Company Name`) %>% 
        summarise(count = n()) %>%
        arrange(desc(count)), n = 10)
```


* There are 292 products in which top 5 provider or companies govt will be interested in dealing with this.

* Lets look product id which is like a key between enagagemmnet district files and the product files. To give consistent names we will change the name of the column to match it with district engagement file.

## Product's ID
``` {r,echo=FALSE,message=FALSE}
#LP Id
productsInfo <- productsInfo %>% rename(lp_id = 'LP ID')
```

#Engagement Data

* Let's do the merging of the files Let's do the engagemnt of the files

* To do this we will provide the extra column to our engagement district file to merge with dirstrict file.
``` {r,echo=FALSE,message=FALSE}
#district file
#adding column to the distric id 
d1000 <- d1000 %>% mutate(district_id = 1000)
d1039 <- d1039 %>% mutate(district_id = 1039)
d1044 <- d1044 %>% mutate(district_id = 1044)
d1052 <- d1052 %>% mutate(district_id = 1052)
d1131 <- d1131 %>% mutate(district_id = 1131)

print("Extra Column is added")
```


*We will use the inner join to take the common intersection of the districts dataset and the engagement files of district with the common key of district_id

``` {r,echo=FALSE,message=FALSE}
d1000 <- d1000 %>% inner_join(districtsInfo, by = c("district_id" = "district_id"))
d1039 <- d1039 %>% inner_join(districtsInfo, by = c("district_id" = "district_id"))
d1044 <- d1044 %>% inner_join(districtsInfo, by = c("district_id" = "district_id"))
d1052 <- d1052 %>% inner_join(districtsInfo, by = c("district_id" = "district_id"))
d1131 <- d1131 %>% inner_join(districtsInfo, by = c("district_id" = "district_id"))
```


* We will use the inner join to take the common intersection of the dataset and the engagement files of district by the common key of lp_id

``` {r,echo=FALSE,message=FALSE}
d1000 <- d1000%>% inner_join(productsInfo, by = c("lp_id" = "lp_id"))
d1039 <- d1039%>% inner_join(productsInfo, by = c("lp_id" = "lp_id"))
d1044 <- d1044%>% inner_join(productsInfo, by = c("lp_id" = "lp_id"))
d1052 <- d1052%>% inner_join(productsInfo, by = c("lp_id" = "lp_id"))
d1131 <- d1131%>% inner_join(productsInfo, by = c("lp_id" = "lp_id"))
```



## Engage Data Date 
* let's look into the date format of the files and make this consistent throught out the dataset.
*we will look into each data files date column 
``` {r,echo=FALSE,message=FALSE, results="hide"} 
unique(d1000$time) #this tells the format is ddmmyyyy
unique(d1039$time) #this tells the format is mmddyyyy
unique(d1044$time) #this tells the format is ddmmyyyy
unique(d1052$time) #this tells the format is ddmmyyyy
unique(d1131$time) #this tells the format is ddmmyyyy

d1000$time <- as.Date(d1000$time, format = "%d/%m/%Y")
d1039$time <- as.Date(d1039$time, format = "%m/%d/%Y")
d1044$time <- as.Date(d1044$time, format = "%d/%m/%Y")
d1052$time <- as.Date(d1052$time, format = "%d/%m/%Y")
d1131$time <- as.Date(d1131$time, format = "%d/%m/%Y")
```
##Merging the Data



*Now, we will combine all the engagement files and merge all the rows to the one engageDataset  

``` {r,echo=FALSE,message=FALSE} 

engageData <- bind_rows(d1000,d1039,d1044,d1052,d1131)
```


*Now let's do the data processing and add Year and month from the time column for better analysis
``` {r,echo=FALSE,message=FALSE}
engageData <- engageData %>% mutate(Year = year(time)) %>% 
                             mutate(Day = wday(time, label = T)) %>%
                             mutate(Month = month(time, label = T)) 
```



* We are dealing with 2020 year only therefore we need dataset of only 2020
``` {r,echo=FALSE,message=FALSE}

print(engageData %>% group_by(Year) %>% 
        summarise(count = n()) )
```


* Now, as we only need 2020 we filter out the year 2020 

``` {r,echo=FALSE,message=FALSE}

#as we are dealing with 2020
engageData <- engageData %>% filter(Year == 2020)

print(engageData %>% group_by(Year) %>% 
        summarise(count = n()) )
``` 

 
## Engagement Pct_Access
* Let's move to the next column pct_access  #Percentage of students in the district have at least one page-load event of a given product and on a given day
``` {r,echo=FALSE,message=FALSE}

summary(engageData$pct_access)

```


* As we can see they are denoted as character which is incorrect we need to change the datatype of this, replace na with the median values 
``` {r,echo=FALSE,message=FALSE}
 #engagedatapct access
 typeof(engageData$pct_access)

engageData$pct_access <- as.numeric(engageData$pct_access)

print(summary(engageData$pct_access))
#there are 2 NA's which can be replaced with NA
vec <- which(is.na(engageData$pct_access))
engageData$pct_access[vec] <- median(engageData$pct_access,na.rm = T)


print(summary(engageData$pct_access))
 #Percentage of students in the district have at least one page-load event of a given product and on a given day
```


## Engaagement Index
* Let's take the engagement index as the next column to clean; similarly to the pct_access, we will convert it into a numeric and then put the na's to the median.
* engagement_index Total page-load events per one thousand students of a given product and on a given day

``` {r,echo=FALSE,message=FALSE}
 #engagement index

#enagement index 
typeof(engageData$engagement_index)
summary(engageData$engagement_index)
engageData$engagement_index <- as.numeric(engageData$engagement_index)

vec <- which(is.na(engageData$engagement_index))
engageData$engagement_index <- as.numeric(engageData$engagement_index)
engageData$engagement_index[vec] <- median(engageData$engagement_index,na.rm = T)
summary(engageData$engagement_index
        )

```



**We can see there is a huge difference between the 3rd quartile and the maximum in the Pct_Access, so now we can consider them as the outliers. But since this is the page load, there is a chance that some pages can get unrealistic higher page loads in comparison to very less used pages. Therefore, despitethe fact that the column has uneven variability in the engagement and the page load access, we will keep this uneven variability in the dataset. Let's check up on the NA's in the Engage dataset.*
``` {r,echo=FALSE,message=FALSE}
colSums(is.na(engageData))
#district_ID
engageData$district_id <- as.character(engageData$district_id)

```



**Engage Dataset is now cleaned and let's look for the anlaysis through visualisation**
# Visualisation

Now, we will plot some graphs and do quick analysis of them .
``` {r,echo=FALSE,message=FALSE}
library(viridis)
library(hrbrthemes)
engageData %>%
  ggplot( aes(x= state, y= pct_access)) +
  geom_boxplot(color="Orange") +
  scale_fill_viridis(discrete = TRUE, alpha=0.9) +
  geom_jitter(color="blue", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(hjust = 0.5, size = 12, face = "bold"),
     )+ 
  labs(title="A boxplot of 1 page load per day in the State",
       x = "State",
       y = "%age of Students with 1 page load per day")
  
```




**As per the above graph, we can easily say Missouri State has the highest percentage of students with 1 page load per day in the state. Therefore, the Missouri government has performed well in this state in terms of getting the page load in their district. Now, they should share their strategy with 1039, 1131 districts regarding the upliftment of page loads. 1 page load shows the good connectivity available in this state of Missouri and Illinois.**
  
``` {r,echo=FALSE,message=FALSE}
engageData %>%
  ggplot( aes(x=  state, y= engagement_index)) +
  geom_boxplot(color="Orange") +
  scale_fill_viridis(discrete = TRUE, alpha=0.9) +
  geom_jitter(color="blue", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(hjust = 0.5, size = 10, face = "bold"),
     )+ 
  labs(title="  A boxplot of engagement index",
       x = "State",
       y = "%age of page-load events per thousand")
  
```




**Similarly, as per the above graph, we can easily say Missouri State has the highest percentage of students with 1 page load per day in the state. Therefore, the Missouri government has performed well in this state in terms of getting the page load in their district. Now, they should share their strategy with 1039, 1131 districts regarding the upliftment of page loads. The engagement index also suggests the products used in Missouri were great, and other states can follow the same products for better results.**

``` {r,echo=FALSE,message=FALSE}
engageData %>% filter(state == "Missouri") %>% 
  select(`Product Name`) %>%  group_by(`Product Name`) %>% 
  summarise(count = n()) %>% arrange(desc(count))

```



**Therefore Missouri used ABCya!, Big Ideas Math product a lot therefore other states should learn from their products and try to implemnet these**
``` {r,echo=FALSE,message=FALSE}
engageData %>%
  ggplot( aes(x= state, y= Expenseperpupil)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="red", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(hjust = 0.5, size = 12, face = "bold"),
  ) +
  ggtitle("A boxplot with of Expense Per Pupil") + 
  labs(title="  A boxplot of Expense per pupil in each state",
       x = "State",
       y = "Expense Per Pupil")
  
```




**Per-pupil total expenditure (sum of local and federal expenditure) from Edunomics Lab’s National Education Resource Database on Schools (NERD$) project. The expenditure data are school-by-school, and we use the median value to represent the expenditure of a given school district. As we can see, the Illinois government . in the respective district has done a lot of the expenditure. Further, they have gotten the results, as we can see that Illinois State has gotten a good amount of engagement as well. Therefore, if other states have the budget with them, they can consult Illinois for a better strategy on how to spend the money to get better benefits.**
``` {r,echo=FALSE,message=FALSE}
districtsInfo %>%
  filter(!is.na(state)) %>%
  group_by(locale) %>%
  summarise(pct=round(n()/nrow(.)*100, 0)) %>%
  arrange(desc(pct)) %>%
  ggplot(aes(x=2, y = pct, fill = locale))+
  geom_bar(stat = "identity")+
  coord_polar(theta = "y", start = 0)+ xlim(0.6, 2.5)+
  geom_label(aes(label = paste0(pct,"%",locale)), size=6, show.legend = F,
             label.padding = unit(0.15, "lines"),
             position = position_stack(vjust = 0.5))+
  theme_void()+ 
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size=16)
  )+
  labs(title="Districts Locale (%)", x="", y="%", fill="Locale")
```



**From the above graph, we can see the demographics of the entire USA: 59% of the districts are suburbs. Therefore, government planning for future digital empowerment should include suburbs strategists as well. When they are planning to take some bigger change or subsidy, they should open the gates for suburbs first.  18% rural is the second highest. Therefore, each small or big plan towards Didgita should include representatives of suburbs and locales.**
``` {r,echo=FALSE,message=FALSE}
productsInfo %>% 
  filter(!is.na(Primary)) %>%
  group_by(Primary) %>% 
  summarise(pct=round(n()/nrow(.)*100, 1)) %>% 
  arrange(desc(pct)) %>%
  ggplot(aes(x=2, y = pct, fill = Primary))+
  geom_bar(stat = "identity")+
  geom_label(aes(label = paste0(pct,"%")), size=6, show.legend = F, 
             label.padding = unit(0.15, "lines"),
             position = position_stack(vjust = 0.5))+
  theme_void()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    text = element_text(size=12)
  )+
  labs(title=" Primary Function of the products in (%)", x="", y="%", fill="Primary Function")
```



**The basic function of the product. There are two layers of labels here. Products are first labeled as one of these three categories: LC = learning and curriculum, CM = classroom management, and SDO = school and district operations.  As we can see, the majority of products were focused on LC, which is learning and curriculum. LC Products should be given more importance while designing the future strategy of digital engagement, as in the past this function was very popular in the market.**
``` {r,echo=FALSE,message=FALSE}

productsInfo %>% 
  filter(!is.na(Sector)) %>%
  group_by(Sector) %>% 
  summarise(pct=round(n()/nrow(.)*100, 1)) %>% 
  arrange(desc(pct)) %>%
  ggplot(aes(x=2, y = pct, fill = Sector))+
  geom_bar(stat = "identity")+
  coord_polar(theta = "y", start = 0)+ xlim(0.6, 2.5)+
  geom_label(aes(label = paste0(pct,"%")), size=6, show.legend = F, 
             label.padding = unit(0.15, "lines"),
             position = position_stack(vjust = 0.5))+
  theme_dark()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    text = element_text(size=12)
  )+
  labs(title="Products used in different Secotor (%)", x="", y="%")
```




**The chart's 'PreK-12' sector is very visible, indicating that the majority of items are designed with the kindergarten through twelfth grade (K–12) educational sector in mind. Therefore, we can say mostly students were using these products, as schools were a good part of the dataset. Therefore, those products were a part of them in a greater sense.**


``` {r,echo=FALSE,message=FALSE}


districtsInfo %>%
  group_by(state) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(5) %>%
  ggplot(aes( x = reorder(state,count,desc = T) , y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_label(aes(label = paste0(count)), size=6, show.legend = F, 
             label.padding = unit(0.15, "lines"),
             position = position_stack(vjust = 0.5))+
  labs(x = "State", y = "Count of Districts in the states") +
  theme_minimal() +
  coord_flip() +
  theme(text = element_text(size=20))+
  ylim(0,40)
```

``` {r,echo=FALSE,message=FALSE}  
productsInfo %>%
  group_by(`Provider/Company Name`) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(5) %>%
  ggplot(aes( x = reorder(`Provider/Company Name`,count,desc = T) , y = count)) +
  geom_bar(stat = "identity", fill = "Brown") +
  geom_label(aes(label = paste0(count)), size=6, show.legend = F, 
             label.padding = unit(0.15, "lines"),
             position = position_stack(vjust = 0.5))+
  labs(x = "Providers of the Products", y = "Count of the Providers/Companies of the Product") +
  theme_minimal() +
  coord_flip() +
  theme(text = element_text(size=12))+
  ylim(0,40)
```



**This shows the top companies involved in the digital engagement. Google LLC has launched various product. Government should consult Google for future upcoming strategy, as in the past they are solving the problem with their products**
```` {r,echo=FALSE,message=FALSE}
library(corrplot)
vec <- engageData %>% select(pct_access,engagement_index,minority_range,free_range,Expenseperpupil)
correlation_matrix <- cor(vec)

# Plot the correlation matrix`
corrplot(correlation_matrix, method = "circle",addCoef.col = "black")
```



**The above is the correlation plot between all the numeric variables. We can see the strong relationship between engagement index and Pct_access, which is 0.77. It indicates that if there's a higher one-page load on any of the products, then there's a high chance of engagement as well. Therefore, if a district gets a good product, good connectivity, and a good strategy, they can excel in digital engagement. There's also the chance of a correlation between free range and minority range, which means areas with a higher number of minorities were given a higher percentage of free or reduced services as well.**
```{r,echo=FALSE,message=FALSE}

engageData %>% select(engagement_index,state,Month) %>% group_by(Month,state) %>% 
  summarise(avg = mean(engagement_index), .groups = "drop") %>% arrange(Month) %>%
  ggplot(aes( x = Month , y = avg)) +
  geom_point(color = "blue")+
  geom_line()+
  facet_wrap(~state,ncol = 1)+
  labs(x = "State", y = "Engagement access to page load per thousand")
```


**The above graph clearly indicates the trend is similar in all the districts, which means that in the month of June or July, the average decreases. But among all the states, we can say Connecticut has the lowest index throughout 2020**

``` {r,echo=FALSE,message=FALSE}
engageData %>% select(engagement_index,district_id,Month) %>% group_by(Month) %>% 
  summarise(avg = mean(engagement_index))  %>% 
  ggplot( aes(x = Month, y = avg)) +
  geom_bar(stat = "identity", color = "Blue", fill = "Violet") +
  theme_minimal()# Color for bars
  labs(title = "Average Engagement by Month",  # Plot title
       x = "Month",  # X-axis label
       y = "Average Engagement Index")  # Y-axis label

```


**Now, as per the above graph, we can say the maximum engagement is in March and September. It can be because of the school students having exams during these months.Similarly, for the months of June and July, the engagement is very low. It's because of the holidays. Since the maximum product category is learning curriculum, we can conclude this is a summer holiday. Students don't have exams; therefore, they don't use products during their vacation.**

``` {r,echo=FALSE,message=FALSE}
table(engageData$state, engageData$Expenseperpupil)
``` 


**By this we can say Illinois state has done the highest expenditure in it's district as compared to other's state**


``` {r,echo=FALSE,message=FALSE}
table(engageData$district_id, engageData$state)
```


**Every District ID has gone the one state.Therefore, There is no discrepancies**
``` {r,echo=FALSE,message=FALSE}
table(engageData$state, engageData$minority_range)
```


**70% of Connecticut has the highest minority population. From the correlation graph, we can say they were offered a free lunch, but the school engagement index was very low in this area. Therefore, Connecticut State should improve their strategy and look for better engagement, either by products or from Missouri**
``` {r,echo=FALSE,message=FALSE}
table(engageData$state,engageData$free_range)
```


**Missouri was given less of the free lunch in comparison to the other districts**
``` {r,echo=FALSE,message=FALSE}
table(engageData$district_id,engageData$Ratio)
```


**All the districts had the same connectivity ratio (residential fixed high-speed connections over 200 kbps in at least one direction/household**

``` {r,echo=FALSE,message=FALSE}
table(engageData$district_id,engageData$locale)
```


**ALL the enaged data had the Suburban region . So, no other classification or analysis can be done in this.**
``` {r,echo=FALSE,message=FALSE}
library(gganimate)

# Make a ggplot, but add frame=year: one image per year
ggplot(engageData, aes(pct_access, engagement_index, colour = Sector)) +
  geom_point(alpha = 0.7, show.legend = TRUE) +
  scale_size(range = c(2, 12)) +
  # Here comes the gganimate specific bits
  labs(title = 'Enagement Access and Pct Access across the Sector', x = 'Enagement Access', y = 'Pct Access') +
  theme_bw()
```



**We can conclude this higher the Pct_Access, The engagement index will be higher. Towards the higher use of the page loads or engagement index, we can see more of the engagement was from the corporate, but the data had more of the percentage of the data from prek–12. Therefore, we can say that despite the lower number of corporate employees, we can assume corporate people have the products on which they have spent a lot of time in comparison to prek-12.**

``` {r,echo=FALSE,message=FALSE}
ggplot(engageData, aes(pct_access, engagement_index, colour = Primary)) +
  geom_point(alpha = 0.7, show.legend = TRUE) +
  scale_size(range = c(2, 12)) +
  # Here comes the gganimate specific bits
  labs(title = 'Pct_access and Engagement on Function of the products', x = 'Pct_Access', y = 'Engagement Access') +
  theme_bw() 
```
  


**From the above graph we can say that Learning And Curricullum as grows rapidly as soon as it takes the page load of 40% people in the district it goes viral. Therefore, Government should invest on LC type product, as they are going viral**


``` {r,echo=FALSE,message=FALSE}
engageData %>% select(state,engagement_index) %>% group_by(state) %>% 
  summarise(mean = mean(engagement_index)) %>% 
  ggplot( aes(x = state, y = mean)) +
  theme_minimal()+
  geom_bar(stat = "identity", color = "Blue",  fill = "Blue") + # Color for bars
  labs(title = "Average Engagement by State",  # Plot title
       x = "State",  # X-axis label
       y = "Average Engagement Index")  # Y-axis label

```


**Therefore we can say that 1039 and Illinois state has the maximum engagement, whereas 1131 has the lowest state**

``` {r,echo=FALSE,message=FALSE}
ggplot(engageData, aes(x= time, y= engagement_index)) +
    geom_line( color="blue") +
    geom_point(shape= 1, color="green", size=1) +
    theme_minimal() +
    ggtitle("Engagement_Access throughout the year")
```


** Above Graph shows the engagement index throughout the year. It's the continous chart the explanation of month is already provided in the above ** 

** Recommendation are given below each of the graph as per the analysis **
