
getwd()
list.files()

read.table(file,skip = 9,sep = ',')

filelist <- list.files(pattern = ".dat")
filelist

# combine files

fileCombine <- function(filelist){
  for (file in filelist){
      if(!exists("dataset")) {
        dataset <- read.table(file,skip = 9,sep = ',')
      }else{
        temp_dataset <- read.table(file,skip = 9,sep = ',')
        dataset <- rbind(dataset, temp_dataset)
        rm(temp_dataset)
      }
    }  
}

#different way to merge
newDataset <- do.call("rbind", lapply(filelist, 
                                      FUN = function(files)
                                        {read.table(files,skip=9,sep = ',')}))


library(plyr)
filelist <- list.files()
dataset_new <- ldply(filelist, read.table, skip = 9, sep=",")



#XML format read file

install.packages("XML")
library(XML)
getwd()
xmlfile <- xmlToDataFrame("iris.xml")

xmlfile


install.packages("jsonlite")

#conversion of data frame to JSON
library(jsonlite)

iris
x <- toJSON(iris)
x

library(dplyr)

#conversion of json to DF

y <- fromJSON(x) %>% as.data.frame

y

#using dplyr, we will do data manipulation on IRIS dataset

iris

head(iris, n=10)

#before filtering i did create a few plots to check the data,
names(iris)
library(ggplot2)

plot1 <- ggplot(iris, mapping = aes(x= Sepal.Length, y = Sepal.Width, color= Species)) +
  geom_point()

plot1 + geom_point(size= 2, alpha = 0.4)


#First i will be filtering it on virginica

virginica <- filter(iris, Species == "virginica")

head(virginica)

#then I did some more filtering

sepalLength <- filter(virginica, Sepal.Width > 3)

dim(sepalLength)


tail(sepalLength)


#Now using select 

selectIris <- select(iris, Sepal.Length, Sepal.Width, Petal.Width)

head(selectIris)

selectSetosa <- iris %>% 
  filter(Species == "setosa") %>% 
  select(Petal.Width, Petal.Length, Species) %>% 
  summarise(n = n())

selectSetosa

# mutate the values

newCol <- mutate(iris, bighalf = Petal.Width > 0.4 * Petal.Length)

head(newCol, n= 7)

sum(newCol$bighalf==TRUE) #how many condition satisfy the logic


#Now arranging based on sepal.length size, i used the descending

newCol <- arrange(newCol, desc(Sepal.Length))
head(newCol)


# we will also look at the piping function

data1 <- iris %>% 
  filter(Species == "versicolor") %>% 
  arrange(Sepal.Length)

data1


data2 <- iris %>% 
  mutate(mean.width = mean(Sepal.Width, na.rm = TRUE)) %>% 
  filter(Species == "versicolor") %>% 
  select(Sepal.Length, Sepal.Width, mean.width) %>% 
  arrange(desc(Sepal.Length))

data2


# we can also plot using the basic plot function

hist(iris$Sepal.Length)

plot(iris$Petal.Length, iris$Petal.Width)


plot(iris)
