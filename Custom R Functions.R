convert_Fo_to_Co <- function(temp) {
  Co <- ((temp - 32) * (5 / 9))
  return(Co)
}
# freezing point of water
convert_Fo_to_Co(32)

# boiling point of water
convert_Fo_to_Co(212)

# level of a human fever
convert_Fo_to_Co(100)

# coldest month in Maryland - January: an average
convert_Fo_to_Co(54.6)


#Second Custom Function: Sum of 2 squares
#sum(df$x^2 + df$y^2){print(sum())}

sum_of_squares <- function(num1,num2) {
  return(sum(num1^2 + num2^2) )
}
# test1 
sum_of_squares(1,2)

#test2
sum_of_squares(2,4)

#test3
sum_of_squares(3,5)

#test4
sum_of_squares(5,8)

#Third Custom Function - Univariate data set - Summary of Statistics 
#create variable with 20 values
x <- c(1, 1, 2, 3.5, 4, 4, 4, 5, 5, 6.5, 7, 7.4, 8, 13, 14.2, 16, 17, 19, 21, 25)

summary_of_stats <- function(dataset) {
  minval <- min(dataset)
  meanval <- mean(dataset)
  maxval <- max(dataset)
  deviation <- sd(dataset)
  answer <- c(minval, meanval, maxval, deviation)
  return(answer)
}
summary_of_stats(x)

#Test1
y <- c(1, 2, 3, 4, 5, 5, 6, 7, 8, 9, 10)
summary_of_stats(y)

#Test2
a <- c(100, 200, 300, 400, 500, 600, 700, 800, 900)
summary_of_stats(a)

#Test3
b <- c(500, 1000, 2000, 3000, 7000, 10)
summary_of_stats(b)

#Test4
m <- c(25, 50, 75, 100, 125, 150, 175, 200)
summary_of_stats(m)

#Fourth Custom Function - Univariate dataset with boxplot of the raw dataset & 


x <- c(1, 1, 2, 3.5, 4, 4, 4, 5, 5, 6.5, 7, 7.4, 8, 13, 14.2, 16, 17, 19, 21, 25)

custom_boxplot <- function(x){
  return (boxplot(x))
}

custom_boxplot(x)

# histogram of the square root transformed dataset

x <- c(1, 1, 2, 3.5, 4, 4, 4, 5, 5, 6.5, 7, 7.4, 8, 13, 14.2, 16, 17, 19, 21, 25)

custom_hist <- function(x){
  return (hist(x))
}

custom_hist(x)

#Fifth custom function - Convert Celcius to Kelvin

convert_Co_to_K <- function(temp) {
  Co <- ((temp - 0) + 273.15)
  return(Co)
}
#5 Co is equal to what in Kelvin
convert_Co_to_K(5)

#Freezing point in Kelvin
convert_Co_to_K(0)

#Human body average temperature 
convert_Co_to_K(37)

#Canine body temperature average
convert_Co_to_K(39.2)

#Average temperature of the ocean's surface 
convert_Co_to_K(17)