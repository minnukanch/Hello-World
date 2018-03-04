#Loading the packages using library function
library(ggplot2)
library(magrittr)
library(dplyr)
install.packages("directlabels", repo="http://r-forge.r-project.org") 
library(directlabels)
#Loading the mpg(miles per gallon) dataset from ggplot2 package
ggplot2::mpg
View(mpg)
#Summary statistics of the graph
summary(mpg)

#First Plot with
#highway miles per gallon versus engine displacement in litres
#a)
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv), method = "loess", se = FALSE,span = 1) +
  geom_text(mapping = aes(x = displ, y = hwy, label = model, fontface = "italic", family = "sans"),
            check_overlap = TRUE) + xlim(2,8) + xlab("engine displacement in litres")+
  ylab("highway miles per gallon") + theme(plot.title = element_text(size = 15)) +
  ggtitle("First Plot")
#Second Plot
#b)
#plot using the bar graph(alternative to the above graph but grouping with ID's)
#to fill a contionous variable, group fuction is needed such that 
#bars will be stacked according to the grouping variable
new_data2 <- mpg %>% arrange(hwy) %>% mutate(id = seq_along(hwy))
View(new_data2)
ggplot(data = new_data2) +
  geom_bar(mapping = aes(drv, fill = hwy, group = id)) + theme(plot.title = element_text(size = 15)) +
  ggtitle("Second Plot")

#Third Plot 
#with city miles per gallon versus engine displacement in litres
#and also plotting with drv versus cylinder using facet_grid
#In Us the mileage is calculated using distance travelled with fixed amount of fuel
#a)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = cty), color = "blue")+
  facet_grid(drv~cyl) + xlim(2,7.8) + ylim(1,37) + xlab("engine displacement in litres")+
  ylab("city miles per gallon") + theme(plot.title = element_text(size = 15)) +
  ggtitle("Third Plot")

#Fourth Plot
#Comparing the above plot with European countries whose ecomony is calculated based on fuel
#consumed over fixed distance(100km)
#b)
european_mileage <- function(cty){
  gallon_in_litre <- 3.785
  mile_in_kilometer <- 1.609
  
  conversion_mileage <- (100 * gallon_in_litre)/(cty * mile_in_kilometer)
  conversion_mileage
}
#Mutate the mpg dataset into transmute function to create a new dataset
new_data <- mpg %>% transmute(european_mileage(cty))
View(new_data)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = european_mileage(cty)) , color = "violet")+
  facet_grid(drv~cyl) + xlim(2,7.8) + ylim(1,37) + xlab("engine displacement in litres")+
  ylab("city mileage in european standard(km)") + theme(plot.title = element_text(size = 15)) +
  ggtitle("Fourth Plot")

#Fifth Plot
#Plot with city miles per gallon versus engine displacement in litres
#and also plotting with drv versus tranmission using facet_grid
#c) 
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = cty), color = "blue")+
facet_grid(drv~trans) + xlim(2,7.8) + ylim(1,37) + xlab("engine displacement in litres")+
  ylab("city miles per gallon") + theme(plot.title = element_text(size = 15)) +
  ggtitle("Fifth Plot")


#Sixth Plot using the histogram with city mileage variable to find out the distribution
ggplot(data = mpg) + geom_histogram(mapping = aes(x = cty, fill = drv), binwidth = 2) + 
  facet_wrap(~class) + xlim(10,40) + xlab("city miles per gallon") + ylab("no of counts")+
   theme(plot.title = element_text(size = 15)) +
  ggtitle("Sixth Plot")

#Seventh Plot
#To compare the above graph on highway mileage with the city mileage
ggplot(data = mpg) +
  geom_bar(aes(class, fill = cty, group = cty)) +  theme(plot.title = element_text(size = 15)) +
  ggtitle("Seventh Plot")

#Eighth Plot using the direct labels package from Cran-Project 
#to develop the smart grid which is better for visualization
ggplot(data = mpg,aes(x = displ, y = hwy, color = class)) +
  geom_point(show.legend = FALSE) +
  directlabels::geom_dl(aes(label = class), method = "smart.grid") + theme(plot.title = element_text(size = 15)) +
  ggtitle("Eighth Plot")

  ############################The End#########################################

