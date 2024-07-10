library(ggplot2)
library(cowplot)
library(tidyr)
library(corrplot)
library(tidyverse)
library(dplyr)
data<-read.csv("C://Users//TechMadmin//Desktop//MLR2.csv")
dim(data)
summary(data)
d=head(data,n=10)
tail(data,n=10)
names(data)
plot1<-plot(y=data$FertilityRate,x=data$Year,ylab = " fertility rate",xlab = "Year",col="green",type="l",lwd="3")
plot1
p1 <- ggplot(data, aes(y= PerFemEmploy, x = FertilityRate)) +
  geom_point(color = "red")
p2 <- ggplot(data, aes(x = PerFemEmploy, y =Year)) +
  geom_point(color = "blue")
plot_grid(p1, p2, ncol = 2)
p1data<-data.frame(year=data$Year,agriculture=data$Agriculture,industry=data$Industry,service=data$Services)
p1data_long <- tidyr::gather(p1data, key = "Sectors", value = "Value", -year)
ggplot(p1data_long, aes(x = year, y = Value, fill = Sectors)) +
  geom_col(position = "dodge") +
  labs(title = "Sector Wise Analysis", x = "Year", y = "Value") +
  theme(legend.position = "top")
plot8<-plot(y=data$Wage.Salaried,x=data$Year,ylab = " Wage & Salaried",xlab = "Year",col="pink",type="o", lwd="3")
plot8
p1<-ggplot(data,aes(x=Year))
p1<-p1+geom_line(aes(y=Vulnerable,color="blue"))
p1<-p1+geom_point(aes(y=Vulnerable,color="blue"))
p1<-p1+geom_line(aes(y=Wage.Salaried,color="black"))
p1<-p1+geom_point(aes(y=Wage.Salaried,color="black"))
p1<-p1+theme_classic()
p1<-p1+scale_color_manual(name="variables",labels=c("wages","vulnerable"),values=c("green","black"))
p1
plot1<-plot(x=data$Ratio_MaletoFemale,y=data$Year,ylab = " Male to Female Ratio",xlab = "Year",col="gold",type="l",lwd="3")
plot1
model<-lm(Year~Ratio_MaletoFemale,data=data)
summary(model)
ggplot(data, aes(x = Ratio_MaletoFemale, y = Year)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Linear Regression Plot") +
  xlab("Ratio of Male to Females") +
  ylab("Year")
predicted_year <- predict(model, newdata = data.frame(Ratio_MaletoFemale = 50))
cat("The predicted year when the ratio of males to females will be equal to 1 is", round(predicted_year))     
ggplot(data, aes(x = Ratio_MaletoFemale, y = Year)) +
  geom_point() +
  labs(title = "Predicted Value when Female and Male employment is same", x = "Ratio of Males to Females", y = "Year") +
  theme_bw() +geom_point(aes(x = 50, y = predicted_year), color = "red", size = 3)

data <- data %>% as_tibble()
data <- select_if(data, is.numeric)
corr <- cor(data[, -1])
corrplot(corr, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

