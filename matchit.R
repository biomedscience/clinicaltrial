# library
install.packages("hrbrthemes")
install.packages("MatchIt")
install.packages("lmtest")
install.packages("sandwich")

library("ggplot2")
library("tidyverse")
library("MatchIt")
library("viridis")
library("hrbrthemes")
library("lmtest") #coeftest
library("sandwich") #vcovCL

Gender=sample(c("Male","Female"),200,replace=TRUE)
Group=sample(c("Case","Control"),200,replace=TRUE)
Race=sample(c("Asian","European"),200,replace=TRUE)
Age=rnorm(200,40,5)
Sample<-paste0("S",1:200)
data<-data.frame(Sample,Gender,Race,Age,Group)
data
m <- matchit(Group ~ Age + Gender + Race, data = data, method = "nearest", distance = "glm")
plot(m, type = "jitter", interactive = FALSE)
plot(m, type = "qq", interactive = FALSE,which.xs = c("Age", "Gender", "Race"))
data <- match.data(m)
fit <- lm(Group ~ Age + Gender + Race, data = data,weights = weights)
summary(fit)
coeftest(fit, vcov. = vcovCL, cluster = ~Group)

# Stacked + percent
input<-data %>% select(Group,Gender,Race) %>% count(Gender,Race,Group, sort = TRUE)
input<-data %>% select(Group,Age)
ggplot(input, aes(fill=Gender, y=n,x=Group)) + geom_bar(position="stack", stat="identity") +theme_bw() 
ggplot(input, aes(fill=Race, y=n,x=Group)) + geom_bar(position="stack", stat="identity")
ggplot(data, mapping = aes(x = Age, fill = Group)) +geom_histogram(binwidth = 3)+theme_bw()
ggplot(data, mapping = aes(x = Age, color = Group)) +geom_freqpoly(binwidth = 3,size=2)+theme_bw()
