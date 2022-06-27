library(readxl)
library(car)
library(BSDA)
library(dplyr)

data.q4 <- read.csv("/Users/tigershao/Desktop/Duke/MQE/question_4.csv")

mean.trt.change <- mean(data.q4$Change[1:85])
mean.pla.change <- mean(data.q4$Change[86:170])

sd.trt <- sd(data.q4$Change[1:85])
sd.pla <- sd(data.q4$Change[86:170])


z.test(data.q4$Change[1:85], data.q4$Change[86:170])


data.q3 <- read_excel(path = file.path("/Users/tigershao/Desktop/Duke/MQE", "DrivingdataAll.xls"))
data.q3.mile.car1 <- data.q3$`Odom Reading 1 (Update)` - data.q3$`Odom Reading 1 (Previous)` 

car2 <- data.q3$`Odom Reading 2 (Update)` - data.q3$`Odom Reading 2 (Previous)`
car3 <- data.q3$`Odom Reading 3 (Update)` - data.q3$`Odom Reading 3 (Previous)`
car4 <- data.q3$`Odom Reading 4 (Update)` - data.q3$`Odom Reading 4 (Previous)`
qqPlot(car2)
qqPlot(car3)
qqPlot(car4)
hist(car2)
hist(car3)
hist(car4)


fit.q3 <- lm(data.q3.mile.car1 ~ as.factor(data.q3$`OMR Version`) + data.q3$`Odom Reading 1 (Previous)`)
summary(fit.q3)
summary(aov(fit.q3))






qqPlot(data.q3.mile.car1)
hist(data.q3.mile.car1)

trt <- data.q4$Change[1:85]
pla <- data.q4$Change[86:170]

t.test(trt, pla, sigma.x = sd.trt, sigma.y = sd.pla, alternative = "greater")

############ Question 4
trt_arm <- data.q4[1:85, ]
pla_arm <- data.q4[86:170, ]


ancova.q3.reg <- lm(data.q4$Followup ~ data.q4$AssignedTreatment + data.q4$Baseline)
ancova_fit_1 <- aov(ancova.q3.reg)
summary(ancova_fit_1)

fit.q4 <- lm(data.q4$Change ~ data.q4$AssignedTreatment)
summary(fit.q4)
summary(aov(fit.q4))

ancova.q5 <- aov(lm(data.q4$Change ~ data.q4$AssignedTreatment + data.q4$Baseline))
summary(ancova.q5)


