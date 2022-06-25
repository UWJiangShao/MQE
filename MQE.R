library(readxl)
library(car)

data.q4 <- read.csv("/Users/tigershao/Desktop/Duke/MQE/question_4.csv")

mean.trt.change <- mean(data.q4$Change[1:85])
mean.pla.change <- mean(data.q4$Change[86:170])

sd.trt <- sd(data.q4$Change[1:85])
sd.pla <- sd(data.q4$Change[86:170])


t.test(data.q4$Change[1:85], data.q4$Change[86:170])


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

qqPlot(data.q3.mile.car1)
hist(data.q3.mile.car1)



