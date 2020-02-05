install.packages("psych")
install.packages("FinCal")
library("FinCal")
library("psych")
?Orange
data_frame<-Orange
View(data_frame) #вивід усіх даних
str(data_frame) #структура даних (тип і вміст)'
#дані за різні роки для 5-х дерев; номер дерева - факторна?змінна, причому
#номер рівня впорядкований за зростанням максимального діаметру
#так: 1-ше дерево - фактор 2-го рівня, 2-ге - 4-го, 3-тє - 1-го, 4-те - 5-го, 5-те - 3-го
#Характеристики положення центра
#Математичне сподівання
#порахуємо мат.сподівання для?діаметру стовбуру дерев для кожного віку
#це можна зробити за допомогою однієї функції, щоб не дублювати рядки
#для кожного віку окремо
mean_all<-aggregate(x=data_frame$circumference, by=list(data_frame$Tree),FUN=mean)
colnames(mean_all)<-c("Tree (factor l?vel)", "Mean")
mean_all
#Середнє геометричне (для випадкових величин, які з йм 1 додатні); наші дані невід'ємі
#вказує на темпи зростання
geomean_all<-aggregate(x=data_frame$circumference, by=list(data_frame$Tree),geometric.mean)
colnames(geomean_all)<-c("?ree (factor level)", "Geometric mean")
geomean_all
#Cереднє гармонічне (для в.в. з позитивними значеннями)
harm_mean_1004<-harmonic.mean(data_frame$circumference[data_frame$age==1004])
harm_mean_1004
#Мода, вбудованої функції немає
mode <- function(data_fr?me) {
  uniq <- unique(data_frame) #прибираємо значення, що дублюються
  uniq[which.max(tabulate(match(data_frame,uniq)))] 
}
#тут match повертає вектор позицій першого входження ел-ів x у множині значень х
#tabulate рахує кількість повторів отриманих інде??сів, тоді максимальне з них
#буде індексом моди
mode_<-mode(data_frame$circumference)
mode_
#бо на початку більшість дерев мали діаметр  30
#Медіана (центральний ел-т статистики)
median_<-median(data_frame$circumference)
median_
#Характеристики розсіювання?значень змінної
#Дисперсія
variation_all<-aggregate(x=data_frame$circumference, by=list(data_frame$age),var)
colnames(variation_all)<-c("Age", "Variation")
variation_all
#Стандартне відхилення
#цікаво подивитись відхилення для кожного року окремо і подивит??сь чи змінюється воно
sd_all<-aggregate(x=data_frame$circumference,by=list(data_frame$age),sd)
colnames(sd_all)<-c("Age", "Standart deviation")
sd_all
#бачимо, що зі збільшенням віку діаметр дерев почав суттєво варіюватись,
#тобто вони мали різний темп рос??у
#Коефіцієнт варіації (оскільки мат.спод.<>0)
#має сенс рахувати для певного віку
#як і взагалі всі характеристики
#характеризує ступінь різноманітності(виділяють низький, середній та високий)
#порахуємо для віку 1582
coef_var_1582<-coefficient.variation(?d=sd_1582,avg=mean_1582)
coef_var_1582
#майже 19%, що каже про середню змінюваність
#Ймовірнісне відхилення (половина інтерквартильної широти)
prob_dev <- function(data) {
  IQR(data)/2
}
prob_dev_all<-aggregate(x=data_frame$circumference,by=list(data_fram?$age),prob_dev)
colnames(prob_dev_all)<-c("Age", "IQR of circumference")
prob_dev_all
#Розмах вибірки
range<-range(data_frame$circumference)
range
#на мою думку також варто подивитись розмах по кожному з років
#щоб наочно побачити що він зростав, про що на?? сказало середньоквадратичне відхилення
range_all<-aggregate(x=data_frame$circumference,by=list(data_frame$age),range)
colnames(range_all)<-c("Age", "Range")
range_all
#бачимо, що різниця достатньо суттєва, хоча це все апельсинові дерева
#Інтервал концентр??ції розподілу
#якби розподіл був нормальний, то цей інтервал містив би 99,7% усіх спостережень
conc_interval<- function(data){
  left<-(mean(data)-3*sd(data))
  right<-(mean(data)+3*sd(data))
  return (paste(left,right, sep=" "))
}
conc_int<-aggregate(x=da?a_frame$circumference,by=list(data_frame$age),conc_interval)
colnames(conc_int)<-c("Age", "Interval")
conc_int
#Квантилі
quantile(data_frame$circumference)
#з застосуванням функції describeBy, вона рахує основні статистики по групованим даним
#порахуємо ос??овні оцінки для кожного віку
describeBy(x=data_frame$circumference, group =data_frame$age)
#Розвідувальний аналіз
#Гістограма
library(ggplot2)
ggplot(data_frame, aes(x=circumference))+
  geom_histogram(fill="blue",col="black",binwidth = 20, alpha=0.5)+
  l?bs(title="Hist for Circumference")
#ящик з вусами; для кожного віку окремо
boxplot(circumference~age, data=data_frame, main="Boxplot")
#квантиль-квантиль графік
qqnorm(data_frame$circumference)
qqline(data_frame$circumference, col="red")
#можна сказати що ??озподіл подібний до нормального
#ймовірнісний графік
plot(data_frame$circumference,pnorm(data_frame$circumference, mean(data_frame$circumference),
                 sd(data_frame$circumference)) , main="P-P-plot")
#перевірка нормальності
#нульова гіпотеза: ??ані мають нормальний розподіл
#при p>0.05 нульова гіпотеза не відхиляється
shapiro.test(data_frame$circumference)
#p=0.08, отже, гіпотеза про нормальність не відхиляється
