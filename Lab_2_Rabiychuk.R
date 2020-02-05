library(corrplot)
library(ggplot2)
library(qgraph)
data_frame <- read.csv(file="c:/movies.csv", header=TRUE, sep=",")
View(data_frame)
data_numeric <- data_frame[-c(1,2,3)]
View(data_numeric)
#порахуємо ранговий коеф. кореляції Спірмана для числових данних?датасету
corrtest<-corr.test(data_numeric, method = "spearman")
corrtest$p #рівень значущості; неформально вказує на ймовірність отримати 
#такі результати на вибірці такого розміру й розподілу за умови істинності нульової
#гіпотези; тобто чим він менше ти?? більша ймовірність хибності нульової гіпотези
#тут нульова гіпотеза, що величини некорельовані
#при р<0.05 відхиляюємо нульову гіпотезу і можемо вважати, що величини корельовані
corrtest$r #власне пораховані коефіцієнти кореляції Спірмана
flattenCorrMatri? <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
flattenCorrMatrix(corrtest$r,corrtest$p) #прост?? вивели все в одну матрицю, для наочності
corrplot(corrtest$r)  #будуємо корелограму
#бачимо, що однією з найсильніших кореляцій є між відгуками та голосами (р=0)
#зобразимо їх на графіку
ggplot(data_numeric, aes(x=votes, y=reviews))+
  geom_point(size=1)
?зобразимо також взаємозв'язок між голосами та прибутком фільму (p=0)
ggplot(data_numeric, aes(x=votes, y=gross))+
  geom_point(size=1)
#зобразимо кореляційну мережу
qgraph(corrtest$r)
