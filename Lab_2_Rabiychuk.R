library(corrplot)
library(ggplot2)
library(qgraph)
data_frame <- read.csv(file="c:/movies.csv", header=TRUE, sep=",")
View(data_frame)
data_numeric <- data_frame[-c(1,2,3)]
View(data_numeric)
#�������� �������� ����. ��������� ������� ��� �������� ������?��������
corrtest<-corr.test(data_numeric, method = "spearman")
corrtest$p #����� ���������; ����������� ����� �� ���������� �������� 
#��� ���������� �� ������ ������ ������ � �������� �� ����� ��������� �������
#�������; ����� ��� �� ����� ��?? ����� ���������� ������� ������� �������
#��� ������� �������, �� �������� �������������
#��� �<0.05 ���������� ������� ������� � ������ �������, �� �������� �����������
corrtest$r #������ ���������� ����������� ��������� �������
flattenCorrMatri? <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
flattenCorrMatrix(corrtest$r,corrtest$p) #�����?? ������ ��� � ���� �������, ��� ��������
corrplot(corrtest$r)  #������ �����������
#������, �� ������ � ������������ ��������� � �� �������� �� �������� (�=0)
#��������� �� �� �������
ggplot(data_numeric, aes(x=votes, y=reviews))+
  geom_point(size=1)
?��������� ����� �������'���� �� �������� �� ��������� ������ (p=0)
ggplot(data_numeric, aes(x=votes, y=gross))+
  geom_point(size=1)
#��������� ����������� ������
qgraph(corrtest$r)
