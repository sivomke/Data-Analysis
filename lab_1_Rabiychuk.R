install.packages("psych")
install.packages("FinCal")
library("FinCal")
library("psych")
?Orange
data_frame<-Orange
View(data_frame) #���� ��� �����
str(data_frame) #��������� ����� (��� � ����)'
#���� �� ���� ���� ��� 5-� �����; ����� ������ - ��������?�����, �������
#����� ���� ������������� �� ���������� ������������� �������
#���: 1-�� ������ - ������ 2-�� ����, 2-�� - 4-��, 3-� - 1-��, 4-�� - 5-��, 5-�� - 3-��
#�������������� ��������� ������
#����������� ���������
#�������� ���.��������� ���?������� �������� ����� ��� ������� ���
#�� ����� ������� �� ��������� ������ �������, ��� �� ��������� �����
#��� ������� ��� ������
mean_all<-aggregate(x=data_frame$circumference, by=list(data_frame$Tree),FUN=mean)
colnames(mean_all)<-c("Tree (factor l?vel)", "Mean")
mean_all
#������� ����������� (��� ���������� �������, �� � �� 1 �������); ���� ���� ����'��
#����� �� ����� ���������
geomean_all<-aggregate(x=data_frame$circumference, by=list(data_frame$Tree),geometric.mean)
colnames(geomean_all)<-c("?ree (factor level)", "Geometric mean")
geomean_all
#C������ ���������� (��� �.�. � ����������� ����������)
harm_mean_1004<-harmonic.mean(data_frame$circumference[data_frame$age==1004])
harm_mean_1004
#����, ��������� ������� ����
mode <- function(data_fr?me) {
  uniq <- unique(data_frame) #��������� ��������, �� ����������
  uniq[which.max(tabulate(match(data_frame,uniq)))] 
}
#��� match ������� ������ ������� ������� ��������� ��-�� x � ������� ������� �
#tabulate ���� ������� ������� ��������� ����??��, ��� ����������� � ���
#���� �������� ����
mode_<-mode(data_frame$circumference)
mode_
#�� �� ������� �������� ����� ���� ������  30
#������ (����������� ��-� ����������)
median_<-median(data_frame$circumference)
median_
#�������������� ����������?������� �����
#��������
variation_all<-aggregate(x=data_frame$circumference, by=list(data_frame$age),var)
colnames(variation_all)<-c("Age", "Variation")
variation_all
#���������� ���������
#������ ���������� ��������� ��� ������� ���� ������ � �������??�� �� ��������� ����
sd_all<-aggregate(x=data_frame$circumference,by=list(data_frame$age),sd)
colnames(sd_all)<-c("Age", "Standart deviation")
sd_all
#������, �� � ���������� ��� ������ ����� ����� ������ ����������,
#����� ���� ���� ����� ���� ���??�
#���������� ������� (������� ���.����.<>0)
#�� ���� �������� ��� ������� ���
#�� � ������ �� ��������������
#����������� ������ �������������(�������� �������, �������� �� �������)
#�������� ��� ��� 1582
coef_var_1582<-coefficient.variation(?d=sd_1582,avg=mean_1582)
coef_var_1582
#����� 19%, �� ���� ��� ������� �����������
#���������� ��������� (�������� ��������������� ������)
prob_dev <- function(data) {
  IQR(data)/2
}
prob_dev_all<-aggregate(x=data_frame$circumference,by=list(data_fram?$age),prob_dev)
colnames(prob_dev_all)<-c("Age", "IQR of circumference")
prob_dev_all
#������ ������
range<-range(data_frame$circumference)
range
#�� ��� ����� ����� ����� ���������� ������ �� ������� � ����
#��� ������ �������� �� �� �������, ��� �� ��?? ������� ������������������� ���������
range_all<-aggregate(x=data_frame$circumference,by=list(data_frame$age),range)
colnames(range_all)<-c("Age", "Range")
range_all
#������, �� ������ ��������� ������, ���� �� ��� ���������� ������
#�������� ��������??��� ��������
#���� ������� ��� ����������, �� ��� �������� ����� �� 99,7% ��� ������������
conc_interval<- function(data){
  left<-(mean(data)-3*sd(data))
  right<-(mean(data)+3*sd(data))
  return (paste(left,right, sep=" "))
}
conc_int<-aggregate(x=da?a_frame$circumference,by=list(data_frame$age),conc_interval)
colnames(conc_int)<-c("Age", "Interval")
conc_int
#�������
quantile(data_frame$circumference)
#� ������������� ������� describeBy, ���� ���� ������� ���������� �� ���������� �����
#�������� ��??���� ������ ��� ������� ���
describeBy(x=data_frame$circumference, group =data_frame$age)
#������������� �����
#ó��������
library(ggplot2)
ggplot(data_frame, aes(x=circumference))+
  geom_histogram(fill="blue",col="black",binwidth = 20, alpha=0.5)+
  l?bs(title="Hist for Circumference")
#���� � ������; ��� ������� ��� ������
boxplot(circumference~age, data=data_frame, main="Boxplot")
#��������-�������� ������
qqnorm(data_frame$circumference)
qqline(data_frame$circumference, col="red")
#����� ������� �� ??������ ������� �� �����������
#����������� ������
plot(data_frame$circumference,pnorm(data_frame$circumference, mean(data_frame$circumference),
                 sd(data_frame$circumference)) , main="P-P-plot")
#�������� �����������
#������� �������: ??��� ����� ���������� �������
#��� p>0.05 ������� ������� �� �����������
shapiro.test(data_frame$circumference)
#p=0.08, ����, ������� ��� ������������ �� �����������
