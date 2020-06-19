#判别分析程序
setwd("/Users/auzzer_pang")
#假定数据存储目录为“E:/laimsa/”
#对数据表examp5.2.3进行基于两组协差阵相等的贝叶斯判别
examp5.2.3<-read.table("examp5.2.3.csv", header=T, sep=",") #读取文本文件
names(examp5.2.3) <- c("x1","x2","x3","x4","g")
examp5.2.3
library(MASS) #加载MASS包
ld1<-lda(g~x1+x2+x3+x4, prior=c(0.5, 0.5), examp5.2.3) #先验概率相等的线性判别，先验概率缺省时按与各组样本容量大小成比例的概率
ld1
Z<-predict(ld1) #根据线性判别函数预测所属类别
Z$posterior #后验概率结果
newg<-Z$class #预测的所属类别结果
cbind(g=examp5.2.3$g, round(Z$posterior, 3), newg) #按列合并的结果
table(g=examp5.2.3$g, newg) #判别情况表
ld2<-lda(g~x1+x2+x3+x4, prior=c(0.5, 0.5), CV=T, examp5.2.3) #选项“CV=T”表示采用交叉验证法
newg<-ld2$class #预测的所属类别结果
cbind(g=examp5.2.3$g, round(ld2$posterior, 3), newg) #按列合并的结果
table(g=examp5.2.3$g, newg) #判别情况表
ld3<-lda(g~x1+x2+x3+x4, prior=c(0.1, 0.9), examp5.2.3) #先验概率不相等的线性判别
ld3
Z<-predict(ld3)
newg<-Z$class #预测的所属类别结果
cbind(g=examp5.2.3$g, round(Z$posterior, 3), newg) #按列合并的结果
table(g=examp5.2.3$g, newg) #判别情况表

examp5.3.2<-read.table("examp5.3.2.csv", header=T, sep=",") #读取文本文件
names(examp5.3.2)<-c("x1","x2","x3","x4")
examp5.3.2
newZ<-predict(ld3, examp5.3.2) #预测新样品所属类别
newZ

#对数据表examp5.2.3进行基于两组协差阵不等的贝叶斯判别
examp5.2.3<-read.table("examp5.2.3.csv", header=T, sep=",") #读取文本文件
names(examp5.2.3)<-c("x1","x2","x3","x4","g")
library(MASS) #加载MASS包
qd1<-qda(g~x1+x2+x3+x4, prior=c(0.5, 0.5), examp5.2.3) #二次判别
qd1
Z<-predict(qd1) #根据二次判别函数预测所属类别
newg<-Z$class #预测的所属类别结果
cbind(g=examp5.2.3$g, round(Z$posterior, 3), newg) #显示合并结果
table(g=examp5.2.3$g, newg) #判别情况表
qd2<-qda(g~x1+x2+x3+x4, prior=c(0.5, 0.5), CV=T, examp5.2.3) #使用交叉验证法
newg<- qd2$class #预测的所属类别结果
cbind(g=examp5.2.3$g, round(qd2$posterior, 3), newg) #显示合并结果
table(g=examp5.2.3$g, newg) #判别情况表




#Fisher线性判别
library(MASS);
Z=read.csv("iris.csv");
fit=lda(factor(Z[,6])~Z[,2]+Z[,3]+Z[,4]+Z[,5]);#Fisher线性判别
fit; #输出包括先验概率, 各类的均值/重心, 线性判别函数(原始变量无截距),特征值比例

B=predict(fit,Z)$class #预测的类别
table(Z[,6],B) # cross-table


#下面做距离判别的二次判别分析
fit <- qda(factor(Z[,6])~Z[,2]+Z[,3]+Z[,4]+Z[,5], prior=c(1,1,1)/3)
fit; #仅输出先验概率和各类的均值/重心

B=predict(fit,Z)$class #预测的类别
tab<-table(Z[,6],B) # cross-table
tab
sum(diag(prop.table(tab)))  #计算回判正确率

