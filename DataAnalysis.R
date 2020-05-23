library(jsonlite)
library(dplyr)
library(readr)

dataset1 <- read_csv("D:/jarsh/下載/104.csv")
dataset2 <- read_csv("D:/jarsh/下載/107年各教育程度別初任人員每人每月經常性薪資─按大職類分.csv")

dataset1$大職業別<-gsub("部門|、","",dataset1$大職業別)
dataset2$大職業別<-gsub("_","",dataset2$大職業別)
dataset2$大職業別<-gsub("建工程","造業",dataset2$大職業別)
dataset2$大職業別<-gsub("出版、影音製作、傳播及資通訊服務業","資訊及通訊傳播業",dataset2$大職業別)
dataset2$大職業別<-gsub("教育業","教育服務業",dataset2$大職業別)
dataset2$大職業別<-gsub("醫療保健業","醫療保健服務業",dataset2$大職業別)

for(i in 1:140){
  if (dataset2$大職業別[i] != dataset1$大職業別[i]){
    print(dataset1$大職業別[i])
    print(dataset2$大職業別[i])
  }
}

dataset2$`大學-薪資`<-as.numeric(gsub("—|…",NA,dataset2$`大學-薪資`))
dataset1$`大學-薪資`<-as.numeric(gsub("—|…",NA,dataset1$`大學-薪資`))

salary <- data.frame(profession=dataset1$大職業別)
salary$s<-dataset2$`大學-薪資`/dataset1$`大學-薪資`

salary<-salary[complete.cases(salary$s),]
salary$s<-sort(salary$s,decreasing = T)
head(salary,9)
filter(salary,s>1.05)

salary$profession<-as.character(salary$profession)

for(i in 1:117){
  salary$pp[i]<-strsplit(salary$profession,"-")[[i]][2]
}
names(table(as.character(salary$pp)))


a<-filter(salary,s>1.05)
table(as.character(a$pp))


dataset1$`大學-女/男`<-as.numeric(gsub("—|…",NA,dataset1$`大學-女/男`))
a<-dataset1[complete.cases(dataset1$`大學-女/男`),]
a$`大學-女/男`<-sort(a$`大學-女/男`,decreasing = T)
a<-select(a,大職業別,`大學-女/男`)
dataset2$`大學-女/男`<-as.numeric(gsub("—|…",NA,dataset2$`大學-女/男`))
b<-dataset2[complete.cases(dataset2$`大學-女/男`),]
b$`大學-女/男`<-sort(b$`大學-女/男`,decreasing = T)
b<-select(b,大職業別,`大學-女/男`)
a$`大學-女/男`<-as.character(a$`大學-女/男`)
b$`大學-女/男`<-as.character(b$`大學-女/男`)
tail(a,10)
tail(b,10)


head(a,10)
head(b,10)



dataset2$`研究所-薪資`<-as.numeric(gsub("—|…",NA,dataset2$`研究所-薪資`))
dataset2$`大學-薪資`<-as.numeric(gsub("—|…",NA,dataset2$`大學-薪資`))
difference <- data.frame(name = dataset2$大職業別)
difference$d<-dataset2$`研究所-薪資`/dataset2$`大學-薪資`
difference<-difference[complete.cases(difference$d),]
difference$d<- sort(difference$d,decreasing = T)

head(difference,10)[1]

differenceCC <- data.frame(name = dataset2$大職業別,
                           研究所 =dataset2$`研究所-薪資`,
                           大學 = dataset2$`大學-薪資`)
differenceCC<-differenceCC[complete.cases(differenceCC$研究所&differenceCC$大學),]
differenceCC$研究所<-sort(differenceCC$研究所,decreasing = T)
head(differenceCC[grep("工業及服務業-專業人員|資訊及通訊傳播業",differenceCC$name),],10)


difference[grep("工業及服務業-專業人員|資訊及通訊傳播業",difference$name),]


