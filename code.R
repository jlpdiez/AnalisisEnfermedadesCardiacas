#Descripción del Dataset
hdds <- read.csv("processed.cleveland.data")
colnames(hdds) <- c("age", "sex", "cp", "trestbps",  "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "target")
str(hdds)
head(hdds)
summary(hdds)

#Limpieza de los Datos
colSums(is.na(hdds))
colSums(hdds == "?")
hdds$ca[hdds$ca == "?"] = factor("0.0")
hdds$ca <- droplevels(hdds$ca)
install.packages("VIM")
library(VIM)
hdds[hdds$thal == '?',]
hdds$thal[hdds$thal == "?"] = NA
colSums(is.na(hdds))
hdds$thal <- kNN(hdds)$thal
colSums(is.na(hdds))
hdds$thal <- droplevels(hdds$thal)
for (i in c("sex", "cp", "fbs", "restecg", "exang", "slope", "target")) {
	hdds[,i] <- as.factor(hdds[,i])
}
remove(i)
levels(hdds$sex) <- c("f", "m")
levels(hdds$cp) <- c("typical", "atypical", "non-anginal", "asymptomatic")
levels(hdds$restecg) <- c("normal", "abnormal", "hypertrophy")
levels(hdds$slope) <- c("upsloping", "flat", "downsloping")
levels(hdds$thal) <- c("no", "chronic", "reversible")
hdds$target[hdds$target != 0] = factor("1")
hdds$target <- droplevels(hdds$target)

#Valores extremos
boxplot(hdds$age, ylab="valor", main="age", col="grey")
boxplot(hdds$trestbps, ylab="valor", main="trestbps", col="grey")
boxplot(hdds$chol, ylab="valor", main="chol", col="grey")
boxplot(hdds$thalach, ylab="valor", main="thalach", col="grey")
boxplot(hdds$oldpeak, ylab="valor", main="oldpeak", col="grey")
for (c in list(2,2.5,3)) {
    message("Coef=",c)
    for (i in c("trestbps", "chol", "thalach", "oldpeak")) {
        x <- boxplot.stats(hdds[,i], coef=c)
        message(i)
        print(x$stats)
        print(x$out)
    }
}
hdds <- hdds[!(hdds$chol == 564),]
summary(hdds$chol)

#Exportación de los datos limpios
write.csv(hdds, "cleaned.cleveland.csv")

#Análisis de los datos
#Distribuciones
library(ggplot2)
ggplot(data=hdds,aes(x=age,fill=target)) + geom_bar() + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=age,fill=target)) + geom_density(alpha=0.5) + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=sex,fill=target)) + geom_bar() + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=sex,fill=target)) + geom_bar(position="fill") + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=cp,fill=target)) + geom_bar() + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=cp,fill=target)) + geom_bar(position="fill") + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=trestbps,fill=target)) + geom_bar() + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=trestbps,fill=target)) + geom_density(alpha=0.5) + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=chol,fill=target)) + geom_bar() + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=chol,fill=target)) + geom_density(alpha=0.5) + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=fbs,fill=target)) + geom_bar() + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=fbs,fill=target)) + geom_bar(position="fill") + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=restecg,fill=target)) + geom_bar() + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=restecg,fill=target)) + geom_bar(position="fill") + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=thalach,fill=target)) + geom_bar() + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=thalach,fill=target)) + geom_density(alpha=0.5) + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=exang,fill=target)) + geom_bar() + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=exang,fill=target)) + geom_bar(position="fill") + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=oldpeak,fill=target)) + geom_bar() + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=oldpeak,fill=target)) + geom_density(alpha=0.5) + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=slope,fill=target)) + geom_bar() + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=slope,fill=target)) + geom_bar(position="fill") + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=ca,fill=target)) + geom_bar() + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=ca,fill=target)) + geom_bar(position="fill") + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=thal,fill=target)) + geom_bar() + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=thal,fill=target)) + geom_bar(position="fill") + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))
ggplot(data=hdds,aes(x=target,fill=target)) + geom_bar() + scale_fill_discrete(name="Disease",labels=c("Absence","Presence"))

#Comprobación de la normalidad y homogeneidad
#Normalidad
qqnorm(hdds$age)
qqline(hdds$age)
shapiro.test(hdds$age)
qqnorm(hdds$trestbps)
qqline(hdds$trestbps)
shapiro.test(hdds$trestbps)
qqnorm(hdds$chol)
qqline(hdds$chol)
shapiro.test(hdds$trestbps)
qqnorm(hdds$thalach)
qqline(hdds$thalach)
shapiro.test(hdds$thalach)
qqnorm(hdds$oldpeak)
qqline(hdds$oldpeak)
shapiro.test(hdds$oldpeak)
install.packages("nortest")
library(nortest)
alpha = 0.05
col.names = colnames(hdds)
for (i in 1:ncol(hdds)) {
    if (i == 1) cat("Variables que siguen una distribución normal:\n")
    if (is.integer(hdds[,i]) | is.numeric(hdds[,i])) {
        p_val = ad.test(hdds[,i])$p.value
        if (p_val < alpha) {
            cat(col.names[i])
            message(" - p-valor: ", p_val, " ")
        }
    }
}
#Homomgeneidad
fligner.test(hdds$age ~ hdds$target, data = hdds)
fligner.test(hdds$trestbps ~ hdds$target, data = hdds)
fligner.test(hdds$chol ~ hdds$target, data = hdds)
fligner.test(hdds$thalach ~ hdds$target, data = hdds)
fligner.test(hdds$oldpeak ~ hdds$target, data = hdds)
fligner.test(hdds$age + hdds$trestbps + hdds$chol + hdds$thalach + hdds$oldpeak~ hdds$target, data = hdds)

#Pruebas Estadísticas
#Análisis de correlación
install.packages("corrplot")
library(corrplot)
hddsNum <- subset(hdds, select=c(age, trestbps, chol, thalach, oldpeak, target),)
hddsNum[,c("target")] <- as.numeric(hdds[,c("target")])
corrplot.mixed(cor(hddsNum), lower = "number", upper = "square")
plot(hddsNum, col=hdds$target)
hddsB <- hdds
for (i in c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca", "thal",   "target")) {
    hddsB[,i] <- as.numeric(hdds[,i])
}
corrplot.mixed(cor(hddsB), lower = "number", upper = "square")
corRes <- cor(hddsB, use = "complete.obs", method = "spearman")
corRes["target",]

#Contraste de hipótesis
hddsEnfermo <- hdds[hdds$target==1,]$chol
hddsSano <- hdds[hdds$target==0,]$chol
var.test(hddsEnfermo, hddsSano, alternative = "two.sided", conf.level = 0.95)
t.test(chol~target, alternative='two.sided', conf.level=.95, var.equal=TRUE, data=hdds)

#Regresión logística
hddsFram <- hddsNum[,c("age", "chol", "trestbps", "target")]
corRes <- cor(hddsFram, use = "complete.obs")
corRes
corrplot.mixed(corRes, lower = "ellipse", upper = "square")
lrFram <- glm(hdds$target ~ age+chol+trestbps, hddsFram, family=binomial)
summary(lrFram)
hddsCor <- hddsNum[,c("oldpeak", "thalach", "age", "target")]
corRes <- cor(hddsCor, use = "complete.obs")
corRes
corrplot.mixed(corRes, lower = "ellipse", upper = "square")
lrCor <- glm(hdds$target ~ oldpeak+thalach+age, hddsCor, family=binomial)
summary(lrCor)
hddsCor2 <- hdds[,c("thal", "ca", "cp", "target")]
lrCor2 <- glm(hdds$target ~ thal+ca+cp, hddsCor2, family=binomial)
summary(lrCor2)

