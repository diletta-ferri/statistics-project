risultati <- readRDS("final_results.rds")
View(risultati)

row_names <- rownames(risultati)
print(row_names)


library(scmamp)

#Plot densities and boxplots:
plotDensities(data=risultati)
boxplot(risultati)

#Non-parametric test of equality of means: Friedman Test (general distribution)
friedmanTest(risultati)

#Post-hoc Nemenyi test (all-pairs)
test = nemenyiTest(risultati, alpha=0.05)
plotCD(risultati, alpha=0.05, cex=1) # critical difference plot
test # critical difference 
test$diff.matrix # difference in mean ranks
abs(test$diff.matrix) > test$statistic # significant tests




#Check della normalità:

#Applica lo shapiro.test a tutte le colonne -> controlla normalità delle distribuzioni
sapply(risultati, function(r) shapiro.test(r)$p.value)

#bartlett test per verificare l'omogeneità delle varianze
merged = data.frame(acc=c(t(risultati)), clf=rep(colnames(risultati), nrow(risultati)))
bartlett.test(acc~clf, data=merged) #ci dice che il p value è quasi 1, quindi 

#Visto che sono normali e c'è omogeneità delle varianze è possibile usare ANOVA


