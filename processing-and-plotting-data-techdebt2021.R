# Load packages
library(rlist)
library(listviewer)
library(tidyverse)
library(repurrrsive)
library(RColorBrewer)
library(car)
library(coin)

home <- try(system("echo $HOME", intern = TRUE))
workingDir <- "/TechnicalDebt2021"
dataset <- read.csv(paste0(home, workingDir, "/data/dataset.csv"), header = TRUE)

metricsKeys <- c(
  'lines', 'ncloc', 'comment_lines', 'classes',
  'violations','blocker_violations','major_violations','minor_violations','info_violations',
  'vulnerabilities','bugs','code_smells',
  'duplicated_blocks', 'duplicated_lines','duplicated_lines_density',
  'sqale_rating','sqale_index','sqale_debt_ratio', 'effort_to_reach_maintainability_rating_a',
  'reliability_remediation_effort', 'security_remediation_effort',
  'complexity', 'cognitive_complexity',
  'comment_density', 'smells_density', 
  'reliability_rate', 'security_rate',
  'complexity_ratio', 'cognitive_complexity_ratio',
  'complexity_rate', 'cognitive_complexity_rate',
  'ranking_points'
)

dataset <- modify_at(dataset, metricsKeys, as.numeric)

View(dataset)

metricsToAnalyze <- c(
  'reliability_rate',
  'security_rate',
  'comment_density',
  'sqale_debt_ratio', 
  'smells_density',
  'duplicated_lines_density',
  'complexity_rate',
  'cognitive_complexity_rate'
)

colorsToPlot <- brewer.pal(n = 2, name = "Pastel1")
pathToBoxPlots <- paste0(home, workingDir, "/plots/")

for (columnname in metricsToAnalyze) {
  # Open file
  postscript(paste0(pathToBoxPlots, columnname, '.eps'), horizontal = FALSE)
  with(dataset, 
       Boxplot(get(columnname)~group,
               id=list(labels='o',location='avoid',n=Inf,method="y",col='red',cex=1),
               outline=FALSE,
               axes=F,
               xlab="Approach",
               cex.lab=2,
               ylab=NA,
               ylim = c(min(dataset[,columnname]), max(dataset[,columnname])),
               col=colorsToPlot,
               border="black",
               notch = FALSE
       )
  )
  box()
  # y axis
  axis(2, cex.axis=2)
  # x axis
  axis(1, 1:2, c("penalising", "rewarding"), cex.axis=2)
  #mtext(side=2,columnname,line=2.2,cex = 1.5)
  means <- aggregate(get(columnname)~group, dataset, mean)
  points(means,col='blue',pch=18)
  #legend('top', cex=1.5, fill=colorsToPlot,
  #       c("penalising", "rewarding")
  #)
  legend('topright', cex=2, 
         col=c('red','blue'), 
         pch=c(1,18),
         legend=c("outliers", "means"), text.col=c('red','blue')
  )
  # Close the file
  dev.off() 
}


#Generate latex tables
library(xtable)

# Medidas por proyectos
latex_measures_table <- xtable(dataset, caption='Medidas por proyectos', 
                              label='tab:metrics')
print(latex_measures_table, file=paste0(home,workingDir,'/tables/measures.tex'), floating=TRUE)

# Preparing dataset for statistical tests 

penalizing <- filter(dataset, group == '2018-2019')
rewarding <- filter(dataset, group == '2019-2020')

# Normality tests
metrics <- c()
ws <- c()
pvalues <- c()
for (metric in metricsToAnalyze) {
  # hipótesis nula que los datos sí proceden de una distribución normal 
  s <- shapiro.test(x = penalizing[,metric])
  metrics <- c(metrics, metric)
  ws <- c(ws, s$statistic)
  pvalues <- c(pvalues,s$p.value) 
}

penalizing_shapirotestdf <- data.frame(metrics, ws, pvalues)
names(penalizing_shapirotestdf) <- c('metric','W','p-value')

metrics <- c()
ws <- c()
pvalues <- c()

for (metric in metricsToAnalyze) {
  # hipótesis nula que los datos sí proceden de una distribución normal 
  s <- shapiro.test(x = rewarding[,metric])
  metrics <- c(metrics, metric)
  ws <- c(ws, s$statistic)
  pvalues <- c(pvalues,s$p.value) 
}

rewarding_shapirotestdf <- data.frame(metrics, ws, pvalues)
names(rewarding_shapirotestdf) <- c('metric','W','p-value')


  home <- try(system("echo $HOME", intern = TRUE))
  workingDir <- "/TechnicalDebt2021"
  
  latex_rewarding_shapirotestdf_table <- xtable(rewarding_shapirotestdf, caption='Shapiro-Wilk normality tests results', 
                                 label='tab:normalityTestrewarding')
  print(latex_rewarding_shapirotestdf_table, file=paste0(home,workingDir,'/tables/normalityTestRewarding.tex'), floating=TRUE)
  
  latex_penalizing_shapirotestdf_table <- xtable(penalizing_shapirotestdf, caption='Shapiro-Wilk normality tests results', 
                                                label='tab:normalityTestrewarding')
  print(latex_penalizing_shapirotestdf_table, file=paste0(home,workingDir,'/tables/normalityTestPenalizing.tex'), floating=TRUE)

 
metrics <- c()
pvalues <- c()
zs <- c()
effect_sizes <- c()
h1s <- c()
cohen_effectsize_classifications <- c()


for (metric in metricsToAnalyze) {
  if (metric=='comment_density') use_alternative = 'less'
  else use_alternative='greater'
  toCoin <- data.frame(
    measures = c(penalizing[,metric], rewarding[,metric]),
    group = factor(c(rep.int('penalizing', 11), rep.int('rewarding',13)))
  )
  wilcox <- wilcox_test(measures~group,toCoin,alternative=use_alternative)
  metrics <- c(metrics, metric) 
  pvalues <- c(pvalues, pvalue(wilcox))
  zs <- c(zs, statistic(wilcox)) 
  effect_size <-  abs(statistic(wilcox))/sqrt(11+13)
  effect_sizes <- c(effect_sizes, effect_size)
  h1s <- c(h1s, use_alternative)
  if (effect_size < 0.1) classification <- 'very small effect' 
  else if (effect_size < 0.3) classification <- 'small effect'
  else if (effect_size < 0.5) classification <- 'medium effect'
  else if (effect_size >= 0.5) classification <- 'large effect'
  cohen_effectsize_classifications <- c(cohen_effectsize_classifications, classification)
}
  
  wilcoxResults <-  data.frame(metrics, h1s, pvalues, zs, effect_sizes, cohen_effectsize_classifications)
  names(wilcoxResults) <- c('Metric','H1','p-value', 'Z', 'Effect size', "Cohen's effect size classification")
  
  latex_wilcoxResults_table <- xtable(wilcoxResults, caption='Asymptotic Wilcoxon-Mann-Whitney Test and effect size', 
                                                 label='tab:wilcoxresults')
  print(latex_wilcoxResults_table, file=paste0(home,workingDir,'/tables/wilcoxresults.tex'), floating=TRUE)
  
