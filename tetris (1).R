## reading the dataset
game_data  = read.csv ("tetris.csv")

## viewing some rows to find how the data looks
head(game_data)
tail(game_data)

## setting the unique values under condition as factor 
game_data$condition = factor(game_data$condition)

## checking the levels
levels (game_data$condition)

## Creating seperate dataframes for each condition 
auditory = subset (game_data$score, game_data$condition=="auditory")
auditory

visual = subset (game_data$score, game_data$condition=="visual")
visual

## Function for displaying the descriptive statistics
summary.stats <- function (x)
{
  vmean<- mean(x)
  vmedian<- median (x)
  vvar<- var(x)
  vsd<- sd(x)
  vn<- length(x)
  vmin = min(x)
  vmax = max(x)
  vIQR = IQR(x)
  
  return (list(min=vmin, max=vmax, mean=vmean, median=vmedian, var=vvar, sd=vsd, IQR= vIQR, N=vn))
  
}

## Calling the function to display descriptive statistics
summary.stats (game_data$score)
summary.stats (auditory)
summary.stats (visual)

## Plotting boxplots for each condition and for the scores overall. 
boxplot (game_data$score,main="boxplot of game scores", ylim= c(0,100), ylab = "Score")
boxplot(game_data$score~game_data$condition, main="boxplot of game scores by condition", ylim= c(0,100),xlab = "condition", ylab = "Score")

## Plotting two histograms, one for each condition, right on top of each other! 
hist (auditory, xlab="Tetris game scores", main="scores for visual (light blue) and auditory (dark blue) interruption conditions", xlim=c(0,100), breaks=seq(0,100,10), col='#193f5e')
hist (visual, breaks=seq(0,100,10), col=rgb(.25,.63,0.9,1/3), add=TRUE,)
legend("topleft",c("auditory","visual"),fill=c('#193f5e','#82a9c1'))

## To Check if the data satisfies assumptions for a t-test . 
## we run shapiro-wilk test to confirm that the data is normally distributed
## By getting p value greater than .05 we can prove data is normally distributed
shapiro.test (auditory)
shapiro.test (visual)

## To check equality of variance we run levene's test.
library ('car')
leveneTest(game_data$score, game_data$condition)

## since both assumptions are satisfied we can run our t-test
t.test (auditory, visual, paired=FALSE)

## The mean score during auditory interruption is significantly greater (M = 67.94)
## than the mean score during visual interruption (M = 46.63)
## t(61.74) = 7.456, p < .001.

## Creating new object to hold mean and standard deviation to plot error bars in bar plot
game_data.mean = c( mean(auditory), mean(visual) )
game_data.sd = c( sd(auditory), sd(visual) )

## We'll also use the names command to label our means.
names(game_data.mean) = c("Auditory", "Visual")

## Plotting the bar plots
barplot (game_data.mean, main = "Graph of Condition Means", xlab= "Game Condition", ylab="Game Score", ylim=c(0,100),col=c(col= "#9ac0f0", "#193f5e"))
## custom function to generate error bar
se.bar = function(x, y, sds, n, upper=sds/sqrt(n), lower=upper, length=0.1,...)
{
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

#plotting bar plot with error bar
br = barplot (game_data.mean, main = "Graph of Condition Means", xlab= "Interruption", ylab="Game score", ylim=c(0,100),col=c(col="#9ac0f0", "#193f5e"))
se.bar(br,game_data.mean,game_data.sd,32)
