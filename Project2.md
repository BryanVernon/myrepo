Analyzing NBA statistics to see if Position affects the amount of Points
per Game for an NBA player.
================
2021-05-10

## Bryan Vernon bav662

### Part 1: In this report, I chose to use the NBA player statistics and salaries for the 2019-2020 season. The variables I am interested in are the player names, their points per game, their salaries (in millions of dollars), and their minutes per game. I found this data on the internet using nbastuffer.com and espn.com. There are 69 observations in this dataset. I had to tidy the data to remove some variables and move variable columns around so that they make sense in order. I also manipulated some variables so they are easier to interpret.

### I find these statistics very interesting because I always wondered which players are more likely to get all the points. I am a basketball fan so I already have a good idea as to who are the top scorers in the league and I know they are the ones making big bucks. I expect there to be a relationship between points per game based on position.

------------------------------------------------------------------------

#### First, I transferred the raw data I got from nbastuffer.com and espn.com into excel documents so that I could then import them into R. Then, I decided to tidy my data by removing some columns and moving the points per game column to the left because this is what I would like to analyze. I also seperated one column into two variables since the “Player Name” column for one dataset included both the position and player name. I also divided the salary variable by 1 million so it’s easier to interpret. From now on the salary variable should be viewed in millions of dollars.

``` r
library(tidyverse)
library(readxl)
library(dplyr)

# First, I installed and called the appropriate libraries and I imported my dataset for my NBA Player Statistics.

NBA_Stats <- read_excel("C:/Users/User/OneDrive/Desktop/Comp. Bio/2019-2020 NBA Player Stats.xlsx")

NBA_Stats <- NBA_Stats %>% select(!RANK) %>% relocate("PPG", .after = "GP") %>% rename(`Player Name`="FULL NAME")# Removes the "RANK" column and relocates the "points per game" column and renames the "FULL NAME" column so it is the same as Player Name in the other dataset.

NBA_Salaries <- read_excel("C:/Users/User/OneDrive/Desktop/Comp. Bio/Salaries.xlsx") # import NBA salaries data

NBA_Salaries <- NBA_Salaries %>% select(!Rank) # Removes the "Rank" column.

NBA_Salaries$Position <- substr(NBA_Salaries$`Player Name`, nchar(NBA_Salaries$`Player Name`)-1, nchar(NBA_Salaries$`Player Name`)) # Adds a column named "Position" and fills it with the last two characters of the "Player Name" column. 

NBA_Salaries$`Player Name` <- substr(NBA_Salaries$`Player Name`,1, nchar(NBA_Salaries$`Player Name`)-3) # Removes the last 3 characters of the "Player Name" column.

NBA_Salaries$`Player Name`= gsub(",","",NBA_Salaries$`Player Name`) # Took out the "," that was following all the player names.

NBA_Salaries$"Salary" <- NBA_Salaries$Salary / 1000000 # Makes new variable that presents the salary in millions of dollars.
```

#### In the code below, I used an inner join function to match the player names on both datsets and add the salary column to the statistics dataset. I used this function because the column “Player Name” is the same on both datasets and so are the observations. Some players who were in the salaries dataset were not included because they were not playing in the regular season that year but they were still getting paid a salary. This could be because of injuries or suspension. I also removed some players for playing two positions and only having one observation for the position they are in. Only eleven players from the salaries dataset were not included. I also made a new column to show Points per Game per million dollars in salary.

``` r
NBA_Stats1 <- NBA_Stats %>% inner_join(NBA_Salaries, by = "Player Name") # Joins the two datasets.

NBA_Stats1 <- NBA_Stats1 %>% select(1,3,6,7,29,30) %>% relocate("PPG", .after = "Team Name") # Here, I chose the columns that I would like to analyze and I moved the points per game column so that it is next to the salary column.

NBA_Stats1 <- NBA_Stats1 %>% mutate("PPG/Million"= PPG/Salary) # Gives a new column that calculates points per game per million in salary.

NBA_Stats1 <- NBA_Stats1 %>% filter(POS!= "F-G")
NBA_Stats1 <- NBA_Stats1 %>% filter(POS!= "G-F") # Removed the odd positions
```

#### The columns I chose for this dataset were Points per Game (PPG), Player Name, Team Name, Position (POS), Minutes per Game (MPG), Salary, and Points per Game per Million in Salary (PPG/Million)

------------------------------------------------------------------------

#### Part 2: I will now explore this dataset by showing some graphs of average points per game accross different categories.

``` r
NBA_Stats1 %>% select(`Team Name`, PPG) %>% pivot_longer(-1,names_to='DV', values_to='PPG') %>% ggplot(aes(reorder(`Team Name`,PPG),PPG, fill=`Team Name`)) + geom_bar(stat="summary", fun = "mean") + geom_errorbar(stat="summary", fun.data = "mean_se", width=.5) +
  facet_wrap(~DV, nrow=2) +
  coord_flip() +
  ylab("") + 
  theme(legend.position = "none") +
  xlab("Team Name") # Bar plot relating PPG and Team Name
```

<img src="Project2_files/figure-gfm/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

``` r
NBA_Stats1 %>% group_by(POS) %>% summarize(mean = mean(PPG))
```

    ## # A tibble: 5 x 2
    ##   POS    mean
    ##   <chr> <dbl>
    ## 1 C      17.2
    ## 2 C-F    17.3
    ## 3 F      19.3
    ## 4 F-C    22.3
    ## 5 G      20.4

#### Here, we see the points per game per NBA team in descending order. Note that this is only data made up from 69 players in the NBA and does not say anything about the league as a whole. We see the statistical representation as well.

``` r
NBA_Stats1 %>% select(POS, PPG) %>% pivot_longer(-1,names_to='DV', values_to='PPG') %>% ggplot(aes(reorder(POS,PPG),PPG, fill=POS)) + geom_bar(stat="summary", fun = "mean") + geom_errorbar(stat="summary", fun.data = "mean_se", width=.5) +
  facet_wrap(~DV, nrow=2) +
  coord_flip() +
  ylab("") + 
  theme(legend.position = "none") +
  xlab("Position") # Bar plot relating PPG and Position
```

<img src="Project2_files/figure-gfm/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

#### This time, we observed points per game based on position played. To my surprise, the point guards were not at the top.

------------------------------------------------------------------------

#### Below, I will show the relationship between salary and points per game while also tracking the regression lines of the different positions. I also showed some summary statistics that are represented by the graph.

``` r
ggplot(NBA_Stats1, aes(PPG, Salary)) + ggtitle("Correlation between salary and points per game by position") + xlab('Points per Game') + ylab("Salary")+geom_point(col = 'Blue')+geom_smooth(method= lm, aes(color=POS))
```

<img src="Project2_files/figure-gfm/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

``` r
# Correlation coefficient
cor(NBA_Stats1$PPG, NBA_Stats1$Salary)
```

    ## [1] 0.4554302

#### In this graph and correlation coefficient, we see that there is not a very strong relationship between salary and points per game with a correlation coefficient of 0.455. I will now explore the relationship between points per game and position further.

``` r
# Making a heatmap
# Choosing the numeric variables
plot <- NBA_Stats1 %>% select_if(is.numeric)

# Plotting the map
cor(plot, use = "pairwise.complete.obs")%>% as.data.frame %>% rownames_to_column %>% pivot_longer(-1, names_to = "other_var", values_to = "correlation") %>% ggplot(aes(rowname,other_var,fill = correlation)) +geom_tile()+scale_fill_gradient2(low = "blue", mid = "white", high = "yellow")+geom_text(aes(label=round(correlation,2)), color = "brown", size = 4) + labs(title = "Correlation matrix for NBA Statistics", x = "variable 1", y = "variable 2")
```

<img src="Project2_files/figure-gfm/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

#### In this correlation matrix, we see the different correlations between the different variables in the dataset. My favorite coefficient is between Minutes per Game and Points per Game. I added the Minutes per Game column because this is another relationship I would like to keep an eye on. It would be cool to see which players score the most points with the least time on the court.

------------------------------------------------------------------------

#### Part 3: Next, I will check some assumptions to prepare for the future tests I will run. First I checked for normality and then I checked for equal variance

``` r
NBA_Stats1= NBA_Stats1 %>% mutate(logPPG = log(PPG))

NBA_Stats1 %>% group_by(POS) %>% summarize(p.value = shapiro.test(logPPG)$p.value)
```

    ## # A tibble: 5 x 2
    ##   POS   p.value
    ##   <chr>   <dbl>
    ## 1 C      0.0707
    ## 2 C-F    0.190 
    ## 3 F      0.834 
    ## 4 F-C    0.779 
    ## 5 G      0.480

``` r
NBA_Stats1 %>% group_by(POS) %>% summarize(variance = var(logPPG))
```

    ## # A tibble: 5 x 2
    ##   POS   variance
    ##   <chr>    <dbl>
    ## 1 C       0.145 
    ## 2 C-F     0.111 
    ## 3 F       0.0609
    ## 4 F-C     0.0211
    ## 5 G       0.0653

#### In checking for normality we found that all the p-values are greater than 0.05 which means we can accept the null hypothesis that the data is normally distributed. We also found all the variances to be around the same values.

------------------------------------------------------------------------

#### Since we passed our assumptions, I will now run an ANOVA since I only have two variables to compare to see if there is a significant difference between the average Points per Game and position. The null hypothesis is that Position is not a significant indicator of Points per Game or Salary. The alternative hypothesis is that Position is a significant indicator of Points per Game or Salary.

``` r
anova_NBA <- aov(logPPG ~ POS, data = NBA_Stats1)
summary(anova_NBA)
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## POS          4  0.478 0.11956   1.568  0.194
    ## Residuals   64  4.880 0.07624

#### Based on our p-value from our MANOVA: we see that, on average, the salary of a player and their points per game are not significantly different based on what position they play. This came as a surprise to me, but after looking at my graph from earlier I found that this analysis is a testament to how much Centers and other positions produce points in the NBA. (In most scenarios, Centers get less points and more rebounds.) The probability of a type I error in this test was 0.05.

------------------------------------------------------------------------

#### Part 4: I will now perform a randomization test on this data.

``` r
# Randomization testing

ggplot(NBA_Stats1, aes(PPG, fill = POS)) + geom_histogram()+ facet_wrap(~POS) + theme(legend.position = "none") # Represent the distribution of PPG by Position. 
```

<img src="Project2_files/figure-gfm/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

``` r
# Calculate the mean difference between two conditions
true_diff = NBA_Stats1 %>% group_by(POS) %>% summarise(means= mean(PPG)) %>% summarise(mean_diff = diff(means)) %>% 
  pull 
true_diff
```

    ## [1]  0.02380952  2.01285714  3.00500000 -1.92327586

``` r
# Repeat randomization many times
# Create an empty vector to store the mean differences

mean_diff= vector()

# Create many randomizations with a for loop

for(i in 1:5000){
  temp= data.frame(POS = NBA_Stats1$POS, PPG = sample(NBA_Stats1$PPG))
  mean_diff[i] = temp %>% group_by(POS) %>% summarise(means= mean(PPG)) %>% summarise(mean_diff = diff(means)) %>% 
  pull
}

{hist(mean_diff, main = "Distribution of the mean differences"); abline(v= mean(true_diff), col = "red")}
```

<img src="Project2_files/figure-gfm/unnamed-chunk-9-2.png" style="display: block; margin: auto;" />

#### With this distribution, we see there is a normal pattern with the mean differences being just over one on average. This means there are not many (if any) outliers in our data.

------------------------------------------------------------------------

#### Part 5: I will now conduct a linear regression to show the relationship between Position and Points per Game.

``` r
NBA_Stats1 = NBA_Stats1 %>%
  mutate(logPPG = log(PPG)) # Adds the logPPG column for sample1 in order to standardize the data for lm
fit = lm(logPPG ~ POS, data = NBA_Stats1)
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = logPPG ~ POS, data = NBA_Stats1)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.87349 -0.17729 -0.00512  0.16641  0.55450 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 2.790408   0.092041  30.317   <2e-16 ***
    ## POSC-F      0.009265   0.139153   0.067   0.9471    
    ## POSF        0.139643   0.110832   1.260   0.2123    
    ## POSF-C      0.305119   0.165929   1.839   0.0706 .  
    ## POSG        0.190239   0.105360   1.806   0.0757 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2761 on 64 degrees of freedom
    ## Multiple R-squared:  0.08926,    Adjusted R-squared:  0.03234 
    ## F-statistic: 1.568 on 4 and 64 DF,  p-value: 0.1936

``` r
# Checking assumptions for linear regression
# Residuals against fitted values plot to check for any problematic patterns (nonlinear, equal variance)
plot(fit, which = 1)
```

<img src="Project2_files/figure-gfm/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

``` r
# Q-Q plot to check for normality of the residuals
plot(fit, which = 2)
```

<img src="Project2_files/figure-gfm/unnamed-chunk-10-2.png" style="display: block; margin: auto;" />

#### In this linear regression model, we see that using the standardized logPPG values shows that Position is not indicative of points per game. The appropriate assumptions have been checked.

------------------------------------------------------------------------

#### Part 6: Logistic Regression

``` r
# Logistic Regression
NBA_Stats1 = NBA_Stats1 %>% mutate(clump_logPPG = ntile(logPPG, 3)) %>% mutate(clump_logPPG = factor(clump_logPPG, labels = c("S", "M", "L"))) %>% mutate(y = ifelse(PPG >= 16, 1, 0))


fit1 = glm(y ~ POS, data = NBA_Stats1, family = binomial(link = "logit"))
summary(fit1)
```

    ## 
    ## Call:
    ## glm(formula = y ~ POS, family = binomial(link = "logit"), data = NBA_Stats1)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7751  -1.0579   0.6809   0.8446   1.3018  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)    0.2231     0.6708   0.333    0.739
    ## POSC-F        -0.5108     1.0165  -0.503    0.615
    ## POSF           0.6242     0.8295   0.752    0.452
    ## POSF-C        17.3429  1978.0903   0.009    0.993
    ## POSG           1.1206     0.8125   1.379    0.168
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 83.079  on 68  degrees of freedom
    ## Residual deviance: 75.930  on 64  degrees of freedom
    ## AIC: 85.93
    ## 
    ## Number of Fisher Scoring iterations: 16

``` r
table(NBA_Stats1$y, NBA_Stats1$POS)
```

    ##    
    ##      C C-F  F F-C  G
    ##   0  4   4  6   0  6
    ##   1  5   3 14   4 23

``` r
exp(coef(fit1))
```

    ##  (Intercept)       POSC-F         POSF       POSF-C         POSG 
    ## 1.250000e+00 6.000000e-01 1.866667e+00 3.403585e+07 3.066667e+00

``` r
NBA_Stats1$prob1 = predict(fit1, type="response")

NBA_Stats1$predicted = ifelse(NBA_Stats1$prob1 > 0.25, "Over 16 points a game", "Less than 16 points a game")

table(true_condition = NBA_Stats1$y, predicted_condition = NBA_Stats1$predicted) %>% addmargins
```

    ##               predicted_condition
    ## true_condition Over 16 points a game Sum
    ##            0                      20  20
    ##            1                      49  49
    ##            Sum                    69  69

``` r
print("Sensitivity:")
```

    ## [1] "Sensitivity:"

``` r
mean(NBA_Stats1[NBA_Stats1$y == 1, ]$prob1 > 0.25)
```

    ## [1] 1

``` r
print("Specificity:")
```

    ## [1] "Specificity:"

``` r
mean(NBA_Stats1[NBA_Stats1$y == 0, ]$prob1 <= 0.25)
```

    ## [1] 0

``` r
NBA_Stats1$logit = predict(fit1, type = "link")
```

#### The confusion matrix shows that with a sensistivity value and specificity value of binary type, we cannot use this prediction model to explain the results of those that scored less than 16 points per game.

``` r
sens = function(p, data = NBA_Stats1, y=y) mean(data[NBA_Stats1$y == 1, ]$prob1 > p) 
spec = function(p, data = NBA_Stats1, y=y) mean(data[NBA_Stats1$y == 0, ]$prob1 <= p)

sensitivity = sapply(seq(0,1,.01), sens, NBA_Stats1)
specificity = sapply(seq(0,1,.01), spec, NBA_Stats1)

ROC = data.frame(sensitivity, specificity, cutoff = seq(0,1,.01))

ROC %>%
  pivot_longer(-cutoff, names_to = "Key", values_to = "Rate") %>% 
  ggplot(aes(cutoff, Rate, color = Key)) + geom_path() +
  geom_vline(xintercept = c(.1,.5,.9), lty = 2, color = "gray50") +
  labs(title= "Relationship between sensitivity and specificity")
```

<img src="Project2_files/figure-gfm/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

``` r
ROC$TPR = sensitivity
ROC$FPR = 1 - specificity

ROC %>% 
  ggplot(aes(FPR, TPR)) +
  geom_path(size = 1.5)+
  geom_segment(aes(x=0, y=0, xend = 1, yend = 1), lty = 2) + scale_x_continuous(limits = c(0,1))
```

<img src="Project2_files/figure-gfm/unnamed-chunk-12-2.png" style="display: block; margin: auto;" />

``` r
library(plotROC)

ROCplot = ggplot(NBA_Stats1) + geom_roc(aes(d = y, m = prob1), cutoffs.at = list(0.1, 0.5, 0.9)) + labs(title = "ROC Plot")
ROCplot
```

<img src="Project2_files/figure-gfm/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

``` r
calc_auc(ROCplot)
```

    ##   PANEL group       AUC
    ## 1     1    -1 0.6744898

#### The area under the curve is roughly .67. This is a good thing and means that the model I made had fair prediction power. This could be because there are just about as many players that scored less than 16 points a game when compared to those who scored more than 16 points per game.

------------------------------------------------------------------------

#### For all the tests I ran, the p-values were always over 0.05. This means we can accept all the null hypothesis that Position is not a significant indicator of Points per Game.
