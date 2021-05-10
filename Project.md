Analyzing NBA statistics and comparing them to player salaries to see
who gets the most points with the smallest contract.
================
2021-05-09

## Bryan Vernon bav662

### In this report, I chose to use the NBA player statistics for the 2019-2020 season and the NBA player salaries for the 2019-2020 season. The variables I am interested in are the player names, their points per game, and their salaries (in millions of dollars). I found this data on the internet using nbastuffer.com and espn.com. I find these statistics very interesting because of how much money is involved in the NBA. I want to see if the teams spending millions of dollars on their players, are doing so in a wise fasion. I am a basketball fan so I already have a good idea as to who are the top scorers in the league and I know they are the ones making big bucks, I just want to know who’s getting the worst deal from their teams based on their performances last year.

------------------------------------------------------------------------

##### First, I transferred the raw data I got from nbastuffer.com into an excel document so that I could then import it into R.

``` r
library(tidyverse)
library(readxl)
library(dplyr)

# First, I installed and called the appropriate libraries and I imported my dataset for my NBA Player Statistics.

NBA_Stats <- read_excel("C:/Users/User/OneDrive/Desktop/Comp. Bio/2019-2020 NBA Player Stats.xlsx")

# Then, I displayed the raw data I obtained from the internet. 

head(NBA_Stats)
```

    ## # A tibble: 6 x 29
    ##    RANK `FULL NAME`      TEAM  POS     AGE    GP   MPG `MIN%` `USG%` `TO%`   FTA
    ##   <dbl> <chr>            <chr> <chr> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>
    ## 1     1 James Harden     Hou   G      31.0    68  36.5   76.1   36.2  14     800
    ## 2     2 Bradley Beal     Was   G      27.1    57  36     75     34.4  11.5   458
    ## 3     3 Damian Lillard   Por   G      30.1    66  37.5   78.1   30.3  11     518
    ## 4     4 Trae Young       Atl   G      21.9    60  35.3   73.6   34.9  16.2   559
    ## 5     5 Giannis Antetok~ Mil   F      25.7    63  30.4   63.4   37.5  13.2   629
    ## 6     6 Luka Doncic      Dal   G-F    21.5    61  33.6   69.9   36.8  14.8   562
    ## # ... with 18 more variables: FT% <dbl>, 2PA <dbl>, 2P% <dbl>, 3PA <dbl>,
    ## #   3P% <dbl>, eFG% <dbl>, TS% <dbl>, PPG <dbl>, RPG <dbl>, TRB% <dbl>,
    ## #   APG <dbl>, AST% <dbl>, SPG <dbl>, BPG <dbl>, TOPG <dbl>, VI <dbl>,
    ## #   ORTG <dbl>, DRTG <dbl>

#### Part 1: Next, I decided to tidy my NBA statistics data by removing some columns and moving the points per game column to the left because this is what I would like to analyze.

``` r
NBA_Stats <- NBA_Stats %>% select(!RANK) %>% relocate("PPG", .after = "GP") %>% rename(`Player Name`="FULL NAME")# Removes the "RANK" column and relocates the "points per game" column and renames the "FULL NAME" column so it is the same as Player Name in the other dataset.

head(NBA_Stats)
```

    ## # A tibble: 6 x 28
    ##   `Player Name`    TEAM  POS     AGE    GP   PPG   MPG `MIN%` `USG%` `TO%`   FTA
    ##   <chr>            <chr> <chr> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>
    ## 1 James Harden     Hou   G      31.0    68  34.3  36.5   76.1   36.2  14     800
    ## 2 Bradley Beal     Was   G      27.1    57  30.6  36     75     34.4  11.5   458
    ## 3 Damian Lillard   Por   G      30.1    66  30    37.5   78.1   30.3  11     518
    ## 4 Trae Young       Atl   G      21.9    60  29.6  35.3   73.6   34.9  16.2   559
    ## 5 Giannis Antetok~ Mil   F      25.7    63  29.5  30.4   63.4   37.5  13.2   629
    ## 6 Luka Doncic      Dal   G-F    21.5    61  28.8  33.6   69.9   36.8  14.8   562
    ## # ... with 17 more variables: FT% <dbl>, 2PA <dbl>, 2P% <dbl>, 3PA <dbl>,
    ## #   3P% <dbl>, eFG% <dbl>, TS% <dbl>, RPG <dbl>, TRB% <dbl>, APG <dbl>,
    ## #   AST% <dbl>, SPG <dbl>, BPG <dbl>, TOPG <dbl>, VI <dbl>, ORTG <dbl>,
    ## #   DRTG <dbl>

##### Now I am going to do the same thing and import my NBA salaries dataset from espn.com.

``` r
library(readxl)
NBA_Salaries <- read_excel("C:/Users/User/OneDrive/Desktop/Comp. Bio/Salaries.xlsx")
# I installed and called the appropriate libraries and imported my dataset for the NBA Player Salaries.


# I then displayed the raw data I obtained from the internet.

head(NBA_Salaries)
```

    ## # A tibble: 6 x 4
    ##    Rank `Player Name`         `Team Name`             Salary
    ##   <dbl> <chr>                 <chr>                    <dbl>
    ## 1     1 Stephen Curry, PG     Golden State Warriors 40231758
    ## 2     2 Russell Westbrook, PG Houston Rockets       38506482
    ## 3     3 Chris Paul, PG        Oklahoma City Thunder 38506482
    ## 4     4 James Harden, SG      Houston Rockets       38199000
    ## 5     5 Kevin Durant, PF      Brooklyn Nets         38199000
    ## 6     6 John Wall, PG         Houston Rockets       38199000

#### I will now tidy my data by removing a column and seperating one column into two variables since the “Player Name” column included both the position and player name. I also divided the salary variable by 1 million so it’s easier to see. From now on the salary variable should be viewed in millions of dollars.

``` r
NBA_Salaries <- NBA_Salaries %>% select(!Rank) # Removes the "Rank" column.
NBA_Salaries$Position <- substr(NBA_Salaries$`Player Name`, nchar(NBA_Salaries$`Player Name`)-1, nchar(NBA_Salaries$`Player Name`)) # Adds a column named "Position" and fills it with the last two characters of the "Player Name" column. 

NBA_Salaries$`Player Name` <- substr(NBA_Salaries$`Player Name`,1, nchar(NBA_Salaries$`Player Name`)-3) # Removes the last 3 characters of the "Player Name" column.

NBA_Salaries$`Player Name`= gsub(",","",NBA_Salaries$`Player Name`) # Took out the "," that was following all the player names.

NBA_Salaries$"Salary" <- NBA_Salaries$Salary / 1000000 # Makes new variable that presents the salary in millions of dollars.

head(NBA_Salaries)
```

    ## # A tibble: 6 x 4
    ##   `Player Name`     `Team Name`           Salary Position
    ##   <chr>             <chr>                  <dbl> <chr>   
    ## 1 Stephen Curry     Golden State Warriors   40.2 PG      
    ## 2 Russell Westbrook Houston Rockets         38.5 PG      
    ## 3 Chris Paul        Oklahoma City Thunder   38.5 PG      
    ## 4 James Harden      Houston Rockets         38.2 SG      
    ## 5 Kevin Durant      Brooklyn Nets           38.2 PF      
    ## 6 John Wall         Houston Rockets         38.2 PG

#### Part 2: In the code below, I used an inner join function to match the player names on both datsets and add the salary column to the statistics dataset. I used this function because the column “Player Name” is the same on both datasets and so are the observations. Some players who were in the salaries dataset were not included because they were not playing in the regular season that year but they were still getting paid a salary. This could be because of injuries or suspension. Only eight players from the salaries dataset were not included.

``` r
NBA_Stats1 <- NBA_Stats %>% inner_join(NBA_Salaries, by = "Player Name") # Joins the two datasets.
NBA_Stats1 <- NBA_Stats1 %>% select(1,3,6,29,30) %>% relocate("PPG", .after = "Team Name") # Here, I chose the columns that I would like to analyze and I moved the points per game column so that it is next to the salary column.

head(NBA_Stats1)
```

    ## # A tibble: 6 x 5
    ##   `Player Name`         POS   `Team Name`              PPG Salary
    ##   <chr>                 <chr> <chr>                  <dbl>  <dbl>
    ## 1 James Harden          G     Houston Rockets         34.3   38.2
    ## 2 Bradley Beal          G     Washington Wizards      30.6   27.1
    ## 3 Damian Lillard        G     Portland Trail Blazers  30     29.8
    ## 4 Giannis Antetokounmpo F     Milwaukee Bucks         29.5   25.8
    ## 5 Kyrie Irving          G     Brooklyn Nets           27.4   31.7
    ## 6 Russell Westbrook     G     Houston Rockets         27.2   38.5

#### Part 2: Now, I will use different dplyr functions to analze the data. (I already used the select function in the process of tidying this data).

``` r
NBA_Stats1 %>% filter(PPG<14) %>% arrange(desc(PPG))%>% group_by(POS = "G") 
```

    ## # A tibble: 8 x 5
    ## # Groups:   POS [1]
    ##   `Player Name`    POS   `Team Name`              PPG Salary
    ##   <chr>            <chr> <chr>                  <dbl>  <dbl>
    ## 1 Jeff Teague      G     Minnesota Timberwolves  13.2   19  
    ## 2 Ricky Rubio      G     Phoenix Suns            13     16.2
    ## 3 Myles Turner     G     Indiana Pacers          12.1   18  
    ## 4 James Johnson    G     Minnesota Timberwolves  12     15.3
    ## 5 Tristan Thompson G     Cleveland Cavaliers     12     18.5
    ## 6 Al Horford       G     Philadelphia 76ers      11.9   28  
    ## 7 Otto Porter Jr.  G     Chicago Bulls           11.9   27.3
    ## 8 Hassan Whiteside G     Portland Trail Blazers   6.8   27.1

``` r
# Shows all point guards that scored less than 14 points per game in descending order of points
```

#### I wanted to see all the point guards that scored less than 14 points per game because these players are usually the leading scorers of their team and for them to not score over 14 points per game is a little embarrassing. As you can see, there are only 8 players that fit in this category. As we will see later with the summarise function, the average points per game of the entire dataset was about 20 points per game.

``` r
NBA_Stats1 <- NBA_Stats1 %>% mutate("PPG/Million"= PPG/Salary) # Gives a new column that calculates points per game per million in salary.
head(NBA_Stats1)
```

    ## # A tibble: 6 x 6
    ##   `Player Name`         POS   `Team Name`              PPG Salary `PPG/Million`
    ##   <chr>                 <chr> <chr>                  <dbl>  <dbl>         <dbl>
    ## 1 James Harden          G     Houston Rockets         34.3   38.2         0.898
    ## 2 Bradley Beal          G     Washington Wizards      30.6   27.1         1.13 
    ## 3 Damian Lillard        G     Portland Trail Blazers  30     29.8         1.01 
    ## 4 Giannis Antetokounmpo F     Milwaukee Bucks         29.5   25.8         1.14 
    ## 5 Kyrie Irving          G     Brooklyn Nets           27.4   31.7         0.863
    ## 6 Russell Westbrook     G     Houston Rockets         27.2   38.5         0.706

#### This statistic of points per game per million in salary will be the most important variable because this is how we will later determine who gave the most value to their team.

#### Part 3: In the following code I will show some summarizing statistics of my data to further explore it.

``` r
# Show the mean points per game across all 73 players.
NBA_Stats1 %>% summarise(PPG = mean(PPG))
```

    ## # A tibble: 1 x 1
    ##     PPG
    ##   <dbl>
    ## 1  19.5

``` r
# Show the mean salary across all 73 players.
mean(NBA_Stats1$Salary)
```

    ## [1] 26.25122

``` r
# Show the standard deviation of points per game across all 73 players.
sd(NBA_Stats1$PPG)
```

    ## [1] 5.205337

``` r
# Show the standard deviation of salary across all 73 players.
sd(NBA_Stats1$Salary)
```

    ## [1] 6.744362

``` r
# Show the standard deviation of points per game per million in salary across all 73 players.
sd(NBA_Stats1$'PPG/Million')
```

    ## [1] 0.2145606

``` r
# Show the mean of points per game per million in salary across all 73 players.
mean(NBA_Stats1$'PPG/Million')
```

    ## [1] 0.7718211

#### At this point, we have found the means and standard deviations for all numerical variables. This was done to be able to better understand and explore the data.

``` r
# Find the average points per game of each team represented in the dataset
NBA_Stats1 %>% group_by(`Team Name`) %>% summarise(PPG = mean(PPG))
```

    ## # A tibble: 29 x 2
    ##    `Team Name`             PPG
    ##    <chr>                 <dbl>
    ##  1 Boston Celtics         19.2
    ##  2 Brooklyn Nets          27.4
    ##  3 Charlotte Hornets      18  
    ##  4 Chicago Bulls          18.7
    ##  5 Cleveland Cavaliers    14.8
    ##  6 Dallas Mavericks       20.0
    ##  7 Denver Nuggets         19.9
    ##  8 Detroit Pistons        16.9
    ##  9 Golden State Warriors  20.8
    ## 10 Houston Rockets        26.5
    ## # ... with 19 more rows

#### We can see here that there are 29 teams represented in this dataset

``` r
# Find the standard deviation of points per game of each team represented in the dataset
NBA_Stats1 %>% group_by(`Team Name`) %>% summarise(PPG = sd(PPG)) 
```

    ## # A tibble: 29 x 2
    ##    `Team Name`             PPG
    ##    <chr>                 <dbl>
    ##  1 Boston Celtics         1.50
    ##  2 Brooklyn Nets         NA   
    ##  3 Charlotte Hornets     NA   
    ##  4 Chicago Bulls          9.62
    ##  5 Cleveland Cavaliers    3.96
    ##  6 Dallas Mavericks       3.97
    ##  7 Denver Nuggets        NA   
    ##  8 Detroit Pistons        1.25
    ##  9 Golden State Warriors NA   
    ## 10 Houston Rockets        8.22
    ## # ... with 19 more rows

#### For the values that say NA, this must mean that there is only one player from this team represented in the dataset.

``` r
summary(NBA_Stats1)
```

    ##  Player Name            POS             Team Name              PPG       
    ##  Length:72          Length:72          Length:72          Min.   : 6.80  
    ##  Class :character   Class :character   Class :character   1st Qu.:15.72  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :19.00  
    ##                                                           Mean   :19.49  
    ##                                                           3rd Qu.:22.12  
    ##                                                           Max.   :34.30  
    ##      Salary       PPG/Million    
    ##  Min.   :15.35   Min.   :0.2510  
    ##  1st Qu.:19.97   1st Qu.:0.6402  
    ##  Median :27.27   Median :0.7379  
    ##  Mean   :26.25   Mean   :0.7718  
    ##  3rd Qu.:30.57   3rd Qu.:0.8828  
    ##  Max.   :40.23   Max.   :1.3077

#### Part 4: I will now create plots to show some relationships between variables.

``` r
# Group by position
ggplot(NBA_Stats1, aes(x=POS, y=PPG, fill = Salary))+geom_bar(stat="identity")+ ggtitle("Different positions and how many points per game they score shaded by salary")
```

<img src="Project_files/figure-gfm/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

#### This shows a nice graph of the different positions and how many points per game they score and it is also shaded in by salary. We can see that the tallest column has a dark band about three ticks down and that is probably going to be our most valuable player because they score the most points with the least salary.

``` r
# Makes a Correlation plot for salary and points per game by position
ggplot(NBA_Stats1, aes(PPG, Salary)) + ggtitle("Correlation between salary and points per game by position") + xlab('Points per Game') + ylab("Salary")+geom_point(col = 'Blue')+geom_smooth(method= lm, aes(color=POS))
```

<img src="Project_files/figure-gfm/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

#### This plot is one that tracks the relationship between salary and points per game by position

``` r
# Making a heatmap
# Choosing the numeric variables
plot <- NBA_Stats1 %>% select_if(is.numeric)

# Plotting the map
cor(plot, use = "pairwise.complete.obs")%>% as.data.frame %>% rownames_to_column %>% pivot_longer(-1, names_to = "other_var", values_to = "correlation") %>% ggplot(aes(rowname,other_var,fill = correlation)) +geom_tile()+scale_fill_gradient2(low = "blue", mid = "white", high = "yellow")+geom_text(aes(label=round(correlation,2)), color = "brown", size = 4) + labs(title = "Correlation matrix for NBA Statistics", x = "variable 1", y = "variable 2")
```

<img src="Project_files/figure-gfm/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

#### This is a heatmap that shows the correlation between all the numeric variable in the dataset.

#### Part 5: Next, we will use cluster data to analyze the means and standard deviations of points per game and salary.

``` r
# Setting our variables to find euclidean distances
centers <- NBA_Stats1 %>% mutate(cluster = sample(c('1','2','3'), 72, replace=T))
NBA_Stats2 <- NBA_Stats1 %>% mutate(dist1 = sqrt((PPG - centers$PPG[1])^2 + (Salary - centers$Salary[1])^2), dist2 = sqrt((PPG - centers$PPG[2])^2 + (Salary - centers$Salary[2])^2), dist3 = sqrt((PPG - centers$PPG[3])^2 + (Salary - centers$Salary[3])^2)) %>% rowwise() %>% mutate(cluster = which.min(c(dist1,dist2,dist3))) %>% ungroup()
# Making a ggplot to show distribution of variables
ggplot(NBA_Stats1) + geom_point(aes(PPG, Salary)) + geom_point(data = centers, aes(PPG, Salary ,fill=""), color="black", size=4) + scale_fill_manual(name="cluster", values = "blue")+ ggtitle("Cluster data")
```

<img src="Project_files/figure-gfm/unnamed-chunk-15-1.png" style="display: block; margin: auto;" />

#### This shows a kindof scatterplot that shows the distances the variables are between each other in a cluster format.

``` r
# Iterate and give colors to the different clusters
centers <- NBA_Stats2 %>%
     group_by(cluster) %>%
     summarize(PPG = mean(PPG), Salary = mean(Salary))
ggplot(NBA_Stats2) +     geom_point(aes(PPG, Salary, color = as.factor(cluster))) +     geom_point(data = centers, aes(PPG,Salary), color="black", size=4)+ ggtitle("Cluster data after one iteration")
```

<img src="Project_files/figure-gfm/unnamed-chunk-16-1.png" style="display: block; margin: auto;" />

#### This is our first iteration of clustering our data

``` r
# Iterate a second time
NBA_Stats3 <- NBA_Stats1 %>% mutate(dist1 = sqrt((PPG - centers$PPG[1])^2 + (Salary - centers$Salary[1])^2), dist2 = sqrt((PPG - centers$PPG[2])^2 + (Salary - centers$Salary[2])^2), dist3 = sqrt((PPG - centers$PPG[3])^2 + (Salary - centers$Salary[3])^2)) %>% rowwise() %>% mutate(cluster = which.min(c(dist1,dist2,dist3))) %>% ungroup()
centers <- NBA_Stats3 %>%
     group_by(cluster) %>%
     summarize(PPG = mean(PPG), Salary = mean(Salary))
 ggplot(NBA_Stats3) +
     geom_point(aes(PPG, Salary, color = as.factor(cluster))) +
     geom_point(data = centers, aes(PPG,Salary), color="black", size=4) + ggtitle("Cluster data after two iterations")
```

<img src="Project_files/figure-gfm/unnamed-chunk-17-1.png" style="display: block; margin: auto;" />

#### This is our second, more accurate dipiction of our clustered data.

``` r
# Iterate 100 times
for(i in 1:100){NBA_Stats4 <- NBA_Stats1 %>% mutate(dist1 = sqrt((PPG - centers$PPG[1])^2 + (Salary - centers$Salary[1])^2), dist2 = sqrt((PPG - centers$PPG[2])^2 + (Salary - centers$Salary[2])^2), dist3 = sqrt((PPG - centers$PPG[3])^2 + (Salary - centers$Salary[3])^2)) %>% rowwise() %>% mutate(cluster = which.min(c(dist1,dist2,dist3))) %>% ungroup()
centers <- NBA_Stats4 %>%
     group_by(cluster) %>%
     summarize(PPG = mean(PPG), Salary = mean(Salary))}
 ggplot(NBA_Stats4) +
     geom_point(aes(PPG, Salary, color = as.factor(cluster))) +
     geom_point(data = centers, aes(PPG,Salary), color="black", size=4) + ggtitle("Cluster data after many iterations")
```

<img src="Project_files/figure-gfm/unnamed-chunk-18-1.png" style="display: block; margin: auto;" />

#### This is our clustered data after 100 iterations, therefore this is as accurate as the relationship is going to be.

------------------------------------------------------------------------

    ##           sysname           release           version          nodename 
    ##         "Windows"          "10 x64"     "build 19041" "DESKTOP-C7HNN7K" 
    ##           machine             login              user    effective_user 
    ##          "x86-64"            "User"            "User"            "User"
