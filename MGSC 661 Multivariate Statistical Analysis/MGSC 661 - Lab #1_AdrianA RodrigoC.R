### First assignment
# Adrian Alarcon
# Rodrigo Castro

## Packages
library('ggplot2')
library('tidyverse')
library('summarytools')
library('png')
library(grid)
library('cowplot')
library('gridExtra')
library('ggpubr')

## Read dataset
dataset = read.csv('dataset/video_games_fall_2023.csv')

# 1. VISUALIZING VARIABLES
###########################

## 1.A. Score variable Summary statistics
summary(dataset$score)
descr(dataset$score)

## 1.B. Box plot

quantile_25 = quantile(dataset$score,0.25)
median = quantile(dataset$score,0.5)
quantile_75 = quantile(dataset$score,0.75)

theme =  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    plot.title = element_text(size = 16, hjust = 0.5),  
    plot.background = element_rect(fill = "white"), 
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()
  )

score_boxplot = ggplot(data = dataset, aes(x = factor(1), y = score, fill = 'blue')) +
  geom_boxplot(color = "gray", width = 0.5, show.legend = FALSE) +
  labs(x = "", y = "Score") +  
  ggtitle("Score Boxplot") +  
  scale_fill_manual(values = c('blue' = "blue")) +
  geom_text(aes(x = factor(1), y = quantile_25, label = sprintf("Q1=%.2f", quantile_25)),
            vjust = -0.5, size = 2.5, color = "white") +
  geom_text(aes(x = factor(1), y = median, label = sprintf("Median=%.2f", median)),
            vjust = 1.2, size = 2.5, color = "white") +
  geom_text(aes(x = factor(1), y = quantile_75, label = sprintf("Q3=%.2f", quantile_75)),
            vjust = 1.5, size = 2.5, color = "white") +
  theme

print(score_boxplot)

##1.C. Histplot

sumstats = data.frame(whichstat = c("mean",
                                    "median", 
                                    "Q3",
                                    "Q1"),
                      value     = c(mean(dataset$score),
                                    median(dataset$score),
                                    quantile(dataset$score, 0.75),
                                    quantile(dataset$score, 0.25)))



score_histplot = ggplot(data = dataset, aes (x = score)) +
  geom_histogram(fill = 'blue', color = "black" , bins = 20) +
  geom_density(col = "black") + 
  geom_vline(data = sumstats, aes(xintercept = value, color = whichstat),
             linewidth = 1) +
  labs(title = 'Score Histogram', x = 'Score', y = 'Count') +
  scale_color_manual(values = c("mean" = "yellow", "median" = "green", "Q3" = "red", "Q1" = "black"),
                     labels = c("mean" = "Mean", "median" = "Median", "Q3" = "Q3", "Q1" = "Q1"))+
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    plot.title = element_text(size = 16, hjust = 0.5),  
    plot.background = element_rect(fill = "white"), 
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    legend.position = "bottom", 
    legend.title = element_blank(), 
    legend.text = element_text(size = 12) 
  )

print(score_histplot)

## 1.2 Sales Global variable Summary statistics

descr(dataset$sales_global)

quantile_25 = quantile(dataset$sales_global,0.25)
median = quantile(dataset$sales_global,0.5)
quantile_75 = quantile(dataset$sales_global,0.75)

### Box plot
sales_global_boxplot = ggplot(data = dataset, aes(x = factor(1), y = sales_global, fill = 'green')) +
  geom_boxplot(color = "green", width = 0.5, show.legend = FALSE) +
  labs(x = "", y = "Global Sales") +  
  ggtitle("Global Sales Boxplot") +  
  scale_fill_manual(values = c('green' = "green")) +
  geom_text(aes(x = factor(1), y = quantile_25, label = sprintf("Q1=%.2f", quantile_25)),
            vjust = -0.5, size = 3.5, color = "black") +
  geom_text(aes(x = factor(1), y = median, label = sprintf("Median=%.2f", median)),
            vjust = -2.5, size = 3.5, color = "black") +
  geom_text(aes(x = factor(1), y = quantile_75, label = sprintf("Q3=%.2f", quantile_75)),
            vjust = -3.5, size = 3.5, color = "black") +
  theme

print(sales_global_boxplot)

### Histogram
sumstats = data.frame(whichstat = c("mean",
                                    "median", 
                                    "Q3",
                                    "Q1"),
                      value     = c(mean(dataset$sales_global),
                                    median(dataset$sales_global),
                                    quantile(dataset$sales_global, 0.75),
                                    quantile(dataset$sales_global, 0.25)))

sales_global_histplot = ggplot(data = dataset, aes (x = sales_global)) +
  geom_histogram(fill = 'green', color = "#000000", bins = 100) +
  geom_density(col = "black") + 
  geom_vline(data = sumstats, aes(xintercept = value, color = whichstat),
             size = 1) +
  labs(title = 'Global Sales Histogram', x = 'Global Sales', y = 'Count') +
  scale_color_manual(values = c("mean" = "yellow", "median" = "blue", "Q3" = "red", "Q1" = "black"),
                     labels = c("mean" = "Mean", "median" = "Median", "Q3" = "Q3", "Q1" = "Q1"))+
  theme +
  theme(
    text = element_text(size = 14),  
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    plot.title = element_text(size = 16, hjust = 0.5),  
    plot.background = element_rect(fill = "white"), 
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    legend.position = "bottom", 
    legend.title = element_blank(), 
    legend.text = element_text(size = 12) 
  )

print(sales_global_histplot)

## 1.3 Release Year variable Summary statistics

descr(dataset$release_year)

quantile_25 = quantile(dataset$release_year,0.25)
median = quantile(dataset$release_year,0.5)
quantile_75 = quantile(dataset$release_year,0.75)

## Box plot
release_year_boxplot = ggplot(data = dataset, aes(x = factor(1), y = release_year, fill = 'orange')) +
  geom_boxplot(color = "gray", width = 0.5, show.legend = FALSE) +
  labs(x = "", y = "Year of release") +  
  ggtitle("Year of Release Boxplot") +  
  scale_fill_manual(values = c('orange' = "orange")) +
  geom_text(aes(x = factor(1), y = quantile_25, label = sprintf("Q1=%.2f", quantile_25)),
            vjust = -0.5, size = 3.5, color = "white") +
  geom_text(aes(x = factor(1), y = median, label = sprintf("Median=%.2f", median)),
            vjust = 1.2, size = 3.5, color = "white") +
  geom_text(aes(x = factor(1), y = quantile_75, label = sprintf("Q3=%.2f", quantile_75)),
            vjust = 1.5, size = 3.5, color = "white") +
  theme

print(release_year_boxplot)

### Histogram
sumstats = data.frame(whichstat = c("mean",
                                    "median", 
                                    "Q3",
                                    "Q1"),
                      value     = c(mean(dataset$release_year),
                                    median(dataset$release_year),
                                    quantile(dataset$release_year, 0.75),
                                    quantile(dataset$release_year, 0.25)))

release_year_histplot = ggplot(data = dataset, aes (x = release_year)) +
  geom_histogram(fill = 'orange', color = "#000000", bins = 25) +
  geom_density(col = "black") + 
  geom_vline(data = sumstats, aes(xintercept = value, color = whichstat),
             size = 1) +
  labs(title = 'Year of Release Histogram', x = 'Year of Release', y = 'Count') +
  theme(legend.position = c(.95, .95)) +
  scale_color_manual(values = c("mean" = "yellow", "median" = "blue", "Q3" = "red", "Q1" = "black"),
                     labels = c("mean" = "Mean", "median" = "Median", "Q3" = "Q3", "Q1" = "Q1"))+
  theme +
  theme(
    text = element_text(size = 14),  
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    plot.title = element_text(size = 16, hjust = 0.5),  
    plot.background = element_rect(fill = "white"), 
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    legend.position = "bottom", 
    legend.title = element_blank(), 
    legend.text = element_text(size = 12) 
  )

print(release_year_histplot)

## 1.4 Release Year variable Summary statistics

descr(dataset$count_critic)

quantile_25 = quantile(dataset$count_critic,0.25)
median = quantile(dataset$count_critic,0.5)
quantile_75 = quantile(dataset$count_critic,0.75)

### Box plot
count_critic_boxplot = ggplot(data = dataset, aes(x = factor(1), y = count_critic, fill = 'purple')) +
  geom_boxplot(color = "gray", width = 0.5, show.legend = FALSE) +
  labs(x = "", y = "Count Critic") +  
  ggtitle("Count Critic Boxplot") +  
  scale_fill_manual(values = c('purple' = "purple")) +
  geom_text(aes(x = factor(1), y = quantile_25, label = sprintf("Q1=%.2f", quantile_25)),
            vjust = -0.5, size = 3.5, color = "white") +
  geom_text(aes(x = factor(1), y = median, label = sprintf("Median=%.2f", median)),
            vjust = 1.2, size = 3.5, color = "white") +
  geom_text(aes(x = factor(1), y = quantile_75, label = sprintf("Q3=%.2f", quantile_75)),
            vjust = 1.5, size = 3.5, color = "white") +
  theme

print(count_critic_boxplot)

### Histogram
sumstats = data.frame(whichstat = c("mean",
                                    "median", 
                                    "Q3",
                                    "Q1"),
                      value     = c(mean(dataset$count_critic),
                                    median(dataset$count_critic),
                                    quantile(dataset$count_critic, 0.75),
                                    quantile(dataset$count_critic, 0.25)))

count_critic_histplot = ggplot(data = dataset, aes (x = count_critic)) +
  geom_histogram(fill = 'purple', color = "#000000", bins = 10) +
  geom_density(col = "black") + 
  geom_vline(data = sumstats, aes(xintercept = value, color = whichstat),
             size = 1) +
  labs(title = 'Count Critic Histogram', x = 'Count Critic', y = 'Count') +
  theme(legend.position = c(.95, .95)) +
  scale_color_manual(values = c("mean" = "yellow", "median" = "blue", "Q3" = "red", "Q1" = "black"),
                     labels = c("mean" = "Mean", "median" = "Median", "Q3" = "Q3", "Q1" = "Q1"))+
  theme +
  theme(
    text = element_text(size = 14),  
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    plot.title = element_text(size = 16, hjust = 0.5),  
    plot.background = element_rect(fill = "white"), 
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    legend.position = "bottom", 
    legend.title = element_blank(), 
    legend.text = element_text(size = 12) 
  )

print(count_critic_histplot)

## 1.5 Scatter plot

scatter_sales_global = ggplot(dataset, aes(x = score, y = sales_global, color = genre)) + 
                       geom_point() + 
                       theme_minimal() + 
                       ggtitle("Score and Sales Global")
  

scatter_release_year = ggplot(dataset, aes(x = score, y = release_year, color = genre)) + 
  geom_point() + 
  theme_minimal()+
  ggtitle("Score and Release Year")

scatter_count_critic = ggplot(dataset, aes(x = score, y = count_critic, color = genre)) + 
  geom_point() + 
  theme_minimal() + 
  ggtitle("Score and Count Critic")

final = ggarrange(scatter_sales_global, scatter_release_year,scatter_count_critic,  ncol=3, nrow=1, common.legend = TRUE, legend="bottom")
annotate_figure(final, top = text_grob("Scatter Plots", 
                                      color = 'black', face = "bold", size = 14))


# 2. SIMPLE LINEAR REGRESSION
###############################

## 2.1 Regression: score = b0 + b1(sales_global)
sreg1 = lm(score~sales_global)
summary(sreg1)
b0 = coef(sreg1)[1]
b1 = coef(sreg1)[2]

summary(sreg1)$r.squared
confint(sreg1, "sales_global", level = 0.95)
summary(sreg1)$coefficients["sales_global", "t value"]
summary(sreg1)$coefficients["sales_global", "Pr(>|t|)"]

ggplot(data = dataset, aes(x = sales_global, y = score)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)+
  labs(x = "sales_global", y = "score")+
  ggtitle("Simple Regression: sales_global ~ score")+
  theme(plot.title = element_text(hjust = 0.5))

## 2.2 Regression: score = b0 + b1(release_year)
sreg2 = lm(score~release_year)
summary(sreg2)
b0 = coef(sreg2)[1]
b1 = coef(sreg2)[2]

summary(sreg2)$r.squared
confint(sreg2, "release_year", level = 0.95)
summary(sreg2)$coefficients["release_year", "t value"]
summary(sreg2)$coefficients["release_year", "Pr(>|t|)"]

ggplot(data = dataset, aes(x = release_year, y = score)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)+
  labs(x = "release_year", y = "score")+
  ggtitle("Simple Regression: release_year ~ score")+
  theme(plot.title = element_text(hjust = 0.5))

## 2.3 Regression: score = b0 + b1(count_critic)
sreg3 = lm(score~count_critic)
summary(sreg3)
b0 = coef(sreg3)[1]
b1 = coef(sreg3)[2]

summary(sreg3)$r.squared
confint(sreg3, "count_critic", level = 0.95)
summary(sreg3)$coefficients["count_critic", "t value"]
summary(sreg3)$coefficients["count_critic", "Pr(>|t|)"]

ggplot(data = dataset, aes(x = count_critic, y = score)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)+
  labs(x = "count_critic", y = "score")+
  ggtitle("Simple Regression: count_critic ~ score")+
  theme(plot.title = element_text(hjust = 0.5))


# 3. PREDICTIONS 
#################

## 3.1 Prediction: score = b0 + b1(sales_global)
b0 = coef(sreg1)[1]
b1 = coef(sreg1)[2]
b0 + 0.75*b1

## 3.2 Prediction: score = b0 + b1(release_year)
b0 = coef(sreg2)[1]
b1 = coef(sreg2)[2]
b0 + 2009*b1

# 3.3 Prediction: score = b0 + b1(count_critic)
lm.fit = lm(score~count_critic)
b0 = coef(lm.fit)[1]
b1 = coef(lm.fit)[2]
b0 + 80*b1

## 3.3 Prediction: score = b0 + b1(count_critic)
b0 = coef(sreg3)[1]
b1 = coef(sreg3)[2]
b0 + 80*b1


# 4. MULTIPLE LINEAR REGRESSION
################################

mreg = lm(score~sales_global+release_year+count_critic)
summary(mreg)

b0 = coef(mreg)[1]
b1 = coef(mreg)[2]
b2 = coef(mreg)[3]
b3 = coef(mreg)[4]

b0 + b1*0.75 + b2*2009 + b3*80


## Plots

quantile_25 = quantile(dataset$score,0.25)
median = quantile(dataset$score,0.5)
quantile_75 = quantile(dataset$score,0.75)

min_theme =  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    plot.title = element_text(size = 16, hjust = 0.5),  
    plot.background = element_rect(fill = "white"), 
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()
  )

score_boxplot = ggplot(data = dataset, aes(x = factor(1), y = score, fill = 'blue')) +
  geom_boxplot(color = "gray", width = 0.5, show.legend = FALSE) +
  labs(x = "", y = "Score") +  
  ggtitle("Score Boxplot") +  
  scale_fill_manual(values = c('blue' = "blue")) +
  geom_text(aes(x = factor(1), y = quantile_25, label = sprintf("Q1=%.2f", quantile_25)),
            vjust = -0.5, size = 2.5, color = "white") +
  geom_text(aes(x = factor(1), y = median, label = sprintf("Median=%.2f", median)),
            vjust = 1.2, size = 2.5, color = "white") +
  geom_text(aes(x = factor(1), y = quantile_75, label = sprintf("Q3=%.2f", quantile_75)),
            vjust = 1.5, size = 2.5, color = "white") +
  theme

print(score_boxplot)

### Histplot

sumstats = data.frame(whichstat = c("mean",
                                     "median", 
                                     "Q3",
                                     "Q1"),
                       value     = c(mean(dataset$score),
                                     median(dataset$score),
                                     quantile(dataset$score, 0.75),
                                     quantile(dataset$score, 0.25)))


score_histplot = ggplot(data = dataset, aes (x = score))+
                 geom_histogram(fill = 'blue', color = "#000000" , bins = 20)+
                 geom_density(col="black") + 
                 geom_vline(data=sumstats, aes(xintercept = value),
                               size=1, col = c('yellow','green','red','black'))+
                 labs(title='Score Histogram', x='Score', y='Count')+
                 theme(legend.position = c(.95,.95))
print(score_histplot)


# 5. CATEGORICAL VARIABLES
##########################

##### Categorical Variables

####### Create new variable
dataset = mutate(dataset, nintendo = if_else(dataset$publisher == 'Nintendo',1,0))

####### Linear regression

model_5 = lm(score ~ release_year + nintendo, dataset)

summary(model_5)

####### ScatterPlot

### nintendo = 1

intercept_one = coef(model_5)[1] + coef(model_5)[3]
slope_one = coef(model_5)[2]

### nintendo = 0

intercept_zero = coef(model_5)[1] 
slope_zero = coef(model_5)[2]

colors = c('1' = 'green', '0' = 'blue')

logo = readPNG('desautels_logo.png')
logo_width = 3  # Width of the logo (proportion of the plot width)
logo_height = 1  # Height of the logo (proportion of the plot height)
logo_aspect_ratio = dim(logo)[2] / dim(logo)[1]
logo_aspect_adjusted_height <- logo_height / logo_aspect_ratio

regmodel_5 = ggplot(dataset, aes(x = release_year, y = score ,color = factor(nintendo))) +
             geom_point(shape = 15)+
             theme_minimal() +
             geom_abline(intercept = intercept_one, slope = slope_one, color = 'green',linetype="dashed", linewidth = 2)+
             geom_abline(intercept = intercept_zero, slope = slope_zero, color = 'blue', linetype = 'dashed', linewidth = 2)+
             theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             axis.line = element_line(linewidth  = 0.5, color = "gray"))+
             scale_color_manual(values = colors)+
             labs(color = 'Nintendo', x = 'Release Year', y = 'Score')+
             ggtitle("Scatterplot Score vs. Release Year") +
             theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 16))

final_plot = grid.arrange(
  arrangeGrob(regmodel_5, nullGrob(), widths = c(1.4, 0.1)),
  arrangeGrob(nullGrob(), rasterGrob(logo), nullGrob(), heights = c(0.4, 0.3)),
  nrow = 2,
  heights = c(0.9, 0.1)
)

print(final_plot)

# 6. CATEGORICAL VARIABLES WITH MULTIPLE CATEGORIES
####################################################

table(dataset$genre)
dataset$genre  = as.factor(dataset$genre)
dataset$genre = relevel(dataset$genre, ref = 'Racing')

regmodel_6 = lm(score ~ genre, dataset)
summary(regmodel_6)

dataset$genre  = as.factor(dataset$genre)
dataset$genre = relevel(dataset$genre, ref = 'Shooter')

regmodel_6 = lm(score ~ genre, dataset)
summary(regmodel_6)



# 7. INTERACTION TERMS
#######################

dataset = mutate(dataset, strategy = if_else(dataset$genre == 'Strategy',1,0))
dataset$strategy = factor(dataset$strategy)

regmodel_7 = lm(score ~ nintendo + strategy + strategy*nintendo, dataset)
summary(regmodel_7)


regmodel_7c = lm(score ~ release_year + nintendo + release_year*nintendo, dataset)
summary(regmodel_7c)

## nintendo = 1

intercept_one = regmodel_7c$coefficients[1] + regmodel_7c$coefficients[3] 
coef_one = regmodel_7c$coefficients[2] + regmodel_7c$coefficients[4] 

## nintendo = 0

intercept_zero = regmodel_7c$coefficients[1]
coef_zero = regmodel_7c$coefficients[2]



regmodel_7c = ggplot(dataset, aes(x = release_year, y = score ,color = factor(nintendo))) +
  geom_point(shape = 17)+
  theme_minimal() +
  geom_abline(intercept = intercept_one, slope = coef_one, color = 'green',linetype="dashed", linewidth = 2)+
  geom_abline(intercept = intercept_zero, slope = coef_zero, color = 'blue', linetype = 'dashed', linewidth = 2)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth  = 0.5, color = "gray"))+
  scale_color_manual(values = colors)+
  labs(color = 'Nintendo', x = 'Release Year', y = 'Score')+
  ggtitle("Scatterplot Score vs. Release Year") +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 16))

final_plot = grid.arrange(
  arrangeGrob(regmodel_7c, nullGrob(), widths = c(1.4, 0.1)),
  arrangeGrob(nullGrob(), rasterGrob(logo), nullGrob(), heights = c(0.4, 0.3)),
  nrow = 2,
  heights = c(0.9, 0.1)
)

print(final_plot)


