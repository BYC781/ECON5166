dt <- read.csv('dataset.csv')
new_df <- fastDummies::dummy_cols(dt, select_columns = c('college', 'orientation', 'color', 'face_shape'))
library(dplyr)
new_df1 <- new_df %>% select(!c(id,college, orientation, color, face_shape, pic_id))

df2 <-new_df1
for (i in 1:nrow(df2)){
    if (df2$friend_request[i] >= 3){
        df2$swipe[i] <- 1
    }
    else if (df2$friend_request[i] < 3){
        df2$swipe[i] <- 0
    }
}
colnames(df2)
xnam <- colnames(df2)[c(4:10, 12:15, 17:24, 46:51, 25:45, 1, 2)]
fmla <- as.formula(paste("swipe ~ ", paste(xnam, collapse= "+")))

ols_model <- lm(fmla, data = df2)
mylogit <- glm(fmla, family = binomial, data = df2)
summary(mylogit)
summary(ols_model)

set.seed(1234)
seq <- round(rnorm(18, 5, 5),0)
T <- (mean(seq) - 5) / sd(seq)
critical_point <- c(qt(0.025, df=17), qt(0.975, df=17))

sd(seq)
mean