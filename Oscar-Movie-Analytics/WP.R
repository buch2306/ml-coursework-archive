setwd('C:\\Users\\17653\\Downloads\\Web Data Analytics')

#loading in the file
df <- read.csv('final_model.csv')


#removing unecessary columns
df <- subset(df, select = -c(movie,X) )



# First Logisitc Regression Model
y <-glm(Oscar ~ run_time+log(user_score+0.00001)+log(critic_score+0.00001)*log(critics_votes)+
        +log(audience_votes)+log(critics_votes)+log(user_score)*log(audience_votes)+
        +Action+Animation+Comedy+Documentary+Drama+Fantasy+Horror
    +Mystery+Romance+Science+Suspense+Western+PG.13 + R+ NR+ PG+log(run_time),data=df)

#summary of the first model
summary(y)

# Second Logisitc Regression Model
z <- glm(Oscar ~ run_time+log(user_score+0.00001)+log(critic_score+0.00001)*log(critics_votes)+
           +log(audience_votes)+log(critics_votes)+log(user_score)*log(audience_votes)+
           +Action+Animation+Comedy+Documentary+Drama+Fantasy+Horror
         +Mystery+Romance+Science+Suspense+Western+PG.13 + R+ NR+ PG+R*Drama + NR * Drama
         +PG.13*Drama+PG*Drama
         +R*Fantasy + NR * Fantasy+PG.13*Fantasy+PG*Fantasy+R*Suspense + NR * Suspense
         +PG.13*Suspense+PG*Suspense+R*Romance + NR * Romance
         +PG.13*Romance+PG*Romance, data=df)

#summary of the second model
summary(z)

# Third Logisitc Regression Model
y1 <- glm(Oscar ~ log(audience_votes) + log(user_score+0.0001) +
            Suspense + Fantasy + Romance +log(audience_votes) * log(user_score+0.0001)+ 
            Drama+ R*Drama +
            log(run_time)*Drama + PG.13 * Drama + R*Romance,data=df)


#sumamry of the third model
summary(y1)
