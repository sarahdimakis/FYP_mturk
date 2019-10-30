library(rio)
mturk <- import("mturk_oct9.csv")

library(tidyverse)

table(mturk$Gender)
df <- data.frame(
  Gender = c("Male", "Female"),
  Value = c(46.3, 53.7)
)
bp <- ggplot(data = df, aes(x="", y =Value, fill = Gender))+
  geom_bar(width = 1, stat = "identity")

pie <- bp + coord_polar("y", start=0)
pie

mycols <- c("pink", "#BECEEF", "#868686FF", "#CD534CFF")

df <- df %>%
  arrange(desc(Gender)) %>%
  mutate(lab.ypos = cumsum(Value) - 0.5*Value)
df

df$percent <- paste(df$Value, "%", sep = "")

ggplot(df, aes(x = "", y = Value, fill = Gender)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = percent), size = 5, color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void() +
  labs(title = "Gender identity of subjects")
  

m <- mean(mturk$Age) %>% round(2)
m
sd <- sd(mturk$Age) %>% round(2)
sd
v <-paste("(M = ", m, ", SD = ", sd, ")",sep = "")
v

ggplot(data = mturk, aes(x=Age)) +
  geom_histogram(bins = 12, color="black", fill="white") +
  geom_text(aes(y = 20, x =55, label = v), size = 5, color = "black") +
  labs(title = "Ages of subjects")

colrs = c("#eb544d", "#EA8671", "#ECEAE9", "#BECEEF", "#7298E8")
len = c(8,23,37,44,22)
ggplot(data = mturk, aes(x=Political_orientation)) +
  geom_bar(color="black", fill = colrs)+
  scale_x_discrete(limits=positions)+
  labs(title = "Political orientation of subjects", x="Political orientation")+
  theme(axis.text.x=element_blank())

positions <- c("Extremely conservative", "Moderately conservative", "Moderate", "Moderately liberal","Extremely liberal")

ggplot(data = mturk, aes(x=Ethnicity)) +
  geom_bar(color="black")+
  labs(title = "Political orientation of subjects", x="Political orientation")
library(knitr)
table(mturk$Ethnicity) %>% kable()


cbind(mturk$fear_comp_spss, mturk$anger_comp_spss, mturk$neutral_comp_spss) %>% head(30)
View(mturk
     )
