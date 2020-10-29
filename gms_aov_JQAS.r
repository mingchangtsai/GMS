library(broom)
library(tidyverse)
library(readr)
library(emmeans)


### winning time
oly_boat <- c('LM2x','LW2x','M1x','M2-','M2x','M4x','W1x','W2-','W2x','W4-','W4x','W8+','M8+','M4-')
mydata <- read_csv('gmsdata20200413v3.csv')

a <- mydata %>% group_by(ref,boatname) %>% summarise(count=n())

mod.lm <- lm(delta~grp-1, mydata)
res <- tidy(summary(mod.lm),'grp') %>% 
  mutate(ref1=sapply(term, function(x){strsplit(x,"v")[[1]][1]}),
         ref2=sapply(term, function(x){strsplit(x,"v")[[1]][2]})) %>% 
  mutate(ref1=gsub("grp","",ref1))



mod.aov <- aov(mod.lm)
posthoc <- tidy(TukeyHSD(mod.aov)) %>% 
  mutate(ref1=sapply(comparison, function(x){strsplit(x,"[v-]")[[1]][1]}),
         ref2=sapply(comparison, function(x){strsplit(x,"[v-]")[[1]][2]}),
         ref3=sapply(comparison, function(x){strsplit(x,"[v-]")[[1]][3]}),
         ref4=sapply(comparison, function(x){strsplit(x,"[v-]")[[1]][4]}),
         chk1=ifelse(ref2==ref4,1,0),
         chk2=ifelse(ref1==ref3,1,0)) %>% 
  select(-term) %>% 
  filter(ref1!=ref2,ref3!=ref4, adj.p.value <=0.05) %>% 
  arrange(desc(chk1),ref1) %>% as.data.frame()

p1 <- posthoc %>% filter(chk1==1, ref1 %in% oly_boat, ref3 %in% oly_boat) %>% 
  arrange(ref1,ref3) %>% 
  select(-ref2,-ref4,-chk1,-chk2)
p2 <- posthoc %>% filter(chk2==1, ref2 %in% oly_boat, ref4 %in% oly_boat) %>% 
  arrange(ref2,ref4) %>% 
  select(-ref1,-ref3, -chk1,-chk2)

k <- tidy(lsmeans(mod.lm,'grp')) %>% 
  mutate(grp=as.character(grp),
         ref1=sapply(grp, function(x){strsplit(x,"v")[[1]][1]}),
         ref2=sapply(grp, function(x){strsplit(x,"v")[[1]][2]})) %>% 
  filter(ref1!=ref2)
