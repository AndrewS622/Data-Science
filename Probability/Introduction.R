library(gtools)
library(tidyverse)

## Olympic Running
# 8 runners and 3 medals
perm <- permutations(8,3)
dim(perm)[1]

# 3 runners from Jamaica
# how many different ways can they win all 3
Jamaica <- c(1,2,3)
win <- function(i) {
  all(Jamaica %in% perm[i,])
}
is <- 1:dim(perm)[1]
Jwins <- sapply(is, win)
sum(Jwins)/dim(perm)[1]

# 8 runner nationalities using MC simulation
set.seed(1)
B <- 10000
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
winners <- replicate(B, {
  idx <- sample(1:8, 3)
  ifelse(all(runners[idx] == "Jamaica"), 1, 0)
})
mean(winners)

## Restaurant Management
# each meal: 1 entree, 2 sides, 1 drink
# choices: 6 entrees, 6 sides, 2 drinks
(dim(combinations(6, 2))[1]) * 
(dim(combinations(6, 1))[1]) * 
(dim(combinations(2, 1))[1])

# adding a third drink
(dim(combinations(6, 2))[1]) * 
  (dim(combinations(6, 1))[1]) * 
  (dim(combinations(3, 1))[1])

# allowing for a third side selection
(dim(combinations(6, 3))[1]) * 
  (dim(combinations(6, 1))[1]) * 
  (dim(combinations(3, 1))[1])

# adding entree options instead of sides
entrees <- function(n) {
  (dim(combinations(6, 2))[1]) * 
    (dim(combinations(n, 1))[1]) * 
    (dim(combinations(3, 1))[1])
}

entreeNum <- 1:12
combos <- sapply(entreeNum, entrees)
combos
entreeNum[which(combos > 365)[1]]

# adding side options instead
sides <- function(n) {
  (dim(combinations(n, 2))[1]) * 
    (dim(combinations(6, 1))[1]) * 
    (dim(combinations(3, 1))[1])
}
sideNum <- 2:12
combos <- sapply(sideNum, sides)
combos
sideNum[which(combos > 365)[1]]

## Esophageal cancer and alcohol/tobacco use
# comparing people with esophageal cancer to controls
# in terms of substance use, matched on various characteristics
data(esoph)
library(tidyverse)
head(esoph)
dim(esoph)[1]

all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)
highAlc <- esoph %>% filter(alcgp == "120+") %>%
  summarize(cases = sum(ncases), controls = sum(ncontrols))

# probability of being in highest alcohol group given cancer
highAlc$cases/(highAlc$cases + highAlc$controls)

lowAlc <- esoph %>% filter(alcgp == "0-39g/day") %>%
  summarize(cases = sum(ncases), controls = sum(ncontrols))

# repeat for lowest alcohol consumption
lowAlc$cases/(lowAlc$cases + lowAlc$controls)

# probability of having the cancer
pcase <- all_cases/(all_cases + all_controls)

# probability of having the cancer and smoking >= 10g/day
p10gandcase <- sum(esoph %>% filter(tobgp != "0-9g/day") %>% select(ncases))/(all_cases + all_controls)

# probability of smoking >= 10g/day given they have cancer
p10ggivencase <- p10gandcase/pcase

# repeat for controld
p10gandcontrol <- sum(esoph %>% filter(tobgp != "0-9g/day") %>% select(ncontrols))/(all_cases + all_controls)
pcontrol <- 1-pcase
p10ggivencontrol <- p10gandcontrol/pcontrol

# probability of being in highest alcohol group for a case
highAlc$cases /(all_cases)

# same for highest tobacco group
highTob <-  esoph %>% filter(tobgp == "30+") %>%
  summarize(cases = sum(ncases), controls = sum(ncontrols))
highTob$cases/all_cases

# probability of being in highest for both
highBoth <- esoph %>% filter(alcgp == "120+" & tobgp == "30+") %>%
  summarize(cases = sum(ncases), controls = sum(ncontrols))
highBoth$cases/all_cases

# probability of being in either
(highTob$cases/all_cases) + (highAlc$cases /all_cases) - highBoth$cases/all_cases

# for controls
(highAlc$controls /all_controls)
(highAlc$cases /(all_cases))/(highAlc$controls /all_controls)
highTob$controls/all_controls
highBoth$controls/all_controls
(1/all_controls)*(highTob$controls + highAlc$controls - highBoth$controls)

0.33/0.1394872