library(tidyverse)

participants <- read_csv("other/test-retest_participants.csv")

set.seed(29834723)
sample(participants$Name,1)
# Rebecca Schwarz is the winner!