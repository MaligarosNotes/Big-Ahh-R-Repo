library(tidyverse)
DATA =ISLR::Weekly
DATA[, "Direction"] <- ifelse(DATA[, "Direction"] == "Up", 1, 0)
DATA

X
