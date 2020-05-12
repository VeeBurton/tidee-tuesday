
# load data
tuesdata <- tidytuesdayR::tt_load(2020, week = 20)
volcano <- tuesdata$volcano
head(volcano)
str(volcano)
summary(volcano)
