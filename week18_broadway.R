
#Install via 
devtools::install_github("thebioengineer/tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2020-04-28')
tuesdata <- tidytuesdayR::tt_load(2020, week = 18)

grosses <- tuesdata$grosses