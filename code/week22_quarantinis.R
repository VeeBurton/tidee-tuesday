
library(tidyverse)

# data please
tuesdata <- tidytuesdayR::tt_load(2020, week = 22)
cocktails <- tuesdata$cocktails

# warnings
# measure column intentionally left as a string with a number + volume/unit... 
# so that you can try out potential strategies to cleaning it up.
# some potential tools:
# tidyr::separate(): Link
# stringr::str_split(): Link

# have a looook
glimpse(cocktails)
cocktails$measure
# mayyyybe i'll just ignore measure

# what question do i want to ask?
# how faffy are cocktails to make? can they take up maximum time in quarantine

candidates <- cocktails %>% 
  group_by(drink) %>% 
  summarise(faff_level=sum(ingredient_number)) %>% 
  arrange(desc(faff_level))

(average_faff <- mean(candidates$faff_level))
summary(candidates$faff_level)

very_faffy <- candidates %>% 
  filter(faff_level>=15)
very_faffy <- unique(very_faffy$drink)

quarantinis <- cocktails[cocktails$drink %in% very_faffy, ]

summary(quarantinis)

quarantinis2 <- quarantinis %>% 
  group_by(drink) %>% 
  mutate(faff_level=sum(ingredient_number))

summary(quarantinis2$faff_level)
quarantinis2 <- quarantinis2 %>% 
  mutate(faffiness=ifelse(faff_level<=21, "very faffy", "ridiculously faffy"))

ggplot(quarantinis2)+
  theme_minimal()+
  ggtitle("The faffiest quarantinis available")+
  geom_col(aes(x=drink,y=faff_level))+
  coord_polar()+
  facet_wrap(~faffiness)
