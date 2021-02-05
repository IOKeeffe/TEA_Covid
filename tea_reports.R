library(tidyverse)
library(ggplot2)
tea_data <- read.csv("artf354001_Part2_District_Crisis.csv")
large_districts <- list(246909, 61902, 43910, 101917, 101915, 57909, 71902, 220901, 60902, 43905, 101902, 15910, 79907, 220905, 101914, 227901, 101907, 57905, 244905, 101912)
# tea_data <- as.numeric(gsub(",","",STUDENT.COUNT))
# cleaned_data <- tea_data %>%
#   transform(STUDENT.COUNT = as.numeric(gsub(",","",STUDENT.COUNT))) %>%
#   filter(!is.na(STUDENT.COUNT)&STUDENT.COUNT != -999) %>%
#   filter(DISTRICT %in% large_districts)

houston <- tea_data %>%
  filter(DISTRICT.NAME == "HOUSTON ISD") %>%
  transform(STUDENT.COUNT = as.numeric(gsub(",","",STUDENT.COUNT)))
  # transform(STUDENT.COUNT = as.numeric(STUDENT.COUNT))
  # mutate(Percent = (STUDENT.COUNT/sum(STUDENT.COUNT) * 100))

district_percentages <- cleaned_data %>%
  group_by(DISTRICT.NAME) %>%
  arrange(sum(STUDENT.COUNT)) %>%
  mutate(Percent = (STUDENT.COUNT/sum(STUDENT.COUNT) * 100)) %>%
  summarize(DISTRICT.NAME, CRISIS.CODEX, Pct = round(Percent, digits=2))

# This creates a plot but it is UGLY
# ggplot(data=district_percentages, aes(x=DISTRICT.NAME, y=Pct, fill=CRISIS.CODEX)) +
#   geom_bar(stat="identity", position=position_dodge())+
#   geom_text(aes(label=Pct), vjust=1.6, color="white",
#             position = position_dodge(0.9), size=3.5)+
#   scale_fill_brewer(palette="Paired")+
#   theme_minimal()

#get percentage of students of crisis code

# a report of who has high percentage of non-contacted and non-engaged
# a report of who is losing contact/engagement
