library(tidyverse)
library(ggplot2)
tea_data <- read.csv("artf354001_Part2_District_Crisis.csv")
large_districts <- list(246909, 61902, 43910, 101917, 101915, 57909, 71902, 220901, 60902, 43905, 101902, 15910, 79907, 220905, 101914, 227901, 101907, 57905, 244905, 101912)

calculate_score <- function(row) {
  if(row$CRISIS.CODE == "7A") {
    4 * STUDENT.COUNT
  } else if(row$CRISIS.CODE %in% list('7D', '7G')) {
    3 * STUDENT.COUNT
  } else if(row$CRISIS.CODE %in% list('7C', '7F', '7H', '7I')) {
    2 * STUDENT.COUNT
  } else if(row$CRISIS.CODE == "7E") {
    1 * STUDENT.COUNT
  } else if(row$CRISIS.CODE == "7B") {
    0 * STUDENT.COUNT
  }
  return(score * row$STUDENT.COUNT)
}

cleaned_data <- tea_data %>%
  transform(STUDENT.COUNT = as.numeric(gsub(",","",STUDENT.COUNT))) %>%
  filter(STUDENT.COUNT != -999) %>%
  filter(DISTRICT %in% large_districts)

houston <- tea_data %>%
  filter(DISTRICT.NAME == "HOUSTON ISD") %>%
  transform(STUDENT.COUNT = as.numeric(gsub(",","",STUDENT.COUNT))) %>%
  mutate(Percent = (STUDENT.COUNT/sum(STUDENT.COUNT) * 100))

district_percentages <- cleaned_data %>%
  group_by(DISTRICT) %>%
  arrange(sum(STUDENT.COUNT)) %>%
  mutate(Percent = round(STUDENT.COUNT/sum(STUDENT.COUNT) * 100, digits=2)) %>%
  summarize(DISTRICT.NAME, CRISIS.CODEX, CRISIS.CODE, STUDENT.COUNT, sum(STUDENT.COUNT), Percent)

scored_districts <- cleaned_data %>%
  mutate(CRISIS.SCORE = case_when(
    CRISIS.CODE == "7A" ~ 4 * STUDENT.COUNT,
    CRISIS.CODE %in% list('7D', '7G') ~ 3 * STUDENT.COUNT,
    CRISIS.CODE %in% list('7C', '7F', '7H', '7I') ~ 2 * STUDENT.COUNT,
    CRISIS.CODE == "7E" ~ 1 * STUDENT.COUNT,
    CRISIS.CODE == "7B" ~ 0)
  ) %>%
  group_by(DISTRICT) %>%
  mutate(TOTAL.CRISIS.SCORE = sum(CRISIS.SCORE)) %>%
  mutate(MAX.CRISIS.SCORE = sum(STUDENT.COUNT) * 4) %>%
  mutate(CRISIS_GRADE = TOTAL.CRISIS.SCORE / MAX.CRISIS.SCORE) %>%
  summarize(DISTRICT.NAME, TOTAL.CRISIS.SCORE, MAX.CRISIS.SCORE, CRISIS_GRADE) %>% collapse

# Top 3: 1: Frisco, 2: Katy, 3: Lewisville
# Bottom 3: Aldine, Houston, Arlington

  # arrange(sum(CRISIS.SCORE))
  # aggregate(CRISIS.SCORE, list(district = DISTRICT), sum)

# This creates a plot but it is UGLY
# ggplot(data=district_percentages, aes(x=DISTRICT.NAME, y=Pct, fill=CRISIS.CODEX)) +
#   geom_bar(stat="identity", position=position_dodge())+
#   geom_text(aes(label=Pct), vjust=1.6, color="white",
#             position = position_dodge(0.9), size=3.5)+
#   scale_fill_brewer(palette="Paired")+
#   theme_minimal()

# get percentage of students of crisis code

# a report of who has high percentage of non-contacted and non-engaged
# a report of who is losing contact/engagement
# not fully engaged - combine no contact / no engagement
# find top 2 or three districts in terms of engagement, both ends (least engaged and most engaged)
# Find some qualitative information as well - black / hispanic students? Neighborhoods / internet activity

# 7A - ENGAGED FOR MAJORITY OF TIME PERIOD
# 7B - NO CONTACT FOR ENTIRE TIME PERIOD
# 7C - NO CONTACT BEFORE 5/1; CONTACT 5/1 OR AFTER BUT NOT ENGAGED THRU END OF YR
# 7D - NO CONTACT BEFORE 5/1; CONTACT 5/1 OR AFTER AND ENGAGED THRU END OF YR
# 7E - CONTACT BUT NOT ENGAGED FOR MAJORITY OF TIME PERIOD
# 7F - CONTACT BUT NOT ENGAGED BEFORE 5/1; NO CONTACT 5/1 OR AFTER
# 7G - CONTACT BUT NOT ENGAGED BEFORE 5/1; ENGAGED 5/1 OR AFTER THRU END OF YR
# 7H - ENGAGED BEFORE 5/1; NO CONTACT 5/1 THRU END OF YR
# 7I - ENGAGED BEFORE 5/1; NOT ENGAGED 5/1 THRU END OF YR


# Contact / Engagement Failures
# Engaged - list('7A') - 4
# Complete Reengagement - list('7D', '7G) - 3
# Regressions - list('7C', 7F', '7H', '7I') - 2
# No Engagement - list('7E') - 1
# No Contact - list ('7B') - points - 0

