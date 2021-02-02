library(tidyverse)
tea_data <- read.csv("artf354001_Part2_District_Crisis.csv")

cleaned_data <- tea_data %>%
  transform(STUDENT.COUNT = as.numeric(STUDENT.COUNT)) %>%
  filter(!is.na(STUDENT.COUNT)&STUDENT.COUNT != -999)

district_percentages <- cleaned_data %>%
  group_by(DISTRICT.NAME) %>%
  arrange(sum(STUDENT.COUNT)) %>%
  mutate(percent = (STUDENT.COUNT/sum(STUDENT.COUNT) * 100)) %>%
  summarize(DISTRICT.NAME, CRISIS.CODEX, round(percent, digits=2))


  # summarise(STUDENT.COUNT = sum(STUDENT.COUNT))
  # filter(STUDENT.COUNT > 50000)
# sapply(district_aggregates, class)
# aggregate(district_aggregates, by=list(district_aggregates$STUDENT.COUNT), FUN=sum)

  # transform(t2, percent = ave(STUDENT.COUNT, CRISIS.CODE, FUN = prop.table))
# t2$STUDENT.COUNT
  #mutate(percent = STUDENT.COUNT/sum(STUDENT.COUNT))
#get percentage of students of crisis code



  
#t2 %>% summarise(
#  disp = mean(disp),
#  hp = mean(hp)
#)
  #arrange(STUDENT.COUNT)
  #group_by(DISTRICT)
#as.character(unique(unlist(df)))
