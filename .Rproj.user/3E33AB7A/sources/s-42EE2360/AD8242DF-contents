library(tidyverse)
library(ggplot2)
crisis_data <- read.csv("artf354001_Part2_District_Crisis.csv")
esc_region_data <- read.csv("DREF.csv")
esc_crisis_data <- merge(crisis_data, esc_region_data, by="DISTRICT")
raw_enrollment_data <- read.csv("Enrollment Report_Statewide_Districts_Gender_2019-2020.csv") 
large_districts <- list(246909, 61902, 43910, 101917, 101915, 57909, 71902, 220901, 60902, 43905, 101902, 15910, 79907, 220905, 101914, 227901, 101907, 57905, 244905, 101912)

find_extremes <- function(code) {
  return(district_percentages %>%
           filter(CRISIS.CODE == code))
}

subtraction_filter <- function(x, y) {
  return(ifelse(x>=y,x-y,NaN))
}

district_enrollment_data <- raw_enrollment_data %>%
  transform(ENROLLMENT = as.numeric(ENROLLMENT)) %>%
  group_by(DISTRICT) %>%
  summarize(ENROLLMENT = sum(ENROLLMENT))

total_enrollment = sum(district_enrollment_data$ENROLLMENT)

esc_enrollment_data <- merge(esc_crisis_data, district_enrollment_data, by="DISTRICT") %>%
  transform(STUDENT.COUNT = as.numeric(gsub(",","",STUDENT.COUNT)))

region_enrollment_data = merge(esc_region_data, district_enrollment_data, by="DISTRICT") %>%
  group_by(REGION) %>%
  mutate(region_enrollment = sum(ENROLLMENT)) %>%
  summarize(REGION, region_enrollment, .groups="keep") %>%
  distinct()

statewide_crisis_data <- merge(crisis_data, district_enrollment_data, by="DISTRICT") %>%
  transform(STUDENT.COUNT = as.numeric(gsub(",","",STUDENT.COUNT)))

cleaned_data <- statewide_crisis_data  %>%
  filter(STUDENT.COUNT != -999)

large_districts <- cleaned_data %>%
  filter(DISTRICT %in% large_districts)

large_district_percentages <- large_districts %>%
  group_by(DISTRICT) %>%
  arrange(sum(STUDENT.COUNT)) %>%
  mutate(percent = round(STUDENT.COUNT/sum(STUDENT.COUNT) * 100, digits=2)) %>%
  summarize(DISTRICT.NAME, CRISIS.CODEX, CRISIS.CODE, STUDENT.COUNT, percent)

error_free_students = statewide_crisis_data %>%
  filter(STUDENT.COUNT != -999)

statewide_error_counts <- statewide_crisis_data %>%
  mutate(total_crisis_students = sum(STUDENT.COUNT[STUDENT.COUNT != -999])) %>%
  group_by(CRISIS.CODE) %>%
  mutate(crisis_students = sum(STUDENT.COUNT[STUDENT.COUNT != -999])) %>%
  mutate(errors = sum(STUDENT.COUNT == -999)) %>%
  mutate(undercounted_crisis_students = subtraction_filter(total_enrollment, total_crisis_students)) %>%
  summarize(
    crisis_students,
    total_enrollment,
    total_crisis_students,
    CRISIS.CODE,
    CRISIS.CODEX,
    undercounted_crisis_students,
    errors) %>%
  distinct()

esc_error_counts <- merge(region_enrollment_data, esc_enrollment_data, by="REGION") %>%
  group_by(REGION) %>%
  mutate(region_errors = sum(STUDENT.COUNT == -999)) %>%
  mutate(total_crisis_students = sum(STUDENT.COUNT[STUDENT.COUNT != -999])) %>%
  group_by(CRISIS.CODE) %>%
  mutate(crisis_students = sum(STUDENT.COUNT[STUDENT.COUNT != -999])) %>%
  mutate(undercounted_crisis_students = subtraction_filter(region_enrollment, total_crisis_students)) %>%
  summarize(
    REGION,
    region_enrollment,
    crisis_students,
    total_crisis_students,
    CRISIS.CODE,
    CRISIS.CODEX,
    undercounted_crisis_students,
    region_errors) %>%
  distinct()


# esc_error_counts <- esc_enrollment_data %>%
#   aggregate(by = list(REGION), FUN=sum) %>%
#   group_by(REGION) %>%
#   mutate(total_crisis_students = sum(STUDENT.COUNT[STUDENT.COUNT != -999])) %>%
#   mutate(region_errors = sum(STUDENT.COUNT == -999)) %>%
#   mutate(region_enrollment = sum(ENROLLMENT))
#   mutate(overcounted_crisis_students = subtraction_filter(total_crisis_students, ENROLLMENT)) %>%
#   mutate(undercounted_crisis_students = subtraction_filter(ENROLLMENT, total_crisis_students)) %>%
#   summarize(
#     DISTRICT,
#     DISTRICT.NAME,
#     region_enrollment,
#     total_crisis_students,
#     STUDENT.COUNT,
#     CRISIS.CODE,
#     CRISIS.CODEX,
#     overcounted_crisis_students,
#     undercounted_crisis_students,
#     region_errors)

large_district_count_errors <- count_errors %>%
  filter(DISTRICT %in% large_districts)

# 7A - ENGAGED FOR MAJORITY OF TIME PERIOD
# 7B - NO CONTACT FOR ENTIRE TIME PERIOD
# 7C - NO CONTACT BEFORE 5/1; CONTACT 5/1 OR AFTER BUT NOT ENGAGED THRU END OF YR
# 7D - NO CONTACT BEFORE 5/1; CONTACT 5/1 OR AFTER AND ENGAGED THRU END OF YR
# 7E - CONTACT BUT NOT ENGAGED FOR MAJORITY OF TIME PERIOD
# 7F - CONTACT BUT NOT ENGAGED BEFORE 5/1; NO CONTACT 5/1 OR AFTER
# 7G - CONTACT BUT NOT ENGAGED BEFORE 5/1; ENGAGED 5/1 OR AFTER THRU END OF YR
# 7H - ENGAGED BEFORE 5/1; NO CONTACT 5/1 THRU END OF YR
# 7I - ENGAGED BEFORE 5/1; NOT ENGAGED 5/1 THRU END OF YR

scored_districts <- large_districts %>%
  mutate(CRISIS.SCORE = case_when(
    CRISIS.CODE == "7A" ~ 4 * STUDENT.COUNT,
    CRISIS.CODE %in% list('7D', '7G') ~ 3 * STUDENT.COUNT,
    CRISIS.CODE %in% list('7C', '7F', '7H', '7I') ~ 2 * STUDENT.COUNT,
    CRISIS.CODE == "7E" ~ 1 * STUDENT.COUNT,
    CRISIS.CODE == "7B" ~ 0)) %>%
  group_by(DISTRICT) %>%
  mutate(total_students = sum(STUDENT.COUNT)) %>%
  mutate(TOTAL.CRISIS.SCORE = sum(CRISIS.SCORE)) %>%
  mutate(MAX.CRISIS.SCORE = sum(STUDENT.COUNT) * 4) %>%
  mutate(CRISIS_GRADE = TOTAL.CRISIS.SCORE / MAX.CRISIS.SCORE) %>%
  summarize(DISTRICT.NAME, TOTAL.CRISIS.SCORE, MAX.CRISIS.SCORE, CRISIS_GRADE, total_students, .groups = "keep") %>%
  distinct()

crisis_code_percentages <- cleaned_data %>%
  mutate(total_students = sum(STUDENT.COUNT)) %>%
  group_by(CRISIS.CODE) %>%
  mutate(CRISIS.PERCENT = mean(round(sum(STUDENT.COUNT)/total_students * 100, digits=2))) %>%
  summarize(CRISIS.PERCENT, CRISIS.CODE, .groups = "keep") %>%
  distinct()

 # filter looking at districts w/o missing rows
 # which variable is most frequently missing? look at top 2 or 3
 # compare student count to external data
 # try replicating for ethnicity data set

  # Top 3: 1: Frisco, 2: Katy, 3: Lewisville
  # Bottom 3: Aldine, Houston, Arlington

fully_engaged <- find_extremes("7A")
# Top 3 Engagement (7A) -> 1: Frisco, 2: Katy, 3: Lewisville
# Bottom 3 Engagement (7A) -> 1(worst): Aldine, 2: Houston, 3: Arlington

no_contact <- find_extremes("7B")
# Top 3 No Contact (7B) -> 1: Frisco 2: Cypress-Fairbanks 3: Lewisville
# Bottom 3 No Contact (7B) -> 1(worst): Aldine, 2: Houston 3: Fort Worth

  

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

# data_with_errors <- esc_crisis_data %>%
#   transform(STUDENT.COUNT = as.numeric(gsub(",","",STUDENT.COUNT))) %>%
#   filter(DISTRICT %in% large_districts)

# Contact / Engagement Failures
# Engaged - list('7A') - 4
# Complete Reengagement - list('7D', '7G) - 3
# Regressions - list('7C', 7F', '7H', '7I') - 2
# No Engagement - list('7E') - 1
# No Contact - list ('7B') - points - 0

