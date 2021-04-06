library(tidyverse)
library(ggplot2)
crisis_codes <- c("7A", "7B", "7C", "7D", "7E", "7F", "7G", "7H", "7I")
raw_crisis_data <- read.csv("artf354001_Part2_District_Crisis.csv")
completed_crisis_data = raw_crisis_data %>%
  complete(DISTRICT, nesting(CRISIS.CODE, CRISIS.CODEX), fill=list(STUDENT.COUNT = -999)) %>%
  fill(DISTRICT.NAME) %>%
  select(-c(YEAR))
esc_region_data <- read.csv("DREF.csv")
esc_crisis_data <- merge(completed_crisis_data, esc_region_data, by="DISTRICT")
raw_enrollment_data <- read.csv("Enrollment Report_Statewide_Districts_Gender_2019-2020.csv") 
large_district_codes <- list(246909, 61902, 43910, 101917, 101915, 57909, 71902, 220901, 43905, 101902, 15910, 79907, 220905, 101914, 227901, 101907, 57905, 15915, 101912)

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

statewide_total_enrollment = sum(district_enrollment_data$ENROLLMENT)
statewide_total_errors = nrow(subset(esc_crisis_data, STUDENT.COUNT==-999))
statewide_total_entries = nrow(esc_crisis_data)

esc_enrollment_data <- merge(esc_crisis_data, district_enrollment_data, by="DISTRICT") %>%
  transform(STUDENT.COUNT = as.numeric(gsub(",","",STUDENT.COUNT)))

region_enrollment_data = merge(esc_region_data, district_enrollment_data, by="DISTRICT") %>%
  group_by(REGION) %>%
  mutate(region_enrollment = sum(ENROLLMENT)) %>%
  summarize(REGION, region_enrollment, .groups="keep") %>%
  distinct()

statewide_crisis_data <- merge(completed_crisis_data, district_enrollment_data, by="DISTRICT") %>%
  transform(STUDENT.COUNT = as.numeric(gsub(",","",STUDENT.COUNT)))

large_districts <- statewide_crisis_data %>%
  filter(DISTRICT %in% large_district_codes)
large_districts <- merge(large_districts, esc_region_data, by="DISTRICT") 

large_district_data <- large_districts %>%
  group_by(DISTRICT) %>%
  mutate(errors = sum(STUDENT.COUNT == -999)) %>%
  mutate(error_percentage = round(errors/9 * 100, digits=2 )) %>%
  mutate(total_crisis_students = sum(STUDENT.COUNT[STUDENT.COUNT != -999])) %>%
  group_by(DISTRICT, CRISIS.CODE) %>%
  mutate(crisis_students = sum(STUDENT.COUNT[STUDENT.COUNT != -999])) %>%
  mutate(crisis_percentage = round(crisis_students/ENROLLMENT * 100, digits=2)) %>%
  mutate(undercounted_crisis_students = ENROLLMENT - total_crisis_students) %>%
  mutate(undercounted_percentage = round(undercounted_crisis_students/ENROLLMENT * 100, digits=2)) %>%
  summarize(
    DISTRICT.NAME,
    CRISIS.CODE,
    ENROLLMENT,
    errors,
    error_percentage,
    total_crisis_students,
    crisis_students,
    crisis_percentage,
    undercounted_crisis_students,
    undercounted_percentage,
    REGION,
    )

# A comparison of statewide crisis code info
statewide_crisis_summary <- statewide_crisis_data %>%
mutate(total_crisis_students = sum(STUDENT.COUNT[STUDENT.COUNT != -999])) %>%
group_by(CRISIS.CODE) %>%
mutate(crisis_students = sum(STUDENT.COUNT[STUDENT.COUNT != -999])) %>%
mutate(crisis_percentage = round(crisis_students/statewide_total_enrollment * 100, digits=2)) %>%
mutate(errors = sum(STUDENT.COUNT == -999)) %>%
mutate(total_error_percentage = round(statewide_total_errors / statewide_total_entries * 100, digits=2)) %>%
mutate(crisis_error_percentage = round(errors / statewide_total_districts * 100, digits=2)) %>%
mutate(undercounted_crisis_students = subtraction_filter(statewide_total_enrollment, total_crisis_students)) %>%
mutate(undercounted_percentage = round(undercounted_crisis_students/statewide_total_enrollment * 100, digits=2)) %>%
summarize(
  crisis_students,
  crisis_percentage,
  statewide_total_enrollment,
  total_crisis_students,
  undercounted_crisis_students,
  undercounted_percentage,
  CRISIS.CODE,
  CRISIS.CODEX,
  errors,
  crisis_error_percentage,
  total_error_percentage) %>%
distinct()

# This is a comparison of code 7A to all other crisis codes statewide.
statewide_simple_crisis_summary <- statewide_crisis_data %>%
  mutate(total_crisis_students = sum(STUDENT.COUNT[STUDENT.COUNT != -999])) %>%
  mutate(crisis_code_summary = ifelse(CRISIS.CODE == "7A", "Fully Engaged", "All Other variables")) %>%
  group_by(crisis_code_summary) %>%
  mutate(crisis_students = sum(STUDENT.COUNT[STUDENT.COUNT != -999])) %>%
  mutate(crisis_percentage = round(crisis_students/total_crisis_students * 100, digits=2)) %>%
  mutate(errors = sum(STUDENT.COUNT == -999)) %>%
  mutate(total_error_percentage = round(statewide_total_errors / statewide_total_entries * 100, digits=2)) %>%
  mutate(crisis_error_percentage = round(errors / statewide_total_districts * 100, digits=2)) %>%
  mutate(undercounted_crisis_students = subtraction_filter(statewide_total_enrollment, total_crisis_students)) %>%
  mutate(undercounted_percentage = round(undercounted_crisis_students/statewide_total_enrollment * 100, digits=2)) %>%
  summarize(
    crisis_students,
    crisis_percentage,
    statewide_total_enrollment,
    total_crisis_students,
    undercounted_crisis_students,
    undercounted_percentage,
    crisis_code_summary,
    errors,
    crisis_error_percentage,
    total_error_percentage,
    .groups = "keep") %>%
  distinct()


esc_crisis_counts <- merge(region_enrollment_data, esc_enrollment_data, by="REGION") %>%
  group_by(REGION) %>%
  mutate(region_districts = length(unique(DISTRICT))) %>%
  mutate(region_errors = sum(STUDENT.COUNT == -999)) %>%
  mutate(crisis_students = sum(STUDENT.COUNT[STUDENT.COUNT != -999])) %>%
  mutate(undercounted_crisis_students = subtraction_filter(region_enrollment, crisis_students)) %>%
  group_by(REGION, CRISIS.CODE) %>%
  mutate(crisis_percentage = round(crisis_students/region_enrollment * 100, digits=2))


# crisis_students is total students counted in data, enrollment is official numbers.
esc_crisis_display <- esc_crisis_counts %>%
  summarize(
    REGION,
    crisis_students,
    crisis_percentage,
    CRISIS.CODE,
    CRISIS.CODEX) %>%
  distinct()

# error_percentage is how many errors (missing crisis code or -999 data) an esc region has compared to total possible reporting
# crisis_grade is a score based on how many students are engaged out of max possible.
# undercounted compares crisis reported students vs. official enrollment number
esc_region_summary <- esc_crisis_counts %>%
  group_by(REGION) %>%
  mutate(total_districts = sum(region_districts)) %>%
  # mutate(crisis_score = case_when(
  #   CRISIS.CODE == "7A" ~ 4 * crisis_students,
  #   CRISIS.CODE %in% list('7D', '7G') ~ 3 * crisis_students,
  #   CRISIS.CODE %in% list('7C', '7F', '7H', '7I') ~ 2 * crisis_students,
  #   CRISIS.CODE == "7E" ~ 1 * crisis_students,
  #   CRISIS.CODE == "7B" ~ 0)) %>%
  # mutate(total_crisis_score = sum(crisis_score)) %>%
  # mutate(max_crisis_score = sum(crisis_students) * 4) %>%
  # mutate(crisis_grade = round(total_crisis_score / max_crisis_score * 100, digits=2)) %>%
  mutate(region_error_percentage = round((region_errors/(region_districts * 9)) * 100, digits=2)) %>%
  mutate(undercounted_percentage = round(undercounted_crisis_students/region_enrollment * 100, digits=2)) %>%
  summarize(
    REGION,
    region_error_percentage,
    undercounted_percentage,
    # region_districts,
    # region_enrollment,
    # total_region_crisis_count,
    # region_errors,
    # crisis_grade,
    .groups = "keep"
  ) %>%
  distinct()

esc_region_simplified_summary <- esc_crisis_counts %>%
  group_by(REGION) %>%
  mutate(total_students = sum(STUDENT.COUNT[STUDENT.COUNT != -999])) %>%
  mutate(crisis_code_summary = ifelse(CRISIS.CODE == "7A", "Fully Engaged", "All Other variables")) %>%
  group_by(REGION, crisis_code_summary) %>%
  mutate(crisis_student_count = sum(STUDENT.COUNT[STUDENT.COUNT != -999])) %>%
  mutate(crisis_percentage = round(crisis_student_count / total_students * 100, digits=2)) %>%
  summarize(
    REGION,
    region_enrollment,
    crisis_student_count,
    crisis_percentage,
    .groups = "keep"
  ) %>%
  distinct()

esc_region_simplified_summary <- esc_crisis_counts %>%
  group_by(REGION) %>%
  mutate(total_districts = sum(region_districts)) %>%
  mutate(crisis_code_summary = ifelse(CRISIS.CODE == "7A", "Fully Engaged", "All Other variables")) %>%
  mutate(region_error_percentage = round((region_errors/(region_districts * 9)) * 100, digits=2)) %>%
  mutate(undercounted_percentage = round(undercounted_crisis_students/region_enrollment * 100, digits=2)) %>%
  mutate(total_crisis_students = sum(STUDENT.COUNT[STUDENT.COUNT != -999])) %>%
  group_by(REGION, crisis_code_summary) %>%
  mutate(crisis_student_count = sum(STUDENT.COUNT[STUDENT.COUNT != -999])) %>%
  mutate(crisis_student_percent = round(crisis_student_count / total_crisis_students * 100, digits=2)) %>%
  summarize(
    REGION,
    region_districts,
    crisis_code_summary,
    region_enrollment,
    crisis_student_count,
    crisis_student_percent,
    total_region_crisis_count,
    undercounted_percentage,
    region_errors,
    region_error_percentage,
    .groups = "keep"
  ) %>%
  distinct()

mutate(region_error_percentage = round((region_errors/(region_districts * 9)) * 100, digits=2)) %>%
  mutate(undercounted_percentage = round(undercounted_crisis_students/region_enrollment * 100, digits=2)) %>%group_by(REGION) %>%large_district_count_errors <- count_errors %>%
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
  mutate(crisis_score = case_when(
    CRISIS.CODE == "7A" ~ 4 * STUDENT.COUNT,
    CRISIS.CODE %in% list('7D', '7G') ~ 3 * STUDENT.COUNT,
    CRISIS.CODE %in% list('7C', '7F', '7H', '7I') ~ 2 * STUDENT.COUNT,
    CRISIS.CODE == "7E" ~ 1 * STUDENT.COUNT,
    CRISIS.CODE %in% list('7B', -999) ~ 0)) %>%
  group_by(DISTRICT) %>%
  mutate(errors = sum(STUDENT.COUNT == -999)) %>%
  mutate(total_students = sum(STUDENT.COUNT[STUDENT.COUNT != -999])) %>%
  mutate(total_crisis_score = sum(crisis_score)) %>%
  mutate(max_crisis_score = sum(STUDENT.COUNT[STUDENT.COUNT != -999]) * 4) %>%
  mutate(crisis_grade = total_crisis_score / max_crisis_score) %>%
  summarize(DISTRICT.NAME, crisis_grade, total_students, errors, .groups = "keep") %>%
  distinct()

crisis_code_percentages <- cleaned_data %>%
  mutate(total_students = sum(STUDENT.COUNT)) %>%
  group_by(CRISIS.CODE) %>%
  mutate(CRISIS.PERCENT = mean(round(sum(STUDENT.COUNT)/total_students * 100, digits=2))) %>%
  summarize(CRISIS.PERCENT, CRISIS.CODE, .groups = "keep") %>%
  distinct()

fully_engaged <- find_extremes("7A")
# Top 3 Engagement (7A) -> 1: Frisco, 2: Katy, 3: Lewisville
# Bottom 3 Engagement (7A) -> 1(worst): Aldine, 2: Houston, 3: Arlington

no_contact <- find_extremes("7B")
# Top 3 No Contact (7B) -> 1: Frisco 2: Cypress-Fairbanks 3: Lewisville
# Bottom 3 No Contact (7B) -> 1(worst): Aldine, 2: Houston 3: Fort Worth

# Contact / Engagement Failures
# Engaged - list('7A') - 4
# Complete Reengagement - list('7D', '7G) - 3
# Regressions - list('7C', 7F', '7H', '7I') - 2
# No Engagement - list('7E') - 1
# No Contact - list ('7B') - points - 0

