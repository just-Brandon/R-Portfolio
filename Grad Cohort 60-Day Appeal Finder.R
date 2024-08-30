# README -----------------------------------------------------------------------

## The aim of this project is to filter a list of our current graduating cohort students and find students who meet the criteria for a 60-Day Appeal.
### There are two primary qualifications for a 60-Day Appeal:
### 1. The student must not be enrolled at their most recent school for more than 60 days across all four years.
### 2. Across all four years, the student must have a majority enrollment at another school.

## To achieve this, my task is to calculate each students' enrollment:
## 1. at their current school, across all four years,
## 2. within the MSCS school district,
## 3. and at each previously attended school.

### To accomplish this, this script calls "Student Calendars.R" (also included in this portfolio) to calculate days enrolled.


# Setup ------------------------------------------------------------------------
options(java.parameters = "-Xmx16g")

suppressMessages(library(tidyverse))
suppressMessages(library(xlsx))

smas21 <- read.csv("G:/P&A Data Store/Student Master/SY2021/Student Master 2021.csv", check.names = F)
smas22 <- read.csv("G:/P&A Data Store/Student Master/SY2122/Student Master 2122.csv", check.names = F)
smas23 <- read.csv("G:/P&A Data Store/Student Master/SY2223/Student Master 2223.csv", check.names = F)
smas24 <- read.csv("G:/P&A Data Store/Student Master/Student Master 2324.csv", check.names = F)

cohort <- read.xlsx("G:/Assessment & Accountability/Assessment Works/Graduation Cohort Work/2324/Tracking Sheets/Grad Cohort District Level 2024-06-11.xlsx", 1, check.names = F)


# Filtering to Most Recent -----------------------------------------------------
## Student Master
smas21rec <- smas21 %>%
  filter(
    MostRecent == 1
  ) %>%
  select(
    "STATE ID" = TNID7,
    "STCode21" = `StCode`,
    "Ent21" = EntDate,
    "With21" = WithDate
  )
smas22rec <- smas22 %>%
  filter(
    MostRecent == 1
  ) %>%
  select(
    "STATE ID" = TNID7,
    "STCode22" = `StCode`,
    "Ent22" = EntDate,
    "With22" = WithDate
  )
smas23rec <- smas23 %>%
  filter(
    MostRecent == 1
  ) %>%
  select(
    "STATE ID" = TNID7,
    "STCode23" = `StCode`,
    "Ent23" = EntDate,
    "With23" = WithDate
  )
smas24rec <- smas24 %>%
  filter(
    MostRecentWithinYear == 1
  ) %>%
  select(
    "STATE ID" = TNID7,
    "STCode24" = `StCode`,
    "Ent24" = EntDate,
    "With24" = WithDate
  )


## Cohort Tracker
cohortfilt <- cohort %>%
  filter(
    is.na(`COMPLETION DATE`) & `INCLUDED IN COHORT` == "YES"
  ) %>%
  select(
    `STATE ID`,
    `FIRST NAME`,
    `LAST NAME`,
    `COMPLETION DATE`,
    `INCLUDED IN COHORT`,
    `SCHOOL NAME`,
    `ST CODE`
  )


# Merging in Student Masters ---------------------------------------------------
cohortrec <- merge(cohortfilt, smas21rec, by = "STATE ID", all.x = T)
cohortrec <- merge(cohortrec, smas22rec, by = "STATE ID", all.x = T)
cohortrec <- merge(cohortrec, smas23rec, by = "STATE ID", all.x = T)
cohortrec <- merge(cohortrec, smas24rec, by = "STATE ID", all.x = T)


# Formatting Dates -------------------------------------------------------------
cohortrec$Ent21 <- as.Date(cohortrec$Ent21, format = "%m/%d/%Y")
cohortrec$With21 <- as.Date(cohortrec$With21, format = "%m/%d/%Y")
cohortrec$Ent22 <- as.Date(cohortrec$Ent22, format = "%m/%d/%Y")
cohortrec$With22 <- as.Date(cohortrec$With22, format = "%m/%d/%Y")
cohortrec$Ent23 <- as.Date(cohortrec$Ent23)
cohortrec$With23 <- as.Date(cohortrec$With23)
cohortrec$Ent24 <- as.Date(cohortrec$Ent24)
cohortrec$With24 <- as.Date(cohortrec$With24)


# Filter to Most Recent Year ---------------------------------------------------
cohortrec <- cohortrec %>%
  mutate(EntMostRec = pmax(`Ent21`, `Ent22`, `Ent23`, `Ent24`, na.rm = T))
cohortrec <- cohortrec %>%
  mutate(WithMostRec = pmax(`With21`, `With22`, `With23`, `With24`, na.rm = T))

# Add Previous StCode
cohortrec$PrevStCode <- ifelse(
  !is.na(cohortrec$Ent24),
  cohortrec$STCode23,
  ifelse(
    !is.na(cohortrec$Ent23),
    cohortrec$STCode22,
    ifelse(
      !is.na(cohortrec$Ent22),
      cohortrec$STCode21,
      ifelse(
        !is.na(cohortrec$Ent21),
        print('No Prev Year'),
        NA
      )
    )
  )
)


# Separate Students Missing Dates ----------------------------------------------
missingfromsmas <- cohortrec %>%
  filter(
    is.na(WithMostRec)
  ) %>%
  select(
    1:7,
    WithMostRec,
    PrevStCode
  )
cohortinc <- cohortrec %>%
  filter(
    !is.na(WithMostRec)
  ) %>%
  select(
    1:7,
    20:22
  )


# Calendar ---------------------------------------------------------------------
source("Student Calendars.R")
cohortinc$"Days Enrolled" <- bizdays(cohortinc$EntMostRec, cohortinc$WithMostRec, stcal)+1


# Formatting to Current Tracking Sheet -----------------------------------------
cohortbind <- bind_rows(cohortinc, missingfromsmas)
tracker <- cohortbind %>%
  filter(
    cohortbind$`Days Enrolled` <= 60
  )

tracker <- tracker %>%
  rename(
    "TNID" = `STATE ID`
  )


# Reading in Students Already Checked and Removing Them ------------------------
current_tracker <- read.xlsx("C:/Users/PEREZB/Downloads/2023-24 Possible 60-day Appeals.xlsx", 1, check.names = F)
tracker_version_1.9 <- anti_join(tracker, current_tracker[c("TNID", "Version")], by = "TNID")
tracker_version_1.9$Explanation <- paste(tracker_version_1.9$EntMostRec, "-", tracker_version_1.9$WithMostRec, ",", tracker_version_1.9$`Days Enrolled`, "School Days", sep = " ")

tracker_version_2 <- tracker_version_1.9 %>%
  select(
    "School Name" = `SCHOOL NAME`,
    "School Number" = `ST CODE`,
    TNID,
    "First Name" = `FIRST NAME`,
    "Last Name" = `LAST NAME`,
    Explanation
  )

tracker_version_2$"Same School As Previous Year" <- tracker_version_2$`School Number` == tracker_version_1.9$PrevStCode

write.xlsx(tracker_version_2, "C:/Users/PEREZB/OneDrive - Shelby County Schools - SCS/Desktop/Additions - Cohort 60-Day Check.xlsx", row.names = F, showNA = F)


# Adding to Full Cohort Sheet --------------------------------------------------
cohortbind <- bind_rows(cohortinc, missingfromsmas)
cohortALL <- merge(cohort, cohortbind[c("STATE ID" ,"EntMostRec", "WithMostRec", "Days Enrolled")], by = "STATE ID", all.x = T)
#write.xlsx(cohortALL, "C:/Users/PEREZB/OneDrive - Shelby County Schools - SCS/Desktop/Full Cohort 60-Day Check.xlsx", row.names = F, showNA = F)

# Filtered to 60-day Appealable
cohortappealable <- cohortALL %>%
  filter(
    cohortALL$`Days Enrolled` <= 60
  )

#write.xlsx(cohortappealable, "C:/Users/PEREZB/OneDrive - Shelby County Schools - SCS/Desktop/Appealable Cohort 60-Day Check.xlsx", row.names = F, showNA = F)


# Calculating Total Enrollment -------------------------------------------------
## Using tracker_version_1.9, I am going to merge back with the Student Master files to find how long the students were enrolled with MSCS, and at their final schools.

smas21all <- smas21 %>%
  select(
    "TNID" = TNID7,
    `StCode`,
    EntDate,
    WithDate
  )
smas22all <- smas22 %>%
  select(
    "TNID" = TNID7,
    `StCode`,
    EntDate,
    WithDate
  )
smas23all <- smas23 %>%
  select(
    "TNID" = TNID7,
    `StCode`,
    EntDate,
    WithDate
  )
smas24all <- smas24 %>%
  select(
    "TNID" = TNID7,
    `StCode`,
    EntDate,
    WithDate
  )

smas_appealable_21 <- merge(tracker_version_1.9, smas21all, by = "TNID")
smas_appealable_22 <- merge(tracker_version_1.9, smas22all, by = "TNID")
smas_appealable_23 <- merge(tracker_version_1.9, smas23all, by = "TNID")
smas_appealable_24 <- merge(tracker_version_1.9, smas24all, by = "TNID")

smas_appealable_21$EntDate <- as.Date(smas_appealable_21$EntDate, format = "%m/%d/%Y")
smas_appealable_21$WithDate <- as.Date(smas_appealable_21$WithDate, format = "%m/%d/%Y")
smas_appealable_22$EntDate <- as.Date(smas_appealable_22$EntDate, format = "%m/%d/%Y")
smas_appealable_22$WithDate <- as.Date(smas_appealable_22$WithDate, format = "%m/%d/%Y")
smas_appealable_23$EntDate <- as.Date(smas_appealable_23$EntDate)
smas_appealable_23$WithDate <- as.Date(smas_appealable_23$WithDate)
smas_appealable_24$EntDate <- as.Date(smas_appealable_24$EntDate)
smas_appealable_24$WithDate <- as.Date(smas_appealable_24$WithDate)

smas_appealable_all <- bind_rows(smas_appealable_21, smas_appealable_22, smas_appealable_23, smas_appealable_24)

smas_appealable_all$"Per Year Days Enrolled" <- bizdays(smas_appealable_all$EntDate, smas_appealable_all$WithDate, stcal)+1

smas_appealable_all$"StCode Match" <- smas_appealable_all$`ST CODE` == smas_appealable_all$StCode
smas_app_same_school <- smas_appealable_all %>%
  filter(
    smas_appealable_all$`StCode Match` == T
  )

smas_all_days <- smas_appealable_all %>%
  group_by(TNID) %>%
  summarise("MSCS Total Days Enrolled" = sum(`Per Year Days Enrolled`))

tracker_better <- merge(tracker_version_1.9, smas_all_days, by = "TNID")

smas_same_school_days <- smas_app_same_school %>%
  group_by(TNID) %>%
  summarise("Total Days At Most Recent School" = sum(`Per Year Days Enrolled`))

tracker_better <- merge(tracker_better, smas_same_school_days, by = "TNID")

tracker_better$"% Enrollment At Most Recent School" <- round((tracker_better$`Total Days At Most Recent School` / tracker_better$`MSCS Total Days Enrolled`) * 100)

write.xlsx(tracker_better, "C:/Users/PEREZB/OneDrive - Shelby County Schools - SCS/Desktop/tracker_better.xlsx", row.names = F, showNA = F)

tracker_better_big_enrollment <- tracker_better %>%
  filter(`Total Days At Most Recent School` >= 360)

tracker_version_3 <- anti_join(tracker_version_2, tracker_better_big_enrollment, by = "TNID")
tracker_version_3 <- tracker_version_3 %>%
  select(
    1:6
  )


# Final Export -----------------------------------------------------------------
write.xlsx(tracker_version_3, "C:/Users/PEREZB/OneDrive - Shelby County Schools - SCS/Desktop/FINAL TRACKER.xlsx", row.names = F, showNA = F)
