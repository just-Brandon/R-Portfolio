# README -----------------------------------------------------------------------

## The purpose of this project is to create a digital student calendar, including all student holidays, going back to the 2020-2021 school year.
## I have added optional stopping points which can create a calendar for each school year, and the functions on lines 83-85 will create one all-encompassing calendar.
## Once the calendar has been created, the function on line 87 can be used to easily calculate a student's enrollment by entering their enter and exit dates.

### This script is called and run within several other projects, including "Grad Cohort 60-Day Appeal Finder.R" (also included in this portfolio).


#install.packages(bizdays)
library(bizdays)

# 2020-2021 Student Calendar ---------------------------------------------------
beforeschool2021 <- seq.Date(as.Date("2020-07-01"), as.Date("2020-08-28"), by = "day")
afterschool2021 <- seq.Date(as.Date("2021-06-17"), as.Date("2021-06-30"), by = "day")
fall2021 <- seq.Date(as.Date("2020-10-12"), as.Date("2020-10-16"), by = "day")
thanksgiving2021 <- seq.Date(as.Date("2020-11-23"), as.Date("2020-11-27"), by = "day")
winter2021 <- seq.Date(as.Date("2020-12-21"), as.Date("2021-01-01"), by = "day")
spring2021 <- seq.Date(as.Date("2021-03-29"), as.Date("2021-04-02"), by = "day")
other2021 <- as.Date(c("2020-09-07", "2020-11-03", "2020-11-11", "2021-01-18", "2021-02-05", "2021-02-15", "2021-04-16"))
holidays2021 <- c(beforeschool2021, afterschool2021, fall2021, thanksgiving2021, winter2021, spring2021, other2021)

# sy2021 <- create.calendar("SY 20-21", holidays = holidays2021, weekdays = c("saturday", "sunday"), start.date = "2020-07-01", end.date = "2021-06-30")
# bizdays(as.Date("yyyy-mm-dd"), as.Date("yyyy-mm-dd"), sy2021)+1


# 2021-2022 Student Calendar ---------------------------------------------------
beforeschool2122 <- seq.Date(as.Date("2021-07-01"), as.Date("2021-08-06"), by = "day")
afterschool2122 <- seq.Date(as.Date("2022-05-30"), as.Date("2022-06-30"), by = "day")
fall2122 <- seq.Date(as.Date("2021-10-11"), as.Date("2021-10-15"), by = "day")
thanksgiving2122 <- seq.Date(as.Date("2021-11-22"), as.Date("2021-11-26"), by = "day")
winter2122 <- seq.Date(as.Date("2021-12-20"), as.Date("2021-12-31"), by = "day")
spring2122 <- seq.Date(as.Date("2022-03-14"), as.Date("2022-03-18"), by = "day")
other2122 <- as.Date(c("2021-09-06", "2021-11-11", "2022-01-17", "2022-02-21", "2022-04-15"))
holidays2122 <- c(beforeschool2122, afterschool2122, fall2122, thanksgiving2122, winter2122, spring2122, other2122)

# sy2122 <- create.calendar("SY 21-22", holidays = holidays2122, weekdays = c("saturday", "sunday"), start.date = "2021-07-01", end.date = "2022-06-30")
# bizdays(as.Date("yyyy-mm-dd"), as.Date("yyyy-mm-dd"), sy2122)+1


# 2022-2023 Student Calendar ---------------------------------------------------
beforeschool2223 <- seq.Date(as.Date("2022-07-01"), as.Date("2022-08-05"), by = "day")
afterschool2223 <- seq.Date(as.Date("2023-05-29"), as.Date("2023-06-30"), by = "day")
fall2223 <- seq.Date(as.Date("2022-10-10"), as.Date("2022-10-14"), by = "day")
thanksgiving2223 <- seq.Date(as.Date("2022-11-21"), as.Date("2022-11-25"), by = "day")
winter2223 <- seq.Date(as.Date("2022-12-19"), as.Date("2022-12-30"), by = "day")
spring2223 <- seq.Date(as.Date("2023-03-13"), as.Date("2023-03-17"), by = "day")
other2223 <- as.Date(c("2022-09-05", "2022-11-08", "2023-11-11", "2023-01-16", "2023-03-07"))
holidays2223 <- c(beforeschool2223, afterschool2223, fall2223, thanksgiving2223, winter2223, spring2223, other2223)

# sy2223 <- create.calendar("SY 22-23", holidays = holidays2223, weekdays = c("saturday", "sunday"), start.date = "2022-07-01", end.date = "2023-06-30")
# bizdays(as.Date("yyyy-mm-dd"), as.Date("yyyy-mm-dd"), sy2223)+1


# 2023-2024 Student Calendar ---------------------------------------------------
beforeschool2324 <- seq.Date(as.Date("2023-07-03"), as.Date("2023-08-04"), by = "day")
afterschool2324 <- seq.Date(as.Date("2024-05-27"), as.Date("2024-06-28"), by = "day")
fall2324 <- seq.Date(as.Date("2023-10-09"), as.Date("2023-10-13"), by = "day")
thanksgiving2324 <- seq.Date(as.Date("2023-11-20"), as.Date("2023-11-24"), by = "day")
winter2324 <- seq.Date(as.Date("2023-12-21"), as.Date("2024-01-03"), by = "day")
spring2324 <- seq.Date(as.Date("2024-03-11"), as.Date("2024-03-15"), by = "day")
other2324 <- as.Date(c("2023-09-04", "2023-11-10", "2024-01-15", "2024-02-19", "2024-03-29"))
holidays2324 <- c(beforeschool2324, afterschool2324, fall2324, thanksgiving2324, winter2324, spring2324, other2324)

# sy2324 <- create.calendar("SY 23-24", holidays = holidays2324, weekdays = c("saturday", "sunday"), start.date = "2023-07-03", end.date = "2024-06-28")
# bizdays(as.Date("yyyy-mm-dd"), as.Date("yyyy-mm-dd"), sy2324)+1


# 2024-2025 Student Calendar ---------------------------------------------------
beforeschool2425 <- seq.Date(as.Date("2024-07-01"), as.Date("2024-08-02"), by = "day")
afterschool2425 <- seq.Date(as.Date("2025-05-26"), as.Date("2025-06-30"), by = "day")
fall2425 <- seq.Date(as.Date("2024-10-07"), as.Date("2024-10-11"), by = "day")
thanksgiving2425 <- seq.Date(as.Date("2024-11-25"), as.Date("2024-11-29"), by = "day")
winter2425 <- seq.Date(as.Date("2024-12-23"), as.Date("2025-01-03"), by = "day")
spring2425 <- seq.Date(as.Date("2025-03-10"), as.Date("2025-03-14"), by = "day")
other2425 <- as.Date(c("2024-09-02", "2024-11-05", "2024-11-11", "2025-01-20", "2025-02-17", "2025-04-18"))
holidays2425 <- c(beforeschool2425, afterschool2425, fall2425, thanksgiving2425, winter2425, spring2425, other2425)

# sy2425 <- create.calendar("SY 24-25", holidays = holidays2425, weekdays = c("saturday", "sunday"), start.date = "2024-07-03", end.date = "2025-06-28")
# bizdays(as.Date("yyyy-mm-dd"), as.Date("yyyy-mm-dd"), sy2425)+1


# Total Student Calendar -------------------------------------------------------
allholidays <- unique(do.call(c, mget(ls(pattern = "^holiday"))))
stcal <- create.calendar("Total Student Calendar", holidays = allholidays, weekdays = c("saturday", "sunday"))

#bizdays(as.Date("yyyy-mm-dd"), as.Date("yyyy-mm-dd"), stcal)+1
