# README -----------------------------------------------------------------------
## The goal of this project is to add percentile and demographic data to our existing Tennessee Comprehensive Assessment Program (TCAP) & End of Course testing (EOC) student-level file.

### Tables were provided by the Tennessee Department of Education (TDOE) which give percentile cut scores; One table per TCAP subject area, plus one for EOC.
### These tables will need to be further split into one table per grade per subject.
### These tables also include duplicate percentile cut scores (ex. percentile 9 and percentile 10 may both be reached by scoring a 280), so duplicates will need to be removed, favoring the higher percentile.
### Students who score lower than the cut for the first percentile will be rounded up to the first percentile.

### Demographic files were also provided by the TDOE.
### My colleague, Becky, worked these and provided me with two files, each with one row per student.


## Sensitive information has been edited out of this file where applicable.


# Setup ------------------------------------------------------------------------
options(java.parameters = "-Xmx16g")

library(tidyverse)
library(xlsx)


# TCAP -------------------------------------------------------------------------
# I manually separated the Excel file by subject because of its size.
tcap_ela <- read.xlsx2("TCAP & EOC Student Level_Percentiles and Demos 7.16.24.xlsx", 2, check.names = F)
tcap_math <- read.xlsx2("TCAP & EOC Student Level_Percentiles and Demos 7.16.24.xlsx", 3, check.names = F)
tcap_sci <- read.xlsx2("TCAP & EOC Student Level_Percentiles and Demos 7.16.24.xlsx", 4, check.names = F)
tcap_ss <- read.xlsx2("TCAP & EOC Student Level_Percentiles and Demos 7.16.24.xlsx", 5, check.names = F)

# I had to change the name of the columns to be more R friendly. I named them: percentile, ela3, ela4, etc.
k8_ela <- read.xlsx("Ach 2024 Percentile.xlsx", 1, startRow = 2)
k8_math <- read.xlsx("Ach 2024 Percentile.xlsx", 2, startRow = 2)
k8_sci <- read.xlsx("Ach 2024 Percentile.xlsx", 3, startRow = 2)
k8_ss <- read.xlsx("Ach 2024 Percentile.xlsx", 4, startRow = 2)


## Percentile Lists By subject & Grade -----------------------------------------

## ELA
ela_colnames <- colnames(k8_ela[2:ncol(k8_ela)])
ela_list <- lapply(ela_colnames, function(col){
  k8_ela %>%
    select(
      percentile,
      all_of(col)
    ) %>%
    group_by(
      across(all_of(col))
    ) %>%
    arrange(
      desc(percentile),
      .by_group = T
    ) %>%
    slice(1) %>%
    ungroup()
  }
)
names(ela_list) <- ela_colnames


## Math
math_colnames <- colnames(k8_math[2:ncol(k8_math)])
math_list <- lapply(math_colnames, function(col){
  k8_math %>%
    select(
      percentile,
      all_of(col)
    ) %>%
    group_by(
      across(all_of(col))
    ) %>%
    arrange(
      desc(percentile),
      .by_group = T
    ) %>%
    slice(1) %>%
    ungroup()
  }
)
names(math_list) <- math_colnames


## Science
sci_colnames <- colnames(k8_sci[2:ncol(k8_sci)])
sci_list <- lapply(sci_colnames, function(col){
  k8_sci %>%
    select(
      percentile,
      all_of(col)
    ) %>%
    group_by(
      across(all_of(col))
    ) %>%
    arrange(
      desc(percentile),
      .by_group = T
    ) %>%
    slice(1) %>%
    ungroup()
  }
)
names(sci_list) <- sci_colnames


## Social
ss_colnames <- colnames(k8_ss[2:ncol(k8_ss)])
ss_list <- lapply(ss_colnames, function(col){
  k8_ss %>%
    select(
      percentile,
      all_of(col)
    ) %>%
    group_by(
      across(all_of(col))
    ) %>%
    arrange(
      desc(percentile),
      .by_group = T
    ) %>%
    slice(1) %>%
    ungroup()
  }
)
names(ss_list) <- ss_colnames


## Subject Lists by Grade ------------------------------------------------------
stu_ela_list <- split(tcap_ela, tcap_ela$`Test Grade`)
names(stu_ela_list) <- ela_colnames

stu_math_list <- split(tcap_math, tcap_math$`Test Grade`)
names(stu_math_list) <- math_colnames

stu_sci_list <- split(tcap_sci, tcap_sci$`Test Grade`)
names(stu_sci_list) <- sci_colnames

stu_ss_list <- split(tcap_ss, tcap_ss$`Test Grade`)
names(stu_ss_list) <- ss_colnames


## Breaks & Labels -------------------------------------------------------------

## ELA

# Adding Breaks
ela_breaks <- lapply(
  ela_list, function(df){
    c(df[[2]], 1000)
  }
)

# Adding Labels
ela_labels <- lapply(
  ela_list, function(df){
    c(df[[1]])
  }
)


## Math

# Adding Breaks
math_breaks <- lapply(
  math_list, function(df){
    c(df[[2]], 1000)
  }
)

# Adding Labels
math_labels <- lapply(
  math_list, function(df){
    c(df[[1]])
  }
)


## Science

# Adding Breaks
sci_breaks <- lapply(
  sci_list, function(df){
    c(df[[2]], 1000)
  }
)

# Adding Labels
sci_labels <- lapply(
  sci_list, function(df){
    c(df[[1]])
  }
)


## Social

# Adding Breaks
ss_breaks <- lapply(
  ss_list, function(df){
    c(df[[2]], 1000)
  }
)

# Adding Labels
ss_labels <- lapply(
  ss_list, function(df){
    c(df[[1]])
  }
)


## Applying Cut Scores ---------------------------------------------------------

## ELA
ela_stu_perc_list <- Map(function(df, breaks, labels) {
  if (is.data.frame(df) && "Scale Score" %in% names(df)) {
    df$Percentile <- cut(as.numeric(df$`Scale Score`), breaks = breaks, labels = labels, right = FALSE)
    df$Percentile[is.na(df$Percentile) & df$`Scale Score` < breaks[1]] <- 1
  } else {
    stop("Each element in stu_ela_list must be a data frame with a 'Scale Score' column")
  }
  return(df)
}, stu_ela_list, ela_breaks, ela_labels)


## Math

math_stu_perc_list <- Map(function(df, breaks, labels) {
  if (is.data.frame(df) && "Scale Score" %in% names(df)) {
    df$Percentile <- cut(as.numeric(df$`Scale Score`), breaks = breaks, labels = labels, right = FALSE)
    df$Percentile[is.na(df$Percentile) & df$`Scale Score` < breaks[1]] <- 1
  } else {
    stop("Each element in stu_math_list must be a data frame with a 'Scale Score' column")
  }
  return(df)
}, stu_math_list, math_breaks, math_labels)


## Science

sci_stu_perc_list <- Map(function(df, breaks, labels) {
  if (is.data.frame(df) && "Scale Score" %in% names(df)) {
    df$Percentile <- cut(as.numeric(df$`Scale Score`), breaks = breaks, labels = labels, right = FALSE)
    df$Percentile[is.na(df$Percentile) & df$`Scale Score` < breaks[1]] <- 1
  } else {
    stop("Each element in stu_sci_list must be a data frame with a 'Scale Score' column")
  }
  return(df)
}, stu_sci_list, sci_breaks, sci_labels)


## Social

ss_stu_perc_list <- Map(function(df, breaks, labels) {
  if (is.data.frame(df) && "Scale Score" %in% names(df)) {
    df$Percentile <- cut(as.numeric(df$`Scale Score`), breaks = breaks, labels = labels, right = FALSE)
    df$Percentile[is.na(df$Percentile) & df$`Scale Score` < breaks[1]] <- 1
  } else {
    stop("Each element in stu_ss_list must be a data frame with a 'Scale Score' column")
  }
  return(df)
}, stu_ss_list, ss_breaks, ss_labels)


## Combining Lists -------------------------------------------------------------
ela_stu_perc_all <- bind_rows(ela_stu_perc_list)
math_stu_perc_all <- bind_rows(math_stu_perc_list)
sci_stu_perc_all <- bind_rows(sci_stu_perc_list)
ss_stu_perc_all <- bind_rows(ss_stu_perc_list)

stu_perc <- bind_rows(ela_stu_perc_all, math_stu_perc_all, sci_stu_perc_all, ss_stu_perc_all)


## Exporting -------------------------------------------------------------------
# Optional stop to export TCAP data alone.
#write.xlsx2(stu_perc, "TCAP K8 Student Level Percentiles Added.xlsx", row.names = F)



# EOC --------------------------------------------------------------------------
eoc_b1 <- read.xlsx2("TCAP & EOC Student Level_Percentiles and Demos 7.16.24.xlsx", 6, check.names = F)
eoc_e1 <- read.xlsx2("TCAP & EOC Student Level_Percentiles and Demos 7.16.24.xlsx", 7, check.names = F)
eoc_e2 <- read.xlsx2("TCAP & EOC Student Level_Percentiles and Demos 7.16.24.xlsx", 8, check.names = F)
eoc_ush <- read.xlsx2("TCAP & EOC Student Level_Percentiles and Demos 7.16.24.xlsx", 9, check.names = F)
eoc_cut_table <- read.xlsx2("EOC Spring 2024 Percentile (B1, E1, E2, and USH).xlsx", 1)


eoc_sub_list <- list(eoc_b1, eoc_e1, eoc_e2, eoc_ush)
names(eoc_sub_list) <- eoc_colnames


## Percentile Lists by Subject -------------------------------------------------
eoc_colnames <- colnames(eoc_cut_table[2:ncol(eoc_cut_table)])
eoc_list <- lapply(eoc_colnames, function(col){
  eoc_cut_table %>%
    select(
      percentile,
      all_of(col)
    ) %>%
    group_by(
      across(all_of(col))
    ) %>%
    arrange(
      desc(percentile),
      .by_group = T
    ) %>%
    slice(1) %>%
    ungroup()
  }
)
names(eoc_list) <- eoc_colnames

## Breaks & Labels -------------------------------------------------------------

# Adding Breaks
eoc_breaks <- lapply(
  eoc_list, function(df){
    c(df[[2]], 1000)
  }
)

# Adding Labels
eoc_labels <- lapply(
  eoc_list, function(df){
    c(df[[1]])
  }
)


## Applying Cut Scores ---------------------------------------------------------

eoc_stu_perc_list <- Map(function(df, breaks, labels) {
  if (is.data.frame(df) && "Scale Score" %in% names(df)) {
    df$Percentile <- cut(as.numeric(df$`Scale Score`), breaks = breaks, labels = labels, right = FALSE)
    df$Percentile[is.na(df$Percentile) & df$`Scale Score` < breaks[1]] <- 1
  } else {
    stop("Each element in eoc_sub_list must be a data frame with a 'Scale Score' column")
  }
  return(df)
}, eoc_sub_list, eoc_breaks, eoc_labels)


## Combining Lists -------------------------------------------------------------
eoc_stu_perc_all <- bind_rows(eoc_stu_perc_list)

## Exporting -------------------------------------------------------------------
# Optional stop to export EOC data alone.
#write.xlsx2(eoc_stu_perc_all, "EOC Student Level Percentiles Only.xlsx", row.names = F)



# Combining TCAP, EOC, and Demographics ----------------------------------------

# Read in TCAP and EOC exported files if beginning here.
#stu_perc <- read.xlsx2("TCAP K8 Student Level Percentiles Added.xlsx", 1, check.names = F)
#eoc_stu_perc_all <- read.xlsx2("EOC Student Level Percentiles Only.xlsx", 1, check.names = F)

# Demographic files, courtesy of Becky.
stu_demos_1 <- read.xlsx2("TCAP & EOC Demographics 1.xlsx", 1, check.names = F)
stu_demos_2 <- read.xlsx2("TCAP & EOC Demographics 2.xlsx", 1, check.names = F)


## Combining Percentile Files & Combining Demographics Files -------------------
all_percentiles <- bind_rows(stu_perc, eoc_stu_perc_all)
all_demos <- bind_rows(stu_demos_1, stu_demos_2)

# The two demo files had students in common, so those need to be filtered out.
all_demos_cut <- all_demos %>%
  group_by(
    USID
  ) %>%
  slice(1) %>%
  ungroup()


## Merging Percentiles & Demographics ------------------------------------------
all_perc_demos <- merge(all_percentiles, all_demos_cut, by.x = "TNID", by.y = "USID", all.x = T)

# Re-Ordering File
all_perc_demos_formatted <- all_perc_demos %>%
  select(
    2:6,
    TNID,
    7:13,
    as.numeric(14:18),
    19:30
  )


## Exporting -------------------------------------------------------------------
write.xlsx2(all_perc_demos_formatted, paste0("TCAP & EOC Percentiles & Demographics ", Sys.Date(), ".xlsx"), row.names = F)
