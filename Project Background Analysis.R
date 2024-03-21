# Load packages
library(readr)
library(table1)
library(kableExtra)
library(readr)
library(tableone)

# Load SAS data
project_data <- read_csv("project_data.csv")

# Make a subset of the data that has only the variables of interest:
table <- subset(project_data, select = c(AGER, RACE, PARMARR, INTACT18, TOTINCR, HIEDUC, RELRAISD, ATTND14, discuss_topic8, premarital))

# Remove the null values from premarital (due to missing answers)
table <- table[!is.na(table$premarital), ]

# Make variables into factors as necessary
table$discuss_topic8 <- factor(table$discuss_topic8, levels = c(0,1), labels = c("Discussed Abstinence from Premarital Sex", "Did Not Discuss Abstinence from Premarital Sex"))
table$premarital <- factor(table$premarital, levels = c(0,1), labels = c("No", "Yes"))

# Label variables:
label(table$AGER) <- "Age"
label(table$RACE) <- "Race"
label(table$PARENTSMARR) <- "Parents Married"
label(table$INTACT18) <- "Lived in Intact Family From 0-18"
label(table$TOTINCR) <- "Family Income"
label(table$HIEDUC) <- "Highest Level of Education Achieved"
label(table$RELRAISD) <- "Religion Raised In"
label(table$ATTND14) <- "Frequency of Church Attendance at 14"

# Actually build the Table 1
my_table <- table1(~ AGER + RACE + PARMARR + INTACT18 + TOTINCR + HIEDUC + RELRAISD + ATTND14 | discuss_topic8, data = table)