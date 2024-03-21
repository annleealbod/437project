# Load packages:
library(readr)
library(table1)
library(kableExtra)
library(readr)
library(tableone)

# Load SAS data:
project_data <- read_csv("project_data.csv")

# Make a subset of the data that has only the variables of interest:
table <- subset(project_data, select = c(AGER, RACE, PARMARR, INTACT18, TOTINCR, HIEDUC, RELRAISD, ATTND14, RELIGION, EVRMARRY, ONOWN18, VRY1STAG, topics_discussed_count, discuss_topic1,
discuss_topic2, discuss_topic3, discuss_topic4, discuss_topic5, discuss_topic6, discuss_topic8, premarital))

# Remove the null values from premarital (due to missing answers):
table <- table[!is.na(table$premarital), ]

# Make variables into factors as necessary:
table$discuss_topic8 <- factor(table$discuss_topic8, levels = c(0,1), labels = c("Taught to Abstain From PS", "Not Taught to Abstain From PS"))
table$discuss_topic1 <- factor(table$discuss_topic1, levels = c(0,1), labels = c("No, Yes"))
table$discuss_topic2 <- factor(table$discuss_topic2, levels = c(0,1), labels = c("No, Yes"))
table$discuss_topic3 <- factor(table$discuss_topic3, levels = c(0,1), labels = c("No, Yes"))
table$discuss_topic4 <- factor(table$discuss_topic4, levels = c(0,1), labels = c("No, Yes"))
table$discuss_topic5 <- factor(table$discuss_topic5, levels = c(0,1), labels = c("No, Yes"))
table$discuss_topic6 <- factor(table$discuss_topic6, levels = c(0,1), labels = c("No, Yes"))

# Label variables:
label(table$AGER) <- "Age"
label(table$RACE) <- "Race"
label(table$PARMARR) <- "Parents Married"
label(table$INTACT18) <- "Lived in Intact Family From 0-18"
label(table$TOTINCR) <- "Family Income"
label(table$HIEDUC) <- "Highest Level of Education Achieved"
label(table$RELRAISD) <- "Religion Raised In"
label(table$ATTND14) <- "Frequency of Church Attendance at 14"
label(table$RELIGION) <- "Current Religious Affiliation"
label(table$EVRMARRY) <- "Ever Been Married"
label(table$ONOWN18) <- "Lived on Their Own Before 18"
label(table$VRY1STAG) <- "Age at First Intercourse"
label(table$topics_discussed_count) <- "How Many Sexual Topics Taught"
label(table$discuss_topic1) <- "Taught How to Say No to Sex"
label(table$discuss_topic2) <- "Taught Methods of Birth Control"
label(table$discuss_topic3) <- "Taught Where to Get Birth Control"
label(table$discuss_topic4) <- "Taught About Sexually Transmitted Diseases"
label(table$discuss_topic5) <- "Taught How to Prevent HIV/AIDS"
label(table$discuss_topic6) <- "Taught How to Use a Condom"

# Actually build the Table 1:
my_table <- table1(~ AGER + RACE + PARMARR + INTACT18 + TOTINCR + HIEDUC + RELRAISD + ATTND14 + RELIGION + EVRMARRY + ONOWN18 + VRY1STAG + topics_discussed_count + discuss_topic1 +
discuss_topic2 + discuss_topic3 + discuss_topic4 + discuss_topic5 + discuss_topic6 | discuss_topic8, data = table)
