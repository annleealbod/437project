# Load packages:
library(readr)
library(table1)
library(kableExtra)
library(readr)
library(tableone)

# Load SAS data:
project_data <- read_csv("project_data.csv")

# Make a subset of the data that has only the variables of interest:
subdata <- subset(project_data, select = c(AGER, RACE, PARMARR, INTACT18, TOTINCR, HIEDUC, RELRAISD, ATTND14, RELIGION, EVRMARRY, ONOWN18, VRY1STAG, topics_discussed_count, discuss_topic1,
discuss_topic2, discuss_topic3, discuss_topic4, discuss_topic5, discuss_topic6, discuss_topic8, premarital))

# Remove the null values from premarital (due to missing answers):
subdata <- subdata[!is.na(subdata$premarital), ]

# Setting up a render function for logical variables to save space:
rndr <- function(x, ...) {
  y <- render.default(x, ...)
  if (is.logical(x)) y[2] else y
}

# Make variables into factors/logicals as necessary:
subdata$discuss_topic8 <- factor(subdata$discuss_topic8, levels = c(0,1), labels = c("Taught to Abstain From PS", "Not Taught to Abstain From PS"))
subdata$discuss_topic1 <- as.logical(subdata$discuss_topic1)
subdata$discuss_topic2 <- as.logical(subdata$discuss_topic2)
subdata$discuss_topic3 <- as.logical(subdata$discuss_topic3)
subdata$discuss_topic4 <- as.logical(subdata$discuss_topic4)
subdata$discuss_topic5 <- as.logical(subdata$discuss_topic5)
subdata$discuss_topic6 <- as.logical(subdata$discuss_topic6)
subdata$premarital <- as.logical(subdata$premarital)

# Reorder certain variable factor levels:
inc_order <- c("Under $5000", "$5000-$7499", "$7500-$9999", "$10,000-$12,499", "$12,500-$14,999", "$15,000-$19,999", 
"$20,000-$24,999", "$25,000-$29,999", "$30,000-$34,999", "$35,000-$39,999", "$40,000-$49,999", "$50,000-$59,999", "$60,000-$74,999",
"$75,000-$99,999", "$100,000 or more")
subdata$TOTINCR <- factor(subdata$TOTINCR, levels = inc_order)

race_order <- c("WHITE", "BLACK", "OTHER")
subdata$RACE <- factor(subdata$RACE, levels = race_order)

parmar_order <- c("Yes", "No", "Don't Know", "Refused")
subdata$PARMARR <- factor(subdata$PARMARR, levels = parmar_order)

intact_order <- c("Yes", "No", "Don't Know")
subdata$INTACT18 <- factor(subdata$INTACT18, levels = intact_order)

educ_order <- c("9TH GRADE OR LESS", "10TH GRADE", "11TH GRADE", "12TH GRADE, NO DIPLOMA (NOR GED)", "HIGH SCHOOL GRADUATE (DIPLOMA OR GED)",
"SOME COLLEGE BUT NO DEGREE", "ASSOCIATE DEGREE IN COLLEGE/UNIVERSITY", "BACHELOR'S DEGREE", "MASTER'S DEGREE", "DOCTORATE DEGREE")
subdata$HIEDUC <- factor(subdata$HIEDUC, levels = educ_order)

rel_order <- c("Catholic", "Baptist/Southern Baptist", "Methodist, Lutheran, Presbyterian, Episcopal", "Fundamentalist Protestant",
"Other Protestant denomination","Protestant - No specific denomination", "Other religion", "No religion", "Don't know", "Refused")
subdata$RELRAISD <- factor(subdata$RELRAISD, levels = rel_order)

chufreq_order <- c("More than once a week", "Once a week", "2-3 times a month", "Once a month (about 12 times a year)", "3-11 times a year", "Once or twice a year",
"Never", "Refused", "Missing")
subdata$ATTND14 <- factor(subdata$ATTND14, levels = chufreq_order)



# Label variables:
label(subdata$AGER) <- "Age at Time of Survey"
label(subdata$RACE) <- "Race"
label(subdata$PARMARR) <- "Parents Married at Birth"
label(subdata$INTACT18) <- "Lived in Intact Family From 0-18"
label(subdata$TOTINCR) <- "Family Income"
label(subdata$HIEDUC) <- "Highest Level of Education Achieved"
label(subdata$RELRAISD) <- "Religion Raised In"
label(subdata$ATTND14) <- "Frequency of Church Attendance at 14"
label(subdata$RELIGION) <- "Current Religious Affiliation"
label(subdata$EVRMARRY) <- "Ever Been Married"
label(subdata$ONOWN18) <- "Lived on Their Own Before 18"
label(subdata$VRY1STAG) <- "Age at First Intercourse"
label(subdata$topics_discussed_count) <- "How Many Sexual Topics Taught"
label(subdata$discuss_topic1) <- "Taught How to Say No to Sex"
label(subdata$discuss_topic2) <- "Taught Methods of Birth Control"
label(subdata$discuss_topic3) <- "Taught Where to Get Birth Control"
label(subdata$discuss_topic4) <- "Taught About Sexually Transmitted Diseases"
label(subdata$discuss_topic5) <- "Taught How to Prevent HIV/AIDS"
label(subdata$discuss_topic6) <- "Taught How to Use a Condom"
label(subdata$premarital) <- "Had Premarital Sex"

# Actually build the Table 1:
my_table <- table1(~ AGER + RACE + PARMARR + INTACT18 + TOTINCR + HIEDUC + RELRAISD + ATTND14 + EVRMARRY + ONOWN18 + VRY1STAG + topics_discussed_count + discuss_topic1 +
discuss_topic2 + discuss_topic3 + discuss_topic4 + discuss_topic5 + discuss_topic6 | discuss_topic8, data = subdata, render = rndr, render.continuous=c("Mean (SD)"="MEAN (SD)"))
