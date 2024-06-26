# Load packages:
library(readr)
library(table1)
library(kableExtra)
library(readr)
library(tableone)
library(ggplot2)
library(kableExtra)
library(gridExtra)
library(webshot2)
library(tidyverse)
library(MatchIt)
library(Matching)
library(tools)


# Load SAS data:
project_data <- read.csv("project_data.csv")

# Remove rows with NAs in the outcome variable (premarital)
project_data <- project_data %>% drop_na(premarital)

# Remove rows with missing values in discuss_topic8
project_data <- project_data %>% drop_na(discuss_topic8)

# Make a subset of the data that has only the variables of interest:
subdata <- subset(project_data, select = c(AGER, RACE, PARMARR, INTACT18, TOTINCR, HIEDUC, RELRAISD, ATTND14, RELIGION, EVRMARRY, ONOWN18, VRY1STAG, topics_discussed_count, discuss_topic1,
discuss_topic2, discuss_topic3, discuss_topic4, discuss_topic5, discuss_topic6, discuss_topic8, premarital))

# Remove the null values from premarital (due to missing answers):
subdata <- subdata[!is.na(subdata$premarital), ]

# Filter out the nevers and refused
subdata <- subset(subdata, !(RELRAISD %in% c("Don't know", "Refused")))
subdata <- subset(subdata, ATTND14 != "Refused")
subdata <- subset(subdata, ATTND14 != "Missing")

# Setting up a render function for logical variables to save space:
rndr <- function(x, ...) {
  y <- render.default(x, ...)
  if (is.logical(x)) y[2] else y
}

# Make variables into factors/logicals as necessary:
subdata$discuss_topic8 <- factor(subdata$discuss_topic8, levels = c(0,1), labels = c("Taught Abstinence", "Not Taught Abstinence"))
subdata$discuss_topic1 <- as.logical(subdata$discuss_topic1)
subdata$discuss_topic2 <- as.logical(subdata$discuss_topic2)
subdata$discuss_topic3 <- as.logical(subdata$discuss_topic3)
subdata$discuss_topic4 <- as.logical(subdata$discuss_topic4)
subdata$discuss_topic5 <- as.logical(subdata$discuss_topic5)
subdata$discuss_topic6 <- as.logical(subdata$discuss_topic6)
subdata$premarital <- as.logical(subdata$premarital)
subdata$EVRMARRY <- as.logical(subdata$EVRMARRY == "EVER MARRIED")
subdata$ONOWN18 <- as.logical(subdata$ONOWN18 == "Yes")
subdata$PARMARR <- as.logical(subdata$PARMARR == "Yes")
subdata$INTACT18 <- as.logical(subdata$INTACT18 == "Yes")



# Reorder certain variable factor levels:
inc_order <- c("Under $5000", "$5000-$7499", "$7500-$9999", "$10,000-$12,499", "$12,500-$14,999", "$15,000-$19,999", 
"$20,000-$24,999", "$25,000-$29,999", "$30,000-$34,999", "$35,000-$39,999", "$40,000-$49,999", "$50,000-$59,999", "$60,000-$74,999",
"$75,000-$99,999", "$100,000 or more")
subdata$TOTINCR <- factor(subdata$TOTINCR, levels = inc_order)

race_order <- c("WHITE", "BLACK", "OTHER")
subdata$RACE <- factor(subdata$RACE, levels = race_order)
levels(subdata$RACE) <- toTitleCase(tolower(levels(subdata$RACE)))

educ_order <- c("9TH GRADE OR LESS", "10TH GRADE", "11TH GRADE", "12TH GRADE, NO DIPLOMA (NOR GED)", "HIGH SCHOOL GRADUATE (DIPLOMA OR GED)",
"SOME COLLEGE BUT NO DEGREE", "ASSOCIATE DEGREE IN COLLEGE/UNIVERSITY", "BACHELOR'S DEGREE", "MASTER'S DEGREE", "DOCTORATE DEGREE")
subdata$HIEDUC <- factor(subdata$HIEDUC, levels = educ_order)
# Rename the levels to lowercase
levels(subdata$HIEDUC) <- toTitleCase(tolower(levels(subdata$HIEDUC)))

rel_order <- c("Catholic", "Baptist/Southern Baptist", "Methodist, Lutheran, Presbyterian, Episcopal", "Fundamentalist Protestant",
"Other Protestant denomination","Protestant - No specific denomination", "Other religion", "No religion")
subdata$RELRAISD <- factor(subdata$RELRAISD, levels = rel_order)

chufreq_order <- c("More than once a week", "Once a week", "2-3 times a month", "Once a month (about 12 times a year)", "3-11 times a year", "Once or twice a year")
subdata$ATTND14 <- factor(subdata$ATTND14, levels = chufreq_order)

age_order <- c("15 YEARS", "16 YEARS", "17 YEARS", "18 YEARS", "19 YEARS", "20 YEARS", "21 YEARS", "22 YEARS", "23 YEARS", "24 YEARS", "25 YEARS")
subdata$AGER <- factor(subdata$AGER, levels = age_order)
levels(subdata$AGER) <- tolower(levels(subdata$AGER))

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
label(subdata$EVRMARRY) <- "Have Been Married"
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
table_one <- table1(~ AGER + RACE + PARMARR + INTACT18 + TOTINCR + HIEDUC + RELRAISD + ATTND14 + EVRMARRY + ONOWN18 + VRY1STAG + topics_discussed_count + discuss_topic1 +
discuss_topic2 + discuss_topic3 + discuss_topic4 + discuss_topic5 + discuss_topic6 | discuss_topic8, data = subdata, render = rndr, render.continuous=c("Mean (SD)"="MEAN (SD)"))

table_one

# Now on to evidence of association:
# Create a 2x2 matrix
table_counts <- table(subdata$discuss_topic8, subdata$premarital)

matrix_counts <- as.matrix(table_counts)

# Add row and column names for clarity
rownames(matrix_counts) <- c("Not Taught", "Taught")
colnames(matrix_counts) <- c("No Premarital Sex", "Premarital Sex")

# Display the matrix
matrix_counts


# Performing a fisher exact test:
fisher.test(matrix_counts)


# Propensity scores, matching, and outcome analysis:

# Fit a propensity score model using logistic regression
psmodel <- glm(discuss_topic8 ~ RELRAISD + ATTND14 + discuss_topic1 + discuss_topic2 +
 discuss_topic3 + discuss_topic4 + discuss_topic5 + discuss_topic6, 
               family = binomial(), data = project_data)

# Show coefficients etc
summary(psmodel)

# compare treatement to other variables
# Create propensity score
pscore <- psmodel$fitted.values

# Check overlap
histdat <- tibble(pscore = pscore, 
                  treatment = recode(project_data$discuss_topic8, 
                                     '0' = 'No Treatment', 
                                     '1' = 'Treatment'))
g <- ggplot(histdat, aes(x = pscore)) +
  geom_histogram(color = "black", fill = "white") + 
  facet_grid(treatment ~ .) +  # Facet by treatment variable
  theme(legend.position = "none")
g


#exclude any individuals in treated group with propensity score
#greater than the max in the control group
max(pscore[project_data$discuss_topic8==0])
max(pscore[project_data$discuss_topic8==1])

#exclude any individuals in control group with propensity score 
#smaller than the min in the treated group
min(pscore[project_data$discuss_topic8==0])
min(pscore[project_data$discuss_topic8==1])

# Now to calculate how many were excluded:

# Calculate the maximum propensity score in the control group
max_control <- max(pscore[project_data$discuss_topic8 == 0])

# Calculate the minimum propensity score in the treated group
min_treated <- min(pscore[project_data$discuss_topic8 == 1])

# Count the number of individuals in the treated group with propensity score greater than max_control
excluded_treated <- sum(pscore[project_data$discuss_topic8 == 1] > max_control)

# Count the number of individuals in the control group with propensity score smaller than min_treated
excluded_control <- sum(pscore[project_data$discuss_topic8 == 0] < min_treated)

# Report the number of excluded individuals (HOW DO I INCLUDE THIS IN THE REPORT? QMD or JUST WRITE?)
cat("Number of individuals excluded from the treated group:", excluded_treated, "\n")
cat("Number of individuals excluded from the control group:", excluded_control, "\n")


#matching WITHOUT caliper
logit <- function(p){log(p)-log(1-p)}
psmatch1 <- Match(Tr=project_data$discuss_topic8, M=1, X=logit(pscore), replace=FALSE)
matched1 <- project_data[c(psmatch1$index.treated, psmatch1$index.control),]

xvars <- c("RELRAISD", "ATTND14","discuss_topic1",
"discuss_topic2", "discuss_topic3", "discuss_topic4", "discuss_topic5", "discuss_topic6")

#evaluate matching via table 1 (REMAKING TABLE 1, lots of formatting)
# Filter out the nevers and refused
matched1 <- subset(matched1, !(RELRAISD %in% c("Don't know", "Refused")))
matched1 <- subset(matched1, ATTND14 != "Refused")
matched1 <- subset(matched1, ATTND14 != "Missing")

matched1$discuss_topic8 <- factor(matched1$discuss_topic8, levels = c(0,1), labels = c("Taught Abstinence", "Not Taught Abstinence"))
matched1$discuss_topic1 <- as.logical(matched1$discuss_topic1)
matched1$discuss_topic2 <- as.logical(matched1$discuss_topic2)
matched1$discuss_topic3 <- as.logical(matched1$discuss_topic3)
matched1$discuss_topic4 <- as.logical(matched1$discuss_topic4)
matched1$discuss_topic5 <- as.logical(matched1$discuss_topic5)
matched1$discuss_topic6 <- as.logical(matched1$discuss_topic6)
matched1$premarital <- as.logical(matched1$premarital)
matched1$EVRMARRY <- as.logical(matched1$EVRMARRY == "EVER MARRIED")
matched1$ONOWN18 <- as.logical(matched1$ONOWN18 == "Yes")
matched1$PARMARR <- as.logical(matched1$PARMARR == "Yes")
matched1$INTACT18 <- as.logical(matched1$INTACT18 == "Yes")



# Reorder certain variable factor levels:
inc_order <- c("Under $5000", "$5000-$7499", "$7500-$9999", "$10,000-$12,499", "$12,500-$14,999", "$15,000-$19,999", 
"$20,000-$24,999", "$25,000-$29,999", "$30,000-$34,999", "$35,000-$39,999", "$40,000-$49,999", "$50,000-$59,999", "$60,000-$74,999",
"$75,000-$99,999", "$100,000 or more")
matched1$TOTINCR <- factor(matched1$TOTINCR, levels = inc_order)

race_order <- c("WHITE", "BLACK", "OTHER")
matched1$RACE <- factor(matched1$RACE, levels = race_order)
levels(matched1$RACE) <- toTitleCase(tolower(levels(matched1$RACE)))

educ_order <- c("9TH GRADE OR LESS", "10TH GRADE", "11TH GRADE", "12TH GRADE, NO DIPLOMA (NOR GED)", "HIGH SCHOOL GRADUATE (DIPLOMA OR GED)",
"SOME COLLEGE BUT NO DEGREE", "ASSOCIATE DEGREE IN COLLEGE/UNIVERSITY", "BACHELOR'S DEGREE", "MASTER'S DEGREE", "DOCTORATE DEGREE")
matched1$HIEDUC <- factor(matched1$HIEDUC, levels = educ_order)
# Rename the levels to lowercase
levels(matched1$HIEDUC) <- toTitleCase(tolower(levels(matched1$HIEDUC)))

rel_order <- c("Catholic", "Baptist/Southern Baptist", "Methodist, Lutheran, Presbyterian, Episcopal", "Fundamentalist Protestant",
"Other Protestant denomination","Protestant - No specific denomination", "Other religion", "No religion")
matched1$RELRAISD <- factor(matched1$RELRAISD, levels = rel_order)

chufreq_order <- c("More than once a week", "Once a week", "2-3 times a month", "Once a month (about 12 times a year)", "3-11 times a year", "Once or twice a year")
matched1$ATTND14 <- factor(matched1$ATTND14, levels = chufreq_order)

age_order <- c("15 YEARS", "16 YEARS", "17 YEARS", "18 YEARS", "19 YEARS", "20 YEARS", "21 YEARS", "22 YEARS", "23 YEARS", "24 YEARS", "25 YEARS")
matched1$AGER <- factor(matched1$AGER, levels = age_order)
levels(matched1$AGER) <- tolower(levels(matched1$AGER))

# Label variables:
label(matched1$AGER) <- "Age at Time of Survey"
label(matched1$RACE) <- "Race"
label(matched1$PARMARR) <- "Parents Married at Birth"
label(matched1$INTACT18) <- "Lived in Intact Family From 0-18"
label(matched1$TOTINCR) <- "Family Income"
label(matched1$HIEDUC) <- "Highest Level of Education Achieved"
label(matched1$RELRAISD) <- "Religion Raised In"
label(matched1$ATTND14) <- "Frequency of Church Attendance at 14"
label(matched1$RELIGION) <- "Current Religious Affiliation"
label(matched1$EVRMARRY) <- "Have Been Married"
label(matched1$ONOWN18) <- "Lived on Their Own Before 18"
label(matched1$VRY1STAG) <- "Age at First Intercourse"
label(matched1$topics_discussed_count) <- "How Many Sexual Topics Taught"
label(matched1$discuss_topic1) <- "Taught How to Say No to Sex"
label(matched1$discuss_topic2) <- "Taught Methods of Birth Control"
label(matched1$discuss_topic3) <- "Taught Where to Get Birth Control"
label(matched1$discuss_topic4) <- "Taught About Sexually Transmitted Diseases"
label(matched1$discuss_topic5) <- "Taught How to Prevent HIV/AIDS"
label(matched1$discuss_topic6) <- "Taught How to Use a Condom"
label(matched1$premarital) <- "Had Premarital Sex"

# Actually build the Table 1:
table_one_matched <- table1(~ AGER + RACE + PARMARR + INTACT18 + TOTINCR + HIEDUC + RELRAISD + ATTND14 + EVRMARRY + ONOWN18 + VRY1STAG + topics_discussed_count + discuss_topic1 +
discuss_topic2 + discuss_topic3 + discuss_topic4 + discuss_topic5 + discuss_topic6 | discuss_topic8, data = matched1, render = rndr, render.continuous=c("Mean (SD)"="MEAN (SD)"))

print(matchedtab1, smd = TRUE)


#outcome analysis
y_trt1<-matched1$premarital[matched1$discuss_topic8==1]
y_con1<-matched1$premarital[matched1$discuss_topic8==0]

v1 <- table(y_trt1, y_con1)
mcnemar.test(v1)

sd_pscore <- sd(pscore)

#matching WITH caliper
psmatch2 <- Match(Tr=project_data$discuss_topic8, M=1, X=logit(pscore), replace=FALSE, caliper=(.2*sd_pscore))
matched2 <- project_data[c(psmatch2$index.treated, psmatch2$index.control),]

#get standardized differences
matchedtab2<-CreateTableOne(vars=xvars, strata ="discuss_topic8", data=matched2, test = FALSE)

#outcome analysis
y_trt2<-matched2$premarital[matched2$discuss_topic8==1]
y_con2<-matched2$premarital[matched2$discuss_topic8==0]

ver2 <- table(y_trt2, y_con2)
mcnemar.test(ver2)