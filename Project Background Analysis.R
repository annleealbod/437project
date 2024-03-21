# Load packages
library(readr)
library(table1)
library(kableExtra)
library(readr)

# Load SAS data
project_data <- read_csv("project_data.csv")

# General table one stuff below, need to change to new data:

table$ARF <- factor(table$ARF, levels = c(0,1), labels = c("Absent", "Present"))
table$CHF <- factor(table$CHF, levels = c(0,1), labels = c("Absent", "Present"))
table$Cirr <- factor(table$Cirr, levels = c(0,1), labels = c("Absent", "Present"))
table$colcan <- factor(table$colcan, levels = c(0,1), labels = c("Absent", "Present"))
table$Coma <- factor(table$Coma, levels = c(0,1), labels = c("Out", "In"))
table$lungcan <- factor(table$lungcan, levels = c(0,1), labels = c("Absent", "Present"))
table$MOSF <- factor(table$MOSF, levels = c(0,1), labels = c("Absent", "Present"))
table$sepsis <- factor(table$sepsis, levels = c(0,1), labels = c("Absent", "Present"))
table$female <- factor(table$female, levels = c(0,1), labels = c("Male", "Female"))
table$treatment <- factor(table$treatment, levels = c(0,1), labels = c("No RHC", "RHC"))
table$died <- factor(table$died, levels = c(0,1), labels = c("Alive", "Died"))

label(table$Cirr) <- "Cirrhosis"
label(table$colcan) <- "Colon Cancer"
label(table$lungcan) <- "Lung Cancer"
label(table$sepsis) <- "Sepsis"
label(table$female) <- "Sex"
label(table$age) <- "Age"
label(table$meanbp1) <- "Mean BP"
label(table$aps) <- "APS"
label(table$treatment) <- "Treatment"

units(table$age) <- "years"
units(table$meanbp1) <- "mmHg"

renderMeanMed <- function(x, name, ...) {
  if (!is.numeric(x)) return(render.categorical.default(x))
  what <- switch(name,
                 age = "Median (IQR)",
                 meanbp1  = "Mean (SD)")
  parse.abbrev.render.code(c("", what))(x)
}

table1(~ ARF + CHF + Cirr + colcan + Coma + lungcan + MOSF + sepsis + female
       + age + meanbp1 + aps + treatment | died, 
       data = table, overall = "Total", 
       render = renderMeanMed)
