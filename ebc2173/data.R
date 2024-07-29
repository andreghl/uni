rm(list = ls())
setwd("~/Maastricht/BYear2/Labour Economics/Denmark")


NEET <- read.csv("data/YoungNEET.csv", sep = ";", header = TRUE)

men <- ts(data = NEET$INDHOLD[1:9], start = 2014, frequency = 1)

women <- ts(NEET$INDHOLD[10:18], start = 2014, frequency = 1)

total <- men + women

ts.plot(total, ylab = "Number of People")
title(
        main = "Not in Education, Employment, and Training",
        sub = "Between the ages of 16 to 24 years old"
)
legend("topright",
        legend = c("Total"),
        col = c("black"), lty = c(1)
)

afterEduc <- read.csv("data/AfterEduc.csv", sep = ";", header = TRUE)

after <- data.frame(afterEduc$INDHOLD[1:5],
        afterEduc$INDHOLD[6:10],
        afterEduc$INDHOLD[11:15],
        afterEduc$INDHOLD[16:20],
        afterEduc$INDHOLD[21:25],
        afterEduc$INDHOLD[26:30],
        afterEduc$INDHOLD[31:35],
        afterEduc$INDHOLD[36:40],
        row.names = c(
                "In Education",
                "Employed",
                "Unemployed",
                "Outside Labor Market",
                "Not in Population"
        )
)

colnames(after) <- c(
        "Primary", "Upper Secondary",
        "VET", "Short Higher Educ",
        "Voc. Bachelor", "Bachelor",
        "Master", "PhD"
)

index <- c(
        "(1)", "(2)", "(3)", "(4)",
        "(5)", "(6)", "(7)", "(8)"
)

barplot(as.matrix(after),
        names.arg = index,
        beside = TRUE,
        axes = TRUE,
        col = c(
                "#F4F269", "#CEE26B",
                "#A8D26D", "#82C26E", "#5CB270"
        )
)
title(
        main = "Situation after Education",
        sub = "Situation after 21 months (2019)",
        xlab = "Education Level", ylab = "Number of People"
)
legend("topright",
        legend = row.names(after),
        lty = 1, lwd = 3,
        col = c(
                "#F4F269", "#CEE26B",
                "#A8D26D", "#82C26E", "#5CB270"
        )
)

# Average monthly salary in Denmark in 2022, by education level (in DKK)
# Source:
# https://www.statista.com/statistics/1166546/average-monthly-salary-in-denmark-by-education-level/

avgSalary <- c(
        34192.21, 37066.75, 39465.21,
        42209.21, 42845.6, 47688.52,
        48359.4, 48414.06, 60606.32,
        67204.7
)

mSalary <- data.frame(avgSalary,
        row.names = c(
                "Primary", "Secondary",
                "Qual. Ed.", "VET",
                "Not Stated", "Bac",
                "Short Ed.", "Voc. Bac",
                "Master", "PhD"
        )
)

legSal <- c(
        "34192.21 DKK", "37066.75 DKK", "39465.21 DKK",
        "42209.21 DKK", "42845.6 DKK", "47688.52 DKK",
        "48359.4 DKK", "48414.06 DKK", "60606.32 DKK",
        "67204.7 DKK"
)

barplot(
        height = mSalary$avgSalary,
        width = 1, space = 1,
        names.arg = row.names(mSalary),
        horiz = TRUE, las = 1,
        col = c(
                "#ffa585", "#ffad88", "#ffb58b",
                "#ffbd8e", "#ffc591", "#ffcd94",
                "#ffd597", "#ffdd9a", "#ffe59d",
                "#ffeda0"
        ),
)

title(
        main = "Average montly income by Education level",
        sub = "In Danish Krona (2022), Source: Statista"
)
par(oma = c(2, 3, 2, 2))

tPrimary <- sum(after$Primary)
tSecondary <- sum(after$`Upper Secondary`)
tVET <- sum(after$VET)
tShortEd <- sum(after$`Short Higher Educ`)
tVBac <- sum(after$`Voc. Bachelor`)
tBac <- sum(after$Bachelor)
tMas <- sum(after$Master)
tPhD <- sum(after$PhD)

afterP <- data.frame(round(afterEduc$INDHOLD[1:5] / tPrimary, 3),
        round(afterEduc$INDHOLD[6:10] / tSecondary, 3),
        round(afterEduc$INDHOLD[11:15] / tVET, 3),
        round(afterEduc$INDHOLD[16:20] / tShortEd, 3),
        round(afterEduc$INDHOLD[21:25] / tVBac, 3),
        round(afterEduc$INDHOLD[26:30] / tBac, 3),
        round(afterEduc$INDHOLD[31:35] / tMas, 3),
        round(afterEduc$INDHOLD[36:40] / tPhD, 3),
        row.names = c(
                "In Education",
                "Employed",
                "Unemployed",
                "Outside Labor Market",
                "Not in Population"
        )
)

colnames(afterP) <- c(
        "Primary", "Upper Secondary",
        "VET", "Short Higher Educ",
        "Voc. Bachelor", "Bachelor",
        "Master", "PhD"
)

index <- c(
        "(1)", "(2)", "(3)", "(4)",
        "(5)", "(6)", "(7)", "(8)"
)

barplot(as.matrix(afterP),
        names.arg = index,
        beside = TRUE,
        axes = TRUE,
        col = c(
                "#F4F269", "#CEE26B",
                "#A8D26D", "#82C26E", "#5CB270"
        )
)
title(
        main = "Situation after Education",
        sub = "Situation after 21 months (2019)",
        xlab = "Education Level", ylab = "Percentage of People"
)
legend("topright",
        legend = row.names(after),
        lty = 1, lwd = 3,
        col = c(
                "#F4F269", "#CEE26B",
                "#A8D26D", "#82C26E", "#5CB270"
        ),
        cex = 0.6
)

NEET1 <- read.csv("data/NEET1.csv", sep = ";", header = TRUE)

neetGdr <- data.frame(NEET1[, 2:11], row.names = NEET1[, 1])
colnames(neetGdr) <- c(
        "2013", "2014", "2015", "2016",
        "2017", "2018", "2019", "2020",
        "2021", "2022"
)

barplot(as.matrix(neetGdr),
        names.arg = colnames(neetGdr),
        beside = TRUE,
        axes = TRUE,
        col = c("#5CB270", "#A8D26D", "#F4F269")
)
title(
        main = "Not in Education, Employment, and Training",
        sub = "By gender, Source: Statistics Denmark",
        xlab = "Time", ylab = "Number of People"
)
legend("topright",
        legend = c("Total", "Men", "Women"),
        col = c("#5CB270", "#A8D26D", "#F4F269"), lty = 1, lwd = 2,
        cex = 0.6
)
par(oma = c(2, 2, 2, 0))

NEET2 <- read.csv("data/NEET2.csv", sep = ";", header = TRUE)

tNEET2 <- data.frame(NEET2[1:10, 3:17],
        row.names = NEET2[1:10, 2]
)

barplot(as.matrix(tNEET2[2:9, ]), # Plot of NEET by age over years
        names.arg = colnames(tNEET2),
        beside = TRUE,
        axes = TRUE
)


tNEET2_age <- as.data.frame(t(tNEET2))

barplot(as.matrix(tNEET2_age[, 2:10]), # Plot of NEET by years over age
        names.arg = colnames(tNEET2_age[2:10]),
        beside = TRUE,
        axes = TRUE
)


educSpendcsv <- read.csv("data/EducSpend.csv", sep = ";", header = TRUE)

educSpend <- data.frame(educSpendcsv[, 2:8], row.names = educSpendcsv[, 1])
colnames(educSpend) <- c(
        "2016", "2017", "2018", "2019", "2020", "2021", "2022"
)
View(educSpend)

barplot(as.matrix(educSpend), # Plot of Educ Spending for UpperSecondary
        names.arg = colnames(educSpend),
        beside = TRUE,
        axes = TRUE
)

educSpendP <- data.frame(
        educSpend[2:6, 1] / educSpend[1, 1],
        educSpend[2:6, 2] / educSpend[1, 2],
        educSpend[2:6, 3] / educSpend[1, 3],
        educSpend[2:6, 4] / educSpend[1, 4],
        educSpend[2:6, 5] / educSpend[1, 5],
        educSpend[2:6, 6] / educSpend[1, 6],
        educSpend[2:6, 7] / educSpend[1, 7],
        row.names = c(
                "State", "Municipalities and regions",
                "Households", "Companies", "International sources"
        )
)
colnames(educSpendP) <- colnames(educSpend)

View(educSpendP)

barplot(as.matrix(educSpendP), # Plot of Educ Spending for UpperSecondary
        names.arg = colnames(educSpendP),
        beside = TRUE,
        axes = TRUE
)


time <- c(2016, 2017, 2018, 2019, 2020, 2021, 2022)
firms <- educSpend[5, 1:7]

firmsP <- educSpend[5, 1:7] / educSpend[2, 1:7]

length(time)


plot(time, firmsP,
        type = "l",
        main = "Spending on Education by firms as a % of state spending",
        ylab = "Spending (firm / state)", xlab = "Time"
)

unempEduccsv <- read.csv("data/unempEduc.csv", sep = ",", header = TRUE, na.strings = c(" ", "NA"))
unempEduccsv$TIME_PERIOD[27:36]

unempEduc <- data.frame(
        unempEduccsv$TIME_PERIOD[7:14],
        unempEduccsv$OBS_VALUE[7:14],
        unempEduccsv$OBS_VALUE[17:24],
        unempEduccsv$OBS_VALUE[27:34]
)
colnames(unempEduc) <- c("Time", "Upper Secondary (All)", "Gymnasium", "Vocational Ed.")



matplot(unempEduc$Time, cbind(unempEduc[, 2], unempEduc[, 3], unempEduc[, 4]),
        type = "l", lty = 1, col = c("blue", "red", "darkgrey"),
        xlab = "Time", ylab = "Unemployment rate (%)",
        main = "Unemployment rate by Education level"
)
legend("topright",
        legend = c("Up. Secondary (All)", "Gymnasium", "Voc. Ed."),
        col = c("blue", "red", "grey"), lty = 1
)

empEduccsv <- read.csv("data/employEduc.csv", sep = ",", header = TRUE, na.strings = c(" ", "NA"))
empEduccsv$TIME_PERIOD[27:36]

empEduc <- data.frame(
        empEduccsv$TIME_PERIOD[7:16],
        empEduccsv$OBS_VALUE[7:16],
        empEduccsv$OBS_VALUE[17:26],
        empEduccsv$OBS_VALUE[27:36]
)

colnames(empEduc) <- c("Time", "Upper Secondary (All)", "Gymnasium", "Vocational Ed.")

matplot(empEduc$Time, cbind(empEduc[, 2], empEduc[, 3], empEduc[, 4]),
        type = "l", lty = 1, col = c("blue", "red", "darkgrey"),
        xlab = "Time", ylab = "Employment rate (%)",
        main = "Employment rate by Education level"
)
legend("topright",
        legend = c("Up. Secondary (All)", "Gymnasium", "Voc. Ed."),
        col = c("blue", "red", "grey"), lty = 1
)

dropEduccsv <- read.csv("data/dropoutEduc.csv", sep = ",", header = TRUE, na.strings = c(" ", "NA"))
dropEduccsv$TIME_PERIOD[1:10]

dropEduc <- data.frame(
        dropEduccsv$TIME_PERIOD[1:10],
        dropEduccsv$OBS_VALUE[1:10],
        dropEduccsv$OBS_VALUE[11:20],
        dropEduccsv$OBS_VALUE[21:30],
        dropEduccsv$OBS_VALUE[31:40],
        dropEduccsv$OBS_VALUE[41:50]
)

colnames(dropEduc) <- c("Time", "Employed", "Not Employed", "Don't want work", "Population", "Want work")

plot(dropEduc$Time, dropEduc$Population,
        type = "l", lty = 1, 
        xlab = "Time", ylab = "Drop out rate (%)",
        main = "Early leavers of education and training"
)
