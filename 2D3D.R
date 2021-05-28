## ----------- PACKAGES --------------
# Packages
library("readxl")
library("tidyverse")
library("tidyselect")
library("tableone")
library("olsrr")
library("lintr")
library("styler")
library("docstring")

## ------------ DATA PREPERATION (MANDATORY TO RUN) ---------------
# Dataframe maken van resultaten excel, eerste drie rijen skippen
# all_results <- data.frame(read_excel("H:/PhD/2D3D/Data/Results_excel.xlsx", range = "B4:BY193"))
all_results <- data.frame(read_excel("~/Desktop/Results_excel.xlsx", range = "B4:BY193"))

glimpse(all_results)
#dit laat een overzicht van alle kolommen zien met datatype en de eerste 7 waardes in elke kolom

#alternative with for-loop
cols <- c("Cadaver", "Testcase", "Number.of.markers", "Rotation.angle")

for (i in cols){
  all_results[ ,i] <- as.numeric(all_results[, i])
}

# Eigen naam geven (makkelijker voor verdere programmering)
all_results <- all_results %>% rename(
  cadaver = Cadaver, 
  testcase = Testcase, 
  no.markers = Number.of.markers,
  slice.thickness = Slice.thickness, 
  rotation.angle = Rotation.angle,
  target.v = Middle.vertebrae, 
  detector.distance = Detector.distance
)

# SUBSETTING DATAFRAMES + categorie voor registratie instellingen
# Testcase 4 en 22 eruit. Deze zijn geen onderdeel van de analyse voor het paper.
# Rotation angle die niet hetzelfde is, eruit --> 2045 e.d.
# En alle columns eruit die niet TRE/xyz/v/relevant zijn
all_data_for_paper <- subset(all_results, (((testcase != 4) & (testcase != 22)) & (rotation.angle < 46)))
all_data_for_paper <- select(all_data_for_paper, c(
  cadaver, testcase, no.markers,
  target.v, detector.distance,
  slice.thickness, rotation.angle,
  starts_with(c("TRE", "v", "x", "y", "z"))
))
# Categorie per registratie
all_data_for_paper$cat_registration <- 99

# alternative with function
assignCat <- function(a, b, c, d, e, f, cat) {
  #' @title Assign Categories to testcases
  #' @description This function assigns the variable cat_registration
  #' the correct numbers based on testcase and rotation angle
  #' @param a-e are the testcases that apply
  #' @param f is the rotation angle
  #' @param cat is the number that is assigned to each category
  #' @return Returns the correct category for each testcase
  all_data_for_paper$cat_registration[(((all_data_for_paper$testcase == a) | 
                                          (all_data_for_paper$testcase == b) |
                                          (all_data_for_paper$testcase == c) | 
                                          (all_data_for_paper$testcase == d) |
                                          (all_data_for_paper$testcase == e)) & 
                                         (all_data_for_paper$rotation.angle == f))] <- cat
  return(all_data_for_paper)
}

?assignCat

# 1 = default
all_data_for_paper <- assignCat(2, 7, 11, 15, 19, 30, 1)
# 2 = detector op 30cm
all_data_for_paper <- assignCat(3, 8, 12, 16, 20, 30, 2)
# 3 = 7 markers
all_data_for_paper <- assignCat(1, 6, 10, 14, 18, 30, 3)
# 4 = 9 markers
all_data_for_paper <- assignCat(5, 9, 13, 17, 21, 30, 4)
# 5 = 20 degrees
all_data_for_paper <- assignCat(2, 7, 11, 15, 19, 20, 5)
# 6 = 28 degrees
all_data_for_paper <- assignCat(2, 7, 11, 15, 19, 28, 6)
# 7 = 32 degrees
all_data_for_paper <- assignCat(2, 7, 11, 15, 19, 32, 7)
# 8 = 45 degrees
all_data_for_paper <- assignCat(2, 7, 11, 15, 19, 45, 8)


check0 <- all_data_for_paper %>%
  count(cat_registration)
'
# 1 = default
all_data_for_paper$cat_registration[(((all_data_for_paper$testcase == 2) | (all_data_for_paper$testcase == 7) |
  (all_data_for_paper$testcase == 11) | (all_data_for_paper$testcase == 15) |
  (all_data_for_paper$testcase == 19)) &
  (all_data_for_paper$rotation.angle == 30))] <- 1
# 2 = detector op 30cm
all_data_for_paper$cat_registration[(((all_data_for_paper$testcase == 3) | (all_data_for_paper$testcase == 8) |
  (all_data_for_paper$testcase == 12) | (all_data_for_paper$testcase == 16) |
  (all_data_for_paper$testcase == 20)) &
  (all_data_for_paper$rotation.angle == 30))] <- 2
# 3 = 7 markers
all_data_for_paper$cat_registration[(((all_data_for_paper$testcase == 1) | (all_data_for_paper$testcase == 6) |
  (all_data_for_paper$testcase == 10) | (all_data_for_paper$testcase == 14) |
  (all_data_for_paper$testcase == 18)) &
  (all_data_for_paper$rotation.angle == 30))] <- 3
# 4 = 9 markers
all_data_for_paper$cat_registration[(((all_data_for_paper$testcase == 5) | (all_data_for_paper$testcase == 9) |
  (all_data_for_paper$testcase == 13) | (all_data_for_paper$testcase == 17) |
  (all_data_for_paper$testcase == 21)) &
  (all_data_for_paper$rotation.angle == 30))] <- 4
# 5 = 20 degrees
all_data_for_paper$cat_registration[(((all_data_for_paper$testcase == 2) | (all_data_for_paper$testcase == 7) |
  (all_data_for_paper$testcase == 11) | (all_data_for_paper$testcase == 15) |
  (all_data_for_paper$testcase == 19)) &
  (all_data_for_paper$rotation.angle == 20))] <- 5
# 6 = 28 degrees
all_data_for_paper$cat_registration[(((all_data_for_paper$testcase == 2) | (all_data_for_paper$testcase == 7) |
  (all_data_for_paper$testcase == 11) | (all_data_for_paper$testcase == 15) |
  (all_data_for_paper$testcase == 19)) &
  (all_data_for_paper$rotation.angle == 28))] <- 6
# 7 = 32 degrees
all_data_for_paper$cat_registration[(((all_data_for_paper$testcase == 2) | (all_data_for_paper$testcase == 7) |
  (all_data_for_paper$testcase == 11) | (all_data_for_paper$testcase == 15) |
  (all_data_for_paper$testcase == 19)) &
  (all_data_for_paper$rotation.angle == 32))] <- 7
# 8 = 45 degrees
all_data_for_paper$cat_registration[(((all_data_for_paper$testcase == 2) | (all_data_for_paper$testcase == 7) |
  (all_data_for_paper$testcase == 11) | (all_data_for_paper$testcase == 15) |
  (all_data_for_paper$testcase == 19)) &
  (all_data_for_paper$rotation.angle == 45))] <- 8
# Labeling cat_registration
all_data_for_paper$cat_registration <- factor(all_data_for_paper$cat_registration,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 99),
  labels = c(
    "default", "30 cm detector", "7 markers",
    "9 markers", "20 degrees", "28 degrees",
    "32 degrees", "45 degrees", "no analysis for now"
  )
)
'
# ff op goede plek zetten voor overzicht
all_data_for_paper <- all_data_for_paper %>% relocate(cat_registration, .after = testcase)

##
### Distribution of TRE's
##
# bins is hoe breed zo'n bar is
# fatten is voor dikte median, lwd voor dikte van boxplot lijnen
# verdeling van ALLE TRE's, dus onafhankelijk van cadaver, testcase, anatomical region of target level.
normality <- subset(all_data_for_paper, (testcase != 4) | (testcase != 22))
level_statistics <- select(normality, c(
  cadaver, testcase, no.markers,
  target.v, detector.distance,
  slice.thickness, rotation.angle,
  starts_with("TRE.V")
))

# reshape van wide naar long, dus alle TRE's onder elkaar en dan op level gesplitst
# handmatig gecontroleerd in originele tabel en huidig dataframe of TRE's per level overeenkomen. Dit is het geval.
level_statistics <- reshape(level_statistics,
                            direction = "long",
                            varying = list(names(level_statistics)[8:12]),
                            v.names = "TRE",
                            idvar = c(
                              "cadaver", "testcase", "no.markers", "target.v", "detector.distance",
                              "slice.thickness", "rotation.angle"
                            ),
                            timevar = "level",
                            times = c("two above", "one above", "target", "one below", "two below")
)
level_statistics$target <- 0
level_statistics$target[(level_statistics$target.v == "L1")] <- 1
level_statistics$target[(level_statistics$target.v == "L4")] <- 1

level_statistics$target <- factor(level_statistics$target,
                                  levels = c(0, 1),
                                  labels = c("Thoracic", "Lumbar")
)
level_statistics$detector.distance[(level_statistics$detector.distance == 20)] <- 1
level_statistics$detector.distance[(level_statistics$detector.distance == 30)] <- 0
level_statistics$detector.distance <- factor(level_statistics$detector.distance,
                                             levels = c(0, 1),
                                             labels = c("30cm", "20cm")
)
level_statistics$levels <- 0
level_statistics$levels[(level_statistics$level == "one below")] <- 1
level_statistics$levels[(level_statistics$level == "one above")] <- 1
level_statistics$levels[(level_statistics$level == "two below")] <- 2
level_statistics$levels[(level_statistics$level == "two above")] <- 2
level_statistics$levels <- factor(level_statistics$levels,
                                  levels = c(0, 1, 2),
                                  labels = c(
                                    "target level", "plus one level",
                                    "plus two levels"
                                  )
)
# rotation angle is eigenlijk keer 2
level_statistics$rotation.angle <- level_statistics$rotation.angle * 2
# ook aparte groepsvariabele maken voor rotation angle -> belangrijk voor boxplot, maar moet continue voor regressie!
level_statistics$rotation.group <- "40 degrees"
level_statistics$rotation.group[(level_statistics$rotation.angle == 56)] <- "56 degrees"
level_statistics$rotation.group[(level_statistics$rotation.angle == 60)] <- "60 degrees"
level_statistics$rotation.group[(level_statistics$rotation.angle == 64)] <- "64 degrees"
level_statistics$rotation.group[(level_statistics$rotation.angle == 90)] <- "90 degrees"

marker_statistics <- subset(level_statistics, ((testcase == 1) | (testcase == 2) | (testcase == 5) |
                                                 (testcase == 6) | (testcase == 7) | (testcase == 9) |
                                                 (testcase == 10) | (testcase == 11) | (testcase == 13) |
                                                 (testcase == 14) | (testcase == 15) | (testcase == 17) |
                                                 (testcase == 18) | (testcase == 19) | (testcase == 21)))
marker_statistics <- marker_statistics[(marker_statistics$rotation.angle == 60), ]

marker_statistics$marker.group <- "seven markers"
marker_statistics$marker.group[(marker_statistics$no.markers == 8)] <- "eight markers"
marker_statistics$marker.group[(marker_statistics$no.markers == 9)] <- "nine markers"

marker_statistics$marker.group <- ordered(marker_statistics$marker.group,
                                          levels = c(
                                            "seven markers", "eight markers",
                                            "nine markers"
                                          )
)

## ---------- DATA EXPLORATION FOR NORMALITY ------------------------------

# qqplot voor normale verdeeldheid
qqnorm(level_statistics$TRE)
qqline(level_statistics$TRE, col = "red")

# verdeling opslaan
ggplot(level_statistics, aes(x = TRE)) +
  geom_histogram(bins = 25, fill = "white", colour = "black") +
  xlab(
    "All TRE's in mm"
  ) +
  geom_boxplot(
    fill = "grey", colour = "black", outlier.colour = "red", outlier.shape = 1, lwd = 1.2, fatten = 0.7
  )
ggsave("H:/PhD/2D3D/Data/Figures/distribution.png")

median(level_statistics$TRE, na.rm = T)
quantile(level_statistics$TRE, na.rm = T, 0.25)
quantile(level_statistics$TRE, na.rm = T, 0.75)
quantile(level_statistics$TRE, na.rm = T, 0.95)

## ------------- UNIVARIATE ANALYSIS FOR CADAVER -----
#
# cadaver moet categoriale variabele zijn + ook voor volgorde
level_statistics$cadaver[level_statistics$cadaver == 1] <- "M78-middle BMI"
level_statistics$cadaver[level_statistics$cadaver == 2] <- "V51-lowest BMI"
level_statistics$cadaver[level_statistics$cadaver == 3] <- "M88-highest BMI"

level_statistics$cadaver <- ordered(level_statistics$cadaver,
                                    levels = c("M88-highest BMI", "M78-middle BMI", "V51-lowest BMI")
)
# TRE
ggplot(level_statistics, aes(x = cadaver, y = TRE, fill = cadaver)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  xlab(NULL) +
  ylab("TRE") +
  ylim(0, 2.2) +
  theme(legend.position = "none")
ggsave("H:/PhD/2D3D/Data/Figures/cadaver.png")

## Kruskal wallis
kruskal.test(TRE ~ cadaver, data = level_statistics)

# Post-hoc is pariwise wilcoxon (significant difference)
pairwise.wilcox.test(level_statistics$TRE, level_statistics$cadaver,
                     p.adjust.method = "bonferroni"
)

## -------------- UNIVARIATE ANALYSIS FOR ANATOMIC REGION ----------------------

## dit is voor plot per anatomic region
# order variabele maken voor plot
level_statistics$target.v <- ordered(level_statistics$target.v,
                                     levels = c(
                                       "T4", "T7",
                                       "T10", "L1", "L4"
                                     )
)

# plotje maken voor anatomical region
ggplot(level_statistics, aes(x = target.v, y = TRE, fill = target.v)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  xlab(NULL) +
  ylab("TRE") +
  ylim(0, 2.2) +
  theme(legend.position = "none")
ggsave("H:/PhD/2D3D/Data/Figures/anatomical.png")

kruskal.test(TRE ~ target.v, data = level_statistics)

# plotje maken voor anatomical region dichotoom
ggplot(level_statistics, aes(x = target, y = TRE, fill = target)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  xlab(NULL) +
  ylab("TRE") +
  ylim(0, 2.2) +
  theme(legend.position = "none")
ggsave("H:/PhD/2D3D/Data/Figures/anatomical_dich.png")

# Mann Whitney U
wilcox.test(level_statistics$TRE ~ level_statistics$target, mu = 0, alt = "two.sided", conf.int = T, conf.level = 0.95, paired = F, exact = T)


## -------------- UNIVARIATE ANALYSIS FOR TARGET LEVEL ----------------------
## dit is voor plot per target level
# order variabele maken voor plot
level_statistics$level <- ordered(level_statistics$level,
                                  levels = c(
                                    "two above", "one above",
                                    "target", "one below", "two below"
                                  )
)

# plotje maken
ggplot(level_statistics, aes(x = level, y = TRE, fill = level)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  xlab(NULL) +
  ylab("TRE") +
  ylim(0, 2.2) +
  theme(legend.position = "none")
ggsave("H:/PhD/2D3D/Data/Figures/levels.png")

kruskal.test(TRE ~ level, data = level_statistics)

# plotje maken voor target level ordinaal
ggplot(level_statistics, aes(x = levels, y = TRE, fill = levels)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  xlab(NULL) +
  ylab("TRE") +
  ylim(0, 2.2) +
  theme(legend.position = "none")
ggsave("H:/PhD/2D3D/Data/Figures/levels_dich.png")

# kruskal
kruskal.test(TRE ~ levels, data = level_statistics)


class(level_statistics$target)
boxplot(level_statistics$TRE ~ level_statistics$target)

## ------------- UNIVARIATE ANALYSIS FOR ROTATION ANGLE --------------------

## plotje maken voor angle
ggplot(level_statistics, aes(x = rotation.group, y = TRE, fill = rotation.group)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  xlab(NULL) +
  ylab("TRE") +
  ylim(0, 2.2) +
  theme(legend.position = "none")
ggsave("H:/PhD/2D3D/Data/Figures/angle.png")

# alsnog kruskal ook al is variabele continue -> zo gaat ie wel de regressie in
kruskal.test(TRE ~ rotation.group, data = level_statistics)

## ------------ UNIVARIATE ANALYSIS FOR DETECTOR DISTANCE ----------------
ggplot(level_statistics, aes(x = detector.distance, y = TRE, fill = detector.distance)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  xlab(NULL) +
  ylab("TRE") +
  ylim(0, 2.2) +
  theme(legend.position = "none")
ggsave("H:/PhD/2D3D/Data/Figures/detector.png")

wilcox.test(level_statistics$TRE ~ level_statistics$detector.distance, mu = 0, alt = "two.sided", conf.int = T, conf.level = 0.95, paired = F, exact = T)

## ------------ UNIVARIATE ANALYSIS FOR NO OF MARKERS ------------------

ggplot(marker_statistics, aes(x = marker.group, y = TRE, fill = marker.group)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  xlab(NULL) +
  ylab("TRE") +
  ylim(0, 2.2) +
  theme(legend.position = "none")
ggsave("H:/PhD/2D3D/Data/Figures/marker.png")

# alsnog kruskal ook al is variabele continue
kruskal.test(TRE ~ marker.group, data = marker_statistics)

# Post-hoc is pariwise wilcoxon (significant difference)
pairwise.wilcox.test(marker_statistics$TRE, marker_statistics$marker.group,
                     p.adjust.method = "bonferroni"
)

## ------------- MULTIVARIATE REGRESSION ANALYSIS -----------

## voor multipele logistische regressie moeten alle variabelen continue of nominaal zijn
## dependent = tre (continue)
## independent; cadaver (nominaal), anatomic region (dichotoom),
## target level (continue), hoek (continue), detector (dichotoom)
## casus 1,5,6,9,10,13,14,17,18,21 eruit --> 2,3 7,8 11,12 15,16 19,20 mogen blijven

multiple_regression <- subset(level_statistics, (testcase == 2) | (testcase == 3) | (testcase == 7) |
                                (testcase == 8) | (testcase == 11) | (testcase == 12) | (testcase == 15) |
                                (testcase == 16) | (testcase == 19) | (testcase == 20))

multiple_regression$target <- 1
multiple_regression$target[(multiple_regression$target.v == "L1")] <- 0
multiple_regression$target[(multiple_regression$target.v == "L4")] <- 0

multiple_regression$target <- factor(multiple_regression$target,
                                     levels = c(0, 1),
                                     labels = c("Lumbar", "Thoracic"))

multiple_regression$subject <- 0
multiple_regression$subject[(multiple_regression$cadaver == 2)] <- 2
multiple_regression$subject[(multiple_regression$cadaver == 1)] <- 1
multiple_regression$subject <- factor(multiple_regression$subject,
                                      levels = c(0, 1, 2),
                                      labels = c(
                                        "male-highest BMI", "male-middle BMI",
                                        "female-lowest BMI"))

multiple_regression <- select(multiple_regression, c(
  subject, testcase, no.markers,
  target, detector.distance, TRE,
  slice.thickness, rotation.angle
))

# drop NA
multiple_regression <- multiple_regression %>% drop_na()

## wie hebben er interacties?
regression_model <- lm(TRE ~ rotation.angle + target + detector.distance + subject, 
                       data = multiple_regression
)
summary(regression_model)
ols_plot_resid_qq(regression_model)
ols_test_normality(regression_model)
ols_test_correlation(regression_model)



# https://www.youtube.com/watch?v=6HCJGYYCPR0
# https://www.youtube.com/watch?v=ejR8LnQziPY&list=RDCMUCeoF_5Kw0YyWOqhAbQGrxJQ&start_radio=1

step(regression_model, direction = "forward")


## -------------------------------------


##
##
##
##
##
##
##
##
##
##















# Datasubset voor gold registration TRE;8 beads, -30+30, 20cm, 0.67S AND Testcase 2/7/11/15/19
# Filteren op Testcase en rotation angle
descriptive_statistics <- subset(all_data_for_paper, (cat_registration != "no analysis for now"))

# BASELINE TABELLEN/FLOWCHART
baseline_variables <- c(
  "TRE.V.2", "TRE.V.1", "TRE.V", "TRE.V.1.1", "TRE.V.2.1",
  "TREp1.V.2", "TREp1.V.1", "TREp1.V", "TREp1.V.1.1", "TREp1.V.2.1"
)
baseline_table <- CreateTableOne(
  vars = baseline_variables, strata = "cat_registration",
  data = descriptive_statistics, includeNA = TRUE,
  test = FALSE, addOverall = TRUE
)
print(baseline_table, nonnormal = baseline_variables, quote = T, noSpaces = T)
# Gewoon de hele print kopieren en plakken in excel. Na het plakken even op Text Import Wizard klikken en dan voltooien.
# In een later stadium dit wel automatiseren, zinloos werk.

##
baseline_table
class(baseline_table)
## missingss, helaas wel handmatig over schrijven -> plakken naar Word en invoeren in flowchart
summary(baseline_table, digits = 2)


## STATISTICAL ANALYSIS

# Is de data normaal verdeeld? Bij n<30 moet je dit nagaan. In het geval van univariate variabele
# kan Shapiro-Wilk's method worden gebruikt. Bij p > 0.05 is de data normaal verdeeld.

# shapiro.test(default_registration$TRE.V)
# shapiro.test(default_registration$TRE.V.1)
# shapiro.test(default_registration$TRE.V.2)
# shapiro.test(default_registration$TRE.V.1.1)
# shapiro.test(default_registration$TRE.V.2.1)

### ALLEMAAL NORMAAL VERDEELD, dus Paired t-test moet verricht worden.

# We gaan uit van niet normaal verdeeldheid

# Om Wilcoxon Rank Sum te moeten doen, moet er ook een vergelijkende variabele zijn. Dus target.3d3d = 0

descriptive_statistics$target.3d3d <- 0

# dataframes voor categorische registratie
default_registration <- subset(descriptive_statistics, cat_registration == "default")
detector_registration <- subset(descriptive_statistics, cat_registration == "30 cm detector")
sevenmarkers_registration <- subset(descriptive_statistics, cat_registration == "7 markers")
ninemarkers_registration <- subset(descriptive_statistics, cat_registration == "9 markers")
twentydegrees_registration <- subset(descriptive_statistics, cat_registration == "20 degrees")
twentyeightdegrees_registration <- subset(descriptive_statistics, cat_registration == "28 degrees")
thirtytwodegrees_registration <- subset(descriptive_statistics, cat_registration == "32 degrees")
fortyfivedegrees_registration <- subset(descriptive_statistics, cat_registration == "45 degrees")




###
### Baseline maken met Wilcoxon
###

## eerst invullen voor welke subset je dit wilt doen!
attach(fortyfivedegrees_registration)

# DEFAULT REGISTRATION, #H0: median change in TRE is = 1 mm, #H1: median change in TRE != 1 mm
# two-sided to get 95% CI, #paired measurements, #calc 95% CI


## TRE.V
target.vertebra <- list(wilcox.test(TRE.V, target.3d3d,
                                    mu = 1, paired = T,
                                    conf.int = T, conf.level = 0.95
))
target.vertebra <- c(target.vertebra[[1]]$estimate, target.vertebra[[1]]$conf.int)
## TRE.V.1
one.above <- list(wilcox.test(TRE.V.1, target.3d3d,
                              mu = 1, paired = T,
                              conf.int = T, conf.level = 0.95
))
one.above <- c(one.above[[1]]$estimate, one.above[[1]]$conf.int)
## TRE.V.2
two.above <- list(wilcox.test(TRE.V.2, target.3d3d,
                              mu = 1, paired = T,
                              conf.int = T, conf.level = 0.95
))
two.above <- c(two.above[[1]]$estimate, two.above[[1]]$conf.int)
## TRE.V.1.1
one.below <- list(wilcox.test(TRE.V.1.1, target.3d3d,
                              mu = 1, paired = T,
                              conf.int = T, conf.level = 0.95
))
one.below <- c(one.below[[1]]$estimate, one.below[[1]]$conf.int)
## TRE.V.2.1
two.below <- list(wilcox.test(TRE.V.2.1, target.3d3d,
                              mu = 1, paired = T,
                              conf.int = T, conf.level = 0.95
))
two.below <- c(two.below[[1]]$estimate, two.below[[1]]$conf.int)

forreshape <- data.frame(c("median difference", "lower 95% CI", "upper 95% CI"))
wilcox <- data.frame(forreshape, two.above, one.above, target.vertebra, one.below, two.below)
view(wilcox)

# einde baseline



# Subvergelijkingen!
# voor de TRE

## Subsetting voor specifieke verglijkingen, dus default vs 7 of 9 markers. default vs alle hoeken
# Detector
detector_statistics <- subset(descriptive_statistics, ((cat_registration == "default") | (cat_registration == "30 cm detector")))
ggplot(detector_statistics, aes(x = cat_registration, y = TRE.V.2.1, fill = cat_registration)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  xlab(NULL) +
  ylab("TRE-2") +
  ylim(0, 2) +
  theme(legend.position = "none")
ggsave("H:/PhD/2D3D/Data/Figures/detector21.png")

# Markers
marker_statistics <- subset(descriptive_statistics, ((cat_registration == "7 markers") |
                                                       (cat_registration == "default") | (cat_registration == "9 markers")))
# voor de order in de boxplot
marker_statistics$re_order[marker_statistics$cat_registration == "7 markers"] <- 1
marker_statistics$re_order[marker_statistics$cat_registration == "default"] <- 2
marker_statistics$re_order[marker_statistics$cat_registration == "9 markers"] <- 3
ggplot(marker_statistics, aes(x = reorder(cat_registration, re_order), y = TRE.V.2.1, fill = cat_registration)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  xlab(NULL) +
  ylab("TRE-2") +
  ylim(0, 2) +
  theme(legend.position = "none")
ggsave("H:/PhD/2D3D/Data/Figures/marker21.png")

# Angles
angle_statistics <- subset(descriptive_statistics, ((cat_registration == "20 degrees") |
                                                      (cat_registration == "28 degrees") | (cat_registration == "default") |
                                                      (cat_registration == "32 degrees") | cat_registration == "45 degrees"))
# voor de order in de boxplot
angle_statistics$re_order[angle_statistics$cat_registration == "20 degrees"] <- 1
angle_statistics$re_order[angle_statistics$cat_registration == "28 degrees"] <- 2
angle_statistics$re_order[angle_statistics$cat_registration == "default"] <- 3
angle_statistics$re_order[angle_statistics$cat_registration == "32 degrees"] <- 4
angle_statistics$re_order[angle_statistics$cat_registration == "45 degrees"] <- 5
ggplot(angle_statistics, aes(x = reorder(cat_registration, re_order), y = TRE.V.2.1, fill = cat_registration)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  xlab(NULL) +
  ylab("TRE-2") +
  ylim(0, 2) +
  theme(legend.position = "none")
ggsave("H:/PhD/2D3D/Data/Figures/angle21.png")


#
##
###
#### Subanalysis per anatomical level
###
##
# use descriptive statistics: regardless of detector or angle. Everything together. eerst variabele voor volgorde.
descriptive_statistics$re_order[descriptive_statistics$target.v == "T4"] <- 1
descriptive_statistics$re_order[descriptive_statistics$target.v == "T7"] <- 2
descriptive_statistics$re_order[descriptive_statistics$target.v == "T10"] <- 3
descriptive_statistics$re_order[descriptive_statistics$target.v == "L1"] <- 4
descriptive_statistics$re_order[descriptive_statistics$target.v == "L4"] <- 5

# TRE
ggplot(descriptive_statistics, aes(x = reorder(target.v, re_order), y = TRE.V, fill = target.v)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  xlab(NULL) +
  ylab("TRE") +
  ylim(0, 2) +
  theme(legend.position = "none")
ggsave("H:/PhD/2D3D/Data/Figures/anatomical.png")

# TRE+1
ggplot(descriptive_statistics, aes(x = reorder(target.v, re_order), y = TRE.V.1.1, fill = target.v)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  xlab(NULL) +
  ylab("TRE-1") +
  ylim(0, 2) +
  theme(legend.position = "none")
ggsave("H:/PhD/2D3D/Data/Figures/anatomical11.png")

# TRE+2
ggplot(descriptive_statistics, aes(x = reorder(target.v, re_order), y = TRE.V.2.1, fill = target.v)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  xlab(NULL) +
  ylab("TRE-2") +
  ylim(0, 2) +
  theme(legend.position = "none")
ggsave("H:/PhD/2D3D/Data/Figures/anatomical21.png")






#
##
### TRE > 0.95 analysis AND < 0.4 analysis
##
#


# NOG FF NAAR KIJKEN, ZO PAS JE VOLGORDE AAN
# my_data$group <- ordered(my_data$group,
#                         levels = c("ctrl", "trt1", "trt2"))


'
# outcomes
descriptive_statistics$high_TRE[descriptive_statistics$TRE.V >0.94] <- "TRE>0.94mm"
descriptive_statistics$high_TRE[descriptive_statistics$TRE.V <=0.94] <- "TRE<=0.94mm"
# outcomes
descriptive_statistics$low_TRE[descriptive_statistics$TRE.V <0.45] <- "TRE<0.45mm"
descriptive_statistics$low_TRE[descriptive_statistics$TRE.V >=0.45] <- "TRE>=0.45mm"

# incomes
descriptive_statistics$target_cat <- "thoracic"
descriptive_statistics$target_cat[descriptive_statistics$target.v == "L1"] <- "lumbar"
descriptive_statistics$target_cat[descriptive_statistics$target.v == "L4"] <- "lumbar"

# incomes
descriptive_statistics$rotation_cat[descriptive_statistics$rotation.angle <28] <- "20 degrees"
descriptive_statistics$rotation_cat[(descriptive_statistics$rotation.angle >=28) & (descriptive_statistics$rotation.angle <=32)] <- "28-32 degrees"
descriptive_statistics$rotation_cat[descriptive_statistics$rotation.angle >32] <- "45 degrees"

#wat willen we weten
baselinevariables <- c("testcase", "no.markers", "target_cat", "detector.distance", "rotation_cat")
descriptive_statistics[baselinevariables] <- lapply(descriptive_statistics[baselinevariables], factor)

#
## baselines
# high
baseline_TRE_high <- CreateTableOne(vars = baselinevariables, strata = "high_TRE",
                                 data = descriptive_statistics)
baseline_TRE_high <- print(baseline_TRE_high)     
write.csv(baseline_TRE_high, file = "H:/PhD/2D3D/Data/Tables/hightre.csv")

# low  
baseline_TRE_low <- CreateTableOne(vars = baselinevariables, strata = "low_TRE",
                                    data = descriptive_statistics)
baseline_TRE_low <- print(baseline_TRE_low)     
write.csv(baseline_TRE_low, file = "H:/PhD/2D3D/Data/Tables/lowtre.csv")'