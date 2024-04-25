# Import libraries

# install.packages(c("readr", "dplyr", "forcats", "stringi",
#                    "stringr", "tidyr"))

library(readr)
library(dplyr)
library(forcats)
library(stringi)
library(stringr)
library(tidyr)

# Define secondary function

survey = readr::read_csv("survey.csv")

remove_accents <- function(text) {
  # Use stri_trans_nf* to decompose and then recompose characters
  without_accents <- stri_trans_nfkd(text)
  without_accents <- gsub("\\p{M}", "", without_accents, perl = TRUE)
  
  return(without_accents)
}

# Import data

list_of_greek_cities = readLines("file.txt") # Personal project stesiam/GreekCities
survey <- read_csv("survey.csv")

# Give short names to vars

survey = survey %>% as.data.frame() %>%
  setNames(c("Timestamp", "TypeDev", "ProgLang",
             "YearsProgr", "PersonalProj", "Gender", "WorkType",
             "TownLive","TownWork", "Company", "Supervising",
             "Studies", "RelativeComp", "AnnualNetSalary"))

# Recoding variables

survey$PersonalProj <- survey$PersonalProj %>%
  fct_recode(
    "Yes" = "Ναι",
    "No" = "Όχι"
  )


survey$Gender <- survey$Gender %>%
  fct_recode(
    "Man" = "Άντρας",
    "Woman" = "Γυναίκα",
    "-" = "Συκής :)"
  )

# Filter out rows with "-"
survey <- survey %>%
  filter(Gender != "-")

# Remove unused level
survey$Gender <- droplevels(survey$Gender)


survey$WorkType <- survey$WorkType %>%
  fct_recode(
    "Remote" = "Απομακρυσμένα",
    "Hybrid" = "Και τα δύο",
    "Onsite" = "Στον χώρο του εργοδότη"
  )

survey$Supervising <- survey$Supervising %>%
  fct_recode(
    "Yes" = "Ναι",
    "No" = "Όχι"
  )


## Recoding survey$Studies
survey$Studies <- survey$Studies %>%
  fct_recode(
    "Bachelor" = "Bachelor's",
    "Post-secondary Education" = "ΙΕΚ",
    "High School" = "Λύκειο",
    "Primary Education" = "Χωρίς Δευτεροβάθμια Εκπαίδευση"
  ) %>%
  fct_relevel(
    "Primary","HighSchool", "Post-secondary", 
    "Bachelor", "Master", "PhD"
  )
survey$Studies = factor(survey$Studies, ordered = T)

## Relative Comp

survey$RelativeComp <- if_else((survey$RelativeComp != "Ναι") | (is.na(survey$RelativeComp)), 
                               "Other",
                               "Yes") %>%
  
  as.factor()


# Company

survey$Company <- survey$Company %>%
  fct_recode(
    "1to10" = "1 - 10",
    "101to200" = "101 - 200",
    "11to50" = "11 - 50",
    "201to500" = "201 - 500",
    "501plus" = "501+",
    "51to100" = "51 - 100"
  ) |>
  fct_relevel(
    "1to10", "11to50", "51to100", "101to200", "201to500", "501plus"
  )

survey$Company = factor(survey$Company, ordered = T)


survey = survey  %>%
  mutate(
    TownWork = survey$TownWork %>% remove_accents() %>% tolower(),
    Employer = ifelse(TownWork  %in% list_of_greek_cities, "Greece", ifelse(is.na(TownWork), NA_character_ , "Abroad")),
    TownLive = survey$TownLive %>% remove_accents() %>% tolower(),
    Employee = ifelse(TownLive  %in% list_of_greek_cities, "Greece", ifelse(is.na(TownWork), NA_character_ , "Abroad")),
  ) %>%
  dplyr::select(.,-c(TownWork, TownLive, Timestamp))


## 

survey = survey %>%
  mutate(observation_count = str_count(ProgLang, ", ") + 1)



survey$TypeDev = str_trim(tolower(survey$TypeDev)) 

# survey = survey |>
#   mutate(
#     id = row_number(),
#     isFrontend = ifelse(str_detect(TypeDev, "frontend"), TRUE, FALSE),
#     isBackend = ifelse(str_detect(TypeDev, "backend"), TRUE, FALSE),
#     isDevops = ifelse(str_detect(TypeDev, "devops"), TRUE, FALSE)
#   )
# 
# 
# 
# e <- sapply(strsplit(as.character(survey$TypeDev), ","), function(x) paste0("TypeDev_", x))
# 
# # One-hot encode VarA
# encoded_VarA <- sapply(unique(unlist(e)), function(x) grepl(x, e))
# 
# 
# 

# surveyNEW = bind_cols(survey, encoded_VarA) |>
#   select(-c(TypeDev, ProgLang))


surv = survey |>
  select(-c(TypeDev, ProgLang)) %>%
  mutate(
    AnnualNetSalary = stringr::str_remove(AnnualNetSalary, 
                                          "\\.") %>% as.numeric()
  )


surv$AnnualNetSalary = as.numeric(surv$AnnualNetSalary)

quartiles <- quantile(surv$AnnualNetSalary, probs=c(.25, .75))
IQR <- IQR(surv$AnnualNetSalary)

Lower <- quartiles[1] - 3*IQR
Upper <- quartiles[2] + 3*IQR 

surv <- subset(surv, surv$AnnualNetSalary > Lower & surv$AnnualNetSalary < Upper)

surv = surv |>
  select(-starts_with("ProgLang")) 




readr::write_csv(x = surv, "clean_survey.csv")
