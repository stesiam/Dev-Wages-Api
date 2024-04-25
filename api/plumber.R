library(plumber)
library(rapidoc)
library(dplyr)
library(bundle)

model = readr::read_rds("../devwages.rds")
model = bundle::unbundle(model)

#* @apiTitle Web Developers' Wages
#* @apiDescription An API to predict the annual net salary of Greek Developers.
#* @apiContact list(name = "API Support", url = "https://github.com/stesiam/DevWagesAPI")
#* @apiVersion v1.0
#* @apiTag Predictions Predictions


#* @tag Gender Inequality
#* Return difference in expected net salary between men and women
#* @param YearsProgr:int Years of experience in programming
#* @param PersonalProj:str Do you have any personal projects ? (Yes / No)
#* @param Company:str Size of Company (1to10 / 11to50 / 51to100 / 101to200 / 201to500 / 501plus)
#* @param Supervising:str 
#* @param Employer:str (Greece / Abroad)
#* @param Employee:str (Greece / Abroad)
#* @param WorkType:str (Onsite / Hybrid / Remote)
#* @param Studies:str (Primary / HighSchool / PostSecond / Bachelor / Master / PhD)
#* @param RelativeComp:str (Yes / Other)
#* @param observation_count:int How many programming langiages do you know?
#* @get /predict_gender_inequality_wage
salary = function(YearsProgr, PersonalProj, Company, Supervising,
                  Employer, Employee, WorkType, Studies, RelativeComp,
                  observation_count) {
  
  # Check if any params are missing
  
  user_data_man = dplyr::tibble(
    YearsProgr = as.numeric(YearsProgr),
    PersonalProj = PersonalProj,
    Gender = "Man",
    Company = Company,
    Supervising = Supervising,
    Employer = Employer,
    Employee = Employee,
    WorkType = WorkType,
    Studies = Studies, 
    RelativeComp =RelativeComp,
    observation_count = as.numeric(observation_count)
  )
  
  user_data_woman = dplyr::tibble(
    YearsProgr = as.numeric(YearsProgr),
    PersonalProj = PersonalProj,
    Gender = "Woman",
    Company = Company,
    Supervising = Supervising,
    Employer = Employer,
    Employee = Employee,
    WorkType = WorkType,
    Studies = Studies, 
    RelativeComp = RelativeComp,
    observation_count = as.numeric(observation_count)
  )
  
  expected_annual_salary_men = predict(model, new_data = user_data_man)
  expected_annual_salary_women = predict(model, new_data = user_data_woman)
  
  difference = expected_annual_salary_men - expected_annual_salary_women
  
  return(round( difference ,digits = 1))
}


#* @tag Predictions
#* Return annual net salary ignoring gender
#* @param YearsProgr:int Years of experience in programming
#* @param PersonalProj:str Do you have any personal projects ? (Yes / No)
#* @param Company:str Size of Company (1to10 / 11to50 / 51to100 / 101to200 / 201to500 / 501plus)
#* @param Supervising:str 
#* @param Employer:str (Greece / Abroad)
#* @param Employee:str (Greece / Abroad)
#* @param WorkType:str (Onsite / Hybrid / Remote)
#* @param Studies:str (Primary / HighSchool / PostSecond / Bachelor / Master / PhD)
#* @param RelativeComp:str (Yes / Other)
#* @param observation_count:int How many programming langiages do you know?
#* @get /predict_result_wout_gender
salary = function(YearsProgr, PersonalProj, Company, Supervising,
                  Employer, Employee, WorkType, Studies, RelativeComp,
                  observation_count) {
  
  # Check if any params are missing
  
  
  
  user_data = dplyr::tibble(
    YearsProgr = as.numeric(YearsProgr),
    PersonalProj = PersonalProj,
    Gender = NA,
    Company = Company,
    Supervising = Supervising,
    Employer = Employer,
    Employee = Employee,
    WorkType = WorkType,
    Studies = Studies, 
    RelativeComp =RelativeComp,
    observation_count = as.numeric(observation_count)
  )
  
  predict(model, new_data = user_data)
}


#* @tag Predictions
#* Return the annual net salary
#* @param YearsProgr:int Years of experience in programming
#* @param PersonalProj:str Do you have any personal projects ? (Yes / No)
#* @param Gender:str What is your gender (Man / Woman)
#* @param Company:str Size of Company (1to10 / 11to50 / 51to100 / 101to200 / 201to500 / 501plus)
#* @param Supervising:str 
#* @param Employer:str (Greece / Abroad)
#* @param Employee:str (Greece / Abroad)
#* @param WorkType:str (Onsite / Hybrid / Remote)
#* @param Studies:str (Primary / HighSchool / PostSecond / Bachelor / Master / PhD)
#* @param RelativeComp:str (Yes / Other)
#* @param observation_count:int How many programming langiages do you know?
#* @get /predict_result_with_probs
salary = function(YearsProgr, PersonalProj, Gender, Company, Supervising,
         Employer, Employee, WorkType, Studies, RelativeComp,
         observation_count) {
  
  # Check if any params are missing

  
  
  user_data = dplyr::tibble(
    YearsProgr = as.numeric(YearsProgr),
    PersonalProj = PersonalProj,
    Gender = Gender,
    Company = Company,
    Supervising = Supervising,
    Employer = Employer,
    Employee = Employee,
    WorkType = WorkType,
    Studies = Studies, 
    RelativeComp =RelativeComp,
    observation_count = as.numeric(observation_count)
  )
  
  predict(model, new_data = user_data)
}


# Programmatically alter your API
#* @plumber
function(pr) {
  pr %>%
    # Overwrite the default serializer to return unboxed JSON
    pr_set_docs("rapidoc",
                ## Attributes
                render_style="view",
                theme= "light", 
                layout="column",
                heading_text = "Web Developers API",
                description ="hello"
    )
}
