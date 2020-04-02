
# Script to gather data into appropriate structure

# The required format 

# Popn
# Sex
# Age
# mx
# N


pacman::p_load(tidyverse, here, readxl)

lifetables <- read_rds(here("data", "lifetables", "all_ons_lifetables_tidied.rds"))

# So this is the mx part

(pop_mx <- 
lifetables %>% 
  filter(year == max(year)) %>% 
  select(population, x, sex, mx)
)


# Now the N part 

# N for England and Wales 

(pop_ew <- 
  read_excel(here("data", "population", "theanalysisofpopulationestimatestool2018.xlsx"),
             sheet = "MYEB2"
  )
)

(
pop_ew_tidy <- 
  pop_ew %>% 
    select(code, age, sex, population_2018) %>% 
    filter(code %in% c("E", "W")) %>% 
    mutate(
      population = case_when(
        code == "E"  ~ "England",
        code == "W"  ~ "Wales",
        TRUE         ~ NA_character_
      )
    ) %>% 
    mutate(
      sex = case_when(
        sex == 1     ~ "male",
        sex == 2     ~ "female",
        TRUE         ~ NA_character_
      )
    ) %>% 
    rename(N = population_2018) %>% 
    select(population, age, sex, N)
)


# Now the same for Scotland 



pop_scot_loc <- "https://www.opendata.nhs.scot/dataset/7f010430-6ce1-4813-b25c-f7f335bdc4dc/resource/c505f490-c201-44bd-abd1-1bd7a64285ee/download/dz2011-pop-est_30082019.csv"
(dta_scot <- read_csv(pop_scot_loc))


(
  dta_scot_2018 <- 
    dta_scot %>% 
    set_names(nm = tolower(names(.))) %>% 
    mutate(sex = tolower(sex)) %>% 
    filter(dz2011 == "S92000003") %>% 
    filter(year == max(year)) %>% 
    select(-allages, -dz2011, -dz2011qf, -year) %>% 
    gather(key = "age", value = N,  -sex) %>% 
    mutate(age = str_remove(age, "^age") %>% str_remove("plus$") %>% as.numeric()) %>% 
    mutate(population = "Scotland") %>% 
    select(population, age, sex, N)
)


# And Northern Ireland 


pop_ni_tidy <- read_excel(
  here("data", "population", "MYE18_SYA.xlsx"), 
  sheet = "Flat"
) %>% 
  filter(area == "1. Northern Ireland") %>% 
  filter(year == max(year))  %>% 
  #  count(gender) %>% 
  mutate(
    sex = case_when(
      gender == "Males"        ~ "male",
      gender == "Females"      ~ "female",
      TRUE                     ~ NA_character_
    )
  ) %>% 
  filter(!is.na(sex)) %>%
  mutate(population = "Northern Ireland") %>% 
  select(population, age, sex, N = MYE)



(
  pop_struct <- 
    bind_rows(
      dta_scot_2018,
      pop_ew_tidy,
      pop_ni_tidy
    )
)



# Now to join to lifetable


pop_mx %>% 
  rename(age = x) %>% 
  filter(population %in% c("England", "Wales", "Scotland", "Northern Ireland")) %>% 
  inner_join(pop_struct) %>% 
  write_rds(path = here("covid_scenario_explorer", "data", "mx_and_N.rds"))