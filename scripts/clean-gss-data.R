library(tidyverse)
library(haven)
library(readxl)

# The raw GSS data used in this script is not provided in the repository. 
# It can be downloaded at 'http://gss.norc.org/get-the-data/spss'.
# gss_full <- read_sav("data/GSS7218_R2.sav")

# Moral policy issues:
moralpol_issues <- read_rds("data/moral-policy-issues.rds") %>% 
  pull(code)

##############################

gss <- gss_full %>%
  rename_all(tolower) %>% 
  # Combine the different wordings of a given issue:
  mutate(hubbywk1 = coalesce(hubbywk1, hubbywrk),
         twoincs1 = coalesce(twoincs1, twoincs)) %>% 
  # Select moral policy issues and other relevant variables:
  select(id, year, wtssall, oversamp, sample, polviews, wordsum, educ, sex, age, race, news, fincome = realinc,
         one_of(moralpol_issues))

gss <- gss %>% 
  # Remove labels from certain variables:
  mutate_at(vars(id, wtssall, oversamp, age, educ, fincome, year, polviews, wordsum), zap_labels) %>% 
  # Transform remaining labelled variables in factors:
  mutate_if(is.labelled, ~fct_relabel(as_factor(.), tolower)) %>% 
  mutate(birth_year = year - age,
         # Re-scale 'polviews' between 0 (extr. conservative) and 6 (extr. liberal):
         polviews_cont = (polviews - 7)*-1,
         polviews_full = factor(polviews_cont, 
                                levels = c(0:6),
                                labels = c("Extremely\nconservative",
                                           "Conservative",
                                           "Slightly\nconservative", 
                                           "Moderate",
                                           "Slightly\nliberal", 
                                           "Liberal", 
                                           "Extremely\nliberal")),
         wordsum_gr = case_when(wordsum < median(wordsum, na.rm = TRUE) ~ "Below average", 
                                wordsum > median(wordsum, na.rm = TRUE) ~ "Above average", 
                                TRUE ~ NA_character_),
         wordsum_gr = factor(wordsum_gr, levels = c("Above average", "Below average")),
         log_fincome = log(fincome),
         news_cont = as.numeric(news),
         # Re-scale 'news_cont' so that 'everyday' equals 5 and 'never' equals 1:
         news_cont = (news_cont - 6)*(-1),
         wgt = wtssall*oversamp)

# Re-code neutral levels that are not in the middle of factor levels to NA:
gss <- gss %>% 
  mutate(sexeduc = fct_recode(sexeduc, NULL = "depends")) 

# Drop issues about race whenever they were asked to non-blacks only:
for(i in c("racmar")) {
  gss[gss$year %in% 1972:1977, i] <- NA
}

# Drop unused levels:
gss <- gss %>% droplevels()

##############################

# Re-code issues to binary with 1 indicating agreement to the default position: 
# If relevant, the neutral middle category is omitted.

dichotomize <- function(var){
  # For already binary items re-code "yes" (1st level) to 1 and "no" to 0:
  if(nlevels(var) == 2){
    return(2 - as.numeric(var))
  }
  # If item has an even number of levels re-code first half of levels as 1, and second as 0:
  if(nlevels(var) %% 2 == 0){
    return(ifelse(as.numeric(var) > nlevels(var)/2, 0, 1))
  } else {
    # If item has odd number of levels re-code middle level to NA, first half of levels as 1, and second as 0:
    middle <- ceiling(nlevels(var)/2)
    return(case_when(
      as.numeric(var) == middle ~ NA_real_,
      as.numeric(var) < middle ~ 1,
      as.numeric(var) > middle ~ 0,
      TRUE ~ NA_real_)
    )
  }
}

gss <- gss %>% 
  mutate(pornlaw = ifelse(pornlaw == "legal", 0 , 1)) %>% 
  mutate_at(moralpol_issues[moralpol_issues != "pornlaw"], dichotomize)

##############################

# Convert data to long format:
gss_long <- gss %>% 
  select(id:fincome, birth_year:wgt, moralpol_issues) %>% 
  pivot_longer(cols = moralpol_issues,
               names_to = "issue",
               values_to = "opinion") %>% 
  drop_na(polviews, wordsum, educ, sex, age, race, fincome, opinion)

# Create 'id_year' variable and scale continuous variables:
scaled_vars <- gss_long %>%
  distinct(id, year, polviews_cont, wordsum, educ, age, log_fincome, news_cont) %>%
  mutate(id_year = str_c(year, "_", id),
         across(c(polviews_cont, wordsum, educ, age, log_fincome, news_cont), list(sc = ~scale(.)[,1])))

gss_long <- gss_long %>% 
  left_join(scaled_vars) %>% 
  # Center year around earliest survey year:
  mutate(time = (year - 1972)/10)

gss_long <- gss_long %>% 
  select(id, year, time, id_year, wgt, matches("polviews"), matches("wordsum"), matches("educ"), sex, matches("age"), birth_year, race, matches("news"), matches("fincome"), issue, opinion)


write_rds(gss_long, "data/gss_clean.rds", compress = "gz")



