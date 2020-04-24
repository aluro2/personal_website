
library(tidyverse)
library(vitae)
library(rorcid)
library(glue)

orcid_auth()

edu <- 
  rorcid::orcid_educations("0000-0003-2423-5691") %>% 
  .$`0000-0003-2423-5691` %>% 
  .$`affiliation-group` %>% 
  .$summaries %>% 
  do.call(bind_rows, .) %>% 
  mutate(`education-summary.end-date.year.value` = replace_na(`education-summary.end-date.year.value`, "Present")) %>% 
  detailed_entries(
    what = `education-summary.role-title`,
    when = glue::glue("{`education-summary.start-date.year.value`} - {`education-summary.end-date.year.value`}"),
    with = `education-summary.organization.name`,
    where = paste(`education-summary.organization.address.city`, `education-summary.organization.address.region`, sep = ", ")
  ) 

saveRDS(edu, "CV/edu.RDS")

pubs <-
  scholar::get_publications("t1t2_RkAAAAJ") %>% 
    detailed_entries(
      what = title,
      when = year,
      with = author,
      where = journal,
      why = as.character(number)
    ) %>% 
  arrange(desc(when))

saveRDS(pubs, "CV/pubs.RDS")

