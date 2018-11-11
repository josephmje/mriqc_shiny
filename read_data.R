library(tidyverse)
library(stringr)

xnat_sessions <-
  # read in all csv files exported from XNAT
  map_df(
    list.files(path = "data/xnat",
               pattern = ".*csv",
               full.names = TRUE),
    read_csv) %>%
  # filter out phantom scans
  filter(!grepl("_PHA_", subject_id)) %>%
  separate(subject_id, c("study_raw", "site", "participant_id", "visit", "session"), "_") %>%
  mutate(new_id = paste(study_raw, site, participant_id, visit, sep = "_")) %>%
  select(-c(study_raw, session)) %>%
  unique() %>%
  group_by(new_id) %>%
  top_n(1, date) %>%
  arrange(new_id) %>%
  mutate(diagnosis = case_when(study %in% c("SPASD", "CLZ") & str_detect(participant_id, "P00") ~ "Pilot",
                               study == "SPASD" & participant_id %in% c("0007") ~ "Control",
                               study == "SPINS" & str_detect(participant_id, "P00") ~ "Travelling Phantom",
                               study == "SPINS" & str_detect(participant_id, "P99") ~ "Pilot",
                               study == "SPINS" & participant_id %in% c("0178") ~ "Control",
                               study == "OPT" & str_detect(participant_id, "UT9999") ~ "Pilot",
                               study == "CLZ" & str_detect(participant_id, "^6") ~ "Control"),
         diagnosis = ifelse(is.na(diagnosis), "Case", diagnosis))

anat_report <-
  list.files(path = "data/mriqc",
             pattern = ".*_T.w.csv",
             full.names = TRUE) %>%
  map_df(function(x) read_csv(x) %>% mutate(filename = gsub(".csv", "", basename(x)))) %>%
  separate(filename, c("study", "scan_type"), sep = "_") %>%
  mutate(modality = "anat",
         study = str_to_upper(study),
         site = substr(subject_id, 1, 3),
         subject_id = substr(subject_id, 4, 10)) %>%
  left_join(xnat_sessions,
            by = c("subject_id" = "participant_id",
                   "session_id" = "visit",
                   "study",
                   "site")) %>%
  mutate(site = ifelse(study == "OPT" & site == "UT2", "CMP",
                       ifelse(study == "OPT" & site == "UT1", "CMH",
                              ifelse(study == "CLZ" & site == "ZHH", "ZHP",
                                     site))))

bold_report <-
  list.files(path = "data/mriqc",
             pattern = ".*bold.csv",
             full.names = TRUE) %>%
  map_df(function(x) read_csv(x) %>% mutate(filename = gsub(".csv", "", basename(x)))) %>%
  separate(filename, c("study", "fd_threshold", "modality"), sep = "_") %>%
  rename(scan_type = task_id) %>%
  mutate(study = str_to_upper(study),
         site = substr(subject_id, 1, 3),
         subject_id = substr(subject_id, 4, 10)) %>%
  left_join(xnat_sessions,
            by = c("subject_id" = "participant_id",
                   "session_id" = "visit",
                   "study",
                   "site")) %>%
  mutate(site = ifelse(study == "OPT" & site == "UT2", "CMP",
                       ifelse(study == "OPT" & site == "UT1", "CMH",
                              ifelse(study == "CLZ" & site == "ZHH", "ZHP",
                                     site))),
         fd_threshold = ifelse(fd_threshold == "02mm", "0.2 mm",
                               ifelse(fd_threshold == "05mm", "0.5 mm",
                                      NA)))

mriqc<-
  bind_rows(anat_report %>%
              gather(metric, measurement, cjv:wm2max),
            bold_report %>%
              gather(metric, measurement, aor:tsnr))

cols <- c("study", "scan_type", "modality", "metric", "fd_threshold", "site", "diagnosis")
mriqc[cols] <- lapply(mriqc[cols], as_factor)
