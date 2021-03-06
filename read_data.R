library(tidyverse)
library(stringr)

redcap_export <-
  read_csv("data/redcap/spins.csv") %>%
  mutate(new_id = paste(record_id, "01", sep = "_"),
         diagnosis = case_when(redcap_event_name == "case_arm_2" ~ "Case",
                               redcap_event_name == "control_arm_1" ~ "Control")) %>%
  select(new_id, diagnosis, mri_doa)

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
  left_join(redcap_export) %>%
  mutate(date = as.Date(ifelse(date == "1900-01-01", mri_doa, 
                               ifelse(date == "1990-01-01", mri_doa, date)), origin = "1970-01-01")) %>%
  select(-mri_doa) %>%
  mutate(diagnosis = ifelse(study %in% c("SPASD", "CLZ") & str_detect(participant_id, "P00"), "Pilot",
                            ifelse(study == "SPASD" & participant_id %in% c("0007"), "Control",
                                   ifelse(study == "SPINS" & str_detect(participant_id, "P00"), "Travelling Phantom",
                                          ifelse(study == "SPINS" & str_detect(participant_id, "P99"), "Pilot",
                                                 ifelse(study == "SPINS" & participant_id %in% c("0178"), "Control",
                                                        ifelse(study == "OPT" & str_detect(participant_id, "UT9999"), "Pilot",
                                                               ifelse(study == "CLZ" & str_detect(participant_id, "^6"), "Control",
                                                                      diagnosis))))))),
         diagnosis = ifelse(is.na(diagnosis), "Case", diagnosis))

anat_report <-
  list.files(path = "data/mriqc",
             pattern = ".*_T.w.tsv",
             full.names = TRUE) %>%
  map_df(function(x) read_tsv(x) %>% mutate(filename = gsub(".tsv", "", basename(x)))) %>%
  separate(filename, c("study", "scan_type"), sep = "_") %>%
  mutate(modality = "anat",
         full_subject_id = str_match(bids_name, "sub-([[:alnum:]]+)_")[,2],
         session_id = str_match(bids_name, "ses-([[:alnum:]]+)_")[,2],
         acquisition = str_match(bids_name, "acq-([[:alnum:]]+)_")[,2],
         run_num = str_match(bids_name, "run-([[:alnum:]]+)_")[,2],
         study = str_to_upper(study),
         site = substr(full_subject_id, 1, 3),
         subject_id = substr(full_subject_id, 4, 10)) %>%
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
             pattern = ".*bold.tsv",
             full.names = TRUE) %>%
  map_df(function(x) read_tsv(x) %>% mutate(filename = gsub(".tsv", "", basename(x)))) %>%
  separate(filename, c("study", "fd_threshold", "modality"), sep = "_") %>%
  mutate(full_subject_id = str_match(bids_name, "sub-([[:alnum:]]+)_")[,2],
         session_id = str_match(bids_name, "ses-([[:alnum:]]+)_")[,2],
         acquisition = str_match(bids_name, "acq-([[:alnum:]]+)_")[,2],
         run_num = str_match(bids_name, "run-([[:alnum:]]+)_")[,2],
         scan_type = str_match(bids_name, "task-([[:alnum:]]+)_")[,2],
         study = str_to_upper(study),
         site = substr(full_subject_id, 1, 3),
         subject_id = substr(full_subject_id, 4, 10)) %>%
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
