################################################
################################################
##### Read in and clean all relevant data for HERA analyses
################################################
################################################


#####################
## Physio
#####################

getPhysio <- function(f,data_dir) {
  df <- read.csv(here::here(data_dir,f)) %>%
    select(-X) %>%
    rename(mission_day = md) %>%
    janitor::clean_names()
  return(df)
}

#####################
## HERA IDs
#####################

getAndMergeHERA_IDs <- function(physio_df,data_dir,role_file) {
  roles <- readxl::read_excel(here::here(data_dir,role_file)) %>%
    mutate(Role = tolower(Role)) %>%
    rename(team = Mission, part_id = ID, role = Role) %>%
    mutate(part_id = as.character(part_id))
  physio_df <- physio_df %>% left_join(roles,by = c('role' = 'role', 'team' = 'team'))
  return(physio_df)
}

getAggPhysioMetrics <- function(df) {
  df <- df %>%
    select(
      part_id,task_num,team,mission_day,task_category,
      eda_eda_tonic_empath_scores_2, eda_eda_tonic_driver_2,
      eda_eda_phasic_empath_scores_2, eda_eda_phasic_driver_2,
      cardiac_bpm_empath_scores_2, cardiac_bpm_driver_2,
      cardiac_ibi_driver_2, cardiac_ibi_empath_scores_2,
      eda_eda_tonic_s_e_2, eda_eda_phasic_s_e_2,
      cardiac_bpm_s_e_2,cardiac_ibi_s_e_2
      # ends_with("_2")
      ) 
  task_cat_df <- df %>%
    group_by(team,mission_day,part_id,task_category) %>%
    summarize(across(ends_with("_2"), ~ mean(.x, na.rm = TRUE))
    ) %>%
    pivot_wider(
      id_cols = c(team,mission_day,part_id),
      names_from = task_category,
      values_from = ends_with("_2"),
      names_glue = "mean_{task_category}_{.value}"
    )
  
  task_cat_df_2 <- df |>
    mutate(task_category_v2 = case_when(
      str_detect(task_category, 'team|dyad') ~ 'tot_team_task',
      str_detect(task_category,'social') ~ 'tot_social',
      .default = task_category
    )) |>
      group_by(team,mission_day,part_id,task_category_v2) %>%
      summarize(across(ends_with("_2"), ~ mean(.x, na.rm = TRUE))
      ) |>
      pivot_wider(
        id_cols = c(team,mission_day,part_id),
        names_from = task_category_v2,
        values_from = ends_with("_2"),
        names_glue = "mean_{task_category_v2}_{.value}"
      )
  
  daily_sum_df <- df %>%
    select(-task_category) %>%
    group_by(team,mission_day,part_id) %>%
    summarize(across(ends_with("_2"), ~ mean(.x, na.rm = TRUE), .names = '{.col}_daily_mean'))
  df <- daily_sum_df |>
    full_join(task_cat_df, by = c('team','mission_day','part_id')) |>
    full_join(task_cat_df_2, by = c('team','mission_day','part_id'))
  
  return(df)
}

#####################
## HERA Surveys
#####################

getTeamPerfSurveys <- function(f_list) {
  first <- TRUE
  for (f in f_list) {
    df <- readxl::read_excel(f, sheet = 'Data', col_names = TRUE) %>%
      filter(MissionPhase_Nom == 'In-Mission') %>%
      mutate(
        mission_day = as.numeric(gsub('MD',"",MissionDay_Nom)),
        team = gsub('HERA',"",ID_Crew)) %>%
      rename(part_id = ID) %>%
      mutate(part_id = as.character(part_id))
    if (first) {
      first <- FALSE
      team_perf <- df
      } else {team_perf <- rbind(team_perf,df)}
  }
  return(janitor::clean_names(team_perf))
}

get_traits <- function(data_dir,jh_f,ipipList) {
  trait_df <- read.csv(here::here(data_dir,jh_f)) %>%
    rename(part_id = participant_id) %>%
    mutate(part_id = as.character(part_id))
  first <- TRUE
  for (f in ipipList) {
    df <- readxl::read_excel(f,sheet = 'Data', col_names = TRUE) %>%
      dplyr::select(ID, IPIP_Neuroticism, IPIP_Extraversion, IPIP_Openness, IPIP_Agreeableness, IPIP_Conscientiousness) %>%
      rename(part_id = ID) %>%
      mutate(part_id = as.character(part_id))
    print(f)
    print(names(df))
    if (first) {
      first <- FALSE
      ipip_df <- df
    } else {ipip_df <- rbind(ipip_df,df)}
  }
  trait_df <- trait_df %>% full_join(ipip_df,by='part_id')
  return(janitor::clean_names(trait_df))
}

getCohesion <- function(f_list) {
  first <- TRUE
  for (f in f_list) {
    df <- readxl::read_excel(f, sheet = 'Data', col_names = TRUE) %>%
      filter(MissionPhase_Nom == 'In-Mission') %>%
      mutate(Mission_day = as.numeric(gsub('MD',"",MissionDay_Nom))) %>%
      dplyr::select(ID, Mission_day, CohesionTask_Indiv, CohesionSoc_Indiv, CohesionOverall_Indiv, CohesionTask_TeamFinal, CohesionSoc_TeamFinal, CohesionOverall_TeamFinal) %>%
      rename(part_id = ID) %>%
      mutate(part_id = as.character(part_id))
    if (first) {
      cohese_df <- df
      first <- FALSE
    } else {cohese_df <- rbind(cohese_df,df)}
  }
  return(janitor::clean_names(cohese_df))
}

getWorkload <- function(f_list) {
  first <- TRUE
  for (f in f_list) {
    df <- readxl::read_excel(f, sheet = 'Data', col_names = TRUE) %>%
      filter(MissionPhase_Nom == 'In-Mission') %>%
      mutate(Mission_day = as.numeric(gsub('MD',"",MissionDay_Nom))) %>%
      dplyr::select(ID, Mission_day, Workload, Sleep_Interfere, Sleep_OtherPerf) %>%
      rename(part_id = ID) %>%
      mutate(part_id = as.character(part_id))
    if (first) {
      first <- FALSE
      wl_df <- df
    } else {wl_df <- rbind(wl_df,df)}
  } 
  return(janitor::clean_names(wl_df))
}

#####################
## RUN SCRIPTS
#####################

physio_df <- getPhysio(
  f = config$physio_df_file,
  data_dir = config$data_dir)
physio_df <- getAndMergeHERA_IDs(
    physio_df = physio_df,
    data_dir = config$data_dir,
    role_file = config$hera_roles_ID_file) 
physio_df <-getAggPhysioMetrics(physio_df)

# physio_df %>% select(starts_with('cardiac')) %>% select(-ends_with('s_e_2')) %>% #skimr::skim(.)
#   modelsummary::datasummary_correlation()

trait_df <- get_traits(
  data_dir = config$data_dir,
  jh_f = config$jh_trait_file,
  ipipList = list.files(here::here(config$data_dir,config$ipip_dir), full.names = TRUE)
)

team_perf_df <- getTeamPerfSurveys(
  f_list = list.files(here::here(config$data_dir,config$team_perf_dir), full.names = TRUE)
)

cohese_df <- getCohesion(
  f_list = list.files(here::here(config$data_dir,config$cohese_dir), full.names = TRUE)
)

wl_df <- getWorkload(
  f_list = list.files(here::here(config$data_dir,config$wl_dir), full.names = TRUE)
)

survey_df <- team_perf_df %>%
  full_join(cohese_df, by = c('part_id','mission_day')) %>%
  full_join(wl_df, by = c('part_id','mission_day'))

cmb_df <- survey_df %>% full_join(physio_df, by = c('team','mission_day','part_id'))

test <- trait_df %>% select(part_id,ends_with('mlq')) %>% drop_na()
setdiff(
  unique(physio_df$part_id),
  unique(test$part_id)
)