library(tidyverse)
library(MplusAutomation)
#####################
## Ml-LPA for empath/driver scores
#####################

##################################################################################
# Following six steps from Mäkikangas et al., 2018


##################################################################################
# Preliminary data anlaysis
skimr::skim(cmb_df)

df_lpa <- cmb_df %>%
  select(part_id, mission_day,
         #ends_with('daily_mean')
         contains('_tot_')
         ) |>
  select(!contains('bpm')) %>%
  select(!contains('s_e_2')) |>
  drop_na()
names(df_lpa)
skimr::skim(df_lpa)

df_lpa_md <- df_lpa %>%
  dplyr::select(contains('_tot_')) %>%
  careless::mahad() %>% 
  cbind(df_lpa) %>%
  rename('mahad' = '.') %>%
  filter(mahad <= 50)

# inter-correlations; vars are supposed to be uncorrelated
df_lpa_md %>%
  dplyr::select(contains('_tot_')) %>%
  modelsummary::datasummary_correlation()

# ICC's 
tpd_metrics <- names(df_lpa_md)[str_detect(names(df_lpa_md), 'tot', negate = FALSE)]


for (var in tpd_metrics) {
  f <- as.formula(paste(var,'~ part_id'))
  ICC1 <- multilevel::ICC1(aov(f,data=df_lpa_md))
  print(paste0(var,': ',ICC1))
}
# drop task eda phasic
#tpd_metrics <- tpd_metrics[str_detect(tpd_metrics, 'task_eda_eda_tonic', negate = TRUE)]


##################################################################################
# Prep data for Mplus analysis

# https://cran.r-project.org/web/packages/MplusAutomation/vignettes/vignette.html

out_f_dir <- 'fifth_try'

ml_lpa_df <- df_lpa_md |>
  # rename('PCP_ID' = 'provider_id') %>%
  #dplyr::select(all_of(tpd_metrics)) |>
  dplyr::select(all_of(tpd_metrics),mission_day) |>
  rename(
    mis_day = mission_day,
    e_t_s_e = mean_tot_social_eda_eda_tonic_empath_scores_2,
    e_t_s_d = mean_tot_social_eda_eda_tonic_driver_2,
    e_p_s_e = mean_tot_social_eda_eda_phasic_empath_scores_2,
    e_p_s_d = mean_tot_social_eda_eda_phasic_driver_2,
    e_t_t_e = mean_tot_team_task_eda_eda_tonic_empath_scores_2,
    e_t_t_d = mean_tot_team_task_eda_eda_tonic_driver_2,
    e_p_t_e = mean_tot_team_task_eda_eda_phasic_empath_scores_2,
    e_p_t_d = mean_tot_team_task_eda_eda_phasic_driver_2,
    c_s_d = mean_tot_social_cardiac_ibi_driver_2,
    c_s_e = mean_tot_social_cardiac_ibi_empath_scores_2,
    c_t_d = mean_tot_team_task_cardiac_ibi_driver_2,
    c_t_e = mean_tot_team_task_cardiac_ibi_empath_scores_2
  )
# dplyr::select(drives, cognition, affect, social, big_words, pcp_to_pt_wc, PCP_ID, row_num)

df_prep <- df_lpa_md %>%
  # rename('PCP_ID' = 'provider_id') %>%
  #dplyr::select(all_of(tpd_metrics)) |>
  dplyr::select(all_of(tpd_metrics),mission_day) |>
  rename(
    mis_day = mission_day,
    e_t_s_e = mean_tot_social_eda_eda_tonic_empath_scores_2,
    e_t_s_d = mean_tot_social_eda_eda_tonic_driver_2,
    e_p_s_e = mean_tot_social_eda_eda_phasic_empath_scores_2,
    e_p_s_d = mean_tot_social_eda_eda_phasic_driver_2,
    e_t_t_e = mean_tot_team_task_eda_eda_tonic_empath_scores_2,
    e_t_t_d = mean_tot_team_task_eda_eda_tonic_driver_2,
    e_p_t_e = mean_tot_team_task_eda_eda_phasic_empath_scores_2,
    e_p_t_d = mean_tot_team_task_eda_eda_phasic_driver_2,
    c_s_d = mean_tot_social_cardiac_ibi_driver_2,
    c_s_e = mean_tot_social_cardiac_ibi_empath_scores_2,
    c_t_d = mean_tot_team_task_cardiac_ibi_driver_2,
    c_t_e = mean_tot_team_task_cardiac_ibi_empath_scores_2
  ) %>%
  # dplyr::select(drives, cognition, affect, social, big_words, pcp_to_pt_wc, PCP_ID, row_num) %>%
  prepareMplusData(df = .,
                   filename = glue::glue('mplus_analyses/{out_f_dir}/profile_mPlus.dat')
  )

# formatting for MPLUS scrit
tpd_metrics_short <- c('e_t_s_e','e_t_s_d','e_p_s_e','e_p_s_d',
                       'e_t_t_e','e_t_t_d',
                       'e_p_t_e\n', # breaks line so it fits in MPLUS
                       'e_p_t_d',
                       'c_s_d','c_s_e','c_t_d','c_t_e')

tpd_string <- paste(unlist(tpd_metrics_short), collapse = ' ')
tpd_string <- str_replace_all(tpd_string, "\n ", "\n")

#### Automating MPLUS models

class_str <- character()

ml_lpa1_10 <- lapply(1:5, function(k)
{
  
  for (x in 1:k) {
    if (x == 1) {
      class_str <<- glue::glue("%C#{x}%
[{tpd_string}];
{tpd_string};")
    } else {
      class_str <<- paste(class_str,
                          glue::glue("%C#{x}%
[{tpd_string}];
{tpd_string};"
                          ), sep="\n")
    }
    print(class_str)
  }
  
  lca_enum <- MplusAutomation::mplusObject(
    TITLE = glue::glue("L1_P_{k}_lpa_freeVar;"),
    VARIABLE = glue::glue("CLASSES = c({k});"),
    # USEVARIABLES = "drives cognition affect social big_words wc;",
    DEFINE = glue::glue("STANDARDIZE {tpd_string};"),
    ANALYSIS = "TYPE = MIXTURE;
    ESTIMATOR=MLR;
    STARTS=1000 50;
    STITERATIONS=50;
    LRTSTARTS=1000 50 1000 50;
    PROCESSORS=12;",
    MODEL = glue::glue('%OVERALL%\nc ON mis_day;\n',class_str),
    OUTPUT = "SAMPSTAT STANDARDIZED MOD (5.00) TECH7 TECH11 TECH13 TECH14;",
    # PLOT = "type = plot3;",
    usevariables = colnames(ml_lpa_df),
    rdata = ml_lpa_df
  )
  
  ml_lpa_fit <- MplusAutomation::mplusModeler(lca_enum,
                                              dataout = glue::glue("mplus_analyses/{out_f_dir}/L1_prof_{k}_ml_lpa_freeVar_enum.dat"),
                                              modelout = glue::glue("mplus_analyses/{out_f_dir}/L1_prof_{k}_ml_lpa_freeVar_enum.inp"),
                                              check = TRUE, run = TRUE, hashfilename = FALSE)
}
)

output_enum <- MplusAutomation::readModels(here::here(glue::glue("mplus_analyses/{out_f_dir}")), quiet = TRUE)
enum_summary <- LatexSummaryTable(output_enum,
                                  keepCols=c("Title",
                                             "LL",
                                             "BIC",
                                             "aBIC"),
                                  sortBy = "Title")
enum_summary %>%
  gt::gt()

l1_k_profiles_plot <- enum_summary %>%
  as.data.frame() %>%
  filter(str_detect(Title, pattern = 'L1_')) %>%
  # filter(str_detect(Title, pattern = 'L2', negate = TRUE)) %>%
  pivot_longer(
    !Title, names_to = 'criteria', values_to = 'value'
  ) %>%
  mutate(
    num_profiles = readr::parse_number(str_remove(Title,'L1_P_'))
  ) %>%
  filter(criteria != 'LL') %>%
  ggplot(aes(x = num_profiles, y = value, group = criteria, color = criteria)) + 
  geom_point() + geom_line() + ggthemes::geom_rangeframe() + ggthemes::theme_tufte() + 
  scale_x_continuous(breaks= scales::pretty_breaks()) + theme(legend.position = 'bottom') +
  labs(
    title='Model fit by number of daily (L1) profiles',
    x = 'Number of daily (L1) profiles',
    y = 'Model fit statistic value',
    color = 'Fit statistic')

################################
######## Check for fit for different number of L2 vars
################################

out_f_dir <- 'fourth_try'

ml_lpa_df <- df_lpa_md %>%
  # rename('PCP_ID' = 'provider_id') %>%
  dplyr::select(all_of(tpd_metrics),part_id,mission_day) %>%
  mutate(row_num = row_number()) %>%
  rename(
    mis_day = mission_day,
    e_t_s_e = mean_tot_social_eda_eda_tonic_empath_scores_2,
    e_t_s_d = mean_tot_social_eda_eda_tonic_driver_2,
    e_p_s_e = mean_tot_social_eda_eda_phasic_empath_scores_2,
    e_p_s_d = mean_tot_social_eda_eda_phasic_driver_2,
    e_t_t_e = mean_tot_team_task_eda_eda_tonic_empath_scores_2,
    e_t_t_d = mean_tot_team_task_eda_eda_tonic_driver_2,
    e_p_t_e = mean_tot_team_task_eda_eda_phasic_empath_scores_2,
    e_p_t_d = mean_tot_team_task_eda_eda_phasic_driver_2,
    c_s_d = mean_tot_social_cardiac_ibi_driver_2,
    c_s_e = mean_tot_social_cardiac_ibi_empath_scores_2,
    c_t_d = mean_tot_team_task_cardiac_ibi_driver_2,
    c_t_e = mean_tot_team_task_cardiac_ibi_empath_scores_2
  )
# dplyr::select(drives, cognition, affect, social, big_words, pcp_to_pt_wc, PCP_ID, row_num)

df_prep <- df_lpa_md %>%
  # rename('PCP_ID' = 'provider_id') %>%
  dplyr::select(all_of(tpd_metrics),part_id,mission_day) %>%
  rename(
    mis_day = mission_day,
    e_t_s_e = mean_tot_social_eda_eda_tonic_empath_scores_2,
    e_t_s_d = mean_tot_social_eda_eda_tonic_driver_2,
    e_p_s_e = mean_tot_social_eda_eda_phasic_empath_scores_2,
    e_p_s_d = mean_tot_social_eda_eda_phasic_driver_2,
    e_t_t_e = mean_tot_team_task_eda_eda_tonic_empath_scores_2,
    e_t_t_d = mean_tot_team_task_eda_eda_tonic_driver_2,
    e_p_t_e = mean_tot_team_task_eda_eda_phasic_empath_scores_2,
    e_p_t_d = mean_tot_team_task_eda_eda_phasic_driver_2,
    c_s_d = mean_tot_social_cardiac_ibi_driver_2,
    c_s_e = mean_tot_social_cardiac_ibi_empath_scores_2,
    c_t_d = mean_tot_team_task_cardiac_ibi_driver_2,
    c_t_e = mean_tot_team_task_cardiac_ibi_empath_scores_2
  ) %>%
  mutate(row_num = row_number()) %>%
  # dplyr::select(drives, cognition, affect, social, big_words, pcp_to_pt_wc, PCP_ID, row_num) %>%
  prepareMplusData(df = .,
                   filename = glue::glue('mplus_analyses/{out_f_dir}/profile_mPlus.dat')
  )
#tpd_metrics_short <- c('eda_t_e','eda_t_d','eda_p_e','eda_p_d','ibi_d','ibi_e')
class_str <- character()

k <- 3 # L1 profiles
ml_lpa2 <- lapply(1:6, function(j) # L2 profiles
{
  
  for (x in 1:k) {
    if (x == 1) {
      class_str <<- glue::glue("%C#{x}%
[{tpd_string}];
 {tpd_string};")
    } else {
      class_str <<- paste(class_str,
                          glue::glue("%C#{x}%
[{tpd_string}];
 {tpd_string};"
                          ), sep="\n")
    }
    print(class_str)
  }
  
  lca_enum <- MplusAutomation::mplusObject(
    TITLE = glue::glue("L2_{j}_P_{k}_lpa_freeVar;"),
    VARIABLE = glue::glue("IDVARIABLE IS row_num;\nCLASSES = BC({j}) c({k});\nCLUSTER IS part_id;\nWITHIN ARE  mis_day {tpd_string};\nBETWEEN ARE BC;"),
    DEFINE = glue::glue("STANDARDIZE  {tpd_string};"),
    ANALYSIS = "TYPE = MIXTURE TWOLEVEL;
    ESTIMATOR=MLR;
    STARTS=1000 200;
    STITERATIONS=50;
    LRTSTARTS=1000 50 1000 50;
    PROCESSORS=12;",
    MODEL = glue::glue('%WITHIN%\n%OVERALL%\nc ON mis_day;\n%BETWEEN%\n%OVERALL%\nc ON BC;\nMODEL c:\n%WITHIN%\n',class_str),
    OUTPUT = "SAMPSTAT STANDARDIZED MOD (5.00) TECH7 TECH11 TECH13 TECH14;",
    # PLOT = "type = plot3;",
    SAVEDATA = glue::glue("file=mlLpa_L2_{j}_L1_{k}.dat;\nsave=cprob;\nTECH4 IS tech4.dat;"),
    usevariables = colnames(ml_lpa_df),
    rdata = ml_lpa_df
  )
  
  ml_lpa_fit <- MplusAutomation::mplusModeler(lca_enum,
                                              dataout = glue::glue("mplus_analyses/{out_f_dir}/L2_{j}_P_{k}_ml_lpa_freeVar.dat"),
                                              modelout = glue::glue("mplus_analyses/{out_f_dir}/L2_{j}_P_{k}_ml_lpa_freeVar.inp"),
                                              check = TRUE, run = TRUE, hashfilename = FALSE)
}
)


output_enum <- MplusAutomation::readModels(here::here(glue::glue("mplus_analyses/{out_f_dir}")), quiet = TRUE)
enum_summary <- LatexSummaryTable(output_enum,
                                  keepCols=c("Title",
                                             "LL",
                                             "BIC",
                                             "aBIC"),
                                  sortBy = "Title")
enum_summary %>%
  gt::gt()

k <- 3
l2_k_range_plot <- enum_summary %>%
  as.data.frame() %>%
  filter(str_detect(Title,pattern = 'L1_', negate = TRUE)) %>%
  filter(str_detect(Title, pattern = glue::glue('_P_{k}_'))) %>%
  pivot_longer(
    !Title, names_to = 'criteria', values_to = 'value'
  ) %>%
  mutate(
    num_profiles = readr::parse_number(str_remove(Title,'L2_'))
  ) %>%
  filter(criteria != 'LL') %>%
  ggplot(aes(x = num_profiles, y = value, group = criteria, color = criteria)) + 
  geom_point() + geom_line() + ggthemes::geom_rangeframe() + ggthemes::theme_tufte() + 
  scale_x_continuous(breaks= scales::pretty_breaks()) + theme(legend.position = 'bottom') +
  labs(
    title='Model fit by number of clinician (L2) classes \nfor a 4 L1 profile model',
    x = 'Number of clinician (L2) classes',
    y = 'Model fit statistic value',
    color = 'Fit statistic')

library(patchwork)

l1_k_profiles_plot + l2_k_range_plot + plot_layout(guides = "collect")  & theme(legend.position = 'bottom') & plot_annotation(tag_levels = 'A')
# test <- MplusAutomation::getSavedata_Fileinfo(glue::glue('mplus_analyses/psych_process/L2_{L2}_P_{L1}_ml_lpa_freeVar.out')) 
k <- 2
mlLPA_results <- MplusAutomation::readModels(glue::glue("mplus_analyses/{out_f_dir}/L2_{k}_P_3_ml_lpa_freeVar.out"), what="savedata")$savedata
skimr::skim(mlLPA_results)
# L! bar charts faceted by profile
l1_by_tpd_plot <- mlLPA_results %>%
  rename('L1' = 'C',
         'L2' = 'BC') %>%
  dplyr::select(E_T_S_E:C_T_E, L1) %>%
  drop_na() %>%
  # rename('row_num' = ROW_NUM) %>%
  # select(-starts_with("V")) %>%
  pivot_longer(
    cols = E_T_S_E:C_T_E,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    L1 = as.factor(L1)#,
    # variable = ordered(tolower(variable), levels = c("EDA_T_E", "EDA_T_D","EDA_P_E","EDA_P_D","IBI_D","IBI_E"))
  ) %>%
  group_by(L1, variable) %>%
  summarize(
    m = mean(value),
    sd = sd(value, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = variable, y = m, fill = variable)) + geom_col() + facet_grid(~L1) + 
  ggthemes::theme_tufte() + theme(
    legend.position="bottom",
    panel.background = element_rect(fill = NA, color = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  ) + labs(fill = 'Linguistic dimension',
           y = 'Mean standardized value',
           x = 'Linguistic dimension',
           title = 'Linguistic dimensions by interaction (L1) profile')

# L2 profiles by L1 composition
l2_by_l1_plot <- mlLPA_results %>% 
  rename('L1' = 'C',
         'L2' = 'BC') %>%
  janitor::clean_names() %>%
  select(l1,l2) %>%
  mutate(
    l1 = as.factor(l1),
    l2 = as.factor(l2)
  ) %>%
  group_by(l2,l1) %>%
  summarize(l1_count = n())%>%
  ungroup() %>%
  group_by(l2) %>%
  mutate(l1_perc = l1_count / sum(l1_count)) %>%
  ungroup() %>%
  ggplot(aes(fill=l1, y=l1_perc, x=l2)) + 
  geom_bar(position="fill", stat="identity") + ggthemes::theme_tufte() + 
  theme(legend.position="bottom") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = 'Frequency of L1 interaction profiles in L2 clinician classes',
    x = 'L2 clinician class',
    y = 'Percentage of L1 profiles',
    fill = 'L1 profile'
  )