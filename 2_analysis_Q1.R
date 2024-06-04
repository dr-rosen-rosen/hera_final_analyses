################################################
################################################
##### Analyses for Question 1: tpd -> outcomes
################################################
################################################

library(tidyverse)
library(sjPlot)
library(lme4)

#####################
## Final data prep
#####################

skimr::skim(cmb_df)
q1_df <- cmb_df %>%
  select(campaign,mission,id_crew,part_id,role,mission_day,
         starts_with('team_perf'),starts_with('cohesion_'),
         workload,
         ends_with('_daily_mean'), # all daily tpd aggregates
         contains('social')# add task type categories
         ) %>%
  mutate(
    mission_day = mission_day-1, #starts md at 0
    id_crew = as.factor(id_crew),
    part_id = as.factor(part_id)
  ) %>% 
  mutate(across(ends_with('_daily_mean'), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE)))) %>%
  mutate(across(starts_with('team_perf'), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE)))) %>%
  mutate(across(starts_with('cohesion'), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE)))) 

q1_df <- q1_df %>% drop_na()
#####################
## LME4 analysis for INDIVIDUAL outcomes
#####################

m.coh.overall.ind.eda <- lme4::lmer(cohesion_overall_indiv ~
                      mission_day +
                        
                        # poly(mission_day,2) +
                      # poly(mission_day,2) +
                      # eda_eda_tonic_empath_scores_2_daily_mean +
                      # eda_eda_tonic_driver_2_daily_mean +
                      #   eda_eda_phasic_empath_scores_2_daily_mean +
                      #   eda_eda_phasic_driver_2_daily_mean +
                      # eda_eda_tonic_s_e_2_daily_mean +
                      # eda_eda_phasic_s_e_2_daily_mean +
                      # (1|part_id),# + 
                        mean_social_eda_eda_phasic_empath_scores_2 +
                        mean_social_eda_eda_phasic_driver_2 +
                        mean_social_eda_eda_tonic_empath_scores_2 +
                        mean_social_eda_eda_tonic_driver_2 +
                        mean_social_cardiac_ibi_driver_2 +
                        mean_social_cardiac_ibi_empath_scores_2 +
                        (1|id_crew/part_id),
                        # (1|id_crew),
                      data = q1_df,
                      #REML = FALSE,
                      control = lme4::lmerControl(optimizer ="Nelder_Mead"))
summary(m.coh.overall.ind.eda) 
sjPlot::tab_model(m.coh.overall.ind.eda)
performance::check_model(m.coh.overall.ind.eda)
lme4::allFit(m.coh.overall.ind.eda)

m.coh.overall.ind.card <- lme4::lmer(cohesion_overall_indiv ~
                                      mission_day +
                                       # poly(mission_day,2) +
                                       # cardiac_bpm_empath_scores_2_daily_mean +
                                       # cardiac_bpm_driver_2_daily_mean +
                                       cardiac_ibi_empath_scores_2_daily_mean +
                                       cardiac_ibi_driver_2_daily_mean +
                                      # (1|part_id), #+ 
                                       
                                       (1|id_crew/part_id),
                                       #(1|id_crew),
                                    data = q1_df,
                                    #REML = FALSE,
                                    control = lme4::lmerControl(optimizer ="Nelder_Mead"))
summary(m.coh.overall.ind.card) 
sjPlot::tab_model(m.coh.overall.ind.card)
performance::check_model(m.coh.overall.ind.card)
lme4::allFit(m.coh.overall.ind.card)

m.coh.overall.ind.combined <- lme4::lmer(cohesion_overall_indiv ~
                                      mission_day +
                                      eda_eda_tonic_empath_scores_2_daily_mean +
                                      eda_eda_tonic_driver_2_daily_mean +
                                      eda_eda_phasic_empath_scores_2_daily_mean +
                                      eda_eda_phasic_driver_2_daily_mean +
                                        cardiac_ibi_empath_scores_2_daily_mean +
                                        cardiac_ibi_driver_2_daily_mean +
                                      # eda_eda_tonic_s_e_2_daily_mean +
                                      # eda_eda_phasic_s_e_2_daily_mean +
                                      # (1|part_id),# + 
                                        
                                        (1|id_crew/part_id),
                                    # (1|id_crew),
                                    data = q1_df,
                                    #REML = FALSE,
                                    control = lme4::lmerControl(optimizer ="Nelder_Mead"))
summary(m.coh.overall.ind.combined) 
sjPlot::tab_model(m.coh.overall.ind.combined)
performance::check_model(m.coh.overall.ind.combined)
lme4::allFit(m.coh.overall.ind.combined)




m.tp.overall.ind.eda <- lme4::lmer(team_perf_indiv ~
                                      mission_day +
                                      # poly(mission_day,2) +
                                      eda_eda_tonic_empath_scores_2_daily_mean +
                                      eda_eda_tonic_driver_2_daily_mean +
                                     eda_eda_phasic_empath_scores_2_daily_mean +
                                     eda_eda_phasic_driver_2_daily_mean +
                                      # eda_eda_tonic_s_e_2_daily_mean +
                                      # eda_eda_phasic_s_e_2_daily_mean +
                                      # (1|part_id),# + 
                                     
                                     (1|id_crew/part_id),
                                     #(1|id_crew),
                                    data = q1_df,
                                    #REML = FALSE,
                                    control = lme4::lmerControl(optimizer ="Nelder_Mead"))
summary(m.tp.overall.ind.eda) 
sjPlot::tab_model(m.tp.overall.ind.eda)
performance::check_model(m.tp.overall.ind.eda)
lme4::allFit(m.tp.overall.ind.eda)

m.tp.overall.ind.card <- lme4::lmer(team_perf_indiv ~
                                       mission_day +
                                       # poly(mission_day, 2) +
                                       # cardiac_bpm_empath_scores_2_daily_mean +
                                       # cardiac_bpm_driver_2_daily_mean +
                                       cardiac_ibi_empath_scores_2_daily_mean +
                                       cardiac_ibi_driver_2_daily_mean +
                                       # (1|part_id) + (1|id_crew),
                                      
                                      (1|id_crew/part_id),
                                     data = q1_df,
                                     #REML = FALSE,
                                     control = lme4::lmerControl(optimizer ="Nelder_Mead"))
summary(m.tp.overall.ind.card) 
sjPlot::tab_model(m.tp.overall.ind.card)
performance::check_model(m.tp.overall.ind.card)
lme4::allFit(m.tp.overall.ind.card)


m.tp.overall.ind.combined <- lme4::lmer(team_perf_indiv ~
                                           mission_day +
                                           eda_eda_tonic_empath_scores_2_daily_mean +
                                           eda_eda_tonic_driver_2_daily_mean +
                                           eda_eda_phasic_empath_scores_2_daily_mean +
                                           eda_eda_phasic_driver_2_daily_mean +
                                           cardiac_ibi_empath_scores_2_daily_mean +
                                           cardiac_ibi_driver_2_daily_mean +
                                           # eda_eda_tonic_s_e_2_daily_mean +
                                           # eda_eda_phasic_s_e_2_daily_mean +
                                           # (1|part_id),# + 
                                          
                                          (1|id_crew/part_id),
                                         # (1|id_crew),
                                         data = q1_df,
                                         #REML = FALSE,
                                         control = lme4::lmerControl(optimizer ="Nelder_Mead"))
summary(m.tp.overall.ind.combined) 
sjPlot::tab_model(m.tp.overall.ind.combined)
performance::check_model(m.tp.overall.ind.combined)
lme4::allFit(m.tp.overall.ind.combined)

#####################
## LME4 analysis for TEAM outcomes
#####################

m.coh.overall.team.eda <- lme4::lmer(cohesion_overall_team_final ~
                                      mission_day +
                                      # poly(mission_day,2) +
                                      # eda_eda_tonic_empath_scores_2_daily_mean +
                                      # eda_eda_tonic_driver_2_daily_mean +
                                      # eda_eda_phasic_empath_scores_2_daily_mean +
                                      # eda_eda_phasic_driver_2_daily_mean +
                                      eda_eda_tonic_s_e_2_daily_mean +
                                       eda_eda_phasic_s_e_2_daily_mean +
                                       # poly(mission_day,2)*eda_eda_phasic_s_e_2_daily_mean +
                                      # (1|part_id) + 
                                       (1|id_crew),
                                       
                                       # (1|id_crew/part_id),
                                    data = q1_df,
                                    #REML = FALSE,
                                    control = lme4::lmerControl(optimizer ="Nelder_Mead"))
summary(m.coh.overall.team.eda) 
sjPlot::tab_model(m.coh.overall.team.eda)
performance::check_model(m.coh.overall.team.eda)
lme4::allFit(m.coh.overall.team.eda)

m.coh.overall.team.card <- lme4::lmer(cohesion_overall_team_final ~
                                       mission_day +
                                       # poly(mission_day,2) +
                                       # cardiac_bpm_empath_scores_2_daily_mean +
                                       # cardiac_bpm_driver_2_daily_mean + 
                                       # cardiac_ibi_empath_scores_2_daily_mean +
                                       # cardiac_ibi_driver_2_daily_mean + 
                                        # cardiac_bpm_s_e_2_daily_mean +
                                        # poly(mission_day,2)*cardiac_bpm_s_e_2_daily_mean +
                                        cardiac_ibi_s_e_2_daily_mean +
                                       # (1|part_id) + 
                                        (1|id_crew),
                                     data = q1_df,
                                     #REML = FALSE,
                                     control = lme4::lmerControl(optimizer ="Nelder_Mead"))
summary(m.coh.overall.team.card) 
sjPlot::tab_model(m.coh.overall.team.card)
performance::check_model(m.coh.overall.team.card)
lme4::allFit(m.coh.overall.team.card)


m.coh.overall.team.combined <- lme4::lmer(cohesion_overall_team_final ~
                                            mission_day +
                                            # poly(mission_day, 2) +
                                            eda_eda_tonic_s_e_2_daily_mean +
                                            eda_eda_phasic_s_e_2_daily_mean +
                                            # cardiac_bpm_s_e_2_daily_mean +
                                            cardiac_ibi_s_e_2_daily_mean +
                                            # (1|part_id) +
                                            (1|id_crew),
                                          data = q1_df,
                                          #REML = FALSE,
                                          control = lme4::lmerControl(optimizer ="Nelder_Mead"))
summary(m.coh.overall.team.combined) 
sjPlot::tab_model(m.coh.overall.team.combined)
performance::check_model(m.coh.overall.team.combined)
lme4::allFit(m.coh.overall.team.combined)




m.tp.overall.team.eda <- lme4::lmer(team_perf_team ~
                                     mission_day +
                                     # poly(mission_day,2) +
                                     # eda_eda_tonic_empath_scores_2_daily_mean +
                                     # eda_eda_tonic_driver_2_daily_mean +
                                     # eda_eda_phasic_empath_scores_2_daily_mean +
                                     # eda_eda_phasic_driver_2_daily_mean +
                                     eda_eda_tonic_s_e_2_daily_mean +
                                     eda_eda_phasic_s_e_2_daily_mean +
                                     # (1|part_id) + 
                                      (1|id_crew),
                                   data = q1_df,
                                   #REML = FALSE,
                                   control = lme4::lmerControl(optimizer ="Nelder_Mead"))
summary(m.tp.overall.team.eda) 
sjPlot::tab_model(m.tp.overall.team.eda)
performance::check_model(m.tp.overall.team.eda)
lme4::allFit(m.tp.overall.team.eda)

m.tp.overall.team.card <- lme4::lmer(team_perf_team ~
                                      mission_day +
                                      # poly(mission_day, 2) +
                                      # cardiac_bpm_empath_scores_2_daily_mean +
                                      # cardiac_bpm_driver_2_daily_mean +
                                      # cardiac_ibi_empath_scores_2_daily_mean +
                                      # cardiac_ibi_driver_2_daily_mean +
                                      # cardiac_bpm_s_e_2_daily_mean +
                                      cardiac_ibi_s_e_2_daily_mean +
                                      # (1|part_id) + 
                                       (1|id_crew),
                                    data = q1_df,
                                    #REML = FALSE,
                                    control = lme4::lmerControl(optimizer ="Nelder_Mead"))
summary(m.tp.overall.team.card) 
sjPlot::tab_model(m.tp.overall.team.card)
performance::check_model(m.tp.overall.team.card)
lme4::allFit(m.tp.overall.team.card)





m.tp.overall.team.combined <- lme4::lmer(team_perf_team ~
                                       mission_day +
                                       # poly(mission_day, 2) +
                                       eda_eda_tonic_s_e_2_daily_mean +
                                       eda_eda_phasic_s_e_2_daily_mean +
                                       # cardiac_bpm_s_e_2_daily_mean +
                                       cardiac_ibi_s_e_2_daily_mean +
                                       # (1|part_id) + 
                                       (1|id_crew),
                                     data = q1_df,
                                     #REML = FALSE,
                                     control = lme4::lmerControl(optimizer ="Nelder_Mead"))
summary(m.tp.overall.team.combined) 
sjPlot::tab_model(m.tp.overall.team.combined)
performance::check_model(m.tp.overall.team.combined)
lme4::allFit(m.tp.overall.team.combined)



#####################
## Tables
#####################

# individual 
sjPlot::tab_model(m.coh.overall.ind.combined, m.tp.overall.ind.combined, file = 'outcomesTable.html')


# team 
sjPlot::tab_model(m.coh.overall.team.combined,m.tp.overall.team.combined, file = 'teamOutcomesTable.html')
sjPlot::plot_model(m.coh.overall.team.combined, type = 'pred')
sjPlot::plot_model(m.tp.overall.team.combined, type = 'pred')
