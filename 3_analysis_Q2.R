################################################
################################################
##### Analyses for Question 2: traits -> tpd
################################################
################################################

library(tidyverse)
library(sjPlot)
library(lme4)

#####################
## Final data prep
#####################

skimr::skim(cmb_df)
trait_df <- trait_df %>%
  select(-x,-ends_with('mlq')) %>%
  mutate(across(where(is.numeric), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE))))
  
q2_df <- cmb_df %>%
  select(campaign,mission,id_crew,part_id,role,mission_day,
         ends_with('_daily_mean') # all daily tpd aggregates
         # add task type categories
  ) %>%
  mutate(
    mission_day = mission_day-1, #starts md at 0
    id_crew = as.factor(id_crew),
    part_id = as.factor(part_id)
  ) %>% 
  mutate(across(ends_with('_daily_mean'), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE)))) %>%
  left_join(trait_df, by = "part_id")
skimr::skim(q2_df)
q2_df <- q2_df %>% drop_na()

#####################
## ibi driver
#####################

m.card.d.trait.0 <- lm(as.formula('cardiac_ibi_driver_2_daily_mean ~1'), data = q2_df)
f <- as.formula(cardiac_ibi_driver_2_daily_mean~1 + mission_day + (1|id_crew/part_id))
m.card.d.trait.1 <- lmer(f, data = q2_df, REML = TRUE)
anova(m.card.d.trait.1,m.card.d.trait.0)
summary(m.card.d.trait.1)
lattice::qqmath(m.card.d.trait.1)
sjPlot::tab_model(m.card.d.trait.0,m.card.d.trait.1)

m.card.d.trait.2 <- stats::update(m.card.d.trait.1, .~. +
                                # mean_mrm #+
                                fantasy_total_iri +
                                empathic_concern_total_iri +
                                # perspective_taking_total_iri +
                                # personal_distress_total_iri #+
                                # mean_iri +
                                per_mea_nri #+
                                # em_sel_nri +
                                # em_oth_nri +
                                # self_imp_nri +
                                # conf_avoi_nri #+
                                # mean_nri +
                                # affliation_co_new #+
                                # dominance_co_new #+
                                # mean_co_new +
                                # ipip_neuroticism +
                                # ipip_extraversion +
                                # ipip_openness +
                                # ipip_agreeableness +
                                # ipip_conscientiousness
                                )

anova(m.card.d.trait.2,m.card.d.trait.1)
summary(m.card.d.trait.2)
lattice::qqmath(m.card.d.trait.2)
sjPlot::tab_model(m.card.d.trait.2)

#####################
## ibi empath
#####################

m.card.e.trait.0 <- lm(as.formula('cardiac_ibi_empath_scores_2_daily_mean ~1'), data = q2_df)
f <- as.formula(cardiac_ibi_empath_scores_2_daily_mean~1 + mission_day + (1|id_crew/part_id))
m.card.e.trait.1 <- lmer(f, data = q2_df, REML = TRUE)
anova(m.card.e.trait.1,m.card.e.trait.0)
summary(m.card.e.trait.1)
lattice::qqmath(m.card.e.trait.1)
sjPlot::tab_model(m.card.e.trait.0,m.card.e.trait.1)

m.card.e.trait.2 <- stats::update(m.card.e.trait.1, .~. +
                                    # mean_mrm #+
                                    # fantasy_total_iri +
                                    # empathic_concern_total_iri +
                                    # perspective_taking_total_iri +
                                    # personal_distress_total_iri #+
                                    # mean_iri +
                                    # per_mea_nri +
                                    # em_sel_nri +
                                    # em_oth_nri +
                                    # self_imp_nri +
                                    # conf_avoi_nri #+
                                    # mean_nri +
                                    # affliation_co_new +
                                    # dominance_co_new #+
                                    # mean_co_new +
                                  # ipip_neuroticism +
                                  # ipip_extraversion +
                                  # ipip_openness +
                                  # ipip_agreeableness +
                                  ipip_conscientiousness
                                  )

anova(m.card.e.trait.2,m.card.e.trait.1)
summary(m.card.e.trait.2)
lattice::qqmath(m.card.e.trait.2)
sjPlot::tab_model(m.card.e.trait.2)


#####################
## eda phasic empath
#####################

m.eda.phasic.e.trait.0 <- lm(as.formula('eda_eda_phasic_empath_scores_2_daily_mean ~1'), data = q2_df)
f <- as.formula(eda_eda_phasic_empath_scores_2_daily_mean~1 + mission_day + (1|part_id))
m.eda.phasic.e.trait.1 <- lmer(f, data = q2_df, REML = TRUE)
anova(m.eda.phasic.e.trait.1,m.eda.phasic.e.trait.0)
summary(m.eda.phasic.e.trait.1)
lme4::allFit(m.eda.phasic.e.trait.1)
lattice::qqmath(m.eda.phasic.e.trait.1)
sjPlot::tab_model(m.eda.phasic.e.trait.0,m.eda.phasic.e.trait.1)

m.eda.phasic.e.trait.2 <- stats::update(m.eda.phasic.e.trait.1, .~. +
                                    # mean_mrm #+
                                    # fantasy_total_iri +
                                    # empathic_concern_total_iri +
                                    # perspective_taking_total_iri +
                                    # personal_distress_total_iri #+
                                    # mean_iri +
                                    # per_mea_nri #+
                                    # em_sel_nri +
                                    # em_oth_nri +
                                    # self_imp_nri +
                                    # conf_avoi_nri #+
                                    # mean_nri +
                                    affliation_co_new #+
                                    # dominance_co_new #+
                                    # mean_co_new +
                                    # ipip_neuroticism +
                                    # ipip_extraversion #+
                                    # ipip_openness +
                                    # ipip_agreeableness +
                                    # ipip_conscientiousness
                                 )

anova(m.eda.phasic.e.trait.2,m.eda.phasic.e.trait.1)
summary(m.eda.phasic.e.trait.2)
lattice::qqmath(m.eda.phasic.e.trait.2)
sjPlot::tab_model(m.eda.phasic.e.trait.2)

#####################
## eda phasic driver
#####################

m.eda.phasic.d.trait.0 <- lm(as.formula('eda_eda_phasic_driver_2_daily_mean ~1'), data = q2_df)
f <- as.formula(eda_eda_phasic_driver_2_daily_mean~1 + mission_day + (1|id_crew/part_id))
m.eda.phasic.d.trait.1 <- lmer(f, data = q2_df, REML = TRUE)
anova(m.eda.phasic.d.trait.1,m.eda.phasic.d.trait.0)
summary(m.eda.phasic.d.trait.1)
lme4::allFit(m.eda.phasic.d.trait.1)
lattice::qqmath(m.eda.phasic.d.trait.1)
sjPlot::tab_model(m.eda.phasic.d.trait.0,m.eda.phasic.d.trait.1)

m.eda.phasic.d.trait.2 <- stats::update(m.eda.phasic.d.trait.1, .~. +
                                          # mean_mrm #+
                                          # fantasy_total_iri +
                                          # empathic_concern_total_iri +
                                          # perspective_taking_total_iri +
                                          # personal_distress_total_iri #+
                                          # mean_iri +
                                          # per_mea_nri +
                                          # em_sel_nri +
                                          # em_oth_nri +
                                          # self_imp_nri +
                                          # conf_avoi_nri +
                                        # mean_nri +
                                        # affliation_co_new +
                                        # dominance_co_new #+
                                        # mean_co_new +
                                        # ipip_neuroticism +
                                        # ipip_extraversion +
                                        ipip_openness +
                                        ipip_agreeableness #+
                                        # ipip_conscientiousness
)

anova(m.eda.phasic.d.trait.2,m.eda.phasic.d.trait.1)
summary(m.eda.phasic.d.trait.2)
lattice::qqmath(m.eda.phasic.d.trait.2)
sjPlot::tab_model(m.eda.phasic.d.trait.2)

#####################
## eda tonic empath
#####################

m.eda.tonic.e.trait.0 <- lm(as.formula('eda_eda_tonic_empath_scores_2_daily_mean ~1'), data = q2_df)
f <- as.formula(eda_eda_tonic_empath_scores_2_daily_mean~1 + mission_day + (1|part_id))
m.eda.tonic.e.trait.1 <- lmer(f, data = q2_df, REML = TRUE)
anova(m.eda.tonic.e.trait.1,m.eda.tonic.e.trait.0)
summary(m.eda.tonic.e.trait.1)
lme4::allFit(m.eda.tonic.e.trait.1)
lattice::qqmath(m.eda.tonic.e.trait.1)
sjPlot::tab_model(m.eda.tonic.e.trait.0,m.eda.tonic.e.trait.1)

m.eda.tonic.e.trait.2 <- stats::update(m.eda.tonic.e.trait.1, .~. +
                                          # mean_mrm #+
                                          # fantasy_total_iri +
                                          # empathic_concern_total_iri +
                                          # perspective_taking_total_iri +
                                          # personal_distress_total_iri #+
                                          # mean_iri +
                                          # per_mea_nri +
                                          # em_sel_nri +
                                          # em_oth_nri +
                                          # self_imp_nri +
                                          conf_avoi_nri #+
                                        # mean_nri +
                                        # affliation_co_new +
                                        # dominance_co_new #+
                                        # mean_co_new +
                                        # ipip_neuroticism #+
                                        # ipip_extraversion +
                                        # ipip_openness +
                                        # ipip_agreeableness +
                                        # ipip_conscientiousness
)

anova(m.eda.tonic.e.trait.2,m.eda.tonic.e.trait.1)
summary(m.eda.tonic.e.trait.2)
lattice::qqmath(m.eda.tonic.e.trait.2)
sjPlot::tab_model(m.eda.tonic.e.trait.2)

#####################
## eda tonic driver
#####################

m.eda.tonic.d.trait.0 <- lm(as.formula('eda_eda_tonic_driver_2_daily_mean ~1'), data = q2_df)
f <- as.formula(eda_eda_tonic_driver_2_daily_mean~1 + mission_day + (1|id_crew/part_id))
m.eda.tonic.d.trait.1 <- lmer(f, data = q2_df, REML = TRUE)
anova(m.eda.tonic.d.trait.1,m.eda.tonic.d.trait.0)
summary(m.eda.tonic.d.trait.1)
lme4::allFit(m.eda.tonic.d.trait.1)
lattice::qqmath(m.eda.tonic.d.trait.1)
sjPlot::tab_model(m.eda.tonic.d.trait.0,m.eda.tonic.d.trait.1)

m.eda.tonic.d.trait.2 <- stats::update(m.eda.tonic.d.trait.1, .~. +
                                         # mean_mrm #+
                                         # fantasy_total_iri +
                                         empathic_concern_total_iri +
                                         # perspective_taking_total_iri +
                                         # personal_distress_total_iri #+
                                         # mean_iri +
                                       # per_mea_nri +
                                       # em_sel_nri +
                                       # em_oth_nri +
                                       # self_imp_nri +
                                       conf_avoi_nri +
                                       # # mean_nri #+
                                       # affliation_co_new +
                                       # dominance_co_new #+
                                       # mean_co_new +
                                       ipip_neuroticism +
                                       ipip_extraversion +
                                       # ipip_openness +
                                       # ipip_agreeableness +
                                       ipip_conscientiousness
)

anova(m.eda.tonic.d.trait.2,m.eda.tonic.d.trait.1)
summary(m.eda.tonic.d.trait.2)
lattice::qqmath(m.eda.tonic.d.trait.2)
sjPlot::tab_model(m.eda.tonic.d.trait.2)

sjPlot::tab_model(m.card.d.trait.2,m.card.e.trait.2,m.eda.phasic.e.trait.2,m.eda.phasic.d.trait.2,
                  m.eda.tonic.e.trait.2,m.eda.tonic.d.trait.2, file = 'traitTable.html')
