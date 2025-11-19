library(readxl)
library(tidyverse)
library(lme4)
library(gtsummary)
library(sandwich)
library(marginaleffects)

df <- read_excel("~/Desktop/Заказы/Манджикян Овсеп/df.xlsx")

df1 <- df |> 
 pivot_longer(Pain_01:Pain_14) |> 
 separate(name, sep = "_", into = c("Pain", "Time")) |> 
 select(-Pain) |> 
 mutate(Time = as.numeric(Time),
  Group = factor(Group))

df |> 
 select(-ID) |> 
 tbl_summary(by = Group) |> 
 add_p()

df1 |> 
 ggplot(aes(value, fill = Compression)) +
 geom_histogram(position = position_dodge(width = 0.5), binwidth = 1, alpha = 0.8) +
 #geom_density(alpha = 0.5) +
 facet_wrap(~Time)

df1 |> 
 ggplot(aes(Time, value, color = Compression)) +
 stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.1))

df1 |> 
 group_by(Time, Group) |> 
 summarise(mean = mean(value, na.rm = T))

rstatix::anova_test(formula = value ~ Group*Time + Error(ID/Time), data = df1)

fit_pois <- glmer(value ~ Group*Time + Age + Sex + offset(log(as.numeric(Time))) + (1|ID), 
             df1, family = poisson)

fit_pois@theta

parameters::parameters(fit_pois, exp = T)

avg_predictions(fit_pois, by = c("Time", "Group"), 
                newdata = "balanced", 
                re.form = NA)

plot_predictions(fit_pois, by = c("Time", "Group"), 
                 newdata = "balanced", 
                 re.form = NA)

avg_comparisons(fit_pois, 
                variables = "Group", 
                comparison = "lnratio", 
                transform = "exp",
                newdata = "balanced",
                re.form = NA)

avg_comparisons(fit_pois, 
                variables = "Group", 
                comparison = "lnratio", 
                transform = "exp",
                newdata = datagrid(Group = c(1,2), 
                                   grid_type = "counterfactual"),
                re.form = NA)

avg_comparisons(fit_pois, 
                by = "Time", 
                variables = "Group", 
                comparison = "lnratio", 
                transform = "exp",
                newdata = "balanced",
                re.form = NA)

avg_comparisons(fit_pois, 
                by = "Time", 
                variables = "Group", 
                newdata = "balanced",
                re.form = NA, 
                hypothesis = 1)

plot_comparisons(fit_pois, condition = "Time", 
                 variables = "Group", 
                 comparison = "lnratio", 
                 transform = "exp",
                 newdata = "balanced",
                 re.form = NA)

df1 |> 
 as.data.frame() |> 
 friedman.test(value ~ Group|Time, data = _)

friedman.test(value ~ Group|ID, df1)

df1 |> 
 mutate(Group = factor(Group),
        Time = factor(Time),
        ID = factor(ID)) |> 
 rstatix::friedman_test(formula = value ~ Group|ID, data = _)


performance::performance(fit_pois)

performance::check_autocorrelation(fit_pois)

plot(fit_pois)

df1 |> 
 ggplot(aes(x = factor(Time), y = value, color = Group, fill = Group)) +
 gghalves::geom_half_violin(trim = T, alpha = 0.3, draw_quantiles = c(0.05, 0.5, 0.95)) +
 stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.75))




str(df)


c(0.1,0.5) |> style_number(digits = 2, decimal.mark = ",")



avg_predictions(fit_pois, by = c("Время", "Компрессия"), 
                newdata = "balanced", 
                re.form = NA) |> 
 tidy()



avg_comparisons(fit_pois, 
                by = "Время", 
                variables = "Компрессия", 
                hypothesis = 1,
                newdata = "balanced",
                re.form = NA) |> 
 tidy()




plot_predictions(fit_pois, condition = c("Время", "Компрессия"),
                 newdata = "balanced", 
                 re.form = NA, draw = F) |> 
 ggplot(aes(x = Время, y = estimate, ymin = conf.low, ymax = conf.high, 
            fill = Компрессия)) +
 geom_line(aes(color = Компрессия), linewidth = 1) +
 geom_ribbon(alpha = 0.15) +
 theme_bw(base_size = 14) +
 ggsci::scale_color_lancet() +
 ggsci::scale_fill_lancet() +
 labs(x = "П/о дни", y = "Значение ВАШ") +
 scale_x_continuous(breaks = seq(1, 14, 2)) +
 scale_y_continuous(breaks = seq(0, 3, 0.5)) +
 theme(legend.position = c(0.8, 0.8),
       legend.background = element_blank())


plot_predictions(fit_pois, condition = c("Время", "Компрессия"),
                 newdata = "balanced", 
                 re.form = NA,
                 type = "link") 


plot_comparisons(fit_pois, condition = "Время", 
                 variables = "Компрессия",
                 newdata = "balanced", 
                 #comparison = "lnratio", 
                 #transform = "exp",
                 re.form = NA)



SampleSize4ClinicalTrials::ssc_meancomp(design = 3L, ratio = 1, alpha = 0.5, 
                                        power = 0.8, 
                                        sd = 2, theta = 0, delta = 1)

20/82

avg_comparisons(fit_pois, 
                by = "Time",  
                variables = list(Compression = c("2 level", "1 level")), 
                hypothesis = -1,
                newdata = "balanced")


str(df)


df$CIVIQ_20_0 |> 
 median()

df$VCSS_0 |> 
 median()





df_est |> 
 ungroup(value) |> 
 ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, 
            y = fct_reorder(fct_rev(value), name, .desc = T))) +
 geom_point(color = "darkblue") +
 geom_errorbar(width = 0.5) +
 scale_x_continuous(limits = c(0.09, 6), 
                    breaks = c(0.1, 0.25, 0.5, 1, 2, 4, 6),
                    transform = "log") +
 theme_classic(base_size = 14) +
 labs(x = "Risk ratio", y = NULL) +
 facet_wrap(~ name, ncol = 1, strip.position = "left", scales = "free_y") +
 geom_vline(xintercept = 1, linetype = 2, color = "grey60") +
 theme(axis.text.y = element_blank(), axis.line.y = element_blank(), 
       axis.ticks.y = element_blank(), strip.text = element_blank(), 
       axis.title.x = element_text(hjust = 0.575))



df_uniq <- df_est |> 
 arrange(name, value) |> 
 ungroup() |> 
 distinct(name, .keep_all = T)

df_est |> 
 mutate(est = paste0(round(estimate, 2), " (", 
                     round(conf.low, 2), ", ",
                     round(conf.high, 2), ")")) |> 
 ggplot(aes(y = fct_reorder(fct_rev(value), name, .desc = T))) +
 geom_text(aes(x = 1.5, label = value)) +
 geom_text(aes(x = 2.5, label = est)) +
 geom_text(data = df_uniq, aes(x = 0, label = fct_reorder(name, value, 
                                                          .desc = T)), 
           hjust = 0) +
 coord_cartesian(xlim = c(0, 3)) +
 facet_wrap(~ name, ncol = 1, strip.position = "left", scales = "free_y") +
 geom_hline(yintercept = 19) +
 labs(y = NULL, 
      subtitle = "Variable                            Subgroup            RR (95% CI)") +
 theme_classic(base_size = 14) +
 theme(strip.text = element_blank(), axis.text.y = element_blank(), 
       axis.line.y = element_blank(), axis.ticks.y = element_blank(),
       axis.line.x = element_line(linewidth = 0), axis.ticks.x = element_line(linewidth = 0),
       axis.text.x = element_text(color = "white"), 
       axis.title.x = element_text(color = "white"))

df_est |> 
 ggplot(aes(y = fct_reorder(fct_rev(value), name, .desc = T))) +
 geom_text(aes(x = 1, label = round(p.value, 3))) +
 facet_wrap(~ name, ncol = 1, strip.position = "left", scales = "free_y") +
 lims(x = c(0.9, 1.1)) +
 labs(y = NULL) +
 theme_classic(base_size = 14) +
 theme(strip.text = element_blank(), axis.text.y = element_blank(), 
       axis.line.y = element_blank(), axis.ticks.y = element_blank(),
       axis.line.x = element_line(linewidth = 0), axis.ticks.x = element_line(linewidth = 0),
       axis.text.x = element_text(color = "white"), 
       axis.title.x = element_text(color = "white"))


1/6




equatiomatic::extract_eq(fit_pois, show_distribution = T)

parameters::parameters(fit_pois)
parameters::parameters(fit_pois, exponentiate = T)

tbl_regression(fit_pois, exp = T)

sjPlot::tab_model(fit_pois)

sjPlot::plot_model(fit_pois, terms = c("Time", "Compression"), type = "pred")

avg_comparisons(fit_pois,
                variables = "Compression", 
                comparison = "lnratio", 
                #transform = "exp",
                newdata = "balanced",
                re.form = NA)

fit_pois <- glmer(value ~ Compression*Time + Age + Sex + 
                   offset(log(Time)) + (1|ID), df1, family = poisson)

library(DHARMa)
testDispersion(fit_pois)

simulateResiduals(fittedModel = fit_pois, plot = F) |> 
 plotQQunif()
 plotResiduals()
 plot()

simulateResiduals(fittedModel = fit_pois, plot = F) |> 
 testQuantiles()

simulateResiduals(fittedModel = fit_pois, plot = F) |> 
 testCategorical(catPred = df1$Compression)


simulateResiduals(fittedModel = fit_pois, plot = F) |> 
 plot(quantreg = T)


A <- c(20, 20, 26, 25, 22, 23)
B <- c(28, 25, 24, 27, 30, 30, 29)

log(mean(B)/mean(A))

exp(-0.3365)




avg_comparisons(fit_pois, 
                by = "Time", 
                variables = "Compression", 
                newdata = "balanced",
                re.form = NA) |> 
 tidy()

avg_predictions(fit_pois, by = c("Time", "Compression"), 
                newdata = "balanced", 
                re.form = NA) |> 
 tidy() |> 
 select(Time, Compression, estimate) |> 
 pivot_wider(id_cols = Time, names_from = Compression, values_from = estimate) |> 
 mutate(prct = (`2 level` - `1 level`)/`1 level`) |> 
 summarise(mean = mean(prct))

avg_comparisons(fit_pois,
                variables = "Compression", 
                comparison = "lnratio", 
                transform = "exp",
                newdata = "balanced",
                re.form = NA)

plot(fit_pois)

performance::performance(fit_nb)
performance::check_model(fit_nb)
performance::check_autocorrelation(fit_nb)
performance::check_overdispersion(fit_nb)
performance::check_distribution(fit_nb)
performance::check_outliers(fit_nb)
performance::check_group_variation(fit_nb)
performance::check_residuals(fit_nb)
performance::check_zeroinflation(fit_nb)

library(glmtoolbox)

fit_pois <- geeglm(value ~ Compression*Time + Age + Sex + offset(log(Time)), 
                   id = ID, df1, family = poisson, corstr = "ar1")

lmtest::dwtest(fit_pois)


df1 |> 
 select(value, Compression, Time, Age, Sex) |> 
 acf(na.action = na.pass)

acf(residuals(fit_pois))


fit_pois$geese$alpha

fit_pois <- geeglm(value ~ Compression*Time + Age + Sex + offset(log(Time)),
       id = ID, data = df1, family = poisson, corstr = "ar1", waves = log(Time))

fit_pois <- geeglm(value ~ Compression*Time + Age + Sex,
                   id = ID, data = df1, family = glmmTMB::nbinom2, corstr = "ar1",
                   waves = log(Time))


equatiomatic::extract_eq(fit_pois, wrap = T, intercept = "beta",  
                         terms_per_line = 1, show_distribution = T)



str(df1)

df1 |> 
 group_by(Compression) |> 
 summarise(sum = sum(value)/700)

1.57/1.85

plot(fit_pois)

predictions(fit_pois) |> 
 tidy() |> 
 ggplot(aes(Time, estimate, color = Compression)) +
 geom_line(aes(group = ID), alpha = 0.1) +
 geom_smooth(method = "loess", se = F) + 
 ggsci::scale_color_lancet() +
 theme_bw()

plot_predictions(fit_pois, condition = c("Time", "Compression"), newdata = "balanced") +

df1 |> 
 ggplot(aes(Time, value, color = Compression)) +
 stat_summary(fun.data = "mean_cl_boot", 
              position = position_dodge(width = 0.1), show.legend = F)

df1 |> 
 group_by(Time, Compression) |> 
 summarise(mean = mean(value))

library(lme4)

fit_nb <- glmer.nb(value ~ Compression*Time + Age + Sex + 
              offset(log(Time)) + (1|ID), df1, verbose = T)

fit_nb <- glmmTMB::glmmTMB(value ~ Compression*Time + Age + Sex + 
                  (1|ID), df1, family = glmmTMB::tweedie, offset = log(Time), 
                  verbose = T)

fit_nb <- glmmTMB::glmmTMB(value ~ Compression*Time + Age + Sex + (1|ID), 
                           df1, family = glmmTMB::nbinom2, offset = log(Time), 
                           verbose = T)

fit_nb <- MASS::glmmPQL(value ~ Compression*Time + Age + Sex + 
                      offset(log(Time)), random = ~ 1 | ID, df1,
              family = poisson,
              verbose=T)

performance::performance(fit_nb)
performance::check_model(fit_nb)
performance::check_autocorrelation(fit_nb)
performance::check_overdispersion(fit_nb)
performance::check_distribution(fit_nb)
performance::check_outliers(fit_nb)
performance::check_group_variation(fit_nb)
performance::check_residuals(fit_nb)
performance::check_zeroinflation(fit_nb)

#correlation = nlme::corAR1(form = ~ 1 | ID), 

parameters::parameters(fit_nb, exp = T, summary = T)

parameters::parameters(fit_pois, exp = T)

summary(fit_pois)


avg_comparisons(fit_nb,
                variables = list(Compression = c("2 level", "1 level")),
                comparison = "lnratio", 
                transform = "exp",
                newdata = "balanced",
                re.form = NA)

avg_comparisons(fit_pois,
                variables = list(Compression = c("2 level", "1 level")),
                comparison = "lnratio", 
                transform = "exp",
                newdata = "balanced")

plot_predictions(fit_nb, 
                 condition = c("Time", "Compression"),
                 newdata = "balanced", vcov = F,
                 re.form = NA)


avg_predictions(fit_pois, by = c("Time", "Compression"), 
                newdata = datagrid(Time = c(1,2), grid_type = "mean_or_mode"),
                re.form = NA) |> 
 tidy()

avg_predictions(fit_pois, by = c("Time", "Compression"), 
                newdata = datagrid(Time = c(1,2), grid_type = "balanced"),
                re.form = NA) |> 
 tidy()





df |> 
 select(Compression, Pain_03) |> 
 filter(Pain_03 != 0) |> 
 summarise(mean = exp(mean(log(Pain_03))), .by = Compression)
 




df |> 
 mutate(`Age, years` = factor(cut(Age, 
                                  breaks = c(18, 45, 60, 90), 
                                  labels = c("18-44", "45-59", "60 and older")), 
                              levels = c("18-44", "45-59", "60 and older")),
        ) |> 
 select(ID, `Age, years`,Compression, Pain_01:Pain_14) |>
 pivot_longer(Pain_01:Pain_14) |> 
 separate(name, sep = "_", into = c("Pain", "Time")) |>
 mutate(Time = as.numeric(Time),
        Compression = factor(Compression),
        Pain = value) |> 
 group_by(`Age, years`) |> 
 nest() |> 
 mutate(mod = map(data, model),
        est = map(mod, avg_comparisons,  
                  variables = list(Compression = c("2 level", "1 level")),
                  comparison = "lnratio", 
                  transform = "exp",
                  newdata = "balanced"),
        n = pull(count(count(data[[1]], ID)))) |> 
 unnest(est)

1/2.33

1/1.2

df_est |> 
 mutate(est = paste0(round(estimate, 2), " (", 
                     round(conf.low, 2), ", ",
                     round(conf.high, 2), ")")) |> 
 ggplot(aes(y = fct_reorder(fct_rev(value), name, .desc = T))) +
 geom_text(aes(x = 1.5, label = value)) +
 geom_text(aes(x = 3.5, label = est)) +
 geom_text(aes(x = 2.5, label = n)) +
 geom_text(data = df_uniq, aes(x = 0, label = fct_reorder(name, value, 
                                                          .desc = T)), 
           hjust = 0) +
 coord_cartesian(xlim = c(0, 4)) +
 facet_wrap(~ name, ncol = 1, strip.position = "left", scales = "free_y") +
 geom_hline(yintercept = 19) +
 labs(y = NULL, 
      subtitle = "Variable                    Subgroup      IRR (95% CI)") +
 theme_classic(base_size = 14) 










df |> 
 mutate(`Age, years` = factor(cut(Age, 
                                  breaks = c(18, 45, 60, 90), 
                                  labels = c("18-44", "45-59", "60 and older")), 
                              levels = c("18-44", "45-59", "60 and older")),
        `Ankle circumference, cm` = factor(cut(Ankle_circumference_0,
                                               breaks = c(0,25,40),
                                               labels = c("<25", "≥25")), 
                                           levels = c("<25", "≥25")),
        VCSS = factor(cut(VCSS_0,
                          breaks = c(0, 10, 20),
                          labels = c("less 10", "10 and more")),
                      levels = c("less 10", "10 and more")),
        `CIVIQ-20` = factor(cut(CIVIQ_20_0,
                                breaks = c(0, 33, 80),
                                labels = c("<33", "≥33")),
                            levels = c("<33", "≥33")),
        Compliance = factor(ifelse(Compliance == 100, "Full", "Non-full"), 
                            levels  = c("Full", "Non-full")),
        `СЕАР class` = factor(СЕАР_class_0, 
                              levels = c("2", "3", "4"))) |> 
 select(ID, `Age, years`, Sex, Side, Compression, `Ankle circumference, cm`,
        VCSS, `CIVIQ-20`, Compliance, `СЕАР class`, Pain_01:Pain_14) |>
 pivot_longer(Pain_01:Pain_14) |> 
 separate(name, sep = "_", into = c("Pain", "Time")) |>
 mutate(Time = as.numeric(Time),
        Compression = factor(Compression),
        Pain = value) |> 
 select(-value) -> df12444
 
df_1 |> 
 select(`Age, years`, Sex, Side, `Ankle circumference, cm`,
        VCSS, `CIVIQ-20`, Compliance, `СЕАР class`) |> 
 map(~geeglm(Pain ~ Compression*Time*.x, 
             id = ID, df_1, family = poisson, corstr = "ar1", waves = log(Time))) |> 
 map(anova) |> 
 map(tidy) |> 
 map(filter, term == "Compression:Time:.x" ) |> 
 map(select, -c(term, df,X2)) |> 
 bind_rows(.id="term") |> 
 rename(name = "term")
 

df_pval |> 
 left_join(df_est, by = join_by(name == name)) |> 
 rename(p.value = "p.value.x") |> 
 arrange(name, value) |> 
 ungroup() |> 
 distinct(name, .keep_all = T)


geeglm(Pain ~ Compression*Time*`Age, years`, 
       id = ID, df12444, family = poisson, corstr = "ar1", waves = log(Time)) |> 
 anova() |> 
 tidy()
 
 mutate_all(as.character, as.numeric) |> 
 pivot_longer(cols = c(`Age, years`:Side,
                       `Ankle circumference, cm`:`СЕАР class`)) |> 
 na.omit() |> 
 mutate(Time = as.numeric(Time),
        Compression = factor(Compression),
        Pain = as.numeric(Pain))
 
 
 
 
 
 
df1 <- df |> 
  select(ID, Compression, VCSS_0:Burning_0, 
         VCSS_1:Burning_1) |> 
  rename(Achingpain_0 = Aching_pain_0,
         Achingpain_1 = Aching_pain_1,
         CIVIQ20_0 = CIVIQ_20_0,
         CIVIQ20_1 = CIVIQ_20_1) |> 
  pivot_longer(VCSS_0:Burning_1) |> 
  separate(name, into = c("name", "time"), sep = "_") |> 
  pivot_wider(id_cols = c(ID, time, Compression), names_from = name, values_from = value)

df1 |> 
 filter(Compression == "1 level") |> 
 select(-Compression) |> 
  tbl_summary(by = "time", 
              missing = "no", 
              include = -ID,
              digits = list(all_continuous() ~ c(1,1)), 
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              type = list(all_dichotomous() ~ "categorical",
                          c(Pulsation, Tingling, Achingpain, 
                            Shyness, Itching, Anxiety) ~ "continuous"
              ),
              label = list(CIVIQ20 ~ "CIVIQ-20",
                           Achingpain ~ "Aching pain",
                           Pulsation ~ "Pulsation",
                           Shyness ~ "Shyness",
                           Severity ~ "Severity",
                           Fatigue ~ "Fatigue",
                           Edema ~ "Edema",
                           Seizures ~ "Seizures",
                           Itching ~ "Itching",
                           Anxiety ~ "Anxiety",
                           Tingling ~ "Tingling",
                           Burning ~ "Burning"
              )) |> 
 add_p(pvalue_fun = label_style_pvalue(digits = 3),
       test = everything() ~ "paired.t.test", group = ID) |> 
 modify_footnote(everything() ~ NA) |> 
 modify_header(stat_1 ~ "Baseline\n(preoperative)", 
               stat_2 ~ "Visit 1\n(15 day)") |> 
 tab_tbl() |> 
 align(j = 2:4, 
       align = "center", 
       part = "all")
  

chisq.test(df1$time, df1$CEAPclass)


rstatix::pearson_residuals(df1$time, df1$CEAPclass)

df_count <- df1 |> 
 filter(Compression == "2 level")

mc <- as.table(matrix(c(0, 1, 0, 19, 15,
                        1, 21, 13, 14, 13), ncol = 5, nrow = 2))

DescTools::StuartMaxwellTest(mc)
DescTools::BhapkarTest(mc)
coin::mh_test(mc)
nonpar::stuart.maxwell(mc)
mcnemar.test(mc)
rstatix::mcnemar_test(mc)
rstatix::cochran_qtest(mc)
RVAideMemoire::cochran.qtest(time ~ CEAPclass, df_count)
coin::symmetry_test(time ~ CEAPclass, df_count)

dumm

DescTools::Dummy(df_count$CEAPclass) |> 
 as.data.frame() |> 
 tibble() |> 
 mutate(time = df_count$time, 
        ID = df_count$ID) |> 
 pivot_longer(1:4) |> 
 rstatix::convert_as_factor(time, ID, name) |>  
 na.omit() |> 
 unique() |> 
 as.matrix() |> 
 rstatix::cochran_qtest(value ~ name|ID, data = _)



SampleSize4ClinicalTrials::ssc_meancomp(design = 3L, ratio = 1, alpha = 0.05, 
                                        power = 0.8, 
                                        sd = 2, theta = 0, delta = 1)


lm(Pain_14 ~ Compression*Aching_pain_0*Sex, data = df) |> 
 car::Anova()


df |> 
 select(Compression, Aching_pain_0, Aching_pain_1, Aching_pain_2, Pain_01:Pain_14) |> 
 tbl_summary(by = Compression, 
             type = list(all_continuous() ~ "continuous2",
                         c(Pain_09, Pain_10, Aching_pain_1, Aching_pain_2) ~ "continuous2"),
             statistic = all_continuous() ~ c("{mean} ({sd})", 
                                              "{median} [{p25}, {p75}]", 
                                              "{min} - {max}")) |> 
# add_p(test = everything() ~ "t.test",
     #  pvalue_fun = label_style_pvalue(digits = 3)) |> 
 add_difference(pvalue_fun = label_style_pvalue(digits = 3))

df |> 
 mutate(Aching_pain_0 = Aching_pain_0/10) |> 
 lm(Pain_14 ~ Compression*(Aching_pain_0 + Age + Sex), data = _) |> 
 avg_comparisons(variables = list(Compression = c("2 level", "1 level")), 
                                              newdata = "balanced", 
                 vcov = "HC3") |> 
 hypotheses(equivalence = c(-1,1))

library(quantreg)

df |> 
 mutate(Aching_pain_0 = Aching_pain_0/10) |> 
 rq(Pain_14 ~ Compression*(Aching_pain_0 + Age + Sex), data = _, tau = 0.5) |> 
 avg_comparisons(variables = list(Compression = c("2 level", "1 level")), 
                 newdata = "balanced",
                 vcov = "HC3") |> 
 hypotheses(equivalence = c(-1,1))

df |> 
 mutate(Aching_pain_0 = Aching_pain_0/10) |> 
 rq(Pain_14 ~ Compression*(Aching_pain_0 + Age + Sex), data = _, tau = 0.5) |> 
 avg_predictions(by = "Compression", type = "link",
                 newdata = "balanced",
                 vcov = "HC3")



df |> 
 group_by(Sex) |> 
 count(Compression) |> 
 mutate(n = n/sum(n))

df |> 
 group_by(Side) |> 
 count(Compression) |> 
 mutate(n = n/sum(n))


library(sensemakr)

fit <- lm(Pain_14 ~ Compression + Age + Sex, df)

fit

sens <- sensemakr(fit, 
          treatment = "Compression2 level", 
          benchmark_covariates = "SexMale",
          kd = 1:3,
          q = 1,
          alpha = 0.05,
          reduce = T)

summary(sens)
plot(sens)
ovb_minimal_reporting(sens)
plot(sens, type = "extreme")

df1 <- df |> 
 pivot_longer(Pain_01:Pain_14) |> 
 separate(name, sep = "_", into = c("Pain", "Time")) |> 
 select(-Pain) |> 
 mutate(Time = as.numeric(Time),
        Compression = factor(Compression))

fit_pois <- geeglm(value ~ Compression*Time*Aching_pain_0 + Age + Sex, 
                   id = ID, df1, family = poisson, corstr = "ar1", 
                   waves = log(Time))

avg_comparisons(fit_pois,
                variables = list(Compression = c("2 level", "1 level")),
                comparison = "lnratio", 
                transform = "exp", 
                newdata = "balanced")

plot_predictions(fit_pois, 
                 condition = c("Time", "Compression"),
                 newdata = "balanced",
                 draw = T)


DescTools::RunsTest(df$Compression, exact = T, correct = T)

x <- as.numeric(factor(df$Compression, labels = c(0,1))) -1
y <- rbinom(n = 100, size = 1, prob = 0.5)

ks.test(x, y)

randtests::bartels.rank.test(as.numeric(factor(df$Compression, labels = c(0,1))))
randtests::cox.stuart.test(as.numeric(factor(df$Compression, labels = c(0,1))))
randtests::difference.sign.test(as.numeric(factor(df$Compression, labels = c(0,1))))
randtests::rank.test(as.numeric(factor(df$Compression, labels = c(0,1))))
randtests::rank.test(rbinom(n = 100, size = 1, prob = 0.5))
randtests::runs.test(as.numeric(factor(df$Compression, labels = c(0,1))))
randtests::turning.point.test(as.numeric(factor(df$Compression, labels = c(0,1))))
randtests::turning.point.test(rbinom(n = 100, size = 1, prob = 0.5))





df |> 
 mutate(Compliance = factor(ifelse(Compliance == 100, "Full", "Non-full"), 
                            levels  = c("Full", "Non-full")),
        Aching_pain_0 = Aching_pain_0/10) |> 
 filter(Compliance == "Non-full") |> 
 select(ID, Compression, Aching_pain_0, Pain_14) |> 
 lm(Pain_14 ~ Compression*Aching_pain_0, data = _) |> 
 avg_comparisons(variables = list(Compression = c("2 level", "1 level")),
                 newdata = "balanced")

df_1 |> 
 mutate_all(as.character, as.numeric) |> 
 pivot_longer(cols = c(`Age, years`:Side,
                       `Ankle circumference, cm`:`CEAP class`)) |> 
 na.omit() |> 
 #mutate(Time = as.numeric(Time),
 #      Compression = factor(Compression),
 #     Pain = as.numeric(Pain)) |> 
 group_by(name, value) |> 
 nest() |> 
 filter(value == "Non-full") |> 
 unnest(data) |> 
 ungroup() |> 
 lm(Pain_14 ~ Compression*Aching_pain_0, data = _) |> 
 avg_comparisons(variables = list(Compression = c("2 level", "1 level")),
                 newdata = "balanced")






df1_VCSS <- df |> 
 mutate(Aching_pain_0 = round(Aching_pain_0/10,1)) |> 
 pivot_longer(starts_with("VCSS_"), names_to = "VCSS_time", values_to = "VCSS") |> 
 separate(VCSS_time, sep = "_", into = c("VCSS_time", "Time")) |> 
 select(-VCSS_time) |> 
 mutate(Time = as.numeric(Time),
        Compression = factor(Compression))

df1_VCSS |> 
 ggplot(aes(fill = Compression)) +
 geom_histogram(aes(x = VCSS, y =  stat(density)), color = "white", binwidth = 1) +
 facet_wrap(Compression~Time)

fit_pois_VCSS <- geeglm(VCSS ~ Compression*Time*(Aching_pain_0 + Age + Sex), 
                   id = ID, df1_VCSS, family = poisson, corstr = "ar1", 
                   waves = log(Time))

plot_predictions(fit_pois_VCSS, 
                 by = c("Time", "Compression"),
                 newdata = "balanced",
                 draw = F) |> 
 ggplot(aes(x = Time, y = estimate, ymin = conf.low, ymax = conf.high, 
            fill = Compression)) +
 geom_line(aes(color = Compression), linewidth = 1) +
 geom_ribbon(alpha = 0.15, show.legend = F) +
 theme_bw(base_size = 15) +
 ggsci::scale_color_lancet() +
 ggsci::scale_fill_lancet() +
 labs(x = "POD", y = "VAS value") +
 scale_x_continuous(breaks = seq(0, 2, 1)) +
 scale_y_continuous(breaks = seq(0, 7, 1)) +
 #coord_cartesian(xlim = c(1, 14), ylim = c(0, 5)) +
 theme(legend.position = c(0.85, 0.9),
       legend.text = element_text(size = 15),
       legend.title = element_blank(),
       legend.background = element_blank())



df$CIVIQ_20_2 |> 
 hist()


df1_CIVIQ_20 <- df |> 
 mutate(Aching_pain_0 = round(Aching_pain_0/10,1)) |> 
 pivot_longer(starts_with("CIVIQ_20_"), names_to = "CIVIQ_20_time", values_to = "CIVIQ_20") |> 
 separate(CIVIQ_20_time, sep = "20_", into = c("CIVIQ_20_time", "Time")) |> 
 select(-CIVIQ_20_time) |> 
 mutate(Time = as.numeric(Time),
        Compression = factor(Compression))

df1_CIVIQ_20 |> 
 ggplot(aes(fill = Compression)) +
 geom_histogram(aes(x = CIVIQ_20, y =  stat(density)), color = "white", binwidth = 5) +
 facet_wrap(Compression~Time)

df1_CIVIQ_20 |> 
 summarise(min = min(CIVIQ_20, na.rm = T), .by = c(Compression, Time))

df1_CIVIQ_20 |> 
 select(ID, CIVIQ_20, Compression, Time) |> 
 group_by(Compression, Time) |> 
 filter(CIVIQ_20 < 20)

fit_pois_VCSS <- geeglm(VCSS ~ Compression*Time*(Aching_pain_0 + Age + Sex), 
                        id = ID, df1_VCSS, family = poisson, corstr = "ar1", 
                        waves = log(Time))

plot_predictions(fit_pois_VCSS, 
                 by = c("Time", "Compression"),
                 newdata = "balanced",
                 draw = F) |> 
 ggplot(aes(x = Time, y = estimate, ymin = conf.low, ymax = conf.high, 
            fill = Compression)) +
 geom_line(aes(color = Compression), linewidth = 1) +
 geom_ribbon(alpha = 0.15, show.legend = F) +
 theme_bw(base_size = 15) +
 ggsci::scale_color_lancet() +
 ggsci::scale_fill_lancet() +
 labs(x = "POD", y = "VAS value") +
 scale_x_continuous(breaks = seq(0, 2, 1)) +
 scale_y_continuous(breaks = seq(0, 7, 1)) +
 #coord_cartesian(xlim = c(1, 14), ylim = c(0, 5)) +
 theme(legend.position = c(0.85, 0.9),
       legend.text = element_text(size = 15),
       legend.title = element_blank(),
       legend.background = element_blank())

