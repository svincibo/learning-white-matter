
# data loading ------------------------------------------------------------

library(tidyverse)
recog <- read_csv("wml_data_beh_recog_n39_20220314.csv")
wr <- read_csv("wml_data_beh_write_n39_20220314.csv")
tracts <- read_csv("wml_data_mri_tractprofiles_n39_20220314.csv")

# use fractional anisotropy
fa <- tracts %>% select(subID, tractname, nodeID, fa)
rm(tracts)
fa_wide_means <- fa %>%
  filter(nodeID > 20, nodeID < 181) %>%
  group_by(subID, tractname) %>%
  summarise(fa = mean(fa, na.rm = TRUE)) %>%
  pivot_wider(names_from = tractname, values_from = fa)
fa_wide <- fa %>%
  filter(nodeID > 20, nodeID < 181) %>%
  pivot_wider(names_from = c(tractname, nodeID), values_from = fa)


# examine draw duration ---------------------------------------------------

wr %>%
  ggplot(aes(block, drawduration, color = stimulus)) +
  geom_point(alpha = .2, size=.2) +
  geom_smooth(se=FALSE, method = "lm") +
  facet_wrap(~as.factor(subID)) +
  theme_bw()


# Check to see that Random Intercept is sufficient, most
fp <- function(x) {
  m1 = lme4::lmer(drawduration ~ block + (1 | stimulus), data = x)
  m2 = lme4::lmer(drawduration ~ block + (block | stimulus), data = x)
  anova(m1, m2)$`Pr(>Chisq)`[2]
}
pvals <- wr %>%
  group_by(subID) %>%
  group_modify(~ fp(.x))
sort(unlist(pvals))

# grab the slopes

fits <- wr %>%
  group_by(subID) %>%
  group_modify( ~ {
    as.data.frame(summary(
      lme4::lmer(drawduration ~ block + (1 | stimulus), data=.x)
    )$coefficients[2,,drop=FALSE])
  })

ggplot(fits, aes(y=as.factor(subID))) +
  geom_errorbar(
    aes(xmin = Estimate - 2*`Std. Error`, 
        xmax = Estimate + 2*`Std. Error`,
        color = as.factor(subID))
  ) +
  geom_point(aes(Estimate)) +
  theme_bw() +
  scale_color_viridis_d() +
  theme(legend.position = "none") +
  ylab("subID") +
  geom_vline(xintercept = 0)


# examine fa --------------------------------------------------------------

fa %>% 
  filter(nodeID > 20, nodeID < 181) %>%
  ggplot(aes(nodeID, fa, color = tractname)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~subID) +
  scale_color_viridis_d() +
  theme_bw() +
  theme(legend.position = "none")

fa %>%
  filter(nodeID > 20, nodeID < 181, tractname == "leftTPC") %>%
  ggplot(aes(nodeID, fa)) +
  geom_point(alpha = .2) +
  geom_smooth(se = FALSE) +
  facet_wrap(~subID) +
  scale_color_viridis_d() +
  theme_bw() +
  theme(legend.position = "none")

fa %>%
  filter(nodeID > 20, nodeID < 181, tractname == "rightpArc") %>%
  ggplot(aes(nodeID, fa)) +
  geom_point(alpha = .2) +
  geom_smooth(se = FALSE) +
  facet_wrap(~subID) +
  scale_color_viridis_d() +
  theme_bw() +
  theme(legend.position = "none")

# Suggests a spline with a few degrees of freedom, say 5

df = 5
fa_wide_spline <- fa %>%
  group_by(subID, tractname) %>%
  group_modify( ~ {
    if (sum(!is.na(.x$fa)) < df + 2)
      return(data.frame(nms = c("int", 1:df), cc = NA))
    out = lm(fa ~ splines::bs(nodeID, df=df), data = .x)
    data.frame(nms = c("int", 1:df), cc = coef(out))
  })
fa_wide_spline <- fa_wide_spline %>%
  pivot_wider(names_from = c(tractname, nms), values_from = cc)


# build up outcomes -------------------------------------------------------

recog_resp <- recog %>%
  group_by(subID) %>%
  summarise(rt = mean(RT[acc == 1], na.rm=TRUE),
            acc = mean(acc == 1, na.rm=TRUE))
all_resp <- full_join(recog_resp, fits) %>%
  select(subID, rt, acc, Estimate) %>%
  rename(learn = Estimate)

y <- as.matrix(all_resp %>% select(-subID))


# fit the mean fa model ---------------------------------------------------
library(glmnet)

x <- fa_wide_means %>% ungroup() %>% select(-subID) %>% as.matrix()
bad_obs <- rowSums(is.na(x))
bad_cols <- colSums(is.na(x[bad_obs < 4,]))
x <- x[bad_obs < 4, bad_cols < 1]
y <- y[bad_obs < 4,]
mean_model <- cv.glmnet(x, y, family = "mgaussian")
coef_ests <- lapply(coef(mean_model, s = "lambda.min"), 
                    function(x) data.frame(t(as.matrix(x)))) %>%
  bind_rows() %>%
  mutate(resp = colnames(y)) %>%
  pivot_longer(-resp) %>%
  filter(abs(value) > 0)

ggplot(coef_ests, aes(value, name, color = resp)) +
  geom_point() + 
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  ylab("")


# mean model,  separate glmnet fits ---------------------------------------

fitter <- function(y) {
  yy <- y[bad_obs < 4]
  mod <- cv.glmnet(x, yy)
  cc <- coef(mod, s = "lambda.min")
  data.frame(t(as.matrix(cc)))
}

mean_model_s <- lapply(all_resp %>% select(-subID), fitter) %>%
  bind_rows() %>%
  mutate(resp = colnames(y)) %>%
  pivot_longer(-resp) %>%
  filter(abs(value) > 0)
  
ggplot(mean_model_s, aes(value, name, color = resp)) +
  geom_point() + 
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  ylab("")

