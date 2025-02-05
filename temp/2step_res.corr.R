# Clean workspace
rm(list = objects())  # Removes all objects from the environment.

# Packages ----
library(tidyverse) # R packages for data science.
library(asreml) # ASReml-R package.

# Load data ----
## Pheno & Ginv
load('ILYT_Pheno-Gmatrix.RData')


test.df <- ILYT_Pheno |> filter(Env=='24-Urb') |> droplevels() |>
  mutate(IDEU = paste(Env,Col,Row,sep = '-'), IDEU = as.factor(IDEU)) |>
  arrange(IDEU,Trait) |>
  glimpse()

str(test.df)

test.asr <- asreml(
  Pheno_z ~ 1 + Trait:Gen,
  random = ~ at(Trait):ar1v(Col):ar1(Row),
  residual = ~IDEU:corgh(Trait),
  data = test.df,
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '1gb'
)

test.asr <- update(test.asr)

test.asr <- update(test.asr)

#test.blues <- predict(test.asr, classify='Trait:Gen')$pvals

summary(test.asr)$varcomp

summary(test.asr, coef = T)$coef.fixed

test.blues <- predict(test.asr, classify='Trait:Gen', pworkspace = '4gb')$pvals

test.blues |>
  ggplot(aes(x=predicted.value)) +
  geom_histogram() +
  facet_wrap(~Trait, scales = 'free')

test.blues |>
  ggplot(aes(x=std.error)) +
  geom_histogram() +
  facet_wrap(~Trait, scales = 'free')

test0.blues |>
  ggplot(aes(x=predicted.value)) +
  geom_histogram() +
  facet_wrap(~Trait, scales = 'free')

test0.blues |>
  ggplot(aes(x=std.error)) +
  geom_histogram() +
  facet_wrap(~Trait, scales = 'free')

test.df |>
  ggplot(aes(x=Pheno_z)) +
  geom_histogram() +
  facet_wrap(~Trait, scales = 'free')



test0.asr <- asreml(
  Pheno_z ~ 1 + Trait:Gen,
  residual = ~dsum(~ar1(Col):ar1(Row) | Trait),
  data = test.df |> arrange(Trait, Col, Row),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '1gb'
)

test0.asr <- update(test0.asr)

test0.asr <- update(test0.asr)

test0.blues <- predict(test0.asr, classify='Trait:Gen', pworkspace = '4gb')$pvals

test.comb <- test0.blues |>
  rename(pred0 = predicted.value,
         se0 = std.error) |>
  select(-status) |>
  left_join(test.blues |>
              rename(pred1 = predicted.value,
                     se1 = std.error) |>
              select(-status)) |>
  glimpse()

summary(test0.asr)$aic
summary(test.asr)$aic

test.comb |>
  ggplot(aes(x=pred0,y=pred1)) +
  geom_point()

test.comb |>
  ggplot(aes(x=se0,y=se1)) +
  geom_point()
