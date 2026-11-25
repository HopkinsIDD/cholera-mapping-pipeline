# run the main notebook, then run this

# estimates from Fig S12
est <- ma_dia$TE.random
k <- ma_dia$k
df <- k-1

lower <- ma_dia$lower.predict
upper <- ma_dia$upper.predict
se <- (upper - lower) / (2*1.96)
draws <- rnorm(8000, mean = est, sd = se)
draws_prob <- plogis(draws)
write.csv(draws_prob, here::here('data', 'generated_data', 'draws', 'prop_sought_general_pred.csv'))

lower <- ma_dia$lower.random
upper <- ma_dia$upper.random
se <- (upper - lower) / (2*1.96)
draws <- rnorm(8000, mean = est, sd = se)
draws_prob <- plogis(draws)
write.csv(draws_prob, here::here('data', 'generated_data', 'draws', 'prop_sought_general_est.csv'))

# estimates from Fig S13
est <- ma_sev$TE.random
k <- ma_dia$k
df <- k-1

lower <- ma_sev$lower.predict
upper <- ma_sev$upper.predict
se <- (upper - lower) / (2*1.96)
draws <- rnorm(8000, mean = est, sd = se)
draws_prob <- plogis(draws)
write.csv(draws_prob, here::here('data', 'generated_data', 'draws', 'prop_sought_severe_pred.csv'))

lower <- ma_sev$lower.random
upper <- ma_sev$upper.random
se <- (upper - lower) / (2*1.96)
draws <- rnorm(8000, mean = est, sd = se)
draws_prob <- plogis(draws)
write.csv(draws_prob, here::here('data', 'generated_data', 'draws', 'prop_sought_severe_est.csv'))