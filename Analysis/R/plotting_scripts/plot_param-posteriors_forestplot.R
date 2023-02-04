##### posteriors check
library(dplyr)
library(ggplot2)
library(mgcv)
library(rstan)

##### names to check: rho, log_std_dev_w, alpha, eta[1] ~ eta[5]    
## Copied for loop
# Where the data folder is
data_dir <- "/home/.../data"

# Find and load the model output 
fns <- list.files(data_dir, full.names = FALSE, recursive=FALSE)
country_list <- unique(stringr::str_extract(fns, "^[A-Z]{3}"))
country_list <- country_list[country_list != "ZAF" & !is.na(country_list)] #delete ZAF
country_list <- c(country_list, "Tanzania")
fns_filtered <- fns[grepl("stan_output.rdata$", fns) | grepl("initial_values.rdata$", fns)]

# Loop through all the files
forest_table <- tibble::tibble(country = as.character(), parameter = as.character(), 
                                min = as.numeric(), l_95 = as.numeric(), median = as.numeric(), u_95 = as.numeric(), max = as.numeric())

for(country_code in country_list){
    load(paste(data_dir, fns_filtered[grepl(country_code, fns_filtered)], sep = "/")[1])
    load(paste(data_dir, fns_filtered[grepl(country_code, fns_filtered)], sep = "/")[2])

    list_of_draws <- extract(model.rand) 

    for(param in c("rho", "log_std_dev_w", "alpha")){
        forest_table <- forest_table %>%
            add_row(country = country_code, parameter = param, 
                    min = min(list_of_draws[[param]]), l_95 = quantile(list_of_draws[[param]], 0.025), 
                    median = quantile(list_of_draws[[param]], 0.5), u_95 = quantile(list_of_draws[[param]], 0.975), 
                    max = max(list_of_draws[[param]])
            )
    }

    for(idx in 1:5){
        param <- paste0("est", idx)
        forest_table <- forest_table %>%
            add_row(country = country_code, parameter = param, 
                    min = min(list_of_draws$eta[, idx]), l_95 = quantile(list_of_draws$eta[, idx], 0.025), 
                    median = quantile(list_of_draws$eta[, idx], 0.5), u_95 = quantile(list_of_draws$eta[, idx], 0.975), 
                    max = max(list_of_draws$eta[, idx])
            ) %>%
            add_row(country = country_code, parameter = paste0("gam_est", idx), 
                    min = NA, 
                    l_95 = summary(initial_values_data$gam_fit_output)$p.coeff[idx] - qnorm(0.975) * summary(initial_values_data$gam_fit_output)$se[idx], 
                    median = summary(initial_values_data$gam_fit_output)$p.coeff[idx], 
                    u_95 = summary(initial_values_data$gam_fit_output)$p.coeff[idx] + qnorm(0.975) * summary(initial_values_data$gam_fit_output)$se[idx], 
                    max = NA
            )
    }

}

### Plotting
forest_table <- forest_table %>%
    group_by(country) %>%
    mutate(index = 1:n()) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(l_95 = ifelse(abs(l_95) > 100, NA, l_95)) %>%
    mutate(u_95 = ifelse(abs(u_95) > 100, NA, u_95))

plt <- forest_table %>%
    ggplot() +
    geom_rect(aes(xmin = l_95, xmax = u_95, ymin = index - 0.15, ymax = index + 0.15), fill = "lightblue") + 
    # ggplot(aes(y=index, x=median, xmin=min, xmax=max)) +
    geom_point(aes(y=index, x=median)) + 
    geom_errorbarh(aes(y=index, xmin=min, xmax=max), height=.1) +
    scale_y_continuous(breaks=1:nrow(forest_table), labels=forest_table$parameter) +
    labs(title='Estimates by parameters', x='Posterior distribution', y = 'Parameter') +
    geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
    theme_minimal() +
    facet_wrap(~country, scales = "free", ncol = 3)

pdf("Analysis/output/posterior_check.pdf", width = 15, height = 50)
plt
dev.off()



### Testing
list_of_draws <- extract(model.rand) 

forest_table <- tibble::tibble(country = as.character(), parameter = as.character(), 
                                min = as.numeric(), l_95 = as.numeric(), median = as.numeric(), u_95 = as.numeric(), max = as.numeric())

for(param in c("rho", "log_std_dev_w", "alpha")){
    forest_table <- forest_table %>%
        add_row(country = "AGO", parameter = param, 
                min = min(list_of_draws[[param]]), l_95 = quantile(list_of_draws[[param]], 0.025), 
                median = quantile(list_of_draws[[param]], 0.5), u_95 = quantile(list_of_draws[[param]], 0.975), 
                max = max(list_of_draws[[param]])
        )
}

for(idx in 1:5){
    param <- paste0("est", idx)
    forest_table <- forest_table %>%
        add_row(country = "AGO", parameter = param, 
                min = min(list_of_draws$eta[, idx]), l_95 = quantile(list_of_draws$eta[, idx], 0.025), 
                median = quantile(list_of_draws$eta[, idx], 0.5), u_95 = quantile(list_of_draws$eta[, idx], 0.975), 
                max = max(list_of_draws$eta[, idx])
        ) %>%
        add_row(country = "AGO", parameter = paste0("gam_est", idx), 
                min = NA, 
                l_95 = summary(initial_values_data$gam_fit_output)$p.coeff[idx] - qnorm(0.975) * summary(initial_values_data$gam_fit_output)$se[idx], 
                median = summary(initial_values_data$gam_fit_output)$p.coeff[idx], 
                u_95 = summary(initial_values_data$gam_fit_output)$p.coeff[idx] + qnorm(0.975) * summary(initial_values_data$gam_fit_output)$se[idx], 
                max = NA
        )
}



# plotting 
"https://www.statology.org/forest-plot-in-r/#:~:text=A%20forest%20plot%20(sometimes%20called,results%20from%20each%20individual%20study."
forest_table$index <- 1:nrow(forest_table)
plt <- forest_table %>%
    ggplot() +
    geom_rect(aes(xmin = l_95, xmax = u_95, ymin = index - 0.15, ymax = index + 0.15), fill = "lightblue") + 
    # ggplot(aes(y=index, x=median, xmin=min, xmax=max)) +
    geom_point(aes(y=index, x=median)) + 
    geom_errorbarh(aes(y=index, xmin=min, xmax=max), height=.1) +
    scale_y_continuous(breaks=1:nrow(forest_table), labels=forest_table$parameter) +
    labs(title='Estimates by parameters', x='Posterior distribution', y = 'Parameter') +
    geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
    theme_minimal()

pdf("Analysis/output/posterior_check.pdf")
plt
dev.off()
