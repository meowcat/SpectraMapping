

arusdata <- sp_arus@backend@variables %>% nest(data = c(-spectrum_id, -formatKey)) %>% pivot_wider(names_from = "formatKey", values_from = "data")

massbankdata <- sp4@backend@variables %>% nest(data = c(-spectrum_id, -formatKey)) %>% pivot_wider(names_from = "formatKey", values_from = "data")

massbankdata <- sp4@backend@variables %>% pivot_wider(names_from = "formatKey", values_from = "value", values_fn = list)

arusdata <- sp_arus@backend@variables %>% pivot_wider(names_from = "formatKey", values_from = "value", values_fn = list)


massbankdata$`AC$MASS_SPECTROMETRY` %>% 
  map( ~ tibble(value = .x)) %>% 
  map( ~ extract(.x, value, regex = "(.*?) (.*)", into = c("key", "value"))) %>%
    map(pivot_wider, 
        names_from = "key", 
        names_prefix = "MS@", 
        values_from = "value",
        values_fn = list)


massbankdata3 <- 
  massbankdata %>% mutate(
    `AC$CHROMATOGRAPHY` = `AC$CHROMATOGRAPHY`  %>% 
  map( ~ tibble(value = .x) %>% mutate(id_dummy = 1)) %>% 
  map( ~ extract(.x, value, regex = "(.*?) (.*)", into = c("key", "value"))) %>%
  map(pivot_wider, 
      names_from = "key", 
      names_prefix = "MS@", 
      values_from = "value",
      values_fn = list)
  )

massbankdata4 <- massbankdata3[c(1,1),]
massbankdata4$`AC$CHROMATOGRAPHY`[[2]] <- 
  massbankdata4$`AC$CHROMATOGRAPHY`[[2]] %>% mutate(gugus2 = list(c("b", "b"))) %>% select(-`MS@COLUMN_NAME`)

massbankdata4$`AC$CHROMATOGRAPHY` %>% bind_rows()
