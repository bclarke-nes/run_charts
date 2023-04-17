# helper code to find good examples

# find a random sample and make run charts
run_chart(ae_attendances %>%
            filter(type == "1") %>%
            pull(org_code) %>%
            unique() %>%
            sample(5), breaches)

# find the outliers
ae_attendances %>%
  pivot_longer(c(admissions, attendances, breaches)) %>%
  group_by(org_code, type, name) %>%
  filter(type == "1", value > 10) %>%
  mutate(mean = mean(value)) %>%
  mutate(outlier = value / mean) %>%
  arrange(desc(outlier))

# more complicated ggplot with facets for type

run_chart <- function(org, cat) {
  
  ae_attendances %>%
    filter(org_code %in% {{org}}) %>%
    group_by(org_code, type) %>%
    mutate(median = median({{cat}})) %>%
    ggplot(aes(x=period, y={{cat}}, color=org_code)) +
    geom_point() +
    geom_line() +
    geom_hline(aes(yintercept=median, color=org_code)) +
    ggtitle(glue("{paste(org, collapse=' / ')} {deparse(substitute(cat))}")) +
    facet_wrap(~ type + org_code, scales="free")
}