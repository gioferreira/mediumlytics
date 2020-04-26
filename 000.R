
## Base de dados de Posts Totais por mês para comparar com média ou soma de recommends
total_posts_month <- posts_tbl_processed %>%
  group_by(day_published = floor_date(first_published_at, "1 month")) %>%
  tally()

## Média de recommends, total_clap_count e total posts
posts_tbl_processed %>%
  group_by(day_published = floor_date(first_published_at, "1 month")) %>%
  summarise(
    mean_recommends = mean(recommends),
    mean_clap_count = mean(total_clap_count)
  ) %>%
  gather(key, value, -day_published) %>%
  ggplot(mapping = aes(x = day_published, y = value, fill = key)) +
  ggtitle("Média de Recommends, Total Clap Count e Posts por mês") +
  geom_bar(stat = "identity") +
  theme_tufte() +
  scale_fill_economist() +
  theme(axis.title = element_blank(),
        legend.title = element_blank()) +
  geom_line(
    data = total_posts_month,
    mapping = aes(x = day_published, y = n),
    inherit.aes = FALSE
  )


## Soma de recommends, total_clap_count per month
posts_tbl_processed %>%
  group_by(day_published = floor_date(first_published_at, "1 month")) %>%
  summarise(
    sum_recommends = sum(recommends),
    sum_clap_count = sum(total_clap_count)
  ) %>%
  gather(key, value, -day_published) %>%
  ggplot(mapping = aes(x = day_published, y = value, fill = key)) +
  ggtitle("Soma de Recommends e Total Clap Count por mês") +
  geom_bar(stat = "identity") +
  theme_tufte() +
  scale_fill_economist() +
  theme(axis.title = element_blank(),
        legend.title = element_blank())

## Success Metrics overtime
posts_tbl_processed %>%
  mutate(claps_per_clapper = total_clap_count / recommends) %>%
  group_by(day_published = floor_date(first_published_at, "1 month")) %>%
  summarise(
    mean_recommends = mean(recommends),
    mean_clap_count = mean(total_clap_count),
    mean_responses = mean(responses_created_count),
    mean_claps_per_clapper = mean(claps_per_clapper)
  ) %>%
  gather(key, value, -day_published) %>%
  ggplot(mapping = aes(x = day_published, y = value)) +
  ggtitle("Média de Métricas de Sucesso") +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(key), scales = "free_y") +
  theme_tufte() +
  theme(axis.title = element_blank())

## Diff from mean
posts_tbl_processed %>%
  group_by(day_published = floor_date(first_published_at, "1 day")) %>%
  mutate(
    pmean_clap_count = mean(total_clap_count),
    pmean_recommends = mean(recommends),
    pmean_responses = mean(responses_created_count)
  ) %>%
  ungroup() %>%
  mutate(
    diff_pmean_clap_count = total_clap_count - pmean_clap_count,
    diff_pmean_recommends = recommends - pmean_recommends,
    diff_pmean_responses = responses_created_count - pmean_responses
  ) %>%
  mutate(success_score = (
    (diff_pmean_clap_count) +
      (2 * diff_pmean_recommends) +
      (3 * diff_pmean_responses)
  ) / 6) %>%
  mutate(success_score_normal = (success_score - min(success_score)) / (max(success_score) -
                                                                          min(success_score))) %>%
  # group_by(day_published) %>%
  select(
    total_clap_count,
    recommends,
    responses_created_count,
    pmean_clap_count,
    pmean_recommends,
    pmean_responses,
    diff_pmean_recommends,
    diff_pmean_responses,
    diff_pmean_clap_count,
    success_score,
    success_score_normal,
    id,
    first_published_at,
    day_published,
    title
  ) %>%
  # filter(first_published_at >= "2017-01-01",
  #        first_published_at < "2018-01-01") %>%
  arrange(desc(success_score_normal)) %>%
  select(title) %>%
  head(n = 20L)

normalized = (x - min(x)) / (max(x) - min(x))


########### Success Score Normal over time
#### CHECAR se pmean should be pmedian
posts_tbl_processed %>%
  group_by(day_published = floor_date(first_published_at, "1 day")) %>%
  mutate(
    pmean_clap_count = mean(total_clap_count),
    pmean_recommends = mean(recommends),
    pmean_responses = mean(responses_created_count)
  ) %>%
  ungroup() %>%
  mutate(
    diff_pmean_clap_count = total_clap_count - pmean_clap_count,
    diff_pmean_recommends = recommends - pmean_recommends,
    diff_pmean_responses = responses_created_count - pmean_responses
  ) %>%
  mutate(success_score = (
    (diff_pmean_clap_count) +
      (2 * diff_pmean_recommends) +
      (4 * diff_pmean_responses)
  ) / 7) %>%
  mutate(success_score_normal =
           (success_score - min(success_score)) /
           (max(success_score) - min(success_score))) %>%
  mutate(type = as_factor(
    case_when(
      success_score_normal > 0.32 ~ "Success",
      success_score_normal <= 0.32 &
        success_score_normal > 0.29 ~ "Average",
      TRUE ~ "Below Average"
    )
  )) %>%
  select(
    first_published_at,
    success_score_normal,
    total_clap_count,
    responses_created_count,
    recommends,
    type
  ) %>%
  gather(key = "métrica", value = "valor", -first_published_at, -type) %>%
  ggplot(aes(x = first_published_at, y = valor, color = type)) +
  geom_point() +
  facet_grid(rows = vars(métrica), scales = "free_y")

ggplot(aes(
  x = first_published_at,
  y = success_score,
  color = type,
  alpha = .1
)) +
  geom_point() #+ geom_smooth() + facet_grid(rows = vars(type))
filter(type == "Success") %>%
  arrange(desc(success_score_normal)) %>%
  select(id, title) %>%
  print(n = 20)


ggplot(aes(
  x = first_published_at,
  y = total_clap_count,
  color = type,
  alpha = .1
)) +
  geom_point()

geom_histogram(binwidth = 100) +
  facet_grid(rows = vars(type), scales = "free_y")


filter(success_score_normal > 0.33) %>%
  ggplot(mapping = aes(x = image_count, y = success_score_normal, alpha = .1)) +
  geom_point()





##############################
posts_tbl_processed %>%
  group_by(day_published = floor_date(first_published_at, "1 day")) %>%
  mutate(
    pmean_clap_count = mean(total_clap_count),
    pmean_recommends = mean(recommends),
    pmean_responses = mean(responses_created_count)
  ) %>%
  ungroup() %>%
  mutate(
    diff_pmean_clap_count = total_clap_count - pmean_clap_count,
    diff_pmean_recommends = recommends - pmean_recommends,
    diff_pmean_responses = responses_created_count - pmean_responses
  ) %>%
  filter(diff_pmean_recommends <= 500,
         diff_pmean_recommends >= -500) %>%
  ggplot(mapping = aes(x = diff_pmean_recommends,
                       y = diff_pmean_clap_count,
                       alpha = 0.5)) +
  geom_point(position = "jitter") +
  geom_smooth()#(method = "lm")




#################################################
## Clap over time + Success Metrics Over Time
posts_tbl_processed %>%
  mutate(
    days_since_posted = (first_published_at %--% "2018-07-24") / ddays(),
    claps_over_time = total_clap_count / days_since_posted,
    claps_per_clapper = total_clap_count / recommends
  ) %>%
  group_by(day_published = floor_date(first_published_at, "1 month")) %>%
  summarise(
    mean_recommends = mean(recommends),
    mean_clap_count = mean(total_clap_count),
    mean_responses = mean(responses_created_count),
    mean_claps_over_time = mean(claps_over_time),
    mean_claps_per_clapper = mean(claps_per_clapper)
  ) %>%
  gather(key, value, -day_published) %>%
  ggplot(mapping = aes(x = day_published, y = value)) +
  ggtitle("Média de Métricas de Sucesso") +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(key), scales = "free_y") +
  theme_tufte() +
  theme(axis.title = element_blank())


## Hist Recommends
posts_tbl_processed %>%
  filter(recommends < 500) %>%
  ggplot(aes(recommends)) +
  ggtitle("Histograma de Unique Clappers") +
  geom_histogram(binwidth = 5) +
  theme_tufte() +
  theme(axis.title = element_blank(),
        legend.title = element_blank())

## Hist Responses
posts_tbl_processed %>%
  filter(responses_created_count < 20) %>%
  ggplot(aes(responses_created_count)) +
  ggtitle("Histograma de Responses") +
  geom_histogram(binwidth = 1) +
  theme_tufte() +
  theme(axis.title = element_blank(),
        legend.title = element_blank())


## Hist counts
posts_tbl_processed %>%
  filter(image_count < 20) %>%
  ggplot(aes(image_count)) +
  ggtitle("Histograma Image Count") +
  geom_histogram(binwidth = 1) +
  theme_tufte() +
  theme(axis.title = element_blank(),
        legend.title = element_blank())
