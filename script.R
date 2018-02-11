library(dplyr)
library(tidyr)
library(tibble)
library(tidyjson)
library(ca)
library(d3heatmap)

stellingen_json <- readLines('data/kieskompas_stellingen.json')
answers_json    <- readLines('data/kieskompas_answers.json')

stellingen_df <- jsonlite::fromJSON(stellingen_json, flatten = TRUE)
answers_df    <- jsonlite::fromJSON(answers_json, flatten = TRUE) 
answers_df <- answers_df %>% 
  group_by(id, name, short_name, logo, chart_color) %>% 
  do(.$answers[[1]]) %>% 
  ungroup %>% 
  select(-answer.is_skip)

kieskompas_df <- answers_df %>%
  left_join(stellingen_df %>% 
              select(id, statement_name = name, statement_text = statement, axis, inverse, theme.name, theme.identifier),
            by = c('statement' = 'id'))

# verdeling over partijen
kieskompas_df %>% count(name, answer.text_content)

# table
kieskompas_table <- kieskompas_df %>%
  select(name, theme.name, statement_text, answer.value) %>% 
  mutate(answer.value = if_else(answer.value == 99, as.integer(0), answer.value)) %>% 
  spread(name, answer.value)

kk_mat <- kieskompas_table %>% 
  remove_rownames() %>% 
  as.data.frame() %>% 
  column_to_rownames('statement_text') %>% 
  select(-theme.name)

# Correlation heatmap
cor(kk_mat) %>% d3heatmap()
d3heatmap(kk_mat)

# MDS
library(ggrepel)
cmdscale(dist(kk_mat),2) %>%
  as.data.frame %>%
  rownames_to_column('partij') %>%
  ggplot(aes(x=V1, y=V2)) + 
    geom_point() + 
    geom_text_repel(aes(label = partij),size = 3) + 
    theme_minimal()
