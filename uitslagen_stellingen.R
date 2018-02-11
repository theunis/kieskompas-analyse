library(dplyr)
library(tibble)
library(ggplot2)
library(forcats)
uitslag_zetels <- tibble(partij = c('VVD','PVV','CDA','D66','GL','SP','PvdA','CU','PvdD','50Plus','SGP','DENK','FvD'),
                         uitslag = c(33, 20, 19, 19, 14, 14, 9, 5, 5, 4, 3, 3, 2)) %>% 
  arrange(partij) %>% 
  mutate(uitslag_prop = uitslag/sum(uitslag))

kieskompas_parties <- colnames(kk_mat)
selected_parties <- intersect(kieskompas_parties, uitslag_zetels$partij)

kk_mat <- kk_mat[,(kieskompas_parties %in% selected_parties)]
uitslag_zetels <- uitslag_zetels %>% filter(partij %in% selected_parties)

uitslag_stellingen <- as.matrix(kk_mat) %*% uitslag_zetels$uitslag_prop %>%
  as_tibble() %>% cbind(kieskompas_table %>% select(theme.name, statement_text)) %>%
  select(theme.name, statement_text, value = V1) %>% 
  arrange(desc(value))

ggplot(uitslag_stellingen) + 
  geom_bar(aes(x=fct_reorder(statement_text, value), y=value, fill = theme.name), stat = 'identity') + 
  coord_flip()
