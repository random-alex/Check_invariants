
# prereq ------------------------------------------------------------------

require(xml2)
require(ggthemes)
require(ggrepel)
require(tidyverse)

dir <- 'data/'

fol <- 'data/Heiz_chain_dirloop.task1.out.xml'

# read file ---------------------------------------------------------------

pat <- 'Heiz_square\\.task[0-9]{1}\\.out\\.xml'
pat <- 'Heiz_chain\\.task[0-9]{1}\\.out\\.xml'

df <- tibble(dirs = list.files(dir,
                               pattern = pat,
                               full.names = T)) %>% 
  mutate(model = str_extract_all(dirs,'Heiz|Hubbard',simplify = T)[,1],
         data = map(dirs,my_read_xml_aveg)) %>% 
  unnest() %>% 
  select(-dirs) %>% 
  mutate(value = as.numeric(value))

df %>% 
  filter(parameter != 'Ground State Energy')



# pics --------------------------------------------------------------------


df %>% 
  mutate_at(vars(J,value),funs(as.numeric)) %>%
  arrange(as.numeric(temp)) %>% 
  ggplot(aes(J,value)) +
  geom_point(aes(col = as_factor(temp)),size = 2) +
  # geom_label_repel(aes(label = str_c('Temp=',`T`))) +
  geom_line() +
  facet_wrap(c('parameter'),scales = 'free') +
  scale_color_brewer(palette="Paired") +
  theme_bw()


df_mod <- df %>% 
  filter(parameter == 'Energy Gap') %>% 
  mutate(J  = as.numeric(J))
mod <- lm(value ~ J,df_mod)
modelr::add_predictions(df_mod,mod)

tibble(intercept = mod$coefficients[1],
       slope = mod$coefficients[2])
