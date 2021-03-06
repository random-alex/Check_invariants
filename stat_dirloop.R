
# prereq ------------------------------------------------------------------

require(xml2)
require(ggthemes)
require(ggrepel)
require(RColorBrewer)
require(tidyverse)

dir <- 'data/'
pat <- 'Heiz_square_dirloop\\.task[0-9]{1}\\.out\\.xml'
pat <- 'Heiz_chain_dirloop\\.task[0-9]{1}\\.out\\.xml'


# functions ---------------------------------------------------------------

my_read_xml_aveg <- function(fol,param = c('J','L','T')) {
  df <- read_xml(fol)
  par <- xml_find_all(df,'.//PARAMETER') 
  df_param <- tibble(mod_param = xml_attr(par,'name'),
                     mod_value  = xml_text(par)) %>% 
    filter(mod_param %in% param) %>% 
    mutate(key = fol)
  
  dat <- xml_find_all(df,'.//AVERAGES//SCALAR_AVERAGE') 
  
  df_data <- tibble(parameter = xml_attr(dat,'name'),
                    value  = xml_child(dat,search = 'MEAN') %>% xml_text(),
                    error = xml_child(dat,search = 'ERROR') %>% xml_text(),
                    key = fol) %>% 
    drop_na()
 
  df_res <- left_join(df_param,df_data) %>% 
    select(-key) %>% 
    spread(mod_param,mod_value) %>% 
    rename(temp = `T`)
  return(df_res)
}

my_read_xml_corr <- function(fol,param = c('J','L','T')) {
  df <- read_xml(fol)
  par <- xml_find_all(df,'.//PARAMETER') 
  df_param <- tibble(mod_param = xml_attr(par,'name'),
                     mod_value  = xml_text(par)) %>% 
    filter(mod_param %in% param) %>% 
    mutate(key = fol)
  
  dat <- xml_find_all(df,'.//AVERAGES//SCALAR_AVERAGE') 

  df_data <- tibble(parameter = 'spin_corr',
                    coordinats = xml_attr(dat,'indexvalue') %>% 
                      str_replace_all('\\( | \\)','') %>% 
                      str_replace_all(' -- ','__') ,
                    value  = xml_child(dat,search = 'MEAN') %>% xml_text(),
                    error = xml_child(dat,search = 'ERROR') %>% xml_text(),
                    key = fol) %>% 
    drop_na()
  
  df_res <- left_join(df_param,df_data) %>% 
    select(-key) %>% 
    spread(mod_param,mod_value) %>% 
    rename(temp = `T`)
  return(df_res)
}




# read file ---------------------------------------------------------------

df <- tibble(dirs = list.files(dir,pattern = pat,full.names = T)) %>% 
  mutate(model = str_extract_all(dirs,'Heiz|Hubbard',simplify = T)[,1],
         data = map(dirs,my_read_xml_aveg)) %>% 
  unnest() %>% 
  select(-dirs) %>% 
  mutate(value = as.numeric(value),error = as.numeric(error))

df_corr <- tibble(dirs = list.files(dir,
                                    pattern = pat,
                                    full.names = T)) %>% 
  mutate(model = str_extract_all(dirs,'Heiz|Hubbard',
                                 simplify = T)[,1],
         data = map(dirs,my_read_xml_corr)) %>% 
  unnest() %>% 
  select(-dirs) %>% 
  mutate(value = as.numeric(value),error = as.numeric(error))




# plots -------------------------------------------------------------------

df %>% 
  mutate_at(vars(J,value),funs(as.numeric)) %>%
  arrange(as.numeric(temp)) %>% 
  ggplot(aes(J,value)) +
  # geom_point(aes(col = as_factor(temp)),size = 2) +
  geom_pointrange(aes(col = as_factor(temp),ymin = value - error, ymax = value + error),size = 1) +
  geom_line() +
  facet_wrap(c('parameter'),scales = 'free') +
  scale_color_brewer(palette="Paired") +
  theme_bw()

df_corr %>% 

  filter(coordinats == '0,0__0,1') %>% 
  arrange(as.numeric(temp)) %>% 
  mutate_at(vars(J,value),funs(as.numeric)) %>% 
  ggplot(aes(J,value)) +
  geom_pointrange(aes(col = as_factor(temp),ymin = value - error, ymax = value + error),size = 1) +
  geom_line() +
  facet_wrap(c('parameter'),scales = 'free') +
  scale_color_brewer(palette="Paired") +
  theme_bw()





