
require(xml2)
require(ggthemes)
require(ggrepel)
require(RColorBrewer)
require(tidyverse)


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


