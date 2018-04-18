
# prereq ------------------------------------------------------------------

require(xml2)
require(ggthemes)
require(ggrepel)
require(tidyverse)

dir <- 'data/'

fol <- 'data/Heiz_chain_dirloop.task1.out.xml'
# functions ---------------------------------------------------------------

my_read_xml <- function(fol,param = c('J','L','T')) {
  df <- read_xml(fol)
  par <- xml_find_all(df,'.//PARAMETER') 
  df_param <- tibble(mod_param = xml_attr(par,'name'),
                     mod_value  = xml_text(par))%>% 
    filter(mod_param %in% param) %>% 
    mutate(key = fol)
  
  dat <- xml_find_all(df,'.//AVERAGES//SCALAR_AVERAGE') 
  df_data <- tibble(parameter = xml_attr(dat,'name'),
                    value  = xml_child(dat,search = 'MEAN') %>% xml_text(),
                    key = fol)
  df_res <- left_join(df_param,df_data) %>% 
    select(-key) %>% 
    spread(mod_param,mod_value) %>% 
    rename(temp = `T`)
  return(df_res)
}


# read file ---------------------------------------------------------------



df <- tibble(dirs = list.files(dir,pattern = 'dirloop\\.task[0-9]{1}\\.out\\.xml|[0-9]{2}\\.out\\.xml',full.names = T)) %>% 
  mutate(model = str_extract_all(dirs,'Heiz|Hubbard',simplify = T)[,1],
         data = map(dirs,my_read_xml)) %>% 
  unnest() %>% 
  select(-dirs) %>% 
  mutate(value = as.numeric(value))



df %>% 
  mutate_at(vars(J,value),funs(as.numeric)) %>% 
  ggplot(aes(J,value,col = as.factor(temp)),group = temp) +
  geom_point() +
  # geom_label_repel(aes(label = str_c('Temp=',`T`))) +
  geom_line() +
  facet_wrap(c('parameter'),scales = 'free') +
  theme_bw()
