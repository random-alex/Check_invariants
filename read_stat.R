
# prereq ------------------------------------------------------------------

require(xml2)
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
    spread(mod_param,mod_value)
  return(df_res)
}


# read file ---------------------------------------------------------------



df <- tibble(dirs = list.files(dir,pattern = 'chain\\.task[0-9]{1}\\.out\\.xml|[0-9]{2}\\.out\\.xml',full.names = T)) %>% 
  mutate(model = str_extract_all(dirs,'Heiz|Hubbard',simplify = T)[,1],
         data = map(dirs,my_read_xml)) %>% 
  unnest() %>% 
  select(-dirs) %>% 
  mutate(value = as.numeric(value))

df %>% 
  filter(parameter != 'Ground State Energy')

 df %>% 
  ggplot

df <- tibble(dirs = list.files(dir,pattern = 'dirloop\\.task[0-9]{1}\\.out\\.xml|[0-9]{2}\\.out\\.xml',full.names = T)) %>% 
  mutate(model = str_extract_all(dirs,'Heiz|Hubbard',simplify = T)[,1],
         data = map(dirs,my_read_xml)) %>% 
  unnest() %>% 
  select(-dirs) %>% 
  mutate_at(vars(value,J,`T`),funs(as.numeric))

res <- df %>% 
  filter(str_detect(parameter,' Magnetization$')) %>% 
  arrange(J,`T`) %>% 
  .[c(2,5),]
res
res$value
