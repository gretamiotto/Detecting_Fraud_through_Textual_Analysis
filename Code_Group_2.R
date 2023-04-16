## ----setup, include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, 
                      message = FALSE, 
                      warning = FALSE)


## ---- message = FALSE----------------------------------------------------------------
setwd('C:/Users/steef/Documents/NHH/BAN439 Detecting Fraud through Textual Analysis/BAN439---Detection-of-Fraud-Through-Textual-Analysis')

library(dplyr)
library(tidyverse)
library(ggplot2)
library(tm)
library(maps)
library(ggrepel)
library(RColorBrewer)
library(stringr)
library(tidyr)
library(igraph)
library(ggridges)

YlOrRd <- brewer.pal(9, 'YlOrRd')


## ----make-map------------------------------------------------------------------------

readLines('files_that_mention_Shell.txt') %>%
  gsub('.+?([A-Z]{1,})[0-9]{1,}.html','\\1',.) %>%
  as_tibble() %>%
  group_by(value) %>%
  summarise(shell_related_cables = n()) %>%
  rename(city = value) %>%
  mutate(city = tolower(city)) -> frequencies 

world.cities %>% # Retrive data about long and lat of cities 
  rename(city = name) %>%
  mutate(city = city %>%
           tolower() %>%
           gsub(' ', '', .) %>%
           gsub("'", '', .)) %>%
  filter(country.etc != 'USA') %>%
  group_by(city) %>%
  arrange(desc(pop)) %>%
  slice(n = 1) %>%
  ungroup() %>%
  right_join(.,frequencies) %>% 
  # Only select cities for which data was found (can be improved) 146 -> 125
  drop_na() -> map 

world <- map_data('world')
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    fill = 'lightgrey'
  ) +
  geom_point(data = map, 
             aes(long, lat, size = shell_related_cables),
             color = 'red', alpha = 0.5) +
  theme_void() +
  theme(legend.position = 'bottom') +
  # Add labels for cities (only if above 15 documents)
  geom_text_repel(data = map, 
                  aes(long, lat, 
                      label = ifelse(shell_related_cables > 15, str_to_title(city), '')),
                  size = 2.5) +
  guides(size = guide_legend(title = 'Number of cables mentioning Shell')) +
  labs(title = 'Figure 1: Distribution of cables mentioning Shell acrros the world')



## ----exploratory-analysis------------------------------------------------------------
load('nigeria_shell_df.Rdata')

nigeria.shell.df %>%
  mutate(Classification.simplified = 
           gsub('(.+)?//{1,}.+', '\\1', Classification) %>%
           str_to_title(),
         Year = gsub('([0-9]{4}).+','\\1', Created) %>%
           as.numeric(),
         Created = as.Date(Created),
         Sender = gsub('.+ (.)','\\1',Text) %>%
           tolower() %>%
           str_to_title()) -> nigeria.shell.df

nigeria.shell.df %>%
  ggplot() +
  geom_bar(aes(as.factor(Year), 
               fill = factor(Classification.simplified, 
                             levels = c('Secret', 'Confidential', 'Unclassified')))) +
  facet_wrap(~Origin, nrow = 2) +
  labs(y = 'Number of cables',
       x = 'Year',
       title = 'Figure 2: Cables distribution by type and location') +
  guides(fill = guide_legend(title = 'Sensitivity level')) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_fill_manual(values = c(YlOrRd[7], YlOrRd[5], YlOrRd[3])) 



## ----most-active-ambassadors---------------------------------------------------------

nigeria.shell.df %>%
  select(Sender) %>%
  mutate(Sender = gsub('#$','', Sender)) %>%
  group_by(Sender) %>%
  summarise(n()) %>%
  filter(`n()` > 1) %>%
  ggplot(aes(x = `n()`, y = reorder(Sender, `n()`), fill = `n()`)) +
  geom_bar(stat = 'identity') +
  scale_fill_distiller(palette = 'YlOrRd', direction = 1) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = 'none') +
  labs(y = 'Surname of the cable sender',
       x = 'Number of cables in the sample',
       title = 'Figure 3: Cable distribution by sender') 

nigeria.shell.df %>%
  select(Sender) %>%
  group_by(Sender) %>%
  summarise(n()) %>%
  filter(`n()` > 20) %>%
  pull(Sender) -> Ambassadors



## ----names-and-topics, fig.width = 12, fig.height = 19-------------------------------

nigeria.shell.df %>%
  select(Created, Sender, Text) %>%
  mutate(Oil.spills = grepl('oil spill', Text, ignore.case = TRUE),
         Human.rights = 
           grepl('attack', Text, ignore.case = TRUE) |
           grepl('violence', Text, ignore.case = TRUE) |
           grepl('human rights', Text, ignore.case = TRUE) |
           grepl('violation', Text, ignore.case = TRUE) |
           grepl('hostage', Text, ignore.case = TRUE) |
           grepl('kidnap', Text, ignore.case = TRUE),
         Corruption = 
           grepl('corruption', Text, ignore.case = TRUE) |
           grepl('bribe', Text, ignore.case = TRUE), 
         Protests = 
           grepl('protest', Text, ignore.case = TRUE) |
           grepl('strike', Text, ignore.case = TRUE) |
           grepl('militant', Text, ignore.case = TRUE),
         Military = grepl('military', Text, ignore.case = TRUE)) %>%
  pivot_longer(cols = Oil.spills:Military, 
               names_to = 'Topic', values_to = 'Mention') %>%
  filter(Mention == TRUE) %>%
  filter(Sender %in% Ambassadors) %>%
  ggplot() +
  geom_density_ridges(aes(x = Created, y = Topic, fill = Topic, color = Topic),
                      alpha = 0.1, size = 1) +
  theme_ridges() +
  theme(legend.position = 'bottom',
        panel.grid = element_blank(),
        axis.text = element_text(size = 16),
        text = element_text(size = 20),
        plot.title = element_text(size = 22)) +
  facet_wrap(~Sender, ncol = 2) +
  scale_fill_manual(values = 
                      c(YlOrRd[9], YlOrRd[7], YlOrRd[5], YlOrRd[4], YlOrRd[3])) +
  scale_color_manual(values = c(YlOrRd[9], YlOrRd[7], YlOrRd[5], YlOrRd[4], YlOrRd[3])) +
  labs(y = 'Topic',
       x = 'Date',
       title = 'Figure 4: Topic mentions distribution by date')



## ----corruption-by-name--------------------------------------------------------------

nigeria.shell.df %>%
  select(Created, Sender, Text) %>%
  mutate(Corruption = 
           grepl('corruption', Text, ignore.case = TRUE) |
           grepl('bribe', Text, ignore.case = TRUE)) %>%
  group_by(Sender) %>%
  mutate(first.cable = min(Created)) %>%
  ungroup() %>%
  filter(Sender %in% Ambassadors) %>%
  ggplot() +
  geom_point(aes(x = Created, 
                 y = reorder(Sender, first.cable, decreasing = TRUE), 
                 color = Corruption), size = 3, alpha = 0.4) +
  scale_color_manual(values = c(YlOrRd[4], YlOrRd[7])) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = 'bottom') +
  labs(y = 'Surname of the cable sender',
       x = 'Date',
       title = 'Figure 5: Corruption mentions distribution by date') +
  guides(color = guide_legend(title = 'Corruption was mentioned in the cable')) 



## ----make-functions-for-network-plots------------------------------------------------
# compute co-occurrence (function)
coumpute_co_occurence <- function(kw,
                                  cooc_limit = 0.25) {
  lapply(kw, 
         function(x) {
           # cables that include kw
           ix <- grepl(x, nigeria.shell.df$Text, ignore.case = T)
           
           # output matrix
           output <- tibble(kw1 = x,
                         kw2 = dplyr::setdiff(kw, x))
           
           # compute overlap variable
           output$cooccur <- sapply(output$kw2, function(x2) {
             sum(grepl(x2, nigeria.shell.df$Text[ix], ignore.case = T)) / sum(ix)
           })
           
           # output
           return(output)
         }) %>%
    # combine
    bind_rows() %>%
    filter(cooccur > cooc_limit) -> links
  return(links)
}

# make plot
make_network_plot <- function(network, title) {
  # Define width of arrows
  E(network)$width <- links$cooccur * 7
  
  # Define colors of arrows
  V(network)$color <- RColorBrewer::brewer.pal(5, 'YlOrRd')
  
  plot(
    network,
    main = title,
    edge.arrow.size = 0.8,
    # define the "general" size of the arrows
    vertex.shape = 'none',
    # take out the circles
    # define color
    edge.color = V(network)$color[ends(network, es = E(network), names =
                                         F)[, 1]],
    vertex.label.color = 'black',
    # color of text
    edge.curved = .3,
    # curve edges/arrows. Helps if arrows run both ways
    layout = layout_in_circle(network)
  ) # layout as circle
}



## ----general-keywords-network--------------------------------------------------------
# identify keywords
kw <- c('military', 
        'human right',
        'protest',
        'abuse',
        'kidnap')

links <- coumpute_co_occurence(kw)

# make network object
network <- graph_from_data_frame(d = links, directed = T)

make_network_plot(network,
                  title = 'Figure 6: General keywords network')



## ----corruption-network, out.height="70%"--------------------------------------------

# identify keywords
kw <- c('corruption',
        'bribe', 
        'government', 
        'military')

links <- coumpute_co_occurence(kw)

# make network object
network <- graph_from_data_frame(d=links, directed = T)

make_network_plot(network,
                  title = 'Figure 7: Government corruption network')

# identify keywords
kw <- c('corruption',
        'obansajo',
        'military',
        'bribe', 
        "yar'adua", 
        'asari', 
        'lukman',
        'ibori')

links <- coumpute_co_occurence(kw)

# make network object
network <- graph_from_data_frame(d=links, directed = T)

make_network_plot(network,
                  title = 'Figure 8: Politician corruption network')




## ----to-look-at----------------------------------------------------------------------

Subject.of.cable <- c('Corruption incentives', 'Bribe details', 
                      'Corruption awareness')

nigeria.shell.df %>%
  filter(Sender == 'Sanders' &
           (Created == '2009-02-10' |
              Created == '2009-10-20') |
           `Reference ID` == '06LAGOS1030') %>%
  select(-Year, -Classification, -Text, -file, - Released) %>%
  bind_cols(., Subject.of.cable) %>%
  rename(Subject.of.cable = `...6`) %>%
  select(Sender, Subject.of.cable, Origin, Created, 
         Classification.simplified, `Reference ID`) %>%
  knitr::kable(caption = 'Cables containing the most important information',
               col.names = c('Sender', 'Subject', 'Origin', 'Created', 
                             'Classification', 'Reference ID'))


