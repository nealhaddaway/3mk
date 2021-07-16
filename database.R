source('dataframe2html.R')
data <- read.csv('./inst/extdata/3mk_map.csv', stringsAsFactors = FALSE)
data$metals <- data$metals_all
data <- subset(data, select=-c(article_id,
                review_id,
                eppi_id,
                authors,
                title,
                year,
                type,
                journal,
                unique_mine,
                metals_all,
                multiple_metals))

data[] <- lapply(data, function(x) gsub('Human environment', 'Societies', x))
data$effect <- paste0('_', gsub("[^A-Za-z0-9]","",paste0(trimws(sub('.*-', '', data$affected_system)), trimws(sub('.*-', '', data$affected_component)), trimws(sub('.*-', '', data$affected_factor)))))

library(magrittr)
library(tidyverse)
#separate
data$stage_abandonment <- data$stage_abandonment %>% 
  gsub('Y', 'abandonment', .) %>%
  gsub('N||NR||NS', '', .)
data$stage_prospecting <- data$stage_prospecting %>% 
  gsub('Y', 'prospecting', .) %>%
  gsub('N||NR||NS', '', .)
data$stage_exploration <- data$stage_exploration %>% 
  gsub('Y', 'exploration', .) %>%
  gsub('N||NR||NS', '', .)
data$stage_construction <- data$stage_construction %>% 
  gsub('Y', 'construction', .) %>%
  gsub('N||NR||NS', '', .)
data$stage_operation <- data$stage_operation %>% 
  gsub('Y', 'operation', .) %>%
  gsub('N||NR||NS', '', .)
data$stage_decommissioning_closure <- data$stage_decommissioning_closure %>% 
  gsub('Y', 'decommissioning_closure', .) %>%
  gsub('N||NR||NS', '', .)
data$stage_postclosure <- data$stage_postclosure %>% 
  gsub('Y', 'postclosure', .) %>%
  gsub('N||NR||NS', '', .)
data$stage_remediation <- data$stage_remediation %>% 
  gsub('Y', 'remediation', .) %>%
  gsub('N||NR||NS', '', .)
data$stage_expansion <- data$stage_expansion %>% 
  gsub('Y', 'expansion', .) %>%
  gsub('N||NR||NS', '', .)
data[data == ""] <- NA
data[data == "N/A"] <- NA

data <- data %>%
  tidyr::unite(data,
               starts_with(paste('stage_',
                                 sep = '')),
               sep = '; ',
               na.rm = TRUE,
               remove = TRUE)

data <- data[order(data$citation),]
html <- dataframe2html(data,
                       tooltips = '',
                       table_width = '6000px',
                       hyperlinks = 'TRUE')

