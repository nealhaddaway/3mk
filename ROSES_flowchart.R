dbresults_text <- 'Records identified from bibliographic database searches'
otherresults_text <- 'Database results:'
deduped_text <- 'Records after duplicates removed'
dupesremoved_text <- 'Duplicates removed'
tandaincl_text <- 'Records after title and abstract screening'
tandaexcl_text <- 'Excluded titles and abstracts'
titleincl_text <- 'Records after title screening'
titleexcl_text <- 'Excluded titles'
abstractincl_text <- 'Records after abstract screening'
abstractexcl_text <- 'Excluded abstracts'
ftretr_text <- 'Articles retrieved at full text'
ftnotretr_text <- 'Unretrievable full texts'
ftincl_text <- 'Articles after full text screening'
ftexcl_text <- 'Excluded full texts'
prescreened_text <- 'Pre-screened records from other sources (websites and review bibliographies)'
studart_text <- 'Articles / Studies included in the review'
caincl_text <- 'Studies included after critical appraisal'
caexcl_text <- 'Excluded from further synthesis:'
narrincl_text <- 'Studies included in narrative synthesis'
finalincl_text <- 'Studies included in quantitative/ qualitative/ other synthesis'
finalexcl_text <- 'Studies not included in further synthesis'
finalmapincl_text <- 'Studies included in the systematic map database and narrative synthesis'

ROSES_flowchart <- function (dbresults,
                             otherresults,
                             deduped,
                             dupesremoved,
                             tandaincl,
                             tandaexcl,
                             titleincl,
                             titleexcl,
                             abstractincl,
                             abstractexcl,
                             ftretr,
                             ftnotretr,
                             ftincl,
                             ftexcl,
                             prescreened,
                             studart,
                             caincl,
                             caexcl,
                             narrincl,
                             finalincl,
                             finalexcl,
                             finalmapincl,
                             interactive = FALSE,
                             type,
                             combined = FALSE,
                             font = 'Helvetica',
                             input_colour = 'Gainsboro',
                             output_colour = 'LightSteelBlue1',
                             main_colour = 'Black',
                             arrow_colour = 'Black',
                             arrow_head = 'normal',
                             arrow_tail = 'none') {
  
    #------------------------------------------------------------------------
    
      ystart <- 1.5
      titleabstract <- paste0('titleincl [label = \'', paste0(text_wrap(titleincl_text, 40),
                                                              '\n(n = ',
                                                              titleincl,
                                                              ')'
      ), "', width = 4, height = 0.5, pos='",4,",", 10,"!', tooltip = '']
      
      titleexcl [label = '", paste0(text_wrap(titleexcl_text, 40),
                                    '\n(n = ',
                                    titleexcl,
                                    ')'
      ), "', width = 4, height = 0.5, pos='", 9, ",", 10, "!', tooltip = '']
      
      abstractincl [label = \'", paste0(text_wrap(abstractincl_text, 40),
                                        '\n(n = ',
                                        abstractincl,
                                        ')'
      ), "', width = 4, height = 0.5, pos='",4,",",8.5,"!', tooltip = '']
      
      abstractexcl [label = '", paste0(text_wrap(abstractexcl_text, 40),
                                       '\n(n = ',
                                       abstractexcl,
                                       ')'
      ), "', width = 4, height = 0.5, pos='", 9, ",", 8.5, "!', tooltip = '']
      ")
      tanda_edges <- 'deduped->titleincl;\ntitleincl->titleexcl;\ntitleincl->abstractincl;\nabstractincl->abstractexcl;\nabstractincl->ftretr;\n'
      screenboxh <- 8.65
      screenboxy <- 7.65
      searchboxy <- 13
    
    if(nrow(ftexcl) > 5){
      ftexclh <- 5.5 - ((nrow(ftexcl)-5)/4)
    } else {
      ftexclh <- 5.5
    }
    
    #------------------------------------------------------------------------
    x <- DiagrammeR::grViz(
      paste0("digraph TD {
      
      graph[splines=ortho, layout=neato, tooltip = 'Click the boxes for further information', fontsize = 20]
      
      node [shape = box,
            fontname = ", font, ",
            color = ", input_colour, "]
      dbresults [label = '", paste0(text_wrap(dbresults_text, 40),
                                    '\n(n = ',
                                    dbresults,
                                    ')'
      ), "', width = 4, height = 0.5, pos='4,",ystart+14.3,"!', tooltip = '', style = 'filled']
      
      otherresults [label = '", paste0(text_wrap(otherresults_text, 40),
                                       '\n',
                                       otherresults
      ), "', width = 4, height = 3.5, pos='9,",ystart+13,"!', tooltip = '', style = 'filled']
      
      prescreened [label = '", paste0(text_wrap(prescreened_text, 30),
                                      '\n(n = ',
                                      prescreened,
                                      ')'
      ), "', width = 2.5, height = 0.5, pos='2,4!', tooltip = '', style = 'filled']
    
      node [shape = box,
            fontname = ", font, ",
            color = ", main_colour, "]
      deduped [label = '", paste0(text_wrap(deduped_text, 40),
                                  '\n(n = ',
                                  deduped,
                                  ')'
      ), "', width = 4, height = 0.5, pos='4,",ystart+10,"!', tooltip = '']
      
      dupesremoved [label = '", paste0(text_wrap(dupesremoved_text, 40),
                                       '\n(n = ',
                                       dupesremoved,
                                       ')'
      ), "', width = 4, height = 0.5, pos='9,",ystart+10,"!', tooltip = '']
    
      ", titleabstract, "
    
      ftretr [label = '", paste0(text_wrap(ftretr_text, 40),
                                 '\n(n = ',
                                 ftretr,
                                 ')'
      ), "', width = 4, height = 0.5, pos='4,7!', tooltip = '']
      
      ftnotretr [label = '", paste0(text_wrap(ftnotretr_text, 40),
                                    '\n(n = ',
                                    ftnotretr,
                                    ')'
      ), "', width = 4, height = 0.5, pos='9,7!', tooltip = '']  
    
      ftincl [label = '", paste0(text_wrap(ftincl_text, 40),
                                 '\n(n = ',
                                 ftincl,
                                 ')'
      ), "', width = 4, height = 0.5, pos='4,5.5!', tooltip = '']
      
      ftexcl [label = '", paste0(text_wrap(ftexcl_text, 40),
                                 '\n\nExcluded on:\n',
                                 paste(paste0(ftexcl[,1], ' (n=', ftexcl[,2], ')'), collapse = '\n')
      ), "', width = 4, height = 0.5, pos='9,", ftexclh, "!', tooltip = '']  
    
      studart [label = '", paste0(text_wrap(studart_text, 40),
                                  '\n(n = ',
                                  studart[1],
                                  ' / n = ',
                                  studart[2],
                                  ')'
      ), "', width = 4, height = 0.5, pos='4,2.5!', tooltip = '']
      
      node [shape = box,
            fontname = ", font, ",
            color = ", output_colour, "]
      finalmapincl [label = '", paste0(text_wrap(finalmapincl_text, 40),
                                       '\n(n = ',
                                       finalmapincl,
                                       ')'
      ), "', width = 4, height = 0.5, pos='4,1!', tooltip = '', style = 'filled']
      
      node [shape = square, width = 0, color=White]
      A2 [label = '', width = 0, height = 0, pos='4,4!', tooltip='']
      C1 [label = '', width = 0, height = 0, pos='0.5,2.5!', tooltip='']
      C2 [label = '', width = 0, height = 0, pos='1.9,2.5!', tooltip='']
      D1 [label = '', width = 0, height = 0, pos='6.1,2.5!', tooltip='']
      D2 [label = '', width = 0, height = 0, pos='11.5,2.5!', tooltip='']
      
            
      edge [color = 'Goldenrod1', 
            style = filled,
            arrowhead = 'none',
            penwidth = 4,
            alpha = 0.2]
      C1->C2; D1->D2
      
      edge [color = ", arrow_colour, ", 
            arrowhead = ", arrow_head, ", 
            arrowtail = ", arrow_tail, ", 
            style = filled,
            penwidth = 0.7]
      deduped->dupesremoved;
      ", tanda_edges, "
      ftretr->ftnotretr;
      ftretr->ftincl;
      ftincl->ftexcl;
      prescreened->A2;
      studart->finalmapincl;
      dbresults->deduped;
      dbresults->otherresults;
      
      edge [color = ", arrow_colour, ", 
            arrowhead = 'none', 
            arrowtail = ", arrow_tail, ", 
            style = filled]
      ftincl->A2;
      
      edge [color = ", arrow_colour, ", 
            arrowhead = ", arrow_head, ", 
            arrowtail = 'none', 
            style = filled]
      A2->studart;
      
      }"))
  
  return(x)
}


#------------------------------------------------------------------------
text_wrap <- function(text, width){
  text <- stringr::str_wrap(text,
                            width = width)
  return(text)
}