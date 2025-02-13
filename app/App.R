
# R version 4.4.1


# Load packages----
library(shiny) # 1.9.1
library(bslib) #
library(reactable) # 0.4.4
library(rio) # 1.2.3
library(tidyverse) # 2.0.0
library(shinyFeedback) # 0.4.0
library(circlize) # 0.4.16
library(ComplexHeatmap) # 2.21.1
library(shinybusy) # 0.3.3
library(fst) # 0.9.18




# Set working directory----




# Load data----
obs_df <- rio::import("Observational_data.csv")
prot_anno <- rio::import("ProteinInstruments_annotation.csv")
measured_prot_anno <- rio::import("ProteinsMeasurements_annotation.csv")
met_anno <- rio::import("Metabolites_annotation.csv")
studios_description <- rio::import("Studies_description.csv")
mr_df <- fst::read_fst("MendelianRandomization_data.fst")
annex <- fst::read_fst("www/Proteomics_and_metabolomics_analyses_in_POEM.fst")
export(annex, "www/Proteomics_and_metabolomics_analyses_in_POEM.csv")
rm(annex)






# Custom functions and options----

# Function to download sorted reactables
convertToDataFrame <- function(x, session, inputname) {
  do.call("rbind.data.frame", x)
}

registerInputHandler("to_csv", convertToDataFrame, force = TRUE)




# Function to transform Heatmap metabolites' names to uppercase
first_letter_uppercase <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


# Option to plot the longest heatmaps
options(ragg.max_dim = 100000)








# User interface----
ui <- fluidPage(
  
  # Prevent error messages showing in user interface
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  
  
  
  # Loading data spinner
  busy_start_up(
    loader = spin_epic("orbit", color = "#FFF"),
    text = "Loading data, this could take up to 15 seconds",
    timeout = 15000,
    color = "#FFF",
    background = "#112446"
  ),
  
  
  
  
  page_navbar(
    
    title = tags$b("The Proteome meets the Metabolome"),
    bg = "#2F4F4F",
    inverse = TRUE,
    theme = bslib::bs_theme(version = 5),
    
    
    
    
    
    
    nav_panel(title = "Welcome",
              p(""),
              p(""),
              tags$span(style = "color:black; font-size:13pt", "Welcome!"),
              p(""),
              tags$span(style = "color:black; font-size:13pt", "This webpage was created to provide you with the results from analyses of how proteins are related to metabolites as presented in the paper ",tags$i("When the Proteome Meets the Metabolome - Observational and Mendelian Randomization Analyses.") ),
              
              tags$span(style = "color:black; font-size:13pt", "The results consist of two parts."),
              
              tags$span(style = "color:black; font-size:13pt", "The first part used epidemiological observational data in which each of 245 proteins were related to each of 790 non-xenobiotic metabolites. A discovery/validation approach was used. The discovery part was performed in the EpiHealth study and the validation phase was performed in the POEM and PIVUS studies."),
              tags$span(style = "color:black; font-size:13pt", "In order to see the results, please click the ", tags$i("Observational analyses"),  " tab inside the ", tags$i("Tables"), " tab at the top of this page and then enter the name of a protein or a metabolite to see the results. Two degrees of adjustment were used, age and sex-adjustment and also additional adjustment for BMI and kidney function (eGFR)."),
              tags$span(style = "color:black; font-size:13pt", "You can also see heatmap plots for any of these association in the ", tags$i("Observational analyses"),  " tab inside the ", tags$i("Heatmaps"), " tab at the top of this page. Models adjusted by age, sex, BMI and kidney function (eGFR) are displayed."),
              
              tags$span(style = "color:black; font-size:13pt", "The second part used two-sample Mendelian randomization (MR) to evaluate how proteins are related to metabolites. Cis-instruments for 1,621 proteins were derived from UK Biobank data and were related to our own GWAS data for metabolites derived from the EpiHealth and SCAPIS studies. Only the protein->metabolite relationships were evaluated since it is hard to find non-pleotrophic instruments for the majority of the metabolites."),
              tags$span(style = "color:black; font-size:13pt", "In order to see the MR results, please click the ", tags$i("Mendelian Randomization"), " tab in the ", tags$i("Tables"), " tab at the top of this page and then enter the name of a protein or a metabolite to see the results."),
              tags$span(style = "color:black; font-size:13pt", "You can also see heatmap plots for any of these associations in the ", tags$i("Mendelian Randomization"), " tab inside the ", tags$i("Tables"), " tab at the top of this page."),
              
              tags$span(style = "color:black; font-size:13pt", "The result tables can be downloaded by pressing the ", tags$i("Download"), " button. The heatmap plots can be downloaded by pressing the ", tags$i("Download plot"), " button, and the underlying data can be downloaded by pressing the ", tags$i("Download data"), " button."),
              
              tags$span(style = "color:black; font-size:13pt", "We have also analyzed how 1,319 proteins were related to each of 790 non-xenobiotic metabolites in the POEM study. Since these relationships were not validated in an external cohort, these results are only available as a table to download in the ", tags$i("Annex"), " tab."),
              p(""),
              p(""),
              p(""),
              p(""),
              p(""),
              p(tags$b("In Collaboration with") ),
              fluidRow(
                column(3, tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/c/cb/Uppsala_universitet_logo.jpg", width = "190px", height = "190px")) ,
                column(3, tags$img(src = "https://upload.wikimedia.org/wikipedia/en/3/3a/Lunds_universitet.svg", width = "180px", height = "180px")) ,
                column(3, tags$img(src = "https://images.ctfassets.net/e8gvzq1fwq00/61AhHssAP6zsqjxPVX5CzD/d1b15d2717f2e35546f51a187ff0826f/HLF_Logotyp_120_RGB_822x222.svg", width = "200px", height = "200px") )
              )
    ),
    
    nav_panel(title = "Tables",
              tabsetPanel(
                
                tabPanel(title = "Observational analyses",
                         
                         p(""),
                         p(""),
                         p(""),
                         tags$span(style = "color:black; font-size:13pt", "Please select one metabolite and/or one protein, and click", tags$i("Check!."), "If you select the boolean operator OR, you will see all the associations for that metabolite as well as for that protein.") ,
                         tags$span(style = "color:black; font-size:13pt", "If you want to remove a metabolite or a protein, click on the respective box and press backspace.") ,
                         tags$span(style = "color:black; font-size:13pt", "Also, if you cannot open the dropdown menu below, please wait, it could take up to 45 seconds for them to show.") ,
                         p(""),
                         p(""),
                         p(""),
                         
                         
                         
                         
                         fluidPage(
                           
                           
                           useShinyFeedback(),
                           
                           tags$div(  selectizeInput("metabolite_obs", "Select or type a metabolite", choices = NULL, multiple = FALSE,
                                                     selected = NULL,
                                                     options = list('plugins' = list('remove_button'),
                                                                    placeholder = '' ) )      ,  style="display:inline-block"),
                           tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                           tags$div(  selectizeInput("boolean_obs", "AND / OR", choices = c("AND", "OR"), multiple = FALSE,
                                                     selected = "OR",
                                                     options = list('plugins' = list('remove_button'),
                                                                    placeholder = '' ) )          ,  style="display:inline-block; width: 100px;"),
                           tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                           
                           tags$div(  selectizeInput("protein_obs", "Select or type a protein", choices = NULL, multiple = FALSE,
                                                     selected = NULL,
                                                     options = list('plugins' = list('remove_button'),
                                                                    placeholder = '' ) )          ,  style="display:inline-block"),
                           tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                           
                           tags$div(  actionButton("button_obs", "Check!", style = 'margin-top:0px') ,  style="display:inline-block"),
                           
                           
                           
                           fluidRow(htmlOutput("result_text_obs")),
                           tags$head(tags$style("#result_text_obs{font-size: 17px;
                                       }"
                           )
                           ),
                           p(""),
                           p(""),
                           
                           fluidRow(
                             column(12,reactableOutput("selected_results")
                             )
                             
                           ),
                           fluidRow( column(6, align = "left", uiOutput("download.button.results") ) ),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           
                           fluidRow(htmlOutput("protein_text_obs")),
                           tags$head(tags$style("#protein_text_obs{font-size: 17px;
                                       }"
                           )
                           ),
                           p(""),
                           p(""),
                           
                           fluidRow(
                             column(12,reactableOutput("selected_protein_details")
                             )
                             
                           ),
                           fluidRow( column(6, align = "left",uiOutput("download.button.proteins") ) ),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           
                           fluidRow(htmlOutput("metabolite_text_obs")),
                           tags$head(tags$style("#metabolite_text_obs{font-size: 17px;
                                               }"
                           )
                           ),
                           p(""),
                           p(""),
                           
                           fluidRow(
                             column(12,reactableOutput("selected_metabolite_details")
                             )
                             
                           ),
                           fluidRow( column(6, align = "left", uiOutput("download.button.metabolites") ) ),
                           
                           
                           
                         ) # end fluidPage
                         
                ), # end tabsetPanel
                
                
                tabPanel(title = "Mendelian Randomization",
                         
                         p(""),
                         p(""),
                         p(""),
                         tags$span(style = "color:black; font-size:13pt", "Please select one metabolite and/or one protein, and click", tags$i("Check!."), "If you select the boolean operator OR, you will see all the associations for that metabolite as well as for that protein.") ,
                         tags$span(style = "color:black; font-size:13pt", "If you want to remove a metabolite or a protein, click on the respective box and press backspace.") ,
                         tags$span(style = "color:black; font-size:13pt", "Also, if you cannot open the dropdown menu below, please wait, it could take up to 45 seconds for them to show.") ,
                         p(""),
                         p(""),
                         p(""),
                         
                         
                         
                         
                         fluidPage(
                           
                           
                           useShinyFeedback(),
                           
                           tags$div(selectizeInput("metabolite_mr", "Select or type a metabolite", choices = NULL, multiple = FALSE,
                                                   selected = NULL,
                                                   options = list('plugins' = list('remove_button'),
                                                                  placeholder = '' )   ) ,  style="display:inline-block"),
                           tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                           tags$div(selectizeInput("boolean_mr", "AND / OR", choices = c("AND", "OR"), multiple = FALSE,
                                                   selected = "OR",
                                                   options = list('plugins' = list('remove_button'),
                                                                  placeholder = '' )) ,  style="display:inline-block; width: 100px;"),
                           tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                           
                           tags$div(selectizeInput("protein_mr", "Select or type a protein", choices = NULL, multiple = FALSE,
                                                   selected = NULL,
                                                   options = list('plugins' = list('remove_button'),
                                                                  placeholder = '' )  ) ,  style="display:inline-block"),
                           tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                           
                           tags$div(actionButton("button_mr", "Check!", style = 'margin-top:0px') ,  style="display:inline-block"),
                           

                           fluidRow(htmlOutput("result_text_mr")),
                           tags$head(tags$style("#result_text_mr{font-size: 17px;
                                       }"
                           )
                           ),
                           p(""),
                           p(""),
                           
                           fluidRow(
                             column(12,reactableOutput("selected_results_mr")
                             )
                             
                           ),
                           fluidRow( column(6, align = "left", uiOutput("download.button.results_mr") ) ),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           
                           fluidRow(htmlOutput("protein_text_mr")),
                           tags$head(tags$style("#protein_text_mr{font-size: 17px;
                                       }"
                           )
                           ),
                           p(""),
                           p(""),
                           
                           fluidRow(
                             column(12,reactableOutput("selected_protein_details_mr")
                             )
                             
                           ),
                           fluidRow( column(6, align = "left",uiOutput("download.button.proteins_mr") ) ),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           
                           fluidRow(htmlOutput("metabolite_text_mr")),
                           tags$head(tags$style("#metabolite_text_mr{font-size: 17px;
                                               }"
                           )
                           ),
                           p(""),
                           p(""),
                           
                           fluidRow(
                             column(12,reactableOutput("selected_metabolite_details_mr")
                             )
                             
                           ),
                           fluidRow( column(6, align = "left", uiOutput("download.button.metabolites_mr") ) ),
                           
                           
                           
                           
                           
                         ) # end fluidPage
                         
                         
                )
                
                
              ) # End tabPanel
              
    ), # end nav_panel Observational Analyses
    
    
    nav_panel(title = "Heatmaps",
              tabsetPanel(
                
                tabPanel(title = "Observational analyses",
                         
                         p(""),
                         p(""),
                         p(""),
                         tags$span(style = "color:black; font-size:13pt", "Please select one super- or sub-pathway, and one or several proteins and click", tags$i("Plot!."), "Nominal p-value <0.01 = two stars; <0.05 = one star; ≥0.05 = no stars. Model adjusted for age, sex, BMI and kidney function (eGFR)" ) ,
                         tags$span(style = "color:black; font-size:13pt", "If you want to remove a super- or sub-pathway, or a protein, click on the respective box and press backspace.") ,
                         tags$span(style = "color:black; font-size:13pt", "Also, if you cannot open the dropdown menu below, please wait, it could take up to 45 seconds for them to show.") ,
                         p(""),
                         p(""),
                         p(""),
                         
                         
                         fluidPage(
                           
                           
                           
                           useShinyFeedback(),
                           
                           tags$div(  selectizeInput("pathway_obs_plot", "Select or type a super- or sub-pathway", choices = NULL, multiple = FALSE,
                                                     selected = NULL,
                                                     options = list('plugins' = list('remove_button'),
                                                                    placeholder = '' ) )      ,  style="display:inline-block"),
                           tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                           tags$div(  selectizeInput("boolean_obs_plot", "AND", choices = c("AND"), multiple = FALSE,
                                                     selected = "AND",
                                                     options = list('plugins' = list('remove_button'),
                                                                    placeholder = '' ) )         ,  style="display:inline-block; width: 100px;"),
                           tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                           
                           tags$div(  selectizeInput("protein_obs_plot", "Select or type a protein", choices = NULL, multiple = TRUE,
                                                     selected = NULL,
                                                     options = list('plugins' = list('remove_button'),
                                                                    placeholder = '' ) )          ,  style="display:inline-block"),
                           tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                           
                           tags$div(  actionButton("button_obs_plot", "Plot!", style = 'margin-top:0px') ,  style="display:inline-block"),
                           
                           
                           
                           
                           tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                           tags$div(  uiOutput("download.button.obs.plot") ,  style="display:inline-block"),
                           tags$div(  uiOutput("download.button.obs.plot.data") ,  style="display:inline-block"),
                           
                           
                           br(),
                           br(),
                           
                           tags$div(  plotOutput("heatmap_obs") , style = "display:block;"   )
                           
                         ) # end fluidPage
                         
                ), # end tabsetPanel
                
                
                tabPanel(title = "Mendelian Randomization",
                         
                         p(""),
                         p(""),
                         p(""),
                         tags$span(style = "color:black; font-size:13pt", "Please select one super- or sub-pathway, and one or several proteins and click", tags$i("Plot!."), "Nominal p-value <0.01 = two stars; <0.05 = one star; ≥0.05 = no stars." ) ,
                         tags$span(style = "color:black; font-size:13pt", "If you want to remove a super- or sub-pathway, or a protein, click on the respective box and press backspace.") ,
                         tags$span(style = "color:black; font-size:13pt", "Also, if you cannot open the dropdown menu below, please wait, it could take up to 45 seconds for them to show.") ,
                         p(""),
                         p(""),
                         p(""),
                         
                         
                         
                         fluidPage(
                           
                           
                           
                           useShinyFeedback(),
                           
                           tags$div(  selectizeInput("pathway_mr_plot", "Select or type a super- or sub-pathway", choices = NULL, multiple = FALSE,
                                                     selected = NULL,
                                                     options = list('plugins' = list('remove_button'),
                                                                    placeholder = '' ) )      ,  style="display:inline-block"),
                           tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                           tags$div(  selectizeInput("boolean_mr_plot", "AND", choices = c("AND"), multiple = FALSE,
                                                     selected = "AND",
                                                     options = list('plugins' = list('remove_button'),
                                                                    placeholder = '' ))         ,  style="display:inline-block; width: 100px;"),
                           tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                           
                           tags$div(  selectizeInput("protein_mr_plot", "Select or type a protein", choices = NULL, multiple = TRUE,
                                                     selected = NULL,
                                                     options = list('plugins' = list('remove_button'),
                                                                    placeholder = '' ) )          ,  style="display:inline-block"),
                           tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                           
                           tags$div(  actionButton("button_mr_plot", "Plot!", style = 'margin-top:0px;') ,  style="display:inline-block"),
                           
                           
                           
                           
                           
                           tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                           tags$div(  uiOutput("download.button.mr.plot") ,  style="display:inline-block"),
                           tags$div(  uiOutput("download.button.mr.plot.data") ,  style="display:inline-block"),
                           
                           
                           
                           br(),
                           br(),
                           
                           
                           fluidRow(htmlOutput("line_mr")),
                           tags$head(tags$style("#line_mr{font-size: 17px;
                                                          color: 	#bb2124}"
                           )
                           ),
                           
                           
                           tags$div( plotOutput("heatmap_mr") , style = "display:block;" )
                           
                           
                           
                         ) # end fluidPage
                         
                         
                )
                
                
              ) # End tabPanel
              
    ), # end nav_panel
    
    nav_panel(title = "Downloads",
              p(""),
              p(""),
              tags$span(style = "color:black; font-size:13pt", "Here you can find all supplementary tables to our research article."),
              tags$span(style = "color:black; font-size:13pt", "For each table, you can either sort and download it completely, or you can search, filter, sort and download your customized table."),
              tags$span(style = "color:black; font-size:13pt", "Also, if you cannot see the tables below, please wait, it could take up to 45 seconds for them to show.") ,
              
              p(""),
              p(""),
              
              p( tags$span(style = "font-size:17px", tags$b("Supplementary Table 1."), "Description of the 242 measured proteins used in Observational analyses.") ),
              
              # Supplementary 1; table render and download botton
              fluidRow(
                column(12,
                       reactableOutput("table_supp1"),
                       # tableOutput("test"),
                       
                       shiny::downloadButton(
                         "downloadData_1", "Download",
                         onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('table_supp1').sortedData)"
                       )
                )
              ),
              
              p(""),
              p(""),
              p(""),
              p(""),
              
              p( tags$span(style = "font-size:17px", tags$b("Supplementary Table 2."), "Description of the genetic instruments of proteins' levels used for the Mendelian Randomization analyses. Log10 p-values equal to 100.000 mean infinite.") ),
              
              # Supplementary 2; table render and download botton
              fluidRow(
                column(12,
                       reactableOutput("table_supp2"),
                       # tableOutput("test"),
                       
                       shiny::downloadButton(
                         "downloadData_2", "Download",
                         onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('table_supp2').sortedData)"
                       )
                )
              ),
              
              p(""),
              p(""),
              p(""),
              p(""),
              
              p( tags$span(style = "font-size:17px", tags$b("Supplementary Table 3."), "Description of the 790 non-xenobiotic metabolites analysed in the study.") ),
              
              # Supplementary 3; table render and download botton
              fluidRow(
                column(12,
                       reactableOutput("table_supp3"),
                       # tableOutput("test")
                       shiny::downloadButton(
                         "downloadData_3", "Download",
                         onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('table_supp3').sortedData)"
                       )
                )
              ),
              
              p(""),
              p(""),
              p(""),
              p(""),
              
              p( tags$span(style = "font-size:17px", tags$b("Supplementary Table 4."), " Baseline characteristics of study participants from SCAPIS, EpiHealth and POEM studies. Means and (SD) are given, or proportions in %.") ),
              
              # Supplementary 4; table render and download botton
              fluidRow(
                column(12,
                       reactableOutput("table_supp4"),
                       shiny::downloadButton(
                         "downloadData_4", "Download",
                         onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('table_supp4').sortedData)"
                       )
                )
              )
              
              
    ), # end nav_panel Downloads
    
    
    nav_panel(title = "Annex",
              
              p(""),
              p(""),
              p(""),
              p(""),
              
              tags$span(style = "font-size:17px", tags$b("Annex 1."), " Associations between 1,319 proteins and 790 non-xenobiotic plasma metabolites in the POEM study (not validated in an external cohort)."),
              p(""),

              fluidRow(column(6, downloadButton("statFile", "Download" ) ) )
              
              
    ), # End panel Annex
    
    
    nav_spacer(),
    
    nav_panel(title = "Contact",
              p(""),
              p(""),
              tags$span(style = "color:black; font-size:13pt", "We would love to hear from you!"),
              p(""),
              tags$span(style = "color:black; font-size:13pt", "Please get in touch with Professor Lars Lind at", tags$span(style = "color: #2D89C8", "lars.lind@medsci.uu.se"  ), "and with Dr. Rui Zheng at", tags$span(style = "color: #2D89C8", "rui.zheng@uu.se"  ) ),
              p(""),
              p(""),
              p(""),
              p(tags$b("In Collaboration with") ),
              fluidRow(
                column(3, tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/c/cb/Uppsala_universitet_logo.jpg", width = "190px", height = "190px")) ,
                column(3, tags$img(src = "https://upload.wikimedia.org/wikipedia/en/3/3a/Lunds_universitet.svg", width = "180px", height = "180px")) ,
                column(3, tags$img(src = "https://images.ctfassets.net/e8gvzq1fwq00/61AhHssAP6zsqjxPVX5CzD/d1b15d2717f2e35546f51a187ff0826f/HLF_Logotyp_120_RGB_822x222.svg", width = "200px", height = "200px") )
              )
    ),
    
    nav_panel(title = "Report a bug",
              p(""),
              p(""),
              tags$span(style = "color:black; font-size:13pt", "Help us improve!"),
              p(""),
              tags$span(style = "color:black; font-size:13pt", "Please report any bug in our website at", tags$span(style = "color: #2D89C8", "mario.delgado.velandia@uu.se"  ) ),
              tags$span(style = "color:black; font-size:13pt", "Thanks in advance."),
              p(""),
              p(""),
              p(""),
              p(tags$b("In Collaboration with") ),
              fluidRow(
                column(3, tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/c/cb/Uppsala_universitet_logo.jpg", width = "190px", height = "190px")) ,
                column(3, tags$img(src = "https://upload.wikimedia.org/wikipedia/en/3/3a/Lunds_universitet.svg", width = "180px", height = "180px")) ,
                column(3, tags$img(src = "https://images.ctfassets.net/e8gvzq1fwq00/61AhHssAP6zsqjxPVX5CzD/d1b15d2717f2e35546f51a187ff0826f/HLF_Logotyp_120_RGB_822x222.svg", width = "200px", height = "200px") )
              ) ),
    
    nav_menu(
      title = "More",
      align = "right",
      nav_item(tags$a("About Us",
                      href = "https://www.uu.se/en/department/medical-sciences/research/research-groups/clinical-epidemiology",
                      target = "_blank" )),
      nav_item(tags$a("EpiHealth",
                      href = "https://www.epihealth.lu.se/en/about-us",
                      target = "_blank" )),
      nav_item(tags$a("SCAPIS",
                      href = "https://www.scapis.org/",
                      target = "_blank" )),
      # nav_item(tags$a("PIVUS",
      #                 href = "https://www.uu.se/en/department/medical-sciences/research/epidemiological-studies/pivus")),
      nav_item(tags$a("POEM",
                      href = "https://www.maelstrom-research.org/study/poem",
                      target = "_blank" ))
      
    )
    
    
    
  ) # end pageNavbar
) # end fluidPage





# Server side----
server <- function(input, output, session) {
  
  
  
  # Observational analyses page start----
  
  
  # Input alerts for selection boxes----
  
  
  
  observeEvent(input$button_obs,{
    
    if ( input$metabolite_obs == "" && input$protein_obs == "" ) {
      showFeedbackWarning(
        inputId = "metabolite_obs",
        text = "Please select a metabolite from the list"
      )
      showFeedbackWarning(
        inputId = "protein_obs",
        text = "Please select a protein from the list"
      )
      
    } else if( input$metabolite_obs == "" && input$boolean_obs == "AND" ){
      showFeedbackWarning(
        inputId = "metabolite_obs",
        text = "Please select a metabolite from the list"
      )
      hideFeedback("protein_obs")
      
    } else if( input$protein_obs == "" && input$boolean_obs == "AND" ){
      showFeedbackWarning(
        inputId = "protein_obs",
        text = "Please select a protein from the list"
      )
      hideFeedback("metabolite_obs")
      
    }  else {
      hideFeedback("metabolite_obs")
      hideFeedback("protein_obs")
    }
    
  }
  )
  
  
  
  
  
  # Selection boxes----
  updateSelectizeInput(session, 'metabolite_obs', choices = unique(obs_df$Metabolite[order(obs_df$Metabolite)]) , server = TRUE, selected = "" )
  updateSelectizeInput(session, 'protein_obs', choices = unique(c( obs_df$`Protein name` )[order(c( obs_df$`Protein name` )) ]), server = TRUE, selected = "" )
  
  
  
  # Table titles to display----
  line_1 <- eventReactive(input$button_obs,{
    
    req(isTruthy(input$metabolite_obs) || isTruthy(input$protein_obs))
    
    line1 <- paste("<br>", "<br>", "<br>", "<b>Table 1. ", "</b>", "Associations between measured protein levels and plasma metabolites. m1, model adjusted for age and sex; m2, adjusted as m1 and for BMI and kidney function (eGFR).")
    
  } )
  
  
  line_2 <- eventReactive(input$button_obs,{
    
    req(isTruthy(input$metabolite_obs) || isTruthy(input$protein_obs))
    
    line2 <- paste("<br>", "<br>", "<br>", "<b>Table 2. ", "</b>", "Description of the measured proteins used for observational analyses.")
    
  } )
  
  line_3 <- eventReactive(input$button_obs,{
    
    req(isTruthy(input$metabolite_obs) || isTruthy(input$protein_obs))
    
    line3 <- paste("<br>", "<br>", "<br>", "<b>Table 3. ", "</b>", "Description of the plasma metabolites used for observational analyses.")
    
  } )
  
  
  
  ## Rendering Table titles----
  output$result_text_obs  <- renderText( { line_1() } )
  
  output$protein_text_obs  <- renderText( { line_2() } )
  
  output$metabolite_text_obs  <- renderText( { line_3() } )
  
  
  
  # Results tables to display----
  selected_results_df <- eventReactive(input$button_obs,{
    
    req( isTruthy(input$metabolite_obs) || isTruthy(input$protein_obs) )
    
    
    if(input$boolean_obs == "AND"){
      
      df <- obs_df[ which( obs_df$`Protein name` %in% c( input$protein_obs ) &  obs_df$Metabolite %in% c(input$metabolite_obs) ), ]
      
    } else if(input$boolean_obs == "OR"){
      
      df <- obs_df[ which( obs_df$`Protein name` %in% c( input$protein_obs ) |  obs_df$Metabolite %in% c(input$metabolite_obs) ), ]
      
    }
    
    return(df)
    
  })
  
  selected_protein_details_df <- eventReactive(input$button_obs,{
    
    req( isTruthy(input$metabolite_obs) || isTruthy(input$protein_obs) )
    
    
    if(input$boolean_obs == "AND"){
      
      df1 <- obs_df[ which( obs_df$`Protein name` %in% c( input$protein_obs ) &  obs_df$Metabolite %in% c(input$metabolite_obs) ), ]
      
    } else if(input$boolean_obs == "OR"){
      
      df1 <- obs_df[ which( obs_df$`Protein name` %in% c( input$protein_obs ) |  obs_df$Metabolite %in% c(input$metabolite_obs) ), ]
      
    }
    
    
    df2 <- measured_prot_anno[ which( measured_prot_anno$`Protein name` %in% c(df1$`Protein name` ) ), ]
    
    return(df2)
    
  })
  
  selected_metabolite_details_df <- eventReactive(input$button_obs,{
    
    req( isTruthy(input$metabolite_obs) || isTruthy(input$protein_obs) )
    
    
    if(input$boolean_obs == "AND"){
      
      df3 <- obs_df[ which( obs_df$`Protein name` %in% c( input$protein_obs ) &  obs_df$Metabolite %in% c(input$metabolite_obs) ), ]
      
    } else if(input$boolean_obs == "OR"){
      
      df3 <- obs_df[ which( obs_df$`Protein name` %in% c( input$protein_obs ) |  obs_df$Metabolite %in% c(input$metabolite_obs) ), ]
      
    }
    
    
    df4 <- met_anno[ which( met_anno$Metabolite %in% c( df3$Metabolite ) ), ]
    
    return(df4)
    
  })
  
  
  
  ## Rendering Results tables----
  observeEvent(input$button_obs, {
    
    req( isTruthy(input$metabolite_obs) || isTruthy(input$protein_obs) )
    
    
    output$selected_results <- renderReactable({
      reactable(selected_results_df(),
                filterable = TRUE,
                searchable = TRUE,
                bordered = TRUE,
                highlight = TRUE,
                defaultColDef = colDef(
                  align = "center",
                  headerStyle = list(background = "#f7f7f8")
                ),
                columns = list(
                  `UniProt ID` = colDef(minWidth = 50),
                  Gene = colDef(minWidth = 50),
                  HMDB = colDef(minWidth = 80),
                  KEGG = colDef(minWidth = 60),
                  `Beta m1` = colDef(minWidth = 50,
                                     filterable = FALSE,
                                     cell = function(value) format(value, digits = 2, scientific = FALSE ) ),
                  `SE m1` = colDef(minWidth = 50,
                                   filterable = FALSE,
                                   cell = function(value) format(value, digits = 2, scientific = FALSE ) ),
                  `Nominal p-value m1` = colDef(minWidth = 50,
                                                filterable = FALSE,
                                                cell = function(value) format(value, digits = 2, scientific = TRUE ) ),
                  `Beta m2` = colDef(minWidth = 50,
                                     filterable = FALSE,
                                     cell = function(value) format(value, digits = 2, scientific = FALSE ) ),
                  `SE m2` = colDef(minWidth = 50,
                                   filterable = FALSE,
                                   cell = function(value) format(value, digits = 2, scientific = FALSE ) ),
                  `Nominal p-value m2` = colDef(minWidth = 50,
                                                filterable = FALSE,
                                                cell = function(value) format(value, digits = 2, scientific = TRUE) )
                ),
                defaultPageSize = 5,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(5, 10, 25, 50, 100),
                paginationType = "jump",
                showPagination = TRUE ) } )
    
  })
  
  
  observeEvent(input$button_obs, {
    
    req( isTruthy(input$metabolite_obs) || isTruthy(input$protein_obs) )
    
    
    output$selected_protein_details <- renderReactable({
      reactable(selected_protein_details_df(),
                filterable = TRUE,
                searchable = TRUE,
                bordered = TRUE,
                highlight = TRUE,
                defaultColDef = colDef(
                  align = "center",
                  headerStyle = list(background = "#f7f7f8")
                ),
                defaultPageSize = 5,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(5, 10, 25, 50, 100),
                paginationType = "jump",
                showPagination = TRUE ) } )
    
  } )
  
  
  observeEvent(input$button_obs, {
    
    req( isTruthy(input$metabolite_obs) || isTruthy(input$protein_obs) )
    
    
    output$selected_metabolite_details <- renderReactable({
      reactable(selected_metabolite_details_df(),
                filterable = TRUE,
                searchable = TRUE,
                bordered = TRUE,
                highlight = TRUE,
                defaultColDef = colDef(
                  align = "center",
                  headerStyle = list(background = "#f7f7f8")
                ),
                defaultPageSize = 5,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(5, 10, 25, 50, 100),
                paginationType = "jump",
                showPagination = TRUE ) } )
    
  } )
  
  
  
  # Download buttons-----
  observeEvent(input$button_obs,{
    
    req(isTruthy(input$metabolite_obs) || isTruthy(input$protein_obs))
    
    output$download.button.results <- renderUI({
      
      req( selected_results_df() )
      
      shiny::downloadButton(
        "download_results", "Download",
        onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('selected_results').sortedData)")
    } )
    
    
    
  } )
  
  
  observeEvent(input$button_obs,{
    
    req(isTruthy(input$metabolite_obs) || isTruthy(input$protein_obs))
    
    output$download.button.proteins <- renderUI({
      
      req( selected_protein_details_df() )
      
      shiny::downloadButton(
        "download_proteins", "Download",
        onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('selected_protein_details').sortedData)")
    } )
    
  } )
  
  observeEvent(input$button_obs,{
    
    req(isTruthy(input$metabolite_obs) || isTruthy(input$protein_obs))
    
    output$download.button.metabolites  <- renderUI({
      
      req( selected_metabolite_details_df() )
      
      shiny::downloadButton(
        "download_metabolites", "Download",
        onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('selected_metabolite_details').sortedData)")
    } )
    
  } )
  
  
  ## Rendering Download buttons----
  observeEvent(input$button_obs, {
    
    req( isTruthy(input$metabolite_obs) || isTruthy(input$protein_obs) )
    
    
    output$download_results <- downloadHandler(
      filename = function() {
        paste("Results_Obs_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- input$table_state
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    
  })
  
  
  
  observeEvent(input$button_obs, {
    
    output$download_proteins <- downloadHandler(
      filename = function() {
        paste("Proteins_Obs_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- input$table_state
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    
  })
  
  
  
  observeEvent(input$button_obs, {
    
    output$download_metabolites <- downloadHandler(
      filename = function() {
        paste("Metabolites_Obs_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- input$table_state
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    
  })
  
  # Observational Analyses page end
  
  
  
  
  
  # Mendelian Randomization page start----
  
  
  
  observeEvent(input$button_mr,{
    
    if ( input$metabolite_mr == "" && input$protein_mr == "" ) {
      showFeedbackWarning(
        inputId = "metabolite_mr",
        text = "Please select a metabolite from the list"
      )
      showFeedbackWarning(
        inputId = "protein_mr",
        text = "Please select a protein from the list"
      )
      
    } else if( input$metabolite_mr == "" && input$boolean_mr == "AND" ){
      showFeedbackWarning(
        inputId = "metabolite_mr",
        text = "Please select a metabolite from the list"
      )
      hideFeedback("protein_mr")
      
    } else if( input$protein_mr == "" && input$boolean_mr == "AND" ){
      showFeedbackWarning(
        inputId = "protein_mr",
        text = "Please select a protein from the list"
      )
      hideFeedback("metabolite_mr")
      
    } else if ( input$metabolite_mr == "" && input$protein_mr == "" ) {
      showFeedbackWarning(
        inputId = "metabolite_mr",
        text = "Please select a metabolite from the list"
      )
      showFeedbackWarning(
        inputId = "protein_mr",
        text = "Please select a protein from the list"
      )
      
    } else {
      hideFeedback("metabolite_mr")
      hideFeedback("protein_mr")
    }
    
  }
  )
  
  
  
  
  # Selection boxes----
  updateSelectizeInput(session, 'metabolite_mr', choices = unique(mr_df$Metabolite[order(mr_df$Metabolite)]) , server = TRUE, selected = "" )
  updateSelectizeInput(session, 'protein_mr', choices = unique(c(mr_df$`Protein abbreviation`, mr_df$`Protein name`)[order(c(mr_df$`Protein abbreviation`, mr_df$`Protein name`)) ]), server = TRUE, selected = "" )
  
  
  
  # Table titles to display----
  line_1_mr <- eventReactive(input$button_mr,{
    
    req(isTruthy(input$metabolite_mr) || isTruthy(input$protein_mr))
    
    line1 <- paste("<br>", "<br>", "<br>", "<b>Table 1. ", "</b>", "Associations between genetically predicted protein levels and plasma metabolites.")
    
  } )
  
  
  line_2_mr <- eventReactive(input$button_mr,{
    
    req(isTruthy(input$metabolite_mr) || isTruthy(input$protein_mr))
    
    line2 <- paste("<br>", "<br>", "<br>", "<b>Table 2. ", "</b>", "Description of the genetic instruments of proteins' levels used for the Mendelian Randomization analyses.")
    
  } )
  
  line_3_mr <- eventReactive(input$button_mr,{
    
    req(isTruthy(input$metabolite_mr) || isTruthy(input$protein_mr))
    
    line3 <- paste("<br>", "<br>", "<br>", "<b>Table 3. ", "</b>", "Description of the plasma metabolites used in the Mendelian Randomization analyses.")
    
  } )
  
  
  
  ## Rendering Table titles----
  output$result_text_mr  <- renderText( { line_1_mr() } )
  
  output$protein_text_mr  <- renderText( { line_2_mr() } )
  
  output$metabolite_text_mr  <- renderText( { line_3_mr() } )
  
  
  
  # Results tables to display----
  selected_results_df_mr <- eventReactive(input$button_mr,{
    
    req( isTruthy(input$metabolite_mr) || isTruthy(input$protein_mr) )
    
    
    if(input$boolean_mr == "AND"){
      
      df5 <- mr_df[ which( ( mr_df$`Protein abbreviation` %in% c( input$protein_mr ) | mr_df$`Protein name` %in% c( input$protein_mr ) ) &  mr_df$Metabolite %in% c(input$metabolite_mr) ), ]
      
    } else if(input$boolean_mr == "OR"){
      
      df5 <- mr_df[ which( mr_df$`Protein abbreviation` %in% c( input$protein_mr ) | mr_df$`Protein name` %in% c( input$protein_mr ) |  mr_df$Metabolite %in% c(input$metabolite_mr) ), ]
      
    }
    
    return(df5)
    
  })
  
  selected_protein_details_df_mr <- eventReactive(input$button_mr,{
    
    req( isTruthy(input$metabolite_mr) || isTruthy(input$protein_mr) )
    
    if(input$boolean_mr == "AND"){
      
      df6 <- mr_df[ which( ( mr_df$`Protein abbreviation` %in% c( input$protein_mr ) | mr_df$`Protein name` %in% c( input$protein_mr ) ) &  mr_df$Metabolite %in% c(input$metabolite_mr) ), ]
      
    } else if(input$boolean_mr == "OR"){
      
      df6 <- mr_df[ which( mr_df$`Protein abbreviation` %in% c( input$protein_mr ) | mr_df$`Protein name` %in% c( input$protein_mr ) |  mr_df$Metabolite %in% c(input$metabolite_mr) ), ]
      
    }
    
    df7 <- prot_anno[ which( prot_anno$`Protein abbreviation` %in% c( df6$`Protein abbreviation` ) |  prot_anno$`Protein name` %in% c(df6$`Protein name` ) ), ]
    
    return(df7)
    
  })
  
  selected_metabolite_details_df_mr <- eventReactive(input$button_mr,{
    
    req( isTruthy(input$metabolite_mr) || isTruthy(input$protein_mr) )
    
    
    if(input$boolean_mr == "AND"){
      
      df8 <- mr_df[ which( ( mr_df$`Protein abbreviation` %in% c( input$protein_mr ) | mr_df$`Protein name` %in% c( input$protein_mr ) ) &  mr_df$Metabolite %in% c(input$metabolite_mr) ), ]
      
    } else if(input$boolean_mr == "OR"){
      
      df8 <- mr_df[ which( mr_df$`Protein abbreviation` %in% c( input$protein_mr ) | mr_df$`Protein name` %in% c( input$protein_mr ) |  mr_df$Metabolite %in% c(input$metabolite_mr) ), ]
      
    }
    
    df9 <- met_anno[ which( met_anno$Metabolite %in% c( df8$Metabolite ) ), ]
    
    return(df9)
    
  })
  
  
  
  ## Rendering Results tables----
  observeEvent(input$button_mr, {
    
    req( isTruthy(input$metabolite_mr) || isTruthy(input$protein_mr) )
    
    
    output$selected_results_mr <- renderReactable({
      reactable(selected_results_df_mr(),
                filterable = TRUE,
                searchable = TRUE,
                bordered = TRUE,
                highlight = TRUE,
                defaultColDef = colDef(
                  align = "center",
                  headerStyle = list(background = "#f7f7f8")
                ),
                columns = list(
                  Beta = colDef( minWidth = 50,
                                 cell = function(value) format(value, digits = 2, scientific = FALSE ),
                                 filterable = FALSE,
                                 searchable = TRUE),
                  SE = colDef( minWidth = 50,
                               cell = function(value) format(value, digits = 2, scientific = FALSE ),
                               filterable = FALSE,
                               searchable = TRUE),
                  `Nominal p-value` = colDef(minWidth = 50,
                                             cell = function(value) format(value, digits = 2, scientific = TRUE ),
                                             filterable = FALSE,
                                             searchable = TRUE)
                ),
                defaultPageSize = 5,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(5, 10, 25, 50, 100),
                paginationType = "jump",
                showPagination = TRUE ) } )
    
  } )
  
  
  
  observeEvent(input$button_mr, {
    
    req( isTruthy(input$metabolite_mr) || isTruthy(input$protein_mr) )
    
    
    output$selected_protein_details_mr <- renderReactable({
      reactable(selected_protein_details_df_mr(),
                filterable = TRUE,
                searchable = TRUE,
                bordered = TRUE,
                highlight = TRUE,
                defaultColDef = colDef(
                  align = "center",
                  headerStyle = list(background = "#f7f7f8")
                ),
                columns = list(
                  `Effect allele frequency` = colDef(cell = function(value) format(value, digits = 2, scientific = FALSE ) ),
                  `Beta` = colDef(minWidth = 50,
                                  cell = function(value) format(value, digits = 2, scientific = FALSE ),
                                  filterable = FALSE,
                                  searchable = TRUE),
                  `SE` = colDef(minWidth = 50,
                                cell = function(value) format(value, digits = 2, scientific = FALSE ),
                                filterable = FALSE,
                                searchable = TRUE),
                  `Log10 p-value` = colDef(filterable = FALSE,
                                           searchable = TRUE),
                  `F statistic` = colDef(cell = function(value) format(value, digits = 5, scientific = FALSE ),
                                         filterable = FALSE,
                                         searchable = TRUE)
                ),
                defaultPageSize = 5,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(5, 10, 25, 50, 100),
                paginationType = "jump",
                showPagination = TRUE ) } )
    
  } )
  
  
  observeEvent(input$button_mr, {
    
    req( isTruthy(input$metabolite_mr) || isTruthy(input$protein_mr) )
    
    
    output$selected_metabolite_details_mr <- renderReactable({
      reactable(selected_metabolite_details_df_mr(),
                filterable = TRUE,
                searchable = TRUE,
                bordered = TRUE,
                highlight = TRUE,
                defaultColDef = colDef(
                  align = "center",
                  headerStyle = list(background = "#f7f7f8")
                ),                ,
                defaultPageSize = 5,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(5, 10, 25, 50, 100),
                paginationType = "jump",
                showPagination = TRUE ) } )
    
  } )
  
  
  # Download buttons-----
  observeEvent(input$button_mr,{
    
    req(isTruthy(input$metabolite_mr) || isTruthy(input$protein_mr) )
    
    output$download.button.results_mr <- renderUI({
      
      req( selected_results_df_mr() )
      
      
      shiny::downloadButton(
        "download_results_mr", "Download",
        onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('selected_results_mr').sortedData)")
    })
    
  } )
  
  
  observeEvent(input$button_mr,{
    
    req(isTruthy(input$metabolite_mr) || isTruthy(input$protein_mr))
    
    output$download.button.proteins_mr <- renderUI({
      
      req( selected_protein_details_df_mr() )
      
      shiny::downloadButton(
        "download_proteins_mr", "Download",
        onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('selected_protein_details_mr').sortedData)")
    })
    
  } )
  
  
  observeEvent(input$button_mr,{
    
    req(isTruthy(input$metabolite_mr) || isTruthy(input$protein_mr))
    
    output$download.button.metabolites_mr  <- renderUI({
      
      req( selected_metabolite_details_df_mr() )
      
      shiny::downloadButton(
        "download_metabolites_mr", "Download",
        onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('selected_metabolite_details_mr').sortedData)")
    })
    
  } )
  
  
  ## Rendering Download buttons----
  observeEvent(input$button_mr, {
    
    req( isTruthy(input$metabolite_mr) || isTruthy(input$protein_mr) )
    
    
    output$download_results_mr <- downloadHandler(
      filename = function() {
        paste("Results_MR_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- input$table_state
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    
  })
  
  
  
  observeEvent(input$button_mr, {
    
    req( isTruthy(input$metabolite_mr) || isTruthy(input$protein_mr) )
    
    
    output$download_proteins_mr <- downloadHandler(
      filename = function() {
        paste("Proteins_MR_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- input$table_state
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    
  })
  
  
  
  observeEvent(input$button_mr, {
    
    req( isTruthy(input$metabolite_mr) || isTruthy(input$protein_mr) )
    
    
    output$download_metabolites_mr <- downloadHandler(
      filename = function() {
        paste("Metabolites_MR_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- input$table_state
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    
  })
  
  
  # Mendelian Randomization page end
  
  
  
  
  
  
  
  # Heatmap page starts
  
  
  # Selection boxes Obs plot----
  
  observeEvent(input$button_obs_plot,{
    
    if ( input$pathway_obs_plot == "" && (is.null(input$protein_obs_plot )) ) {
      showFeedbackWarning(
        inputId = "pathway_obs_plot",
        text = "Please select a pathway from the list"
      )
      showFeedbackWarning(
        inputId = "protein_obs_plot",
        text = "Please select a protein from the list"
      )
      
    } else if( input$pathway_obs_plot == "" && input$boolean_obs_plot == "AND" ){
      showFeedbackWarning(
        inputId = "pathway_obs_plot",
        text = "Please select a pathway from the list"
      )
      hideFeedback("protein_obs_plot")
      
    } else if( (is.null(input$protein_obs_plot )) && input$boolean_obs_plot == "AND" ){
      showFeedbackWarning(
        inputId = "protein_obs_plot",
        text = "Please select a protein from the list"
      )
      hideFeedback("pathway_obs_plot")
      
    } else {
      hideFeedback("pathway_obs_plot")
      hideFeedback("protein_obs_plot")
    }
    
  }
  )
  
  
  
  updateSelectizeInput(session, 'pathway_obs_plot', choices = c( unique(obs_df$`Super pathway`[order(obs_df$`Super pathway`)]), unique(obs_df$`Sub pathway`[order(obs_df$`Sub pathway`)]) ), server = TRUE, selected = "" )
  updateSelectizeInput(session, 'protein_obs_plot', choices = unique(c(obs_df$`Protein abbreviation`, obs_df$`Protein name`)[order(c(obs_df$`Protein abbreviation`, obs_df$`Protein name`)) ]), server = TRUE, selected = "" )
  
  
  
  
  observeEvent( input$button_obs_plot, {# To take plot and its dimensions outside of the reactive, to save it
    
    req( isTruthy(input$pathway_obs_plot) || isTruthy(input$protein_obs_plot)  )
    
    HT_obs_plot <- NULL
    makeReactiveBinding("HT_obs_plot")
    
    final_width <- NULL
    makeReactiveBinding("final_width")
    
    final_height <- NULL
    makeReactiveBinding("final_height")
    
    df_to_download <- NULL
    makeReactiveBinding("df_to_download")
    
  }
  
  )
  
  
  
  
  
  
  ht_obs <- observeEvent(input$button_obs_plot,{
    
    req( isTruthy(input$pathway_obs_plot) || isTruthy(input$protein_obs_plot)  )

    
    {
      
      
      
      if(input$boolean_obs_plot == "AND"){
        
        
        req( isTruthy(input$pathway_obs_plot) && isTruthy(input$protein_obs_plot)  )
        
        
        df_filtered <- obs_df[ which( (obs_df$`Super pathway` %in% c( input$pathway_obs_plot ) | obs_df$`Sub pathway` %in% c(input$pathway_obs_plot ) ) & ( obs_df$`Protein abbreviation` %in% c(input$protein_obs_plot ) | obs_df$`Protein name` %in% c(input$protein_obs_plot ) ) ), c("Protein name", "Protein abbreviation", "Metabolite", "Beta m2", "Nominal p-value m2") ]
        
        showNotification("Processing your request.", type ="message",  duration = 1.5 )
        
        
      } 
      
      
    }
    
    df_to_download <<- df_filtered[order(df_filtered$`Protein abbreviation`),]
    
    
    
    
    df_filtered$Metabolite <- first_letter_uppercase(df_filtered$Metabolite)
    df_filtered$Abbrev <- paste(df_filtered$`Protein abbreviation`, "-", df_filtered$`Protein name` )
    
    
    
    df_betas <- df_filtered %>%
      select(Abbrev, Metabolite, `Beta m2`) %>%
      pivot_wider(names_from = Metabolite, values_from = `Beta m2` )
    
    
    
    
    betas <- as.matrix(df_betas[!colnames(df_betas) %in% c("Abbrev") ])
    
    row.names(betas) <- df_betas$Abbrev
    
    
    
    df_pvalues <- df_filtered %>%
      select(Abbrev, Metabolite, `Nominal p-value m2`) %>%
      pivot_wider(names_from = Metabolite, values_from = `Nominal p-value m2` )
    
    
    
    
    pvalues <- as.matrix(df_pvalues[!colnames(df_pvalues) %in% c("Abbrev")])
    
    row.names(pvalues) <- df_pvalues$Abbrev
    
    pvalues[is.na(pvalues)] <- 1.5 # To draw absent lines
    
    
    
    # Starts function but no lines for absent metabolites
    stars_pvalues <- function(j, i, x, y, w, h, fill) {
      
      # Measure star to render in the center
      gb = textGrob("*")
      gb_w = convertWidth(grobWidth(gb), "mm")
      gb_h = convertHeight(grobHeight(gb), "mm")
      
      
      # Vector of cell positions for each group of stars
      v = pindex(pvalues, i, j)
      double_star <-  v < 0.01
      single_star <-  v >= 0.01 & v < 0.05
      no_star <-  v >= 0.05 & v <= 1
      
      
      
      grid.text("**", x[ double_star ] , y[ double_star ] - gb_h*0.35  , gp = gpar(fontsize = 16))
      grid.text("*", x[ single_star ], y[ single_star ] - gb_h*0.35   , gp = gpar(fontsize = 16))
      grid.text(" ", x[ no_star ], y[ no_star ] - gb_h*0.35   , gp = gpar(fontsize = 16))
      
    }
    
    
    
    
    # Colors for the heatmap body
    col_fun = colorRamp2(c(-1, 0, 1), c("blue", "white", "red"))
    
    
    
    
    
    
    
    stars_pvalues_cell <- function(j, i, x, y, w, h, fill) {
      
      gb = textGrob("*")
      gb_w = convertWidth(grobWidth(gb), "mm")
      gb_h = convertHeight(grobHeight(gb), "mm")
      
      
      
      if(pvalues[i, j] < 0.01 ) {
        grid.text("**", x, y - gb_h*0.35, gp = gpar(fontsize = 16))
      } else if( (pvalues[i, j] >= 0.01 & pvalues[i, j] < 0.05 )) {
        grid.text("*", x, y - gb_h*0.35, gp = gpar(fontsize = 16))
      } else if( (pvalues[i, j] >= 0.05 & (pvalues[i, j] <= 1 )) ) {
        grid.text(" ", x, y - gb_h*0.35, gp = gpar(fontsize = 16))
      }
      
    }
    
    
    
    
    
    if( nrow( df_filtered ) > 400 ){
      
      ht <- Heatmap(  betas, name = "Beta", col = col_fun,
                      cluster_columns = FALSE,
                      show_row_dend = FALSE,
                      show_column_dend= FALSE,
                      row_order = rownames(betas)[order(rownames(betas))],
                      show_row_names = TRUE,
                      row_names_gp = gpar(fontsize = 12),
                      row_names_side = "left",
                      column_labels = colnames(betas),
                      column_order = colnames(betas)[order(colnames(betas))],
                      column_names_max_height = convertHeight( grobWidth(textGrob(colnames(betas), gpar(fontsize = 11, fontface = 1))) , "mm"),
                      column_names_side = "top",
                      column_title_side = c("top"),
                      column_names_rot = 45,
                      column_names_gp = gpar(fontsize = 11),
                      width = unit(dim(betas)[2]*0.7, "cm"), height = unit(dim(betas)[1]*0.6, "cm"),
                      layer_fun = stars_pvalues,
                      heatmap_legend_param = list(
                        title = "Beta",
                        title_gp = gpar(fontsize = 12, fontface = "bold"),
                        title_position = "leftcenter",
                        col = col_fun,
                        at = (c(-1,-0.5,0,0.5, 1)),
                        labels = c("-1", "-0.5", "0", "0.5", "1"),
                        legend_width = unit(16, "cm"),
                        labels_gp = gpar(fontsize = 12), labels_rot = 0,
                        title_gp = gpar(fontsize = 12.5, fontface = "bold"),
                        legend_direction = c("horizontal") ),
                      border = TRUE
      )
      
      ht = ComplexHeatmap::draw(ht,
                                annotation_legend_side = "bottom",
                                legend_grouping = "original",
                                heatmap_legend_side = c("top")
      )
      
      
      print("---> Running layer_fun")
      
    } else {
      
      
      ht <- Heatmap(  betas, name = "Beta", col = col_fun,
                      cluster_columns = FALSE,
                      show_row_dend = FALSE,
                      show_column_dend= FALSE,
                      row_order = rownames(betas)[order(rownames(betas))],
                      show_row_names = TRUE,
                      row_names_gp = gpar(fontsize = 12),
                      row_names_side = "left",
                      column_labels = colnames(betas),
                      column_order = colnames(betas)[order(colnames(betas))],
                      column_names_max_height = convertHeight( grobWidth(textGrob(colnames(betas), gpar(fontsize = 11, fontface = 1))) , "mm"),
                      column_names_side = "top",
                      column_title_side = c("top"),
                      column_names_rot = 45,
                      column_names_gp = gpar(fontsize = 11),
                      width = unit(dim(betas)[2]*0.7, "cm"), height = unit(dim(betas)[1]*0.6, "cm"),
                      cell_fun = stars_pvalues_cell,
                      heatmap_legend_param = list(
                        title = "Beta",
                        title_gp = gpar(fontsize = 12, fontface = "bold"),
                        title_position = "leftcenter",
                        col = col_fun,
                        at = (c(-1,-0.5,0,0.5, 1)),
                        labels = c("-1", "-0.5", "0", "0.5", "1"),
                        legend_width = unit(16, "cm"),
                        labels_gp = gpar(fontsize = 12), labels_rot = 0,
                        title_gp = gpar(fontsize = 12.5, fontface = "bold"),
                        legend_direction = c("horizontal") ),
                      border = TRUE
      )
      
      ht = ComplexHeatmap::draw(ht,
                                annotation_legend_side = "bottom",
                                legend_grouping = "original",
                                heatmap_legend_side = c("top")
      )
      
      
      print("---> Running cell_fun")
      
      
    }
    
    
    
    
    HT_obs_plot <<- ht
    
    
    final_width <<- (( (convertX( grobWidth(textGrob(rownames(betas), gpar(fontsize = 11, fontface = 1))) , "inch", valueOnly = TRUE)/4) + convertX(  ComplexHeatmap:::width( draw( ht)) , "inch", valueOnly = TRUE )  ) )
    
    final_height <<- ( convertX(  ComplexHeatmap:::height(draw(ht)) , "inch", valueOnly = TRUE ) )
    
    
    
    
    output$heatmap_obs <- shiny::renderPlot({
      
      draw(ht)
      
    },
    
    width = ( final_width * 72 ) ,
    
    height = ( final_height * 72 )
    
    )
    
    
    
    
  }
  
  )
  
  
  
  
  
  
  observeEvent(input$button_obs_plot,{
    
    req( isTruthy(input$pathway_obs_plot) && isTruthy(input$protein_obs_plot)  && input$boolean_obs_plot == "AND"  )

    
    output$download.button.obs.plot <- renderUI({
      
      
      shiny::downloadButton(
        "download_obs_plot", "Download Plot" )
      
      
    } )
    
    
    
  } )
  
  
  
  
  
  
  output$download_obs_plot <- downloadHandler(
    filename = function() {
      
      if( !is.null( input$pathway_obs_plot ) & is.null( input$pathway_obs_plot ) ){
        
        paste0("Heatmap_" , input$pathway_obs_plot, "_obs.svg")
        
      }else if( is.null( input$pathway_obs_plot ) & !is.null( input$pathway_obs_plot ) ){
        
        paste0("Heatmap_" , input$protein_obs_plot, "_obs.svg")
        
      } else if( !is.null( input$pathway_obs_plot ) & !is.null( input$pathway_obs_plot ) ){
        
        paste0("Heatmap_" , input$pathway_obs_plot, "_",input$protein_obs_plot, "_obs.svg")
        
        
      }
      
    },
    
    content = function(file) {
      
      svg(file, width = final_width, height = final_height  )
      
      draw(HT_obs_plot)
      
      dev.off()
      
    }
    
  )
  
  
  
  
  
  
  observeEvent(input$button_obs_plot,{
    
    req( isTruthy(input$pathway_obs_plot) && isTruthy(input$protein_obs_plot)  && input$boolean_obs_plot == "AND"  )
    

    output$download.button.obs.plot.data <- renderUI({
      
      
      shiny::downloadButton(
        "download_obs_plot_data", "Download Data" )
      
      
    } )
    
    
    
  } )
  
  
  
  
  
  
  output$download_obs_plot_data <- downloadHandler(
    filename = function() {
      
      if( !is.null( input$pathway_obs_plot ) & is.null( input$pathway_obs_plot ) ){
        
        paste0("Data_" , input$pathway_obs_plot, "_obs.csv")
        
      }else if( is.null( input$pathway_obs_plot ) & !is.null( input$pathway_obs_plot ) ){
        
        paste0("Data_" , input$protein_obs_plot, "_obs.csv")
        
      } else if( !is.null( input$pathway_obs_plot ) & !is.null( input$pathway_obs_plot ) ){
        
        paste0("Data_" , input$pathway_obs_plot, "_",input$protein_obs_plot, "_obs.csv")
        
        
      }
      
    },
    
    content = function(file) {
      
      write.csv(df_to_download, file, row.names = FALSE)
      
    }
    
  )
  
  
  
  
  
  
  
  
  # MR heatmaps
  
  
  # Selection boxes MR plot----
  
  
  
  observeEvent(input$button_mr_plot,{
    
    if ( input$pathway_mr_plot == "" && (is.null(input$protein_mr_plot ) ) ) {
      showFeedbackWarning(
        inputId = "pathway_mr_plot",
        text = "Please select a pathway from the list"
      )
      showFeedbackWarning(
        inputId = "protein_mr_plot",
        text = "Please select a protein from the list"
      )
      
    } else if( input$pathway_mr_plot == "" && input$boolean_mr_plot == "AND" ){
      showFeedbackWarning(
        inputId = "pathway_mr_plot",
        text = "Please select a pathway from the list"
      )
      hideFeedback("protein_mr_plot")
      
    } else if( (is.null(input$protein_mr_plot ) ) && input$boolean_mr_plot == "AND" ){
      showFeedbackWarning(
        inputId = "protein_mr_plot",
        text = "Please select a protein from the list"
      )
      hideFeedback("pathway_mr_plot")
      
    } else {
      hideFeedback("pathway_mr_plot")
      hideFeedback("protein_mr_plot")
    }
    
  }
  )
  
  
  
  
  updateSelectizeInput(session, 'pathway_mr_plot', choices = c( unique(mr_df$`Super pathway`[order(mr_df$`Super pathway`)]), unique(mr_df$`Sub pathway`[order(mr_df$`Sub pathway`)]) ), server = TRUE, selected = "" )
  updateSelectizeInput(session, 'protein_mr_plot', choices = unique(c(mr_df$`Protein abbreviation`, mr_df$`Protein name`)[order(c(mr_df$`Protein abbreviation`, mr_df$`Protein name`)) ]), server = TRUE, selected = "" )
  
  
  
  
  observeEvent(input$button_mr_plot, {
    
    
    req( isTruthy(input$pathway_mr_plot) || isTruthy(input$protein_mr_plot)  )
    
    
    
    # To take plot and its dimenstions outside of the reactive, to save it
    HT_mr_plot <- NULL
    makeReactiveBinding("HT_mr_plot")
    
    final_width_mr <- NULL
    makeReactiveBinding("final_width")
    
    final_height_mr <- NULL
    makeReactiveBinding("final_height")
    
    df_to_download <- NULL
    makeReactiveBinding("df_to_download")
    
  }
  
  )
  
  
  
  
  
  
  ht_mr <- observeEvent(input$button_mr_plot,{
    
    req( isTruthy( input$pathway_mr_plot != "" & !is.null(input$protein_mr_plot) & input$boolean_mr_plot == "AND" ) ||
           isTruthy( (input$pathway_mr_plot != "" & is.null(input$protein_mr_plot)) & input$boolean_mr_plot == "ONLY" )  ||
           isTruthy( (input$pathway_mr_plot == "" & !is.null(input$protein_mr_plot)) & input$boolean_mr_plot == "ONLY" ) )
    
    
    
    
    
    {
      
      
      if(input$boolean_mr_plot == "AND"){
        
        
        req( isTruthy(input$pathway_mr_plot) && isTruthy(input$protein_mr_plot)  )
        
        
        df_filtered <- mr_df[ which( (mr_df$`Super pathway` %in% c( input$pathway_mr_plot ) | mr_df$`Sub pathway` %in% c(input$pathway_mr_plot ) ) & ( mr_df$`Protein abbreviation` %in% c(input$protein_mr_plot ) | mr_df$`Protein name` %in% c(input$protein_mr_plot ) ) ), c("Protein name", "Protein abbreviation", "Metabolite", "Beta", "Nominal p-value") ]
        
        
        showNotification("Processing your request.", type ="message",  duration = 1.5 )
        
        
      } 
      
      
    }
    
    
    
    
    
    df_to_download <<- df_filtered[order(df_filtered$`Protein abbreviation`),]
    
    
    
    
    df_filtered$Metabolite <- first_letter_uppercase(df_filtered$Metabolite)
    df_filtered$Abbrev <- paste(df_filtered$`Protein abbreviation`, "-", df_filtered$`Protein name` )
    
    
    
    df_betas <- df_filtered %>%
      select(Abbrev, Metabolite, `Beta`) %>%
      pivot_wider(names_from = Metabolite, values_from = `Beta` )
    
    
    
    
    betas <- as.matrix(df_betas[!colnames(df_betas) %in% c("Abbrev") ])
    
    row.names(betas) <- df_betas$Abbrev
    
    
    
    df_pvalues <- df_filtered %>%
      select(Abbrev, Metabolite, `Nominal p-value`) %>%
      pivot_wider(names_from = Metabolite, values_from = `Nominal p-value` )
    
    
    
    
    pvalues <- as.matrix(df_pvalues[!colnames(df_pvalues) %in% c("Abbrev")])
    
    row.names(pvalues) <- df_pvalues$Abbrev
    
    pvalues[is.na(pvalues)] <- 1.5 # To draw absent lines
    
    
    
    
    
    
    stars_pvalues <- function(j, i, x, y, w, h, fill) {
      
      # Measure star to render in the center
      gb = textGrob("*")
      gb_w = convertWidth(grobWidth(gb), "mm")
      gb_h = convertHeight(grobHeight(gb), "mm")
      
      
      # Vector of cell positions for each group of stars
      v = pindex(pvalues, i, j)
      double_star <-  v < 0.01
      single_star <-  v >= 0.01 & v < 0.05
      no_star <-  v >= 0.05 & v <= 1
      
      
      grid.text("**", x[ double_star ] , y[ double_star ] - gb_h*0.35  , gp = gpar(fontsize = 16))
      grid.text("*", x[ single_star ], y[ single_star ] - gb_h*0.35   , gp = gpar(fontsize = 16))
      grid.text(" ", x[ no_star ], y[ no_star ] - gb_h*0.35   , gp = gpar(fontsize = 16))
      
      
    }
    
    
    
    stars_pvalues_cell <- function(j, i, x, y, w, h, fill) {
      
      gb = textGrob("*")
      gb_w = convertWidth(grobWidth(gb), "mm")
      gb_h = convertHeight(grobHeight(gb), "mm")
      
      
      
      if(pvalues[i, j] < 0.01 ) {
        grid.text("**", x, y - gb_h*0.35, gp = gpar(fontsize = 16))
      } else if( (pvalues[i, j] >= 0.01 & pvalues[i, j] < 0.05 )) {
        grid.text("*", x, y - gb_h*0.35, gp = gpar(fontsize = 16))
      } else if( (pvalues[i, j] >= 0.05 & (pvalues[i, j] <= 1 )) ) {
        grid.text(" ", x, y - gb_h*0.35, gp = gpar(fontsize = 16))
      }
      
    }
    
    
    
    
    # Colors for the heatmap body
    col_fun = colorRamp2(c(-1, 0, 1), c("blue", "white", "red"))
    
    
    
    
    
    
    if( nrow( df_filtered ) > 400 ){
      
      ht <- Heatmap(  betas, name = "Beta", col = col_fun,
                      cluster_columns = FALSE,
                      show_row_dend = FALSE,
                      show_column_dend= FALSE,
                      row_order = rownames(betas)[order(rownames(betas))],
                      show_row_names = TRUE,
                      row_names_gp = gpar(fontsize = 12),
                      row_names_side = "left",
                      column_labels = colnames(betas),
                      column_order = colnames(betas)[order(colnames(betas))],
                      column_names_max_height = convertHeight( grobWidth(textGrob(colnames(betas), gpar(fontsize = 11, fontface = 1))) , "mm"),
                      column_names_side = "top",
                      column_title_side = c("top"),
                      column_names_rot = 45,
                      column_names_gp = gpar(fontsize = 11),
                      width = unit(dim(betas)[2]*0.7, "cm"), height = unit(dim(betas)[1]*0.6, "cm"),
                      layer_fun = stars_pvalues,
                      heatmap_legend_param = list(
                        title = "Beta",
                        title_gp = gpar(fontsize = 12, fontface = "bold"),
                        title_position = "leftcenter",
                        col = col_fun,
                        at = (c(-1,-0.5,0,0.5, 1)),
                        labels = c("-1", "-0.5", "0", "0.5", "1"),
                        legend_width = unit(16, "cm"),
                        labels_gp = gpar(fontsize = 12), labels_rot = 0,
                        title_gp = gpar(fontsize = 12.5, fontface = "bold"),
                        legend_direction = c("horizontal") ),
                      border = TRUE
      )
      
      ht = ComplexHeatmap::draw(ht,
                                annotation_legend_side = "bottom",
                                legend_grouping = "original",
                                heatmap_legend_side = c("top")
      )
      
      print("---> Running layer_fun")
      
      
    } else{
      
      ht <- Heatmap(  betas, name = "Beta", col = col_fun,
                      cluster_columns = FALSE,
                      show_row_dend = FALSE,
                      show_column_dend= FALSE,
                      row_order = rownames(betas)[order(rownames(betas))],
                      show_row_names = TRUE,
                      row_names_gp = gpar(fontsize = 12),
                      row_names_side = "left",
                      column_labels = colnames(betas),
                      column_order = colnames(betas)[order(colnames(betas))],
                      column_names_max_height = convertHeight( grobWidth(textGrob(colnames(betas), gpar(fontsize = 11, fontface = 1))) , "mm"),
                      column_names_side = "top",
                      column_title_side = c("top"),
                      column_names_rot = 45,
                      column_names_gp = gpar(fontsize = 11),
                      width = unit(dim(betas)[2]*0.7, "cm"), height = unit(dim(betas)[1]*0.6, "cm"),
                      cell_fun = stars_pvalues_cell,
                      heatmap_legend_param = list(
                        title = "Beta",
                        title_gp = gpar(fontsize = 12, fontface = "bold"),
                        title_position = "leftcenter",
                        col = col_fun,
                        at = (c(-1,-0.5,0,0.5, 1)),
                        labels = c("-1", "-0.5", "0", "0.5", "1"),
                        legend_width = unit(16, "cm"),
                        labels_gp = gpar(fontsize = 12), labels_rot = 0,
                        title_gp = gpar(fontsize = 12.5, fontface = "bold"),
                        legend_direction = c("horizontal") ),
                      border = TRUE
      )
      
      ht = ComplexHeatmap::draw(ht,
                                annotation_legend_side = "bottom",
                                legend_grouping = "original",
                                heatmap_legend_side = c("top")
      )
      
      print("---> Running cell_fun")
      
      
    }
    
    
    
    HT_mr_plot <<- ht
    
    
    final_width_mr <<- (( (convertX( grobWidth(textGrob(rownames(betas), gpar(fontsize = 11, fontface = 1))) , "inch", valueOnly = TRUE)/4 ) + convertX(  ComplexHeatmap:::width(ht) , "inch", valueOnly = TRUE )  ) )
    
    final_height_mr <<- ( convertX(  ComplexHeatmap:::height(ht) , "inch", valueOnly = TRUE ) )
    
    print(final_width_mr)
    print(final_height_mr)
    
    output$heatmap_mr <- shiny::renderPlot({
      
      
      draw(ht)
      
    },
    
    width = ( final_width_mr * 72 ) ,
    
    height = ( final_height_mr * 72 )
    
    )
    
    
    
    
  }
  
  )
  
  
  
  
  observeEvent(input$button_mr_plot,{
    
    req( isTruthy(input$pathway_mr_plot) && isTruthy(input$protein_mr_plot)  && input$boolean_mr_plot == "AND"  )
    
    
    output$download.button.mr.plot <- renderUI({
      
      
      shiny::downloadButton(
        "download_mr_plot", "Download Plot" )
    } )
    
    
    
  } )
  
  
  
  
  output$download_mr_plot <- downloadHandler(
    
    
    filename = function() {
      
      if( !is.null( input$pathway_mr_plot ) & is.null( input$pathway_mr_plot ) ){
        
        paste0("Heatmap_" , input$pathway_mr_plot, "_mr.svg")
        
      }else if( is.null( input$pathway_mr_plot ) & !is.null( input$pathway_mr_plot ) ){
        
        paste0("Heatmap_" , input$protein_mr_plot, "_mr.svg")
        
      } else if( !is.null( input$pathway_mr_plot ) & !is.null( input$pathway_mr_plot ) ){
        
        paste0("Heatmap_" , input$pathway_mr_plot, "_",input$protein_mr_plot, "_mr.svg")
        
        
      }
      
    },
    
    content = function(file) {
      
      svg(file, width = final_width_mr, height = final_height_mr  )
      
      draw(HT_mr_plot)
      
      dev.off()
      
    }
    
  )
  
  
  
  
  
  
  observeEvent(input$button_mr_plot,{
    
    req( isTruthy(input$pathway_mr_plot) && isTruthy(input$protein_mr_plot)  && input$boolean_mr_plot == "AND"  )

    
    output$download.button.mr.plot.data <- renderUI({
      
      
      shiny::downloadButton(
        "download_mr_plot_data", "Download Data" )
    } )
    
    
    
  } )
  
  
  
  
  
  
  output$download_mr_plot_data <- downloadHandler(
    filename = function() {
      
      if( !is.null( input$pathway_mr_plot ) & is.null( input$pathway_mr_plot ) ){
        
        paste0("Data_" , input$pathway_mr_plot, "_mr.csv")
        
      }else if( is.null( input$pathway_mr_plot ) & !is.null( input$pathway_mr_plot ) ){
        
        paste0("Data_" , input$protein_mr_plot, "_mr.csv")
        
      } else if( !is.null( input$pathway_mr_plot ) & !is.null( input$pathway_mr_plot ) ){
        
        paste0("Data_" , input$pathway_mr_plot, "_",input$protein_mr_plot, "_mr.csv")
        
        
      }
      
    },
    
    content = function(file) {
      
      write.csv(df_to_download, file, row.names = FALSE)
      
    }
    
  )
  
  
  
  
  # End Heatmap
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Download page start
  
  # Supplementary table 1
  output$table_supp1 <- renderReactable({
    reactable(measured_prot_anno,
              filterable = TRUE,
              searchable = TRUE,
              bordered = TRUE,
              highlight = TRUE,
              defaultColDef = colDef(
                # cell = function(value) format(value, nsmall = 1),
                align = "center",
                headerStyle = list(background = "#f7f7f8")
              ),
              defaultPageSize = 5,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(5, 10, 25, 50, 100),
              paginationType = "jump",
              showPagination = TRUE )
  })
  
  output$downloadData_1 <- downloadHandler(
    filename = function() {
      paste("SuppTable1_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- input$table_state
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Supplementary table 2
  output$table_supp2 <- renderReactable({
    reactable(prot_anno,
              filterable = TRUE,
              searchable = TRUE,
              bordered = TRUE,
              highlight = TRUE,
              defaultColDef = colDef(
                # cell = function(value) format(value, nsmall = 1),
                align = "center",
                headerStyle = list(background = "#f7f7f8")
              ),
              columns = list(
                `Effect allele frequency` = colDef(cell = function(value) format(value, digits = 2, scientific = FALSE ) ),
                `Beta` = colDef(minWidth = 50,
                                cell = function(value) format(value, digits = 2, scientific = FALSE ),
                                filterable = FALSE,
                                searchable = TRUE),
                `SE` = colDef(minWidth = 50,
                              cell = function(value) format(value, digits = 2, scientific = FALSE ),
                              filterable = FALSE,
                              searchable = TRUE),
                `Log10 p-value` = colDef(filterable = FALSE,
                                         searchable = TRUE),
                `F statistic` = colDef(cell = function(value) format(value, digits = 5, scientific = FALSE ),
                                       filterable = FALSE,
                                       searchable = TRUE)
              ),
              defaultPageSize = 5,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(5, 10, 25, 50, 100),
              paginationType = "jump",
              showPagination = TRUE )
  })
  
  output$downloadData_2 <- downloadHandler(
    filename = function() {
      paste("SuppTable2_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- input$table_state
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Supplementary table 3
  output$table_supp3 <- renderReactable({
    reactable(met_anno,
              filterable = TRUE,
              searchable = TRUE,
              bordered = TRUE,
              highlight = TRUE,
              defaultColDef = colDef(
                # cell = function(value) format(value, nsmall = 1),
                align = "center",
                headerStyle = list(background = "#f7f7f8")
              ),
              defaultPageSize = 5,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(5, 10, 25, 50, 100),
              paginationType = "jump",
              showPagination = TRUE )
  })
  
  output$downloadData_3 <- downloadHandler(
    filename = function() {
      paste("SuppTable3_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- input$table_state
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Supplementary table 4
  output$table_supp4 <- renderReactable({
    reactable(studios_description,
              filterable = FALSE,
              searchable = FALSE,
              sortable = FALSE,
              bordered = TRUE,
              highlight = TRUE,
              defaultColDef = colDef(
                # cell = function(value) format(value, nsmall = 1),
                align = "center",
                headerStyle = list(background = "#f7f7f8")
              ),
              defaultPageSize = 19,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(5, 10, 25, 50, 100),
              paginationType = "jump",
              showPagination = TRUE )
  })
  
  output$downloadData_4 <- downloadHandler(
    filename = function() {
      paste("SuppTable4_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- input$table_state
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  
  # Download page finish
  
  
  
  # Download Annex
  output$statFile <- downloadHandler(
    filename = function() {
      paste("Annex1_", Sys.Date(), ".csv", sep = "")
    },
    content=function(file) {
      file.copy("www/Proteomics_and_metabolomics_analyses_in_POEM.csv", file)
    }
  )
  
  
  
} # end server


shinyApp(ui, server)



