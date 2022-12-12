important_info_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    # Important information Button
    actionButton(
      ns("button"), 
      "Important Information", 
      style = "color: white; background-color: purple; border-color:purple",
      width = "98%"),
    
    # Pop-up window
    bsModal(
      
      id = ns("popup_window"), 
      title = "Important Information", 
      trigger = ns("button"), 
      size = "large",
      
      HTML(paste0(
        
        # Missing data symbols ----
        
        p("Where data is not shown the following codes are used to symbolise
               the reason:"),
        tags$ul(
          tags$li("c = The data is confidential and has been suppressed to 
                    ensure the identity of individuals is not disclosed"),
          tags$li("z = This value does not apply to this school/area"),
          tags$li("x = This data was not available at the time of publishing"),
          tags$li("w = No data was recorded"),
        ),

        hr(),
        
        # Useful links ----
        
        h4(strong("Useful links:")),
        p(a("Scottish Government website",
            href="https://www.gov.scot",
            target = "_blank")),
        p(a(" Parentzone Scotland",
            href="https://education.gov.scot/parentzone",
            target = "_blank")),
        p(a("Parentzone Education Glossary",
            href="https://education.gov.scot/parentzone/my-school/Education%20glossary",
            target = "_blank")),
        p(a("Scottish Credit and Qualifications Framework (SCQF)",
            href="https://scqf.org.uk/",
            target = "_blank")),
        p(a("Scottish Qualifications Authority (SQA)",
            href="https://www.sqa.org.uk/",
            target = "_blank")),
        p(a("Scottish Government Education and Training",
            href="https://www.gov.scot/Topics/Education",
            target = "_blank")),
        p(a(" Scottish Index of Multiple Deprivation (SIMD)",
            href="https://www.gov.scot/Topics/Statistics/SIMD/",
            target = "_blank")),
        p(a("Scottish Government Education Statistics",
            href="https://www.gov.scot/collections/school-education-statistics/",
            target = "_blank")),
        p(a("Achievement of CfE Levels publication",
            href="https://www.gov.scot/collections/school-education-statistics/#achievementofcurriculumforexcellencelevels",
            target = "_blank")),
        p(a("Insight",
            href="https://www.gov.scot/insightbenchmarking",
            target = "_blank")),
        p(a("Summary Statistics for schools in Scotland publication",
            href="https://www.gov.scot/collections/school-education-statistics/#summarystatisticsforschoolsinscotland",
            target = "_blank")),
        p(a("Summary Statistics for attainment, leaver destinations and healthy 
              living publication",
            href="https://www.gov.scot/collections/school-education-statistics/#schoolleaverinitialdestinationsandattainment",
            target = "_blank")),

        hr(),
        
        # Accessibility statement ----
        
        h4(strong("Accessibility Statement")),
        
        p("This accessibility statement applies to the School Information 
            Dashboards."),
        
        p("This website is run by the Scottish Government. We want as many 
            people as possible to be able to use this website. For example, 
            that means you should be able to:"),
        tags$ul(
          tags$li("navigate most of the website using just a keyboard"),
          tags$li("navigate most of the website using speech recognition 
                    software"),
          tags$li("listen to most of the website using a screen reader"),
          tags$li("interactive charts have been combined with a data download 
                    button to enable interaction with the data"),
          tags$li("we've also made the website text as simple as possible to 
                    understand.")
        ),
        
     
        p("We know some parts of this website are not fully accessible. The 
            content listed below is non-accessible for the following reasons."),
        tags$ul(
          tags$li("interactive charts are not accessible to those using a 
                    screen reader. We have however made the data downloadable
                    in an excel format for this purpose"),
          tags$li("you cannot modify the line height or spacing of text"),
        
       
         ),

        p("This statement was prepared on 31/10/2022. 
            It was last reviewed on 31/10/2022."),
        p("This website is scheduled for testing on 17/12/2022. 
            The test will be carried out by the digital accessibility team at the 
            Scottish Government."),
        
        strong("Feedback and contact information"),
        p("Please contact us at:",
          a("ceu@gov.scot.", href="mailto:ceu@gov.scot"),
          "We aim to get back to you in 7 days."),
        
        hr(),
        
        # Link to GitHub code ----
        
        p("The code used to produce this dashboard is publicly available on",
        a("GitHub.", 
          href="https://github.com/DataScienceScotland/school-information-dashboard",
          target = "_blank"))
        
      ))
      
    )
    
  )
  
}