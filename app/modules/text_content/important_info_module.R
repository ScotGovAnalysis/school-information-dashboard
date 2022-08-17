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
        
        ### Important Information ----
        
        p(h3("Where data is not shown the following codes are used to symbolise
               the reason:")),
        p("c = The data is confidential and has been suppressed to ensure 
               the identity of individuals is not disclosed"),
        p("z = This value does not apply to this school/area"),
        p("x = This data was not available at the time of publishing"),
        p("w = No data was recorded"),
        
        br(),
        br(),
        
        p(h3("Useful links:")),
        p(a("Scottish Government website",
            href="https://www.gov.scot")),
        p(a(" Parentzone Scotland",
            href="https://education.gov.scot/parentzone")),
        p(a("Parentzone Education Glossary",
            href="https://education.gov.scot/parentzone/my-school/Education%20glossary")),
        p(a("Scottish Credit and Qualifications Framework (SCQF)",
            href="https://scqf.org.uk/")),
        p(a("Scottish Qualifications Authority (SQA)",
            href="https://www.sqa.org.uk/")),
        p(a("Scottish Government Education and Training",
            href="https://www.gov.scot/Topics/Education")),
        p(a(" Scottish Index of Multiple Deprivation (SIMD)",
            href="https://www.gov.scot/Topics/Statistics/SIMD/")),
        p(a("Scottish Government Education Statistics",
            href="https://www.gov.scot/collections/school-education-statistics/")),
        
        p(a("Achievement of CfE Levels publication",
            href="https://www.gov.scot/collections/school-education-statistics/#achievementofcurriculumforexcellencelevels")),
        p(a("Insight",
            href="https://www.gov.scot/insightbenchmarking")),
        p(a("Summary Statistics for schools in Scotland publication",
            href="https://www.gov.scot/collections/school-education-statistics/#summarystatisticsforschoolsinscotland")),
        p(a("Summary Statistics for attainment, leaver destinations and healthy living publication",
            href="https://www.gov.scot/collections/school-education-statistics/#schoolleaverinitialdestinationsandattainment")),
        
        br(),
        br(),
        
        ### Accessibility Statement ----
        
        p(h3("Accessibility Statement")),
        p("This accessibility statement applies to the School Information Dashboards."),
        br(),
        p("This website is run by the Scottish Government. We want as many people as
                possible to be able to use this website. For example, that means you should be able to:"),
        p("navigate most of the website using just a keyboard"),
        p("navigate most of the website using speech recognition software"),
        p("listen to most of the website using a screen reader "),
        p("interactive charts have been combined with a data download button to enable interaction with the data"),
        p("we've also made the website text as simple as possible to understand."),
        br(),
        
        
        p("This website is fully compliant with the Web Content Accessibility
          Guidelines version 2.1 AA standard. This website is partially compliant
          with the Web Content Accessibility Guidelines version 2.1 AA standard,
          due to [insert one of the following: 'the non-compliances',
          'the exemptions' or 'the non-compliances and exemptions']"),
        
        p("We know some parts of this website are not fully accessible. The content listed below is non-accessible for the following reasons."),
        p("interactive charts are not accessible to those using a screen reader. We have however made the data downloadable
            in an excel format for this purpose"),
        p("you cannot modify the line height or spacing of text"),
        br(),
        p("This statement was prepared on [date when it was first published]. It was
          last reviewed on [date when it was last reviewed]."),
        p("This website was last tested on [date]. The test was carried out by
            [add name of organisation that carried out test, or indicate that you did your own testing]."),
        p("We used this approach to deciding on a sample of pages to test [add
            link to explanation of how you decided which pages to test]."),
        br(),
        p("Feedback and contact information"),
        p("Please contact us at:"),
        p("email [email address]"),
        p("call [phone number]"),
        p("[add any other contact details]"),
        p("We aim to get back to you in 7 days.")
        
      ))
      
    )
    
  )
  
}
