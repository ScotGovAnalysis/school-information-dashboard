covid19_ui <- function(id, school_type) {
  
  ns <- NS(id)
  
  tagList(
    
    # Covid-19 Button
    actionButton(
      ns("button"), 
      "COVID-19", 
      style = "color: white; background-color: purple; border-color:purple",
      width = "49%"),
    
    # Pop-up window
    bsModal(
      id = ns("popup_window"), 
      title = "COVID-19", 
      trigger = ns("button"), 
      size = "large",
      
      if(school_type == "Secondary") {
        
        HTML(paste0(
          p("S3 Attainment in literacy and numeracy statistics - Impact on data collection for 2019/20 and 2020/21:"),
          p("The Scottish Government did not collect Achievement of CfE Levels data for S3 pupils.
                More detail is available by following this link to ACEL publication:",
            a("ACEL publication", href="https://www.gov.scot/publications/scottish-exchange-of-data-achievement-of-curriculum-for-excellence-levels")),
          p("School leavers attainment statistics:"),
          p("The cancellation of exams and external assessment of coursework in 2020, and the use of the
              Alternative Certification Model in 2021, will have affected the attainment of many 2020/21 school leavers.
              The impacts of these different approaches to certification upon school leaver attainment means that care
              should be taken when making comparisons. Any changes between the attainment levels of the 2020/21 cohort,
              the 2019/20 cohort and those of previous years should not be seen as an indication that performance has
              improved or worsened, without further evidence. More detail is available in the publication which can be
              found using this link:",
            a("School summary statistics publication", href="https://www.gov.scot/publications/summary-statistics-attainment-initial-leaver-destinations-no-4-2022-edition/")),
          p("School leavers destinations statistics:"),
          p("Destinations figures reflect both choices made by pupils, and the opportunities available to them upon
                leaving school. For 2019/20 school leavers the start dates for some opportunities were delayed, leading
                to a direct impact on the destinations recorded for some pupils. The pandemic may also have affected
                local partnerships' ability to track some school leavers through home visits. It is likely that the
                pandemic will have continued to affect the choices made by, and opportunities available to, some school
                leavers in 2020/21. The impact of the pandemic on school leaver destinations should be kept in mind when
                making comparisons between 2020/21 and 2019/20, and when comparing with earlier years. More detail is
                available in the publication which can be accessed using this link:",
            a("School leavers destinations", href = "https://www.gov.scot/publications/summary-statistics-attainment-initial-leaver-destinations-no-4-2022-edition/pages/3/"))
          
        ))
        
      } else {
        
        HTML(paste0(
          p("Impact on data collection: The Scottish Government did not collect Achievement of CfE Levels data for any pupils in 2019/20."),
          p("Schools were closed in Scotland in March 2020 and January 2021 as a result of the pandemic. This is likely to have had a
              negative effect on some pupils' progress and attainment. Attainment of socio-economically deprived children may have been
              amongst those most negatively affected."),
          p("It is therefore likely to have had an impact on the CfE levels some children have achieved. This will be reflected in the
              2020/21 figures and should be kept in mind when interpreting these. In particular, when comparing with figures for 2018/19 and
              before."),
          p("More detail is available using this link here: ",
            a("Scottish Exchange of Data: achievement of Curriculum for Excellence levels",
              href="https://www.gov.scot/publications/scottish-exchange-of-data-achievement-of-curriculum-for-excellence-levels"))
        ))
        
      }
      
    )
    
   #Secondary: 
  #   bsModal(
  #     id = ns("popup_window"), 
  #     title = "COVID-19", 
  #     trigger = ns("button"), 
  #     size = "large",
  #     p("S3 Attainment in literacy and numeracy statistics - Impact on data collection for 2019/20 and 2020/21:"),
  #     p("The Scottish Government did not collect Achievement of CfE Levels data for S3 pupils. 
  #       More detail is available by following this link to ACEL publication:", 
  #       a(href="https://www.gov.scot/publications/scottish-exchange-of-data-achievement-of-curriculum-for-excellence-levels")),
  #     p("School leavers attainment statistics:"),
  #     p("The cancellation of exams and external assessment of coursework in 2020, and the use of the 
  #     Alternative Certification Model in 2021, will have affected the attainment of many 2020/21 school leavers. 
  #     The impacts of these different approaches to certification upon school leaver attainment means that care 
  #     should be taken when making comparisons. Any changes between the attainment levels of the 2020/21 cohort, 
  #     the 2019/20 cohort and those of previous years should not be seen as an indication that performance has 
  #     improved or worsened, without further evidence. More detail is available in the publication which can be
  #     found using this link:", 
  #     a(href="https://www.gov.scot/publications/summary-statistics-attainment-initial-leaver-destinations-no-4-2022-edition/")),
  #     p("School leavers destinations statistics:"),
  #     P("Destinations figures reflect both choices made by pupils, and the opportunities available to them upon 
  #       leaving school. For 2019/20 school leavers the start dates for some opportunities were delayed, leading 
  #       to a direct impact on the destinations recorded for some pupils. The pandemic may also have affected 
  #       local partnerships' ability to track some school leavers through home visits. It is likely that the 
  #       pandemic will have continued to affect the choices made by, and opportunities available to, some school
  #       leavers in 2020/21. The impact of the pandemic on school leaver destinations should be kept in mind when 
  #       making comparisons between 2020/21 and 2019/20, and when comparing with earlier years. More detail is 
  #       available in the publication which can be accessed using this link:", 
  #       a(href = "https://www.gov.scot/publications/summary-statistics-attainment-initial-leaver-destinations-no-4-2022-edition/pages/3/"))
  #     
  # )
 
  )
  
}