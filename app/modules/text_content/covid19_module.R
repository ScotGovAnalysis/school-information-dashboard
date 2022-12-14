covid19_ui <- function(id, school_type) {
  
  ns <- NS(id)
  
  tagList(
    
    # Covid-19 Button
    actionButton(
      inputId = ns("button"), 
      label = "COVID-19", 
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
          p("S3 Attainment in literacy and numeracy statistics - 
            Impact on data collection for 2019/20 and 2021/22:"),
          p("Achievement of CfE Levels data for secondary school pupils were 
              not collected in 2020/21 due to other pressures on these schools 
              including implementation of the SQA NationalQualifications 
              Alternative Certification Model which was used to award 
              National 5s, Highers and Advanced Highers in 2021 More detail 
              is available by following this link to ACEL publication:",
              a("ACEL publication", 
                href="https://www.gov.scot/publications/scottish-exchange-of-data-achievement-of-curriculum-for-excellence-levels")),
          p("School leavers attainment statistics:"),
          p("The cancellation of exams and external assessment of 
              coursework in 2020, and the use of the
              Alternative Certification Model in 2021, will have affected 
              the attainment of many 2020/21 school leavers.
              The impacts of these different approaches to certification upon 
              school leaver attainment means that care
              should be taken when making comparisons. Any changes between the 
              attainment levels of the 2020/21 cohort,
              the 2019/20 cohort and those of previous years should not be seen 
              as an indication that performance has
              improved or worsened, without further evidence. More detail is 
              available in the publication which can be
              found using this link:",
              a("School summary statistics publication", 
                href="https://www.gov.scot/publications/summary-statistics-attainment-initial-leaver-destinations-no-4-2022-edition/")),
          p("School leavers destinations statistics:"),
          p("Destinations figures reflect both choices made by pupils, and the 
              opportunities available to them upon
              leaving school. For 2019/20 school leavers the start dates for 
              some opportunities were delayed, leading
              to a direct impact on the destinations recorded for some pupils. 
              The pandemic may also have affected
              local partnerships' ability to track some school leavers through 
              home visits. It is likely that the
              pandemic will have continued to affect the choices made by, and 
              opportunities available to, some school
              leavers in 2020/21. The impact of the pandemic on school leaver 
              destinations should be kept in mind when
              making comparisons between 2020/21 and 2019/20, and when comparing 
              with earlier years. More detail is
              available in the publication which can be accessed using this 
              link:",
              a("School leavers destinations", 
                href = "https://www.gov.scot/publications/summary-statistics-attainment-initial-leaver-destinations-no-4-2022-edition/pages/3/"))
        ))
        
      } else {
        
        HTML(paste0(
          p("Impact on data collection: In 2019/20, Achievement of Curriculum for
          Excellence data were not collected for any pupils due to difficulties 
          in collecting data whilst schools were closed due to the COVID-19 pandemic."),
          p("Pupils' achievement of CfE levels in 2020/21 were affected by the 
          coronavirus (COVID-19) pandemic. It is likely that 2021/22 results may 
          also be affected by the ongoing impact of the pandemic on young people's 
          learning."),
          p("This should be kept in mind when making comparisons over time"),
          p("More detail is available using this link here: ",
            a("Scottish Exchange of Data: achievement of Curriculum for Excellence levels",
              href="https://www.gov.scot/publications/scottish-exchange-of-data-achievement-of-curriculum-for-excellence-levels"))
        ))
        
      }
      
    )
    
  )
  
}