covid19_ui <- function(id) {
  
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
    )
    
  )
  
}