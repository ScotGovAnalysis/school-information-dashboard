
section_header_output <- function(id) {
  
  # # Initiate namespace for module
  ns <- NS(id)
  
  # # Value Box Output
  valueBoxOutput(ns("header"), width = 12)
  
}

section_header_server <- function(input, output, session, section_name, box_colour = "yellow") {
  
  section_header <- ifelse(stringr::str_detect(section_name, "Attainment"),
                           "Attainment Profile",
                           paste(section_name, "Profile"))
  
  # Render Value Box
  output$header <- renderValueBox({
    valueBox(value = section_header,
             subtitle = "",
             color = box_colour)
  })
  
  
  # Text to be included in popup for each section
  text <- case_when(
    
    ### Pupil Profile ----
    
    section_name == "Pupil" ~ 
      paste0(
        "Male/Female: This chart shows the percentage of pupils recorded as male 
    or female in the pupil census. The information collected is gathered by 
    schools.",
        p("Pupils by stage: This chart gives a breakdown in percentage of all the 
    pupils in your chosen area or school by stage. This information is collected 
    from the Pupil Census."),
        
        p("SIMD: The Scottish Index of Multiple Deprivation (SMD) shows where 
    Scotland's most deprived areas are. Deprived does not mean poor or 
    low income. It can also mean people living in those areas have fewer 
    resources and opportunities. For example in health and education. Please note 
    that not all income deprived people live in deprived areas, and not everyone 
    living in a deprived area is income deprived.
    This chart shows the areas where children and young people in the school/area live 
    (SIMD 1 = most deprived areas, SIMD 2 = least deprived areas). This information 
    is based on their postcodes. For more information, please see the section 9, 
    question 9.1 in the frequently asked questions"),
        
        p("ASN: This chart shows the percentage of pupils in your chosen school/area 
    who were recorded as having Additional Support Needs (ASN).
    Care must be taken then looking at the number of pupils identified with ASN as 
    the recording of pupils with ASN varies according to local practice and 
    definition. Therefore, comparisons should not be made over time, or between 
    schools. For more information please see section 9 question 9.2 of the 
    frequently asked questions."),
        
        p("FSM: This chart shows the percentage of pupils in your chosen school/area 
    who were registered for free school meals.  Please note that there may be 
    some pupils who are entitled to free school meals but have not registered.  
    These pupils will be included in the 'No FSM' category. 
    Since January 2015, all pupils in Primary 1, Primary 2 and Primary 3 are 
    provided with free school meals, regardless of their personal circumstances.  
    Therefore, the information shown here is only for the pupils in Primary 4 to 
    Primary 7 inclusive - as these children and young people are to meet the 
    required criteria to register for free school meals.     
    In some rare cases, schools may have their own local initiatives to give free 
    school meals to pupils who are not eligible to register for free school meals 
    under the national criteria. 
    Note that in Glasgow City, free school meals are also provided for all pupils 
    in Primary 4. Therefore the percentage of pupils registered for free school 
    meals data in Glasgow is based on pupils in Primary 5 to Primary 7 inclusive.
    For more information, please see the Section 9, Question 9.3 in the 
      Frequently Asked Questions"),
        
        p("EAL: This chart shows the percentage of pupils in your chosen school/area 
    who were recorded as having English as an Additional Language. 
    For more information, please see the Section 9, Question 9.4 in the 
    Frequently Asked Questions."),
        
        p("Ethnicity: This chart shows the percentage of pupils in your chosen 
      school/area by their ethnicity. Categories are grouped together as follows:"),
        p("White UK : White Scottish, White Other British"),
        p("White Other: White Gypsy/Traveller, White Polish, White Irish, White Other"),
        p("Ethnic Minority: Mixed, Asian - Indian, Asian - Pakistani, 
      Asian - Bangladeshi, Asian - Chinese, Asian - Other, Caribbean/Black, African, Arab, Other "),
        p("Not known: Unknown, Not disclosed "),
        p("For more information, please see the Section 9, Question 9.5 in the Frequently Asked Questions."),
        
        p("Gaelic: This chart shows the percentage of pupils in your chosen 
    school/area who were taught in Gaelic or are learning Gaelic as an 
    additional language. for more information, please see the Section 9, 
      Question 9.6 in the Frequently Asked Questions."),
        p("Area: This chart shows the percentage of pupils in your chosen school/area 
    by whether they lived in an urban or rural area.  This information is based 
    on their postcode. For more information, please see the Section 9, Question 
      9.7 in the Frequently Asked Questions."),
        
        p("A link to the publication from which this data was taken can be found here: ",
          a("Schools Summary Statistics Link", 
            href= "https://www.gov.scot/collections/school-education-statistics/#summarystatisticsforschoolsinscotland"))
        
      ),
    
    ### Attendance ----
    
    section_name == "Attendance" ~ 
      paste0(
        "This chart provides more information on the rates of attendance,
      authorised absence and unauthorised absence.  This information is shown
      by stage (i.e. which year the pupil is in) for your chosen school/area.
      Attendance and absence data is collected from publicly funded schools every 2
      years. The information shown here is for the 2018/19 school year. For more
      information follow this link to the publication: ",
        a("Schools Summary Statistics Link",
          href= "https://www.gov.scot/collections/school-education-statistics/#summarystatisticsforschoolsinscotland")
      ),
    
    ### Primary Attainment ----
    
    section_name == "Primary Attainment" ~ 
      paste0(
        "This chart is designed to add context to the achievements of pupils in
      your chosen school/area.  For example, the doughnut chart in the right
      on this box shows how pupils in your chosen school/area are achieving
      against the Curriculum for Excellence (CfE) level relevant to their stage
      (without taking into account the characteristics of the pupils being
      taught in the school). Whereas, the bar chart shown here attempts to show
      you how pupils in your chosen school/area are collectively achieving
      compared to pupils across Scotland who are similar to them (based on 9
      characteristics). Therefore, if the chart shows  that 'Your chosen school
    / area' is higher than 'Pupils in Scotland with similar characteristics',
    this means that the achievements of your pupils in your chosen school area
    have been declared as being higher/lower than pupils in Scotland with
    similar characteristics",
        p("This chart uses information collected from publicly funded schools as at
      June of each year in the Achievement of Curriculum for Excellence (CfE)
      Levels data collection."),
        p("Please note that the 2018/19 information is now badged as 'Official
    Statistics' rather than 'Experimental Statistics' . More information on this
    can be found in the information button next to the Achievement of a CfE
    Level charts in the top right corner of the dashboard. For more information,
    please see the Section 5 in the Frequently Asked Questions or follow this
    link to the publication: ",
          a("Achievement of Curriculum for Excellence Levels Link",
            href= "https://www.gov.scot/collections/school-education-statistics/#achievementofcurriculumforexcellencelevels"))
        
      ),
    
    ### Secondary Attainment ----
    
    section_name == "Secondary Attainment" ~
      paste0(
        p("Curriculum for Excellence"),
        p("Most pupils are expected to achieve specific Curriculum for Excellence (CfE) levels,
    by the end of a particular stage.  These are:  CfE Early Level (at end of Primary 1);
    CfE 1st Level (at end of Primary 4); CfE 2nd Level (at end of Primary 7); and CfE 3rd/4th Level
    (at end of Secondary 3)."),
        p("For more information, please see the Section 4 in the Frequently Asked Questions tab"),
        
        br(),
        p("Leavers' Breadth and Depth Profile"),
        p("The chart shows the full range of SCQF accredited awards gained at SCQF levels 1 to 7.
     The data is taken from Insight. Note that the data is accurate to the point in time that Insight
     data was first published on ParentZone or on this dashboard. There may have been changes to the
     data for previous years which will be reflected in Insight itself but not in any of the published information."),
        p("Only the latest and best awards in each subject are counted. For example if someone has a
    national 5 and a Higher in Mathematics, it is only the Higher that will contribute to this measure. Schools
    that offer young people a curriculum based on a range of leaver destinations may have young people
    who sit fewer qualifications but who are better prepared, with more appropriate skills and experiences
    for the world of work."),
        p("For more information on SCQF levels, please see the Section 6 in the Frequently Asked Questions tab."),
        p("For more information on the virtual comparator and Insight see Section 7 in the Frequently Asked Questions tab."),
        
        br(),
        p("Percentage of school leavers gaining SCQF credited awards"),
        p("The first table shows the full range of SCQF accredited awards gained at SCQF levels 1 to 7.
    The second table shows the corresponding data for its virtual comparator. When a pupil has been awarded a
    course at a SCQF level, they will be counted in the corresponding box(es) in the table. Only the latest and best awards
    in each subject are counted. For example if someone has a National 5 and a Higher in Mathematics, it is only
    the Higher that will contribute to this measure."),
        p("Schools that offer young people a curriculum based on a range of leaver destinations may have young people
    who sit fewer qualifications but who are better prepared, with more appropriate skills and experiences for
    the world of work. For more information on SCQF levels, please see the Section 6 in the Frequently Asked Questions tab."),
        
        br(),
        p("Leavers' Leavers by SIMD"),
        p("Shows attainment across school leavers by level of deprivation. (Q1 = most deprived areas, Q5 = least deprived areas).
    The school's average total tariff score is shown below alongside its virtual comparator. For more information on
      deprivation, please see the Section 9, Question 9.1 in the Frequently Asked Questions tab."),
        br(),
        p("Leavers' Literacy and Numeracy"),
        p("Shows the percentage of school leavers who attained literacy and numeracy at  SCQF level 4 or better,
    and SCQF level 5 or better.  They show the attainment of school leavers from this school alongside its virtual comparator.
    For more information on the virtual comparator, please see the Section 7, Question 7.2 in the Frequently Asked Questions tab.")
      ),
    
    ### Population ----
    
    section_name == "Population" ~ 
      paste0(
        "This chart shows you the population of pupils and teachers of your chosen
    school/area over time. For more information follow this link to the publication: ",
        a("Schools Summary Statistics Link",
          href= "https://www.gov.scot/collections/school-education-statistics/#summarystatisticsforschoolsinscotland")
      )
    
  )
  
  
  # When user clicks value box, display pop up with information text
  onclick(
    id = "header",
    expr = showModal(modalDialog(
      title = section_header,
      HTML(text)
    ))
  )
  
}
