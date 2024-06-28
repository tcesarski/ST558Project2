library(jsonlite)
library(httr)
library(tidyverse)
library(treemapify)
get_country_info <- function(country_name, chosen_cols){
  #Store the base url before the inputs as start_url.
  start_url <- "https://restcountries.com/v3.1/"
  #Based on documentation provided on API, paste together formatting of initial url, and inputs for what the final url should look like.. Use paste0 to not add spaces.
encoded_name <- URLencode(country_name)
  
  if(country_name == "all"){
    url <- paste0(start_url,
                  "all")
  }
  else{
    url <- paste0(start_url,
                  "name/",
                  encoded_name)
  }
  #Use the GET function to contact API.
  return_data <- GET(url)
  #Parse through data using fromJSON function from jsonlite package.
  parsed_data <- fromJSON(rawToChar(return_data$content))
  #Convert the article information to a tibble.
  exact_match <- parsed_data$name$common == country_name
  tibble_info <- as_tibble(parsed_data[exact_match, ])

  
  #Return ed_info tibble.
  tibble_info <- tibble(
    Country_Name = tibble_info$name$common,
    #Deal with missing values for capital.
    Capital = ifelse(is.null(tibble_info$capital), NA, 
                     as.character(tibble_info$capital)),
    Region = tibble_info$region,
    #Deal with missing values for subregion.
    Subregion = ifelse(is.null(tibble_info$subregion), NA, 
                     as.character(tibble_info$subregion)),
    Area = tibble_info$area,
    Population = tibble_info$population,
    Car_Side_Driving = tibble_info$car$side,
    Independence = ifelse(is.null(tibble_info$independent), NA, 
                       as.character(tibble_info$independent)),
    Landlocked = tibble_info$landlocked,
    UN_Member = tibble_info$unMember)
  
  tibble_info <- tibble_info |>
    select(Country_Name, chosen_cols)
  
  return(tibble_info)
}

get_region_info <- function(region_name, chosen_cols){
  #Store the base url before the inputs as start_url.
  start_url <- "https://restcountries.com/v3.1/region/"
  #Based on documentation provided on API, paste together formatting of initial url, and inputs for what the final url should look like.. Use paste0 to not add spaces.
  url <- paste0(start_url, 
                region_name)
  #Use the GET function to contact API.
  return_data <- GET(url)
  #Parse through data using fromJSON function from jsonlite package.
  parsed_data <- fromJSON(rawToChar(return_data$content))
  #Convert the article information to a tibble.
  tibble_info <- as_tibble(parsed_data)
  #Return as an integer using map_int. Apply the length function.
  
  #Return ed_info tibble.
  tibble_info <- tibble(
    Country_Name = parsed_data$name$common,
    Capital = as.character(parsed_data$capital),
    Region = parsed_data$region,
    Subregion = parsed_data$subregion,
    Area = parsed_data$area,
    Population = parsed_data$population,
    Car_Side_Driving = parsed_data$car$side,
    Independence = parsed_data$independent,
    Landlocked = parsed_data$landlocked,
    UN_Member = parsed_data$unMember) |>
    
    arrange(Country_Name) |>
    select(Country_Name, chosen_cols)
  
  return(tibble_info)
  
}

get_language_info <- function(language_name, chosen_cols){
  #Store the base url before the inputs as start_url.
  start_url <- "https://restcountries.com/v3.1/lang/"
  encode_lang <- URLencode(language_name)
  #Based on documentation provided on API, paste together formatting of initial url, and inputs for what the final url should look like.. Use paste0 to not add spaces.
  url <- paste0(start_url, 
                encode_lang)
  #Use the GET function to contact API.
  return_data <- GET(url)
  #Parse through data using fromJSON function from jsonlite package.
  parsed_data <- fromJSON(rawToChar(return_data$content))
  #Convert the article information to a tibble.
  tibble_info <- as_tibble(parsed_data)
  
  #Return ed_info tibble.
  tibble_info <- tibble(
    Country_Name = parsed_data$name$common,
    Capital = as.character(parsed_data$capital),
    Region = parsed_data$region,
    Subregion = parsed_data$subregion,
    Area = parsed_data$area,
    Population = parsed_data$population,
    Car_Side_Driving = parsed_data$car$side,
    Independence = parsed_data$independent,
    Landlocked = parsed_data$landlocked,
    UN_Member = parsed_data$unMember) |>
    
    arrange(Country_Name) |>
    select(Country_Name, chosen_cols)
  
  return(tibble_info)
}

get_pop_info <- function(min_value = 0, max_value = 20000000000, chosen_cols){
  start_url <- "https://restcountries.com/v3.1/all/"
  return_data <- GET(start_url)
  parsed_data <- fromJSON(rawToChar(return_data$content))
  tibble_info <- as_tibble(parsed_data)
  
  tibble_info <- tibble(
    Country_Name = parsed_data$name$common,
    Capital = as.character(parsed_data$capital),
    Region = parsed_data$region,
    Subregion = parsed_data$subregion,
    Area = parsed_data$area,
    Population = parsed_data$population,
    Car_Side_Driving = parsed_data$car$side,
    Independence = parsed_data$independent,
    Landlocked = parsed_data$landlocked,
    UN_Member = parsed_data$unMember)
  
  tibble_info <- tibble_info |>
    filter(Population >= min_value & Population <= max_value) |>
    arrange(Country_Name) |>
    select(Country_Name, chosen_cols)
  
  return(tibble_info)
}


get_area_info <- function(min_area = 100, max_area = 100000, chosen_cols){
  #Store the base url before the inputs as start_url.
  start_url <- "https://restcountries.com/v3.1/all/"
  #Based on documentation provided on API, paste together formatting of initial url, and inputs for what the final url should look like.. Use paste0 to not add spaces.
  #Use the GET function to contact API.
  return_data <- GET(start_url)
  #Parse through data using fromJSON function from jsonlite package.
  parsed_data <- fromJSON(rawToChar(return_data$content))
  #Convert the article information to a tibble.
  tibble_info <- as_tibble(parsed_data)
  
  #Return ed_info tibble.
  tibble_info <- tibble(
    Country_Name = parsed_data$name$common,
    Capital = as.character(parsed_data$capital),
    Region = parsed_data$region,
    Subregion = parsed_data$subregion,
    Area = parsed_data$area,
    Population = parsed_data$population,
    Car_Side_Driving = parsed_data$car$side,
    Independence = parsed_data$independent,
    Landlocked = parsed_data$landlocked,
    UN_Member = parsed_data$unMember)
  
  tibble_info <- tibble_info |>
    filter(Area >= min_area & Area <= max_area) |>
    arrange(Country_Name) |>
    select(Country_Name, chosen_cols)
  
  return(tibble_info)
}

get_bar <- function(data_input, cat1, cat2){
  ggraph <- ggplot(data_input |> drop_na(cat2) |> drop_na(cat1), aes_string(x = cat1, fill = cat2))
  ggraph + geom_bar() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x = cat1,
         y = "Count",
         title = paste0("Countries by ", cat1, " and ", cat2))
}

get_histogram <- function(input_data, x_val, y_val){
  g <- ggplot(input_data |> drop_na({{y_val}}), aes_string(x = x_val, fill = y_val)) +
    geom_histogram(bins = 20) +
    facet_wrap(y_val) +
    scale_fill_discrete(name = y_val) +
    scale_x_continuous(labels = scales::number_format()) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(x = x_val,
         y = "Count",
         title = paste0(x_val, " by ", y_val))
  
  print(g)
}


get_scatter <- function(input_data, facet_var){
  g <- ggplot(input_data |> drop_na({{facet_var}}), aes_string(x = "Population", y = "Area", color = facet_var)) +
    geom_point() +
    geom_smooth(method = lm) +
    facet_wrap(facet_var) +
    labs(title = paste0("Relationship between Population and Area based on ", facet_var))
  print(g)
}

get_num_sum <- function(dataframe, num_variable, cat_variable){
  dataframe |>
    group_by({{cat_variable}}) |>
    summarize(
      Mean = mean({{num_variable}}, na.rm = TRUE),
      Stdev = sd({{num_variable}}, na.rm = TRUE),
      Min = min({{num_variable}}, na.rm = TRUE),
      Q1 = quantile({{num_variable}}, 0.25, na.rm = TRUE),
      Median = median({{num_variable}}, na.rm = TRUE),
      Q3 = quantile({{num_variable}}, 0.75, na.rm = TRUE),
      Max = max({{num_variable}}, na.rm = TRUE)
    )
}




server <- function(input, output, session) {
  output$filter <- renderUI({
    if(input$filter == "Country Name"){
      return(
        selectInput("country", "Country Name", 
                    choices = c("Afghanistan", "Åland Islands", "Albania", "Algeria", "American Samoa", "Andorra", "Angola", "Anguilla", "Antarctica", "Antigua and Barbuda", "Argentina", "Armenia", "Aruba", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bermuda", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Bouvet Island", "Brazil", "British Indian Ocean Territory", "British Virgin Islands", "Brunei", "Bulgaria", "Burkina Faso", "Burundi", "Cambodia", "Cameroon", "Canada", "Cape Verde", "Caribbean Netherlands", "Cayman Islands", "Central African Republic", "Chad", "Chile", "China", "Christmas Island", "Cocos (Keeling) Islands", "Colombia", "Comoros", "Cook Islands", "Costa Rica", "Croatia", "Cuba", "Curaçao", "Cyprus", "Czechia", "DR Congo", "Denmark", "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini", "Ethiopia", "Falkland Islands", "Faroe Islands", "Fiji", "Finland", "France", "French Guiana", "French Polynesia", "French Southern and Antarctic Lands", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Gibraltar", "Greece", "Greenland", "Grenada", "Guadeloupe", "Guam", "Guatemala", "Guernsey", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Heard Island and McDonald Islands", "Honduras", "Hong Kong", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Isle of Man", "Israel", "Italy", "Ivory Coast", "Jamaica", "Japan", "Jersey", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Macau", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Martinique", "Mauritania", "Mauritius", "Mayotte", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", "Montserrat", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", "New Caledonia", "New Zealand", "Nicaragua", "Niger", "Nigeria", "Niue", "Norfolk Island", "North Korea", "North Macedonia", "Northern Mariana Islands", "Norway", "Oman", "Pakistan", "Palau", "Palestine", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Pitcairn Islands", "Poland", "Portugal", "Puerto Rico", "Qatar", "Republic of the Congo", "Romania", "Russia", "Rwanda", "Réunion", "Saint Barthélemy", "Saint Helena, Ascension and Tristan da Cunha", "Saint Kitts and Nevis", "Saint Lucia", "Saint Martin", "Saint Pierre and Miquelon", "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Sint Maarten", "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Georgia", "South Korea", "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Svalbard and Jan Mayen", "Sweden", "Switzerland", "Syria", "São Tomé and Príncipe", "Taiwan", "Tajikistan", "Tanzania", "Thailand", "Timor-Leste", "Togo", "Tokelau", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Turks and Caicos Islands", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "United States", "United States Minor Outlying Islands", "United States Virgin Islands", "Uruguay", "Uzbekistan", "Vanuatu", "Vatican City", "Venezuela", "Vietnam", "Wallis and Futuna", "Western Sahara", "Yemen", "Zambia", "Zimbabwe"))
      )
    } else if(input$filter == "Region"){
      return(selectInput("region", "Region Name",
                         choices = c("Africa","Antarctic", "Americas", "Asia", "Europe", "Oceania")))
    } else if(input$filter == "Language"){
      return(selectInput("language", "Language Name",
                         choices = c("Albanian", "Amharic", "Arabic", "Aramaic", "Armenian", "Azerbaijani", "Basque", "Belarusian", "Bengali", "Belizean Creole", "Bislama", "Bosnian", "Bulgarian", "Catalan", "Chamorro", "Chewa", "Chibarwe", "Chinese", "Cook Islands Māori", "Croatian", "Czech", "Danish", "Dari", "Dutch", "Dzongkha", "English", "Estonian", "Faroese", "Filipino", "Finnish", "Fijian", "Fiji Hindi", "French", "Galician", "Georgian", "German", "Gilbertese", "Greek", "Greenlandic", "Guaraní", "Guernésiais", "Haitian Creole", "Hebrew", "Herero", "Hiri Motu", "Hindi", "Hungarian", "Icelandic", "Indonesian", "Irish", "Italian", "Japanese", "Jèrriais", "Kazakh", "Khmer", "Kikongo", "Kinyarwanda", "Kirundi", "Khoekhoe", "Khoisan", "Korean", "Kwangali", "Kyrgyz", "Lao", "Latin", "Latvian", "Lingala", "Lithuanian", "Lozi", "Luxembourgish", "Macedonian", "Malagasy", "Malay", "Maldivian", "Maltese", "Manx", "Māori", "Marshallese", "Mauritian Creole", "Montenegrin", "Mongolian", "Nauru", "Nepali", "New Zealand Sign Language", "Niuean", "Norwegian", "Norwegian Bokmål", "Norwegian Nynorsk", "Norfuk", "Northern Sotho", "Palauan", "Papiamento", "Pashto", "Persian (Farsi)", "Polish", "Portuguese", "Quechua", "Romanian", "Romansh", "Russian", "Sami", "Samoan", "Sango", "Seychellois Creole", "Shona", "Sinhala", "Slovak", "Slovene", "Somali", "Sotho", "Southern Ndebele", "Southern Sotho", "Spanish", "Swahili", "Swazi", "Swedish", "Swiss German", "Tajik", "Tamil", "Tetum", "Thai", "Tigrinya", "Tok Pisin", "Tokelauan", "Tonga", "Tongan", "Tswana", "Tsonga", "Turkmen", "Turkish", "Tuvaluan", "Ukrainian", "Upper Guinea Creole", "Urdu", "Uzbek", "Venda", "Vietnamese", "Zimbabwean Sign Language", "Zulu")))
    }
    else if(input$filter == "Population"){
      return(
        tagList(numericInput("min_pop", "Minimum Population", 0, min = 0),
             numericInput("max_pop", "Maximum Population", 100, min = 0)))
    }
    else if (input$filter == "Area"){
      return(
        tagList(numericInput("min_area", "Minimum Area", 0, min =0),
                numericInput("max_area", "Maximum Area", 100, min =0)))
    }
    else{
      return(NULL)
    }
  })
  
filtered_data <- reactive({
  req(input$filter)
  if (input$filter == "Country Name") {
    #Apply get_country_info function.
    get_country_info(input$country, input$cols)
  } else if (input$filter == "Region") {
    #Apply get_region_info function.
    get_region_info(input$region, input$cols)
  } else if (input$filter == "Language") {
    #Apply get_region_info function.
    get_language_info(input$language, input$cols)
  } else if (input$filter == "Population") {
    get_pop_info(input$min_pop, input$max_pop, input$cols)
  } else if (input$filter == "Area") {
    get_area_info(input$min_area, input$max_area, input$cols)
  } else {
    NULL
  }
})  

output$filtered_data <- renderDataTable({
  filtered_data()
})




  output$download <- downloadHandler(
    filename = "countries_data.csv",
    content = function(file) {
      download_data <- filtered_data()
      write.csv(download_data, file)
    }
  )
  
  
  output$summary_out <- renderUI({
    if(input$summary == "Contingency Tables") {
      selectInput("con_tab", "Type of Contingency Table",
                  choices = c("One Way", "Two Way"))
    } else if(input$summary == "Numerical Summaries") {
      tagList(
        radioButtons("num_sum", "Choose a numeric variable", 
                     choices = c("Population", "Area")),
        radioButtons("cat_sum", "Choose a categorical variable",
                     choices = c("Independence", 
                                 "UN_Member", 
                                 "Landlocked", 
                                 "Region",
                                 "Subregion",
                                 "Car_Side_Driving"))
      )
    } else if(input$summary == "Graphical Displays"){
      selectInput("graph", "Type of Graph",
                  choices = c("Tree Map", "Histograms", "Bar Chart", "Scatterplot"))
    }
    else {
      NULL
    }
  })
  output$con1_opt <- renderUI({
    req(input$summary, input$con_tab)
    if (input$summary == "Contingency Tables" && input$con_tab == "One Way") {
      selectInput("con1_var", "Choose a variable for a One Way Contingency Table",
                  choices = c("UN_Member", "Independence", "Car_Side_Driving", "Landlocked", "Region", "Subregion"))
    } else {
      NULL
    }
  })
  
  
  contingency_table <- reactive({
    req(input$summary, input$con_tab, input$con1_var)
    if(input$summary == "Contingency Tables" & input$con_tab == "One Way"){
      con1_variable <- input$con1_var
      con_table <- as.data.frame(table(filtered_data()[[con1_variable]]))
      colnames(con_table) <- c(con1_variable, "Count")
      return(con_table)
    } else{
      NULL
    }
    })
  
  
  output$contingency_tab <- renderTable({
    if(!(input$con1_var %in% input$cols)){
      print("Please select the column you want to make a contingency table on in the Data Download Tab.")
    } else{
    contingency_table()
    }
  })
  
  output$con2_opt <- renderUI({
    req(input$summary, input$con_tab)
    if (input$summary == "Contingency Tables" && input$con_tab == "Two Way") {
      tagList(
        radioButtons("cat1_tab", "Choose your first variable", 
                     choices = c("Independence", "UN_Member", "Car_Side_Driving",
                                 "Landlocked", "Region", "Subregion")),
        radioButtons("cat2_tab", "Choose your second variable",
                     choices = c("Independence", 
                                 "UN_Member",
                                 "Car_Side_Driving",
                                 "Landlocked", 
                                 "Region",
                                 "Subregion")
        )
      )
    } else {
      NULL
    }
  })
  
  two_contingency_table <- reactive({
    req(input$summary, input$con_tab, input$cat1_tab & input$cat2_tab)
    if(input$summary == "Contingency Tables" & input$con_tab == "Two Way"){
      cat1_variable <- input$cat1_tab
      cat2_variable <- input$cat2_tab
      two_cat_data <- filtered_data()
      
      
      con_two_table <- table(two_cat_data[[cat1_variable]], two_cat_data[[cat2_variable]])
      return(con_two_table)
    } else{
      NULL
    }
  })
  
  output$contingency_two_tab <- renderTable({
    two_contingency_table()
  })
  
  numeric_summaries <- reactive({
    req(input$summary, input$num_sum, input$cat_sum)
    if(input$summary == "Numerical Summaries"){
      numeric_variable <- input$num_sum
      categorical_variable <- input$cat_sum
      dataset <- filtered_data()
      
      tibble_sum <- get_num_sum(dataset, get(numeric_variable), get(categorical_variable))
      colnames(tibble_sum)[1] <- categorical_variable
      return(tibble_sum)
      
    }
  })
  
  
  
  
  output$numeric_sum <- renderTable({
    numeric_summaries()
  })
  
  
  
  
  output$treemap_opt <- renderUI({
    req(input$summary, input$graph)
    if (input$summary == "Graphical Displays" && input$graph == "Tree Map") {
      selectInput("treemap_var", "Choose a variable for Tree Map",
                  choices = c("Population", "Area"))
    } else{
      NULL
    }
  })
  
  treemap_graph <- reactive({
    req(input$treemap_var, input$summary, input$graph)
    tree_var <- input$treemap_var
    
    if(input$summary == "Graphical Displays" && input$graph == "Tree Map" & tree_var == "Population"){
      ggplot(filtered_data(), aes(area = Population, fill = Population, label = Country_Name))+
        geom_treemap()+
        scale_fill_gradient(low = "lightpink", high = "red") +
        labs(title = "Population Treemap",
             fill = "Population") +
        geom_treemap_text(color = "black", place = "center", grow = TRUE)
    } else if(input$summary == "Graphical Displays" & input$graph == "Tree Map" & tree_var == "Area"){
      ggplot(filtered_data(), aes(area = Area, fill = Area, label = Country_Name))+
        geom_treemap()+
        scale_fill_gradient(low = "lightpink", high = "red") +
        labs(title = "Area Treemap",
             fill = "Area") +
        geom_treemap_text(color = "black", place = "center", grow = TRUE)
    } else{
      NULL
    }
  })
  
  output$tree_graph <- renderPlot({
    treemap_graph()
  })
  
  
output$scatter_opt <- renderUI({
    req(input$summary, input$graph)
    if(input$summary == "Graphical Displays" && input$graph == "Scatterplot"){
        selectInput("facet_var", "Faceting Variable",
                    choices = c("Region", 
                                "Subregion",
                                "UN_Member",
                                "Car_Side_Driving",
                                "Independence",
                                "Landlocked"))
    } else{
        NULL
      }
  })
output$facet_scatter <- renderPlot({
  if(input$summary == "Graphical Displays" & input$graph == "Scatterplot"){
    get_scatter(filtered_data(), input$facet_var)
  } else {
    NULL
  }
})

  
  
  output$hist_opt <- renderUI({
    req(input$summary, input$graph)
    if (input$summary == "Graphical Displays" && input$graph == "Histograms"){
      tagList(
        radioButtons("num_sum", "Choose a numeric variable", 
                     choices = c("Population", "Area")),
        radioButtons("cat_sum", "Choose a categorical variable to facet",
                     choices = c("Independence", 
                                 "UN_Member",
                                 "Car_Side_Driving",
                                 "Landlocked", 
                                 "Region",
                                 "Subregion"))
      )
    } else {
      NULL
    }
  })
  
  output$facet_histogram <- renderPlot({
    if(input$summary == "Graphical Displays" & input$graph == "Histograms"){
      get_histogram(filtered_data(), input$num_sum, input$cat_sum)
    } else {
      NULL
    }
  })
  
  
  
  
  output$barchart_opt <- renderUI({
    req(input$summary, input$graph)
    if(input$summary == "Graphical Displays" && input$graph == "Bar Chart"){
      tagList(
        radioButtons("cat1", "Choose your first variable", 
                     choices = c("Independence", "UN_Member", "Car_Side_Driving",
                                 "Landlocked", "Region", "Subregion")),
        radioButtons("cat2", "Choose your second variable",
                     choices = c("Independence", 
                                 "UN_Member",
                                 "Car_Side_Driving",
                                 "Landlocked", 
                                 "Region",
                                 "Subregion")
        )
      )
    } else {
      NULL
    }
  })
  
  output$bar_graph <- renderPlot({
    if(input$summary == "Graphical Displays" & input$graph == "Bar Chart"){
    get_bar(filtered_data(), input$cat1, input$cat2)
    } else {
      NULL
    }
  })
  
  
  
} 
  

  
