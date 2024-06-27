library(jsonlite)
library(httr)
library(tidyverse)
get_country_info <- function(country_name){
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
  
  return(tibble_info)
}

get_region_info <- function(region_name){
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
    
    arrange(Country_Name)
  
  return(tibble_info)
  
}

get_language_info <- function(language_name){
  #Store the base url before the inputs as start_url.
  start_url <- "https://restcountries.com/v3.1/lang/"
  #Based on documentation provided on API, paste together formatting of initial url, and inputs for what the final url should look like.. Use paste0 to not add spaces.
  url <- paste0(start_url, 
                language_name)
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
    
    arrange(Country_Name)
  
  return(tibble_info)
}

get_pop_info <- function(min_value = 0, max_value = 20000000000){
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
    arrange(Country_Name)
  
  return(tibble_info)
}


server <- function(input, output, session) {
  output$filter <- renderUI({
    if(input$filter == "Country Name"){
      return(
        selectInput("country", "Country Name", 
                    choices = c("Afghanistan", "Åland Islands", "Albania", "Algeria", "American Samoa", "Andorra", "Angola", "Anguilla", "Antarctica", "Antigua and Barbuda", "Argentina", "Armenia", "Aruba", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bermuda", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Bouvet Island", "Brazil", "British Indian Ocean Territory", "British Virgin Islands", "Brunei", "Bulgaria", "Burkina Faso", "Burundi", "Cambodia", "Cameroon", "Canada", "Cape Verde", "Carribean Netherlands", "Cayman Islands", "Central African Republic", "Chad", "Chile", "China", "Christmas Island", "Cocos (Keeling) Islands", "Colombia", "Comoros", "Cook Islands", "Costa Rica", "Croatia", "Cuba", "Curacao", "Cyprus", "Czechia", "DR Congo", "Denmark", "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equitorial Guinea", "Eritrea", "Estonia", "Eswatini", "Ethiopia", "Falkland Islands", "Faroe Islands", "Fiji", "Finland", "France", "French Guiana", "French Polynesia", "French Southern and Antarctic Lands", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Gibraltar", "Greece", "Greenland", "Grenada", "Guadeloupe", "Guam", "Guatemala", "Guernsey", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Heard Island and McDonald Islands", "Honduras", "Hong Kong", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Isle of Man", "Israel", "Italy", "Ivory Coast", "Jamaica", "Japan", "Jersey", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Macau", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Martinique", "Mauritania", "Mauritius", "Mayotte", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", "Montserrat", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", "New Caledonia", "New Zealand", "Nicaragua", "Niger", "Nigeria", "Niue", "Norfolk Island", "North Korea", "North Macedonia", "Northern Mariana Islands", "Norway", "Oman", "Pakistan", "Palau", "Palestine", "Panama", "Papau New Guinea", "Paraguay", "Peru", "Philippines", "Pitcairn Islands", "Poland", "Portugal", "Puerto Rico", "Qatar", "Republic of the Congo", "Romania", "Russia", "Rwanda", "Reunion", "Saint Barthelemy", "Saint Helena, Ascension and Tristan da Cunha", "Saint Kitts and Nevis", "Saint Lucia", "Saint Martin", "Saint Pierre and Miquelon", "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Sint Maarten", "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Georgia", "South Korea", "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Svalbard and Jan Mayen", "Sweden", "Switzerland", "Syria", "São Tomé and Príncipe", "Taiwan", "Tajikistan", "Tanzania", "Thailand", "Timor-Leste", "Togo", "Tokelau", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Turks and Caicos Islands", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "United States", "United States Minor Outlying Islands", "United States Virgin Islands", "Uruguay", "Uzbekistan", "Vanuatu", "Vatican City", "Venezuela", "Vietnam", "Wallis and Futuna", "Western Sahara", "Yemen", "Zambia", "Zimbabwe"))
      )
    } else if(input$filter == "Region"){
      return(selectInput("region", "Region Name",
                         choices = c("Africa","Antarctic", "Americas", "Asia", "Europe", "Oceania")))
    } else if(input$filter == "Language"){
      return(selectInput("language", "Language Name",
                         choices = c("Albanian", "Amharic", "Arabic", "Aramaic", "Armenian", "Azerbaijani", "Basque", "Belarusian", "Bengali", "Belizean Creole", "Bislama", "Bosnian", "Bulgarian", "Catalan", "Chamorro", "Chewa", "Chibarwe", "Chinese", "Cook Islands Māori", "Croatian", "Czech", "Danish", "Dari", "Dutch", "Dzongkha", "English", "Estonian", "Faroese", "Filipino", "Finnish", "Fijian", "Fiji Hindi", "French", "Galician", "Georgian", "German", "Gilbertese", "Greek", "Greenlandic", "Guaraní", "Guernésiais", "Haitian Creole", "Hebrew", "Herero", "Hiri Motu", "Hindi", "Hungarian", "Icelandic", "Indonesian", "Irish", "Italian", "Japanese", "Jèrriais", "Kazakh", "Khmer", "Kikongo", "Kinyarwanda", "Kirundi", "Khoekhoe", "Khoisan", "Korean", "Kwangali", "Kyrgyz", "Lao", "Latin", "Latvian", "Lingala", "Lithuanian", "Lozi", "Luxembourgish", "Macedonian", "Malagasy", "Malay", "Maldivian", "Maltese", "Manx", "Maori", "Marshallese", "Mauritian Creole", "Montenegrin", "Mongolian", "Nauru", "Nepali", "New Zealand Sign Language", "Niuean", "Norwegian", "Norwegian Bokmål", "Norwegian Nynorsk", "Norfuk", "Northern Sotho", "Palauan", "Papiamento", "Pashto", "Persian (Farsi)", "Polish", "Portuguese", "Quechua", "Romanian", "Romansh", "Russian", "Sami", "Samoan", "Sango", "Seychellois Creole", "Shona", "Sinhala", "Slovak", "Slovene", "Somali", "Sotho", "Southern Ndebele", "Southern Sotho", "Spanish", "Swahili", "Swazi", "Swedish", "Swiss German", "Tajik", "Tamil", "Tetum", "Thai", "Tigrinya", "Tok Pisin", "Tokelauan", "Tonga", "Tongan", "Tswana", "Tsonga", "Turkmen", "Turkish", "Tuvaluan", "Ukranian", "Upper Guinea Creole", "Urdu", "Uzbek", "Venda", "Vietnamese", "Zimbabwean Sign Language", "Zulu")))
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
  
  
  
  country_data <- reactive({
    req(input$filter) 
    
    if (input$filter == "Country Name") {
      #Apply get_country_info function.
      get_country_info(input$country)
    } else {
      NULL
    }
  })
  output$country_table <- renderDataTable({
    #Create data table from country_data function.
    country_data()

  })

  region_data <- reactive({
    req(input$filter) 
    
    if (input$filter == "Region") {
      #Apply get_region_info function.
      get_region_info(input$region)
    } else {
      NULL
    }
  })
  output$region_table <- renderDataTable({
    #Create data table from country_data function.
    region_data()
    
  })
  language_data <- reactive({
    req(input$filter) 
    
    if (input$filter == "Language") {
      #Apply get_region_info function.
      get_language_info(input$language)
    } else {
      NULL
    }
  })
  output$language_table <- renderDataTable({
    #Create data table from country_data function.
    language_data()
    
  })
  
  population_data <- reactive({
    req(input$filter) 
    
    if (input$filter == "Population") {
      get_pop_info(input$min_pop, input$max_pop)
    } else {
      NULL
    }
  })
  output$population_table <- renderDataTable({
    #Create data table from country_data function.
    population_data()
    
  })
  
  area_data <- reactive({
    req(input$filter) 
    
    if (input$filter == "Area") {
      get_area_info(input$min_area, input$max_area)
    } else {
      NULL
    }
  })
  output$area_table <- renderDataTable({
    #Create data table from country_data function.
    area_data()
    
  })
  
  

}