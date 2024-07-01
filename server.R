library(jsonlite)
library(httr)
library(tidyverse)
library(treemapify)

#Static functions are added up here before the server and then utilized in the server. 

#Create get_country_info function that takes in a name and then the columns you want.
get_country_info <- function(country_name, chosen_cols){
  #Store the base url before the inputs as start_url.
  start_url <- "https://restcountries.com/v3.1/"
  #Based on documentation provided on API, paste together formatting of initial url and country name. Use the URLencode below to deal with countries like United States and such that have spaces so then it will use %20.
  encoded_name <- URLencode(country_name)
  url <- paste0(start_url,
                "name/",
                encoded_name)
  #Use the GET function to contact API.
  return_data <- GET(url)
  #Parse through data using fromJSON function from jsonlite package.
  parsed_data <- fromJSON(rawToChar(return_data$content))
  #Get an exact match by making sure the name equals exactly the inputted country name. Without this, if you chose "Dominica" for example it would return both Dominica and Dominican Republic. I only want it to return one country for this section. Since I'm going to have a dropdown box for countries, spelling and such shouldn't be an issue.
  exact_match <- parsed_data$name$common == country_name
  #Convert to tibble. Return only rows where there was an exact match found. Return all columns for now. (This will be specified later).
  tibble_info <- as_tibble(parsed_data[exact_match, ])
  
  #Name the columns of the tibble.
  tibble_info <- tibble(
    Country_Name = tibble_info$name$common,
    #Deal with missing values for capital (Some in original data frame were given as NULL) causing errors. Convert to character as needed.
    Capital = ifelse(is.null(tibble_info$capital), NA, 
                     as.character(tibble_info$capital)),
    Region = tibble_info$region,
    #Deal with missing values for subregion (Some in original data frame were missing or null causing errors. Convert to character as needed.
    Subregion = tibble_info$subregion,
    Area = tibble_info$area,
    Population = tibble_info$population,
    Car_Side_Driving = tibble_info$car$side,
    #Deal with missing values for subregion (Some in original data frame were missing or null causing errors. Convert to character as needed.
    Independence = ifelse(is.null(tibble_info$independent), NA, 
                          as.character(tibble_info$independent)),
    Landlocked = tibble_info$landlocked,
    UN_Member = tibble_info$unMember)
  
  #Write over the tibble_info by selecting the Country_Name column always and any chosen columns. 
  tibble_info <- tibble_info |>
    select(Country_Name, chosen_cols)
  
  return(tibble_info)
}

#Create a get region info function that takes in a region name and columns and returns all countries from that region.
get_region_info <- function(region_name, chosen_cols){
  #Store the base url before the inputs as start_url.
  start_url <- "https://restcountries.com/v3.1/region/"
  #Based on documentation provided on API, paste together formatting of initial url and region name.
  url <- paste0(start_url, 
                region_name)
  #Use the GET function to contact API.
  return_data <- GET(url)
  #Parse through data using fromJSON function from jsonlite package.
  parsed_data <- fromJSON(rawToChar(return_data$content))
  #Convert the information to a tibble.
  tibble_info <- as_tibble(parsed_data)
  
  #Name the columns of the tibble.
  tibble_info <- tibble(
    Country_Name = tibble_info$name$common,
    Capital = tibble_info$capital,
    Region = tibble_info$region,
    Subregion = tibble_info$subregion,
    Area = tibble_info$area,
    Population = tibble_info$population,
    Car_Side_Driving = tibble_info$car$side,
    Independence = tibble_info$independent,
    Landlocked = tibble_info$landlocked,
    UN_Member = tibble_info$unMember) |>
    
    #Write over the tibble_info by selecting the Country_Name column always and any chosen columns. Put in order alphabetically to make easier to find certain countries.
    arrange(Country_Name) |>
    select(Country_Name, chosen_cols)
  
  return(tibble_info)
  
}

get_subregion_info <- function(subregion_name, chosen_cols){
  #Store the base url before the inputs as start_url.
  start_url <- "https://restcountries.com/v3.1/subregion/"
  #Based on documentation provided on API, paste together formatting of initial url and region name.
  encoded_sub_name <- URLencode(subregion_name)
  url <- paste0(start_url, 
                encoded_sub_name)
  #Use the GET function to contact API.
  return_data <- GET(url)
  #Parse through data using fromJSON function from jsonlite package.
  parsed_data <- fromJSON(rawToChar(return_data$content))
  #Convert the information to a tibble.
  tibble_info <- as_tibble(parsed_data)
  
  #Name the columns of the tibble.
  tibble_info <- tibble(
    Country_Name = tibble_info$name$common,
    Capital = tibble_info$capital,
    Region = tibble_info$region,
    Subregion = tibble_info$subregion,
    Area = tibble_info$area,
    Population = tibble_info$population,
    Car_Side_Driving = tibble_info$car$side,
    Independence = tibble_info$independent,
    Landlocked = tibble_info$landlocked,
    UN_Member = tibble_info$unMember) |>
    
    #Write over the tibble_info by selecting the Country_Name column always and any chosen columns. Put in order alphabetically to make easier to find certain countries.
    arrange(Country_Name) |>
    select(Country_Name, chosen_cols)
  
  return(tibble_info)
  
}

#Create a get language info function that takes in a language name and columns and returns all countries from that speak a particular language.
get_language_info <- function(language_name, chosen_cols){
  #Store the base url before the inputs as start_url.
  start_url <- "https://restcountries.com/v3.1/lang/"
  #Encode in the case of spaces or special characters that appeared in the names of some languages.
  encode_lang <- URLencode(language_name)
  #Based on documentation provided on API, paste together formatting of initial url and region name.
  url <- paste0(start_url, 
                encode_lang)
  #Use the GET function to contact API.
  return_data <- GET(url)
  #Parse through data using fromJSON function from jsonlite package.
  parsed_data <- fromJSON(rawToChar(return_data$content))
  #Convert information to a tibble.
  tibble_info <- as_tibble(parsed_data)
  
  
  #Name the columns of the tibble.
  tibble_info <- tibble(
    Country_Name = tibble_info$name$common,
    Capital = tibble_info$capital,
    Region = tibble_info$region,
    Subregion = tibble_info$subregion,
    Area = tibble_info$area,
    Population = tibble_info$population,
    Car_Side_Driving = tibble_info$car$side,
    Independence = tibble_info$independent,
    Landlocked = tibble_info$landlocked,
    UN_Member = tibble_info$unMember) |>
    
    #Put in order alphabetically to make easier to find certain countries.
    arrange(Country_Name) |>
    #Select the name always and then the other chosen columns.
    select(Country_Name, chosen_cols)
  
  return(tibble_info)
}

#Create a population info function. At this endpoint, the user can modify two different things: the min and the max. By doing this, the user can create a range of values and get all countries within that population range.
get_pop_info <- function(min_value = 0, max_value = 20000000000, chosen_cols){
  start_url <- "https://restcountries.com/v3.1/all/"
  #Contact API.
  return_data <- GET(start_url)
  #Parse through data using fromJSON function from jsonlite package. Convert information to a tibble.
  parsed_data <- fromJSON(rawToChar(return_data$content))
  tibble_info <- as_tibble(parsed_data)
  
  #Name the columns of the tibble.
  tibble_info <- tibble(
    Country_Name = tibble_info$name$common,
    Capital = tibble_info$capital,
    Region = tibble_info$region,
    Subregion = tibble_info$subregion,
    Area = tibble_info$area,
    Population = tibble_info$population,
    Car_Side_Driving = tibble_info$car$side,
    Independence = tibble_info$independent,
    Landlocked = tibble_info$landlocked,
    UN_Member = tibble_info$unMember)
  
  tibble_info <- tibble_info |>
    #Only get those rows that that are greater than or equal to the minimum value and less than or equal to the maximum value.
    filter(Population >= min_value & Population <= max_value) |>
    #Put in order alphabetically to make easier to find certain countries.
    arrange(Country_Name) |>
    #Select name column and any other columns specified.
    select(Country_Name, chosen_cols)
  
  return(tibble_info)
}


#Create an area info function. At this endpoint, the user can modify two different things: the min and the max. By doing this, the user can create a range of values and get all countries within that area range.
get_area_info <- function(min_area = 100, max_area = 100000, chosen_cols){
  #Store the base url before the inputs as start_url.
  start_url <- "https://restcountries.com/v3.1/all/"
  #Use the GET function to contact API.
  return_data <- GET(start_url)
  #Parse through data using fromJSON function from jsonlite package.
  parsed_data <- fromJSON(rawToChar(return_data$content))
  #Convert the article information to a tibble.
  tibble_info <- as_tibble(parsed_data)
  
  #Rename columns in the tibble.
  tibble_info <- tibble(
    Country_Name = parsed_data$name$common,
    Capital = as.character(parsed_data$capital),
    Region = parsed_data$region,
    Subregion = tibble_info$subregion,
    Area = parsed_data$area,
    Population = parsed_data$population,
    Car_Side_Driving = parsed_data$car$side,
    Independence = parsed_data$independent,
    Landlocked = parsed_data$landlocked,
    UN_Member = parsed_data$unMember)
  
  tibble_info <- tibble_info |>
    #Only get those rows that that are greater than or equal to the minimum value and less than or equal to the maximum value.
    filter(Area >= min_area & Area <= max_area) |>
    arrange(Country_Name) |>
    #Order alphabetically by name and only select name column and any other columns specified.
    select(Country_Name, chosen_cols)
  
  return(tibble_info)
}

#Create a get bar function. This function takes in a dataset and two categorical variables.
get_bar <- function(data_input, cat1, cat2){
  #Create initial plotting instance by using ggplot. Drop any values where either categorical variable is missing. Use aes_string since the the variables are being passed as strings. 
  ggraph <- ggplot(data_input |> drop_na(cat2) |> drop_na(cat1), aes_string(x = cat1, fill = cat2))
  #Add a geom bar layer.
  ggraph + geom_bar() +
    #Adjust the x axis labels becuase they were originally going on top of each other so adjust vertically so that you can see them clearly.
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    #Add labels. Dynamically get x axis label as the cat1 variable and the y axis will always represent the count. The title is dynamically generated by using cat1 and cat2 and the paste0 function.
    labs(x = cat1,
         y = "Count",
         title = paste0("Countries by ", cat1, " and ", cat2))
}


#Create a get histogram function. This function takes in a dataset, a quantitative variable, and a categorical variable.

get_histogram <- function(input_data, x_val, y_val){
  #Create plotting instance and drop any missing y val (categorical variable). Use aes_string since the inputs are character strings.
  g <- ggplot(input_data |> drop_na(y_val), aes_string(x = x_val, fill = y_val)) +
    #Add a geom_histogram layer with 20 bins.
    geom_histogram(bins = 20) +
    #Facet wrap on the categorical varialbe to create all possible graphs of the quantitative variable at all levels of the categorical variable.
    facet_wrap(y_val) +
    #Generate the legend with the categorical variable.
    scale_fill_discrete(name = y_val) +
    #Get rid of the scientific notation because it looked terrible and confusing.
    scale_x_continuous(labels = scales::number_format()) +
    #Adjust the x axis labels again because they were overlapping each other.
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    #Dynamically generate x axis label and dynamically create title. Label y axis as Count.
    labs(x = x_val,
         y = "Count",
         title = paste0(x_val, " by ", y_val))
  
  print(g)
}

#Create a get_scatter function that takes in a dataset and a categorical variable as the faceting variable.
get_scatter <- function(input_data, facet_var){
  #Drop any missing variables for the facet_var. Use aes_string since input is categorical. x and y are static as population and area since those are the only quantitative variables. Color by the categorical variable. 
  g <- ggplot(input_data |> drop_na(facet_var), aes_string(x = "Population", y = "Area", color = facet_var)) +
    #Add a scatterplot layer.
    geom_point() +
    #Add a linear regression over top of the points.
    geom_smooth(method = lm) +
    #Create all scatterplots at all levels of the faceting variable.
    facet_wrap(facet_var) +
    #Dynamically generate title by using the name of the facet_var.
    labs(title = paste0("Relationship between Population and Area based on ", facet_var))
  print(g)
}

#get_num_sum generates numeric summaries by taking in a dataset, a numeric variable, and a categorical variable.
get_num_sum <- function(dataframe, num_variable, cat_variable){
  dataframe |>
    #Group by the categorical variable.
    group_by({{cat_variable}}) |>
    #Create a summary table for the  numeric variable. This will respect the grouping made above. Report the mean, standard deviation, minimum, first quartile, median, third quartile, and maximum. Use the argument na.rm = TRUE to remove any missing values.
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



#Start the server function.

server <- function(input, output, session) {
  
  #choice looks at the value of the input$filter passed through from the UI and looks at some if/else logic. Use renderUI to make this dynamic and pass this back through to the UI with uiOutput.
  output$choice <- renderUI({
    if(input$filter == "Country Name"){
      #If the "Choose Filtering Method Box" (internally stored as filter) is selected as country name, then display another dropdown box for selecting the country name. I chose to use this rather than text input to avoid any user issues with spelling and special characters. Have the display name be "Country Name" and store internally as country.
        selectInput("country", "Country Name", 
                    choices = c("Afghanistan", "Åland Islands", "Albania", "Algeria", "American Samoa", "Andorra", "Angola", "Anguilla", "Antarctica", "Antigua and Barbuda", "Argentina", "Armenia", "Aruba", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bermuda", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Bouvet Island", "Brazil", "British Indian Ocean Territory", "British Virgin Islands", "Brunei", "Bulgaria", "Burkina Faso", "Burundi", "Cambodia", "Cameroon", "Canada", "Cape Verde", "Caribbean Netherlands", "Cayman Islands", "Central African Republic", "Chad", "Chile", "China", "Christmas Island", "Cocos (Keeling) Islands", "Colombia", "Comoros", "Cook Islands", "Costa Rica", "Croatia", "Cuba", "Curaçao", "Cyprus", "Czechia", "DR Congo", "Denmark", "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini", "Ethiopia", "Falkland Islands", "Faroe Islands", "Fiji", "Finland", "France", "French Guiana", "French Polynesia", "French Southern and Antarctic Lands", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Gibraltar", "Greece", "Greenland", "Grenada", "Guadeloupe", "Guam", "Guatemala", "Guernsey", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Heard Island and McDonald Islands", "Honduras", "Hong Kong", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Isle of Man", "Israel", "Italy", "Ivory Coast", "Jamaica", "Japan", "Jersey", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Macau", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Martinique", "Mauritania", "Mauritius", "Mayotte", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", "Montserrat", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", "New Caledonia", "New Zealand", "Nicaragua", "Niger", "Nigeria", "Niue", "Norfolk Island", "North Korea", "North Macedonia", "Northern Mariana Islands", "Norway", "Oman", "Pakistan", "Palau", "Palestine", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Pitcairn Islands", "Poland", "Portugal", "Puerto Rico", "Qatar", "Republic of the Congo", "Romania", "Russia", "Rwanda", "Réunion", "Saint Barthélemy", "Saint Helena, Ascension and Tristan da Cunha", "Saint Kitts and Nevis", "Saint Lucia", "Saint Martin", "Saint Pierre and Miquelon", "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Sint Maarten", "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Georgia", "South Korea", "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Svalbard and Jan Mayen", "Sweden", "Switzerland", "Syria", "São Tomé and Príncipe", "Taiwan", "Tajikistan", "Tanzania", "Thailand", "Timor-Leste", "Togo", "Tokelau", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Turks and Caicos Islands", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "United States", "United States Minor Outlying Islands", "United States Virgin Islands", "Uruguay", "Uzbekistan", "Vanuatu", "Vatican City", "Venezuela", "Vietnam", "Wallis and Futuna", "Western Sahara", "Yemen", "Zambia", "Zimbabwe"))
    #If the "Choose Filtering Method Box" (internally stored as filter) is selected as Region, then display another dropdown box for selecting the region name. Have the display name be "Region Name" and store internally as region.
    } else if(input$filter == "Region"){
      selectInput("region", "Region Name",
                         choices = c("Africa","Antarctic", "Americas", "Asia", "Europe", "Oceania"))
    } else if(input$filter == "Subregion"){
      selectInput("subregion", "Subregion Name",
                  choices = c("Northern Africa", "Eastern Africa", "Southern Africa", "Middle Africa", "Western Africa", "Caribbean", "South America", "Central America", "North America", "Southern Asia", "Western Asia", "South-Eastern Asia", "Eastern Asia", "Central Asia", "Southeast Europe", "Southern Europe", "Central Europe", "Eastern Europe", "Western Europe", "Northern Europe", "Polynesia", "Australia and New Zealand", "Melanesia", "Micronesia"))
      #If the "Choose Filtering Method Box" (internally stored as filter) is selected as Language, then display another dropdown box for selecting the language name. Again I chose to type them all out and use a dropdown box to avoid issues with special characters and spelling. Have the display name be "Language Name" and store internally as language.
    } else if(input$filter == "Language"){
      selectInput("language", "Language Name",
                         choices = c("Albanian", "Amharic", "Arabic", "Aramaic", "Armenian", "Azerbaijani", "Basque", "Belarusian", "Bengali", "Belizean Creole", "Bislama", "Bosnian", "Bulgarian", "Catalan", "Chamorro", "Chewa", "Chibarwe", "Chinese", "Cook Islands Māori", "Croatian", "Czech", "Danish", "Dari", "Dutch", "Dzongkha", "English", "Estonian", "Faroese", "Filipino", "Finnish", "Fijian", "Fiji Hindi", "French", "Galician", "Georgian", "German", "Gilbertese", "Greek", "Greenlandic", "Guaraní", "Guernésiais", "Haitian Creole", "Hebrew", "Herero", "Hiri Motu", "Hindi", "Hungarian", "Icelandic", "Indonesian", "Irish", "Italian", "Japanese", "Jèrriais", "Kazakh", "Khmer", "Kikongo", "Kinyarwanda", "Kirundi", "Khoekhoe", "Khoisan", "Korean", "Kwangali", "Kyrgyz", "Lao", "Latin", "Latvian", "Lingala", "Lithuanian", "Lozi", "Luxembourgish", "Macedonian", "Malagasy", "Malay", "Maldivian", "Maltese", "Manx", "Māori", "Marshallese", "Mauritian Creole", "Montenegrin", "Mongolian", "Nauru", "Nepali", "New Zealand Sign Language", "Niuean", "Norwegian", "Norwegian Bokmål", "Norwegian Nynorsk", "Norfuk", "Northern Sotho", "Palauan", "Papiamento", "Pashto", "Persian (Farsi)", "Polish", "Portuguese", "Quechua", "Romanian", "Romansh", "Russian", "Sami", "Samoan", "Sango", "Seychellois Creole", "Shona", "Sinhala", "Slovak", "Slovene", "Somali", "Sotho", "Southern Ndebele", "Southern Sotho", "Spanish", "Swahili", "Swazi", "Swedish", "Swiss German", "Tajik", "Tamil", "Tetum", "Thai", "Tigrinya", "Tok Pisin", "Tokelauan", "Tonga", "Tongan", "Tswana", "Tsonga", "Turkmen", "Turkish", "Tuvaluan", "Ukrainian", "Upper Guinea Creole", "Urdu", "Uzbek", "Venda", "Vietnamese", "Zimbabwean Sign Language", "Zulu"))
      #If the "Choose Filtering Method Box" (internally stored as filter) is selected as Population, then display two numerical input boxes titled Minimum Population and Maximum Population. Start the minimum at 0 and the maximum at 100. Make the minimum for both 0 because you can't have negative people! (Well I guess technically there are negative people, but still). Store them internally as min_pop and max_pop.
    } else if(input$filter == "Population"){
        tagList(numericInput("min_pop", "Minimum Population", 0, min = 0),
             numericInput("max_pop", "Maximum Population", 100, min = 0))
      #If the "Choose Filtering Method Box" (internally stored as filter) is selected as Population, then display two numerical input boxes titled Minimum Area and Maximum Area. Start the minimum at 0 and the maximum at 100. Make the minimum for both 0 because we don't have negative area! Store them internally as min_area and max_area.
    } else if (input$filter == "Area"){
        tagList(numericInput("min_area", "Minimum Area", 0, min =0),
                numericInput("max_area", "Maximum Area", 100, min =0))
    }
    #If none of the other conditions apply, return NULL. This shouldn't really happen though becasue a box is always selected. Just in case though. 
    else{
      return(NULL)
    }
  })
  
  #Create a reactive context for filtered_data. This will allow us to apply different functions based on the filter selected (Country Name, Region, Language, Population, or Area) and then take the inputs of those (say the name of the country for example) and use those as the inputs of the functions that we originally created. We can then store this dataset to render a data table (stay tuned!) and also use this on the other tab do make graphs and such. All of them also have input$cols as the last input. This comes from the UI file where we selected the columns with a checkbox and then we are passing those selections through here.
filtered_data <- reactive({
  #Require that the filter was selected.
  req(input$filter)
  if (input$filter == "Country Name") {
    #If country name was selected, apply get_country_info function. Use the Country Name (internally stored as country) as the first input.
    get_country_info(input$country, input$cols)
  } else if (input$filter == "Region") {
    #If Region was selected, apply get_region_info function. Use the Region Name (internally stored as region) as the first input.
    get_region_info(input$region, input$cols)
  } else if(input$filter == "Subregion") {
    get_subregion_info(input$subregion, input$cols)
  } else if (input$filter == "Language") {
    #If Language was selected, apply get_language_info function. Use the Language Name (internally stored as language) as the first input.
    get_language_info(input$language, input$cols)
  } else if (input$filter == "Population") {
    #If Population was selected, apply get_pop_info function. Use the minimum & maximum (internally stored as min_pop and max_pop from above) as the first two inputs.
    get_pop_info(input$min_pop, input$max_pop, input$cols)
  } else if (input$filter == "Area") {
    #If Area was selected, apply get_area_info function. Use the minimum & maximum (internally stored as min_area and max_area from above) as the first two inputs.
    get_area_info(input$min_area, input$max_area, input$cols)
    #Catch all.
  } else {
    NULL
  }
})  


#Create an output called data_table that runs the filtered_data function from above and renders a data Table. This is then actually produced using the dataTableOutput in the UI file.
output$data_table <- renderDataTable({
  filtered_data()
})

#Create the actual download by using downloadHandler. Didn't know about this functionality before but this is what help("DownloadHandler") says about it: Allows content from the Shiny application to be made available to the user as file downloads (for example, downloading the currently visible data as a CSV file). Name the file countries_data.csv. Call the filtered_data() function as the dataset in a write.csv function to actually create the csv file.

output$download <- downloadHandler(
    filename = "countries_data.csv",
    content = function(file) {
      download_data <- filtered_data()
      write.csv(download_data, file)
    }
  )

#Moving onto output for the Data Exploration Tab. This one is called summary_out. This is passed back to the UI through the UI output. renderUI creates a reactive context to allow it to be dynamic. 

  output$summary_out <- renderUI({
#There is an initial dropdown box called "Summary Type" that is internally stored as summary. If summary is on "Contingency Tables", get a second drop down box named "Type of Contingency Table" that is internally stored as con_tab. Give options for a one way or two way contingency table.    
    if(input$summary == "Contingency Tables") {
      selectInput("con_tab", "Type of Contingency Table",
                  choices = c("One Way", "Two Way"))
    #If summary is set to "Numerical Summaries", display two choices with radio Buttons (basically just multiple choice) - so that the user can choose both a quantitative to create the summary and categorical variable to group by. tagList used to create two outputs. Internally store these inputs as num_sum and cat_sum.
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
 #If summary is set to "Graphical Displays", display another dropdown box with options for the choices for the graphical displays. Call this dropdown box "Type of Graph" and internally store as graph.
    } else if(input$summary == "Graphical Displays"){
      selectInput("graph", "Type of Graph",
                  choices = c("Tree Map", "Histograms", "Bar Chart", "Scatterplot"))
    }
    #Catch all.
    else {
      NULL
    }
  })
  
  #Create a dynamic UI element called con1_opt which is the created options for a one-way table if the user selects both Contingency Tables and One Way.
  output$con1_opt <- renderUI({
    #Require inputs for summary and con_tab
    req(input$summary, input$con_tab)
    #If user selects both of the following, creat a dropdown box for the variable to create the table for. Internally store this variable as con1_var to use when actually making the table next.
    if (input$summary == "Contingency Tables" && input$con_tab == "One Way") {
      selectInput("con1_var", "Choose a variable for a One Way Contingency Table",
                  choices = c("UN_Member", "Independence", "Car_Side_Driving", "Landlocked", "Region", "Subregion"))
    } else {
      NULL
    }
  })
  
  #Create the contingency table if user has chosen input for summary and chosen one way contingency table. Use a reactive context.
  contingency_table <- reactive({
    req(input$summary, input$con_tab, input$con1_var)
    if(input$summary == "Contingency Tables" & input$con_tab == "One Way"){
      #Store the variable that the user chose in the dropdown box as con1_variable
      con1_variable <- input$con1_var
      #Create a data frame using the base R table function. $ didn't work and stack overflow suggested [[ ]] for dynamic elements so that worked! Use the filtered_data function from before to use the data that was already created.
      con_table <- as.data.frame(table(filtered_data()[[con1_variable]]))
      #Give meaningful column names because true/false is not helpful.
      colnames(con_table) <- c(con1_variable, "Count")
      return(con_table)
    } else{
      NULL
    }
    })
  
  #This actually renders the table with the combination of tableOutput in the UI file.
  output$contingency_tab <- renderTable({
    #Print a message that tells the user to go back to the data download tab and select the columns if they haven't rather than giving an error. You can't make a contingency table on region for example if you don't have the region column!
    if(!(input$con1_var %in% input$cols)){
      print("Please select the column you want to make a contingency table on in the Data Download Tab.")
    #If the variable is in the column selected, it's good to go and you can generate the contingency table. This goes with the tableOuptut("contingency_tab") in the UI file.
    } else{
    contingency_table()
    }
  })
  
  #Onto two way contingency tables! Let's make a dynamic UI element called con2_opt - this is actually generated using uiOutput in the UI file. 
  output$con2_opt <- renderUI({
    #Require inputs of summary and con_tab.
    req(input$summary, input$con_tab)
    #If these inputs are selected, then create two multiple choice options for two categorical variables. Don't give them the option of quantitative variables because that doesn't work with contingency tables! tagList used for multiple outputs.
    if (input$summary == "Contingency Tables" & input$con_tab == "Two Way") {
      tagList(
        #First option internally stored as cat1_tab and labeled as "Choose your first variable".
        radioButtons("cat1_tab", "Choose your first variable", 
                     choices = c("Independence", "UN_Member", "Car_Side_Driving",
                                 "Landlocked", "Region", "Subregion")),
        #Second option internally stored as cat2_tab and labeled as "Choose your second variable."
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
  
  #Create the table in a reactive context as long as the user has chosen all of those inputs.
  two_contingency_table <- reactive({
    req(input$summary, input$con_tab, input$cat1_tab, input$cat2_tab)
    if(input$summary == "Contingency Tables" && input$con_tab == "Two Way"){
      #Take the first categorical variable selected and store that as cat1_variable and take the second categorical variable selected and store that as cat2_variable. Take the created dataset from before and store as two_cat_data. You could technically just pass through with the input$ but there's a lot going on here so that's why I stored.
      cat1_variable <- input$cat1_tab
      cat2_variable <- input$cat2_tab
      two_cat_data <- filtered_data()
      
      #Overwrite the dataset by doing the following. Using the tidyverse method for creating two way contingency tables.
      two_cat_data <- two_cat_data |>
        #Drop any missing values for the categorical variables so you don't get a weird row or column called NA. Use !!sym because nothing else was working and Dr. Post suggested this in the project description.
        drop_na(!!sym(cat1_variable), !!sym(cat2_variable)) |>
        #Group by both variables.
        group_by(!!sym(cat1_variable), !!sym(cat2_variable)) |>
        #Count the number in each category.
        summarize(count = n()) |>
        #Need it easier to look at an interpret so pivot_wider so it doesn't have so many different categories. Take the names from the second categorical variable and the values from the new count column created.
        pivot_wider(names_from = !!sym(cat2_variable), values_from = count)
      #Give more meaningful names for the logical elements that are either true or false. Put the first if statement so that it doesn't throw an error if there are only two columns - for example if all countries in your dataset are landlocked. If that happens just figure out the column names by looking at your radio buttons. :) 
      if(ncol(two_cat_data) == 3){
        if(cat2_variable == "Independence"){
        colnames(two_cat_data) <- c(cat1_variable, "Not Independent", "Independent")
      } else if(cat2_variable == "UN_Member"){
        colnames(two_cat_data) <- c(cat1_variable, "Not UN Member", "UN Member")
      } else if(cat2_variable == "Landlocked"){
        colnames(two_cat_data) <- c(cat1_variable, "Not Landlocked", "Landlocked")
      }
      }
      return(two_cat_data)
    } else{
      NULL
    }
  })
  
  #Actually render the table by calling two_contingency_table(). This goes with tableOutput("contingency_two_tab") in the UI output.
  
  output$contingency_two_tab <- renderTable({
      two_contingency_table()
  })
  
  
  #Now for numeric summaries! If numerical summaries is selected, this will call the get_num_sum function above to generate some numerical summaries at each grouping of a categorical variable.
  numeric_summaries <- reactive({
    req(input$summary, input$num_sum, input$cat_sum)
    if(input$summary == "Numerical Summaries"){
      #Store the input$num_sum, input$cat_sum, and filtered_data() to make it a bit cleaner.
      numeric_variable <- input$num_sum
      categorical_variable <- input$cat_sum
      dataset <- filtered_data()
      #Drop any missing values for either the categorical or quantitative variables.
      dataset <- dataset |>
        drop_na(numeric_variable) |> 
        drop_na(categorical_variable)
      
      #Apply the get_num_sum function. Use get() to access the variables since they are being passed as a character string.
      tibble_sum <- get_num_sum(dataset, get(numeric_variable), get(categorical_variable))
      #Assign the first column name to be the categorical variable to make the grouping more clear.
      colnames(tibble_sum)[1] <- categorical_variable
      return(tibble_sum)
      
    }
  })
  
#Actually render the table by calling numeric_summaries() from above and pair it with a tableOutput in the UI file.
  output$numeric_sum <- renderTable({
    numeric_summaries()
  })
  
  
#If Tree Map is selected from the graphical display, then create a dropdown box to select the type of variable.
  output$treemap_opt <- renderUI({
    req(input$summary, input$graph)
    if (input$summary == "Graphical Displays" && input$graph == "Tree Map") {
      #Call this selection "treemap_var" internally and label as "Choose a variable for Tree Map"
      tagList(
      selectInput("treemap_var", "Choose a variable for Tree Map",
                  choices = c("Population", "Area")),
      selectInput("cat_tree", "Choose a categorical variable to facet with",
                  choices = c("Independence", 
                                 "UN_Member",
                                 "Car_Side_Driving",
                                 "Landlocked", 
                                 "Region",
                                 "Subregion"))
      )
    } else{
      NULL
    }
  })
  
  
  #Create the graph in a reactive context. Use if/else logic based on the selection for treemap_var.
  treemap_graph <- reactive({
    req(input$treemap_var, input$summary, input$graph)
    #Store the choice from the dropdown box as tree_var.
    tree_var <- input$treemap_var
    cat_var <- input$cat_tree
    
    #If population is selected, initialize a plot where the area (size of rectangle) and fill (color) is connected to population. I chose to do this all with one quantitative variable even though you can do it with multiple because I felt it was easier to see the difference and focus on one variable. The story of the data became a bit more murky with too much going on. 
    if(input$summary == "Graphical Displays" && input$graph == "Tree Map" & tree_var == "Population"){
      ggplot(filtered_data() |> drop_na(cat_var), aes(area = Population, fill = Population, label = Country_Name))+
        #Add a tree map layer. Note that this is from the treemapify package.
        geom_treemap()+
        #Create the coloring scheme. Low populations will be lighter with higher values being darker.
        scale_fill_gradient(low = "lightpink", high = "red") +
        #Create a title and define the key as filling by Population as well. 
        labs(title = paste0("Population Treemap by ", cat_var),
             fill = "Population") +
        #Change color of the text to black to make it easier to see. Place it in the center. grow= TRUE means it grows with the size of the rectangle.
        geom_treemap_text(color = "black", place = "center", grow = TRUE) +
        facet_wrap(cat_var)
      #If area is selected, initialize a plot where the area (size of rectangle) and fill (color) is connected to area.
    } else if(input$summary == "Graphical Displays" & input$graph == "Tree Map" & tree_var == "Area"){
      ggplot(filtered_data() |> drop_na(cat_var), aes(area = Area, fill = Area, label = Country_Name))+
        #Add a tree map layer. 
        geom_treemap()+
        #Create the coloring scheme. Low areas will be lighter with higher values being darker.
        scale_fill_gradient(low = "lightpink", high = "red") +
        #Create a title and define the key as filling by Area as well. 
        labs(title = paste0("Area Treemap by ", cat_var),
             fill = "Area") +
        #Change color of the text to black to make it easier to see. Place it in the center. grow= TRUE means it grows with the size of the rectangle.
        geom_treemap_text(color = "black", place = "center", grow = TRUE) +
        facet_wrap(cat_var)
    } else{
      NULL
    }
  })
  
  #Actually render the plot by calling treemap_graph. Pair this with plotOutput() in the UI file to actually make the plot. Pass through with the name tree_graph.
  output$tree_graph <- renderPlot({
    treemap_graph()
  })
  
  
#If scatterplot is chosen as the Graphical Display option, provide additional options to create a faceting variable for the scatterplot.
output$scatter_opt <- renderUI({
    req(input$summary, input$graph)
    if(input$summary == "Graphical Displays" && input$graph == "Scatterplot"){
      #Store this value internally as facet_var. Label with "Faceting Variable."
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

#Actually create the plot (paired with plotOutput) by calling the get_scatter function in the renderPlot. The x and y being used are population and area because those are the only two quantitative variables. Create all combinations at each level of the faceting variable selected. 
output$facet_scatter <- renderPlot({
  if(input$summary == "Graphical Displays" & input$graph == "Scatterplot"){
    #Call the get_scatter function. Use the filtered_data() that was stored and the facet variable chosen.
    get_scatter(filtered_data(), input$facet_var)
  } else {
    NULL
  }
})


#Create the options for the histogram if the "Histograms" option was selected.   
  output$hist_opt <- renderUI({
    req(input$summary, input$graph)
    if (input$summary == "Graphical Displays" && input$graph == "Histograms"){
      tagList(
        #Have two radio Buttons of options for a numeric variable and a quantitative variable. Internally store these as num_hist and cat_hist with display names of "Choose a numeric variable" and "Choose a categorical variable to facet".
        radioButtons("num_hist", "Choose a numeric variable", 
                     choices = c("Population", "Area")),
        radioButtons("cat_hist", "Choose a categorical variable to facet",
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
  
  #Actaully render the plot (with the plotOutput in the UI file) if those selections are chosen.
  output$facet_histogram <- renderPlot({
    if(input$summary == "Graphical Displays" & input$graph == "Histograms"){
      #Call the get_histogram function. Use the stored filtered_data() dataset and the options chosen with the radio buttons for the numeric and categorical variables.
      get_histogram(filtered_data(), input$num_hist, input$cat_hist)
    } else {
      NULL
    }
  })
  
  
  #Create a dynamic UI element called barchart_opt (pair with uiOutput in the UI file) that allows options for the bar chart if bar chart is selected.  
  output$barchart_opt <- renderUI({
    req(input$summary, input$graph)
    if(input$summary == "Graphical Displays" && input$graph == "Bar Chart"){
      tagList(
        #Give two radio buttons internally called cat1 and cat2 to give options for the two categorical variables to create a stacked bar chart in the renderPlot below.
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
  
  #Use renderPlot (and plotOutput in the UI file) to actually create the stacked bar chart.
  output$bar_graph <- renderPlot({
    if(input$summary == "Graphical Displays" & input$graph == "Bar Chart"){
      #Use the stored filtered_data() dataset and utilize the two options chosen by the radio buttons in the element above. Call the get_bar() function created above.
    get_bar(filtered_data(), input$cat1, input$cat2)
    } else {
      NULL
    }
  })
  
  
  
} 
  

  
