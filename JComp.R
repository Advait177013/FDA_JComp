#covid dataset from OurWorldInData
{
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(VIM)
  library(mice)
  library(ggplot2)
}
#importing full set and filtering plus cleaning
{
  {
    full_set <- read.csv("C:\\Users\\advai\\Fall 22-23\\Data\\JComp\\owid-covid-data.csv", header=TRUE)
    nrow(full_set)
    ncol(full_set)
    #checking all columns in the dataset, so that we can remove data we assume as irrelevant
    colnames(full_set)
    #country-wise listing
    unique(full_set$location)
    class(full_set$date)
    #converting to date format to enable easy filtering
    dateFormat<-as.Date(full_set$date)
    class(dateFormat)
    full_set$date <- dateFormat
    sum(is.na(full_set))
  }
  #dropping unnecessary cols
  {
  colnames(full_set)
  keeps = c("iso_code", "location", "date", "total_cases", "total_deaths", "total_cases_per_million", "total_deaths_per_million", "total_tests", "total_tests_per_thousand", "positive_rate", "tests_per_case", "people_vaccinated","people_fully_vaccinated", "people_fully_vaccinated_per_hundred", "population", "population_density", "median_age", "gdp_per_capita", "human_development_index")
  min_full_set <- full_set[keeps]
  sum(is.na(min_full_set))
  }
  
  #immediate drop of over 5 million NA values as seen above
  {
    nrow(min_full_set)
    summary(min_full_set)
    country_list <- unique(min_full_set$location)
  }
  #md.pattern(min_full_set[, c(12:13)])
  
  #locations where over 40% of all values are NA, can prompt to check whats wrong
  {
    length(country_list)
    country_list[244]
    for(i in c(1:244))
    {
      country_check <- min_full_set[min_full_set$location==country_list[i], ]
      a <- (nrow(country_check)*19)
      b <- (sum(is.na(country_check[, c(0:18)])))
      if(b/a > 0.4)
        print(country_list[i])
    }
  }
  
  #making three subsets according to the given data, by income,region,country
  {
    {a<- min_full_set %>%
      filter(substr(iso_code, 1, 4) == "OWID")
    unique(a$iso_code)
    }
    
    drop_locations_by_region <- c("International", "World", "Africa", "South America", "North America", "Europe", "European Union", "Asia", "Oceania")
    min_full_set_by_region <- min_full_set[min_full_set$location %in% drop_locations_by_region, ]
    unique(min_full_set_by_region$location)
     
    drop_locations_by_income <- c("High income", "Upper middle income", "Lower middle income", "Low income")
    min_full_set_by_income <- min_full_set[min_full_set$location %in% drop_locations_by_income, ]
    unique(min_full_set_by_income$location)
    
    drop_locations_by_country <- c(drop_locations_by_income, drop_locations_by_region)
    min_full_set_by_country <- min_full_set[!min_full_set$location %in% drop_locations_by_country, ]
    unique(min_full_set_by_country$location)
    country_real_list<-unique(min_full_set_by_country$location)
  }  
  
  #function to fill the sets
  updown_filler<- function(myset){
    c = myset[0, ]
    loc_list <- unique(myset$location)
    for(i in c(1:length(loc_list)))
    {
      a <- myset %>%
        filter(location==loc_list[i])
      
      a <- myset %>%
        filter(location==loc_list[i]) %>%
        fill(people_vaccinated, people_fully_vaccinated, people_fully_vaccinated_per_hundred, total_cases, total_deaths, total_cases_per_million, total_deaths_per_million, total_tests, total_tests_per_thousand, positive_rate, population, population_density, median_age, .direction = "updown")
        #mutate(tests_per_case=total_cases/total_tests)
      
      c<-rbind(c, a)
    }
    return(c)
  }
  #imputing the values which can be imputed with updown
  {
    min_full_set_by_country <- updown_filler(min_full_set_by_country)
    min_full_set_by_income <- updown_filler(min_full_set_by_income)
    min_full_set_by_region <- updown_filler(min_full_set_by_region)
  }
  
  #summaries to get the overall idea of distribution of new NA values
  {
    summary(min_full_set_by_country)
    (colMeans(is.na(min_full_set_by_country)))*100
    
    summary(min_full_set_by_income)
    (colMeans(is.na(min_full_set_by_income)))*100
    #remove columns which have higher than 80% values as NA  
    min_full_set_by_income <- min_full_set_by_income[, which(colMeans(!is.na(min_full_set_by_income)) > 0.8)]
    
    summary(min_full_set_by_region)
    (colMeans(is.na(min_full_set_by_region)))*100
    min_full_set_by_region <- min_full_set_by_region[, which(colMeans(!is.na(min_full_set_by_region)) > 0.8)]
    
    #further introspection shows that the entry OWID_INT has no values of use, so remove it
    min_full_set_by_region <- min_full_set_by_region[!min_full_set_by_region$location %in% c("International"), ]
    
  }
  #only min_full_set_by_country has missing values, imputing them one by one 
  c<-min_full_set_by_country
  md.pattern(c)
  nrow(c)*ncol(c)
  #removing countries with very high NA percentages
  {
    b1<-c()
    f1<-names(c)
      for(i in c(1:length(country_real_list)))
      {
        cnew<- c %>%
          filter(location==country_real_list[i])
        a<-sum(is.na(cnew[, ]))
        b<-nrow(cnew)*ncol(cnew)
        #b2<-ncol(cnew)
        
        if(a/b > 0.2)
        {
          b1<-c(b1, country_real_list[i])
          print(tail(b1, 1))
          {#for(j in c(1:19))
          #{
            #a1<-sum(is.na(cnew[, j]))
            #if(a1/b2>0.5)
              #print(f1[j])
          #}}
        }
      }
      #remove the countries in b1
      c <- c[!c$location %in% b1, ]
    }
    sum(is.na(c[,]))
    (colMeans(is.na(c)))*100
  }
  #countries with HDI missing
  {
    hdi_na <- c %>%
      filter(is.na(human_development_index)) %>%
      select(location)
    unique(hdi_na$location)
    #Aruba HDI - 0.908 below 4 from https://en.populationdata.net/rankings/hdi/americas/
    #Bermuda HDI - 0.981
    #Cayman Islands HDI - 0.888
    #Curacao HDI - 0.811
    #Kosovo HDI - 0.750 below 3 from https://globaldatalab.org/shdi/table/shdi/
    #Somalia HDI - 0.361 
    #Taiwan HDI - 0.961
    c<-c %>%
      mutate(human_development_index = if_else(location=="Aruba", 0.908, human_development_index),
             human_development_index = if_else(location=="Bermuda", 0.981, human_development_index),
             human_development_index = if_else(location=="Cayman Islands", 0.888, human_development_index),
             human_development_index = if_else(location=="Curacao", 0.811, human_development_index),
             human_development_index = if_else(location=="Kosovo", 0.750, human_development_index),
             human_development_index = if_else(location=="Somalia", 0.361, human_development_index),
             human_development_index = if_else(location=="Taiwan", 0.961, human_development_index))
  }
  #countries with GDP (PPP) per capita missing
  {
    gdp_na <- c %>%
      filter(is.na(gdp_per_capita)) %>%
      select(location)
    unique(gdp_na$location)
    #Andorra GDP - 49990 below all from https://www.cia.gov/the-world-factbook/field/real-gdp-per-capita/country-comparison
    #Cuba GDP - 12800
    #Curacao Islands GDP - 24500
    #Liechtenstein GDP - 139100
    #Somalia GDP - 800
    #Syria GDP - 2900 
    #Taiwan GDP - 24502
    c<-c %>%
      mutate(gdp_per_capita = if_else(location=="Andorra", 49990, gdp_per_capita),
             gdp_per_capita = if_else(location=="Cuba", 12800, gdp_per_capita),
             gdp_per_capita = if_else(location=="Liechtenstein", 139100, gdp_per_capita),
             gdp_per_capita = if_else(location=="Curacao", 24500, gdp_per_capita),
             gdp_per_capita = if_else(location=="Syria", 2900, gdp_per_capita),
             gdp_per_capita = if_else(location=="Somalia", 800, gdp_per_capita),
             gdp_per_capita = if_else(location=="Taiwan", 24502, gdp_per_capita))
  }
  #countries with population density missing
  {
    popden_na <- c %>%
      filter(is.na(population_density)) %>%
      select(location)
    unique(popden_na$location)
    #South Sudan popden - 17.01 below all from https://ourworldindata.org/grapher/population-density?tab=table&time=2021..latest
    #Syria popden - 116.13
    #Taiwan popden - 673
    c<-c %>%
      mutate(population_density = if_else(location=="South Sudan", 17.01, population_density),
             population_density = if_else(location=="Syria", 116.13, population_density),
             population_density = if_else(location=="Taiwan", 673, population_density))
    }
  #countries with median age missing
  {
    ma_na <- c %>%
      filter(is.na(median_age)) %>%
      select(location)
    unique(ma_na$location)
    #Andorra GDP - 49990 below all from https://www.cia.gov/the-world-factbook/field/real-gdp-per-capita/country-comparison
    #Bermuda GDP - 12800
    #Cayman Islands Islands GDP - 24500
    #Dominica GDP - 
    #Kosovo GDP - 
    #Liechtenstein GDP - 139100
    #Saint Kitts and Nevis GDP - 800

    c<-c %>%
      mutate(median_age = if_else(location=="Andorra", 41.1, median_age),
             median_age = if_else(location=="Bermuda", 44.5, median_age),
             median_age = if_else(location=="Liechtenstein", 43.5, median_age),
             median_age = if_else(location=="Cayman Islands", 36.8, median_age),
             median_age = if_else(location=="Dominica", 31.3, median_age),
             median_age = if_else(location=="Kosovo", 30.1, median_age),
             median_age = if_else(location=="Saint Kitts and Nevis", 33.6, median_age))
  }
  #countries with total tests missing
  {
    tt_na <- c %>%
      filter(is.na(total_tests)) %>%
      select(location)
    unique(tt_na$location)
    
    #Cape Verse
    #Libya
    #Moldova
    #Palestine
    #Singapore
    #After extensive checking, information unavailable in every case without
    #at least first cumulatively summing each case individually a/c available data
    #and for palestine and moldova, no such data found.
    #taking decision to remove
    
    c <- c[!c$location %in% unique(tt_na$location), ]
  }
  #countries with positive rate missing
  {
    pr_na <- c %>%
      filter(is.na(positive_rate)) %>%
      select(location)
    unique(pr_na$location)
    
    morecasesthantests <- c %>%
      filter(total_cases > total_tests)
    unique(morecasesthantests$location)
    #as we see above, there are many cases where total_cases can be more than total_tests
    #as the source puts it, 
    # "Some countries, however, do include positive antibody tests in their figures for confirmed cases.
    # Our testing figures – which exclude antibody tests – 
    # are not an appropriate comparison in these instances: 
    # on this basis, there could be more cases than tests, which is not possible"
    #Furthermore, they can also include people tested repeatedly
    new1 <- c %>%
      filter(location %in% unique(pr_na$location))
    nrow(new1)
    sum(is.na(new1$positive_rate))
    #as all are empty, we make the decision to remove these countries from the dataset
    c <- c[!c$location %in% unique(pr_na$location), ]
  }
  #countries with tests per case missing
  {
    tpc_na <- c %>%
      filter(is.na(tests_per_case)) %>%
      select(location)
    unique(tpc_na$location)
    #after looking deeper in the data, we realize the following : 
    #Tests Per Case has similar reporting stringency to positive rate
    #Tests Per Case is missing in almost all countries, for some dates
    #As such, we are unable to maintain the quality of the database by imputing missing values
    #either with packages such as MICE using pmm, or with worse methods such as colmeans
    #Therefore, we have made the decision to drop the column
    c$tests_per_case <- NULL
  }
  (colMeans(is.na(c)))*100
  min_full_set_by_country<-c
}

#graphs and analysis
{
  #cases, deaths, vaccinated vs time income based
  {
  high_income_set <- min_full_set_by_income %>% filter(location=="High income")
  ggplot(high_income_set, aes(total_cases, date)) + geom_point()
  ggplot(high_income_set, aes(total_deaths, date)) + geom_point()
  ggplot(high_income_set, aes(people_vaccinated, date)) + geom_point()
  
  up_mid_income_set <- min_full_set_by_income %>% filter(location=="Upper middle income")
  ggplot(up_mid_income_set, aes(total_cases, date)) + geom_point()
  ggplot(up_mid_income_set, aes(total_deaths, date)) + geom_point()
  ggplot(up_mid_income_set, aes(people_vaccinated, date)) + geom_point()
  
  low_mid_income_set <- min_full_set_by_income %>% filter(location=="Lower middle income")
  ggplot(low_mid_income_set, aes(total_cases, date)) + geom_point()
  ggplot(low_mid_income_set, aes(total_deaths, date)) + geom_point()
  ggplot(low_mid_income_set, aes(people_vaccinated, date)) + geom_point()
  
  low_income_set <- min_full_set_by_income %>% filter(location=="Low income")
  ggplot(low_income_set, aes(total_cases, date)) + geom_point()
  ggplot(low_income_set, aes(total_deaths, date)) + geom_point()
  ggplot(low_income_set, aes(people_vaccinated, date)) + geom_point()
  }
  
  #region wise case and death rates in 3 month periods
  {
    region_wise_latest_data <- min_full_set_by_region %>% filter(date == as.Date("2022-09-21"))
    region_wise_latest_data <- region_wise_latest_data %>% mutate(death_per_pop = total_deaths/population, cases_per_pop = total_cases/population, )
    ggplot(region_wise_latest_data, aes(location, total_cases_per_million))+geom_bar(stat='identity')
    ggplot(region_wise_latest_data, aes(location, death_per_pop))+geom_bar(stat='identity')
    
    region_wise_latest_data <- min_full_set_by_region %>% filter(date == as.Date("2022-06-21"))
    region_wise_latest_data <- region_wise_latest_data %>% mutate(death_per_pop = total_deaths/population, cases_per_pop = total_cases/population, )
    ggplot(region_wise_latest_data, aes(location, total_cases_per_million))+geom_bar(stat='identity')
    ggplot(region_wise_latest_data, aes(location, death_per_pop))+geom_bar(stat='identity')
    
    region_wise_latest_data <- min_full_set_by_region %>% filter(date == as.Date("2022-03-21"))
    region_wise_latest_data <- region_wise_latest_data %>% mutate(death_per_pop = total_deaths/population, cases_per_pop = total_cases/population, )
    ggplot(region_wise_latest_data, aes(location, total_cases_per_million))+geom_bar(stat='identity')
    ggplot(region_wise_latest_data, aes(location, death_per_pop))+geom_bar(stat='identity')
    
    region_wise_latest_data <- min_full_set_by_region %>% filter(date == as.Date("2021-12-21"))
    region_wise_latest_data <- region_wise_latest_data %>% mutate(death_per_pop = total_deaths/population, cases_per_pop = total_cases/population, )
    ggplot(region_wise_latest_data, aes(location, total_cases_per_million))+geom_bar(stat='identity')
    ggplot(region_wise_latest_data, aes(location, death_per_pop))+geom_bar(stat='identity')
    
    region_wise_latest_data <- min_full_set_by_region %>% filter(date == as.Date("2021-09-21"))
    region_wise_latest_data <- region_wise_latest_data %>% mutate(death_per_pop = total_deaths/population, cases_per_pop = total_cases/population, )
    ggplot(region_wise_latest_data, aes(location, total_cases_per_million))+geom_bar(stat='identity')
    ggplot(region_wise_latest_data, aes(location, death_per_pop))+geom_bar(stat='identity')
    
    region_wise_latest_data <- min_full_set_by_region %>% filter(date == as.Date("2021-06-21"))
    region_wise_latest_data <- region_wise_latest_data %>% mutate(death_per_pop = total_deaths/population, cases_per_pop = total_cases/population, )
    ggplot(region_wise_latest_data, aes(location, total_cases_per_million))+geom_bar(stat='identity')
    ggplot(region_wise_latest_data, aes(location, death_per_pop))+geom_bar(stat='identity')
    
    region_wise_latest_data <- min_full_set_by_region %>% filter(date == as.Date("2021-03-21"))
    region_wise_latest_data <- region_wise_latest_data %>% mutate(death_per_pop = total_deaths/population, cases_per_pop = total_cases/population, )
    ggplot(region_wise_latest_data, aes(location, total_cases_per_million))+geom_bar(stat='identity')
    ggplot(region_wise_latest_data, aes(location, death_per_pop))+geom_bar(stat='identity')
    
  }
  
  #country wise, gdp per capita vs data, median age vs data
  {
    country_wise_latest_data <- min_full_set_by_country %>% filter(date == as.Date("2022-09-21"))
    ggplot(country_wise_latest_data, aes(gdp_per_capita, total_deaths_per_million))+xlim(0, 8000)+ylim(0, 2000)+geom_point()+geom_smooth()
    ggplot(country_wise_latest_data, aes(gdp_per_capita, total_cases_per_million))+xlim(0, 8000)+ylim(0, 200000)+geom_point()+geom_smooth()
    ggplot(country_wise_latest_data, aes(gdp_per_capita, positive_rate))+xlim(0, 8000)+geom_point()+geom_smooth()
    ggplot(country_wise_latest_data, aes(gdp_per_capita, people_fully_vaccinated_per_hundred))+xlim(0, 8000)+geom_point()+geom_smooth()
    
    ggplot(country_wise_latest_data, aes(median_age, total_deaths_per_million))+xlim(20, 40)+ylim(0, 2000)+geom_point()+geom_smooth()
    ggplot(country_wise_latest_data, aes(median_age, total_cases_per_million))+xlim(20, 40)+ylim(0, 200000)+geom_point()+geom_smooth()
    ggplot(country_wise_latest_data, aes(median_age, positive_rate))+xlim(20, 40)+geom_point()+geom_smooth()
    ggplot(country_wise_latest_data, aes(median_age, people_fully_vaccinated_per_hundred))+xlim(20, 40)+geom_point()+geom_smooth()
    
    
    country_wise_latest_data <- min_full_set_by_country %>% filter(date == as.Date("2021-09-21"))
    ggplot(country_wise_latest_data, aes(gdp_per_capita, total_deaths_per_million))+xlim(0, 8000)+ylim(0, 2000)+geom_point()+geom_smooth()
    ggplot(country_wise_latest_data, aes(gdp_per_capita, total_cases_per_million))+xlim(0, 8000)+ylim(0, 200000)+geom_point()+geom_smooth()
    ggplot(country_wise_latest_data, aes(gdp_per_capita, positive_rate))+xlim(0, 8000)+geom_point()+geom_smooth()
    ggplot(country_wise_latest_data, aes(gdp_per_capita, people_fully_vaccinated_per_hundred))+xlim(0, 8000)+geom_point()+geom_smooth()
    
    ggplot(country_wise_latest_data, aes(median_age, total_deaths_per_million))+xlim(20, 40)+ylim(0, 2000)+geom_point()+geom_smooth()
    ggplot(country_wise_latest_data, aes(median_age, total_cases_per_million))+xlim(20, 40)+ylim(0, 200000)+geom_point()+geom_smooth()
    ggplot(country_wise_latest_data, aes(median_age, positive_rate))+xlim(20, 40)+geom_point()+geom_smooth()
    ggplot(country_wise_latest_data, aes(median_age, people_fully_vaccinated_per_hundred))+xlim(20, 40)+geom_point()+geom_smooth()
    
    }
}
#insights
#OWID_INT (International) is a row which provides no value to the dataset
#owing to the lack of details and updates

#a few locations like Guernsey have no data available at any point of time for 
#categories such as total_cases, total_deaths, etc. While the data is individually
#available on the government websites in most cases (eg. https://covid19.gov.gg/test-results)
#we have currently decided to instead drop the locations without such data in the
#OWID dataset. Such countries include
#Guernsey, Jersey, North Korea, Guam, US Virgin Islands, Faroe Islands, North Cyprus, Vatican, etc

#Many cases exist where the full OWID datasets include values for missing columns
#eg, HDI, Median Age. Or when such data is missing from OWID but available with
#external sets such as CIA world factbook, Global Data Lab, etc.
#We have manually carried over such data into the covid dataset


