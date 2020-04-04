#these libraries need to be loaded
library(utils)
library(httr)
library(ggplot2);

#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")));

#read the Dataset sheet into “R”. The dataset will be called "data".
covid19 <- read.csv(tf);

###################################################
# Data manipulations
###################################################
names(covid19)[1]<-"dateRep";
covid19$dateRep<-as.Date(covid19$dateRep, format="%d/%m/%Y")

#### Calculate total cases
covid19.sum <- aggregate(cbind(cases,deaths) ~ geoId, data = covid19, sum);
names(covid19.sum)[2]<-"sumcases";
names(covid19.sum)[3]<-"sumdeaths";
covid19<-merge(covid19,covid19.sum, all=TRUE);

#### Calculate daily cum cases
covid19<-covid19[order(covid19$dateRep),];
covid19<-covid19[order(covid19$countriesAndTerritories),]
covid19$sumcases.d <- NA;
covid19$sumdeaths.d <- NA;
n<-length(levels(covid19$countriesAndTerritories))
for (i in 1:n) {
  covid19[covid19$countriesAndTerritories==levels(covid19$countriesAndTerritories)[i],"sumcases.d"] <- cumsum(covid19[covid19$countriesAndTerritories==levels(covid19$countriesAndTerritories)[i],"cases"]);
  covid19[covid19$countriesAndTerritories==levels(covid19$countriesAndTerritories)[i],"sumdeaths.d"] <- cumsum(covid19[covid19$countriesAndTerritories==levels(covid19$countriesAndTerritories)[i],"deaths"]);
}

covid19$newcases.pop <- covid19$cases*100000/covid19$popData2018;
covid19$newdeaths.pop <- covid19$deaths*100000/covid19$popData2018;
covid19$sumcases.d.pop <- covid19$sumcases.d*100000/covid19$popData2018;
covid19$sumdeaths.d.pop <- covid19$sumdeaths.d*100000/covid19$popData2018;
covid19$letality <- covid19$sumdeaths.d / covid19$sumcases.d;

##### Subsetting Interesting countries
covid19.red <- covid19[(covid19$geoId=="AT" | covid19$geoId=="IT"| covid19$geoId=="DE"| covid19$geoId=="US"| covid19$geoId=="SE"| covid19$geoId=="UK") 
                       & covid19$month>=3 & covid19$year==2020,];
covid19.red<-covid19.red[is.na(covid19.red$geoId)==0,];


###################################################
# Plots
###################################################

ggplot(data = covid19.red, aes(x=dateRep, y=newcases.pop, color = geoId))+geom_point()+geom_line() + 
  labs(title="New daily SARS-CoV-2+ cases / 100 000 population", x="Date", y="new cases / 100 000 population");
ggplot(data = covid19.red, aes(x=dateRep, y=newdeaths.pop, color = geoId))+geom_point()+geom_line() + 
  labs(title="New daily COVID-19 deaths / 100 000 population", x="Date", y="new COVID-19 deaths / 100 000 population");

ggplot(data = covid19.red, aes(x=dateRep, y=sumcases.d, color = geoId))+geom_point()+geom_line() + 
  labs(title="Total SARS-CoV-2+ cases", x="Date", y="total SARS-CoV-2+ cases");
ggplot(data = covid19.red, aes(x=dateRep, y=sumdeaths.d, color = geoId))+geom_point()+geom_line() + 
  labs(title="Total COVID-19 deaths", x="Date", y="total COVID-19 deaths");

ggplot(data = covid19.red, aes(x=dateRep, y=sumcases.d.pop, color = geoId))+geom_point()+geom_line() + 
  labs(title="Cumulative SARS-CoV-2+ cases / 100 000 population", x="Date", y="cum. SARS-CoV-2+ cases / 100 000 population");
ggplot(data = covid19.red, aes(x=dateRep, y=sumdeaths.d.pop, color = geoId))+geom_point()+geom_line() + 
  labs(title="Cumulative COVID-19 deaths / 100 000 population", x="Date", y="cum. COVID-19 deaths / 100 000 population");

ggplot(data = covid19.red, aes(x=dateRep, y=letality, color = geoId))+geom_point()+geom_line() + 
  labs(title="COVID-19 deaths / SARS-CoV-2+ Cases", x="Date", y="COVID-19 deaths / SARS-CoV-2+ Cases");