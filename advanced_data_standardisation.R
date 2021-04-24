#### Advanced Tasks Part 1 - Data Standardisation 

### 1) Standardise data for suburbs

## 1a) AUS_SubCityDistrictState_Data

setwd("D:/Datasci/MVPstudio/Advanced")

library(openxlsx)

suburbs = read.xlsx("AUS_SubCityDistrictState_Data.xlsx", sheet = 2)

table(suburbs$city)

suburbs[] = lapply(suburbs, trimws)

library(stringr)

suburbs$city = str_replace_all(suburbs$city, "[[:punct:]]", "")

suburbs = suburbs[,1:7]

colnames(suburbs) = c("Postcode", "Suburb", "City", "State", "State_Code", "LAT", "LON")

suburbs = suburbs[rowSums(is.na(suburbs)) == 0,]

plot(suburbs$LON, suburbs$LAT)

suburbs$LON = as.numeric(suburbs$LON)

suburbs2 = suburbs[suburbs$LON > 110,]
suburbs2 = suburbs2[suburbs2$LON < 155,]

write.csv(suburbs2, file = "Suburbs.csv")



### 1) Standardise data for crime data

setwd("D:/Datasci/MVPstudio/Advanced/crime_data")

library(openxlsx)

ACT = read.xlsx("ACT_data_Cleaned.xlsx", sheet = 1)

NT1 = read.xlsx("Northern Territory_data_CLEANED.xlsx", sheet = 1)

NT2 = read.xlsx("Northern Territory_data_CLEANED.xlsx", sheet = 2)

NSW = read.xlsx("NSW_BySuburb_data_CLEANED.xlsx", sheet = 1)
# NEEDS PIVOTING

QLD = read.xlsx("Queensland_data_CLEANED.xlsx", sheet = 1)

SA1 = read.xlsx("South Australia_data_CLEANED.xlsx", sheet = 1)

SA2 = read.xlsx("South Australia_data_CLEANED.xlsx", sheet = 2)

SA3 = read.xlsx("South Australia_data_CLEANED.xlsx", sheet = 3)

SA4 = read.xlsx("South Australia_data_CLEANED.xlsx", sheet = 4)

SA5 = read.xlsx("South Australia_data_CLEANED.xlsx", sheet = 5)

SA6 = read.xlsx("South Australia_data_CLEANED.xlsx", sheet = 6)

VIC = read.xlsx("Victoria_BySuburb_data_CLEANED.xlsx", sheet = 1)

WA = read.xlsx("Western Australia_data_CLEANED.xlsx", sheet = 1)


## Create final data frame

Crime_Final = data.frame(
    State = factor(),
    District = factor(),
    Suburb = factor(),
    Month = integer(),
    Year = integer(),
    Quarter = numeric(),
    Offence_Division = factor(),
    Offence_Subdivision = factor(),
    Offence = factor(),
    Number_of_Offences = numeric(),
    stringsAsFactors=FALSE
)


## NSW

colnames(NSW)[1] = "Suburb"

library(reshape)

pivotedNSW = melt(NSW, id = c("Suburb", "Offence"))

pivotedNSW$State = "New South Wales"

pivotedNSW$District = NA

pivotedNSW$Month = gsub("[[:digit:]]+/", "", pivotedNSW$variable)

pivotedNSW$Year = gsub("/[[:alpha:]]+", "", pivotedNSW$variable)

pivotedNSW$Quarter = ifelse(pivotedNSW$Month %in% c("Jan", "Feb", "Mar"), 1,
                            ifelse(pivotedNSW$Month %in% c("Apr", "May", "Jun"), 2,
                                   ifelse(pivotedNSW$Month %in% c("Jul", "Aug", "Sep"), 3, 4)))

pivotedNSW$Offence_Division = NA

pivotedNSW$Offence_Subdivision = NA

colnames(pivotedNSW)
colnames(Crime_Final)

pivotedNSW = pivotedNSW[, c(5, 6, 1, 7, 8, 9, 10, 11, 2, 4)]

Crime_Final = rbind(Crime_Final, pivotedNSW)

colnames(Crime_Final)[10] = "Number_of_Offences"


## ACT

ACTFinal = ACT

ACTFinal$Month = NA

ACTFinal$Offence_Division = NA

ACTFinal$Offence_Subdivision = NA

colnames(ACTFinal)
colnames(Crime_Final)

ACTFinal = ACTFinal[, c(3, 2, 1, 8, 5, 4, 9, 10, 6, 7)]

colnames(ACTFinal) = colnames(Crime_Final)

Crime_Final = rbind(Crime_Final, ACTFinal)


## NT
# NT1

NT1Final = NT1

NT1Final$Date = NT1Final$Quarter

NT1Final$Quarter = gsub("-[[:digit:]]+", "", NT1Final$Date)
NT1Final$Quarter = gsub("[[:alpha:]]", "", NT1Final$Quarter)

NT1Final$Year = gsub("[[:alnum:]]+-", "", NT1Final$Date)

NT1Final$Month = NA

NT1Final$Offence_Division = NA

NT1Final$Suburb = NA

colnames(NT1Final)
colnames(Crime_Final)

NT1Final = NT1Final[, c(6, 5, 11, 9, 8, 3, 10, 1, 2, 4)]

colnames(NT1Final) = colnames(Crime_Final)

Crime_Final = rbind(Crime_Final, NT1Final)


#NT2

NT2Final = NT2

NT2Final$Month = gsub("-[[:digit:]]+", "", NT2Final$Date)
NT2Final$Year = gsub("[[:alpha:]]+-", "", NT2Final$Date)
NT2Final$Year = paste0("20", NT2Final$Year, sep = "")

NT2Final$Quarter = ifelse(NT2Final$Month %in% c("Jan", "Feb", "Mar"), 1,
                            ifelse(NT2Final$Month %in% c("Apr", "May", "Jun"), 2,
                                   ifelse(NT2Final$Month %in% c("Jul", "Aug", "Sep"), 3, 4)))

NT2Final$Offence_Division = NA

NT2Final$Suburb = NA

colnames(NT2Final)
colnames(Crime_Final)

NT2Final = NT2Final[, c(6, 5, 11, 7, 8, 9, 10, 1, 2, 4)]

colnames(NT2Final) = colnames(Crime_Final)

Crime_Final = rbind(Crime_Final, NT2Final)


## QLD

QLDFinal = QLD

QLDFinal$Offence_Division = NA
    
QLDFinal$Offence_Subdivision = NA

QLDFinal$State = "Queensland"

QLDFinal$Suburb = NA

QLDFinal$Month = gsub("-[[:digit:]]+", "", QLDFinal$Date)
QLDFinal$Year = gsub("[[:digit:]]+-", "", QLDFinal$Date)

library(stringr)
QLDFinal$Month = str_remove(QLDFinal$Month, "^0+")
QLDFinal$Month = month.abb[as.integer(QLDFinal$Month)]

QLDFinal$Quarter = ifelse(QLDFinal$Month %in% c("Jan", "Feb", "Mar"), 1,
                          ifelse(QLDFinal$Month %in% c("Apr", "May", "Jun"), 2,
                                 ifelse(QLDFinal$Month %in% c("Jul", "Aug", "Sep"), 3, 4)))

colnames(QLDFinal)
colnames(Crime_Final)

QLDFinal = QLDFinal[, c(8, 1, 9, 10, 11, 12, 6, 7, 3, 4)]

colnames(QLDFinal) = colnames(Crime_Final)

Crime_Final = rbind(Crime_Final, QLDFinal)


## SA

SAFinal = rbind(SA1, SA2, SA3, SA4, SA5, SA6)

SAFinal$Date = as.Date(SAFinal$Reported.Date, origin = "1899-12-30")

SAFinal$Month = gsub(".{4}-", "", SAFinal$Date)
SAFinal$Month = gsub("-.{2}", "", SAFinal$Month)
SAFinal$Year = gsub("-[[:digit:]]+", "", SAFinal$Date)

SAFinal$Month = str_remove(SAFinal$Month, "^0+")
SAFinal$Month = month.abb[as.integer(SAFinal$Month)]

SAFinal$Quarter = ifelse(SAFinal$Month %in% c("Jan", "Feb", "Mar"), 1,
                          ifelse(SAFinal$Month %in% c("Apr", "May", "Jun"), 2,
                                 ifelse(SAFinal$Month %in% c("Jul", "Aug", "Sep"), 3, 4)))

SAFinal$State = "South Australia"

SAFinal$District = NA

colnames(SAFinal)
colnames(Crime_Final)

SAFinal = SAFinal[, c(12, 13, 2, 9, 10, 11, 4, 5, 6, 7)]

colnames(SAFinal) = colnames(Crime_Final)

Crime_Final = rbind(Crime_Final, SAFinal)


## VIC

VICFinal = VIC

VICFinal$Offence = gsub(".+[[:digit:]]+ ", "", VICFinal$Offence.Subgroup)
VICFinal$Offence_Subdivision = gsub(".+[[:digit:]]+ ", "", VICFinal$Offence.Subdivision)
VICFinal$Offence_Division = gsub("^. ", "", VICFinal$Offence.Division)

VICFinal$Month = NA

VICFinal$Quarter = NA

VICFinal$District = NA

colnames(VICFinal)
colnames(Crime_Final)

VICFinal = VICFinal[, c(8, 14, 3, 12, 1, 13, 11, 10, 9, 7)]

colnames(VICFinal) = colnames(Crime_Final)

Crime_Final = rbind(Crime_Final, VICFinal)


## WA

WAFinal = WA

WAFinal$Month = gsub("-[[:digit:]]+", "", WAFinal$Date)
WAFinal$Year = gsub("[[:alpha:]]+-", "", WAFinal$Date)
WAFinal$Year = paste0("20", WAFinal$Year, sep = "")

WAFinal$Quarter = ifelse(WAFinal$Month %in% c("Jan", "Feb", "Mar"), 1,
                         ifelse(WAFinal$Month %in% c("Apr", "May", "Jun"), 2,
                                ifelse(WAFinal$Month %in% c("Jul", "Aug", "Sep"), 3, 4)))

WAFinal$Suburb = NA

WAFinal$State = "Western Australia"

WAFinal$Offence_Division = NA

colnames(WAFinal)
colnames(Crime_Final)

WAFinal = WAFinal[, c(10, 1, 9, 6, 7, 8, 11, 2, 3, 5)]

colnames(WAFinal) = colnames(Crime_Final)

Crime_Final = rbind(Crime_Final, WAFinal)


nrow(Crime_Final)
sum(nrow(WAFinal), nrow(VICFinal), nrow(SAFinal), nrow(QLDFinal),
    nrow(NT1Final), nrow(NT2Final), nrow(pivotedNSW), nrow(ACTFinal))


## Crime Divisions and Subdivisions


Crime_Final = Crime_Final[-which(Crime_Final$Offence_Subdivision == "Total offences against the person "),]
Crime_Final = Crime_Final[-which(Crime_Final$Offence == "Offences Against Property"),]
Crime_Final = Crime_Final[-which(Crime_Final$Offence == "Property Damage Total"),]
Crime_Final = Crime_Final[-which(Crime_Final$Offence == "Arson Total"),]
Crime_Final = Crime_Final[-which(Crime_Final$Offence == "Drug Offences Total"),]
Crime_Final = Crime_Final[-which(Crime_Final$Offence == "Fraud & Related Offences Total"),]
Crime_Final = Crime_Final[-which(Crime_Final$Offence == "Receiving and Possession of Stolen Property Total"),] 
Crime_Final = Crime_Final[-which(Crime_Final$Offence == "Breach of Violence Restraint Order Total"),]




Crime_Final[which(Crime_Final$Offence == "Serious Assault (Other)"),]


## Standardise offence

Crime_Standard = Crime_Final

Crime_Standard[,c(7,8)] = NA

# Offences against the person

### Homicide

Crime_Standard[which(Crime_Final$Offence == "Murder*"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Homicide and Related Offences", "Murder"),
        each = length(which(Crime_Final$Offence == "Murder*")))

Crime_Standard[which(Crime_Final$Offence == "Attempted murder"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Homicide and Related Offences", "Attempted Murder"),
        each = length(which(Crime_Final$Offence == "Attempted murder")))

Crime_Standard[which(Crime_Final$Offence == "Murder accessory, conspiracy"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Homicide and Related Offences", "Murder Accessory and Conspiracy"),
        each = length(which(Crime_Final$Offence == "Murder accessory, conspiracy")))

Crime_Standard[which(Crime_Final$Offence == "Manslaughter*"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Homicide and Related Offences", "Manslaughter"),
        each = length(which(Crime_Final$Offence == "Manslaughter*")))

Crime_Standard[which(Crime_Final$Offence == "Homicide"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Homicide and Related Offences", "Homicide (General)"),
        each = length(which(Crime_Final$Offence == "Homicide")))

Crime_Standard[which(Crime_Final$Offence == "Murder"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Homicide and Related Offences", "Murder"),
        each = length(which(Crime_Final$Offence == "Murder")))

Crime_Standard[which(Crime_Final$Offence == "Manslaughter"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Homicide and Related Offences", "Manslaughter"),
        each = length(which(Crime_Final$Offence == "Manslaughter")))

Crime_Standard[which(Crime_Final$Offence == "Homicide (Murder)"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Homicide and Related Offences", "Murder"),
        each = length(which(Crime_Final$Offence == "Homicide (Murder)")))

Crime_Standard[which(Crime_Final$Offence == "Other Homicide"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Homicide and Related Offences", "Other Homicide"),
        each = length(which(Crime_Final$Offence == "Other Homicide")))

Crime_Standard[which(Crime_Final$Offence == "Attempted Murder"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Homicide and Related Offences", "Attempted Murder"),
        each = length(which(Crime_Final$Offence == "Attempted Murder")))

Crime_Standard[which(Crime_Final$Offence == "Conspiracy to Murder"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Homicide and Related Offences", "Murder Accessory and Conspiracy"),
        each = length(which(Crime_Final$Offence == "Conspiracy to Murder")))

Crime_Standard[which(Crime_Final$Offence == "Manslaughter (excl. by driving)"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Homicide and Related Offences", "Manslaughter"),
        each = length(which(Crime_Final$Offence == "Manslaughter (excl. by driving)")))

Crime_Standard[which(Crime_Final$Offence == "Manslaughter Unlawful Striking Causing Death"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Homicide and Related Offences", "Manslaughter Unlawful Striking Causing Death"),
        each = length(which(Crime_Final$Offence == "Manslaughter Unlawful Striking Causing Death")))

Crime_Standard[which(Crime_Final$Offence == "Other homicide and related offences"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Homicide and Related Offences", "Other Homicide"),
        each = length(which(Crime_Final$Offence == "Other homicide and related offences")))

Crime_Standard[which(Crime_Final$Offence == "Attempted / Conspiracy to Murder"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Homicide and Related Offences", "Murder Accessory and Conspiracy"),
        each = length(which(Crime_Final$Offence == "Attempted / Conspiracy to Murder")))

### Acts Intended to Cause Injury

Crime_Standard[which(Crime_Final$Offence == "Assault"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Assault (General)"),
        each = length(which(Crime_Final$Offence == "Assault")))

Crime_Standard[which(Crime_Final$Offence == "DV related assault"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Domestic Violence Assault"),
        each = length(which(Crime_Final$Offence == "DV related assault")))

Crime_Standard[which(Crime_Final$Offence == "Non DV related assault"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Non Domestic Violence Assault"),
        each = length(which(Crime_Final$Offence == "Non DV related assault")))

Crime_Standard[which(Crime_Final$Offence == "Assault Police"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Assault on Police or Emergency Personnel"),
        each = length(which(Crime_Final$Offence == "Assault Police")))

Crime_Standard[which(Crime_Final$Offence == "Acts intended to cause injury - other"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Other Acts Intended to Cause Injury"),
        each = length(which(Crime_Final$Offence == "Acts intended to cause injury - other")))

Crime_Standard[which(Crime_Final$Offence == "Grievous Assault"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Serious Assault"),
        each = length(which(Crime_Final$Offence == "Grievous Assault")))

Crime_Standard[which(Crime_Final$Offence == "Serious Assault"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Serious Assault"),
        each = length(which(Crime_Final$Offence == "Serious Assault")))

Crime_Standard[which(Crime_Final$Offence == "Serious Assault (Other)"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Serious Assault"),
        each = length(which(Crime_Final$Offence == "Serious Assault (Other)")))

Crime_Standard[which(Crime_Final$Offence == "Serious Assault not resulting in injury"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Serious Assault not Resulting in Injury"),
        each = length(which(Crime_Final$Offence == "Serious Assault not resulting in injury")))

Crime_Standard[which(Crime_Final$Offence == "Common Assault"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Assault"),
        each = length(which(Crime_Final$Offence == "Common Assault")))

Crime_Standard[which(Crime_Final$Offence == "Common Assault'"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Assault"),
        each = length(which(Crime_Final$Offence == "Common Assault'")))

Crime_Standard[which(Crime_Final$Offence == "Other acts intended to cause injury"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Other Acts Intended to Cause Injury"),
        each = length(which(Crime_Final$Offence == "Other acts intended to cause injury")))

Crime_Standard[which(Crime_Final$Offence == "Serious Assault resulting in injury"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Serious Assault"),
        each = length(which(Crime_Final$Offence == "Serious Assault resulting in injury")))

Crime_Standard[which(Crime_Final$Offence == "FV Serious assault"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Domestic Violence Assault"),
        each = length(which(Crime_Final$Offence == "FV Serious assault")))

Crime_Standard[which(Crime_Final$Offence == "Non-FV Serious assault"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Non Domestic Violence Assault"),
        each = length(which(Crime_Final$Offence == "Non-FV Serious assault")))

Crime_Standard[which(Crime_Final$Offence == "Assault police, emergency services or other authorised officer"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Assault on Police or Emergency Personnel"),
        each = length(which(Crime_Final$Offence == "Assault police, emergency services or other authorised officer")))

Crime_Standard[which(Crime_Final$Offence == "FV Common assault"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Domestic Violence Assault"),
        each = length(which(Crime_Final$Offence == "FV Common assault")))

Crime_Standard[which(Crime_Final$Offence == "Non-FV Common assault"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Non Domestic Violence Assault"),
        each = length(which(Crime_Final$Offence == "Non-FV Common assault")))

Crime_Standard[which(Crime_Final$Offence == "Serious Assault (Family)"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Serious Domestic Violence Assault"),
        each = length(which(Crime_Final$Offence == "Serious Assault (Family)")))

Crime_Standard[which(Crime_Final$Offence == "Common Assault (Family)"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Domestic Violence Assault"),
        each = length(which(Crime_Final$Offence == "Common Assault (Family)")))

Crime_Standard[which(Crime_Final$Offence == "Serious Assault (Non-Family)"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Serious Non Domestic Violence Assault"),
        each = length(which(Crime_Final$Offence == "Serious Assault (Non-Family)")))

Crime_Standard[which(Crime_Final$Offence == "Common Assault (Non-Family)"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Non Domestic Violence Assault"),
        each = length(which(Crime_Final$Offence == "Common Assault (Non-Family)")))

Crime_Standard[which(Crime_Final$Offence == "Assault Police Officer"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Acts Intended to Cause Injury", "Assault on Police or Emergency Personnel"),
        each = length(which(Crime_Final$Offence == "Assault Police Officer")))

### Sexual Assault and Related Offences

Crime_Standard[which(Crime_Final$Offence == "Sexual assault"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Sexual Assault and Related Offences", "Sexual Assault"),
        each = length(which(Crime_Final$Offence == "Sexual assault")))

Crime_Standard[which(Crime_Final$Offence == "Indecent assault, Act of indecency and Other sexual offences"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Sexual Assault and Related Offences", "Other Sexual Offences"),
        each = length(which(Crime_Final$Offence == "Indecent assault, Act of indecency and Other sexual offences")))

Crime_Standard[which(Crime_Final$Offence == "Non-assaultive sexual offences"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Sexual Assault and Related Offences", "Non Assaultive Sexual Offences"),
        each = length(which(Crime_Final$Offence == "Non-assaultive sexual offences")))

Crime_Standard[which(Crime_Final$Offence == "Rape and Attempted Rape"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Sexual Assault and Related Offences", "Rape and Attempted Rape"),
        each = length(which(Crime_Final$Offence == "Rape and Attempted Rape")))

Crime_Standard[which(Crime_Final$Offence == "Other Sexual Offences"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Sexual Assault and Related Offences", "Other Sexual Offences"),
        each = length(which(Crime_Final$Offence == "Other Sexual Offences")))

Crime_Standard[which(Crime_Final$Offence == "Sexual Assault"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Sexual Assault and Related Offences", "Sexual Assault"),
        each = length(which(Crime_Final$Offence == "Sexual Assault")))

Crime_Standard[which(Crime_Final$Offence == "Non-Assaultive Sexual Offences"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Sexual Assault and Related Offences", "Non Assaultive Sexual Offences"),
        each = length(which(Crime_Final$Offence == "Non-Assaultive Sexual Offences")))

### Other Acts Endangering Persons

Crime_Standard[which(Crime_Final$Offence == "Other dangerous or negligent acts endangering persons  "), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Other Acts Endangering Persons", "Other Dangerous or Negligent Acts"),
        each = length(which(Crime_Final$Offence == "Other dangerous or negligent acts endangering persons  ")))

Crime_Standard[which(Crime_Final$Offence == "Life Endangering Acts"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Other Acts Endangering Persons", "Other Dangerous or Negligent Acts"),
        each = length(which(Crime_Final$Offence == "Life Endangering Acts")))

Crime_Standard[which(Crime_Final$Offence == "Dangerous or negligent acts"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Other Acts Endangering Persons", "Other Dangerous or Negligent Acts"),
        each = length(which(Crime_Final$Offence == "Dangerous or negligent acts")))

Crime_Standard[which(Crime_Final$Offence == "Neglect or ill treatment of people"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Other Acts Endangering Persons", "Neglect or Ill Treatment of People"),
        each = length(which(Crime_Final$Offence == "Neglect or ill treatment of people")))

Crime_Standard[which(Crime_Final$Offence == "Other dangerous or negligent acts endangering people"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Other Acts Endangering Persons", "Other Dangerous or Negligent Acts"),
        each = length(which(Crime_Final$Offence == "Other dangerous or negligent acts endangering people")))

### Abduction Harassment Stalking and Threatening

Crime_Standard[which(Crime_Final$Offence == "Abduction and kidnapping"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Abduction Harassment Stalking and Threatening", "Abduction and Kidnapping"),
        each = length(which(Crime_Final$Offence == "Abduction and kidnapping")))

Crime_Standard[which(Crime_Final$Offence == "Harassment and private nuisance"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Abduction Harassment Stalking and Threatening", "Harassment and Private Nuisance"),
        each = length(which(Crime_Final$Offence == "Harassment and private nuisance")))

Crime_Standard[which(Crime_Final$Offence == "Kidnapping & Abduction etc."), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Abduction Harassment Stalking and Threatening", "Abduction and Kidnapping"),
        each = length(which(Crime_Final$Offence == "Kidnapping & Abduction etc.")))

Crime_Standard[which(Crime_Final$Offence == "Abduction, harassment and other offences"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Abduction Harassment Stalking and Threatening", "Abduction and Harassment (General)"),
        each = length(which(Crime_Final$Offence == "Abduction, harassment and other offences")))

Crime_Standard[which(Crime_Final$Offence == "Kidnapping / Child Stealing"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Abduction Harassment Stalking and Threatening", "Abduction and Kidnapping"),
        each = length(which(Crime_Final$Offence == "Kidnapping / Child Stealing")))

Crime_Standard[which(Crime_Final$Offence == "Threatening behaviour"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Abduction Harassment Stalking and Threatening", "Threatening behaviour"),
        each = length(which(Crime_Final$Offence == "Threatening behaviour")))

Crime_Standard[which(Crime_Final$Offence == "Stalking"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Abduction Harassment Stalking and Threatening", "Stalking (General)"),
        each = length(which(Crime_Final$Offence == "Stalking")))

Crime_Standard[which(Crime_Final$Offence == "Harassment, threatening behaviour and private nuisance"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Abduction Harassment Stalking and Threatening", "Harrassment and Private Nuisance (General)"),
        each = length(which(Crime_Final$Offence == "Harassment, threatening behaviour and private nuisance")))

Crime_Standard[which(Crime_Final$Offence == "FV Harassment and private nuisance"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Abduction Harassment Stalking and Threatening", "Domestic Violence Harrassment and Private Nuisance"),
        each = length(which(Crime_Final$Offence == "FV Harassment and private nuisance")))

Crime_Standard[which(Crime_Final$Offence == "Non-FV Harassment and private nuisance"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Abduction Harassment Stalking and Threatening", "Non Domestic Harrassment and Private Nuisance"),
        each = length(which(Crime_Final$Offence == "Non-FV Harassment and private nuisance")))

Crime_Standard[which(Crime_Final$Offence == "FV Stalking"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Abduction Harassment Stalking and Threatening", "Domestic Violence Stalking"),
        each = length(which(Crime_Final$Offence == "FV Stalking")))

Crime_Standard[which(Crime_Final$Offence == "Non-FV Stalking"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Abduction Harassment Stalking and Threatening", "Non Domestic Violence Stalking"),
        each = length(which(Crime_Final$Offence == "Non-FV Stalking")))

Crime_Standard[which(Crime_Final$Offence == "FV Threatening behaviour"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Abduction Harassment Stalking and Threatening", "Domestic Violence Threatening Behaviour"),
        each = length(which(Crime_Final$Offence == "FV Threatening behaviour")))

Crime_Standard[which(Crime_Final$Offence == "Non-FV Threatening behaviour"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Abduction Harassment Stalking and Threatening", "Non Domestic Violence Threatening Behaviour"),
        each = length(which(Crime_Final$Offence == "Non-FV Threatening behaviour")))

Crime_Standard[which(Crime_Final$Offence == "Threatening Behaviour (Family)"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Abduction Harassment Stalking and Threatening", "Domestic Violence Threatening Behaviour"),
        each = length(which(Crime_Final$Offence == "Threatening Behaviour (Family)")))

Crime_Standard[which(Crime_Final$Offence == "Possess Weapon to Cause Fear (Family)"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Abduction Harassment Stalking and Threatening", "Domestic Violence Possess Weapon to Cause Fear"),
        each = length(which(Crime_Final$Offence == "Possess Weapon to Cause Fear (Family)")))

Crime_Standard[which(Crime_Final$Offence == "Threatening Behaviour (Non-Family)"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Abduction Harassment Stalking and Threatening", "Non Domestic Violence Threatening Behaviour"),
        each = length(which(Crime_Final$Offence == "Threatening Behaviour (Non-Family)")))

Crime_Standard[which(Crime_Final$Offence == "Possess Weapon to Cause Fear (Non-Family)"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Abduction Harassment Stalking and Threatening", "Non Domestic Violence Possess Weapon to Cause Fear"),
        each = length(which(Crime_Final$Offence == "Possess Weapon to Cause Fear (Non-Family)")))

### Deprivation of Liberty

Crime_Standard[which(Crime_Final$Offence == "Deprivation of Liberty"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Deprivation of Liberty", "Deprivation of Liberty"),
        each = length(which(Crime_Final$Offence == "Deprivation of Liberty")))

Crime_Standard[which(Crime_Final$Offence == "Deprivation of liberty/false imprisonment"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Deprivation of Liberty", "Deprivation of Liberty"),
        each = length(which(Crime_Final$Offence == "Deprivation of liberty/false imprisonment")))

### Other Offences Against The Person

Crime_Standard[which(Crime_Final$Offence == "Other offences against the person"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Other Offences Against The Person", "Other Offences Against The Person"),
        each = length(which(Crime_Final$Offence == "Other offences against the person")))

Crime_Standard[which(Crime_Final$Offence == "Offences against a person"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Other Offences Against The Person", "Other Offences Against The Person"),
        each = length(which(Crime_Final$Offence == "Offences against a person")))

Crime_Standard[which(Crime_Final$Offence == "Other Offences Against the Person"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Other Offences Against The Person", "Other Offences Against The Person"),
        each = length(which(Crime_Final$Offence == "Other Offences Against the Person")))

Crime_Standard[which(Crime_Final$Offence == "Other crimes against the person"), c(7, 8, 9)] = 
    rep(c("Offences Against the Person", "Other Offences Against The Person", "Other Offences Against The Person"),
        each = length(which(Crime_Final$Offence == "Other crimes against the person")))


# Property and Deception Offences

### Robbery Extortion and Related Offences 

Crime_Standard[which(Crime_Final$Offence == "Robbery without a weapon"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Robbery Extortion and Related Offences", "Unarmed Robbery"),
        each = length(which(Crime_Final$Offence == "Robbery without a weapon")))

Crime_Standard[which(Crime_Final$Offence == "Robbery with a firearm"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Robbery Extortion and Related Offences", "Robbery With a Firearm"),
        each = length(which(Crime_Final$Offence == "Robbery with a firearm")))

Crime_Standard[which(Crime_Final$Offence == "Robbery with a weapon not a firearm"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Robbery Extortion and Related Offences", "Robbery With a Non Firearm Weapon"),
        each = length(which(Crime_Final$Offence == "Robbery with a weapon not a firearm")))

Crime_Standard[which(Crime_Final$Offence == "Robbery"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Robbery Extortion and Related Offences", "Robbery (General)"),
        each = length(which(Crime_Final$Offence == "Robbery")))

Crime_Standard[which(Crime_Final$Offence == "Robbery "), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Robbery Extortion and Related Offences", "Robbery (General)"),
        each = length(which(Crime_Final$Offence == "Robbery ")))

Crime_Standard[which(Crime_Final$Offence == "Armed Robbery"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Robbery Extortion and Related Offences", "Armed Robbery (General)"),
        each = length(which(Crime_Final$Offence == "Armed Robbery")))

Crime_Standard[which(Crime_Final$Offence == "Unarmed Robbery"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Robbery Extortion and Related Offences", "Unarmed Robbery"),
        each = length(which(Crime_Final$Offence == "Unarmed Robbery")))

Crime_Standard[which(Crime_Final$Offence == "Aggravated robbery"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Robbery Extortion and Related Offences", "Aggravated Robbery"),
        each = length(which(Crime_Final$Offence == "Aggravated robbery")))

Crime_Standard[which(Crime_Final$Offence == "Non-aggravated robbery"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Robbery Extortion and Related Offences", "Non Aggravated Robbery"),
        each = length(which(Crime_Final$Offence == "Non-aggravated robbery")))

Crime_Standard[which(Crime_Final$Offence == "Non-Aggravated robbery"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Robbery Extortion and Related Offences", "Non Aggravated Robbery"),
        each = length(which(Crime_Final$Offence == "Non-Aggravated robbery")))

Crime_Standard[which(Crime_Final$Offence == "Robbery (Business)"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Robbery Extortion and Related Offences", "Robbery of a Business"),
        each = length(which(Crime_Final$Offence == "Robbery (Business)")))

Crime_Standard[which(Crime_Final$Offence == "Robbery (Non-Business)"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Robbery Extortion and Related Offences", "Robbery of a Non Business"),
        each = length(which(Crime_Final$Offence == "Robbery (Non-Business)")))

### Residential Break Ins

Crime_Standard[which(Crime_Final$Offence == "Break and enter dwelling"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Residential Break Ins", "Break and Enter Dwelling (General)"),
        each = length(which(Crime_Final$Offence == "Break and enter dwelling")))

Crime_Standard[which(Crime_Final$Offence == "House break-ins, actual"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Residential Break Ins", "Break and Enter Dwelling (General)"),
        each = length(which(Crime_Final$Offence == "House break-ins, actual")))

Crime_Standard[which(Crime_Final$Offence == "House break-ins, attempted"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Residential Break Ins", "Attempted Break and Enter Dwelling"),
        each = length(which(Crime_Final$Offence == "House break-ins, attempted")))

Crime_Standard[which(Crime_Final$Offence == "Unlawful Entry With Intent - Dwelling"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Residential Break Ins", "Break and Enter Dwelling With Intent"),
        each = length(which(Crime_Final$Offence == "Unlawful Entry With Intent - Dwelling")))

Crime_Standard[which(Crime_Final$Offence == "Unlawful Entry Without Violence - Dwelling"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Residential Break Ins", "Break and Enter Dwelling Without Violence"),
        each = length(which(Crime_Final$Offence == "Unlawful Entry Without Violence - Dwelling")))

Crime_Standard[which(Crime_Final$Offence == "Unlawful Entry With Violence - Dwelling"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Residential Break Ins", "Break and Enter Dwelling With Violence"),
        each = length(which(Crime_Final$Offence == "Unlawful Entry With Violence - Dwelling")))

### Non Residential Break Ins

Crime_Standard[which(Crime_Final$Offence == "Break and enter non-dwelling"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Non Residential Break Ins", "Break and Enter Non Dwelling (General)"),
        each = length(which(Crime_Final$Offence == "Break and enter non-dwelling")))

Crime_Standard[which(Crime_Final$Offence == "Commercial break-ins, actual"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Non Residential Break Ins", "Commercial Break and Enter"),
        each = length(which(Crime_Final$Offence == "Commercial break-ins, actual")))

Crime_Standard[which(Crime_Final$Offence == "Commercial break-ins, attempted"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Non Residential Break Ins", "Attempted Commercial Break and Enter"),
        each = length(which(Crime_Final$Offence == "Commercial break-ins, attempted")))

Crime_Standard[which(Crime_Final$Offence == "Unlawful Entry With Intent - Shop"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Non Residential Break Ins", "Break and Enter Shop With Intent"),
        each = length(which(Crime_Final$Offence == "Unlawful Entry With Intent - Shop")))

Crime_Standard[which(Crime_Final$Offence == "Unlawful Entry With Intent - Other"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Non Residential Break Ins", "Break and Enter Non Business or Dwelling Intent"),
        each = length(which(Crime_Final$Offence == "Unlawful Entry With Intent - Other")))

### Break Ins (General)

Crime_Standard[which(Crime_Final$Offence == "Unlawful Entry"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Non Residential Break Ins", "Unlawful Entry"),
        each = length(which(Crime_Final$Offence == "Unlawful Entry")))

### Vehicle Theft and Related Offences

Crime_Standard[which(Crime_Final$Offence == "Illegal use of a motor vehicle"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Vehicle Theft and Related Offences", "Illegal Use of a Vehicle"),
        each = length(which(Crime_Final$Offence == "Illegal use of a motor vehicle")))

Crime_Standard[which(Crime_Final$Offence == "Steal from motor vehicle"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Vehicle Theft and Related Offences", "Steal From a Vehicle"),
        each = length(which(Crime_Final$Offence == "Steal from motor vehicle")))

Crime_Standard[which(Crime_Final$Offence == "Motor vehicle theft"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Vehicle Theft and Related Offences", "Vehicle Theft"),
        each = length(which(Crime_Final$Offence == "Motor vehicle theft")))

Crime_Standard[which(Crime_Final$Offence == "Theft of motor vehicle parts or contents"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Vehicle Theft and Related Offences", "Steal From a Vehicle"),
        each = length(which(Crime_Final$Offence == "Theft of motor vehicle parts or contents")))

Crime_Standard[which(Crime_Final$Offence == "Unlawful Use of Motor Vehicle"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Vehicle Theft and Related Offences", "Illegal Use of a Vehicle"),
        each = length(which(Crime_Final$Offence == "Unlawful Use of Motor Vehicle")))

Crime_Standard[which(Crime_Final$Offence == "Vehicles (steal from/enter with intent)"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Vehicle Theft and Related Offences", "Steal From a Vehicle"),
        each = length(which(Crime_Final$Offence == "Vehicles (steal from/enter with intent)")))

Crime_Standard[which(Crime_Final$Offence == "Theft/Illegal Use of MV"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Vehicle Theft and Related Offences", "Vehicle Theft and Related Offences (General)"),
        each = length(which(Crime_Final$Offence == "Theft/Illegal Use of MV")))

Crime_Standard[which(Crime_Final$Offence == "Steal from a motor vehicle"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Vehicle Theft and Related Offences", "Steal From a Vehicle"),
        each = length(which(Crime_Final$Offence == "Steal from a motor vehicle")))

Crime_Standard[which(Crime_Final$Offence == "Stealing From Motor Vehicle (Contents or Parts)"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Vehicle Theft and Related Offences", "Steal From a Vehicle"),
        each = length(which(Crime_Final$Offence == "Stealing From Motor Vehicle (Contents or Parts)")))

Crime_Standard[which(Crime_Final$Offence == "Stealing of Motor Vehicle"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Vehicle Theft and Related Offences", "Vehicle Theft"),
        each = length(which(Crime_Final$Offence == "Stealing of Motor Vehicle")))

### Theft and Related Offences

Crime_Standard[which(Crime_Final$Offence == "Steal from retail store"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Theft and Related Offences", "Steal From Store"),
        each = length(which(Crime_Final$Offence == "Steal from retail store")))

Crime_Standard[which(Crime_Final$Offence == "Steal from dwelling"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Theft and Related Offences", "Steal From Dwelling"),
        each = length(which(Crime_Final$Offence == "Steal from dwelling")))

Crime_Standard[which(Crime_Final$Offence == "Stock theft"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Theft and Related Offences", "Stock Theft"),
        each = length(which(Crime_Final$Offence == "Stock theft")))

Crime_Standard[which(Crime_Final$Offence == "Other theft"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Theft and Related Offences", "Other Theft"),
        each = length(which(Crime_Final$Offence == "Other theft")))

Crime_Standard[which(Crime_Final$Offence == "Theft (excluding Motor Vehicles)"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Theft and Related Offences", "Non Vehicle Theft"),
        each = length(which(Crime_Final$Offence == "Theft (excluding Motor Vehicles)")))

Crime_Standard[which(Crime_Final$Offence == "Theft from motor vehicle"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Theft and Related Offences", "Theft from Vehicle"),
        each = length(which(Crime_Final$Offence == "Theft from motor vehicle")))

Crime_Standard[which(Crime_Final$Offence == "Theft and related offences (other than MV)"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Theft and Related Offences", "Non Vehicle Theft"),
        each = length(which(Crime_Final$Offence == "Theft and related offences (other than MV)")))

Crime_Standard[which(Crime_Final$Offence == "Other Theft (excl. Unlawful Entry)"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Theft and Related Offences", "Other Theft"),
        each = length(which(Crime_Final$Offence == "Other Theft (excl. Unlawful Entry)")))

Crime_Standard[which(Crime_Final$Offence == "Stealing from Dwellings"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Theft and Related Offences", "Steal From Dwelling"),
        each = length(which(Crime_Final$Offence == "Stealing from Dwellings")))

Crime_Standard[which(Crime_Final$Offence == "Shop Stealing"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Theft and Related Offences", "Steal From Store"),
        each = length(which(Crime_Final$Offence == "Shop Stealing")))

Crime_Standard[which(Crime_Final$Offence == "Other Stealing"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Theft and Related Offences", "Other Theft"),
        each = length(which(Crime_Final$Offence == "Other Stealing")))

Crime_Standard[which(Crime_Final$Offence == "Theft from shop"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Theft and Related Offences", "Steal From Store"),
        each = length(which(Crime_Final$Offence == "Theft from shop")))

Crime_Standard[which(Crime_Final$Offence == "Steal from a retail store"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Theft and Related Offences", "Steal From Store"),
        each = length(which(Crime_Final$Offence == "Steal from a retail store")))

Crime_Standard[which(Crime_Final$Offence == "Theft of a bicycle"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Theft and Related Offences", "Theft of a Bicycle"),
        each = length(which(Crime_Final$Offence == "Theft of a bicycle")))

Crime_Standard[which(Crime_Final$Offence == "Stealing From Retail Premises (Shoplift)"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Theft and Related Offences", "Steal From Store"),
        each = length(which(Crime_Final$Offence == "Stealing From Retail Premises (Shoplift)")))

Crime_Standard[which(Crime_Final$Offence == "Stealing From Dwelling"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Theft and Related Offences", "Steal From Dwelling"),
        each = length(which(Crime_Final$Offence == "Stealing From Dwelling")))

Crime_Standard[which(Crime_Final$Offence == "Stealing From Other Premises or Place"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Theft and Related Offences", "Steal From Premises Not Dwelling or Store"),
        each = length(which(Crime_Final$Offence == "Stealing From Other Premises or Place")))

Crime_Standard[which(Crime_Final$Offence == "Stealing as a Servant"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Theft and Related Offences", "Stealing as a Employee"),
        each = length(which(Crime_Final$Offence == "Stealing as a Servant")))

Crime_Standard[which(Crime_Final$Offence == "Stealing (Not Elsewhere Classified)"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Theft and Related Offences", "Other Theft"),
        each = length(which(Crime_Final$Offence == "Stealing (Not Elsewhere Classified)")))

Crime_Standard[which(Crime_Final$Offence == "Steal from person"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Theft and Related Offences", "Steal from Person"),
        each = length(which(Crime_Final$Offence == "Steal from person")))

### Property Damage Offences

Crime_Standard[which(Crime_Final$Offence == "Malicious damage to property"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Property Damage Offences", "Malicious Damage to Property"),
        each = length(which(Crime_Final$Offence == "Malicious damage to property")))

Crime_Standard[which(Crime_Final$Offence == "Property damage"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Property Damage Offences", "Property Damage (General)"),
        each = length(which(Crime_Final$Offence == "Property damage")))

Crime_Standard[which(Crime_Final$Offence == "Property damage offences"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Property Damage Offences", "Property Damage (General)"),
        each = length(which(Crime_Final$Offence == "Property damage offences")))

Crime_Standard[which(Crime_Final$Offence == "Other Property Damage"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Property Damage Offences", "Other Property Damage"),
        each = length(which(Crime_Final$Offence == "Other Property Damage")))

Crime_Standard[which(Crime_Final$Offence == "Other property damage and environmental"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Property Damage Offences", "Other Property Damage"),
        each = length(which(Crime_Final$Offence == "Other property damage and environmental")))

Crime_Standard[which(Crime_Final$Offence == "Criminal damage"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Property Damage Offences", "Property Damage (General)"),
        each = length(which(Crime_Final$Offence == "Criminal damage")))

Crime_Standard[which(Crime_Final$Offence == "Other property damage offences"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Property Damage Offences", "Other Property Damage"),
        each = length(which(Crime_Final$Offence == "Other property damage offences")))

Crime_Standard[which(Crime_Final$Offence == "Damage"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Property Damage Offences", "Property Damage (General)"),
        each = length(which(Crime_Final$Offence == "Damage")))

Crime_Standard[which(Crime_Final$Offence == "Graffiti"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Property Damage Offences", "Graffiti"),
        each = length(which(Crime_Final$Offence == "Graffiti")))

Crime_Standard[which(Crime_Final$Offence == "Criminal Damage"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Property Damage Offences", "Property Damage (General)"),
        each = length(which(Crime_Final$Offence == "Criminal Damage")))

### Fraud and Deception Related Offences

Crime_Standard[which(Crime_Final$Offence == "Fraud"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Fraud and Deception Related Offences", "Fraud (General)"),
        each = length(which(Crime_Final$Offence == "Fraud")))

Crime_Standard[which(Crime_Final$Offence == "Fraud by Computer"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Fraud and Deception Related Offences", "Fraud by Computer"),
        each = length(which(Crime_Final$Offence == "Fraud by Computer")))

Crime_Standard[which(Crime_Final$Offence == "Fraud by Cheque"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Fraud and Deception Related Offences", "Fraud by Cheque"),
        each = length(which(Crime_Final$Offence == "Fraud by Cheque")))

Crime_Standard[which(Crime_Final$Offence == "Fraud by Credit Card"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Fraud and Deception Related Offences", "Fraud by Credit Card"),
        each = length(which(Crime_Final$Offence == "Fraud by Credit Card")))

Crime_Standard[which(Crime_Final$Offence == "Identity Fraud"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Fraud and Deception Related Offences", "Identity Fraud"),
        each = length(which(Crime_Final$Offence == "Identity Fraud")))

Crime_Standard[which(Crime_Final$Offence == "Other Fraud"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Fraud and Deception Related Offences", "Other Fraud"),
        each = length(which(Crime_Final$Offence == "Other Fraud")))

Crime_Standard[which(Crime_Final$Offence == "Obtain benefit by deception"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Fraud and Deception Related Offences", "Obtain Benefit by Deception"),
        each = length(which(Crime_Final$Offence == "Obtain benefit by deception")))

Crime_Standard[which(Crime_Final$Offence == "Other fraud, deception and related offences"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Fraud and Deception Related Offences", "Other Fraud and Deception Related Offences"),
        each = length(which(Crime_Final$Offence == "Other fraud, deception and related offences")))

Crime_Standard[which(Crime_Final$Offence == "Fare evasion"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Fraud and Deception Related Offences", "Fare evasion"),
        each = length(which(Crime_Final$Offence == "Fare evasion")))

Crime_Standard[which(Crime_Final$Offence == "Forgery and counterfeiting"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Fraud and Deception Related Offences", "Forgery and counterfeiting"),
        each = length(which(Crime_Final$Offence == "Forgery and counterfeiting")))

Crime_Standard[which(Crime_Final$Offence == "State false information"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Fraud and Deception Related Offences", "State False Information"),
        each = length(which(Crime_Final$Offence == "State false information")))

Crime_Standard[which(Crime_Final$Offence == "Deceptive business practices"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Fraud and Deception Related Offences", "Deceptive business Practices"),
        each = length(which(Crime_Final$Offence == "Deceptive business practices")))

Crime_Standard[which(Crime_Final$Offence == "Professional malpractice and misrepresentation"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Fraud and Deception Related Offences", "Professional Malpractice and Misrepresentation"),
        each = length(which(Crime_Final$Offence == "Professional malpractice and misrepresentation")))

Crime_Standard[which(Crime_Final$Offence == "Other deception offences"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Fraud and Deception Related Offences", "Other Deception"),
        each = length(which(Crime_Final$Offence == "Other deception offences")))

Crime_Standard[which(Crime_Final$Offence == "Forgery"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Fraud and Deception Related Offences", "Forgery"),
        each = length(which(Crime_Final$Offence == "Forgery")))

Crime_Standard[which(Crime_Final$Offence == "Fraud (Credit Card)"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Fraud and Deception Related Offences", "Fraud by Credit Card"),
        each = length(which(Crime_Final$Offence == "Fraud (Credit Card)")))

Crime_Standard[which(Crime_Final$Offence == "Fraud (Not Elsewhere Classified)"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Fraud and Deception Related Offences", "Other Fraud"),
        each = length(which(Crime_Final$Offence == "Fraud (Not Elsewhere Classified)")))

Crime_Standard[which(Crime_Final$Offence == "Extortion"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Fraud and Deception Related Offences", "Blackmail and Extortion"),
        each = length(which(Crime_Final$Offence == "Extortion")))

Crime_Standard[which(Crime_Final$Offence == "Blackmail and extortion"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Fraud and Deception Related Offences", "Blackmail and Extortion"),
        each = length(which(Crime_Final$Offence == "Blackmail and extortion")))

Crime_Standard[which(Crime_Final$Offence == "Possess equipment to make false instrument"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Fraud and Deception Related Offences", "Possess Equipment to Make False Instrument"),
        each = length(which(Crime_Final$Offence == "Possess equipment to make false instrument")))

### Trespass and Vagrancy Offences

Crime_Standard[which(Crime_Final$Offence == "Trespass"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Trespass and Vagrancy Offences", "Trespass and Vagrancy (General)"),
        each = length(which(Crime_Final$Offence == "Trespass")))

Crime_Standard[which(Crime_Final$Offence == "Trespassing and Vagrancy"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Trespass and Vagrancy Offences", "Trespass and Vagrancy (General)"),
        each = length(which(Crime_Final$Offence == "Trespassing and Vagrancy")))

Crime_Standard[which(Crime_Final$Offence == "SCT - Residence"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Trespass and Vagrancy Offences", "Resident Trespass"),
        each = length(which(Crime_Final$Offence == "SCT - Residence")))

Crime_Standard[which(Crime_Final$Offence == "SCT - Non Residence"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Trespass and Vagrancy Offences", "Non Resident Trespass"),
        each = length(which(Crime_Final$Offence == "SCT - Non Residence")))

### Burglary

Crime_Standard[which(Crime_Final$Offence == "Burglary"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Burglary", "Burglary (General)"),
        each = length(which(Crime_Final$Offence == "Burglary")))

Crime_Standard[which(Crime_Final$Offence == "Unknown non-aggravated burglary"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Burglary", "Non Aggrivated Burglary (General)"),
        each = length(which(Crime_Final$Offence == "Unknown non-aggravated burglary")))

Crime_Standard[which(Crime_Final$Offence == "Unknown aggravated burglary"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Burglary", "Aggrivated Burglary (General)"),
        each = length(which(Crime_Final$Offence == "Unknown aggravated burglary")))

Crime_Standard[which(Crime_Final$Offence == "Non-residential aggravated burglary"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Burglary", "Non Dwelling Aggrivated Burglary"),
        each = length(which(Crime_Final$Offence == "Non-residential aggravated burglary")))

Crime_Standard[which(Crime_Final$Offence == "Non-residential non-aggravated burglary"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Burglary", "Non Dwelling Non Aggrivated Burglary"),
        each = length(which(Crime_Final$Offence == "Non-residential non-aggravated burglary")))

Crime_Standard[which(Crime_Final$Offence == "Burglary (Non-Dwelling)"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Burglary", "Non Dwelling Burglary (General)"),
        each = length(which(Crime_Final$Offence == "Burglary (Non-Dwelling)")))

Crime_Standard[which(Crime_Final$Offence == "Residential aggravated burglary"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Burglary", "Dwelling Aggrivated Burglary"),
        each = length(which(Crime_Final$Offence == "Residential aggravated burglary")))

Crime_Standard[which(Crime_Final$Offence == "Residential non-aggravated burglary"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Burglary", "Dwelling Non Aggrivated Burglary"),
        each = length(which(Crime_Final$Offence == "Residential non-aggravated burglary")))

Crime_Standard[which(Crime_Final$Offence == "Burglary (Dwelling)"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Burglary", "Dwelling Burglary (General)"),
        each = length(which(Crime_Final$Offence == "Burglary (Dwelling)")))

### Receiving and Possession of Stolen Property

Crime_Standard[which(Crime_Final$Offence == "Possess Stolen Property"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Receiving and Possession of Stolen Property", "Possess Stolen Property"),
        each = length(which(Crime_Final$Offence == "Possess Stolen Property")))

Crime_Standard[which(Crime_Final$Offence == "Possess Property Suspected Stolen"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Receiving and Possession of Stolen Property", "Possess Property Suspected Stolen"),
        each = length(which(Crime_Final$Offence == "Possess Property Suspected Stolen")))

Crime_Standard[which(Crime_Final$Offence == "Receiving Stolen Property"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Receiving and Possession of Stolen Property", "Receiving Stolen Property"),
        each = length(which(Crime_Final$Offence == "Receiving Stolen Property")))

Crime_Standard[which(Crime_Final$Offence == "Possess etc. Tainted Property"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Receiving and Possession of Stolen Property", "Possess Tainted Property"),
        each = length(which(Crime_Final$Offence == "Possess etc. Tainted Property")))

Crime_Standard[which(Crime_Final$Offence == "Other Handling Stolen Goods"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Receiving and Possession of Stolen Property", "Other Handling Stolen Goods"),
        each = length(which(Crime_Final$Offence == "Other Handling Stolen Goods")))

Crime_Standard[which(Crime_Final$Offence == "Receiving or handling stolen goods"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Receiving and Possession of Stolen Property", "Receiving Possession of Stolen Property (General)"),
        each = length(which(Crime_Final$Offence == "Receiving or handling stolen goods")))

Crime_Standard[which(Crime_Final$Offence == "Receive or handle proceeds of crime"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Receiving and Possession of Stolen Property", "Receiving Possession of Stolen Property (General)"),
        each = length(which(Crime_Final$Offence == "Receive or handle proceeds of crime")))

Crime_Standard[which(Crime_Final$Offence == "Other Handling Stolen Goods"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Receiving and Possession of Stolen Property", "Other Handling Stolen Goods"),
        each = length(which(Crime_Final$Offence == "Other Handling Stolen Goods")))

Crime_Standard[which(Crime_Final$Offence == "Receiving or handling stolen goods"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Receiving and Possession of Stolen Property", "Receiving Possession of Stolen Property (General)"),
        each = length(which(Crime_Final$Offence == "Receiving or handling stolen goods")))

Crime_Standard[which(Crime_Final$Offence == "Receive or handle proceeds of crime"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Receiving and Possession of Stolen Property", "Receiving Possession of Stolen Property (General)"),
        each = length(which(Crime_Final$Offence == "Receive or handle proceeds of crime")))

### Arson and Fire Related Offences

Crime_Standard[which(Crime_Final$Offence == "Arson"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Arson and Fire Related Offences", "Arson"),
        each = length(which(Crime_Final$Offence == "Arson")))

Crime_Standard[which(Crime_Final$Offence == "Property damage by fire or explosion"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Arson and Fire Related Offences", "Arson"),
        each = length(which(Crime_Final$Offence == "Property damage by fire or explosion")))

Crime_Standard[which(Crime_Final$Offence == "Cause damage by fire"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Arson and Fire Related Offences", "Arson"),
        each = length(which(Crime_Final$Offence == "Cause damage by fire")))

Crime_Standard[which(Crime_Final$Offence == "Cause Damage by Fire"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Arson and Fire Related Offences", "Arson"),
        each = length(which(Crime_Final$Offence == "Cause Damage by Fire")))

Crime_Standard[which(Crime_Final$Offence == "Cause a bushfire"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Arson and Fire Related Offences", "Cause Bushfire"),
        each = length(which(Crime_Final$Offence == "Cause a bushfire")))

Crime_Standard[which(Crime_Final$Offence == "Other fire related offences"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Arson and Fire Related Offences", "Other Fire Related Offences"),
        each = length(which(Crime_Final$Offence == "Other fire related offences")))

Crime_Standard[which(Crime_Final$Offence == "Cause Bushfire"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Arson and Fire Related Offences", "Cause Bushfire"),
        each = length(which(Crime_Final$Offence == "Cause Bushfire")))

Crime_Standard[which(Crime_Final$Offence == "Other Fire Related Offences"), c(7, 8, 9)] = 
    rep(c("Property and Deception Offences", "Arson and Fire Related Offences", "Other Fire Related Offences"),
        each = length(which(Crime_Final$Offence == "Other Fire Related Offences")))

# Drug Offences

### Drug Dealing and Trafficking

Crime_Standard[which(Crime_Final$Offence == "Dealing, trafficking in cocaine"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Drug Dealing and Trafficking", "Dealing or Trafficking Cocaine"),
        each = length(which(Crime_Final$Offence == "Dealing, trafficking in cocaine")))

Crime_Standard[which(Crime_Final$Offence == "Dealing, trafficking in narcotics"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Drug Dealing and Trafficking", "Dealing or Trafficking Narcotics"),
        each = length(which(Crime_Final$Offence == "Dealing, trafficking in narcotics")))

Crime_Standard[which(Crime_Final$Offence == "Dealing, trafficking in cannabis"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Drug Dealing and Trafficking", "Dealing or Trafficking Cannabis"),
        each = length(which(Crime_Final$Offence == "Dealing, trafficking in cannabis")))

Crime_Standard[which(Crime_Final$Offence == "Dealing, trafficking in amphetamines"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Drug Dealing and Trafficking", "Dealing or Trafficking Amphetamines"),
        each = length(which(Crime_Final$Offence == "Dealing, trafficking in amphetamines")))

Crime_Standard[which(Crime_Final$Offence == "Dealing, trafficking in ecstasy"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Drug Dealing and Trafficking", "Dealing or Trafficking Ecstasy"),
        each = length(which(Crime_Final$Offence == "Dealing, trafficking in ecstasy")))

Crime_Standard[which(Crime_Final$Offence == "Dealing, trafficking in other drugs"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Drug Dealing and Trafficking", "Dealing or Trafficking Other Drugs"),
        each = length(which(Crime_Final$Offence == "Dealing, trafficking in other drugs")))

Crime_Standard[which(Crime_Final$Offence == "Trafficking Drugs"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Drug Dealing and Trafficking", "Trafficking Drugs (General)"),
        each = length(which(Crime_Final$Offence == "Trafficking Drugs")))

Crime_Standard[which(Crime_Final$Offence == "Sell Supply Drugs"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Drug Dealing and Trafficking", "Drug Dealing (General)"),
        each = length(which(Crime_Final$Offence == "Sell Supply Drugs")))

Crime_Standard[which(Crime_Final$Offence == "Drug trafficking"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Drug Dealing and Trafficking", "Trafficking Drugs (General)"),
        each = length(which(Crime_Final$Offence == "Drug trafficking")))

Crime_Standard[which(Crime_Final$Offence == "Drug dealing"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Drug Dealing and Trafficking", "Drug Dealing (General)"),
        each = length(which(Crime_Final$Offence == "Drug dealing")))

Crime_Standard[which(Crime_Final$Offence == "Drug Dealing"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Drug Dealing and Trafficking", "Drug Dealing (General)"),
        each = length(which(Crime_Final$Offence == "Drug Dealing")))

### Cultivate or Manufacture Drugs

Crime_Standard[which(Crime_Final$Offence == "Cultivating cannabis"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Cultivate or Manufacture Drugs", "Cultivating Cannabis"),
        each = length(which(Crime_Final$Offence == "Cultivating cannabis")))

Crime_Standard[which(Crime_Final$Offence == "Manufacture drug"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Cultivate or Manufacture Drugs", "Manufacture Drugs (General)"),
        each = length(which(Crime_Final$Offence == "Manufacture drug")))

Crime_Standard[which(Crime_Final$Offence == "Produce Drugs"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Cultivate or Manufacture Drugs", "Manufacture Drugs (General)"),
        each = length(which(Crime_Final$Offence == "Produce Drugs")))

Crime_Standard[which(Crime_Final$Offence == "Cultivate drugs"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Cultivate or Manufacture Drugs", "Cultivate Drugs (General)"),
        each = length(which(Crime_Final$Offence == "Cultivate drugs")))

Crime_Standard[which(Crime_Final$Offence == "Manufacture drugs"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Cultivate or Manufacture Drugs", "Manufacture Drugs (General)"),
        each = length(which(Crime_Final$Offence == "Manufacture drugs")))

Crime_Standard[which(Crime_Final$Offence == "Cultivate or Manufacture Drugs"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Cultivate or Manufacture Drugs", "Cultivate or Manufacture Drugs (General)"),
        each = length(which(Crime_Final$Offence == "Cultivate or Manufacture Drugs")))

### Drug Use and Possession

Crime_Standard[which(Crime_Final$Offence == "Possession and/or use of cocaine"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Drug Use and Possession", "Possession or Use of Cocaine"),
        each = length(which(Crime_Final$Offence == "Possession and/or use of cocaine")))

Crime_Standard[which(Crime_Final$Offence == "Possession and/or use of narcotics"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Drug Use and Possession", "Possession or Use of Narcotics"),
        each = length(which(Crime_Final$Offence == "Possession and/or use of narcotics")))

Crime_Standard[which(Crime_Final$Offence == "Possession and/or use of cannabis"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Drug Use and Possession", "Possession or Use of Cannabis"),
        each = length(which(Crime_Final$Offence == "Possession and/or use of cannabis")))

Crime_Standard[which(Crime_Final$Offence == "Possession and/or use of amphetamines"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Drug Use and Possession", "Possession or Use of Amphetamines"),
        each = length(which(Crime_Final$Offence == "Possession and/or use of amphetamines")))

Crime_Standard[which(Crime_Final$Offence == "Possession and/or use of ecstasy"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Drug Use and Possession", "Possession or Use of Ecstasy"),
        each = length(which(Crime_Final$Offence == "Possession and/or use of ecstasy")))

Crime_Standard[which(Crime_Final$Offence == "Possession and/or use of other drugs"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Drug Use and Possession", "Possession or Use of Other Drugs"),
        each = length(which(Crime_Final$Offence == "Possession and/or use of other drugs")))

Crime_Standard[which(Crime_Final$Offence == "Possess Drugs"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Drug Use and Possession", "Possession of Drugs (General)"),
        each = length(which(Crime_Final$Offence == "Possess Drugs")))

Crime_Standard[which(Crime_Final$Offence == "Drug use"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Drug Use and Possession", "Drug Use (General)"),
        each = length(which(Crime_Final$Offence == "Drug use")))

Crime_Standard[which(Crime_Final$Offence == "Drug possession"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Drug Use and Possession", "Possession of Drugs (General)"),
        each = length(which(Crime_Final$Offence == "Drug possession")))

Crime_Standard[which(Crime_Final$Offence == "Drug Possession"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Drug Use and Possession", "Possession of Drugs (General)"),
        each = length(which(Crime_Final$Offence == "Drug Possession")))

### Other Drug Offences

Crime_Standard[which(Crime_Final$Offence == "Importing drugs"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Other Drug Offences", "Importing Drugs"),
        each = length(which(Crime_Final$Offence == "Importing drugs")))

Crime_Standard[which(Crime_Final$Offence == "Other drug offences"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Other Drug Offences", "Other Drug Offences"),
        each = length(which(Crime_Final$Offence == "Other drug offences")))

Crime_Standard[which(Crime_Final$Offence == "Liquor Offences"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Other Drug Offences", "Liquor Offences"),
        each = length(which(Crime_Final$Offence == "Liquor Offences")))

Crime_Standard[which(Crime_Final$Offence == "Liquor offences"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Other Drug Offences", "Liquor Offences"),
        each = length(which(Crime_Final$Offence == "Liquor offences")))

Crime_Standard[which(Crime_Final$Offence == "Other Drug Offences"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Other Drug Offences", "Other Drug Offences"),
        each = length(which(Crime_Final$Offence == "Other Drug Offences")))

Crime_Standard[which(Crime_Final$Offence == "Liquor (excl. Drunkenness)"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Other Drug Offences", "Liquor Offences"),
        each = length(which(Crime_Final$Offence == "Liquor (excl. Drunkenness)")))

Crime_Standard[which(Crime_Final$Offence == "Possess drug manufacturing equipment or precursor"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Other Drug Offences", "Possess Drug Manufacturing Equipment or Precursor"),
        each = length(which(Crime_Final$Offence == "Possess drug manufacturing equipment or precursor")))

Crime_Standard[which(Crime_Final$Offence == "Possession of Drug Paraphernalia"), c(7, 8, 9)] = 
    rep(c("Drug Offences", "Other Drug Offences", "Possession of Drug Paraphernalia"),
        each = length(which(Crime_Final$Offence == "Possession of Drug Paraphernalia")))

# Public Order and Security Offences

### Weapons and Explosives Offences

Crime_Standard[which(Crime_Final$Offence == "Prohibited and regulated weapons offences"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Weapons and Explosives Offences", "Prohibited and Regulated Weapons Offences"),
        each = length(which(Crime_Final$Offence == "Prohibited and regulated weapons offences")))

Crime_Standard[which(Crime_Final$Offence == "Unlawful Possess Concealable Firearm"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Weapons and Explosives Offences", "Unlawful Possess Concealable Firearm"),
        each = length(which(Crime_Final$Offence == "Unlawful Possess Concealable Firearm")))

Crime_Standard[which(Crime_Final$Offence == "Unlawful Possess Firearm - Other"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Weapons and Explosives Offences", "Other Unlawful Possession of a Firearm"),
        each = length(which(Crime_Final$Offence == "Unlawful Possess Firearm - Other")))

Crime_Standard[which(Crime_Final$Offence == "Bomb Possess and/or use of"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Weapons and Explosives Offences", "Bomb Possession or Use"),
        each = length(which(Crime_Final$Offence == "Bomb Possess and/or use of")))

Crime_Standard[which(Crime_Final$Offence == "Possess and/or use other weapons; restricted items"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Weapons and Explosives Offences", "Possession or Use of Other Restricted Items"),
        each = length(which(Crime_Final$Offence == "Possess and/or use other weapons; restricted items")))

Crime_Standard[which(Crime_Final$Offence == "Weapons Act Offences - Other"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Weapons and Explosives Offences", "Other Weapons and Explosives Offences"),
        each = length(which(Crime_Final$Offence == "Weapons Act Offences - Other")))

Crime_Standard[which(Crime_Final$Offence == "Throw or discharge object endangering people"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Weapons and Explosives Offences", "Throw or Discharge Object Endangering People"),
        each = length(which(Crime_Final$Offence == "Throw or discharge object endangering people")))

Crime_Standard[which(Crime_Final$Offence == "Firearms offences"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Weapons and Explosives Offences", "Firearms Offences (General)"),
        each = length(which(Crime_Final$Offence == "Firearms offences")))

Crime_Standard[which(Crime_Final$Offence == "Prohibited and controlled weapons offences"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Weapons and Explosives Offences", "Prohibited and Regulated Weapons Offences"),
        each = length(which(Crime_Final$Offence == "Prohibited and controlled weapons offences")))

Crime_Standard[which(Crime_Final$Offence == "Explosives offences"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Weapons and Explosives Offences", "Explosives Offences (General)"),
        each = length(which(Crime_Final$Offence == "Explosives offences")))

Crime_Standard[which(Crime_Final$Offence == "Regulated Weapons Offences"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Weapons and Explosives Offences", "Prohibited and Regulated Weapons Offences"),
        each = length(which(Crime_Final$Offence == "Regulated Weapons Offences")))

### Disorderly and Offensive Conduct

Crime_Standard[which(Crime_Final$Offence == "Offensive conduct"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Disorderly and Offensive Conduct", "Offensive Conduct"),
        each = length(which(Crime_Final$Offence == "Offensive conduct")))

Crime_Standard[which(Crime_Final$Offence == "Offensive language"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Disorderly and Offensive Conduct", "Offensive Language"),
        each = length(which(Crime_Final$Offence == "Offensive language")))

Crime_Standard[which(Crime_Final$Offence == "Drunk and disorderly in public"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Disorderly and Offensive Conduct", "Drunk and Disorderly in Public"),
        each = length(which(Crime_Final$Offence == "Drunk and disorderly in public")))

Crime_Standard[which(Crime_Final$Offence == "Riot and affray"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Disorderly and Offensive Conduct", "Riot and Affray"),
        each = length(which(Crime_Final$Offence == "Riot and affray")))

Crime_Standard[which(Crime_Final$Offence == "Disorderly conduct"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Disorderly and Offensive Conduct", "Disorderly Conduct"),
        each = length(which(Crime_Final$Offence == "Disorderly conduct")))

### Public Nuisance Offences

Crime_Standard[which(Crime_Final$Offence == "Privacy offences"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Public Nuisance Offences", "Privacy Offences"),
        each = length(which(Crime_Final$Offence == "Privacy offences")))

Crime_Standard[which(Crime_Final$Offence == "Hoaxes"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Public Nuisance Offences", "Hoaxes"),
        each = length(which(Crime_Final$Offence == "Hoaxes")))

Crime_Standard[which(Crime_Final$Offence == "Begging"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Public Nuisance Offences", "Begging"),
        each = length(which(Crime_Final$Offence == "Begging")))

Crime_Standard[which(Crime_Final$Offence == "Improper movement on public or private space"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Public Nuisance Offences", "Improper Movement on Public or Private Space"),
        each = length(which(Crime_Final$Offence == "Improper movement on public or private space")))

Crime_Standard[which(Crime_Final$Offence == "Other public nuisance offences"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Public Nuisance Offences", "Other Public Nuisance Offences"),
        each = length(which(Crime_Final$Offence == "Other public nuisance offences")))

Crime_Standard[which(Crime_Final$Offence == "Defamation and libel"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Public Nuisance Offences", "Defamation and Libel"),
        each = length(which(Crime_Final$Offence == "Defamation and libel")))

Crime_Standard[which(Crime_Final$Offence == "Public Nuisance"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Public Nuisance Offences", "Public Nuisance (General)"),
        each = length(which(Crime_Final$Offence == "Public Nuisance")))

### Public Security Offences

Crime_Standard[which(Crime_Final$Offence == "Terrorism offences"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Public Security Offences", "Terrorism Offences"),
        each = length(which(Crime_Final$Offence == "Terrorism offences")))

Crime_Standard[which(Crime_Final$Offence == "Other public security offences"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Public Security Offences", "Other Public Security Offences"),
        each = length(which(Crime_Final$Offence == "Other public security offences")))

Crime_Standard[which(Crime_Final$Offence == "Sabotage"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Public Security Offences", "Sabotage"),
        each = length(which(Crime_Final$Offence == "Sabotage")))

Crime_Standard[which(Crime_Final$Offence == "Hacking"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Public Security Offences", "Hacking"),
        each = length(which(Crime_Final$Offence == "Hacking")))

Crime_Standard[which(Crime_Final$Offence == "Immigration offences"), c(7, 8, 9)] = 
    rep(c("Public Order and Security Offences", "Public Security Offences", "Immigration Offences"),
        each = length(which(Crime_Final$Offence == "Immigration offences")))

# Other Offences

### Transport Regulation Offences

Crime_Standard[which(Crime_Final$Offence == "Traffic Infringement Notices"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Traffic Infringement Notices"),
        each = length(which(Crime_Final$Offence == "Traffic Infringement Notices")))

Crime_Standard[which(Crime_Final$Offence == "Road fatality"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Driving Causing Death"),
        each = length(which(Crime_Final$Offence == "Road fatality")))

Crime_Standard[which(Crime_Final$Offence == "Road collision with injury"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Road Collision with Injury"),
        each = length(which(Crime_Final$Offence == "Road collision with injury")))

Crime_Standard[which(Crime_Final$Offence == "Dangerous Operation of a Vehicle"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Dangerous Operation of a Vehicle"),
        each = length(which(Crime_Final$Offence == "Dangerous Operation of a Vehicle")))

Crime_Standard[which(Crime_Final$Offence == "Drink Driving"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Drink Driving"),
        each = length(which(Crime_Final$Offence == "Drink Driving")))

Crime_Standard[which(Crime_Final$Offence == "Disqualified Driving"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Disqualified Driving"),
        each = length(which(Crime_Final$Offence == "Disqualified Driving")))

Crime_Standard[which(Crime_Final$Offence == "Interfere with Mechanism of Motor Vehicle"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Interfere with Mechanism of Motor Vehicle"),
        each = length(which(Crime_Final$Offence == "Interfere with Mechanism of Motor Vehicle")))

Crime_Standard[which(Crime_Final$Offence == "Dangerous driving"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Dangerous Operation of a Vehicle"),
        each = length(which(Crime_Final$Offence == "Dangerous driving")))

Crime_Standard[which(Crime_Final$Offence == "Public transport"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Public Transport Offences"),
        each = length(which(Crime_Final$Offence == "Public transport")))

Crime_Standard[which(Crime_Final$Offence == "Maritime regulations offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Maritime Regulations Offences"),
        each = length(which(Crime_Final$Offence == "Maritime regulations offences")))

Crime_Standard[which(Crime_Final$Offence == "Other transport regulation offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Other Transport Regulation Offences"),
        each = length(which(Crime_Final$Offence == "Other transport regulation offences")))

Crime_Standard[which(Crime_Final$Offence == "Aviation regulations offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Aviation Regulations Offences"),
        each = length(which(Crime_Final$Offence == "Aviation regulations offences")))

Crime_Standard[which(Crime_Final$Offence == "Transport regulatory offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Transport Regulation Offences (General)"),
        each = length(which(Crime_Final$Offence == "Transport regulatory offences")))

Crime_Standard[which(Crime_Final$Offence == "Driving causing death"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Driving Causing Death"),
        each = length(which(Crime_Final$Offence == "Driving causing death")))

Crime_Standard[which(Crime_Final$Offence == "Driving Causing Death"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Driving Causing Death"),
        each = length(which(Crime_Final$Offence == "Driving Causing Death")))

Crime_Standard[which(Crime_Final$Offence == "Other regulatory driving offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Other Regulatory Driving Offences"),
        each = length(which(Crime_Final$Offence == "Other regulatory driving offences")))

Crime_Standard[which(Crime_Final$Offence == "Drink driving"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Drink Driving"),
        each = length(which(Crime_Final$Offence == "Drink driving")))

Crime_Standard[which(Crime_Final$Offence == "Drug driving"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Drug Driving"),
        each = length(which(Crime_Final$Offence == "Drug driving")))

Crime_Standard[which(Crime_Final$Offence == "Parking offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Parking Offences"),
        each = length(which(Crime_Final$Offence == "Parking offences")))

Crime_Standard[which(Crime_Final$Offence == "Pedestrian offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Pedestrian Offences"),
        each = length(which(Crime_Final$Offence == "Pedestrian offences")))

Crime_Standard[which(Crime_Final$Offence == "Licensing offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Licensing Offences"),
        each = length(which(Crime_Final$Offence == "Licensing offences")))

Crime_Standard[which(Crime_Final$Offence == "Registration and roadworthiness offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Transport Regulation Offences", "Registration and Roadworthiness Offences"),
        each = length(which(Crime_Final$Offence == "Registration and roadworthiness offences")))

### Other Government Regulatory Offences

Crime_Standard[which(Crime_Final$Offence == "Commercial regulation offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Other Government Regulatory Offences", "Commercial Regulation Offences"),
        each = length(which(Crime_Final$Offence == "Commercial regulation offences")))

Crime_Standard[which(Crime_Final$Offence == "Liquor and tobacco licensing offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Other Government Regulatory Offences", "Liquor and Tobacco Licensing Offences"),
        each = length(which(Crime_Final$Offence == "Liquor and tobacco licensing offences")))

Crime_Standard[which(Crime_Final$Offence == "Other government regulatory offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Other Government Regulatory Offences", "Other Government Regulatory Offences"),
        each = length(which(Crime_Final$Offence == "Other government regulatory offences")))

Crime_Standard[which(Crime_Final$Offence == "Intellectual property"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Other Government Regulatory Offences", "Intellectual Property Offences"),
        each = length(which(Crime_Final$Offence == "Intellectual property")))

Crime_Standard[which(Crime_Final$Offence == "Pornography and censorship offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Other Government Regulatory Offences", "Pornography and Censorship Offences"),
        each = length(which(Crime_Final$Offence == "Pornography and censorship offences")))

Crime_Standard[which(Crime_Final$Offence == "Pornography offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Other Government Regulatory Offences", "Pornography and Censorship Offences"),
        each = length(which(Crime_Final$Offence == "Pornography offences")))

### Miscellaneous Offences

Crime_Standard[which(Crime_Final$Offence == "Other miscellaneous offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Miscellaneous Offences", "Other Miscellaneous Offences"),
        each = length(which(Crime_Final$Offence == "Other miscellaneous offences")))

Crime_Standard[which(Crime_Final$Offence == "Other offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Miscellaneous Offences", "Miscellaneous Offences (General)"),
        each = length(which(Crime_Final$Offence == "Other offences")))

Crime_Standard[which(Crime_Final$Offence == "Other Offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Miscellaneous Offences", "Miscellaneous Offences (General)"),
        each = length(which(Crime_Final$Offence == "Other Offences")))

Crime_Standard[which(Crime_Final$Offence == "Miscellaneous Offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Miscellaneous Offences", "Miscellaneous Offences (General)"),
        each = length(which(Crime_Final$Offence == "Miscellaneous Offences")))

Crime_Standard[which(Crime_Final$Offence == "Stock Related Offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Miscellaneous Offences", "Stock Related Offences"),
        each = length(which(Crime_Final$Offence == "Stock Related Offences")))

Crime_Standard[which(Crime_Final$Offence == "Cruelty to animals"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Miscellaneous Offences", "Cruelty to Animals"),
        each = length(which(Crime_Final$Offence == "Cruelty to animals")))

Crime_Standard[which(Crime_Final$Offence == "Dangerous substance offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Miscellaneous Offences", "Dangerous Substance Offences"),
        each = length(which(Crime_Final$Offence == "Dangerous substance offences")))

Crime_Standard[which(Crime_Final$Offence == "Public health and safety offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Miscellaneous Offences", "Public Health and Safety Offences"),
        each = length(which(Crime_Final$Offence == "Public health and safety offences")))

Crime_Standard[which(Crime_Final$Offence == "Environmental offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Miscellaneous Offences", "Environmental Offences"),
        each = length(which(Crime_Final$Offence == "Environmental offences")))

Crime_Standard[which(Crime_Final$Offence == "Bribery of officials"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Miscellaneous Offences", "Bribery of Officials"),
        each = length(which(Crime_Final$Offence == "Bribery of officials")))

Crime_Standard[which(Crime_Final$Offence == "Criminal intent"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Miscellaneous Offences", "Criminal Intent"),
        each = length(which(Crime_Final$Offence == "Criminal intent")))

### Justice Procedures

Crime_Standard[which(Crime_Final$Offence == "Pervert the course of justice or commit perjury"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Justice Procedures", "Pervert the Course of Justice or Commit Perjury"),
        each = length(which(Crime_Final$Offence == "Pervert the course of justice or commit perjury")))

Crime_Standard[which(Crime_Final$Offence == "Prison regulation offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Justice Procedures", "Prison Regulation Offences"),
        each = length(which(Crime_Final$Offence == "Prison regulation offences")))

Crime_Standard[which(Crime_Final$Offence == "Other justice procedures offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Justice Procedures", "Other Justice Procedures Offences"),
        each = length(which(Crime_Final$Offence == "Other justice procedures offences")))

Crime_Standard[which(Crime_Final$Offence == "Fail to appear"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Justice Procedures", "Failure to Appear"),
        each = length(which(Crime_Final$Offence == "Fail to appear")))

Crime_Standard[which(Crime_Final$Offence == "Fare Evasion"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Justice Procedures", "Fare Evasion"),
        each = length(which(Crime_Final$Offence == "Fare Evasion")))

### Breaches of Orders

Crime_Standard[which(Crime_Final$Offence == "Breach Apprehended Violence Order"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Breaches of Orders", "Breach Apprehended Violence Order"),
        each = length(which(Crime_Final$Offence == "Breach Apprehended Violence Order")))

Crime_Standard[which(Crime_Final$Offence == "Breach bail conditions"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Breaches of Orders", "Breach Bail Conditions"),
        each = length(which(Crime_Final$Offence == "Breach bail conditions")))

Crime_Standard[which(Crime_Final$Offence == "Breach Domestic Violence Protection Order"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Breaches of Orders", "Breach Domestic Violence Protection Order"),
        each = length(which(Crime_Final$Offence == "Breach Domestic Violence Protection Order")))

Crime_Standard[which(Crime_Final$Offence == "Breach family violence order"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Breaches of Orders", "Breach Domestic Violence Protection Order"),
        each = length(which(Crime_Final$Offence == "Breach family violence order")))

Crime_Standard[which(Crime_Final$Offence == "Breach intervention order"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Breaches of Orders", "Breach Intervention Order"),
        each = length(which(Crime_Final$Offence == "Breach intervention order")))

Crime_Standard[which(Crime_Final$Offence == "Breach of other orders"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Breaches of Orders", "Breach of Other Orders"),
        each = length(which(Crime_Final$Offence == "Breach of other orders")))

Crime_Standard[which(Crime_Final$Offence == "Breach of Family Violence Restraint Order"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Breaches of Orders", "Breach of Family Violence Restraint Order"),
        each = length(which(Crime_Final$Offence == "Breach of Family Violence Restraint Order")))

Crime_Standard[which(Crime_Final$Offence == "Breach of Violence Restraint Order"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Breaches of Orders", "Breach of Violence Restraint Order"),
        each = length(which(Crime_Final$Offence == "Breach of Violence Restraint Order")))

Crime_Standard[which(Crime_Final$Offence == "Breach of Police Order"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Breaches of Orders", "Breach of Police Order"),
        each = length(which(Crime_Final$Offence == "Breach of Police Order")))

### Prostitution Offences

Crime_Standard[which(Crime_Final$Offence == "Prostitution offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Prostitution Offences", "Prostitution Offences (General)"),
        each = length(which(Crime_Final$Offence == "Prostitution offences")))

Crime_Standard[which(Crime_Final$Offence == "Found in Places Used for Purpose of Prostitution Offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Prostitution Offences", "Found in Places Used for Purpose of Prostitution"),
        each = length(which(Crime_Final$Offence == "Found in Places Used for Purpose of Prostitution Offences")))

Crime_Standard[which(Crime_Final$Offence == "Have Interest in Premises Used for Prostitution Offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Prostitution Offences", "Have Interest in Premises Used for Prostitution"),
        each = length(which(Crime_Final$Offence == "Have Interest in Premises Used for Prostitution Offences")))

Crime_Standard[which(Crime_Final$Offence == "Knowingly Participate in Provision Prostitution Offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Prostitution Offences", "Knowingly Participate in Provision Prostitution"),
        each = length(which(Crime_Final$Offence == "Knowingly Participate in Provision Prostitution Offences")))

Crime_Standard[which(Crime_Final$Offence == "Public Soliciting"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Prostitution Offences", "Public Soliciting Prostitution"),
        each = length(which(Crime_Final$Offence == "Public Soliciting")))

Crime_Standard[which(Crime_Final$Offence == "Procuring Prostitution"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Prostitution Offences", "Procuring Prostitution"),
        each = length(which(Crime_Final$Offence == "Procuring Prostitution")))

Crime_Standard[which(Crime_Final$Offence == "Permit Minor to be at a Place Used for Prostitution Offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Prostitution Offences", "Permit Minor to be at a Prostitution Location"),
        each = length(which(Crime_Final$Offence == "Permit Minor to be at a Place Used for Prostitution Offences")))

Crime_Standard[which(Crime_Final$Offence == "Advertising Prostitution"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Prostitution Offences", "Advertising Prostitution"),
        each = length(which(Crime_Final$Offence == "Advertising Prostitution")))

Crime_Standard[which(Crime_Final$Offence == "Other Prostitution Offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Prostitution Offences", "Other Prostitution Offences"),
        each = length(which(Crime_Final$Offence == "Other Prostitution Offences")))

### Gambling and Related Offences

Crime_Standard[which(Crime_Final$Offence == "Betting and gaming offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Gambling and Related Offences", "Gambling and Related Offences"),
        each = length(which(Crime_Final$Offence == "Betting and gaming offences")))

Crime_Standard[which(Crime_Final$Offence == "Gaming Racing & Betting Offences"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Gambling and Related Offences", "Gambling and Related Offences"),
        each = length(which(Crime_Final$Offence == "Gaming Racing & Betting Offences")))

### Police Related Offences

Crime_Standard[which(Crime_Final$Offence == "Escape custody"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Police Related Offences", "Escape Custody"),
        each = length(which(Crime_Final$Offence == "Escape custody")))

Crime_Standard[which(Crime_Final$Offence == "Resist or hinder officer"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Police Related Offences", "Resist or Hinder Officer"),
        each = length(which(Crime_Final$Offence == "Resist or hinder officer")))

Crime_Standard[which(Crime_Final$Offence == "Other offences against justice procedures"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Police Related Offences", "Other Offences Against Justice Procedures"),
        each = length(which(Crime_Final$Offence == "Other offences against justice procedures")))

Crime_Standard[which(Crime_Final$Offence == "Disobey Move-on Direction"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Police Related Offences", "Disobey Move On Direction"),
        each = length(which(Crime_Final$Offence == "Disobey Move-on Direction")))

Crime_Standard[which(Crime_Final$Offence == "Resist Incite Hinder Obstruct Police"), c(7, 8, 9)] = 
    rep(c("Other Offences", "Police Related Offences", "Resist Incite Hinder or Obstruct Police"),
        each = length(which(Crime_Final$Offence == "Resist Incite Hinder Obstruct Police")))



Crime_Final[is.na(Crime_Final)] = ""



Offence_list = unique(Crime_Standard[, c(7, 8, 9)])

Offence_ordered1 = Offence_list[order(Offence_list[2]),]

Offence_ordered2 = Offence_ordered1[order(Offence_ordered1[1]),]



write.xlsx(Offence_ordered2, "Crime_Data_Offences.xlsx")

write.csv(Crime_Standard, "Crime_Data_Cleaned.csv")



ACT_Standard = Crime_Standard[Crime_Standard$State == "Australian Capital Territory",]
NSW_Standard = Crime_Standard[Crime_Standard$State == "New South Wales",]
NT_Standard = Crime_Standard[Crime_Standard$State == "Northern Territory",]
QLD_Standard = Crime_Standard[Crime_Standard$State == "Queensland",]
SA_Standard = Crime_Standard[Crime_Standard$State == "South Australia",]
VIC_Standard = Crime_Standard[Crime_Standard$State == "Victoria",]
WA_Standard = Crime_Standard[Crime_Standard$State == "Western Australia",]



write.csv(ACT_Standard, "ACT_Standard.csv")
write.csv(NSW_Standard, "NSW_Standard.csv")
write.csv(NT_Standard, "NT_Standard.csv")
write.csv(QLD_Standard, "QLD_Standard.csv")
write.csv(SA_Standard, "SA_Standard.csv")
write.csv(VIC_Standard, "VIC_Standard.csv")
write.csv(WA_Standard, "WA_Standard.csv")















