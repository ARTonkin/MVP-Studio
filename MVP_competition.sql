
/*
Creating tables for Star schema
*/

ALTER TABLE NSWproperty
ADD PropertyID integer IDENTITY(1,1);

ALTER TABLE NSWschools
ADD SchoolID integer IDENTITY(1,1);

ALTER TABLE NSWstation
ADD StationID integer IDENTITY(1,1);

SELECT
    SchoolID,
    school_code AS School_Code,
    AgeID AS Age,
    school_name AS School_Name,
    student_number AS Student_Number,
    indigenous_pct AS Indigenous_PCT,
    lbote_pct AS LBOTE_PCT,
    ICSEA_Value,
    level_of_schooling AS Level_Schooling,
    selective_school AS Selective_School,
    opportunity_class AS Opportunity_Class,
    school_specialty_type AS School_Specialty,
    school_subtype AS School_Subtype,
    preschool_ind AS Preschool,
    distance_education AS Distance_Education,
    intensive_english_centre AS Intensive_English,
    school_gender AS School_Gender,
    late_opening_school AS Late_Opening,
    date_1st_teacher AS Date_1st_Teacher,
    lga AS LGA,
    electorate AS Electorate,
    fed_electorate AS Fed_Electorate,
    operational_directorate AS Operational_Directorate,
    principal_network AS Principal_Network,
    facs_district AS Facs_District,
    local_health_district AS Health_District,
    aecg_region AS AECG_Region,
    ASGS_remoteness AS ASGS_Remoteness,
    date_extracted AS Date_Extracted
INTO DimSchool
FROM NSWschools;

ALTER TABLE DimSchool
ADD PRIMARY KEY (SchoolID);


SELECT 
	StationID,
	Train_Station,
	Entrance_Type,
	Exit_Number
INTO DimStation
FROM NSWstation;

ALTER TABLE DimStation
ADD PRIMARY KEY (StationID);


SELECT
    PropertyID,
    Property_Median_Value,
    Updated_Year,
    Updated_Month
INTO DimProperty
FROM NSWproperty;

ALTER TABLE DimProperty
ADD PRIMARY KEY (PropertyID);

SELECT
    PropertyID as DateID,
    Updated_Year,
    Updated_Month
INTO DimDate
FROM NSWproperty;

ALTER TABLE DimDate
ADD PRIMARY KEY (DateID);


SELECT
    street AS Street,
    town_suburb AS Suburb,
    null AS City,
	postcode AS Postcode,
	latitude AS Latitude,
	longitude AS Longitude,
	SchoolID AS tempschoolID,
	null AS temppropID,
	null AS tempstationID
INTO DimLocation
FROM NSWschools
UNION
SELECT
    null AS Street,
    Suburb,
    City,
	Postcode,
	null AS Latitude,
	null AS Longitude,
	null AS tempschoolID,
	PropertyID AS temppropID,
	null AS tempstationID
FROM NSWproperty
UNION
SELECT
    Street_Name AS Street,
    null AS Suburb,
    null AS City,
	null AS Postcode,
	LAT AS Latitude,
	LONG AS Longitude,
	null AS tempschoolID,
	null AS temppropID,
	StationID AS tempstationID
FROM NSWstation;

ALTER TABLE DimLocation
ADD LocationID integer IDENTITY(1,1);

ALTER TABLE DimLocation
ADD PRIMARY KEY (LocationID);


SELECT
	IDENTITY(INT,1,1) AS FactID,
	l.LocationID,
	d.DateID,
	st.StationID,
	sc.SchoolID,
	np.Property_Median_Value
INTO FactNSW
FROM
	DimLocation l
		LEFT JOIN
	DimDate d ON l.temppropID = d.DateID
		LEFT JOIN
	DimStation st ON l.tempstationID = st.StationID
		LEFT JOIN
	DimSchool sc ON l.tempschoolID = sc.SchoolID
		LEFT JOIN
	NSWproperty np ON l.temppropID = np.PropertyID;


ALTER TABLE FactNSW
    ADD FOREIGN KEY (LocationID) REFERENCES DimLocation(LocationID),
        FOREIGN KEY (DateID) REFERENCES DimDate(DateID),
        FOREIGN KEY (StationID) REFERENCES DimStation(StationID),
        FOREIGN KEY (SchoolID) REFERENCES DimSchool(SchoolID),
		PRIMARY KEY (FactID);

ALTER TABLE DimLocation
	DROP COLUMN tempschoolID, temppropID, tempstationID;









