


ALTER TABLE CrimeByYear
ALTER COLUMN CrimeID int; 

ALTER TABLE CrimeByYear
ALTER COLUMN Year int;

ALTER TABLE CrimeByYear
ALTER COLUMN Number_of_Offences int;

ALTER TABLE CrimeByYear
ADD Crime_Percent float;

UPDATE CrimeByYear
SET Crime_Percent = try_convert(float, Crime_Percentage);

ALTER TABLE CrimeByYear
DROP COLUMN Crime_Percentage;

SELECT TOP 10 * 
FROM CrimeByYear 
ORDER BY CrimeID;



ALTER TABLE School_Data
ADD School_Lat float;

UPDATE School_Data
SET School_Lat = try_convert(float, ["Lat"]);

ALTER TABLE School_Data
DROP COLUMN ["Lat"];


ALTER TABLE School_Data
ADD School_Lon float;

UPDATE School_Data
SET School_Lon = try_convert(float, ["Lon"]);

ALTER TABLE School_Data
DROP COLUMN ["Lon"];


SELECT TOP 10 * 
FROM School_Data;



SELECT 
	LocationID,
	Suburb,
	State_Code,
	lat as Lat,
	lon as Lon
INTO DimGeography
FROM Location_Data;

ALTER TABLE DimGeography
ADD PRIMARY KEY (LocationID);



CREATE TABLE DimState (
	DWStateKey int,
	StateCode varchar(50),
	StateName varchar(50)
	);



INSERT INTO DimState (DWStateKey, StateCode, StateName)
VALUES 
	(1, 'ACT', 'Australian Capital Territory'),
	(2, 'NT', 'Northern Territory'),
	(3, 'WA', 'Western Australia'),
	(4, 'QLD', 'Queensland'),
	(5, 'SA', 'South Australia'),
	(6, 'VIC', 'Victoria'),
	(7, 'NSW', 'New South Wales'),
	(8, 'TAS', 'Tasmania');
	
ALTER TABLE DimState
ALTER COLUMN DWStateKey int not null;

ALTER TABLE DimState
ADD PRIMARY KEY (DWStateKey);




SELECT 
	t.TransportID,
	s.DWStateKey,
	g.LocationID,
	t.Stop_Name,
	t.Postcode,
	t.Stop_Lat AS Lat,
	t.Stop_Lon AS Lon,
	t.Mode
INTO StgTransport
FROM 
	transport_Data t
		INNER JOIN
	DimState s ON t.State_Code = s.StateCode
		INNER JOIN
	DimGeography g ON t.Suburb = g.Suburb;

ALTER TABLE StgTransport
    ADD FOREIGN KEY (LocationID) REFERENCES DimGeography(LocationID),
        FOREIGN KEY (DWStateKey) REFERENCES DimState(DWStateKey),
		PRIMARY KEY (TransportID);



SELECT 
	sc.["SchoolID"] AS SchoolID,
	s.DWStateKey,
	g.LocationID,
	sc.["School_Name"] AS School_Name,
	sc.["Postcode"] AS Postcode,
	sc.["School_Type"] AS School_Type,
	sc.School_Lat AS Lat,
	sc.School_Lon AS Lon
INTO DimAuLocalSchool
FROM 
	School_Data sc
		INNER JOIN
	DimState s ON sc.["State_Code"] = s.StateCode
		INNER JOIN
	DimGeography g ON sc.["Suburb"] = g.Suburb;
	
ALTER TABLE DimAuLocalSchool
ALTER COLUMN SchoolID int not null;

ALTER TABLE DimAuLocalSchool
    ADD FOREIGN KEY (LocationID) REFERENCES DimGeography(LocationID),
        FOREIGN KEY (DWStateKey) REFERENCES DimState(DWStateKey),
		PRIMARY KEY (SchoolID);



SELECT 
	c.CrimeID,
	s.DWStateKey,
	g.LocationID,
	c.Year,
	c.Offence_Subdivision AS Offence,
	c.Number_of_Offences,
	c.Crime_Percent
INTO FactCrimeByYear
FROM 
	CrimeByYear c
		INNER JOIN
	DimState s ON c.State = s.StateName
		INNER JOIN
	DimGeography g ON c.Suburb = g.Suburb;


ALTER TABLE FactCrimeByYear
ALTER COLUMN CrimeID int not null;

ALTER TABLE FactCrimeByYear
    ADD FOREIGN KEY (LocationID) REFERENCES DimGeography(LocationID),
        FOREIGN KEY (DWStateKey) REFERENCES DimState(DWStateKey),
		PRIMARY KEY (CrimeID);


SELECT TOP 10 *
FROM FactCrimeByYear 
ORDER BY newid();


SELECT 
	IDENTITY(INT,1,1) AS RentalID,
	s.DWStateKey,
	g.LocationID,
	r.City,
	r.Rental_Type,
	r.Rental_Value,
	r.Rental_Value_Missing
INTO FactSuburbRentalMedian
FROM 
	Rental_Data r
		INNER JOIN
	DimState s ON r.State = s.StateName
		INNER JOIN
	DimGeography g ON r.Suburb = g.Suburb;


ALTER TABLE FactSuburbRentalMedian
ALTER COLUMN RentalID int not null;

ALTER TABLE FactSuburbRentalMedian
    ADD FOREIGN KEY (LocationID) REFERENCES DimGeography(LocationID),
        FOREIGN KEY (DWStateKey) REFERENCES DimState(DWStateKey),
		PRIMARY KEY (RentalID);
		

SELECT TOP 10 *
FROM FactSuburbRentalMedian 
ORDER BY newid();


SELECT 
	p.PropertyID,
	s.DWStateKey,
	g.LocationID,
	p.PropertyMedian,
	p.Year
INTO FactMedianPropertyValueByYear
FROM 
	Property_Data p
		INNER JOIN
	DimState s ON p.StateCode = s.StateCode
		INNER JOIN
	DimGeography g ON p.Suburb = g.Suburb;


ALTER TABLE FactMedianPropertyValueByYear
ALTER COLUMN PropertyID int not null;

ALTER TABLE FactMedianPropertyValueByYear
    ADD FOREIGN KEY (LocationID) REFERENCES DimGeography(LocationID),
        FOREIGN KEY (DWStateKey) REFERENCES DimState(DWStateKey),
		PRIMARY KEY (PropertyID);
		

SELECT TOP 10 *
FROM FactMedianPropertyValueByYear 
ORDER BY newid();













