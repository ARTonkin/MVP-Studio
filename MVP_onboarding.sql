/*
A.
Display a list of all property names and their property id’s for Owner Id: 1426.
*/

SELECT
    p.Name, op.PropertyId
FROM
	Property p
        LEFT JOIN
    OwnerProperty op ON p.Id = op.PropertyId
WHERE op.OwnerId = 1426
ORDER BY p.Name;


/*
B.
Display the current home value for each property in question a).
*/

SELECT
    p.Name, op.PropertyId, phv.Value
FROM
    Property p
        LEFT JOIN
    OwnerProperty op ON p.Id = op.PropertyId
		LEFT JOIN
	PropertyHomeValue phv ON p.Id = phv.PropertyId
WHERE op.OwnerId = 1426 AND phv.IsActive = 1
ORDER BY p.Name;


/*
C.
For each property in question a), return the following:                                                                      
 i)  Using rental payment amount, rental payment frequency, tenant start date 
		and tenant end date to write a query that returns the sum of all payments
		from start date to end date. 
 ii) Display the yield.
).
*/

SELECT
    p.Name,
	TotalPayment = DATEDIFF(WEEK, tp.StartDate, tp.EndDate) / 
	CASE 
		WHEN tpf.Code = 'Weekly' THEN 1
		WHEN tpf.Code = 'Fortnightly' THEN 2
		WHEN tpf.Code = 'Monthly' THEN 4 
	END * tp.PaymentAmount, pf.Yield
FROM
    Property p
        LEFT JOIN
    OwnerProperty op ON p.Id = op.PropertyId
		LEFT JOIN
	TenantProperty tp ON p.Id = tp.PropertyId
		LEFT JOIN
	TenantPaymentFrequencies tpf ON tp.PaymentFrequencyId = tpf.Id
		LEFT JOIN
	PropertyFinance pf ON p.Id = pf.PropertyId
WHERE op.OwnerId = 1426
ORDER BY p.Name;


/*
D.
Display all the jobs available.
*/

SELECT DISTINCT 
	j.JobDescription 
FROM 
	Job j
		LEFT JOIN
	JobStatus js ON j.JobStatusId = js.Id
WHERE j.JobDescription IS NOT NULL AND js.Status = 'Open'
ORDER BY j.JobDescription;


/*
E.
Display all property names, current tenants first and last names
	and rental payments per week/ fortnight/month for the 
	properties in question a).
*/

SELECT
    p.Name, pe.FirstName, pe.LastName, tp.PaymentAmount, tpf.Code
FROM
    Property p
        LEFT JOIN
    OwnerProperty op ON p.Id = op.PropertyId
		LEFT JOIN
	TenantProperty tp ON p.Id = tp.PropertyId
		LEFT JOIN
	Tenant t ON tp.TenantId = t.Id
		LEFT JOIN
	Person pe ON t.Id = pe.Id
		LEFT JOIN
	TenantPaymentFrequencies tpf ON tp.PaymentFrequencyId = tpf.Id
WHERE op.OwnerId = 1426 AND t.IsActive = 1
ORDER BY p.Name;


