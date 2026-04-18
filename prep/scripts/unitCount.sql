SELECT
    a.StudentID,
	a.SchoolYear,
    a.DistrictCode,
    c.DistrictName,
    a.SchoolCode,
    b.SchoolName,
    a.Grade,
    a.Geography,
    e.County_Name,
    a.Gender,
    d.RaceReportTitle,
    a.LowIncome,
    a.SPEDCode,
    a.SWD,
    a.ELL,
    a.Migrant,
    a.Homeless,
    a.FosterCare,
    a.Immersion,
    a.MilitaryDep
FROM PUBLICREPORTMART.details.P20_STUDENT_ENROLLMENT_UNITCOUNT a
LEFT JOIN CodeLibrary.dbo.School b
    ON a.SchoolYear = b.SchoolYear
   AND a.DistrictCode = b.DistrictCode
   AND a.SchoolCode = b.SchoolCode
LEFT JOIN CodeLibrary.dbo.District c
    ON a.SchoolYear = c.SchoolYear
   AND a.DistrictCode = c.DistrictCode
LEFT JOIN CodeLibrary.dbo.RaceReportCodeEDEN d
    ON a.RaceEDEN = d.RaceReportCode
LEFT JOIN CodeLibrary.dbo.County e
    ON c.County = e.County_Code;