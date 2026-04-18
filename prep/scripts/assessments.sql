with base as (
    select
        a.SchoolYear,
        a.StudentID,
        a.DistrictCode,
        a.SchoolCode,
        a.Grade,
        a.EligibleCount,
        a.Birthdate,
        a.Geography,
        a.Gender,
        a.LowIncome,
        a.SPEDCode,
        a.SWD,
        a.ELL,
        a.Migrant,
        a.Homeless,
        a.FosterCare,
        a.Immersion,
        a.MilitaryDep,
        a.EntryDate,
        a.AssessmentName,
        a.ContentArea,
        a.ScaleScore,
        a.PerformanceScore,
        a.RealScore,
        a.Proficient,
        a.GrowthTarget,
        a.PctAcctGrowthAchieved,
        a.ELGrowth,
        a.ELGrowthMet,
        a.PctELGrowthAchieved,
        a.SSGrowthOPY,
        a.AcctGrowth,
        a.AcctGrowthMet,
        a.CompVGrowth,
        a.CompVGrowthMet,
        cast(nullif(a.RaceEDEN, 'UTM') as int) as RaceReportCode
    from PUBLICREPORTMART.details.P20_STUDENT_ASSESSMENT_SUMMATIVE a
    where a.EligibleCount = 1
)
select
    a.SchoolYear,
    a.StudentID,
    a.DistrictCode,
    c.DistrictName,
    a.SchoolCode,
    b.SchoolName,
    a.Grade,
    a.EligibleCount,
    a.Birthdate,
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
    a.MilitaryDep,
    a.EntryDate,
    a.AssessmentName,
    a.ContentArea,
    a.ScaleScore,
    a.PerformanceScore,
    a.RealScore,
    a.Proficient,
    a.GrowthTarget,
    a.PctAcctGrowthAchieved,
    a.ELGrowth,
    a.ELGrowthMet,
    a.PctELGrowthAchieved,
    a.SSGrowthOPY,
    a.AcctGrowth,
    a.AcctGrowthMet,
    a.CompVGrowth,
    a.CompVGrowthMet
from base a
left join CodeLibrary.dbo.School b
    on a.SchoolYear = b.SchoolYear
   and a.DistrictCode = b.DistrictCode
   and a.SchoolCode = b.SchoolCode
left join CodeLibrary.dbo.District c
    on a.SchoolYear = c.SchoolYear
   and a.DistrictCode = c.DistrictCode
left join CodeLibrary.dbo.RaceReportCodeEDEN d
    on a.RaceReportCode = d.RaceReportCode
left join CodeLibrary.dbo.County e
    on c.County = e.County_Code;