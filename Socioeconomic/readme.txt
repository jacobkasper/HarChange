##household income
https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-income-households.html
see code in prep_socioeconomic.r 

##fuel
https://www.eia.gov/petroleum/gasdiesel/
weekly fuel by area 1993-2020
supplement early years with https://www.eia.gov/state/seds/sep_prices/notes/pr_print.pdf
and
https://www.eia.gov/state/seds/sep_prices/total/csv/pr_all.csv ,view code description at https://www.eia.gov/state/seds/CDF/Codes_and_Descriptions.xlsx.
used regular formula


View code MGTXD for gasoline for each state and year, and code DFACD for diesel

##GDP
GDP downloaded on 12/11/2020 "SAGDP1__ALL_AREAS_1997_2019.csv"
https://apps.bea.gov/regional/downloadzip.cfm
Years 1997-2019, requested earlier data

##median age
https://www2.census.gov/programs-surveys/popest/tables/1990-2000/state/asrh/st-99-08.txt
https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-detail.html
https://www.census.gov/data/tables/time-series/demo/popest/intercensal-2000-2010-state.html
2005-2009 https://data.census.gov/cedsci/deeplinks?url=https%3A%2F%2Ffactfinder.census.gov%2Ffaces%2Fnav%2Fjsf%2Fpages%2Findex.xhtml (no longer works)
2000-2004 h https://www2.census.gov/library/publications/cen2010/briefs/c2010br-14.pdf
see code in age_popdensity.r

##unemployment 
https://data.bls.gov/cgi-bin/dsrv?la

##household size
year specific 2005-2018
2010-2018 https://data.census.gov/cedsci/table?q=S11&g=0400000US01,09,10,12,13,22,23,24,25,28,33,34,36,37,44,45,48,51&d=ACS%201-Year%20Estimates%20Subject%20Tables&tid=ACSST1Y2010.S1101&hidePreview=false
decade-specific 1990-2000 https://www.census.gov/prod/2001pubs/c2kbr01-8.pdf
see code in prep_socioeconomic.r 

##population density
population density was obtained by dividing state population by state area
see code in population directory
state level


1990-2000
https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/co-est2001-12-00.pdf
2001-2009
nst-est2009-01
2010-2018
nst-est2019-01
