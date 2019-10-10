Exploration: Funding Acknowledgements from OpenAIRE in WOS-KB
================
Najko Jahn
10/7/2019

## Summary

![Flow Chart](flow.png)

## OpenAIRE Publications

To start with, a sample of grant-supported publications indexed in
OpenAIRE with EC-funding acknwoledgement was used. Only H2020
EC-projects with participation from the University of Göttingen were
investigated. These data were already obtained for the OpenAIRE
Institutional Dashboard Pilot:
<https://subugoe.shinyapps.io/openaire_ugoe/>

Load publications:

``` r
library(tidyverse)
library(jsonlite)
ugoe_pubs <- jsonlite::stream_in(file("data/pubs_ugoe.json"), verbose = FALSE) %>%
  as_tibble()
```

In total, the sample consists of 1542 distinct records.

For matching, the DOI article identifier is used:

``` r
ugoe_dois_df <- ugoe_pubs %>% 
  select(pid, access, openaire_project_id, openaire_id) %>% 
  unnest(cols = c(pid)) %>% 
  mutate(pid_type = ifelse(grepl("^10.", pid), "doi", NA)) %>%
  mutate(pid = tolower(pid)) %>%
  mutate(openaire_project_id = as.character(openaire_project_id))
```

In total, 1313 records have a DOI, representing a share of 85.15 %.

Store records with DOIs in separate data.frame

``` r
ugoe_dois <- ugoe_dois_df %>%
  filter(pid_type == "doi") %>%
  distinct(pid) %>%
  mutate(pid = trimws(pid))
```

Connect to WOS-KB and store data.frame in KB Table Space

``` r
require(RJDBC)
require(rJava)
.jinit()
jdbcDriver <-
  JDBC(driverClass = "oracle.jdbc.OracleDriver", classPath = "inst/jdbc_driver/ojdbc8.jar")
jdbcConnection <-
  dbConnect(
    jdbcDriver,
    "jdbc:oracle:thin:@//biblio-p-db01:1521/bibliodb01.fiz.karlsruhe",
    Sys.getenv("kb_user"),
    Sys.getenv("kb_pwd")
  ) 
```

``` r
dbWriteTable(conn = jdbcConnection, 
             name = "openaire_ugoe_dois", 
             value = ugoe_dois,
             overwrite = TRUE)
#> [1] TRUE
```

## DOI coverage in WoS-KB

Matching using Oracle SQL statement:

``` sql
select
        wos_b_2019.items.doi,
        wos_b_2019.items.doctype 
    from
        wos_b_2019.items 
    inner join
        openaire_ugoe_dois 
            on lower(wos_b_2019.items.doi) = openaire_ugoe_dois.pid
```

Summary stats

``` r
openaire_wos_doi %>%
  group_by(DOCTYPE) %>%
  summarise(n= n_distinct(DOI)) %>%
  arrange(desc(n)) 
#> # A tibble: 6 x 2
#>   DOCTYPE                n
#>   <chr>              <int>
#> 1 Article              693
#> 2 Proceedings Paper     41
#> 3 Review                20
#> 4 Correction             3
#> 5 Editorial Material     2
#> 6 Letter                 1
```

In total 759 DOIs were both indexed in OpenAIRE and Wos-KB.

## Merge with WOS-KB funding info

Oracle SQL statement to obtain funding information from WoS-KB for the
OpenAIRE-DOI-sample

``` sql
select
        wos_b_2019.fundingorganizations.fundingorganization,
        wos_b_2019.grantnumbers.grantnumber,
        wos_b_2019.items.doi 
    from
        wos_b_2019.items 
    inner join
        wos_b_2019.grantnumbers    
            on wos_b_2019.grantnumbers.fk_items = wos_b_2019.items.pk_items 
    inner join
        wos_b_2019.fundingorganizations    
            on wos_b_2019.fundingorganizations.pk_fundingorganizations = wos_b_2019.grantnumbers.fk_fundingorganizations 
    inner join
        openaire_ugoe_dois 
            on lower(wos_b_2019.items.doi) = openaire_ugoe_dois.pid
```

### Funding Organization

Identify the European Commission as funding organization using pattern
matching.

``` r
openaire_wos %>%
  as_tibble() %>%
  mutate(eu_funding = ifelse(grepl("^EU|^Eur|^ERC", FUNDINGORGANIZATION), "EU", NA)) %>% 
  filter(eu_funding == "EU") 
#> # A tibble: 1,128 x 4
#>    FUNDINGORGANIZATION              GRANTNUMBER      DOI         eu_funding
#>    <chr>                            <chr>            <chr>       <chr>     
#>  1 European ITN project (FP7-PEOPL… PITNGA-2011-289… 10.1007/JH… EU        
#>  2 European Union                   690575           10.1103/Ph… EU        
#>  3 EU                               PITN-GA-2011-28… 10.1103/Ph… EU        
#>  4 European Union                   674896           10.1103/Ph… EU        
#>  5 EU                               FP7 ITN INVISIB… 10.1103/Ph… EU        
#>  6 European Research Council under… 617143           10.1103/Ph… EU        
#>  7 European Union's Horizon resear… 690575           10.1088/14… EU        
#>  8 European Union's Horizon resear… 674896           10.1088/14… EU        
#>  9 European Union's Horizon resear… 674896           10.1007/JH… EU        
#> 10 European Union's Horizon resear… 690575           10.1007/JH… EU        
#> # … with 1,118 more rows
```

In total, 628 Web of Science records acknowledge the European Commission
as funder (`wos_b_2019.fundingorganizations.fundingorganization`).

There is a certain variation how EU support is acknowledged in
`wos_b_2019.fundingorganizations.fundingorganization`: 313 variants were
found based on our sample.

Example

``` r
openaire_wos %>%
  as_tibble() %>%
  mutate(eu_funding = ifelse(grepl("^EU|^Eur|^ERC", FUNDINGORGANIZATION), "EU", NA)) %>% 
  filter(eu_funding == "EU") %>%
  distinct(FUNDINGORGANIZATION, GRANTNUMBER) %>%
  head()
#> # A tibble: 6 x 2
#>   FUNDINGORGANIZATION                                 GRANTNUMBER          
#>   <chr>                                               <chr>                
#> 1 European ITN project (FP7-PEOPLE-ITN)               PITNGA-2011-289442-I…
#> 2 European Union                                      690575               
#> 3 EU                                                  PITN-GA-2011-289442  
#> 4 European Union                                      674896               
#> 5 EU                                                  FP7 ITN INVISIBLES   
#> 6 European Research Council under the European Union… 617143
```

### Grant Agreements

Extract EU Grant IDs, which end with a six-digit number

``` r
openaire_wos_grant <- openaire_wos %>% 
  mutate(eu_funding = ifelse(grepl("^EU|^Eur|^ERC", FUNDINGORGANIZATION), "EU", NA)) %>% 
  filter(eu_funding == "EU") %>%
  mutate(eu_grant_id = str_extract(GRANTNUMBER, "[0-9]{6,6}")) %>%
  mutate(doi = tolower(DOI)) %>%
  inner_join(ugoe_dois_df, by = c("doi" = "pid", "eu_grant_id" = "openaire_project_id"))
```

In total, 538 WoS-KB records have identical Grant IDs.

How many `GRANTNUMBER` values consists of a six-digit number:

``` r
openaire_wos %>% 
  mutate(eu_funding = ifelse(grepl("^EU|^Eur|^ERC", FUNDINGORGANIZATION), "EU", NA)) %>% 
  filter(eu_funding == "EU") %>%
  filter(grepl("^[0-9]{6,6}", GRANTNUMBER)) 
#>                                                                                                                                                                FUNDINGORGANIZATION
#> 1                                                                                                                                                                   European Union
#> 2                                                                                                                                                                   European Union
#> 3                                                                                  European Research Council under the European Union's Seventh Framework Programme (FP)/ERC Grant
#> 4                                                                                European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 5                                                                                European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 6                                                                                European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 7                                                                                European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 8                                                                                      European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie
#> 9                                                                                      European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie
#> 10                                                                                                                      European Union's Horizon research and innovation programme
#> 11                                                                                                                      European Union's Horizon research and innovation programme
#> 12                                                      European Union's Horizon 2020 research and innovation programme InvisiblesPlus RISE under the Marie Sklodowska-Curie grant
#> 13                                                             European Union's Horizon 2020 research and innovation programme Elusives ITN under the Marie Sklodowska-Curie grant
#> 14                                                                               European Union's Horizon research and innovation program under the Marie Skl odowska-Curie Grants
#> 15                                                                               European Union's Horizon research and innovation program under the Marie Skl odowska-Curie Grants
#> 16                                                                      European research Council (ERC) under the European Unions's Horizon 2020 research and innovation programme
#> 17                                                                                                                 European Union's Horizon 2020 research and innovation programme
#> 18                                                                               European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie Grant
#> 19                                                                               European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie Grant
#> 20                                                                               European Unions Horizon research and innovation programme under the Marie Sklodowska-Curie Grants
#> 21                                                                               European Unions Horizon research and innovation programme under the Marie Sklodowska-Curie Grants
#> 22                                                                          European Union's Horizon 2020 research and innovation programme under the Marie-Sklodowska-Curie grant
#> 23                                                                          European Union's Horizon 2020 research and innovation programme under the Marie-Sklodowska-Curie grant
#> 24                                                             European Union Horizon 2020 Research and Innovation Programme under the Marie Sklodowska-Curie: RISE InvisiblesPlus
#> 25                                                                                                                             European Research Council under the European Unions
#> 26                                                                                                                                             EU Horizon 2020 Rafts4Biotech grant
#> 27                                                                                                                                           EU Horizon 2020 Rafts4Biotech project
#> 28                                                                                                EU Horizon 2020 research and innovation program under the Marie-Sklodowska Grant
#> 29                                                                                      European Union Horizon 2020 research and innovation under the Marie Sklodowska-Curie Grant
#> 30                                                                                      European Union Horizon 2020 research and innovation under the Marie Sklodowska-Curie Grant
#> 31                                                                                                                                                                  European Union
#> 32                                                                                                                                                                  European Union
#> 33                                                                              European Union's Horizon 2020 Research and Innovation programme under Marie Sklodowska-Curie grant
#> 34                                                                              European Union's Horizon 2020 Research and Innovation programme under Marie Sklodowska-Curie grant
#> 35                                                                                                                                                                  European Union
#> 36                                                                                                                                                                  European Union
#> 37                                                                           European Unions Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 38                                                                            European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 39                                                                          European Unions Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grants
#> 40                                                                          European Unions Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grants
#> 41                                                                           European Union's Horizon 2020 research and innovation programme under the Marie Skodowska-Curie grant
#> 42                                                                           European Union's Horizon 2020 research and innovation programme under the Marie Skodowska-Curie grant
#> 43                                                                                                                                                                  European Union
#> 44                                                            European Union's Framework Programme for Research and Innovation Horizon 2020 under the Marie Sklodowska-Curie Grant
#> 45                                                                                                                                                 European Research Council (ERC)
#> 46                                                            European Union's Framework Programme for Research and Innovation Horizon 2020 under the Marie Sklodowska-Curie Grant
#> 47                                                            European Union's Framework Programme for Research and Innovation Horizon 2020 under the Marie Sklodowska-Curie Grant
#> 48                                                            European Union's Framework Programme for Research and Innovation Horizon 2020 under the Marie Sklodowska-Curie Grant
#> 49                                                European Union's Framework Programme for Research and Innovation Horizon 2020 (2014-2020) under the Marie Sklodowska-Curie Grant
#> 50                                                                                                                                                 European Research Council (ERC)
#> 51                                                European Union's Framework Programme for Research and Innovation Horizon 2020 (2014-2020) under the Marie Sklodowska-Curie Grant
#> 52                                                European Union's Framework Programme for Research and Innovation Horizon 2020 (2014-2020) under the Marie Sklodowska-Curie Grant
#> 53                                                                           European Unions Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 54                                                                           European Unions Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 55                                                                       European Research Council (ERC) under the European Union's Horizon 2020 research and innovation programme
#> 56                                                                                                                                                                  European Union
#> 57                                                                                                                                                                  European Union
#> 58                                                                       European Research Council (ERC) under the European Union's Horizon 2020 research and innovation programme
#> 59                                                                                European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie
#> 60                                                                                European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie
#> 61                                                                                 European Union Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 62                                                                                                                                                      ERC Advanced Grant MC@NNLO
#> 63                                    European Union's Horizon 2020 research and innovation programme as part of the Marie Skodowska-Curie Innovative Training Network "MCnetITN3"
#> 64                                                                                                                                                                  European Union
#> 65                                                                                 European Union Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 66                                                                                 European Union Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 67                                                                                                                                          European Commission H project OpenAIRE
#> 68                                                                                                                                                                 European Unions
#> 69                                                                                                                                                                 European Unions
#> 70                                                                  European Union's Horizon 2020 research and innovation program InvisiblesPlus RISE under Marie Sklodowska-Curie
#> 71                                                                     European Union's Horizon 2020 research and innovation program Elusives ITN under the Marie Sklodowska-Curie
#> 72                                                                                     European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie
#> 73                                                                                     European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie
#> 74                                                                                      European Union's Horizon 2020 research and innovation program under Marie-Sklodowska-Curie
#> 75                                                                                      European Union's Horizon 2020 research and innovation program under Marie-Sklodowska-Curie
#> 76                                                                               European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 77                                                                            European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie Grant
#> 78                                                                            European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie Grant
#> 79                                                                           European Union's Horizon 2020 research and innovation program under the Twinning Grant, RBI-T-WINNING
#> 80                                                                              European Union's Horizon 2020 Research and Innovation programme under Marie Sklodowska-Curie grant
#> 81                                                                              European Union's Horizon 2020 Research and Innovation programme under Marie Sklodowska-Curie grant
#> 82                                                                                                                 European Union's Horizon 2020 research and innovation programme
#> 83                                          European Union's Horizon research and innovation programme as part of the Marie Sklodowska-Curie Innovative Training Network MCnetITN3
#> 84                                                                                                                                                                 European Unions
#> 85                                                                                                                                                                 European Unions
#> 86                                                                               European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 87                                                                                                                                                                  European Union
#> 88                                                                                                                                                                  European Union
#> 89                                                                                                                                                                  European Union
#> 90                                                                                                                                                                  European Union
#> 91                                                                                                                                                                  European Union
#> 92                                                                                                                                                                  European Union
#> 93                                                                                              EU Horizon 2020 research and innovation programme under the Marie-Sklodowska Grant
#> 94                                                                               European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 95                                                                               European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 96                                                                                                                                                              EU Horizon program
#> 97                                                                                                                                                              EU Horizon program
#> 98                                                                                                                                                              EU Horizon program
#> 99                                                                                                                                                                              EU
#> 100                                                                                                                                                                 European Union
#> 101                                                                               European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie
#> 102                                                                               European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie
#> 103                                                                                    European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie
#> 104                                                                                    European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie
#> 105                                                                                                    EU Horizon research and innovation program under the Marie-Sklodowska Grant
#> 106                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 107                                                                            European Unions Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 108                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 109                                                 European Union Horizon research and innovation programme under the Marie Sklodowska-Curie grant agreements InvisiblesPlus RISE
#> 110                                                                                                                                                                 European Union
#> 111                                                                                European Unions Horizon 2020 Research and Innovation Programme under the Marie Sklodowska-Curie
#> 112                                                                                                                                                                 European Union
#> 113                                                                                                                                                                 European Union
#> 114                                                  European Union's Horizon 2020 research and innovation programme, Marie Sklodowska-Curie Innovative Training Network MCnetITN3
#> 115                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 116                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 117                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 118                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 119                                                                                     European Union Horizon 2020 research and innovation under the Marie Sklodowska-Curie grant
#> 120                                                                                                                                                                 European Union
#> 121                                                                                  European Unions Horizon 2020 research and innovation program under the Marie Sklodowska-Curie
#> 122                                                                                  European Unions Horizon 2020 research and innovation program under the Marie Sklodowska-Curie
#> 123                                                                                                                               European Union Seventh Framework Programme (FP7)
#> 124                                                                                                            European Commissions Horizon 2020 research and innovation programme
#> 125                                                                                                                European Union's Horizon 2020 Research and Innovation Programme
#> 126                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 127                                               European Union's Framework Programme for Research and Innovation Horizon 2020 (2014-2020) under the Marie Sklodowska-Curie Grant
#> 128                                                                                                                                                European Research Council (ERC)
#> 129                                               European Union's Framework Programme for Research and Innovation Horizon 2020 (2014-2020) under the Marie Sklodowska-Curie Grant
#> 130                                               European Union's Framework Programme for Research and Innovation Horizon 2020 (2014-2020) under the Marie Sklodowska-Curie Grant
#> 131                                                                                                                European Union's Horizon 2020 research and innovation programme
#> 132                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 133                                                                                                                                                                        ERC-CoG
#> 134                                                                                                                                                                 European Union
#> 135                                                                                                                                                European Research Council (ERC)
#> 136                                                                                                                                                                 European Union
#> 137                                                                                                                                                                 European Union
#> 138                                                                                                                                                                 European Union
#> 139                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 140                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 141                                                                                                                                                      European Research Council
#> 142                                    European Union's Horizon 2020 research and innovation programme as part of the Marie Sklodowska-Curie Innovative Training Network MCnetITN3
#> 143                                                                                                                                                  ERC Consolidator Grant HICCUP
#> 144                                                                                                                                                                 European Union
#> 145                                                                                                                                                                 European Union
#> 146                                                      European Union's Horizon 2020 Research and Innovation programme under Marie Sklodowska-Curie grant agreement Elusives ITN
#> 147                                               European Union's Horizon 2020 Research and Innovation programme under Marie Sklodowska-Curie grant agreement InvisiblesPlus RISE
#> 148                                                                                                                                                            European Commission
#> 149                                                                                    European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie
#> 150                                                                              European Union's Horizon Research and Innovation Programme under the Marie Sklodowska-Curie Grant
#> 151                                                                                      European Union's Horizon research and innovation program under the Marie Sklodowska-Curie
#> 152                                                                                      European Union's Horizon research and innovation program under the Marie Sklodowska-Curie
#> 153                                                                                                                                                                 European Union
#> 154                                                                                  European Union's Horizon 2020 research and innovation program under the Marie Skodowska-Curie
#> 155                                                                                                                                                                 European Union
#> 156                                                                                                                                                                 European Union
#> 157                                                                                                                                                                 European Union
#> 158                                                                          European Unions Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 159                                                                          European Unions Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 160                                                                                                                                                                European Unions
#> 161                                                                                                                                                                European Unions
#> 162                                                                           European Unions Horizon 2020 research and innovation programme under the Marie Skodowska-Curie Grant
#> 163                                                                                                                                                                 European Union
#> 164                                                                                                                                                                 European Union
#> 165                                                                                                                                                             ERC Advanced Grant
#> 166                                                                                                                                                            EU project FracRisk
#> 167                                                                                              EU through the Marie Curie BeIPD-COFUND postdoctoral fellowship (FP7-MSCA-COFUND)
#> 168                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 169                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 170                                                                                European Unions Horizon Research and Innovation Programme under the Marie Skodowska-Curie Grant
#> 171                                                                               European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie
#> 172                                                                               European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie
#> 173                                                                                      European Union's Horizon research and innovation program under the Marie Sklodowska-Curie
#> 174                                                                                      European Union's Horizon research and innovation program under the Marie Sklodowska-Curie
#> 175                                                                                                                                                                 European Union
#> 176                                                                                      European Union Horizon research and innovation programme under the Marie Sklodowska-Curie
#> 177                                                                                      European Union Horizon research and innovation programme under the Marie Sklodowska-Curie
#> 178                                                                                                                     European Union's Horizon research and innovation programme
#> 179                                                                                                                     European Union's Horizon research and innovation programme
#> 180                                                                                                                     European Union's Horizon research and innovation programme
#> 181                                                                        European Union's Horizon research and innovation program under the Marie Sklodowska-Curie Grants - CNRS
#> 182                                                                                European Research Council (ERC) under the EU Seventh Framework Program (FP7)/ERC Starting Grant
#> 183                                                                        European Union's Horizon research and innovation program under the Marie Sklodowska-Curie Grants - CNRS
#> 184                                                                                                                     European Union's Horizon research and innovation programme
#> 185                                                                                                                     European Union's Horizon research and innovation programme
#> 186                                                                                                                                                                 European Union
#> 187                                                                                                                                                             European Community
#> 188                                                             European Union Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie InvisiblesPlus RISE
#> 189                                                                    European Union Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie Elusives ITN
#> 190                                                                                                                                                             ERC Advanced Grant
#> 191                                                                                                                                                                 European Union
#> 192                                                                                                                                                                 European Union
#> 193                                                                                                    European Union, H Marie Sklodowska-Curie Initial Training Network MCnetITN3
#> 194                                                                                        European Union's Horizon research and innovation programme under Marie Sklodowska-Curie
#> 195                                                                                        European Union's Horizon research and innovation programme under Marie Sklodowska-Curie
#> 196                                                                                                                      European Unions Horizon research and innovation programme
#> 197                                                                                                                      European Unions Horizon research and innovation programme
#> 198                                                                                                                      European Unions Horizon research and innovation programme
#> 199                                                                             European Union's Horizon 2020 research and innovation programme under Marie Sklodowska-Curie grant
#> 200                                                                                                                                                                     EU project
#> 201                                                                                                                                    European Union under Marie Sklodowska-Curie
#> 202                                                                                     European Union's Horizon 2020 research and innovation program under Marie Sklodowska-Curie
#> 203                                                                                     European Union's Horizon 2020 research and innovation program under Marie Sklodowska-Curie
#> 204                                                                             European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie Grants
#> 205                                                                             European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie Grants
#> 206                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie Grant
#> 207                                                                                                                                                                 European Union
#> 208                                                                                                                                                                 European Union
#> 209                                                                                                                  European Union Horizon 2020 research and innovation programme
#> 210                                                                                                                  European Union Horizon 2020 research and innovation programme
#> 211                                                                                                                  European Union Horizon 2020 research and innovation programme
#> 212                                                                               European Union's Horizon Research and Innovation Programme under the Marie Skodowska-Curie Grant
#> 213                                                                               European Union's Horizon Research and Innovation Programme under the Marie Skodowska-Curie Grant
#> 214                                                                                                                       European Union (EU) and Horizon 2020 through grant iNEXT
#> 215                                                                                                                   European Union (EU) and Horizon 2020 through grant EOSCpilot
#> 216                                                                                                                       European Union (EU) and Horizon 2020 through grant iNEXT
#> 217                                                                                                                   European Union (EU) and Horizon 2020 through grant West-Life
#> 218                                                                                                                      European Union (EU) and Horizon 2020 through grant Corbel
#> 219                                                                                                                  European Union Horizon 2020 research and innovation programme
#> 220                                                                                                                  European Union Horizon 2020 research and innovation programme
#> 221                                                                                                                  European Union Horizon 2020 research and innovation programme
#> 222                                                                                                                                                                 European Union
#> 223                                                                                                                                                                 European Union
#> 224                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 225                                                                                                                                                                            ERC
#> 226                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 227                                                                                                                                                                 European Union
#> 228                                                                                                                                                                 European Union
#> 229                                                                                                European Union, H2020 Marie Sklodowska-Curie Initial Training Network MCnetITN3
#> 230                                                                                                                                                                 European Union
#> 231                                                                                                                                                                 European Union
#> 232                                                                                                                                                                 European Union
#> 233                                                                                                                                                                             EU
#> 234                                                                                                                                                                             EU
#> 235                                                                                                                                                                             EU
#> 236                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 237                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 238                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 239                                                                                                                                                European Research Council (ERC)
#> 240                                                                                                                                                                 European Union
#> 241                                                                                                                                                                 European Union
#> 242                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 243                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 244                                                                                                                European Research Council (ERC) under EU Horizon 2020 Programme
#> 245                                                                                                                                                                 European Union
#> 246                                                                                                                                                                 European Union
#> 247                                                                                                                                                                 European Union
#> 248                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 249                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 250                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie Grant
#> 251                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie Grant
#> 252                                                                                          European Research Council under the European Union's Horizon 2020 program (ERC Grant)
#> 253                                                                                                                       European Research Council (ERC) under the European Union
#> 254                                                                                               EU Horizon 2020 research and innovation program under the Marie-Sklodowska Grant
#> 255                                                                                             EU Horizon 2020 research and innovation programme under the Marie-Sklodowska Grant
#> 256                                                                                                                                                            European Commission
#> 257                                                                                                                                                            European Commission
#> 258                                                                                                                                European Union's Horizon 2020 Framework Program
#> 259                                                                                                                                                                 European Union
#> 260                                                                             European Union's Horizon research and innovation programme under the Marie Sk lodowska-Curie grant
#> 261                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 262                                                                                                                                                European Research Council (ERC)
#> 263                                                                              European Union's Horizon 2020 Research AMP; Innovation program under Marie Sklodowska-Curie Grant
#> 264                                               European Union's Framework Programme for Research and Innovation Horizon 2020 (2014-2020) under the Marie Sklodowska-Curie Grant
#> 265                                                                              European Union's Horizon 2020 Research AMP; Innovation program under Marie Sklodowska-Curie Grant
#> 266                                                                             European Union's Horizon research and innovation programme under the Marie Sk lodowska-Curie grant
#> 267                                               European Union's Framework Programme for Research and Innovation Horizon 2020 (2014-2020) under the Marie Sklodowska-Curie Grant
#> 268                                               European Union's Framework Programme for Research and Innovation Horizon 2020 (2014-2020) under the Marie Sklodowska-Curie Grant
#> 269                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 270                                                                                          European Research Council (ERC) under EU Seventh Framework Program/ERC Starting Grant
#> 271                                                                                European Union Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 272                                                     European Union's Horizon 2020 Research and Innovation programme under Marie Sk lodowska-Curie grant agreement Elusives ITN
#> 273                                                                                European Union Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 274                                              European Union's Horizon 2020 Research and Innovation programme under Marie Sk lodowska-Curie grant agreement InvisiblesPlus RISE
#> 275                                                                                                                                                                 European Union
#> 276                                                                                                                                                                 European Union
#> 277                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 278                               European Union's Horizon 2020 Research and Innovation program under Marie Sklodowska-Curie Grant agreements Elusives ITN and InvisiblesPlus RISE
#> 279                                                                                                                     European Union's Horizon research and innovation programme
#> 280                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 281                                                                                                                                                                 European Union
#> 282                                                                                                                     European Union's Horizon research and innovation programme
#> 283                               European Union's Horizon 2020 Research and Innovation program under Marie Sklodowska-Curie Grant agreements Elusives ITN and InvisiblesPlus RISE
#> 284                                                                 European Union Horizon research and innovation programme under the Marie Sklodowska-Curie: RISE InvisiblesPlus
#> 285                                                                                    European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie
#> 286                                                           European Union's Horizon 2020 research and innovation programme Elusives ITN under the Marie Sklodowska- Curie grant
#> 287                                                                            European Unions Horizon 2020 research and innovation program under the Marie Sklodowska Curie Grant
#> 288                                                                                European Union Horizon Research and Innovation Programme under the Marie Sklodowska-Curie Grant
#> 289                                                     European Union's Horizon 2020 research and innovation programme InvisiblesPlus RISE under the Marie Sklodowska-Curie grant
#> 290                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 291                                                                                    European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie
#> 292                                                                                                                                         European Research Council (ERC), Spain
#> 293                                                                    European Union Horizon 2020 research and innovation programme, Spain under the Marie Sklodowska-Curie grant
#> 294                                                                    European Union Horizon 2020 research and innovation programme, Spain under the Marie Sklodowska-Curie grant
#> 295                                                                 European Research Council under the European Union Seventh Framework Programme (FP/2007-2013)/ERC Grant NuMass
#> 296                                                                                                                                                 EU Horizon Programme ReconCell
#> 297                                                                            European Unions Horizon 2020 research and innovation program under the Marie Sklodowska Curie Grant
#> 298                                                                                European Union Horizon Research and Innovation Programme under the Marie Sklodowska-Curie Grant
#> 299                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 300                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie Grant
#> 301                                                                                                                  European Union's Horizon 2020 research and innovation program
#> 302                                                                                                                  European Union's Horizon 2020 research and innovation program
#> 303                                                                                                                                                                 European Union
#> 304                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 305                                                                                                                                                                             EU
#> 306                                                                                                                                                                             EU
#> 307                                                                                                             European Union's Horizon 2020 Framework Programme H2020/2014-2020/
#> 308                                                                           European Research Council (ERC) under the European Union's Horizon research and innovation programme
#> 309                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie Grant
#> 310                                                                                European Union's Horizon research and innovation program under the Marie Sklodowska-Curie Grant
#> 311                                               European Union's Framework Programme for Research and Innovation Horizon 2020 (2014-2020) under the Marie Sklodowska-Curie grant
#> 312                                                                               European Union's Horizon research and innovation program under the Marie Sk lodowska-Curie grant
#> 313                                                                               European Union's Horizon research and innovation program under the Marie Sk lodowska-Curie grant
#> 314                                                                                European Union's Horizon research and innovation program under the Marie Sklodowska-Curie Grant
#> 315                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 316                                                                      European Research Council (ERC) under the European Union's Horizon 2020 research and innovation programme
#> 317                                                                                                                   European Commission Programme, H2020-WATER, INTCATCH Project
#> 318                                                                                                                   European Community Horizon Research and Innovation Programme
#> 319                                                                              European Unions Horizon research and innovation programme under the Marie Sklodowska-Curie grants
#> 320                                                                                                                                                                 European Union
#> 321                                                                                                           European Union (EU)'s Horizon 2020 research and innovation programme
#> 322                                                                              European Unions Horizon research and innovation programme under the Marie Sklodowska-Curie grants
#> 323                                                                                European Union as a part of the H2020 Marie Sklodowska-Curie Initial Training Network MCnetITN3
#> 324                                                                             European Union's Horizon 2020 research and innovation programme under Marie Sklodowska-Curie-Grant
#> 325                                                                                                                                                                 European Union
#> 326                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 327                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 328                                                                               European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie
#> 329                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 330                                                                               European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie
#> 331                                                                                 European Union's Horizon 2020 research and innovation program under the Marie Sklodowska Curie
#> 332                                                                              European Union's Horizon research and innovation Programme under the Marie Sklodowska-Curie grant
#> 333                                                                                               EU Horizon 2020 research and innovation program under the Marie-Sklodowska Grant
#> 334                                                                                                                                                                 European Union
#> 335                                                                                European Union's Horizon research and innovation program under the Marie Sklodowska-Curie grant
#> 336                                                                                                                                                                 European Union
#> 337                                                                                European Union's Horizon research and innovation program under the Marie Sklodowska-Curie grant
#> 338                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 339                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 340                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 341                                                                                                                                                                 European Union
#> 342                                                                                 European Union's Horizon 2020 research and innovation program under the Marie Sklodowska Curie
#> 343                                                                                                                                                                 European Union
#> 344                                                                                                                                                         EU-JAPAN initiative EC
#> 345                                                                                                                                                                 European Union
#> 346                                                                                                                                                                 European Union
#> 347                                                                                               European Research Council under European Union's Seventh Framework Programme/ERC
#> 348                                                                                                                                                                 European Union
#> 349                                                                                                                                                                 European Union
#> 350                                                                        European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie Grants
#> 351                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 352                                                                               European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie
#> 353                                                                        European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie Grants
#> 354                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 355                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 356                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 357                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 358                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 359                                                                                      European Union's Horizon research and innovation program under the Marie Sklodowska-Curie
#> 360                                                                                                                                        EU-Japan ICN Project (EU HORIZON Grant)
#> 361                                                                                                                                                                 European Union
#> 362                                                                                European Union's Horizon research and innovation program under the Marie-Sklodowska-Curie Grant
#> 363                                                                                                                                                                 European Union
#> 364                                                                                                                                                                             EU
#> 365                                                                                European Union's Horizon research and innovation program under the Marie-Sklodowska-Curie Grant
#> 366                                                                                      European Union's Horizon research and innovation program under the Marie Sklodowska-Curie
#> 367                                                                                                                          European Union's Horizon 2020 Research and Innovation
#> 368                                                                               European Unions Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 369                                                         European Union's Horizon research and innovation programme InvisiblesPlus RISE under the Marie Sk lodowska-Curie grant
#> 370                                                                                                                                                                 European Union
#> 371                                                                                                                                                                 European Union
#> 372                                                                European Union's Horizon research and innovation programme Elusives ITN under the Marie Sk lodowska-Curie grant
#> 373                                                                               European Unions Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 374                                                                                                                                                                            ERC
#> 375                                                                                                                European Union's Horizon 2020 research and innovation programme
#> 376                                                                                                                                                    ERC Starting Grant 'NewAve'
#> 377                                                                                                                                                                 European Union
#> 378                                                                                                                                                                 European Union
#> 379                                                                                                                     European Union's Horizon research and innovation programme
#> 380                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 381                                                                                               EU Horizon 2020 research and innovation program under the Marie-Sklodowska Grant
#> 382                                                                                                                                                            European Commission
#> 383                                                                                European Unions Horizon research and innovation programme under the Marie Skodowska-Curie Grant
#> 384                                                                                European Unions Horizon research and innovation programme under the Marie Skodowska-Curie Grant
#> 385                                    European Union's Horizon 2020 research and innovation programme as part of the Marie Sklodowska-Curie Innovative Training Network MCnetITN3
#> 386                                                                                                                                                                 European Union
#> 387                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 388                                                                                                                                                                             EU
#> 389                                                                             European Union's Horizon 2020 research and innovation programme under Marie Sklodowska-Curie grant
#> 390                                                                                                                                                                 European Union
#> 391                                                                                                                                          European Research Council (ERC Grant)
#> 392                                                                                                                                                                 European Union
#> 393                                    European Union's Horizon 2020 research and innovation programme as part of the Marie Sklodowska-Curie Innovative Training Network MCnetITN3
#> 394                                                                                                                                                             ERC Starting Grant
#> 395                                                                                                                               European Union's Horizon research and innovation
#> 396                                                                                                                                                                            ERC
#> 397                                                                                                                               European Union's Horizon research and innovation
#> 398                                      European Union's Horizon 2020 research and innovation program as part of the Marie Sklodowska-Curie Innovative Training Network MCnetITN3
#> 399                                                                               European Union's Horizon 2020 research and innovation program under Marie Sklodowska-Curie grant
#> 400                                                                     European Research Council (ERC) under the European Unions's Horizon 2020 research and innovation programme
#> 401                                                                                    European Union's Horizon research and innovation programme under the Marie-Sklodowska-Curie
#> 402                                                                                                                                                                 European Union
#> 403                                                                                    European Union's Horizon research and innovation programme under the Marie-Sklodowska-Curie
#> 404                                                                                                                European Union's Horizon 2020 research and innovation programme
#> 405                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 406                                                                                  European Union's Horizon Research and Innovation programme under Marie Sklodowska-Curie grant
#> 407                                                                                  European Union's Horizon Research and Innovation programme under Marie Sklodowska-Curie grant
#> 408                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 409                                                                                                                                                                 European Union
#> 410                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska Curie grant
#> 411                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 412                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 413                                                                                                                                                                 European Union
#> 414                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska Curie grant
#> 415                                                                            European Unions Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 416                                                                                       European Union Horizon 2020 Research and Innovation Programme under the Marie-Sklodowska
#> 417                                                                                European Union's Horizon research and innovation program under the Marie Sklodowska-Curie grant
#> 418                                                                                European Union's Horizon research and innovation program under the Marie Sklodowska-Curie grant
#> 419                                                                               European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie
#> 420                                                                               European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie
#> 421                                                                                                                                                                 European Union
#> 422                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 423                                                                                                                                                                            ERC
#> 424                                                                      European Research Council (ERC) under the European Union's Horizon 2020 research and innovation programme
#> 425                                                                                   European Union's Horizon research and innovation programme under the Marie Sk lodowska-Curie
#> 426                                                                                                                                                 European Union COFUND under EU
#> 427                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 428                                                                              European Unions Horizon research, and innovation programme under the Marie Skodowska-Curie Grants
#> 429                                                                              European Unions Horizon research, and innovation programme under the Marie Skodowska-Curie Grants
#> 430                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie Grant
#> 431                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 432                                                                                                                                                            European Commission
#> 433                                                                                      European Union's Horizon research and innovation programme (under Marie Sklodowska-Curie)
#> 434                                                                                                                                                      European Research Council
#> 435                                                                                      European Union's Horizon research and innovation programme (under Marie Sklodowska-Curie)
#> 436                                                                                                                                                                 European Union
#> 437                                                                                                                                                                 European Union
#> 438                                                       European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie Grant Elusives ITN agreement
#> 439                                                                                                                                                                 European Union
#> 440                                                                                                                                                                 European Union
#> 441                                                                                                                                                                 European Union
#> 442                                                                                           European Union's Horizon research and innovation programme under InvisiblesPlus RISE
#> 443                                                                                                                European Union's Horizon 2020 research and innovation programme
#> 444                                                                                                                                                                 European Union
#> 445                                                                              European Research Council under the European Union's Horizon 2020 research and innovation program
#> 446                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 447                                                                                                                                                                 European Union
#> 448                                                                                                                                                                 European Union
#> 449                                                                              European Research Council under the European Union's Horizon 2020 research and innovation program
#> 450                                                                                                                                                                             EU
#> 451                                                                                                            European Union's Horizon Research and Innovation Programme FRACRISK
#> 452                                                                      European Research Council (ERC) under the European Union's Horizon 2020 research and innovation programme
#> 453                                                                                                                                                      European Research Council
#> 454                                                    European Union's Horizon Research and Innovation programme under Marie Sklodowska-Curie grant agreement InvisiblesPlus RISE
#> 455                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 456                                                           European Union's Horizon Research and Innovation programme under Marie Sklodowska-Curie grant agreement Elusives ITN
#> 457                                                                                                                                                                 European Union
#> 458                                                                                                                                                          EU Horizon 2020 grant
#> 459                                                                                                                                                             ERC Advanced Grant
#> 460                                                                                                                                                                 European Union
#> 461                                                                                                                                                                 European Union
#> 462                                                                                                                                                                 European Union
#> 463                                                                                                                                                                 European Union
#> 464                                                                                European Union's Horizon research and innovation program under the Marie Sklodowska-Curie grant
#> 465                                                                                                                                                              European projects
#> 466                                                                                      European Union's Horizon research and innovation program under the Marie Sklodowska-Curie
#> 467                                                                                                    European Union through the Horizon Marie Sklodowska-Curie network MCnetITN3
#> 468                                                                                                                                                                 European Union
#> 469                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie Grant
#> 470                                                                                                                                                                 European Union
#> 471                                                                                                                                                               EU project TRUST
#> 472                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie Grant
#> 473                                                                                                                                                                 European Union
#> 474                                                                                    European Union's Horizon research and innovation program under Marie Sklodowska-Curie Grant
#> 475                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie Grant
#> 476                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie Grant
#> 477                                                                               European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie
#> 478                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 479                                                                               European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie
#> 480                                                                                                                                                                 European Union
#> 481                                                                                European Union Horizon Research and Innovation Programme under the Marie Sklodowska-Curie Grant
#> 482                                                                                                                                                                 European Union
#> 483                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 484                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska Curie grant
#> 485                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska Curie grant
#> 486                                                                                European Union's Horizon research and innovation program under the Marie Sklodowska-Curie Grant
#> 487                                                                  European Union Horizon 2020 research and innovation program under Innovative Training Networks (ITN) Elusives
#> 488                                                                   European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie individual Grant
#> 489                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 490                      European Union Horizon 2020 research and innovation program under the Marie Sklodowska-Curie Research and Innovation Staff Exchange (RISE) InvisiblesPlus
#> 491                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie Grant
#> 492                                                                                European Union's Horizon research and innovation program under the Marie Sklodowska-Curie Grant
#> 493                                                                                                                                                                 European Union
#> 494                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie Grant
#> 495                                                                                      European Union's Horizon research and innovation program under the Marie Sklodowska-Curie
#> 496                                                                                      European Union's Horizon research and innovation program under the Marie Sklodowska-Curie
#> 497                                                                             European Union's Horizon 2020 research and innovation programme under Marie Sklodowska-Curie grant
#> 498                                                                                                                                                                 European Union
#> 499                                                                                                                                                                 European Union
#> 500                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 501                                                                             European Union's Horizon 2020 research and innovation programme under Marie Sklodowska-Curie grant
#> 502                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 503                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 504                                                                                                                                                                 European Union
#> 505                                                European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant agreement InvisiblesPlus RISE
#> 506                                                       European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant agreement Elusives ITN
#> 507                                                                    European Union 2020 research and innovation programme under the Marie Sklodowska-Curie: RISE InvisiblesPlus
#> 508                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 509                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 510                                                                                European Union's Horizon research and innovation program under the Marie Sklodowska-Curie grant
#> 511                                                                                European Union's Horizon research and innovation program under the Marie Sklodowska-Curie grant
#> 512                                                                                                                                                     ERC Advanced Grant MC@NNLO
#> 513                                                                              European Union's Horizon research and innovation programme under the Marie Sklodovska-Curie grant
#> 514                                         European Union's Horizon research and innovation programme as part of the Marie Sklodowska-Curie Innovative Training Network MCnetITN3
#> 515                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 516                                                                                                                                                      European Research Council
#> 517                                                                      European Research Council (ERC) under the European Union's Horizon 2020 research and innovation programme
#> 518                                          European Union's Horizon research and innovation programme as part of the Marie Skodowska-Curie Innovative Training Network MCnetITN3
#> 519                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 520                                                                                                                                                  European Commission under ERC
#> 521                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 522                                                                               European Union's Horizon research and innovation programme under the Marie Skodowska-Curie grant
#> 523                                                            European Union's Horizon 2020 research and innovation programme Elusives ITN under the Marie Sklodowska-Curie grant
#> 524                                                     European Union's Horizon 2020 research and innovation programme InvisiblesPlus RISE under the Marie Sklodowska-Curie grant
#> 525                                                       European Union's Horizon 2020 research and innovation program InvisiblesPlus RISE under the Marie Sklodowska-Curie Grant
#> 526                                                              European Union's Horizon 2020 research and innovation program Elusives ITN under the Marie Sklodowska-Curie Grant
#> 527                                                                             European Union's Horizon 2020 research and innovation programme under Marie Sklodowska-Curie grant
#> 528                                                                                                                                                                 European Union
#> 529                                                                                                                                        European Research Council Synergy Grant
#> 530            European Union's Horizon 2020 research and innovation programme under ALTERFOR project (Alternative models and robust decision-making for future forest management)
#> 531                                                                                                                                                                            ERC
#> 532                                                                                                                                                                 European Union
#> 533                                                                                                                                                                 European Union
#> 534                                                                                                                                                                 European Union
#> 535                                                                                                                                                                 European Union
#> 536                                                                        European Union's Horizon 2020 research and innovation programme under the Marie Skl odowska-Curie Grant
#> 537                                                                        European Union's Horizon 2020 research and innovation programme under the Marie Skl odowska-Curie Grant
#> 538                                                                                                                                                                             EU
#> 539                                                                                                                                                                             EU
#> 540                                                                                                                                                                 European Union
#> 541                                                                                                                                                                             EU
#> 542                                                                                 European Union Horizon research and innovation program under the Marie Sklodowska-Curie grants
#> 543                                                                                 European Union Horizon research and innovation program under the Marie Sklodowska-Curie grants
#> 544                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 545                                    European Union's Horizon 2020 research and innovation programme as part of the Marie Sklodowska-Curie Innovative Training Network MCnetITN3
#> 546                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 547                                                                                   European Union's Horizon 2020 research and innovation programme under Marie Sklodowska-Curie
#> 548                                                                                                                                                                             EU
#> 549                                                            European Union's Horizon 2020 research and innovation programme Elusives ITN under the Marie Sklodowska-Curie grant
#> 550                                                     European Union's Horizon 2020 research and innovation programme InvisiblesPlus RISE under the Marie Sklodowska-Curie grant
#> 551                                                                                                                     European Union's Horizon research and innovation programme
#> 552                                European Union under Horizon 2020 research and innovation programme as part of the Marie Sklodowska-Curie Innovative Training Network MCnetITN3
#> 553                                                                                                                                                                 European Union
#> 554                                                                                                                     European Union's Horizon research and innovation programme
#> 555                                                                                                                                                                 European Union
#> 556                                                                                                                                                                             EU
#> 557                                                                                    European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie
#> 558                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie Grant
#> 559                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 560                                                     European Union's Horizon 2020 research and innovation programme InvisiblesPlus RISE under the Marie Sklodowska-Curie grant
#> 561                                                               European Research Council under the European Union Seventh Framework Programme (FP/2007-2013) / ERC Grant NuMass
#> 562                                                                                                                                                                             EU
#> 563                                                                             European Research Council under the European Unions Seventh Framework Programme (FP/2007-2013)/ERC
#> 564                                                                                    European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie
#> 565                                                                          European Unions Horizon 2020 research and innovation programme under the Marie Sklodowska Curie grant
#> 566                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie Grant
#> 567                                                            European Union's Horizon 2020 research and innovation programme Elusives ITN under the Marie Sklodowska-Curie grant
#> 568                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 569                                                                          European Unions Horizon 2020 research and innovation programme under the Marie Sklodowska Curie grant
#> 570                                                                                       European Unions Horizon research and innovation program under the Marie Sklodowska-Curie
#> 571                                                                                        European Union's Horizon research and innovation programme under Marie Sklodowska-Curie
#> 572                                                                          European Research Council (ERC) under European Union's Horizon 2020 research and innovation programme
#> 573                                                                                       European Unions Horizon research and innovation program under the Marie Sklodowska-Curie
#> 574                                                                               European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie
#> 575                                                          European Union's Horizon research and innovation programme InvisiblesPlus RISE under the Marie Sklodowska-Curie Grant
#> 576                                                                                                                                                                 European Union
#> 577                                                                 European Union's Horizon research and innovation programme Elusives ITN under the Marie Sklodowska-Curie Grant
#> 578                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 579                                                                                                                                                                 European Union
#> 580                                                                                                                             European Research Council under the European Union
#> 581                                                                                                                      European Unions Horizon Research and Innovation Programme
#> 582                                                                                                                                                                 European Union
#> 583                                                          European Union's Horizon research and innovation programme InvisiblesPlus RISE under the Marie Sklodowska-Curie grant
#> 584                                                                 European Union's Horizon research and innovation programme Elusives ITN under the Marie Sklodowska-Curie grant
#> 585                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 586                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 587                                                                                                                       European Union's Horizon research and innovation program
#> 588                                                  European Union's Horizon 2020 research and innovation programme, Marie Sklodowska-Curie Innovative Training Network MCnetITN3
#> 589                                                                                                                                                                 European Union
#> 590                                                                                      European Union Horizon research and innovation programme under the Marie Sklodowska-Curie
#> 591                                                               European Union's Horizon research and innovation programme Invisible-sPlus RISE under the Marie Sklodowska-Curie
#> 592                                                                                      European Union Horizon research and innovation programme under the Marie Sklodowska-Curie
#> 593                                                                                                                                                                 European Union
#> 594                                                                       European Union's Horizon research and innovation programme Elusives ITN under the Marie Sklodowska-Curie
#> 595                                                                                                                     European Union's Horizon Research and Innovation Programme
#> 596                                                                               European Union Horizon research and innovation programme under the Marie Sklodowska-Curie grants
#> 597                                                                                                                                                                 European Union
#> 598                                    European Union's Horizon 2020 research and innovation programme as part of the Marie Sklodowska-Curie Innovative Training Network MCnetITN3
#> 599                                                                               European Union Horizon research and innovation programme under the Marie Sklodowska-Curie grants
#> 600                                                                                                                                                                 European Union
#> 601                                                                                     European Union's Horizon research and innovation programme under the Marie SklodowskaCurie
#> 602                                                                          European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie Grants
#> 603                                                                                     European Union's Horizon research and innovation programme under the Marie SklodowskaCurie
#> 604                                                                                                                      European Unions Horizon research and innovation programme
#> 605                                                                                                                      European Unions Horizon research and innovation programme
#> 606                                                            European Union Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie: RISE InvisiblesPlus
#> 607                                                                         European Research Council under the European Union's Seventh Framework Programme (FP)/ERC Grant NuMass
#> 608                                                                          European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie Grants
#> 609                                                                                                                 European Unions Horizon 2020 research and innovation programme
#> 610                                                                                                                                                                 European Union
#> 611                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie Grant
#> 612                                                                                                                 European Unions Horizon 2020 research and innovation programme
#> 613                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 614                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie Grant
#> 615                                                                              European Unions Horizon research and innovation programme under the Marie Sklodowska-Curie grants
#> 616                                                             European Unionamp;amp;apos;s Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 617                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 618                                                                              European Unions Horizon research and innovation programme under the Marie Sklodowska-Curie grants
#> 619                                                                                    European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie
#> 620                                                                                                                                                                 European Union
#> 621                                                                                                                                                                 European Union
#> 622                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie Grant
#> 623                                                                                                                       European Research Council (ERC) under the European Union
#> 624                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie Grant
#> 625                                                                                                                                                                 European Union
#> 626                                                                                                                                                                 European Union
#> 627                                                                          European Union Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grants
#> 628                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 629                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 630                                                                                                                                                                 European Union
#> 631                                                                          European Union Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grants
#> 632                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 633                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 634                                                                                                                                                                             EU
#> 635                                                                                                                                                                             EU
#> 636                                                                                                                         European Commission Program, H-WATER, INTCATCH Project
#> 637                                                                                                                                                                 European Union
#> 638                                                                          European Unions Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 639                                                                          European Unions Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 640                                                                                                                                                                 European Union
#> 641                                                                                                                                                        EU Horizon 2020 program
#> 642                                                                                                                                                        EU Horizon 2020 program
#> 643                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 644                                                                                                                                                        EU Horizon 2020 program
#> 645                                                                                European Union's Horizon research and innovation program under the Marie Sklodowska-Curie grant
#> 646                                                                                                                                                                 European Union
#> 647                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 648                                                                                                                                                                 European Union
#> 649                                                                                                                  European Union Horizon 2020 research and innovation programme
#> 650                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie Grant
#> 651                                                                                                                  European Union Horizon 2020 research and innovation programme
#> 652                                                                                                                                                European Research Council (ERC)
#> 653                                                                                                                European Union's Horizon 2020 research and innovation programme
#> 654                                                          European Union's Horizon Research and Innovation programme under Marie Sklodowska-Curie grant agreements Elusives ITN
#> 655                                                                                                                           European Research Council (ERC) under European Union
#> 656                                                 European Union's Horizon 2020 Programme through the project Advanced BiomEdical OPTICAL Imaging and Data Analysis (BE-OPTICAL)
#> 657                                                                          European Union's Horizon 2020 research and innovation programme under the Marie Skodowska-Curie Grant
#> 658                                                                          European Union's Horizon 2020 research and innovation programme under the Marie Skodowska-Curie Grant
#> 659                                                                          European Union's Horizon 2020 research and innovation programme under the Marie Skodowska-Curie grant
#> 660                                                                                                                                                                 European Union
#> 661                                                                              European Union's Horizon research and innovation programme under the Marie-Sklodowska-Curie grant
#> 662                                                                              European Union's Horizon research and innovation programme under the Marie-Sklodowska-Curie grant
#> 663                                                                            European UnionaAZs Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 664                                                                            European Unions Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 665                                                                            European UnionaAZs Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 666                                                                                                                                                                 European Union
#> 667                                                                          European Union Horizon 2020 research and innovation initiative under the Marie Sklodowska-Curie Grant
#> 668                                                                                European Unions Horizon research and innovation program, under the Marie Sklodowska-Curie Grant
#> 669                                                                                European Unions Horizon research and innovation program, under the Marie Sklodowska-Curie Grant
#> 670                                                                                                               European Union Marie Curie Innovative Training Network MCnetITN3
#> 671                                                                                                                                                European Research Council (ERC)
#> 672                                                                          European Union Horizon 2020 research and innovation initiative under the Marie Sklodowska-Curie Grant
#> 673                                                                                                                                                                            ERC
#> 674                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 675                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 676                                                          European Union's Horizon research and innovation programme InvisiblesPlus RISE under the Marie Sklodowska-Curie grant
#> 677                                                                       European Union's Horizon Research and Innovation program under Marie Sklodowska-Curie Elusives ITN Grant
#> 678                                                                                       European Union's Horizon Research and Innovation program under InvisiblesPlus RISE Grant
#> 679                                                                 European Union's Horizon research and innovation programme Elusives ITN under the Marie Sklodowska-Curie grant
#> 680                                                                                                                     European Union's Horizon Research and Innovation programme
#> 681                                                                                                                                                                 European Union
#> 682                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 683                                                                                                                                                                 European Union
#> 684                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 685                                                                                                                                                                 European Union
#> 686                                                     European Union Horizon 2020 research and innovation programme under the Marie Skodowska-Curie grant agreement Elusives ITN
#> 687                                                                                                                                                                 European Union
#> 688                                                                                                                                                                 European Union
#> 689                                                                                                                                                                 European Union
#> 690                                                                                                                                                                 European Union
#> 691                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant
#> 692                                              European Union Horizon 2020 research and innovation programme under the Marie Skodowska-Curie grant agreement InvisiblesPlus RISE
#> 693                                                                                          European Research Council under the European Union's Horizon 2020 program (ERC Grant)
#> 694                                                                                                                                                                 European Union
#> 695                                                                                                                                                                 European Union
#> 696                                                       European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant Elusives ITN agreement
#> 697                                                                               European Union's Horizon research and innovation program under the Marie Sklodowska-Curie Grants
#> 698                                                                                           European Union's Horizon research and innovation programme under InvisiblesPlus RISE
#> 699                                                                               European Union's Horizon research and innovation program under the Marie Sklodowska-Curie Grants
#> 700                                                                                                                                                                 European Union
#> 701                                                                              European Research Council under the European Union's Seventh Framework Programme/ERC NuMass Grant
#> 702                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 703                                                                                  European Union's Horizon Research and Innovation Programme under Marie Sklodowska-Curie Grant
#> 704                                                    European Union's Horizon Research and Innovation programme under Marie Sklodowska-Curie grant agreement InvisiblesPlus RISE
#> 705                                                                                         European Union's Horizon research and innovation programme under Marie Skodowska-Curie
#> 706                                                                                                                                                                 European Union
#> 707                                                           European Union's Horizon Research and Innovation programme under Marie Sklodowska-Curie grant agreement Elusives ITN
#> 708                                                                                         European Union's Horizon research and innovation programme under Marie Skodowska-Curie
#> 709                                                                                European Union's Horizon research and innovation program under the Marie Sklodowska-Curie grant
#> 710                                                                                                                                                                 European Union
#> 711                                                                                European Union's Horizon research and innovation program under the Marie Sklodowska-Curie grant
#> 712                                                                                  European Union's Horizon Research and Innovation Programme under Marie Sklodowska-Curie Grant
#> 713                                                                                                                                                                 European Union
#> 714                                    European Union's Horizon 2020 research and innovation programme as part of the Marie Sklodowska-Curie Innovative Training Network MCnetITN3
#> 715                                                                                                                                                                 European Union
#> 716                                                                          European Unions Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 717                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 718                                                                                                                                                                 European Union
#> 719                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 720                                                                          European Unions Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 721                                                                       European Research Council under the European Unions Seventh Framework Programme (FP/2007-2013)/ERC Grant
#> 722                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 723                                                                                        European Union's Horizon research and innovation programme under Marie Sklodowska-Curie
#> 724                                                      European Union's Horizon 2020 Research and Innovation programme under Marie Sklodowska-Curie grant agreement Elusives ITN
#> 725                                                                                European Unions Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie
#> 726                                                                               European Union Horizon research and innovation programme under the Marie Sklodowska-Curie grants
#> 727                                                                        European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie Grants
#> 728                                               European Union's Horizon 2020 Research and Innovation programme under Marie Sklodowska-Curie grant agreement InvisiblesPlus RISE
#> 729                                                                        European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie Grants
#> 730                                                                               European Union Horizon research and innovation programme under the Marie Sklodowska-Curie grants
#> 731                                                                                                                                                                 European Union
#> 732                                                                                                                                                                 European Union
#> 733                                                                                European Unions Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie
#> 734                                                                                                                                                European Research Council (ERC)
#> 735                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 736                                                                                                                     European Union's Horizon research and innovation programme
#> 737                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 738                                                                                 European Unions Horizon research and innovation program under the Marie Sklodowska-Curie grant
#> 739                                                                                 European Unions Horizon research and innovation program under the Marie Sklodowska-Curie grant
#> 740                                                                                             EU Horizon 2020 research and innovation programme under the Marie-Sklodowska grant
#> 741                                                                                                                                                European Research Council (ERC)
#> 742                                                                                                                European Union's Horizon 2020 Research and Innovation Programme
#> 743                                                          European Union's Horizon Research and Innovation programme under Marie Sklodowska-Curie grant agreements Elusives ITN
#> 744                                                                           European Union's Horizon 2020 research and innovation Program under the Marie Sklodowska-Curie grant
#> 745                                                                                      European Union Horizon research and innovation programme under the Marie Sklodowska-Curie
#> 746                                                                                      European Union Horizon research and innovation programme under the Marie Sklodowska-Curie
#> 747                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie Grant
#> 748                                                                                European Union's Horizon research and innovation program under the Marie Sklodowska-Curie grant
#> 749                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 750                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie Grant
#> 751                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie Grant
#> 752                                                                                            EU's Horizon research and innovation program under the Marie Sklodowska-Curie Grant
#> 753                                                                                                  EU Horizon research and innovation programme under the Marie-Sklodowska grant
#> 754                                                                                      European Union's H research and innovation program under the Marie Sklodowska-Curie grant
#> 755                                                                                            EU's Horizon research and innovation program under the Marie Sklodowska-Curie Grant
#> 756                                                        European Union's Horizon 2020 research and innovation programmes H2020-MSCA-RISE under the Marie Sklodowska-Curie Grant
#> 757                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie Grant
#> 758                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie Grant
#> 759                                                        European Union's Horizon 2020 research and innovation programmes H2020-MSCA-RISE under the Marie Sklodowska-Curie Grant
#> 760                                                                             European Union's Horizon 2020 Research and Innovation programme under Marie Sklodowska-Curie grant
#> 761                                                                             European Union's Horizon 2020 Research and Innovation programme under Marie Sklodowska-Curie grant
#> 762                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 763                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 764                                                                               European Union's Horizon research and innovation program under the Marie Sklodowska-Curie Grants
#> 765                                                                 European Unionamp;apos;s Horizon 2020 research and innovation programme under the Marie Sktodowska-Curie Grant
#> 766                                                                          European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie Grants
#> 767                                                                          European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie Grants
#> 768                                                                               European Union's Horizon research and innovation program under the Marie Sklodowska-Curie Grants
#> 769                                                                                      European Union Horizon research and innovation programme under the Marie Sklodowska-Curie
#> 770                                                                                                                                                        EU Horizon 2020 program
#> 771                                                                                      European Union Horizon research and innovation programme under the Marie Sklodowska-Curie
#> 772                                                                                                                                                        EU Horizon 2020 program
#> 773                                                                          European Union's Horizon 2020 research and innovation program under the Marie Sk lodowska-Curie grant
#> 774                                                                               European Union's Horizon 2020 research and innovation program under Marie Sklodowska-Curie grant
#> 775                                                                                    European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie
#> 776                                                                          European Union's Horizon 2020 research and innovation program under the Marie Sk lodowska-Curie grant
#> 777                                                                                European Union Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 778                                                                                European Union Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 779                                                     European Union's Horizon 2020 research and innovation programme InvisiblesPlus RISE under the Marie Sklodowska-Curie grant
#> 780                                                                                                                                                        EU Horizon 2020 program
#> 781                                                            European Union's Horizon 2020 research and innovation programme Elusives ITN under the Marie Sklodowska-Curie grant
#> 782                                                                                European Union Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 783                                                                                European Union's Horizon research and innovation program under the Marie Sklodowska-Curie grant
#> 784                                                                                                                                                            European Commission
#> 785                                                                                European Union's Horizon research and innovation program under the Marie Sklodowska-Curie grant
#> 786                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 787                                                                      European Research Council (ERC) under the European Union's Horizon 2020 research and innovation programme
#> 788                                                                                                                       European Union's Horizon research and innovation program
#> 789                                                                                                                       European Union's Horizon research and innovation program
#> 790                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 791                                                                                 European Union Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie
#> 792                                                                                 European Union Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie
#> 793                                                                                                                       European Union's Horizon research and innovation program
#> 794                                                                                                                       European Union's Horizon research and innovation program
#> 795                                                                                                                                                                 European Union
#> 796                                                                                                                                                                 European Union
#> 797                                                     European Union's Horizon 2020 Research and Innovation programme under Marie Sk lodowska-Curie grant agreement Elusives ITN
#> 798                                              European Union's Horizon 2020 Research and Innovation programme under Marie Sk lodowska-Curie grant agreement InvisiblesPlus RISE
#> 799                                    European Union's Horizon 2020 research and innovation programme as part of the Marie Sklodowska-Curie Innovative Training Network MCnetITN3
#> 800                                                                                 European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie
#> 801                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 802                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 803                                                                                                                     European Union's Horizon research and innovation programme
#> 804                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 805                                                                                                                     European Union's Horizon research and innovation programme
#> 806                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 807                                                                               European Union's Horizon research and innovation program under the Marie Sk lodowska-Curie grant
#> 808                                                            European Union Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie: RISE InvisiblesPlus
#> 809                                                                                                                                                                 European Union
#> 810                                                                      European Research Council (ERC) under the EU Seventh Framework Program (FP7/2007-2013)/ERC Starting Grant
#> 811                                                                          European Union's Horizon 2020 research and innovation programme under the Marie SklodowskaCurie grant
#> 812                                                                                     European Union's Horizon 2020 research and innovation program under Marie Sklodowska-Curie
#> 813                      European Union'sHorizon 2020 research and innovation program under the Marie Sklodowska-Curie Grant - Centre National de la Recherche Scientifique (CNRS)
#> 814                      European Union'sHorizon 2020 research and innovation program under the Marie Sklodowska-Curie Grant - Centre National de la Recherche Scientifique (CNRS)
#> 815                                                                          European Union's Horizon 2020 research and innovation programme under the Marie SklodowskaCurie grant
#> 816                                                                         European Union's Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant
#> 817                                                                                                                                                                 European Union
#> 818                                                                                                                                                European Research Council (ERC)
#> 819                                                                                     European Union's Horizon 2020 research and innovation program under Marie Sklodowska-Curie
#> 820                                                                           European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie Grant
#> 821                                                                                                                                                            EU project Fracrisk
#> 822                                                                                                                                                                 European Union
#> 823                                     European Unions Horizon 2020 research and innovation programme as part of the Marie Sklodowska-Curie Innovative Training Network MCnetITN3
#> 824                                                                                          European Union Horizon research and innovation programme under Marie Sklodowska-Curie
#> 825                                                                      European Research Council (ERC) under the European Union's Horizon 2020 research and innovation programme
#> 826                                                                                          European Union Horizon research and innovation programme under Marie Sklodowska-Curie
#> 827                                                                                          European Union Horizon research and innovation programme under Marie Sklodowska-Curie
#> 828                                                                                                                                                                 European Union
#> 829                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 830                                                                                                                                                                 European Union
#> 831                                                            European Union Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie: RISE InvisiblesPlus
#> 832                                                                                                                                                                 European Union
#> 833                                                                                                                                                                 European Union
#> 834                                                                                                                                    European Commission 7th Framework Programme
#> 835                                                                                                                  European Commission Horizon Research and Innovation Programme
#> 836                                                                                                                                                                 European Union
#> 837 European Union's Horizon Research and Innovation program (project: Furthering the knowledge Base for Reducing the Environmental Footprint of Shale Gas Development - FRACRISK)
#> 838                                                                                                                                                                 European Union
#> 839                                                                                                                                                                 European Union
#> 840                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#> 841                                                                                                                  European Union's Horizon 2020 research and innovation program
#> 842                                                                                                                                                                 European Union
#> 843                                                                                                                  European Union's Horizon 2020 research and innovation program
#> 844                                                                                                                                                      European Research Council
#> 845                                                                                                                                                                 European Union
#> 846                                                                                                                                                                 European Union
#> 847                                                      European Union's Horizon 2020 Research and Innovation programme under Marie Sklodowska-Curie grant agreement Elusives ITN
#> 848                                               European Union's Horizon 2020 Research and Innovation programme under Marie Sklodowska-Curie grant agreement InvisiblesPlus RISE
#> 849                                                                                                                                                                European Unions
#> 850                                                                                             EU Horizon 2020 research and innovation programme under the Marie-Sklodowska grant
#> 851                                                                                                                                                                European Unions
#> 852                                                                                                                European Union's Horizon 2020 research and innovation programme
#> 853                                                                                                                                                                 European Union
#> 854                                                                                                                                                                 European Union
#> 855                                                                                                                                        European Research Council under the ERC
#> 856                                    European Union's Horizon 2020 research and innovation programme as part of the Marie Sklodowska-Curie Innovative Training Network MCnetITN3
#> 857                                                                                                                                                                 European Union
#> 858                                                                              European Union's Horizon research and innovation programme under the Marie Sklodowska-Curie grant
#>                                    GRANTNUMBER
#> 1                                       690575
#> 2                                       674896
#> 3                                       617143
#> 4                                       690575
#> 5                                       674896
#> 6                                       674896
#> 7                                       690575
#> 8                                       690575
#> 9                                       674896
#> 10                                      674896
#> 11                                      690575
#> 12                                      690575
#> 13                                      674896
#> 14                                      674896
#> 15                                      690575
#> 16                                      669255
#> 17                                      731583
#> 18                                      674896
#> 19                                      690575
#> 20                                      690575
#> 21                                      674896
#> 22                                      690575
#> 23                                      674896
#> 24                                      690575
#> 25                         648680 DARKHORIZONS
#> 26                                      720776
#> 27                                      720776
#> 28                                      690575
#> 29                                      674896
#> 30                                      690575
#> 31                                      690575
#> 32                                      674896
#> 33                                      674896
#> 34                                      690575
#> 35                                      674896
#> 36                                      690575
#> 37                                      674896
#> 38                                      674896
#> 39                                      690575
#> 40                                      674896
#> 41                                      674896
#> 42                                      690575
#> 43                                      640979
#> 44                                      674896
#> 45                                 339787-NEXT
#> 46                                      690575
#> 47                                      740055
#> 48                                    20142020
#> 49                                      674896
#> 50                                 339787-NEXT
#> 51                                      740055
#> 52                                      690575
#> 53                                      690575
#> 54                                      674896
#> 55                                      742789
#> 56                                      690575
#> 57                                      674896
#> 58                                      637506
#> 59                                      690575
#> 60                                      674896
#> 61                                      674896
#> 62                                      340983
#> 63                                      722104
#> 64                                      640979
#> 65                                      674896
#> 66                                      690575
#> 67                                      643410
#> 68                                      690575
#> 69                                      674896
#> 70                                      690575
#> 71                                      674896
#> 72                                      674896
#> 73                                      690575
#> 74                                      690575
#> 75                                      674896
#> 76                                      674896
#> 77                                      690575
#> 78                                      674896
#> 79                                      692194
#> 80                                      690575
#> 81                                      674896
#> 82                                      689341
#> 83                                      722104
#> 84                                      674896
#> 85                                      690575
#> 86                                      690575
#> 87                                      674896
#> 88                                      690575
#> 89                                      690575
#> 90                                      674896
#> 91                                      690575
#> 92                                      674896
#> 93                                      690575
#> 94                                      690575
#> 95                                      674896
#> 96                                      674896
#> 97                                      690575
#> 98                                      692194
#> 99                                      690575
#> 100                                     762013
#> 101                                     674896
#> 102                                     690575
#> 103                                     690575
#> 104                                     674896
#> 105                                     690575
#> 106                                     674896
#> 107                                     674896
#> 108                                     721290
#> 109                                     690575
#> 110                                     640979
#> 111                                     690575
#> 112                                     690575
#> 113                                     674896
#> 114                                     722104
#> 115                                     690575
#> 116                                     674896
#> 117                                     674896
#> 118                                     690575
#> 119                                     690575
#> 120                                     674896
#> 121                                     674896
#> 122                                     690575
#> 123                                     607380
#> 124                                     739563
#> 125                                     676754
#> 126                                     690575
#> 127                                     690575
#> 128                                339787-NEXT
#> 129                                     740055
#> 130                                     674896
#> 131                                     668679
#> 132                                     674896
#> 133                                     682586
#> 134                                     690575
#> 135                                339787-NEXT
#> 136                                     690575
#> 137                                     740055
#> 138                                     674896
#> 139                                     674896
#> 140                                     690575
#> 141                                     742104
#> 142                                     722104
#> 143                                     614577
#> 144                                     690575
#> 145                                     674896
#> 146                                     674896
#> 147                                     690575
#> 148                                     283595
#> 149                                     690575
#> 150                                     674896
#> 151                                     690575
#> 152                                     674896
#> 153                                     674896
#> 154                                     690575
#> 155                                     289442
#> 156                                     674896
#> 157                                     690575
#> 158                                     674896
#> 159                                     690575
#> 160                                     674896
#> 161                                     690575
#> 162                                     690575
#> 163                                     674896
#> 164                                     690575
#> 165                                     267985
#> 166                                     640979
#> 167                                     600405
#> 168                                     690575
#> 169                                     674896
#> 170                                     690575
#> 171                                     690575
#> 172                                     674896
#> 173                                     674896
#> 174                                     690575
#> 175                                     640979
#> 176                                     690575
#> 177                                     674896
#> 178                                     674896
#> 179                                     692194
#> 180                                     690575
#> 181                                     690575
#> 182                                     278234
#> 183                                     674896
#> 184                                     690575
#> 185                                     674896
#> 186                                     731761
#> 187                                     610532
#> 188                                     690575
#> 189                                     674896
#> 190                                     267985
#> 191                                     674896
#> 192                                     690575
#> 193                                     722104
#> 194                                     690575
#> 195                                     674896
#> 196                                     674896
#> 197                                     692194
#> 198                                     690575
#> 199                                     674896
#> 200                                     640979
#> 201                                     674896
#> 202                                     690575
#> 203                                     674896
#> 204                                     690575
#> 205                                     674896
#> 206                                     674896
#> 207                                     690575
#> 208                                     674896
#> 209                                     690575
#> 210                                     674896
#> 211                                     692194
#> 212                                     674896
#> 213                                     690575
#> 214                                     676559
#> 215                                     739563
#> 216                                     653706
#> 217                                     675858
#> 218                                     654248
#> 219                                     674896
#> 220                                     692194
#> 221                                     690575
#> 222                                     674896
#> 223                                     690575
#> 224                                     690575
#> 225                                     267985
#> 226                                     674896
#> 227                                     690575
#> 228                                     674896
#> 229                                     722104
#> 230                                     674896
#> 231                                     690575
#> 232                                     731583
#> 233                                     608622
#> 234                                     308393
#> 235                                     284181
#> 236                                     674896
#> 237                                     750627
#> 238                                     690575
#> 239                                     682586
#> 240                                     690575
#> 241                                     674896
#> 242                                     690575
#> 243                                     674896
#> 244                            682676 LDMThExp
#> 245                                     690575
#> 246                                     690575
#> 247                                     674896
#> 248                                     690575
#> 249                                     674896
#> 250                                     674896
#> 251                                     690575
#> 252                        648680 DARKHORIZONS
#> 253                       788781/ERC-AdG IAXO+
#> 254                                     690575
#> 255                                     690575
#> 256                                     674896
#> 257                                     690575
#> 258                                     720996
#> 259                                     722104
#> 260                                     690575
#> 261                                     690575
#> 262                                339787-NEXT
#> 263                                     674896
#> 264                                     674896
#> 265                                     690575
#> 266                                     674896
#> 267                                     740055
#> 268                                     690575
#> 269                                     674896
#> 270                                     278234
#> 271                                     674896
#> 272                                     674896
#> 273                                     690575
#> 274                                     690575
#> 275                                     674896
#> 276                                     690575
#> 277                                     674896
#> 278                                     674896
#> 279                                     690575
#> 280                                     690575
#> 281                                     674896
#> 282                                     674896
#> 283                                     690575
#> 284                                     690575
#> 285                                     690575
#> 286                                     674896
#> 287                                     674896
#> 288                                     674896
#> 289                                     690575
#> 290                                     674896
#> 291                                     690575
#> 292                                339787-NEXT
#> 293                                     690575
#> 294                                     674896
#> 295                                     617143
#> 296                                     680431
#> 297                                     690575
#> 298                                     690575
#> 299                                     690575
#> 300                                     674896
#> 301                                     690575
#> 302                                     674896
#> 303                                     690575
#> 304                                     674896
#> 305                                     690575
#> 306                                     674896
#> 307                                     720996
#> 308                                     668679
#> 309                                     690575
#> 310                                     674896
#> 311                                     823734
#> 312                                     690575
#> 313                                     674896
#> 314                                     690575
#> 315                                     674896
#> 316                                     668679
#> 317                                     689341
#> 318                                     640979
#> 319                                     674896
#> 320                                     690575
#> 321                                     727520
#> 322                                     690575
#> 323                                     722104
#> 324                                     690575
#> 325                                     674896
#> 326                                     690575
#> 327                                     690575
#> 328                                     690575
#> 329                                     674896
#> 330                                     674896
#> 331                                     674896
#> 332                                     721290
#> 333                                     690575
#> 334                                     674896
#> 335                                     674896
#> 336                                     690575
#> 337                                     690575
#> 338                                     674896
#> 339                                     690575
#> 340                                     690575
#> 341                                     692194
#> 342                                     690575
#> 343                                     640979
#> 344                                     723014
#> 345                                     640979
#> 346                                     690575
#> 347                                     279757
#> 348                                     722104
#> 349                                     674896
#> 350                                     690575
#> 351                                     690575
#> 352                                     690575
#> 353                                     674896
#> 354                                     690575
#> 355                                     674896
#> 356                                     674896
#> 357                                     690575
#> 358                                     674896
#> 359                                     674896
#> 360                                     723014
#> 361                                     674896
#> 362                                     690575
#> 363                                     690575
#> 364                                     680431
#> 365                                     674896
#> 366                                     690575
#> 367                                     674896
#> 368                                     674896
#> 369                                     690575
#> 370                                     291763
#> 371                                     640979
#> 372                                     674896
#> 373                                     690575
#> 374                                     267104
#> 375                                     668679
#> 376                                     638528
#> 377                                     690575
#> 378                                     674896
#> 379                                     690575
#> 380                                     674896
#> 381                                     690575
#> 382                                     643410
#> 383                                     674896
#> 384                                     690575
#> 385                                     722104
#> 386                                     636811
#> 387                                     674896
#> 388 690575-InvisiblesPlus-H2020-MSCA-RISE-2015
#> 389                                     690575
#> 390                         720757 Tech4Effect
#> 391                                     646747
#> 392                                     690575
#> 393                                     722104
#> 394                                     715049
#> 395                                     674896
#> 396                                     267985
#> 397                                     690575
#> 398                                     722104
#> 399                                     690575
#> 400                                     669255
#> 401                                     690575
#> 402                                     721290
#> 403                                     674896
#> 404                                     731583
#> 405                                     690575
#> 406                                     674896
#> 407                                     690575
#> 408                                     674896
#> 409                                     674896
#> 410                                     674896
#> 411                                     674896
#> 412                                     690575
#> 413                                     690575
#> 414                                     690575
#> 415                                     674896
#> 416                                     690575
#> 417                                     690575
#> 418                                     674896
#> 419                                     674896
#> 420                                     690575
#> 421                                     270129
#> 422                                     674896
#> 423                                     715049
#> 424                                     637506
#> 425                                     722104
#> 426                                     267209
#> 427                                     690575
#> 428                                     674896
#> 429                                     690575
#> 430                                     674896
#> 431                                     674896
#> 432                                     710722
#> 433                                     674896
#> 434                                     742104
#> 435                                     690575
#> 436                                     674896
#> 437                        674896-Elusives ITN
#> 438                                     674896
#> 439                                     796941
#> 440                 690575-InvisiblesPlus RISE
#> 441                                     690575
#> 442                                     690575
#> 443                                     668679
#> 444                                     731583
#> 445                                     654148
#> 446                                     690575
#> 447                                     720996
#> 448                                     669255
#> 449                                     724932
#> 450                                     690575
#> 451                                     640979
#> 452                                     668679
#> 453                                     617511
#> 454                                     690575
#> 455                                     674896
#> 456                                     674896
#> 457                                     722610
#> 458                                     720776
#> 459                           341157-COCO2CASA
#> 460                                     690575
#> 461                                     720776
#> 462                                     674896
#> 463                                     674896
#> 464                                     721290
#> 465 690575-InvisiblesPlus-H2020-MSCA-RISE-2015
#> 466                                     690575
#> 467                                     722104
#> 468                                     690575
#> 469                                     674896
#> 470                                     690575
#> 471                                     309067
#> 472                                     690575
#> 473                                     674896
#> 474                                     674896
#> 475                            674896-Elusives
#> 476                      690575-InvisiblesPlus
#> 477                                     674896
#> 478                                     690575
#> 479                                     690575
#> 480                                     690575
#> 481                                     674896
#> 482                                     674896
#> 483                                     674896
#> 484                                     690575
#> 485                                     674896
#> 486                                     674896
#> 487                                     674896
#> 488                                     796941
#> 489                                     722104
#> 490                                     690575
#> 491                                     674896
#> 492                                     690575
#> 493                                     690575
#> 494                                     690575
#> 495                                     690575
#> 496                                     674896
#> 497                                     656697
#> 498                                     677353
#> 499                                     674896
#> 500                                     674896
#> 501                                     674896
#> 502                                     690575
#> 503                                     690575
#> 504                                     674896
#> 505                                     690575
#> 506                                     674896
#> 507                                     690575
#> 508                                     690575
#> 509                                     674896
#> 510                                     674896
#> 511                                     690575
#> 512                                     340983
#> 513                                     660171
#> 514                                     722104
#> 515                                     674896
#> 516      740006NNNPDFERC-2016-ADG/ERC-2016-ADG
#> 517                                     668679
#> 518                                     722104
#> 519                                     690575
#> 520                                     339169
#> 521                                     674896
#> 522                                     690575
#> 523                                     674896
#> 524                                     690575
#> 525                                     690575
#> 526                                     674896
#> 527                                     674896
#> 528                                     674896
#> 529                                     610028
#> 530                                     676754
#> 531                                     267985
#> 532                                     690575
#> 533                                     674896
#> 534                                     690575
#> 535                                     690575
#> 536                                     674896
#> 537                                     690575
#> 538                                     692194
#> 539                                     674896
#> 540                                     674896
#> 541                                     690575
#> 542                                     690575
#> 543                                     674896
#> 544                                     690575
#> 545                                     722104
#> 546                                     674896
#> 547                                     674896
#> 548                        648680 DARKHORIZONS
#> 549                                     674896
#> 550                                     690575
#> 551                                     643964
#> 552                                     722104
#> 553                                     690575
#> 554                                     731583
#> 555                                     674896
#> 556                                     723014
#> 557                                     674896
#> 558                                     674896
#> 559                                     674896
#> 560                                     690575
#> 561                                     617143
#> 562                                     680431
#> 563                                     617143
#> 564                                     690575
#> 565                                     674896
#> 566                                     690575
#> 567                                     674896
#> 568                                     690575
#> 569                                     690575
#> 570                                     674896
#> 571                                     674896
#> 572                                     637506
#> 573                                     690575
#> 574                                     674896
#> 575                                     690575
#> 576                                     674896
#> 577                                     674896
#> 578                                     690575
#> 579                                     690575
#> 580                                     648055
#> 581                                     674896
#> 582                                     640979
#> 583                                     690575
#> 584                                     674896
#> 585                                     674896
#> 586                                     690575
#> 587                                     690575
#> 588                                     722104
#> 589                                     674896
#> 590                                     674896
#> 591                                     690575
#> 592                                     690575
#> 593                                     690575
#> 594                                     674896
#> 595                                     677353
#> 596                                     690575
#> 597                                     674896
#> 598                                     722104
#> 599                                     674896
#> 600                                     690575
#> 601                                     690575
#> 602                                     674896
#> 603                                     674896
#> 604                                     690575
#> 605                                     674896
#> 606                                     690575
#> 607                                     617143
#> 608                                     690575
#> 609                                     674896
#> 610                                     640979
#> 611                                     690575
#> 612                                     690575
#> 613                                     690575
#> 614                                     674896
#> 615                                     690575
#> 616                                     690575
#> 617                                     674896
#> 618                                     674896
#> 619                                     674896
#> 620                                     690575
#> 621                                     674896
#> 622                                     690575
#> 623                                     742789
#> 624                                     674896
#> 625                                     674896
#> 626                                     690575
#> 627                                     690575
#> 628                                     674896
#> 629                                     690575
#> 630                                     690575
#> 631                                     674896
#> 632                                     690575
#> 633                                     674896
#> 634                                     309067
#> 635                                     636811
#> 636                                     689341
#> 637                                     722104
#> 638                                     690575
#> 639                                     674896
#> 640                                     690575
#> 641                                     674896
#> 642                                     690575
#> 643                                     674896
#> 644                                     692194
#> 645                                     674896
#> 646                                     690575
#> 647                                     690575
#> 648                                     674896
#> 649                                     690575
#> 650                                     674896
#> 651                                     674896
#> 652                                339787-NEXT
#> 653                                     690575
#> 654                                     674896
#> 655                                     637506
#> 656                                     675512
#> 657                                     674896
#> 658                                     690575
#> 659                                     690575
#> 660                                     690575
#> 661                                     690575
#> 662                                     674896
#> 663                                     690575
#> 664                                     674896
#> 665                                     674896
#> 666                                     674896
#> 667                                     674896
#> 668                                     674896
#> 669                                     690575
#> 670                                     722104
#> 671                                     646747
#> 672                                     690575
#> 673                                     646747
#> 674                                     690575
#> 675                                     674896
#> 676                                     690575
#> 677                                     674896
#> 678                                     690575
#> 679                                     674896
#> 680                                     636811
#> 681                                     674896
#> 682                                     690575
#> 683                                     690575
#> 684                                     690575
#> 685                                     690575
#> 686                                     674896
#> 687                                     690575
#> 688                                     645722
#> 689                                     674896
#> 690                                     674896
#> 691                                     674896
#> 692                                     690575
#> 693                        648680 DARKHORIZONS
#> 694                                     674896
#> 695                                     674896
#> 696                                     674896
#> 697                                     674896
#> 698                                     690575
#> 699                                     690575
#> 700                                     690575
#> 701                                     617143
#> 702                                     690575
#> 703                                     690575
#> 704                                     690575
#> 705                                     690575
#> 706                                     690575
#> 707                                     674896
#> 708                                     674896
#> 709                                     674896
#> 710                                     674896
#> 711                                     690575
#> 712                                     674896
#> 713                                     674896
#> 714                                     722104
#> 715                                     690575
#> 716                                     674896
#> 717                                     674896
#> 718                                     609412
#> 719                                     690575
#> 720                                     690575
#> 721                                     617143
#> 722                                     752533
#> 723                                     674896
#> 724                                     674896
#> 725                                     674896
#> 726                                     690575
#> 727                                     690575
#> 728                                     690575
#> 729                                     674896
#> 730                                     674896
#> 731                                     722610
#> 732                                     720776
#> 733                                     690575
#> 734                                     646747
#> 735                                     690575
#> 736                                     674896
#> 737                                     690575
#> 738                                     674896
#> 739                                     690575
#> 740                                     690575
#> 741                                     267117
#> 742                               677353 IMAGE
#> 743                                     674896
#> 744                                     721290
#> 745                                     690575
#> 746                                     674896
#> 747                                     690575
#> 748                                     690575
#> 749                                     674896
#> 750                                     690575
#> 751                                     674896
#> 752                                     674896
#> 753                                     690575
#> 754                                     691149
#> 755                                     690575
#> 756                                     645722
#> 757                                     690575
#> 758                                     674896
#> 759                                     690575
#> 760                                     690575
#> 761                                     674896
#> 762                                     690575
#> 763                                     674896
#> 764                                     674896
#> 765                                     690575
#> 766                                     690575
#> 767                                     674896
#> 768                                     690575
#> 769                                     674896
#> 770                                     692194
#> 771                                     690575
#> 772                                     674896
#> 773                                     674896
#> 774                                     674896
#> 775                                     674896
#> 776                                     690575
#> 777                                     690575
#> 778                                     674896
#> 779                                     690575
#> 780                                     690575
#> 781                                     674896
#> 782                                     289442
#> 783                                     674896
#> 784                                     689341
#> 785                                     690575
#> 786                                     690575
#> 787                                     668679
#> 788                                     674896
#> 789                                     690575
#> 790                                     674896
#> 791                                     690575
#> 792                                     674896
#> 793                                     674896
#> 794                                     690575
#> 795                                     677353
#> 796                                     674896
#> 797                                     674896
#> 798                                     690575
#> 799                                     722104
#> 800                                     721290
#> 801                                     674896
#> 802                                     690575
#> 803                                     674896
#> 804                                     690575
#> 805                                     690575
#> 806                                     674896
#> 807                                     674896
#> 808                                     690575
#> 809                                     721290
#> 810                                     278234
#> 811                                     674896
#> 812                                     674896
#> 813                                     690575
#> 814                                     674896
#> 815                                     690575
#> 816                                     690575
#> 817                                     690575
#> 818                                     682586
#> 819                                     690575
#> 820                                     674896
#> 821                                     640979
#> 822                                     674896
#> 823                                     722104
#> 824                                     674896
#> 825                                     668679
#> 826                                     690575
#> 827                                     289442
#> 828                                     674896
#> 829                                     690575
#> 830                                     690575
#> 831                                     690575
#> 832                                     674896
#> 833                                     690575
#> 834                                     309067
#> 835                                     640979
#> 836                                     640979
#> 837                                     636811
#> 838                                     674896
#> 839                                     690575
#> 840                                     690575
#> 841                                     690575
#> 842                                     674896
#> 843                                     674896
#> 844                                     646747
#> 845                                     690575
#> 846                                     722104
#> 847                                     674896
#> 848                                     690575
#> 849                                     690575
#> 850                                     690575
#> 851                                     674896
#> 852                                     676754
#> 853                                     690575
#> 854                                     674896
#> 855                                     615172
#> 856                                     722104
#> 857                                     643161
#> 858                                     690575
#>                                 DOI eu_funding
#> 1        10.1103/PhysRevD.94.013002         EU
#> 2        10.1103/PhysRevD.94.013002         EU
#> 3        10.1103/PhysRevD.94.013002         EU
#> 4     10.1088/1475-7516/2018/04/039         EU
#> 5     10.1088/1475-7516/2018/04/039         EU
#> 6           10.1007/JHEP04(2018)101         EU
#> 7           10.1007/JHEP04(2018)101         EU
#> 8           10.1007/JHEP05(2018)066         EU
#> 9           10.1007/JHEP05(2018)066         EU
#> 10          10.1007/JHEP10(2017)162         EU
#> 11          10.1007/JHEP10(2017)162         EU
#> 12   10.1140/epjc/s10052-018-5816-y         EU
#> 13   10.1140/epjc/s10052-018-5816-y         EU
#> 14       10.1103/PhysRevD.97.075039         EU
#> 15       10.1103/PhysRevD.97.075039         EU
#> 16          10.1145/3133956.3133991         EU
#> 17          10.1145/3133956.3133991         EU
#> 18   10.1103/PhysRevLett.120.132503         EU
#> 19   10.1103/PhysRevLett.120.132503         EU
#> 20   10.1140/epjc/s10052-018-5734-z         EU
#> 21   10.1140/epjc/s10052-018-5734-z         EU
#> 22          10.1007/JHEP08(2018)022         EU
#> 23          10.1007/JHEP08(2018)022         EU
#> 24       10.1103/PhysRevD.98.035038         EU
#> 25   10.1140/epjc/s10052-017-5111-3         EU
#> 26            10.1128/mBio.01415-18         EU
#> 27               10.1093/nar/gky383         EU
#> 28       10.1103/PhysRevD.98.063531         EU
#> 29       10.1103/PhysRevD.98.055032         EU
#> 30       10.1103/PhysRevD.98.055032         EU
#> 31    10.1088/1475-7516/2018/09/027         EU
#> 32    10.1088/1475-7516/2018/09/027         EU
#> 33  10.1016/j.nuclphysb.2018.07.022         EU
#> 34  10.1016/j.nuclphysb.2018.07.022         EU
#> 35          10.1007/JHEP10(2018)017         EU
#> 36          10.1007/JHEP10(2018)017         EU
#> 37          10.1007/JHEP09(2018)124         EU
#> 38    10.1088/1475-7516/2018/10/009         EU
#> 39   10.1140/epjc/s10052-018-6250-x         EU
#> 40   10.1140/epjc/s10052-018-6250-x         EU
#> 41       10.1103/PhysRevD.98.083519         EU
#> 42       10.1103/PhysRevD.98.083519         EU
#> 43             10.1002/2015WR018369         EU
#> 44          10.1007/JHEP10(2018)112         EU
#> 45          10.1007/JHEP10(2018)112         EU
#> 46          10.1007/JHEP10(2018)112         EU
#> 47          10.1007/JHEP10(2018)112         EU
#> 48          10.1007/JHEP10(2018)112         EU
#> 49   10.1088/1748-0221/13/10/P10020         EU
#> 50   10.1088/1748-0221/13/10/P10020         EU
#> 51   10.1088/1748-0221/13/10/P10020         EU
#> 52   10.1088/1748-0221/13/10/P10020         EU
#> 53   10.1088/1748-0221/13/10/P10022         EU
#> 54   10.1088/1748-0221/13/10/P10022         EU
#> 55   10.1088/1748-0221/13/10/P10022         EU
#> 56          10.1007/JHEP10(2018)183         EU
#> 57          10.1007/JHEP10(2018)183         EU
#> 58          10.1007/JHEP10(2018)187         EU
#> 59       10.1103/PhysRevD.98.093001         EU
#> 60       10.1103/PhysRevD.98.093001         EU
#> 61          10.3389/fphy.2017.00066         EU
#> 62       10.1103/PhysRevD.98.096002         EU
#> 63       10.1103/PhysRevD.98.096002         EU
#> 64             10.1002/2017WR020905         EU
#> 65          10.1007/JHEP06(2016)147         EU
#> 66          10.1007/JHEP06(2016)147         EU
#> 67     10.1371/journal.pone.0189311         EU
#> 68   10.1140/epjc/s10052-017-4963-x         EU
#> 69   10.1140/epjc/s10052-017-4963-x         EU
#> 70       10.1103/PhysRevD.96.056026         EU
#> 71       10.1103/PhysRevD.96.056026         EU
#> 72         10.1088/1361-6471/aa9098         EU
#> 73         10.1088/1361-6471/aa9098         EU
#> 74       10.1103/PhysRevD.97.023017         EU
#> 75       10.1103/PhysRevD.97.023017         EU
#> 76    10.1088/1475-7516/2017/12/038         EU
#> 77       10.1103/PhysRevD.97.015008         EU
#> 78       10.1103/PhysRevD.97.015008         EU
#> 79       10.1103/PhysRevD.97.015008         EU
#> 80   10.1016/j.physletb.2017.12.058         EU
#> 81   10.1016/j.physletb.2017.12.058         EU
#> 82        10.1007/s10458-016-9344-6         EU
#> 83   10.1140/epjc/s10052-018-5585-7         EU
#> 84    10.1088/1475-7516/2017/04/048         EU
#> 85    10.1088/1475-7516/2017/04/048         EU
#> 86          10.1007/JHEP01(2018)159         EU
#> 87   10.1016/j.physletb.2017.01.014         EU
#> 88   10.1016/j.physletb.2017.01.014         EU
#> 89  10.1016/j.nuclphysb.2017.12.013         EU
#> 90  10.1016/j.nuclphysb.2017.12.013         EU
#> 91          10.1007/JHEP07(2016)033         EU
#> 92          10.1007/JHEP07(2016)033         EU
#> 93       10.1103/PhysRevD.97.064015         EU
#> 94          10.1007/JHEP05(2017)010         EU
#> 95          10.1007/JHEP05(2017)010         EU
#> 96          10.1007/JHEP04(2017)016         EU
#> 97          10.1007/JHEP04(2017)016         EU
#> 98          10.1007/JHEP04(2017)016         EU
#> 99       10.1103/PhysRevD.97.124010         EU
#> 100       10.1109/TNSM.2018.2796720         EU
#> 101         10.1007/JHEP06(2018)032         EU
#> 102         10.1007/JHEP06(2018)032         EU
#> 103         10.1007/JHEP07(2017)089         EU
#> 104         10.1007/JHEP07(2017)089         EU
#> 105  10.1103/PhysRevLett.120.141101         EU
#> 106         10.1007/JHEP07(2018)079         EU
#> 107         10.1007/JHEP07(2018)024         EU
#> 108              10.1039/c8gc00669e         EU
#> 109  10.1016/j.physletb.2017.03.037         EU
#> 110       10.1007/s11004-015-9625-7         EU
#> 111      10.1103/PhysRevD.95.055019         EU
#> 112         10.1007/JHEP04(2018)002         EU
#> 113         10.1007/JHEP04(2018)002         EU
#> 114         10.1007/JHEP07(2018)087         EU
#> 115         10.1007/JHEP07(2018)036         EU
#> 116         10.1007/JHEP07(2018)036         EU
#> 117  10.1016/j.physletb.2018.05.057         EU
#> 118  10.1016/j.physletb.2018.05.057         EU
#> 119         10.1007/JHEP07(2018)094         EU
#> 120      10.1103/PhysRevD.98.023020         EU
#> 121      10.1103/PhysRevD.98.041301         EU
#> 122      10.1103/PhysRevD.98.041301         EU
#> 123        10.1088/1538-3873/aac5d2         EU
#> 124        10.1088/1538-3873/aac5d2         EU
#> 125                10.3390/f9070438         EU
#> 126         10.1007/JHEP07(2018)033         EU
#> 127  10.1088/1748-0221/13/07/P07013         EU
#> 128  10.1088/1748-0221/13/07/P07013         EU
#> 129  10.1088/1748-0221/13/07/P07013         EU
#> 130  10.1088/1748-0221/13/07/P07013         EU
#> 131         10.1007/JHEP11(2018)149         EU
#> 132         10.1007/JHEP11(2018)163         EU
#> 133          10.1002/anie.201808745         EU
#> 134  10.1016/j.physletb.2018.09.057         EU
#> 135  10.1088/1748-0221/13/12/P12010         EU
#> 136  10.1088/1748-0221/13/12/P12010         EU
#> 137  10.1088/1748-0221/13/12/P12010         EU
#> 138  10.1088/1748-0221/13/12/P12010         EU
#> 139  10.1140/epjc/s10052-018-6149-6         EU
#> 140  10.1140/epjc/s10052-018-6149-6         EU
#> 141      10.1103/PhysRevD.98.123006         EU
#> 142  10.1140/epjc/s10052-018-6136-y         EU
#> 143  10.1140/epjc/s10052-018-6136-y         EU
#> 144  10.1016/j.physletb.2018.08.025         EU
#> 145  10.1016/j.physletb.2018.08.025         EU
#> 146         10.1007/JHEP11(2018)173         EU
#> 147         10.1007/JHEP11(2018)173         EU
#> 148       10.1007/s10032-015-0249-8         EU
#> 149         10.1007/JHEP08(2016)165         EU
#> 150      10.1103/PhysRevD.94.055005         EU
#> 151      10.1103/PhysRevD.95.063003         EU
#> 152      10.1103/PhysRevD.95.063003         EU
#> 153      10.1103/PhysRevD.95.035021         EU
#> 154      10.1103/PhysRevD.94.023512         EU
#> 155   10.1088/1475-7516/2017/01/056         EU
#> 156   10.1088/1475-7516/2017/01/056         EU
#> 157   10.1088/1475-7516/2017/01/056         EU
#> 158     10.1209/0295-5075/117/39001         EU
#> 159     10.1209/0295-5075/117/39001         EU
#> 160      10.1103/PhysRevD.95.043506         EU
#> 161      10.1103/PhysRevD.95.043506         EU
#> 162      10.1103/PhysRevD.94.044027         EU
#> 163   10.1088/1475-7516/2017/02/011         EU
#> 164   10.1088/1475-7516/2017/02/011         EU
#> 165      10.1103/PhysRevD.94.115033         EU
#> 166   10.1016/j.jhydrol.2017.02.040         EU
#> 167   10.1016/j.jhydrol.2017.02.040         EU
#> 168   10.1088/1475-7516/2017/04/017         EU
#> 169   10.1088/1475-7516/2017/04/017         EU
#> 170      10.1103/PhysRevD.95.083514         EU
#> 171   10.1088/1475-7516/2016/11/054         EU
#> 172   10.1088/1475-7516/2016/11/054         EU
#> 173         10.1007/JHEP01(2017)141         EU
#> 174         10.1007/JHEP01(2017)141         EU
#> 175                10.3390/w9040252         EU
#> 176         10.1007/JHEP11(2016)084         EU
#> 177         10.1007/JHEP11(2016)084         EU
#> 178  10.1140/epjc/s10052-017-5126-9         EU
#> 179  10.1140/epjc/s10052-017-5126-9         EU
#> 180  10.1140/epjc/s10052-017-5126-9         EU
#> 181  10.1103/PhysRevLett.119.021103         EU
#> 182  10.1103/PhysRevLett.119.021103         EU
#> 183  10.1103/PhysRevLett.119.021103         EU
#> 184         10.1007/JHEP10(2017)029         EU
#> 185         10.1007/JHEP10(2017)029         EU
#> 186        10.1177/1059712317726357         EU
#> 187        10.1177/1059712317726357         EU
#> 188         10.1007/JHEP09(2016)023         EU
#> 189         10.1007/JHEP09(2016)023         EU
#> 190  10.1103/PhysRevLett.118.011801         EU
#> 191         10.1007/JHEP01(2017)078         EU
#> 192         10.1007/JHEP01(2017)078         EU
#> 193         10.1007/JHEP10(2017)096         EU
#> 194      10.1016/j.dark.2017.07.003         EU
#> 195      10.1016/j.dark.2017.07.003         EU
#> 196  10.1140/epjc/s10052-017-4767-z         EU
#> 197  10.1140/epjc/s10052-017-4767-z         EU
#> 198  10.1140/epjc/s10052-017-4767-z         EU
#> 199  10.1016/j.physletb.2017.08.050         EU
#> 200    10.1016/j.enggeo.2017.08.011         EU
#> 201      10.1103/PhysRevD.94.115021         EU
#> 202      10.1103/PhysRevD.96.103510         EU
#> 203      10.1103/PhysRevD.96.103510         EU
#> 204      10.1103/PhysRevD.96.123503         EU
#> 205      10.1103/PhysRevD.96.123503         EU
#> 206      10.1103/PhysRevD.96.122002         EU
#> 207   10.1088/1475-7516/2016/09/012         EU
#> 208   10.1088/1475-7516/2016/09/012         EU
#> 209  10.1140/epjc/s10052-017-5133-x         EU
#> 210  10.1140/epjc/s10052-017-5133-x         EU
#> 211  10.1140/epjc/s10052-017-5133-x         EU
#> 212      10.1103/PhysRevD.94.083522         EU
#> 213      10.1103/PhysRevD.94.083522         EU
#> 214       10.1016/j.jsb.2018.10.001         EU
#> 215       10.1016/j.jsb.2018.10.001         EU
#> 216       10.1016/j.jsb.2018.10.001         EU
#> 217       10.1016/j.jsb.2018.10.001         EU
#> 218       10.1016/j.jsb.2018.10.001         EU
#> 219        10.1088/1361-6471/aaf5de         EU
#> 220        10.1088/1361-6471/aaf5de         EU
#> 221        10.1088/1361-6471/aaf5de         EU
#> 222  10.1103/PhysRevLett.121.241801         EU
#> 223  10.1103/PhysRevLett.121.241801         EU
#> 224         10.1007/JHEP11(2018)191         EU
#> 225         10.1007/JHEP11(2018)191         EU
#> 226         10.1007/JHEP11(2018)191         EU
#> 227         10.1007/JHEP12(2018)003         EU
#> 228         10.1007/JHEP12(2018)003         EU
#> 229         10.1007/JHEP12(2018)088         EU
#> 230         10.1007/JHEP12(2018)007         EU
#> 231         10.1007/JHEP12(2018)007         EU
#> 232    10.1007/978-3-319-76578-5_19         EU
#> 233       10.1186/s40663-019-0163-5         EU
#> 234       10.1186/s40663-019-0163-5         EU
#> 235       10.1186/s40663-019-0163-5         EU
#> 236         10.1007/JHEP01(2019)164         EU
#> 237         10.1007/JHEP01(2019)164         EU
#> 238         10.1007/JHEP01(2019)164         EU
#> 239          10.1002/chem.201805882         EU
#> 240      10.1103/PhysRevD.99.023522         EU
#> 241      10.1103/PhysRevD.99.023522         EU
#> 242         10.1007/JHEP01(2019)119         EU
#> 243         10.1007/JHEP01(2019)119         EU
#> 244      10.1103/PhysRevD.99.015022         EU
#> 245      10.1103/PhysRevD.99.015022         EU
#> 246 10.1016/j.nuclphysb.2018.12.016         EU
#> 247 10.1016/j.nuclphysb.2018.12.016         EU
#> 248   10.1088/1475-7516/2018/05/040         EU
#> 249   10.1088/1475-7516/2018/05/040         EU
#> 250      10.1103/PhysRevD.99.035031         EU
#> 251      10.1103/PhysRevD.99.035031         EU
#> 252      10.1103/PhysRevD.99.035031         EU
#> 253      10.1103/PhysRevD.99.035037         EU
#> 254      10.1103/PhysRevD.99.064010         EU
#> 255  10.1103/PhysRevLett.122.091102         EU
#> 256      10.1103/PhysRevD.99.075017         EU
#> 257      10.1103/PhysRevD.99.075017         EU
#> 258              10.1039/c8sc05510f         EU
#> 259         10.1007/JHEP04(2019)016         EU
#> 260         10.1007/JHEP12(2017)075         EU
#> 261      10.1103/PhysRevD.96.103539         EU
#> 262  10.1088/1748-0221/13/10/P10014         EU
#> 263  10.1103/PhysRevLett.119.241101         EU
#> 264  10.1088/1748-0221/13/10/P10014         EU
#> 265  10.1103/PhysRevLett.119.241101         EU
#> 266         10.1007/JHEP12(2017)075         EU
#> 267  10.1088/1748-0221/13/10/P10014         EU
#> 268  10.1088/1748-0221/13/10/P10014         EU
#> 269      10.1103/PhysRevD.96.103539         EU
#> 270  10.1103/PhysRevLett.119.241101         EU
#> 271         10.1007/JHEP12(2017)022         EU
#> 272         10.1007/JHEP03(2019)067         EU
#> 273         10.1007/JHEP12(2017)022         EU
#> 274         10.1007/JHEP03(2019)067         EU
#> 275         10.1007/JHEP12(2016)014         EU
#> 276         10.1007/JHEP12(2016)014         EU
#> 277  10.1140/epjc/s10052-018-5812-2         EU
#> 278      10.1103/PhysRevD.97.115027         EU
#> 279         10.1007/JHEP10(2017)002         EU
#> 280  10.1140/epjc/s10052-018-5812-2         EU
#> 281         10.1007/JHEP11(2016)035         EU
#> 282         10.1007/JHEP10(2017)002         EU
#> 283      10.1103/PhysRevD.97.115027         EU
#> 284   10.1088/1475-7516/2017/12/024         EU
#> 285         10.1007/JHEP06(2017)109         EU
#> 286         10.1007/JHEP08(2018)141         EU
#> 287      10.1103/PhysRevD.98.015036         EU
#> 288       10.1142/S0217751X18420010         EU
#> 289         10.1007/JHEP08(2018)141         EU
#> 290      10.1103/PhysRevD.98.075003         EU
#> 291        10.1088/1361-6382/aa6856         EU
#> 292      10.1016/j.nima.2018.11.126         EU
#> 293      10.1016/j.nima.2018.11.126         EU
#> 294      10.1016/j.nima.2018.11.126         EU
#> 295      10.1103/PhysRevD.98.015036         EU
#> 296     10.1016/j.robot.2017.09.019         EU
#> 297      10.1103/PhysRevD.98.015036         EU
#> 298       10.1142/S0217751X18420010         EU
#> 299   10.1088/1475-7516/2018/06/019         EU
#> 300      10.1103/PhysRevD.99.051702         EU
#> 301      10.1103/PhysRevD.97.123004         EU
#> 302      10.1103/PhysRevD.97.123004         EU
#> 303         10.1007/JHEP08(2016)079         EU
#> 304   10.1088/1475-7516/2018/06/019         EU
#> 305         10.1007/JHEP03(2018)067         EU
#> 306         10.1007/JHEP03(2018)067         EU
#> 307          10.1002/chem.201801114         EU
#> 308  10.1140/epjc/s10052-018-5645-z         EU
#> 309      10.1103/PhysRevD.99.051702         EU
#> 310      10.1103/PhysRevD.95.095008         EU
#> 311        10.3847/1538-4357/ab05c4         EU
#> 312   10.1088/1475-7516/2017/05/007         EU
#> 313   10.1088/1475-7516/2017/05/007         EU
#> 314      10.1103/PhysRevD.95.095008         EU
#> 315   10.1088/1475-7516/2018/07/009         EU
#> 316  10.1140/epjc/s10052-018-5940-8         EU
#> 317    10.1021/acs.analchem.8b00389         EU
#> 318 10.1016/j.advwatres.2017.10.031         EU
#> 319  10.1016/j.physletb.2017.09.011         EU
#> 320         10.1007/JHEP05(2018)030         EU
#> 321      10.1016/j.agsy.2018.10.009         EU
#> 322  10.1016/j.physletb.2017.09.011         EU
#> 323         10.1007/JHEP01(2018)051         EU
#> 324      10.1103/PhysRevD.97.015001         EU
#> 325         10.1007/JHEP05(2018)030         EU
#> 326         10.1007/JHEP02(2017)045         EU
#> 327       10.1016/j.aop.2018.05.001         EU
#> 328         10.1007/JHEP03(2017)117         EU
#> 329         10.1007/JHEP02(2017)045         EU
#> 330         10.1007/JHEP03(2017)117         EU
#> 331      10.1103/PhysRevD.95.055009         EU
#> 332          10.1002/cctc.201701712         EU
#> 333      10.1103/PhysRevD.97.084050         EU
#> 334         10.1007/JHEP10(2016)075         EU
#> 335  10.1140/epjc/s10052-017-5370-z         EU
#> 336      10.1103/PhysRevD.96.063503         EU
#> 337  10.1140/epjc/s10052-017-5370-z         EU
#> 338   10.1088/1475-7516/2018/12/010         EU
#> 339       10.1007/s00023-017-0637-3         EU
#> 340   10.1088/1475-7516/2018/12/010         EU
#> 341         10.1007/JHEP10(2016)075         EU
#> 342      10.1103/PhysRevD.95.055009         EU
#> 343   10.1016/j.jhydrol.2016.06.037         EU
#> 344         10.1145/3125719.3125734         EU
#> 345            10.1002/2016WR018872         EU
#> 346         10.1007/JHEP10(2016)075         EU
#> 347         10.1007/JHEP09(2017)153         EU
#> 348         10.1007/JHEP03(2019)190         EU
#> 349      10.1103/PhysRevD.96.063503         EU
#> 350      10.1103/PhysRevD.99.015038         EU
#> 351   10.1088/1475-7516/2018/06/007         EU
#> 352         10.1007/JHEP04(2017)001         EU
#> 353      10.1103/PhysRevD.99.015038         EU
#> 354         10.1007/JHEP08(2018)190         EU
#> 355         10.1007/JHEP08(2018)190         EU
#> 356   10.1088/1475-7516/2018/06/007         EU
#> 357   10.1088/1475-7516/2018/08/001         EU
#> 358   10.1088/1475-7516/2018/08/001         EU
#> 359         10.1007/JHEP03(2017)035         EU
#> 360         10.1145/3125719.3125731         EU
#> 361      10.1103/PhysRevD.93.123018         EU
#> 362      10.1103/PhysRevD.97.023002         EU
#> 363      10.1103/PhysRevD.93.123018         EU
#> 364           10.1109/ICCV.2017.443         EU
#> 365      10.1103/PhysRevD.97.023002         EU
#> 366         10.1007/JHEP03(2017)035         EU
#> 367 10.1016/j.nuclphysb.2017.01.017         EU
#> 368   10.1088/1475-7516/2017/01/025         EU
#> 369         10.1007/JHEP09(2016)147         EU
#> 370   10.1088/1475-7516/2017/01/025         EU
#> 371            10.1002/2015WR018348         EU
#> 372         10.1007/JHEP09(2016)147         EU
#> 373   10.1088/1475-7516/2017/01/025         EU
#> 374   10.1088/1475-7516/2017/01/025         EU
#> 375         10.1007/JHEP11(2018)009         EU
#> 376         10.1007/JHEP09(2016)042         EU
#> 377  10.1016/j.physletb.2019.03.002         EU
#> 378  10.1016/j.physletb.2019.03.002         EU
#> 379         10.1007/JHEP11(2017)070         EU
#> 380         10.1007/JHEP09(2016)042         EU
#> 381      10.1103/PhysRevD.97.115034         EU
#> 382     10.1007/978-3-319-53637-8_9         EU
#> 383      10.1103/PhysRevD.95.043512         EU
#> 384      10.1103/PhysRevD.95.043512         EU
#> 385  10.1140/epjc/s10052-019-6815-3         EU
#> 386               10.1063/1.5009075         EU
#> 387         10.1007/JHEP08(2016)090         EU
#> 388  10.1140/epjc/s10052-017-4823-8         EU
#> 389 10.1016/j.nuclphysb.2016.05.028         EU
#> 390                10.3390/f8100396         EU
#> 391            10.1021/jacs.8b03755         EU
#> 392         10.1007/JHEP04(2018)030         EU
#> 393         10.1007/JHEP08(2018)090         EU
#> 394         10.1007/JHEP08(2018)090         EU
#> 395   10.1088/1475-7516/2017/11/020         EU
#> 396         10.1007/JHEP09(2017)061         EU
#> 397   10.1088/1475-7516/2017/11/020         EU
#> 398      10.1103/PhysRevD.98.076010         EU
#> 399   10.1088/1475-7516/2017/10/041         EU
#> 400    10.1007/978-3-319-76581-5_18         EU
#> 401   10.1088/1475-7516/2017/05/051         EU
#> 402              10.1039/c8ta06417b         EU
#> 403   10.1088/1475-7516/2017/05/051         EU
#> 404    10.1007/978-3-319-76581-5_18         EU
#> 405   10.1088/1475-7516/2017/10/042         EU
#> 406         10.1007/JHEP08(2017)019         EU
#> 407         10.1007/JHEP08(2017)019         EU
#> 408   10.1088/1475-7516/2017/10/042         EU
#> 409         10.1007/JHEP10(2018)184         EU
#> 410  10.1016/j.physletb.2017.10.052         EU
#> 411  10.1140/epjc/s10052-018-6396-6         EU
#> 412  10.1140/epjc/s10052-018-6396-6         EU
#> 413         10.1007/JHEP10(2018)184         EU
#> 414  10.1016/j.physletb.2017.10.052         EU
#> 415         10.1007/JHEP02(2019)157         EU
#> 416      10.1103/PhysRevD.94.104069         EU
#> 417   10.1088/1475-7516/2018/04/024         EU
#> 418   10.1088/1475-7516/2018/04/024         EU
#> 419      10.1103/PhysRevD.96.043501         EU
#> 420      10.1103/PhysRevD.96.043501         EU
#> 421    10.1371/journal.pone.0127612         EU
#> 422         10.1007/JHEP08(2018)010         EU
#> 423         10.1007/JHEP09(2017)065         EU
#> 424         10.1007/JHEP08(2018)010         EU
#> 425         10.1007/JHEP09(2017)065         EU
#> 426         10.1007/JHEP09(2017)065         EU
#> 427         10.1007/JHEP08(2018)010         EU
#> 428      10.1103/PhysRevD.94.053002         EU
#> 429      10.1103/PhysRevD.94.053002         EU
#> 430      10.1103/PhysRevD.99.055017         EU
#> 431         10.1007/JHEP12(2016)081         EU
#> 432        10.1177/2158244018816717         EU
#> 433      10.1103/PhysRevD.95.095002         EU
#> 434  10.1016/j.physletb.2017.12.010         EU
#> 435      10.1103/PhysRevD.95.095002         EU
#> 436      10.1103/PhysRevD.99.021301         EU
#> 437         10.1007/JHEP06(2016)051         EU
#> 438  10.1140/epjc/s10052-017-5147-4         EU
#> 439      10.1103/PhysRevD.99.021301         EU
#> 440         10.1007/JHEP06(2016)051         EU
#> 441      10.1103/PhysRevD.99.021301         EU
#> 442  10.1140/epjc/s10052-017-5147-4         EU
#> 443         10.1007/JHEP11(2018)198         EU
#> 444    10.1007/978-3-319-70694-8_22         EU
#> 445          10.1126/sciadv.aat1161         EU
#> 446         10.1007/JHEP01(2018)155         EU
#> 447              10.1039/c8gc01115j         EU
#> 448    10.1007/978-3-319-70694-8_22         EU
#> 449          10.1126/sciadv.aat1161         EU
#> 450      10.1103/PhysRevD.97.103508         EU
#> 451      10.1103/PhysRevE.96.022156         EU
#> 452         10.1007/JHEP10(2018)134         EU
#> 453      10.1103/PhysRevE.96.022156         EU
#> 454         10.1007/JHEP11(2017)136         EU
#> 455   10.1088/1475-7516/2016/11/035         EU
#> 456         10.1007/JHEP11(2017)136         EU
#> 457              10.1093/nar/gky417         EU
#> 458         10.1111/1751-7915.12784         EU
#> 459        10.3847/1538-4357/aa6a18         EU
#> 460  10.1140/epjc/s10052-017-4864-z         EU
#> 461              10.1093/nar/gky417         EU
#> 462  10.1140/epjc/s10052-017-4864-z         EU
#> 463         10.1007/JHEP02(2017)123         EU
#> 464     10.1021/acs.orglett.7b02522         EU
#> 465         10.1007/JHEP06(2017)112         EU
#> 466      10.1103/PhysRevD.96.015003         EU
#> 467         10.1007/JHEP03(2018)022         EU
#> 468         10.1007/JHEP02(2017)123         EU
#> 469  10.1140/epjc/s10052-018-5892-z         EU
#> 470  10.1140/epjc/s10052-016-4384-2         EU
#> 471            10.1002/2017WR020811         EU
#> 472  10.1140/epjc/s10052-018-5892-z         EU
#> 473  10.1140/epjc/s10052-016-4384-2         EU
#> 474  10.1016/j.physletb.2016.10.010         EU
#> 475  10.1140/epjc/s10052-018-6282-2         EU
#> 476  10.1140/epjc/s10052-018-6282-2         EU
#> 477         10.1007/JHEP02(2017)115         EU
#> 478  10.1140/epjc/s10052-018-5772-6         EU
#> 479         10.1007/JHEP02(2017)115         EU
#> 480      10.1103/PhysRevD.95.123534         EU
#> 481      10.1103/PhysRevD.96.073006         EU
#> 482      10.1103/PhysRevD.95.123534         EU
#> 483  10.1140/epjc/s10052-018-5772-6         EU
#> 484         10.1007/JHEP07(2018)037         EU
#> 485         10.1007/JHEP07(2018)037         EU
#> 486      10.1103/PhysRevD.97.103533         EU
#> 487      10.1103/PhysRevD.97.115020         EU
#> 488      10.1103/PhysRevD.97.103533         EU
#> 489         10.1007/JHEP09(2018)074         EU
#> 490      10.1103/PhysRevD.97.115020         EU
#> 491      10.1103/PhysRevD.95.084053         EU
#> 492      10.1103/PhysRevD.97.103533         EU
#> 493  10.1016/j.physletb.2019.01.059         EU
#> 494      10.1103/PhysRevD.95.084053         EU
#> 495      10.1103/PhysRevD.94.065004         EU
#> 496      10.1103/PhysRevD.94.065004         EU
#> 497       10.1186/s12711-017-0345-y         EU
#> 498               10.1111/evj.12747         EU
#> 499         10.1007/JHEP04(2017)164         EU
#> 500      10.1103/PhysRevD.96.043503         EU
#> 501      10.1038/s41598-018-36790-6         EU
#> 502      10.1103/PhysRevD.96.043503         EU
#> 503   10.1088/1475-7516/2017/11/042         EU
#> 504   10.1088/1475-7516/2017/11/042         EU
#> 505         10.1007/JHEP10(2017)148         EU
#> 506         10.1007/JHEP10(2017)148         EU
#> 507      10.1103/PhysRevD.99.063508         EU
#> 508   10.1088/1475-7516/2018/09/040         EU
#> 509   10.1088/1475-7516/2018/09/040         EU
#> 510         10.1007/JHEP08(2017)104         EU
#> 511         10.1007/JHEP08(2017)104         EU
#> 512  10.1140/epjc/s10052-018-5600-z         EU
#> 513         10.1007/JHEP07(2018)101         EU
#> 514         10.1007/JHEP07(2018)101         EU
#> 515   10.1088/1475-7516/2018/04/010         EU
#> 516         10.1007/JHEP07(2018)101         EU
#> 517  10.1140/epjc/s10052-018-6459-8         EU
#> 518  10.1140/epjc/s10052-018-5600-z         EU
#> 519   10.1088/1475-7516/2018/04/010         EU
#> 520  10.1103/PhysRevLett.118.091801         EU
#> 521  10.1140/epjc/s10052-018-6347-2         EU
#> 522   10.1088/1475-7516/2017/04/016         EU
#> 523  10.1140/epjc/s10052-019-6762-z         EU
#> 524  10.1140/epjc/s10052-019-6762-z         EU
#> 525      10.1103/PhysRevD.98.115023         EU
#> 526      10.1103/PhysRevD.98.115023         EU
#> 527  10.1016/j.physletb.2018.07.053         EU
#> 528         10.1007/JHEP08(2016)114         EU
#> 529 10.1016/j.scitotenv.2018.08.419         EU
#> 530 10.1016/j.scitotenv.2018.08.419         EU
#> 531         10.1007/JHEP11(2018)012         EU
#> 532      10.1103/PhysRevD.95.055028         EU
#> 533  10.1140/epjc/s10052-017-5436-y         EU
#> 534      10.1103/PhysRevD.99.043003         EU
#> 535  10.1140/epjc/s10052-017-5436-y         EU
#> 536      10.1103/PhysRevD.98.083501         EU
#> 537      10.1103/PhysRevD.98.083501         EU
#> 538         10.1007/JHEP01(2018)093         EU
#> 539         10.1007/JHEP01(2018)093         EU
#> 540      10.1103/PhysRevD.99.043003         EU
#> 541         10.1007/JHEP01(2018)093         EU
#> 542       10.1142/S0217751X1842006X         EU
#> 543       10.1142/S0217751X1842006X         EU
#> 544         10.1007/JHEP02(2019)109         EU
#> 545  10.1140/epjc/s10052-019-6573-2         EU
#> 546         10.1007/JHEP02(2019)109         EU
#> 547   10.1088/1475-7516/2016/10/036         EU
#> 548      10.1103/PhysRevD.95.055027         EU
#> 549   10.1088/1475-7516/2018/09/017         EU
#> 550   10.1088/1475-7516/2018/09/017         EU
#> 551     10.1007/978-3-319-57339-7_2         EU
#> 552         10.1007/JHEP10(2018)200         EU
#> 553  10.1103/PhysRevLett.117.111803         EU
#> 554     10.1007/978-3-319-57339-7_2         EU
#> 555  10.1103/PhysRevLett.117.111803         EU
#> 556       10.1109/JIOT.2017.2741923         EU
#> 557         10.1007/JHEP10(2017)168         EU
#> 558  10.1140/epjc/s10052-019-6550-9         EU
#> 559  10.1016/j.physletb.2018.08.060         EU
#> 560         10.1007/JHEP08(2018)062         EU
#> 561         10.1007/JHEP03(2019)034         EU
#> 562     10.1016/j.robot.2017.11.012         EU
#> 563  10.1016/j.physletb.2018.08.060         EU
#> 564         10.1007/JHEP10(2017)168         EU
#> 565         10.1007/JHEP03(2019)034         EU
#> 566  10.1140/epjc/s10052-019-6550-9         EU
#> 567         10.1007/JHEP08(2018)062         EU
#> 568  10.1016/j.physletb.2018.08.060         EU
#> 569         10.1007/JHEP03(2019)034         EU
#> 570      10.1103/PhysRevD.96.023502         EU
#> 571      10.1103/PhysRevD.95.075020         EU
#> 572         10.1007/JHEP11(2017)099         EU
#> 573      10.1103/PhysRevD.96.023502         EU
#> 574         10.1007/JHEP11(2017)099         EU
#> 575  10.1140/epjc/s10052-018-5785-1         EU
#> 576  10.1016/j.physletb.2016.10.074         EU
#> 577  10.1140/epjc/s10052-018-5785-1         EU
#> 578         10.1007/JHEP06(2017)148         EU
#> 579  10.1016/j.physletb.2016.10.074         EU
#> 580       10.1017/S0959774318000392         EU
#> 581      10.1103/PhysRevD.95.075035         EU
#> 582            10.1002/2016WR019353         EU
#> 583         10.1007/JHEP02(2018)055         EU
#> 584         10.1007/JHEP02(2018)055         EU
#> 585  10.1088/1742-6596/888/1/012029         EU
#> 586  10.1088/1742-6596/888/1/012029         EU
#> 587        10.3847/1538-4357/aa7261         EU
#> 588  10.1016/j.physletb.2018.10.044         EU
#> 589         10.1007/JHEP08(2018)061         EU
#> 590         10.1007/JHEP03(2017)174         EU
#> 591         10.1007/JHEP11(2017)080         EU
#> 592         10.1007/JHEP03(2017)174         EU
#> 593         10.1007/JHEP08(2018)061         EU
#> 594         10.1007/JHEP11(2017)080         EU
#> 595       10.1186/s12711-018-0385-y         EU
#> 596       10.1142/S0217751X1750018X         EU
#> 597         10.1007/JHEP01(2019)093         EU
#> 598         10.1007/JHEP10(2018)005         EU
#> 599       10.1142/S0217751X1750018X         EU
#> 600         10.1007/JHEP01(2019)093         EU
#> 601         10.1007/JHEP04(2017)038         EU
#> 602      10.1103/PhysRevD.98.023539         EU
#> 603         10.1007/JHEP04(2017)038         EU
#> 604   10.1088/1475-7516/2017/10/008         EU
#> 605   10.1088/1475-7516/2017/10/008         EU
#> 606         10.1007/JHEP01(2019)041         EU
#> 607         10.1007/JHEP04(2017)038         EU
#> 608      10.1103/PhysRevD.98.023539         EU
#> 609      10.1103/PhysRevD.94.116019         EU
#> 610       10.1007/s10596-017-9693-5         EU
#> 611      10.1103/PhysRevD.99.033012         EU
#> 612      10.1103/PhysRevD.94.116019         EU
#> 613      10.1103/PhysRevD.96.023518         EU
#> 614      10.1103/PhysRevD.99.033012         EU
#> 615   10.1088/1475-7516/2018/01/028         EU
#> 616  10.1016/j.physletb.2017.10.027         EU
#> 617    10.1051/epjconf/201713701007         EU
#> 618   10.1088/1475-7516/2018/01/028         EU
#> 619         10.1007/JHEP04(2017)073         EU
#> 620      10.1103/PhysRevD.94.056007         EU
#> 621  10.1140/epjc/s10052-018-5801-5         EU
#> 622  10.1140/epjc/s10052-019-6732-5         EU
#> 623  10.1140/epjc/s10052-018-5801-5         EU
#> 624  10.1140/epjc/s10052-019-6732-5         EU
#> 625      10.1103/PhysRevD.94.056007         EU
#> 626  10.1140/epjc/s10052-018-5801-5         EU
#> 627  10.1016/j.physletb.2018.09.059         EU
#> 628         10.1007/JHEP12(2017)083         EU
#> 629         10.1007/JHEP12(2017)083         EU
#> 630      10.1103/PhysRevD.96.115024         EU
#> 631  10.1016/j.physletb.2018.09.059         EU
#> 632   10.1088/1475-7516/2018/03/011         EU
#> 633   10.1088/1475-7516/2018/03/011         EU
#> 634            10.1029/2018WR022684         EU
#> 635            10.1029/2018WR022684         EU
#> 636      10.1038/s41598-017-01134-3         EU
#> 637  10.1140/epjc/s10052-017-5374-8         EU
#> 638  10.1088/1748-0221/13/07/P07011         EU
#> 639  10.1088/1748-0221/13/07/P07011         EU
#> 640         10.1007/JHEP11(2018)153         EU
#> 641  10.1103/PhysRevLett.120.181802         EU
#> 642  10.1103/PhysRevLett.120.181802         EU
#> 643         10.1007/JHEP06(2018)109         EU
#> 644  10.1103/PhysRevLett.120.181802         EU
#> 645      10.1016/j.ppnp.2018.01.006         EU
#> 646      10.1103/PhysRevD.96.056001         EU
#> 647         10.1007/JHEP06(2018)109         EU
#> 648      10.1103/PhysRevD.96.056001         EU
#> 649         10.3389/fphy.2018.00142         EU
#> 650  10.1140/epjc/s10052-017-4817-6         EU
#> 651         10.3389/fphy.2018.00142         EU
#> 652      10.1016/j.nima.2018.07.013         EU
#> 653         10.1007/JHEP07(2018)026         EU
#> 654  10.1016/j.physletb.2017.11.005         EU
#> 655         10.1007/JHEP03(2018)188         EU
#> 656          10.1126/sciadv.aau1338         EU
#> 657  10.1140/epjc/s10052-019-6608-8         EU
#> 658  10.1140/epjc/s10052-019-6608-8         EU
#> 659        10.1088/1361-6471/aaeba3         EU
#> 660   10.1088/1475-7516/2019/03/012         EU
#> 661      10.1103/PhysRevD.97.043011         EU
#> 662      10.1103/PhysRevD.97.043011         EU
#> 663  10.1140/epjc/s10052-017-5109-x         EU
#> 664         10.1007/JHEP10(2018)067         EU
#> 665  10.1140/epjc/s10052-017-5109-x         EU
#> 666   10.1088/1475-7516/2019/03/012         EU
#> 667      10.1103/PhysRevD.96.115016         EU
#> 668      10.1103/PhysRevD.95.075006         EU
#> 669      10.1103/PhysRevD.95.075006         EU
#> 670         10.1007/JHEP12(2017)063         EU
#> 671              10.1039/c8sc01113c         EU
#> 672      10.1103/PhysRevD.96.115016         EU
#> 673          10.1002/chem.201604407         EU
#> 674 10.1016/j.nuclphysb.2017.10.019         EU
#> 675 10.1016/j.nuclphysb.2017.10.019         EU
#> 676   10.1088/1475-7516/2018/05/006         EU
#> 677      10.1103/PhysRevD.97.075010         EU
#> 678      10.1103/PhysRevD.97.075010         EU
#> 679   10.1088/1475-7516/2018/05/006         EU
#> 680              10.1039/c7ew00474e         EU
#> 681      10.1103/PhysRevD.95.075028         EU
#> 682         10.1007/JHEP06(2018)012         EU
#> 683      10.1103/PhysRevD.95.075028         EU
#> 684       10.1038/s41567-018-0319-1         EU
#> 685         10.1007/JHEP02(2017)051         EU
#> 686         10.1007/JHEP03(2019)071         EU
#> 687  10.1016/j.physletb.2016.11.029         EU
#> 688         10.1007/JHEP02(2017)051         EU
#> 689   10.1088/1475-7516/2019/02/026         EU
#> 690  10.1016/j.physletb.2016.11.029         EU
#> 691       10.1038/s41567-018-0319-1         EU
#> 692         10.1007/JHEP03(2019)071         EU
#> 693         10.1007/JHEP03(2019)071         EU
#> 694         10.1007/JHEP02(2017)051         EU
#> 695  10.1016/j.physletb.2019.02.023         EU
#> 696  10.1016/j.physletb.2016.12.043         EU
#> 697      10.1103/PhysRevD.95.075023         EU
#> 698  10.1016/j.physletb.2016.12.043         EU
#> 699      10.1103/PhysRevD.95.075023         EU
#> 700  10.1016/j.physletb.2019.02.023         EU
#> 701      10.1103/PhysRevD.95.075023         EU
#> 702         10.1007/JHEP05(2018)073         EU
#> 703      10.1103/PhysRevD.96.095014         EU
#> 704         10.1007/JHEP07(2017)033         EU
#> 705       10.1142/S0217751X1730023X         EU
#> 706         10.1007/JHEP12(2016)023         EU
#> 707         10.1007/JHEP07(2017)033         EU
#> 708       10.1142/S0217751X1730023X         EU
#> 709        10.3847/1538-4357/aa9ff5         EU
#> 710         10.1007/JHEP12(2016)023         EU
#> 711        10.3847/1538-4357/aa9ff5         EU
#> 712      10.1103/PhysRevD.96.095014         EU
#> 713         10.1007/JHEP01(2017)077         EU
#> 714         10.1007/JHEP07(2018)076         EU
#> 715         10.1007/JHEP01(2017)077         EU
#> 716  10.1140/epjc/s10052-018-6279-x         EU
#> 717         10.1007/JHEP08(2018)037         EU
#> 718  10.1140/epjc/s10052-018-6279-x         EU
#> 719         10.1007/JHEP08(2018)037         EU
#> 720  10.1140/epjc/s10052-018-6279-x         EU
#> 721  10.1140/epjc/s10052-018-6279-x         EU
#> 722          10.1126/sciadv.aao4641         EU
#> 723 10.1016/j.nuclphysb.2017.08.004         EU
#> 724         10.1007/JHEP08(2018)117         EU
#> 725         10.1007/JHEP06(2018)069         EU
#> 726  10.1016/j.physletb.2017.06.044         EU
#> 727      10.1103/PhysRevD.98.055003         EU
#> 728         10.1007/JHEP08(2018)117         EU
#> 729      10.1103/PhysRevD.98.055003         EU
#> 730  10.1016/j.physletb.2017.06.044         EU
#> 731              10.1093/nar/gky358         EU
#> 732              10.1093/nar/gky358         EU
#> 733         10.1007/JHEP06(2018)069         EU
#> 734      10.1038/s41467-018-03239-3         EU
#> 735         10.1007/JHEP01(2018)004         EU
#> 736   10.1088/1475-7516/2017/12/011         EU
#> 737  10.1016/j.physletb.2017.10.006         EU
#> 738        10.3847/2041-8213/aaa775         EU
#> 739        10.3847/2041-8213/aaa775         EU
#> 740        10.1088/1361-6382/aae9a1         EU
#> 741        10.3847/2041-8213/aaa775         EU
#> 742               10.3382/ps/pey299         EU
#> 743      10.1016/j.ppnp.2017.01.003         EU
#> 744              10.3390/app8071184         EU
#> 745 10.1016/j.nuclphysb.2018.02.018         EU
#> 746 10.1016/j.nuclphysb.2018.02.018         EU
#> 747  10.1140/epjc/s10052-018-6058-8         EU
#> 748       10.1007/s11040-017-9258-9         EU
#> 749   10.1088/1475-7516/2018/10/022         EU
#> 750      10.1103/PhysRevD.99.055019         EU
#> 751      10.1103/PhysRevD.99.055019         EU
#> 752      10.1103/PhysRevD.96.063009         EU
#> 753   10.1088/1475-7516/2018/03/008         EU
#> 754               10.3390/su9030390         EU
#> 755      10.1103/PhysRevD.96.063009         EU
#> 756  10.1140/epjc/s10052-018-6204-3         EU
#> 757      10.1103/PhysRevD.98.015025         EU
#> 758      10.1103/PhysRevD.98.015025         EU
#> 759  10.1140/epjc/s10052-018-6204-3         EU
#> 760         10.1007/JHEP09(2018)069         EU
#> 761         10.1007/JHEP09(2018)069         EU
#> 762         10.3389/fphy.2018.00010         EU
#> 763         10.3389/fphy.2018.00010         EU
#> 764      10.1103/PhysRevD.96.035035         EU
#> 765      10.1103/PhysRevD.96.071701         EU
#> 766      10.1103/PhysRevD.97.115045         EU
#> 767      10.1103/PhysRevD.97.115045         EU
#> 768      10.1103/PhysRevD.96.035035         EU
#> 769         10.1007/JHEP11(2016)081         EU
#> 770      10.1103/PhysRevD.98.115030         EU
#> 771         10.1007/JHEP11(2016)081         EU
#> 772      10.1103/PhysRevD.98.115030         EU
#> 773   10.1088/1475-7516/2018/07/047         EU
#> 774 10.1016/j.nuclphysb.2018.06.022         EU
#> 775         10.1007/JHEP11(2017)004         EU
#> 776   10.1088/1475-7516/2018/07/047         EU
#> 777   10.1088/1475-7516/2018/01/035         EU
#> 778   10.1088/1475-7516/2018/01/035         EU
#> 779   10.1088/1475-7516/2018/02/002         EU
#> 780      10.1103/PhysRevD.98.115030         EU
#> 781   10.1088/1475-7516/2018/02/002         EU
#> 782   10.1088/1475-7516/2018/01/035         EU
#> 783   10.1088/1475-7516/2017/07/027         EU
#> 784       10.1007/s12274-017-1610-7         EU
#> 785   10.1088/1475-7516/2017/07/027         EU
#> 786         10.1007/JHEP05(2018)053         EU
#> 787 10.1016/j.nuclphysa.2018.11.010         EU
#> 788      10.1103/PhysRevD.96.115010         EU
#> 789      10.1103/PhysRevD.96.115010         EU
#> 790         10.1007/JHEP05(2018)053         EU
#> 791  10.1140/epjc/s10052-018-6158-5         EU
#> 792  10.1140/epjc/s10052-018-6158-5         EU
#> 793      10.1103/PhysRevD.95.115023         EU
#> 794      10.1103/PhysRevD.95.115023         EU
#> 795               10.3382/ps/pey360         EU
#> 796      10.1103/PhysRevD.94.053007         EU
#> 797         10.1007/JHEP07(2018)057         EU
#> 798         10.1007/JHEP07(2018)057         EU
#> 799         10.1007/JHEP05(2018)044         EU
#> 800            10.3390/catal8100464         EU
#> 801   10.1088/1475-7516/2018/12/039         EU
#> 802   10.1088/1475-7516/2018/12/039         EU
#> 803   10.1088/1475-7516/2017/06/008         EU
#> 804      10.1103/PhysRevD.98.033001         EU
#> 805   10.1088/1475-7516/2017/06/008         EU
#> 806      10.1103/PhysRevD.98.033001         EU
#> 807   10.1088/1475-7516/2017/12/039         EU
#> 808   10.1088/1475-7516/2018/12/020         EU
#> 809   10.1021/acssuschemeng.7b02945         EU
#> 810      10.1103/PhysRevD.99.061302         EU
#> 811         10.1007/JHEP11(2018)011         EU
#> 812      10.1103/PhysRevD.94.053010         EU
#> 813      10.1103/PhysRevD.99.061302         EU
#> 814      10.1103/PhysRevD.99.061302         EU
#> 815         10.1007/JHEP11(2018)011         EU
#> 816         10.1007/JHEP12(2018)099         EU
#> 817      10.1103/PhysRevD.99.023505         EU
#> 818              10.1039/c7cc06703h         EU
#> 819      10.1103/PhysRevD.94.053010         EU
#> 820      10.1103/PhysRevD.99.055011         EU
#> 821    10.1016/j.petrol.2017.03.011         EU
#> 822      10.1103/PhysRevD.99.023505         EU
#> 823  10.1140/epjc/s10052-019-6670-2         EU
#> 824   10.1088/1475-7516/2017/08/007         EU
#> 825  10.1016/j.physletb.2018.01.069         EU
#> 826   10.1088/1475-7516/2017/08/007         EU
#> 827   10.1088/1475-7516/2017/08/007         EU
#> 828         10.1007/JHEP11(2018)100         EU
#> 829         10.1007/JHEP01(2018)010         EU
#> 830  10.1016/j.physletb.2016.06.009         EU
#> 831      10.1103/PhysRevD.98.015030         EU
#> 832  10.1016/j.physletb.2016.06.009         EU
#> 833         10.1007/JHEP11(2018)100         EU
#> 834    10.1016/j.egypro.2017.08.093         EU
#> 835    10.1016/j.egypro.2017.08.093         EU
#> 836    10.1016/j.petrol.2016.05.031         EU
#> 837       10.5194/hess-21-6219-2017         EU
#> 838      10.1103/PhysRevD.99.035003         EU
#> 839      10.1103/PhysRevD.99.035003         EU
#> 840        10.1088/1751-8121/aac3b3         EU
#> 841      10.1103/PhysRevD.95.064008         EU
#> 842      10.1103/PhysRevD.96.055042         EU
#> 843      10.1103/PhysRevD.95.064008         EU
#> 844              10.1039/c5qi00267b         EU
#> 845         10.1007/JHEP11(2018)159         EU
#> 846  10.1140/epjc/s10052-018-5804-2         EU
#> 847  10.1016/j.physletb.2018.08.056         EU
#> 848  10.1016/j.physletb.2018.08.056         EU
#> 849  10.1140/epjc/s10052-017-4745-5         EU
#> 850   10.1088/1475-7516/2019/01/049         EU
#> 851  10.1140/epjc/s10052-017-4745-5         EU
#> 852               10.3390/su9020298         EU
#> 853     10.1051/0004-6361/201832652         EU
#> 854     10.1051/0004-6361/201832652         EU
#> 855    10.1007/978-3-319-70694-8_21         EU
#> 856         10.1007/JHEP02(2019)071         EU
#> 857    10.1007/978-3-319-70694-8_21         EU
#> 858         10.1007/JHEP05(2017)032         EU
```
