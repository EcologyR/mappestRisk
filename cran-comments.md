## R CMD check results

0 errors | 0 warnings | 1 note

❯ checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Darío San-Segundo Molina <dario.ssm2@gmail.com>’
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    Padfield (17:6)
    al (17:18, 22:25)
    et (17:15, 22:22)

The mispelled words are a false positive. We are citing the work of 'Padfield et al' in DESCRIPTION.
  
This revised version fixes indentation issues in DESCRIPTION and returns users' 
former graphical parameters when exiting `map_risk`. 
