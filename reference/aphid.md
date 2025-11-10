# *Brachycaudus schwartzi* whole life cycle development rate across temperatures

A modified data set from Table 1 in Satar and Yokomi (2002) on days of
development for *Brachycaudus schwartzi* across different constant
temperatures and life stages

## Usage

``` r
data(aphid)
```

## Format

### `aphid`

A data frame with 7 rows and 5 columns. The workflow is reproducible and
available in `/data-raw` folder of the [mappestRisk GitHub
repository](https://github.com/EcologyR/mappestRisk), which includes
both the original summarized data set -`satar_data.xlsx`- and the R
script with the dev. days to dev. rate conversion in `prepare_aphid.R`.

- reference:

  "Satar2002" refers to the source paper as cited below in section
  `Source`.

- temperature:

  Temperature treatments (ºC).

- dev_days:

  Development days (i.e., days to fulfill development requirements from
  a life-stage to the following)

- rate_value:

  Rate of Development (1/days), the reciprocal of Development days, see
  `dev_days`

- stage:

  Life stage or instar evaluated. In this case, only data of the whole
  immature stages (i.e., nymphs) were used

## Source

Satar, S. and Yokomi, R. (2002). Effect of temperature and host on
development of *Brachycaudus schwartzi* (Homoptera: Aphididae). Ann.
Entomol. Soc. Am. 95: 597-602.
[doi:10.1603/0013-8746(2002)095\[0597:EOTAHO\]2.0.CO;2](https://doi.org/10.1603/0013-8746%282002%29095%5B0597%3AEOTAHO%5D2.0.CO%3B2)
.

Licence: [CC BY-NC 3.0](https://creativecommons.org/licenses/by-nc/3.0)
(modified material).
