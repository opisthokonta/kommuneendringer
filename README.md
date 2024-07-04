Kommuneendringer
================

I often find myself working with data that deals with Norwegian
municipalities (kommuner) and counties (fylker), either in individual
level data (a persons residence, for example) or on data on the
municipal level itself. The standard way of working with municipalities
is to use the municipality number, which is a four digit number, where
the two first digits indicate the county number. In the recent years
(2018 - 2024) there has been large reforms of the municipal and county
structure, causing a lot of frustration when working with municipal
data, especially if the data spans several years. I often find myself
just wanting to translate the code for a municipality from one point in
time to another point in time, while taking into account mergers,
splits, name changes, county changes etc. The goal of the
`kommuneendringer` R package is to do just that.

This package is *opinionated*, in the sense that it works in ways that
suits my needs, which might be contrary to official guidelines for how
to deal with changes in the municipality structure. Please take a good
look at the examples and explanations below to get an understanding of
how it works.

## Installation

``` r
install.packages("devtools")
devtools::install_github("opisthokonta/kommuneendringer")
```

## Features and limitatons

The main functionality of this package is the `translate_knr` function.
This can translate the four-digit municipal code between different time
point starting from January 1st 1977. It is updated with all changes up
until January 1st 2024.

It currently only supports translating municipality codes. I will soon
add functionality for translating county codes as well.

Special municipal codes such as those for Svalbard, off-shore
installations , etc, are not supported.

In 2024 some mergers between counties and municipalities from 2020 were
reversed. I hope to add support for taking this into account in the
future.

## Examples

First load the package:

``` r
library(kommuneendringer)
```

### Example 1: Changing county

A simple example is the municipality Halden. It hasn’t merged with any
other municipality recently, but changed county, and therefore its code,
in 2020 (from Østfold to Viken) and back to Østfold again in 2024, when
Viken was split, getting yet a new code.

``` r
# Change code from 0101 to 3001 on January 1st 2020.
translate_knr('0101', from_date = '2019-01-01', to_date = '2020-05-17')
```

    ## [1] "3001"

Lets see what it changed to in 2024:

``` r
translate_knr('3001', from_date = '2020-01-01', to_date = '2024-05-17')
```

    ## [1] "3101"

It is also possible to make longer jumps, say if you have some Halden
data from 2008, and want to merge it with data from 2024:

``` r
translate_knr('0101', from_date = '2008-01-01', to_date = '2024-05-17')
```

    ## [1] "3101"

It is also possible to go backwards in time:

``` r
translate_knr('3101', from_date = '2024-01-01', to_date = '2015-05-17')
```

    ## [1] "0101"

### Example 2: Merged municipalities

An example of municipalities that merged are Søgne, Songdalen and
Kristiansand, which merged in 2020. When municipalities merge, the
`translate_knr` function will, if possible, give you the code for the
merged municipality. You can then perhaps aggregate data from the
historical municipalities as if they are the new municipality.

``` r
# Søgne's code was 1018.
translate_knr('1018', from_date = '2018-01-01', to_date = '2023-05-17')
```

    ## [1] "4204"

``` r
# Songdalen's code was 1017
translate_knr('1017', from_date = '2018-01-01', to_date = '2023-05-17')
```

    ## [1] "4204"

``` r
# Kristiansand's code was 1001
translate_knr('1001', from_date = '2018-01-01', to_date = '2023-05-17')
```

    ## [1] "4204"

Let’s take a look what happens if you try to translate the code
backwards in time:

``` r
translate_knr('4204', from_date = '2023-01-01', to_date = '2015-05-17')
```

    ## Warning in translate_knr_internal(knr = knr[ii], from_date = from_date[ii], :
    ## knr 4204 no unambigious code translation possible from 2023-01-01 to
    ## 2015-05-17.

    ## [1] NA

The function will return NA and a warning, since there is no unique
predecessor municipality. The municipality with code 4204 in 2023 can
correspond to any of the three predecessors (1001, 1017 and 1018) in
2015, and without any other information, we can not know which one is
the correct one.

### Example 3: Split municipality

Tysfjord municipality (code 1850) was in 2020 split in two, and the two
parts merged with other municipalities. There is no unique successor
municipality and it is therefore not possible to translate the code from
before to after January 1st 2020.

``` r
translate_knr('1850', from_date = '2019-01-01', to_date = '2020-05-17')
```

    ## Warning in translate_knr_internal(knr = knr[ii], from_date = from_date[ii], :
    ## knr 1850 no unambigious code translation possible from 2019-01-01 to
    ## 2020-05-17.

    ## [1] NA

## How it works

Data from SSB. The graph model.

New node when:

- Code change
- Name change (including when the municipality get official sami and
  kven names)
- Merger, even when the new municipality keep name and code from
  predecessors.

## Regarding changes in the municipal codes

So dealing with changes in the municipal codes are frustrating for a
number of reasons. Typically, a municipality change code

- When municipalities or counties are merged. The merged municipality
  will usually get a new code, even if it keeps the name of one of its
  predecessors. But there are exceptions to this rule, which is
  documented below.

- When municipalities or counties split. The new municipalities or
  counties will each get a new code.

- When municipalities change county. Since the first two digits of the
  municipal code indicate the county, the code will change
  correspondingly. This also happens when counties merge or split.

The code will not change

- When the municipality changes name. This includes when a municipality
  get an additional official name in one of the minority langues (sami
  or kven).

Code and name changes always takes place on January 1st.

### Exceptional cases

This section is mostly a note to myself, to keep track of those
instances that need particular attention to get right.

Usually when municipalities are merged, they get a new code, but this
has not always happened:

- When 0716 Våle and 0718 Ramnes were merged to become Re in 2002, the
  new municipality kept Våle’s code 0716.
- When 1804 Bodø and 1842 Skjerstad were merged in 2005, the kept the
  name and code for 1804 Bodø.
- When 1141 Finnøy and 1142 Rennesøy were merged with 1103 Stavanger in
  2020, the new municipality kept the name and code for 1103 Stavanger.
- When 5030 Klæbu was merged with 5001 Trondheim in 2020, the new
  municipality kept the name and code for 5001 Trondheim.

#### Municipalities that split

Usually, when there are changes in the municipal structure, it is
because two or more municipalities merge, but there are exceptions to
this, when sometimes municipalities split:

- In 2020, the municipality 1850 Tysfjord was split in two. One part
  merged with 1805 Narvik to become 1806 Narvik, and the other part
  merged with 1849 Hammarøy to become 1875 Hammarøy.
- In 2020, the municipality 5012 Snillfjord was split in 3. One part was
  merged with 3 other muncipalities to become 5059 Orkland. The second
  part was merged with two other municipalities to become 5055 Heim. The
  third part was merged with 5013 Hitra to become 5056 Hitra.
- In 2020, the municipalities 1504 Ålesund and 1534 Haram merged to
  become 1507 Ålesund. In 2024 they reversed the merger, and split into
  1508 Ålesund and 1580 Haram.

#### Municipalities that share names

There are some municipalities that share the same name. Conventionally
the name of the county is added in parenthesis to distinguish them.

- Våler (Innlandet, before 2020: Hedmark), and Våler (Østfold, in
  2020-2023: Viken).
- Herøy (Møre og Romsdal) and Herøy (Nordland).
- Nes (Akershus) Nes (Buskerud). Nes in Buskerud changed name to Nesbyen
  in 2020 since both municipalities then ended up in the same county
  (Viken) when Akershus, Buskerud and Østfold merged in 2020. After
  Viken split in 2024, Nesbyen kept its new name.
- Os (Hedmark) and Os (Hordaland). Os in Hordaland was merged with
  another municipality in 2020 and got a new name.
- Bø (Nordland), Bø (Telemark). Bø in Telemark merged with another
  municipality in 2020 and got a new name.
- Sande (Vestfold) and Sande (Møre og Romsdal). Sande in Vestfold merged
  with another municipality in 2020 and got a new name.

Samisnke navn på karasjon og nesseby, dato..
