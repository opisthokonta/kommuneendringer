


# Particular cases ----


# Halden: Never merged or split, but changed county (and knr) in 2020 and 2024.

test_that("Halden", {

  expect_true(translate_knr(knr = '0101', from_date = '1977-05-17', to_date = '1978-02-11') == '0101')
  expect_true(translate_knr(knr = '0101', from_date = '1977-05-17', to_date = '2019-12-11') == '0101')

  expect_true(translate_knr(knr = '0101', from_date = '2019-05-17', to_date = '2020-02-11') == '3001')
  expect_true(translate_knr(knr = '0101', from_date = '2019-05-17', to_date = '2024-02-11') == '3101')
  expect_true(translate_knr(knr = '3001', from_date = '2020-05-17', to_date = '2024-02-11') == '3101')

  # Test the logic, without hardcoded knr's.
  halden_knr_0 <- '0101'

  expect_true(translate_knr(knr = halden_knr_0, from_date = '1977-05-17', to_date = '2019-12-11') == halden_knr_0)

  expect_silent(halden_knr_2020 <- translate_knr(knr = halden_knr_0, from_date = '2019-05-17', to_date = '2020-01-11'))
  expect_silent(halden_knr_2024 <- translate_knr(knr = halden_knr_0, from_date = '2019-05-17', to_date = '2024-01-11'))

  expect_true(translate_knr(knr = halden_knr_2020, from_date = '2020-05-17', to_date = '2024-01-11') == halden_knr_2024)

  # Backwards in time.
  expect_true(translate_knr(knr = halden_knr_2024, from_date = '2024-05-17', to_date = '2019-01-11') == halden_knr_0)
  expect_true(translate_knr(knr = halden_knr_2024, from_date = '2024-05-17', to_date = '2020-01-11') == halden_knr_2020)
  expect_true(translate_knr(knr = halden_knr_2020, from_date = '2022-05-17', to_date = '2019-01-11') == halden_knr_0)
  expect_true(translate_knr(knr = halden_knr_0, from_date = '2019-05-17', to_date = '2000-01-11') == halden_knr_0)

})




# Snillfjord (changed county in 2018, split in 2020), nothing happened between 1977 and 2018.

test_that("Snillfjord", {

  expect_true(translate_knr(knr = '1613', from_date = '1979-05-17', to_date = '2017-02-11') == '1613')
  expect_true(translate_knr(knr = '1613', from_date = '2017-05-17', to_date = '2018-02-11') == '5012')

  # Backwards in time
  expect_true(translate_knr(knr = '5012', from_date = '2019-05-17', to_date = '2016-02-11') == '1613')

  expect_true(is.na(translate_knr(knr = '1613', from_date = '1979-05-17', to_date = '2020-02-11', show_warnings = FALSE)))

})


# Tysfjord, split in 3 in 2020.

test_that("Tysfjord", {

  expect_true(translate_knr(knr = '1850', from_date = '2012-05-17', to_date = '2016-02-11') == '1850')

  #across a name change, no code change
  expect_true(translate_knr(knr = '1850', from_date = '2005-05-17', to_date = '2016-02-11') == '1850')

  # backwards in time, no change
  expect_true(translate_knr(knr = '1850', from_date = '2015-05-17', to_date = '2012-02-11') == '1850')

  # backwards in time, across a name change
  expect_true(translate_knr(knr = '1850', from_date = '2015-05-17', to_date = '2010-02-11') == '1850')

  # Across the split in 2020, should give NA.
  expect_true(is.na(translate_knr(knr = '1850', from_date = '1979-05-17', to_date = '2020-02-11', show_warnings = FALSE))) # Should give NA + warning.

})






# 1103 Stavanger, 1141 Finnøy and 1142 Rennesøy, Merged in year 2020, kept 1103 as knr.

test_that("Stavanger", {

  expect_true(translate_knr(knr = '1141', from_date = '2015-05-17', to_date = '2020-02-11') == '1103')
  expect_true(translate_knr(knr = '1141', from_date = '2000-05-17', to_date = '2019-02-11') == '1141')

  expect_true(translate_knr(knr = '1142', from_date = '2015-05-17', to_date = '2020-02-11') == '1103')
  expect_true(translate_knr(knr = '1142', from_date = '2000-05-17', to_date = '2019-02-11') == '1142')

  expect_true(translate_knr(knr = '1103', from_date = '2000-05-17', to_date = '2021-02-11') == '1103')

})




# 1804 Bodø and 1842 Skjerstad, merged in year 2005, kept Bodo's name and code.

test_that("Bodo", {

  expect_true(translate_knr(knr = '1804', from_date = '2000-05-17', to_date = '2005-02-11') == '1804')
  expect_true(translate_knr(knr = '1842', from_date = '2000-05-17', to_date = '2005-02-11') == '1804')
  expect_true(translate_knr(knr = '1842', from_date = '2000-05-17', to_date = '2004-02-11') == '1842')

})



# Trondheim and Klaebu

test_that("Trondheim and Klaebu", {

  expect_true(translate_knr(knr = '1601', from_date = '1977-05-17', to_date = '1998-02-11') == '1601')
  expect_true(translate_knr(knr = '1601', from_date = '1977-05-17', to_date = '2017-12-11') == '1601')

  # County change.
  expect_true(translate_knr(knr = '1601', from_date = '2017-05-17', to_date = '2018-01-11') == '5001')

  trondheim_0 <- translate_knr(knr = '1601', from_date = '1977-05-17', to_date = '2017-12-11')
  trondheim_2018 <- translate_knr(knr = trondheim_0, from_date = '2017-05-17', to_date = '2018-02-11')

  expect_true(trondheim_0 != trondheim_2018)

  # After the merger with 5030 Klaebu in 2020.
  expect_true(translate_knr(knr = trondheim_2018, from_date = '2018-01-01', to_date = '2020-02-11') == trondheim_2018)

  # Klaebu, merged with Trondheim 2020, new municipality kept Trondheims code.

  expect_true(translate_knr(knr = '1662', from_date = '1977-01-01', to_date = '2017-02-11') == '1662')
  expect_true(translate_knr(knr = '1662', from_date = '2017-01-01', to_date = '2018-02-11') == '5030')
  expect_true(translate_knr(knr = '5030', from_date = '2018-01-01', to_date = '2020-02-11') == trondheim_2018)

})




# Re kommune, existed from 2002 - 2019. Merged with tønsberg 2020.


test_that("Re and Tonsberg", {

  expect_true(translate_knr(knr = '0716', from_date = '1977-01-01', to_date = '2001-12-31') == '0716') # Vale
  expect_true(translate_knr(knr = '0718', from_date = '1977-01-01', to_date = '2001-12-31') == '0718') # Ramnes

  expect_true(translate_knr(knr = '0716', from_date = '1977-01-01', to_date = '2002-01-01') == '0716')
  expect_true(translate_knr(knr = '0718', from_date = '1977-01-01', to_date = '2002-01-01') == '0716')

  # Merged with tønsberg
  expect_true(translate_knr(knr = '0716', from_date = '2002-01-01', to_date = '2020-01-01') == '3803')
  expect_true(translate_knr(knr = '0716', from_date = '2000-01-01', to_date = '2020-01-01') == '3803')
  expect_true(translate_knr(knr = '0718', from_date = '2000-01-01', to_date = '2020-01-01') == '3803')


  # 0705 Tønsberg merged with 0721 Sem in 1988, new code 0704
  expect_true(translate_knr(knr = '0705', from_date = '1985-01-01', to_date = '2019-01-01') == '0704')
  expect_true(translate_knr(knr = '0721', from_date = '1985-01-01', to_date = '2019-01-01') == '0704')
  expect_true(translate_knr(knr = '0721', from_date = '1987-01-01', to_date = '1987-01-01') == '0721')

  expect_true(translate_knr(knr = '0721', from_date = '1987-01-01', to_date = '2023-01-01') == '3803')

  # Tøsnberg Backwards in time
  expect_true(is.na(translate_knr(knr = '0704', from_date = '1989-01-01', to_date = '1987-01-01', show_warnings = FALSE)))
  expect_true(is.na(translate_knr(knr = '3803', from_date = '2023-01-01', to_date = '2017-01-01', show_warnings = FALSE)))
  expect_true(is.na(translate_knr(knr = '3803', from_date = '2023-01-01', to_date = '1982-01-01', show_warnings = FALSE)))

  # After the county change in 2024.
  expect_true(translate_knr(knr = '3803', from_date = '2023-01-01', to_date = '2024-01-01') == '3905')
  expect_true(translate_knr(knr = '0704', from_date = '1989-01-01', to_date = '2024-01-01') == '3905')
  expect_true(translate_knr(knr = '0705', from_date = '1987-01-01', to_date = '2024-01-01') == '3905')
  expect_true(translate_knr(knr = '0721', from_date = '1987-01-01', to_date = '2024-01-01') == '3905')

})



# Egersund, never changed name, county or code, never merged.

test_that("Egersund", {

  expect_true(translate_knr(knr = '1101', from_date = '1977-01-01', to_date = '1995-01-01') == '1101')
  expect_true(translate_knr(knr = '1101', from_date = '1996-01-01', to_date = '2001-01-01') == '1101')
  expect_true(translate_knr(knr = '1101', from_date = '2006-01-01', to_date = '2019-01-01') == '1101')
  expect_true(translate_knr(knr = '1101', from_date = '2018-01-01', to_date = '2024-03-01') == '1101')
  expect_true(translate_knr(knr = '1101', from_date = '1977-01-01', to_date = '2024-01-01') == '1101')
  expect_true(translate_knr(knr = '1101', from_date = '2005-01-01', to_date = '1977-01-01') == '1101')
  expect_true(translate_knr(knr = '1101', from_date = '2024-01-01', to_date = '1977-01-01') == '1101')

})


# Check changes against kommuneinndelinger ----


years <- unique(kommuneinndelinger$year)

kommuneinndelinger_split <- split(kommuneinndelinger, f = kommuneinndelinger$year)


# Check that there are no change within versions.

no_change <- logical(length(years)-1)

for (ii in 1:(length(years)-1)){

  n_municip <- length(kommuneinndelinger_split[[ii]]$code)

  start_date <- paste0(years[ii], '-01-01')
  end_date <- paste0(as.numeric(years[ii+1])-1, '-12-31')

  translated_codes <- translate_knr(kommuneinndelinger_split[[ii]]$code,
                      # from_date = rep(start_date, n_municip),
                      # to_date = rep(end_date, n_municip))
                      from_date = start_date,
                      to_date = end_date)

  # Check that no change has happened
  no_change[ii] <- all(translated_codes == kommuneinndelinger_split[[ii]]$code)
}


test_that("No change within version", {
  expect_true(all(no_change))
})




# Check changes between versions.


translated_codes_in_next <- logical(length(years)-1)
codes_in_translated <- logical(length(years)-1)
translation_length_ok <- logical(length(years)-1)

for (ii in 1:(length(years)-1)){

  n_municip <- length(kommuneinndelinger_split[[ii]]$code)

  start_date <- paste0(years[ii], '-01-01')
  end_date <- paste0(years[ii+1], '-01-01')

  translated_codes <- translate_knr(kommuneinndelinger_split[[ii]]$code,
                                    #from_date = rep(start_date, n_municip),
                                    #to_date = rep(end_date, n_municip),
                                    from_date = start_date,
                                    to_date = end_date,
                                    show_warnings = FALSE)

  # Look only at unique codes, as merged municipalities will be present several times.
  translated_codes_unique <- unique(translated_codes)

  # Na omit since split municipalities returns NA. Should not occur before 2020.
  translated_codes_unique_na_omit <- na.omit(unique(translated_codes))



  # NA omit in 2019 & 2023, since the splits occured in 2020 and 2024.
  if (years[ii] %in% c('2019', '2023')){

    # Check that all translated codes are in the new kommuneinndeling
    translated_codes_in_next[ii] <- all(translated_codes_unique_na_omit %in% kommuneinndelinger_split[[ii+1]]$code)

    # Check that all codes in the new kommuneinndeling are in the translated codes

    if (years[ii] == '2023'){

      # This is not needed for 2019, because the sucessor municipalities of the split are represented
      # because they are merged with other municipalities.

      codes_to_not_check <- c('1508', '1580') # Do not check the new split Aalesund & Haram
      codes_to_check <- kommuneinndelinger_split[[ii+1]]$code
      codes_to_check <- codes_to_check[!codes_to_check %in% codes_to_not_check]

      codes_in_translated[ii] <- all((codes_to_check %in% translated_codes_unique_na_omit))

    } else {
      codes_in_translated[ii] <- all((kommuneinndelinger_split[[ii+1]]$code %in% translated_codes_unique_na_omit))
    }

    # Should NOT be of equal length, when splits occur.
    translation_length_ok[ii] <- length(translated_codes_unique) != nrow(kommuneinndelinger_split[[ii+1]])

  } else {

    # Check that all translated codes are in the new kommuneinndeling
    translated_codes_in_next[ii] <- all(translated_codes_unique %in% kommuneinndelinger_split[[ii+1]]$code)

    # Check that all codes in the new kommuneinndeling are in the translated codes
    codes_in_translated[ii] <- all((kommuneinndelinger_split[[ii+1]]$code %in% translated_codes_unique))

    translation_length_ok[ii] <- length(translated_codes_unique) == nrow(kommuneinndelinger_split[[ii+1]])
  }


}




test_that("Expected changes between versions", {
  expect_true(all(translated_codes_in_next))
  expect_true(all(codes_in_translated))
  expect_true(all(translation_length_ok))
})






#
# # Should give errors
# translate_knr(knr = 'asd', from_date = '2017-05-17', to_date = '2018-02-11')
#
# translate_knr(knr = '1613', from_date = '1975-05-17', to_date = '2018-02-11')



