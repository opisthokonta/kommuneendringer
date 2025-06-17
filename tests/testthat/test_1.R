


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

  # Should not be able to translate backwards in time
  expect_true(is.na(translate_knr(knr = '1103', from_date = '2020-05-17', to_date = '2019-02-11', show_warnings = FALSE)))


})


# 1804 Bodø and 1842 Skjerstad, merged in year 2005, kept Bodo's name and code.

test_that("Bodo", {

  expect_true(translate_knr(knr = '1804', from_date = '2000-05-17', to_date = '2005-02-11') == '1804')
  expect_true(translate_knr(knr = '1842', from_date = '2000-05-17', to_date = '2005-02-11') == '1804')
  expect_true(translate_knr(knr = '1842', from_date = '2000-05-17', to_date = '2004-02-11') == '1842')

  # Should not be able to translate backwards in time
  expect_true(is.na(translate_knr(knr = '1804', from_date = '2005-05-17', to_date = '2004-02-11', show_warnings = FALSE)))

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


# Trondheim merged with 5030 Kleabu in 2020, but Trondheim did not change code (5001).

test_that("Trondheim", {

  expect_true(translate_knr(knr = '5001', from_date = '2019-01-01', to_date = '2020-05-01') == '5001')
  expect_true(translate_knr(knr = '5030', from_date = '2019-01-01', to_date = '2020-05-01') == '5001')

  # Should not be able to translate back in time.
  expect_true(is.na(translate_knr(knr = '5001', from_date = '2020-05-01', to_date = '2019-05-01', show_warnings = FALSE)))

})



test_that("Ringsaker", {

  expect_true(is.na(translate_knr(knr = '0412', from_date = '1990-01-01', to_date = '1995-05-01', show_warnings = FALSE))) # NA because of split.
  expect_true(translate_knr(knr = '0412', from_date = '1990-01-01', to_date = '1991-05-01') == '0412') # no change here.

})

test_that("Hamar", {

  # Hamar before and after the merger with Vang + part of Ringsaker.
  expect_true(translate_knr(knr = '0401', from_date = '1991-01-01', to_date = '1992-05-01') == '0403')

  # Vang -> Hamar merger.
  expect_true(translate_knr(knr = '0414', from_date = '1991-01-01', to_date = '1992-05-01') == '0403')

})


test_that("Stokke", {

  expect_true(is.na(translate_knr(knr = '0720', from_date = '2016-01-01', to_date = '2017-05-01', show_warnings = FALSE)))

})


test_that("Sandefjord", {

  expect_true(translate_knr(knr = '0706', from_date = '2016-01-01', to_date = '2017-05-01') == '0710')

  # Andebu -> Sandefjord merger
  expect_true(translate_knr(knr = '0719', from_date = '2016-01-01', to_date = '2017-05-01') == '0710')

})


test_that("Sande", {

  expect_true(translate_knr(knr = '1514', from_date = '2001-01-01', to_date = '2002-05-01') == '1514')
  expect_true(translate_knr(knr = '1514', from_date = '2000-01-01', to_date = '2001-05-01') == '1514')

})


test_that("Vanylven", {

  expect_true(translate_knr(knr = '1511', from_date = '2001-01-01', to_date = '2002-05-01') == '1511')
  expect_true(translate_knr(knr = '1511', from_date = '2000-01-01', to_date = '2001-05-01') == '1511')

})


test_that("Varteig", {

  #expect_true(is.na(translate_knr(knr = '0114', from_date = '1991-01-01', to_date = '1992-05-01', show_warnings = FALSE)))
  expect_true(translate_knr(knr = '0114', from_date = '1991-01-01', to_date = '1992-05-01') == '0105')
  expect_true(translate_knr(knr = '0114', from_date = '1990-01-01', to_date = '1990-05-01') == '0114')

})



test_that("Rakkestad", {
  expect_true(translate_knr(knr = '0128', from_date = '1971-01-01', to_date = '1990-05-01') == '0128')
  expect_true(translate_knr(knr = '0128', from_date = '1990-01-01', to_date = '1990-05-01') == '0128')
  expect_true(translate_knr(knr = '0128', from_date = '1991-01-01', to_date = '1992-05-01') == '0128')
  expect_true(translate_knr(knr = '0128', from_date = '1992-01-01', to_date = '1993-05-01') == '0128')

})



# Tolga-Os split in 1976.
test_that("Tolga-Os", {
  expect_true(is.na(translate_knr(knr = '0435', from_date = '1975-01-01', to_date = '1976-05-17', show_warnings = FALSE)))

  # Backwards in time
  expect_true(translate_knr(knr = '0436', from_date = '1976-01-01', to_date = '1975-05-17', show_warnings = FALSE) == '0435')
  expect_true(translate_knr(knr = '0441', from_date = '1976-01-01', to_date = '1975-05-17', show_warnings = FALSE) == '0435')


})

# Moskenes split in 1976.
test_that("Moskenes", {
  expect_true(is.na(translate_knr(knr = '1858', from_date = '1975-01-01', to_date = '1976-05-17', show_warnings = FALSE)))

  # backwards in time
  expect_true(translate_knr(knr = '1859', from_date = '1976-01-01', to_date = '1975-05-17', show_warnings = FALSE) == '1858')
  expect_true(translate_knr(knr = '1874', from_date = '1976-01-01', to_date = '1975-05-17', show_warnings = FALSE) == '1858')

})


# Fron split in 1977.
test_that("Fron", {
  expect_true(is.na(translate_knr(knr = '1518', from_date = '1976-01-01', to_date = '1977-05-17', show_warnings = FALSE)))
})

# Ringerike split in 1977.
test_that("Ringerike", {
  expect_true(is.na(translate_knr(knr = '0601', from_date = '1976-01-01', to_date = '1977-05-17', show_warnings = FALSE)))
})

# 1230 Ullensvang	 split in 1977.
test_that("Ullensvang", {
  expect_true(is.na(translate_knr(knr = '1230', from_date = '1976-01-01', to_date = '1977-05-17', show_warnings = FALSE)))
})


# 1448 Stryn split in 1977.
test_that("Stryn", {
  expect_true(is.na(translate_knr(knr = '1448', from_date = '1976-01-01', to_date = '1977-05-17', show_warnings = FALSE)))
})


# Ålesund split in 1977, and in 2024
test_that("Ålesund", {
  # The 1977 split
  expect_true(is.na(translate_knr(knr = '1501', from_date = '1976-01-01', to_date = '1977-05-17', show_warnings = FALSE)))

  # the 2024 split
  expect_true(is.na(translate_knr(knr = '1507', from_date = '2023-01-01', to_date = '2024-05-17', show_warnings = FALSE)))

  # the 2020 merger
  expect_true(translate_knr(knr = '1504', from_date = '2018-01-01', to_date = '2020-05-17') == '1507')
  expect_true(translate_knr(knr = '1523', from_date = '2018-01-01', to_date = '2020-05-17') == '1507')
  expect_true(translate_knr(knr = '1529', from_date = '2018-01-01', to_date = '2020-05-17') == '1507')
  expect_true(translate_knr(knr = '1534', from_date = '2018-01-01', to_date = '2020-05-17') == '1507')
  expect_true(translate_knr(knr = '1546', from_date = '2018-01-01', to_date = '2020-05-17') == '1507')


  # Check that the allow_reversals work when there is a split it can jump.
  expect_true(translate_knr(knr = '1504', from_date = '2019-05-17', to_date = '2024-12-11', allow_reversals = TRUE) == '1508')
  expect_true(translate_knr(knr = '1523', from_date = '2019-05-17', to_date = '2024-12-11', allow_reversals = TRUE) == '1508')
  expect_true(translate_knr(knr = '1529', from_date = '2019-05-17', to_date = '2024-12-11', allow_reversals = TRUE) == '1508')
  expect_true(translate_knr(knr = '1546', from_date = '2019-05-17', to_date = '2024-12-11', allow_reversals = TRUE) == '1508')

  # Check that it wont work if allow_reversals = FALSE
  expect_true(is.na(translate_knr(knr = '1504', from_date = '2019-05-17', to_date = '2024-12-11', allow_reversals = FALSE, show_warnings = FALSE)))
  expect_true(is.na(translate_knr(knr = '1523', from_date = '2019-05-17', to_date = '2024-12-11', allow_reversals = FALSE, show_warnings = FALSE)))
  expect_true(is.na(translate_knr(knr = '1529', from_date = '2019-05-17', to_date = '2024-12-11', allow_reversals = FALSE, show_warnings = FALSE)))
  expect_true(is.na(translate_knr(knr = '1546', from_date = '2019-05-17', to_date = '2024-12-11', allow_reversals = FALSE, show_warnings = FALSE)))

  # Check that allow_reversals work also when there is not a split.
  expect_true(translate_knr(knr = '1546', from_date = '2019-05-17', to_date = '2023-12-11', allow_reversals = TRUE) == '1507')
  expect_true(translate_knr(knr = '1529', from_date = '2019-05-17', to_date = '2023-12-11', allow_reversals = TRUE) == '1507')
  expect_true(translate_knr(knr = '1504', from_date = '2019-05-17', to_date = '2023-12-11', allow_reversals = TRUE) == '1507')
  expect_true(translate_knr(knr = '1523', from_date = '2019-05-17', to_date = '2023-12-11', allow_reversals = TRUE) == '1507')

  # Check that there will be no fuss with allow_reversals = FALSE.
  expect_true(translate_knr(knr = '1546', from_date = '2019-05-17', to_date = '2023-12-11', allow_reversals = FALSE) == '1507')
  expect_true(translate_knr(knr = '1529', from_date = '2019-05-17', to_date = '2023-12-11', allow_reversals = FALSE) == '1507')
  expect_true(translate_knr(knr = '1504', from_date = '2019-05-17', to_date = '2023-12-11', allow_reversals = FALSE) == '1507')
  expect_true(translate_knr(knr = '1523', from_date = '2019-05-17', to_date = '2023-12-11', allow_reversals = FALSE) == '1507')






})



# Haram - Merged with Aalesund in 2020, split away in 2024.

test_that("Haram", {

  # Before 2020 merger
  expect_true(translate_knr(knr = '1534', from_date = '2011-01-01', to_date = '2019-05-17') == '1534')

  # The merger
  expect_true(translate_knr(knr = '1534', from_date = '2018-01-01', to_date = '2020-05-17') == '1507')

  # Across the merger and split. Default is NA.
  expect_true(is.na(translate_knr(knr = '1534', from_date = '2018-01-01', to_date = '2024-05-17', show_warnings = FALSE)))

  # allow_reversal across the split.
  expect_true(translate_knr(knr = '1534', from_date = '2018-01-01', to_date = '2024-05-17', allow_reversals = TRUE) == '1580')

  # Allow_reversal backwards in time
  expect_true(translate_knr(knr = '1580', from_date = '2024-05-17', to_date = '2020-10-11', allow_reversals = FALSE)  == '1507')
  expect_true(is.na(translate_knr(knr = '1580', from_date = '2024-05-17', to_date = '2019-10-11', allow_reversals = FALSE, show_warnings = FALSE)))
  expect_true(translate_knr(knr = '1580', from_date = '2024-05-17', to_date = '2019-10-11', allow_reversals = TRUE) == '1534')

  expect_true(translate_knr(knr = '1508', from_date = '2024-05-17', to_date = '2020-10-11', allow_reversals = FALSE)  == '1507')
  expect_true(is.na(translate_knr(knr = '1508', from_date = '2024-05-17', to_date = '2019-10-11', allow_reversals = FALSE, show_warnings = FALSE)))

})




# 1527 Ørskog split in 1977.
test_that("Ørskog", {
  expect_true(is.na(translate_knr(knr = '1527', from_date = '1976-01-01', to_date = '1977-05-17', show_warnings = FALSE)))
})

# 1814 Brønnøy split in 1977.
test_that("Brønnøy", {
  expect_true(is.na(translate_knr(knr = '1814', from_date = '1976-01-01', to_date = '1977-05-17', show_warnings = FALSE)))
})

# 1921 Salangen split in 1977.
test_that("Salangen", {
  expect_true(is.na(translate_knr(knr = '1921', from_date = '1976-01-01', to_date = '1977-05-17', show_warnings = FALSE)))
})


# Duplicated input ----

nrep <- 4

test_that("Duplicted", {

  # Use Halden as example.
  expect_true(all(translate_knr(knr = rep('0101', nrep), from_date = '1977-05-17', to_date = '1978-02-11') == rep('0101', nrep)))
  expect_true(all(translate_knr(knr = '0101', from_date = '1977-05-17', to_date = '2019-12-11') == '0101'))

  expect_true(all(translate_knr(knr = rep('0101', nrep), from_date = '2019-05-17', to_date = '2020-02-11') == rep('3001', nrep)))
  expect_true(all(translate_knr(knr = rep('0101', nrep), from_date = '2019-05-17', to_date = '2024-02-11') == rep('3101', nrep)))
  expect_true(all(translate_knr(knr = rep('3001', nrep), from_date = '2020-05-17', to_date = '2024-02-11') == rep('3101', nrep)))


  # Sogne + Halden
  expect_true(all(translate_knr(knr = c(rep('0101', nrep), rep('1018', nrep)), from_date = '1977-05-17', to_date = '2020-02-11') == c(rep('3001', nrep), rep('4204', nrep))))

  # Kristiansand backwards in time, should give NA
  krs_backwards <- translate_knr(knr = rep('4204', nrep), from_date = '2020-05-17', to_date = '2019-02-11', show_warnings = FALSE)

  expect_true(all(is.na(krs_backwards)))
  expect_true(length(krs_backwards) == nrep)

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
                                    from_date = start_date,
                                    to_date = end_date,
                                    show_warnings = FALSE)

  # Look only at unique codes, as merged municipalities will be present several times.
  translated_codes_unique <- unique(translated_codes)

  # Na omit since split municipalities returns NA.
  translated_codes_unique_na_omit <- na.omit(unique(translated_codes))


  # NA omit in 2019 & 2023, since the splits occurred in 2020 and 2024.
  if (years[ii] %in% c('1974', '1976' ,'1991', '2015', '2019', '2023')){

    # Check that all translated codes are in the new kommuneinndeling
    translated_codes_in_next[ii] <- all(translated_codes_unique_na_omit %in% kommuneinndelinger_split[[ii+1]]$code)

    # Check that all codes in the new kommuneinndeling are in the translated codes

    if (years[ii] == '1974'){

      # Do not check 0435 Tolga-Os -> 0436, 0441, as it was split.
      # Do not check 1858 Moskenes -> 1859, 1874, as it was split.
      codes_to_not_check <- c('0436', '0441', '1859', '1874')

      codes_to_check <- kommuneinndelinger_split[[ii+1]]$code
      codes_to_check <- codes_to_check[!codes_to_check %in% codes_to_not_check]

      codes_in_translated[ii] <- all((codes_to_check %in% translated_codes_unique_na_omit))

      # Should be of equal length, with na omit, in this case.
      translation_length_ok[ii] <- length(translated_codes_unique_na_omit) != nrow(kommuneinndelinger_split[[ii+1]])

    } else if (years[ii] == '1976'){

      # Many splits in this version
      codes_to_not_check <- c('0516', '0519', # 0518 Fron
                              '0605', '0612', # 0601 Ringerike
                              '1231', '1232', # 1230 Ullensvang
                              '1444', '1449', # 1448 Stryn
                              '1504', '1531', # 1501 Ålesund
                              '1523', '1526', '1529', # 1527 Ørskog
                              '1812', '1813', # 1814 Brønnøy
                              '1920', '1923' # 1921 Salangen
                              )

      codes_to_check <- kommuneinndelinger_split[[ii+1]]$code
      codes_to_check <- codes_to_check[!codes_to_check %in% codes_to_not_check]

      codes_in_translated[ii] <- all((codes_to_check %in% translated_codes_unique_na_omit))

      # Should be of equal length, with na omit, in this case.
      translation_length_ok[ii] <- length(translated_codes_unique_na_omit) != nrow(kommuneinndelinger_split[[ii+1]])


    }else if (years[ii] == '1991'){

      codes_to_not_check <- c('0412') # Do not check 0412 Ringsaker, as it was split.
      codes_to_check <- kommuneinndelinger_split[[ii+1]]$code
      codes_to_check <- codes_to_check[!codes_to_check %in% codes_to_not_check]

      codes_in_translated[ii] <- all((codes_to_check %in% translated_codes_unique_na_omit))

      # Should be of equal length, with na omit, in this case.
      translation_length_ok[ii] <- length(translated_codes_unique_na_omit) != nrow(kommuneinndelinger_split[[ii+1]])

    } else if (years[ii] == '2015'){

      codes_to_not_check <- c('0720') # Do not check 0720 Stokke, as it was split.
      codes_to_check <- kommuneinndelinger_split[[ii+1]]$code
      codes_to_check <- codes_to_check[!codes_to_check %in% codes_to_not_check]

      codes_in_translated[ii] <- all((codes_to_check %in% translated_codes_unique_na_omit))

      # Should NOT be of equal length, when splits occur.
      translation_length_ok[ii] <- length(translated_codes_unique) != nrow(kommuneinndelinger_split[[ii+1]])

    } else if (years[ii] == '2023'){

      # This is not needed for 2019, because the successor municipalities of the split are represented
      # because they are merged with other municipalities.

      codes_to_not_check <- c('1508', '1580') # Do not check the new split Aalesund & Haram
      codes_to_check <- kommuneinndelinger_split[[ii+1]]$code
      codes_to_check <- codes_to_check[!codes_to_check %in% codes_to_not_check]

      codes_in_translated[ii] <- all((codes_to_check %in% translated_codes_unique_na_omit))

      # Should NOT be of equal length, when splits occur.
      translation_length_ok[ii] <- length(translated_codes_unique) != nrow(kommuneinndelinger_split[[ii+1]])

    } else {
      codes_in_translated[ii] <- all((kommuneinndelinger_split[[ii+1]]$code %in% translated_codes_unique_na_omit))


      # Should NOT be of equal length, when splits occur.
      translation_length_ok[ii] <- length(translated_codes_unique) != nrow(kommuneinndelinger_split[[ii+1]])
    }


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






# # Should give errors
# translate_knr(knr = 'asd', from_date = '2017-05-17', to_date = '2018-02-11')
#
# translate_knr(knr = '1613', from_date = '1975-05-17', to_date = '2018-02-11')



