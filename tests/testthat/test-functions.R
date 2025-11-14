# H-top ------------------------------------------------------------------------
test_that("h_top works for known case (t=50, site index III)", {
  expect_equal(calculate_h_top(t=10,
                               S_i=36.1377,
                               c1=0.0278023,
                               c2=3.4406073,
                               c3=0.0556976,
                               c4=1.3871659),
               4.4, tolerance = 0.01)
})
test_that("h_top works for known case (t=50, site index III)", {
  expect_equal(calculate_h_top(t=50,
                               S_i=36.1377,
                               c1=0.0278023,
                               c2=3.4406073,
                               c3=0.0556976,
                               c4=1.3871659),
               24.0, tolerance = 0.01)
})

# D_bt -------------------------------------------------------------------------
test_that("d_bt works for h_top<7", {
  expect_equal(calculate_d_bt(h_top=4.4,
                              N0=5000,
                              c11=3.5821114,
                              c12=0.3395238,
                              c13=4.557235,
                              c14=0.0902477
                              ),
               2.2, tolerance = 0.01)
})
# G ----------------------------------------------------------------------------
test_that("G calculated correctly when N=5000 and d=2.2", {
  expect_equal(calculate_g(N=5000,
                           d=2.2
  ),
  1.9, tolerance = 0.01)
})

