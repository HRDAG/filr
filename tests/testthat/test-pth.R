context("pth does necessary checks")

test_that("pth errors on missing dependencies", {
   expect_error(pth("does/not/exist.xyz", is_dep = TRUE))
   expect_error(pth("R/not-a-file.R", is_dep = TRUE))
   expect_is(pth("DESCRIPTION", is_dep = TRUE), "pth")
})

test_that("pth errors on missing directories", {
    expect_error(pth("path/not/exists.xyz", is_dep = FALSE))
    expect_is(pth("R/not-a-file.R", is_dep = FALSE), "pth")
})
