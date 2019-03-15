context("filr creates reliable objects, or fails")

test_that("filr fails on unnamed paths", {
    expect_error(filr("DESCRIPTION"), "named")
    expect_error(filr(description = "DESCRIPTION", "NAMESPACE"), "named")
    expect_is(filr(description = "DESCRIPTION"), "filr")
    expect_is(filr(description = "DESCRIPTION",
                   namespace = "NAMESPACE"), "filr")
})

