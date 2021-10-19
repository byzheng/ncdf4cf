test_that("Create and read ncdf4", {
    dim1 <- cfdim_def('dim1', '', seq(1, 5),
                      longname = 'Dimension 1', unlim = TRUE,
                      create_dimvar = TRUE)
    dim2 <- cfdim_def('dim2', '', seq(1, 5),
                      longname = 'Dimension 2', unlim = TRUE,
                      create_dimvar = TRUE)

    var1 <- cfvar_def('var1', list(dim1, dim2),
                      unit = '', prec = 'integer',
                      longname = 'var1')

    filename <- tempfile("test", fileext = ".nc")
    nc <- cf_create(filename = filename, vars = list(var1))
    ncdf4::ncvar_put(nc, 'var1', seq(1, 25))
    cf_close(nc)
    nc <- cf_open(filename)
    new_data <- cfvar_get(nc, "var1")
    expect_equal(as.vector(unlist(new_data)), seq(1, 25))
})
