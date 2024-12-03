exp <- srnadiffExample()
exp <- srnadiff(exp)

test_that("Running plot", {
    expect_error(myplot <- plotRegions(exp, regions(exp, 0.05)[1]), NA)
    expect_true(is.list(myplot))
    expect_equal(length(myplot), 4)
    expect_is(myplot[[1]], "AnnotationTrack")
    expect_is(myplot[[2]], "GenomeAxisTrack")
    expect_is(myplot[[3]], "AnnotationTrack")
    expect_is(myplot[[4]], "DataTrack")
})
