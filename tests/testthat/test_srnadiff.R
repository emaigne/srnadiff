exp <- srnadiffExample()

test_that("Testing regions method", {
    expect_error(expP <- srnadiff(exp), NA)
    expect_equal(length(regions(expP)), 26)
})

test_that("Running with different strategies", {
    expP <- srnadiff(exp, segMethod=c("annotation", "hmm"))
    expect_equal(length(regions(expP)), 176)
})

test_that("Running with different sizes", {
    expP <- srnadiff(exp, useParameters=list(minSize=15, maxSize=30))
    expect_equal(length(regions(expP)), 26)
})

test_that("Running with different minimum depth", {
    expP <- srnadiff(exp, useParameters=list(minDepth=1))
    expect_equal(length(regions(expP)), 26)
})

test_that("Running with different transition probabilities", {
    expP <- srnadiff(exp, segMethod="hmm",
                     useParameters=list(noDiffToDiff=0.5,diffToNoDiff=0.5))
    expect_equal(length(regions(expP)), 16)
})

test_that("Running with different emission probabilities", {
    expP <- srnadiff(exp, segMethod="hmm", useParameters=list(emission=0.75))
    expect_equal(length(regions(expP)), 16)
})

test_that("Running with different emission threshold", {
    expP <- srnadiff(exp, segMethod="hmm",
                     useParameters=list(emissionThreshold=0.5))
    expect_equal(length(regions(expP)), 16)
})

test_that("Running with different number of overlapping base pairs", {
    expP <- srnadiff(exp, segMethod="hmm", useParameters=list(minOverlap=15))
    expect_equal(length(regions(expP)), 16)
})

test_that("Running several threads", {
    expP <- srnadiff(exp)
    expP2 <- srnadiffExample()
    expP2 <- srnadiff(expP2, nThreads=2)
    expect_equal(regions(expP), regions(expP2))
})

test_that("Running main function", {
    expP <- srnadiff(exp)
    expect_equal(length(regions(expP)), 26)
})

test_that("Running redundant regions removal", {
    myregions  <- GRanges(seqnames = "14", strand = "+",
                        ranges = IRanges(start = c(60000000, 60000100),
                        width = 10))
    expect_error(myregions2 <- removeRedundant(myregions), NA)
    expect_equal(length(myregions2), 2)
})

