app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$setInputs(z = "Response")
app$snapshot()
