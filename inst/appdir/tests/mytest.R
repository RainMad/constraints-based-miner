app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$uploadFile(xes_input = "./testfiles/ETM_Configuration1.xes") # <-- This should be the path to the file, relative to the app's tests/ directory
app$setInputs(process_activities = "[]", allowInputNoBinding_ = TRUE)
app$snapshot()
