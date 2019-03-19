Trace1 = "Trace 1"
Trace2 = "Trace 2"
Trace3 = "Trace 3"
Trace4 = "Trace 4"
Trace5 = "Trace 5"
Trace6 = "Trace 6"
Trace7 = "Trace 7"
Trace8 = "Trace 8"
Trace9 = "Trace 9"
Trace10 = "Trace 10"
Trace11 = "Trace 11"

eventlog <- bupaR::simple_eventlog(tibble(CASE_concept_name = c(Trace1, Trace1, Trace1, 
                                                                Trace2, Trace2, Trace2,
                                                                Trace3, Trace3,
                                                                Trace4,
                                                                Trace5,
                                                                Trace6,
                                                                Trace7, Trace7, Trace7,
                                                                Trace8, Trace8,
                                                                Trace9, Trace9, Trace9,
                                                                Trace10, Trace10, Trace10, Trace10, Trace10,
                                                                Trace11, Trace11, Trace11, Trace11),
                                          activity_id = c("A", "B", "C", 
                                                          "A", "B", "A",
                                                          "B", "C",
                                                          "A",
                                                          "B",
                                                          "C",
                                                          "A", "A", "C",
                                                          "B", "A",
                                                          "B", "A", "B",
                                                          "A", "A", "C", "D", "B",
                                                          "C", "A", "B", "D"),
                                          timestamp = as.POSIXct("2013-04-16 08:08:01")),
                                   case_id = "CASE_concept_name",
                                   activity_id = "activity_id",
                                   timestamp = "timestamp")


filter_result <- function(result, case){
  return (result %>% filter(CASE_concept_name == case) %>% pull(resp))
}