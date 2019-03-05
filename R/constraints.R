
responded_existence <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    summarize(
      xcount = sum(activity_id == activity1),
      ycount = sum(activity_id == activity2)
    ) %>%
    mutate(resp = xcount == 0 | ycount > 0) %>%
    select(CASE_concept_name, resp)
}

response <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    mutate(xoccurs = activity1 %in% activity_id) %>%
    filter(activity_id == activity1 |
             activity_id == activity2 | !xoccurs) %>%
    summarize(last_activity = last(activity_id),
              xoccurs = first(xoccurs)) %>%
    mutate(resp = last_activity == activity2 | !xoccurs)  %>%
    select(CASE_concept_name, resp)
}

precedence <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    mutate(yoccurs = activity2 %in% activity_id) %>%
    filter(activity_id == activity1 |
             activity_id == activity2 | !yoccurs) %>%
    summarize(first_activity = first(activity_id),
              yoccurs = first(yoccurs)) %>%
    mutate(resp = first_activity == activity1 | !yoccurs) %>%
    select(CASE_concept_name, resp)
}

chain_response <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    mutate(
      next.activity = lead(activity_id),
      response = activity_id == activity1 &
        (is.na(next.activity) |
           next.activity != activity2)
    ) %>%
    summarize(resp = sum(response) == 0) %>%
    select(CASE_concept_name, resp)
}

chain_precedence <- function(eventlog, activity1, activity2) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    mutate(
      previous.activity = lag(activity_id),
      response = activity_id == activity2 &
        (is.na(previous.activity) |
           previous.activity != activity1)
    ) %>%
    summarize(resp = sum(response) == 0) %>%
    select(CASE_concept_name, resp)
}

init_constraint <- function(eventlog, activity) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    summarize(resp = first(activity_id) == activity) %>%
    select(CASE_concept_name, resp)
}

end_constraint <- function(eventlog, activity) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    summarize(resp = last(activity_id) == activity) %>%
    select(CASE_concept_name, resp)
}

participation <- function(eventlog, activity) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    summarize(resp = activity %in% activity_id) %>%
    select(CASE_concept_name, resp)
}

at_most_once <- function(eventlog, activity) {
  eventlog %>%
    group_by(CASE_concept_name) %>%
    summarize(cnt = sum(activity == activity_id)) %>%
    mutate(resp = cnt <= 1) %>%
    select(CASE_concept_name, resp)
}