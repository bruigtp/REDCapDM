# Fixtures mimicking REDCap exports with repeated instruments and repeating events.

# Classic project: 'demo' is a regular form and 'ae' a repeating instrument.
# Record 1 has age and two adverse events (the second one without term).
# Record 2 has no age and one adverse event.
make_repeat_classic <- function() {
  data <- data.frame(
    record_id = c("1", "1", "1", "2", "2"),
    redcap_repeat_instrument = c(NA, "ae", "ae", NA, "ae"),
    redcap_repeat_instance = c(NA, 1, 2, NA, 1),
    age = c(50, NA, NA, NA, NA),
    ae_term = c(NA, "Headache", NA, NA, "Nausea"),
    ae_grade = c(NA, 1, 2, NA, 3),
    stringsAsFactors = FALSE
  )
  data$redcap_repeat_instrument.factor <- factor(data$redcap_repeat_instrument, levels = "ae", labels = "Adverse events")

  dic <- data.frame(
    field_name = c("record_id", "age", "ae_term", "ae_grade"),
    form_name = c("demo", "demo", "ae", "ae"),
    field_type = "text",
    field_label = c("Record ID", "Age", "AE term", "AE grade"),
    choices_calculations_or_slider_labels = NA,
    text_validation_type_or_show_slider_number = NA,
    branching_logic_show_field_only_if = "",
    stringsAsFactors = FALSE
  )

  list(data = data, dictionary = dic)
}

# Longitudinal project for one record:
# - 'labs' is a regular form at baseline but a repeating instrument at treatment.
# - 'follow_up_arm_1' is a repeating event.
make_repeat_longitudinal <- function() {
  data <- data.frame(
    record_id = "1",
    redcap_event_name = c("baseline_arm_1", "treatment_arm_1", "treatment_arm_1", "treatment_arm_1", "follow_up_arm_1", "follow_up_arm_1"),
    redcap_repeat_instrument = c(NA, NA, "labs", "labs", NA, NA),
    redcap_repeat_instance = c(NA, NA, 1, 2, 1, 2),
    age = c(50, NA, NA, NA, NA, NA),
    hb = c(12, NA, 11, NA, NA, NA),
    sbp = c(NA, 120, NA, NA, NA, NA),
    fu_status = c(NA, NA, NA, NA, "Alive", NA),
    stringsAsFactors = FALSE
  )

  dic <- data.frame(
    field_name = c("record_id", "age", "hb", "sbp", "fu_status"),
    form_name = c("demo", "demo", "labs", "vitals", "followup"),
    field_type = "text",
    field_label = c("Record ID", "Age", "Haemoglobin", "Systolic BP", "Follow-up status"),
    choices_calculations_or_slider_labels = NA,
    text_validation_type_or_show_slider_number = NA,
    branching_logic_show_field_only_if = "",
    stringsAsFactors = FALSE
  )

  event_form <- data.frame(
    arm_num = 1,
    unique_event_name = c("baseline_arm_1", "baseline_arm_1", "treatment_arm_1", "treatment_arm_1", "follow_up_arm_1"),
    form = c("demo", "labs", "vitals", "labs", "followup"),
    stringsAsFactors = FALSE
  )

  list(data = data, dictionary = dic, event_form = event_form)
}


# Longitudinal project with a repeating event whose branching logic uses a variable of another event:
# `preg` (follow-up, repeating event) is only shown to women (`[baseline_arm_1][sex] = '2'`).
# Record 1 (man) has two visits without `preg`; record 2 (woman) has two visits, the second one without `preg`.
make_cross_event <- function() {
  data <- data.frame(
    record_id = c("1", "1", "1", "2", "2", "2"),
    redcap_event_name = c("baseline_arm_1", "follow_up_arm_1", "follow_up_arm_1", "baseline_arm_1", "follow_up_arm_1", "follow_up_arm_1"),
    redcap_repeat_instrument = NA,
    redcap_repeat_instance = c(NA, 1, 2, NA, 1, 2),
    sex = c(1, NA, NA, 2, NA, NA),
    preg = c(NA, NA, NA, NA, 0, NA),
    stringsAsFactors = FALSE
  )
  data$redcap_event_name.factor <- factor(data$redcap_event_name, levels = c("baseline_arm_1", "follow_up_arm_1"), labels = c("Baseline", "Follow-up"))

  dic <- data.frame(
    field_name = c("record_id", "sex", "preg"),
    form_name = c("demo", "demo", "visit"),
    field_type = c("text", "radio", "yesno"),
    field_label = c("Record ID", "Sex", "Pregnant?"),
    choices_calculations_or_slider_labels = c(NA, "1, Man | 2, Woman", NA),
    text_validation_type_or_show_slider_number = NA,
    branching_logic_show_field_only_if = c("", "", "[baseline_arm_1][sex] = '2'"),
    stringsAsFactors = FALSE
  )

  event_form <- data.frame(arm_num = 1, unique_event_name = c("baseline_arm_1", "follow_up_arm_1"), form = c("demo", "visit"), stringsAsFactors = FALSE)

  list(data = data, dictionary = dic, event_form = event_form)
}
