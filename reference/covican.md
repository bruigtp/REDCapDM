# Subset of COVICAN's Database

A random sample of the COVICAN study. An international, multicentre
cohort study of cancer patients with COVID-19 to describe the
epidemiology, risk factors, and clinical outcomes of co-infections and
superinfections in onco-haematological patients with COVID-19.

## Usage

``` r
data(covican)
```

## Format

A data frame with 342 rows and 56 columns

- record_id::

  Identifier of each record. This information does not match the real
  data.

- redcap_event_name::

  Auto-generated name of the events

- redcap_data_access_group::

  Auto-generated name of each center. This information does not match
  the real data.

- inc_1::

  Inclusion criteria of 'Patients older than 18 years' (0 = No ; 1 =
  Yes)

- inc_2::

  Inclusion criteria of 'Cancer patients' (0 = No ; 1 = Yes)

- inc_3::

  Inclusion criteria of 'Diagnosed of COVID-19' (0 = No ; 1 = Yes)

- exc_1::

  Exclusion criteria of 'Solid tumour remission \>1 year' (0 = No ; 1 =
  Yes)

- screening_fail_crit::

  Indicator of non-compliance with inclusion and exclusion criteria (0 =
  compliance ; 1 = non-compliance)

- d_birth::

  Date of birth (y-m-d). This date does not correspond to the original.

- d_admission::

  Date of first visit (y-m-d). This date does not correspond to the
  original.

- age::

  Age in years

- dm::

  Indicator of diabetes (0 = No ; 1 = Yes)

- type_dm::

  Type of diabetes (1 = No complications ; 2 = End-organ
  diabetes-related disease)

- copd::

  Indicator of chronic obstructive pulmonary disease (0 = No ; 1 = Yes)

- fio2::

  Fraction of inspired oxygen in percentage

- available_analytics::

  Indicator of blood test available (0 = No ; 1 = Yes)

- potassium::

  Potassium in mmol/L

- resp_rate::

  Respiratory rate in bpm

- leuk_lymph::

  Indicator of leukemia or lymphoma (0 = No ; 1 = Yes)

- acute_leuk::

  Indicator of acute leukemia (0 = No ; 1 = Yes)

- type_underlying_disease[...](https://rdrr.io/r/base/dots.html)::

  Checkbox with the type of underlying disease (0 = Haematological
  cancer ; 1 = Solid tumour)

- underlying_disease_hemato[...](https://rdrr.io/r/base/dots.html)::

  Checkbox with the type of underlying disease (1 = Acute myeloid
  leukemia ; 2 = Myelodysplastic syndrome ; 3 = Chronic myeloid
  leukaemia ; 4 = Acute lymphoblastic leukaemia ; 5 = Hodgkin lymphoma ;
  6 = Non Hodgkin lymphoma ; 7 = Multiple myeloma ; 8 = Myelofibrosis ;
  9 = Aplastic anaemia ; 10 = Chronic lymphocytic leukaemia ; 11 =
  Amyloidosis ; 12 = Other)

- urine_culture::

  Indicator of urine culture: (0 = Not done ; 1 = Done)

- [...](https://rdrr.io/r/base/dots.html).factor::

  Labels of the different variables

## Note

List with three data frames: the first one with the data, the second one
with the dictionary (`codebook`) of the REDCap project and the last one
with the instrument-event mappings of the REDCap project.

## References

Gudiol, C., Durà-Miralles, X., Aguilar-Company, J., Hernández-Jiménez,
P., Martínez-Cutillas, M., Fernandez-Avilés, F., Machado, M., Vázquez,
L., Martín-Dávila, P., de Castro, N., Abdala, E., Sorli, L., Andermann,
T. M., Márquez-Gómez, I., Morales, H., Gabilán, F., Ayaz, C. M.,
Kayaaslan, B., Aguilar-Guisado, M., Herrera, F. Royo-Cebrecos C, Peghin
M, González-Rico C, Goikoetxea J, Salgueira S, Silva-Pinto A,
Gutiérrez-Gutiérrez B, Cuellar S, Haidar G, Maluquer C, Marin M,
Pallarès N, Carratalà J. (2021). Co-infections and superinfections
complicating COVID-19 in cancer patients: A multicentre, international
study. The Journal of infection, 83(3), 306–313.
https://doi.org/10.1016/j.jinf.2021.07.014
