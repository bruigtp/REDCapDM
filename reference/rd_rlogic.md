# Translate REDCap Logic to R Logic

**\[stable\]**

Converts a REDCap logic expression into R-compatible logic. Processes
one logic expression (`logic`) for one target variable (`var`) at a
time. Supports common REDCap operators (`and`, `or`, `=`, `<`, `>`,
etc.) and handles event-specific logic in longitudinal projects. Logic
involving smart variables or repeated instruments may require manual
review.

## Usage

``` r
rd_rlogic(
  project = NULL,
  data = NULL,
  dic = NULL,
  event_form = NULL,
  logic,
  var
)
```

## Arguments

- project:

  A list containing the REDCap data, dictionary, and event mapping
  (expected
  [`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md)
  output). Overrides `data`, `dic`, and `event_form`.

- data:

  A `data.frame` or `tibble` with the REDCap dataset.

- dic:

  A `data.frame` with the REDCap dictionary.

- event_form:

  Only applicable for longitudinal projects (presence of events).
  Event-to-form mapping for longitudinal projects.

- logic:

  A single REDCap logic string (e.g.,
  `"if([exc_1]='1' or [inc_1]='0', 1, 0)"`).

- var:

  A single string specifying the target variable the logic applies to.

## Value

A list with:

- rlogic:

  The translated R-compatible logic as a string.

- eval:

  The evaluation of the translated logic on the provided dataset,
  filtered by event if applicable.

- eval_data:

  A `data.frame` with the record identifier, the event and repetition
  columns present in the dataset (`redcap_event_name`,
  `redcap_repeat_instrument`, `redcap_repeat_instance`) and `eval`, to
  identify the record of each evaluated value.

## Details

- Translates REDCap operators and functions into R equivalents:

  - `=` → `==`, `<>` → `!=`, `and` → `&`, `or` → `|`.

  - Converts functions like `if()`, `rounddown()`, `datediff()`,
    [`sum()`](https://rdrr.io/r/base/sum.html) to R equivalents.

- Handles date transformations and empty strings (`''`) → `NA`.

- Adjusts logic for longitudinal data using `event_form` if provided.

- Evaluates the translated R logic against the dataset and returns the
  results.

- Logic with repeated instruments, smart variables, or multiple events
  per variable may require manual inspection.

## Examples

``` r
# Translate a single REDCap logic expression for one variable
covican |>
  rd_rlogic(
    logic = "if([exc_1]='1' or [inc_1]='0' or [inc_2]='0' or [inc_3]='0', 1, 0)",
    var = "screening_fail_crit"
  )
#> $rlogic
#> [1] "ifelse(data$exc_1=='1' | data$inc_1=='0' | data$inc_2=='0' | data$inc_3=='0', 1, 0)"
#> 
#> $eval
#>   [1]  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0
#>  [26] NA  0 NA  0 NA  0 NA  0 NA  0  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0
#>  [51] NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0  0  0  0 NA  0  0  0  0  0  0  0
#>  [76]  0  0  0  0  0  0  0  0  0  0  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0
#> [101] NA  0 NA  0 NA  0 NA  0 NA  1  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  1
#> [126]  0 NA  0 NA  0 NA  0 NA  0 NA  0  0 NA  0 NA  0 NA  0  0  0 NA  0 NA  0 NA
#> [151]  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA
#> [176]  0 NA  0  0 NA  0 NA  0 NA  0  0 NA  0 NA  0  0 NA  0 NA  0 NA  0 NA  0 NA
#> [201]  0 NA  0 NA  0 NA  0 NA  0  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA
#> [226]  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0
#> [251] NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA
#> [276]  0 NA  0 NA  0 NA  0 NA  0 NA  1  0 NA  1  0 NA  0 NA  0  0 NA  0  0 NA  0
#> [301] NA  0  0 NA  0  0 NA  0 NA  0  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0
#> [326] NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA
#> 
#> $eval_data
#>     record_id        redcap_event_name eval
#> 1       100-6     baseline_visit_arm_1    0
#> 2       100-6 follow_up_visit_da_arm_1   NA
#> 3      100-13     baseline_visit_arm_1    0
#> 4      100-13 follow_up_visit_da_arm_1   NA
#> 5      100-16     baseline_visit_arm_1    0
#> 6      100-16 follow_up_visit_da_arm_1   NA
#> 7      100-31     baseline_visit_arm_1    0
#> 8      100-31 follow_up_visit_da_arm_1   NA
#> 9      100-34     baseline_visit_arm_1    0
#> 10     100-34 follow_up_visit_da_arm_1   NA
#> 11     100-36     baseline_visit_arm_1    0
#> 12     100-36 follow_up_visit_da_arm_1   NA
#> 13     100-52     baseline_visit_arm_1    0
#> 14     100-52 follow_up_visit_da_arm_1   NA
#> 15     100-58     baseline_visit_arm_1    0
#> 16     100-58 follow_up_visit_da_arm_1   NA
#> 17     100-65     baseline_visit_arm_1    0
#> 18     100-65 follow_up_visit_da_arm_1   NA
#> 19     100-66     baseline_visit_arm_1    0
#> 20     100-66 follow_up_visit_da_arm_1   NA
#> 21     100-72     baseline_visit_arm_1    0
#> 22     100-72 follow_up_visit_da_arm_1   NA
#> 23     100-82     baseline_visit_arm_1    0
#> 24     100-82 follow_up_visit_da_arm_1   NA
#> 25      101-8     baseline_visit_arm_1    0
#> 26      101-8 follow_up_visit_da_arm_1   NA
#> 27     101-13     baseline_visit_arm_1    0
#> 28     101-13 follow_up_visit_da_arm_1   NA
#> 29     101-16     baseline_visit_arm_1    0
#> 30     101-16 follow_up_visit_da_arm_1   NA
#> 31     101-19     baseline_visit_arm_1    0
#> 32     101-19 follow_up_visit_da_arm_1   NA
#> 33     101-22     baseline_visit_arm_1    0
#> 34     101-22 follow_up_visit_da_arm_1   NA
#> 35     101-36     baseline_visit_arm_1    0
#> 36     101-45     baseline_visit_arm_1    0
#> 37     101-45 follow_up_visit_da_arm_1   NA
#> 38     101-48     baseline_visit_arm_1    0
#> 39     101-48 follow_up_visit_da_arm_1   NA
#> 40     101-59     baseline_visit_arm_1    0
#> 41     101-59 follow_up_visit_da_arm_1   NA
#> 42     101-70     baseline_visit_arm_1    0
#> 43     101-70 follow_up_visit_da_arm_1   NA
#> 44     101-74     baseline_visit_arm_1    0
#> 45     101-74 follow_up_visit_da_arm_1   NA
#> 46     101-75     baseline_visit_arm_1    0
#> 47     101-75 follow_up_visit_da_arm_1   NA
#> 48     101-78     baseline_visit_arm_1    0
#> 49     101-78 follow_up_visit_da_arm_1   NA
#> 50     101-83     baseline_visit_arm_1    0
#> 51     101-83 follow_up_visit_da_arm_1   NA
#> 52     101-85     baseline_visit_arm_1    0
#> 53     101-85 follow_up_visit_da_arm_1   NA
#> 54     101-86     baseline_visit_arm_1    0
#> 55     101-86 follow_up_visit_da_arm_1   NA
#> 56     101-88     baseline_visit_arm_1    0
#> 57     101-88 follow_up_visit_da_arm_1   NA
#> 58     101-93     baseline_visit_arm_1    0
#> 59     101-93 follow_up_visit_da_arm_1   NA
#> 60     101-96     baseline_visit_arm_1    0
#> 61     101-96 follow_up_visit_da_arm_1   NA
#> 62     101-98     baseline_visit_arm_1    0
#> 63     101-98 follow_up_visit_da_arm_1   NA
#> 64      102-6     baseline_visit_arm_1    0
#> 65     102-10     baseline_visit_arm_1    0
#> 66     102-13     baseline_visit_arm_1    0
#> 67     102-15     baseline_visit_arm_1    0
#> 68     102-15 follow_up_visit_da_arm_1   NA
#> 69     102-23     baseline_visit_arm_1    0
#> 70     102-27     baseline_visit_arm_1    0
#> 71     102-30     baseline_visit_arm_1    0
#> 72     102-39     baseline_visit_arm_1    0
#> 73     102-48     baseline_visit_arm_1    0
#> 74     102-60     baseline_visit_arm_1    0
#> 75     102-64     baseline_visit_arm_1    0
#> 76     102-73     baseline_visit_arm_1    0
#> 77     102-74     baseline_visit_arm_1    0
#> 78     102-75     baseline_visit_arm_1    0
#> 79     102-83     baseline_visit_arm_1    0
#> 80     102-84     baseline_visit_arm_1    0
#> 81    102-103     baseline_visit_arm_1    0
#> 82    102-104     baseline_visit_arm_1    0
#> 83    102-113     baseline_visit_arm_1    0
#> 84    102-127     baseline_visit_arm_1    0
#> 85    102-134     baseline_visit_arm_1    0
#> 86      103-6     baseline_visit_arm_1    0
#> 87      103-6 follow_up_visit_da_arm_1   NA
#> 88      103-9     baseline_visit_arm_1    0
#> 89      103-9 follow_up_visit_da_arm_1   NA
#> 90     103-10     baseline_visit_arm_1    0
#> 91     103-10 follow_up_visit_da_arm_1   NA
#> 92     103-14     baseline_visit_arm_1    0
#> 93     103-14 follow_up_visit_da_arm_1   NA
#> 94     103-16     baseline_visit_arm_1    0
#> 95     103-16 follow_up_visit_da_arm_1   NA
#> 96     103-17     baseline_visit_arm_1    0
#> 97     103-17 follow_up_visit_da_arm_1   NA
#> 98     103-19     baseline_visit_arm_1    0
#> 99     103-19 follow_up_visit_da_arm_1   NA
#> 100     104-8     baseline_visit_arm_1    0
#> 101     104-8 follow_up_visit_da_arm_1   NA
#> 102    104-12     baseline_visit_arm_1    0
#> 103    104-12 follow_up_visit_da_arm_1   NA
#> 104    104-13     baseline_visit_arm_1    0
#> 105    104-13 follow_up_visit_da_arm_1   NA
#> 106     105-8     baseline_visit_arm_1    0
#> 107     105-8 follow_up_visit_da_arm_1   NA
#> 108     105-9     baseline_visit_arm_1    0
#> 109     105-9 follow_up_visit_da_arm_1   NA
#> 110    105-11     baseline_visit_arm_1    1
#> 111    105-13     baseline_visit_arm_1    0
#> 112    105-13 follow_up_visit_da_arm_1   NA
#> 113    105-19     baseline_visit_arm_1    0
#> 114    105-19 follow_up_visit_da_arm_1   NA
#> 115    105-21     baseline_visit_arm_1    0
#> 116    105-21 follow_up_visit_da_arm_1   NA
#> 117    105-22     baseline_visit_arm_1    0
#> 118    105-22 follow_up_visit_da_arm_1   NA
#> 119    105-30     baseline_visit_arm_1    0
#> 120    105-30 follow_up_visit_da_arm_1   NA
#> 121    105-46     baseline_visit_arm_1    0
#> 122    105-46 follow_up_visit_da_arm_1   NA
#> 123    105-52     baseline_visit_arm_1    0
#> 124    105-52 follow_up_visit_da_arm_1   NA
#> 125    105-56     baseline_visit_arm_1    1
#> 126    105-58     baseline_visit_arm_1    0
#> 127    105-58 follow_up_visit_da_arm_1   NA
#> 128    105-78     baseline_visit_arm_1    0
#> 129    105-78 follow_up_visit_da_arm_1   NA
#> 130    105-85     baseline_visit_arm_1    0
#> 131    105-85 follow_up_visit_da_arm_1   NA
#> 132     106-8     baseline_visit_arm_1    0
#> 133     106-8 follow_up_visit_da_arm_1   NA
#> 134     106-9     baseline_visit_arm_1    0
#> 135     106-9 follow_up_visit_da_arm_1   NA
#> 136    106-15     baseline_visit_arm_1    0
#> 137    106-18     baseline_visit_arm_1    0
#> 138    106-18 follow_up_visit_da_arm_1   NA
#> 139    106-22     baseline_visit_arm_1    0
#> 140    106-22 follow_up_visit_da_arm_1   NA
#> 141    106-27     baseline_visit_arm_1    0
#> 142    106-27 follow_up_visit_da_arm_1   NA
#> 143    106-29     baseline_visit_arm_1    0
#> 144    106-32     baseline_visit_arm_1    0
#> 145    106-33     baseline_visit_arm_1    0
#> 146    106-33 follow_up_visit_da_arm_1   NA
#> 147    107-11     baseline_visit_arm_1    0
#> 148    107-11 follow_up_visit_da_arm_1   NA
#> 149     108-8     baseline_visit_arm_1    0
#> 150     108-8 follow_up_visit_da_arm_1   NA
#> 151    108-13     baseline_visit_arm_1    0
#> 152    108-13 follow_up_visit_da_arm_1   NA
#> 153    108-16     baseline_visit_arm_1    0
#> 154    108-16 follow_up_visit_da_arm_1   NA
#> 155    108-17     baseline_visit_arm_1    0
#> 156    108-17 follow_up_visit_da_arm_1   NA
#> 157    108-19     baseline_visit_arm_1    0
#> 158    108-19 follow_up_visit_da_arm_1   NA
#> 159    108-23     baseline_visit_arm_1    0
#> 160    108-23 follow_up_visit_da_arm_1   NA
#> 161    108-28     baseline_visit_arm_1    0
#> 162    108-28 follow_up_visit_da_arm_1   NA
#> 163    108-29     baseline_visit_arm_1    0
#> 164    108-30     baseline_visit_arm_1    0
#> 165    108-30 follow_up_visit_da_arm_1   NA
#> 166    108-35     baseline_visit_arm_1    0
#> 167    108-35 follow_up_visit_da_arm_1   NA
#> 168    108-44     baseline_visit_arm_1    0
#> 169    108-44 follow_up_visit_da_arm_1   NA
#> 170    108-45     baseline_visit_arm_1    0
#> 171    108-45 follow_up_visit_da_arm_1   NA
#> 172    108-56     baseline_visit_arm_1    0
#> 173    108-56 follow_up_visit_da_arm_1   NA
#> 174    108-63     baseline_visit_arm_1    0
#> 175    108-63 follow_up_visit_da_arm_1   NA
#> 176     109-8     baseline_visit_arm_1    0
#> 177     109-8 follow_up_visit_da_arm_1   NA
#> 178    109-10     baseline_visit_arm_1    0
#> 179    109-11     baseline_visit_arm_1    0
#> 180    109-11 follow_up_visit_da_arm_1   NA
#> 181    109-19     baseline_visit_arm_1    0
#> 182    109-19 follow_up_visit_da_arm_1   NA
#> 183    109-21     baseline_visit_arm_1    0
#> 184    109-21 follow_up_visit_da_arm_1   NA
#> 185    109-22     baseline_visit_arm_1    0
#> 186    109-32     baseline_visit_arm_1    0
#> 187    109-32 follow_up_visit_da_arm_1   NA
#> 188    109-33     baseline_visit_arm_1    0
#> 189    109-33 follow_up_visit_da_arm_1   NA
#> 190    109-34     baseline_visit_arm_1    0
#> 191    109-35     baseline_visit_arm_1    0
#> 192    109-35 follow_up_visit_da_arm_1   NA
#> 193    109-37     baseline_visit_arm_1    0
#> 194    109-37 follow_up_visit_da_arm_1   NA
#> 195    109-38     baseline_visit_arm_1    0
#> 196    109-38 follow_up_visit_da_arm_1   NA
#> 197     110-6     baseline_visit_arm_1    0
#> 198     110-6 follow_up_visit_da_arm_1   NA
#> 199    110-19     baseline_visit_arm_1    0
#> 200    110-19 follow_up_visit_da_arm_1   NA
#> 201     111-7     baseline_visit_arm_1    0
#> 202     111-7 follow_up_visit_da_arm_1   NA
#> 203     111-8     baseline_visit_arm_1    0
#> 204     111-8 follow_up_visit_da_arm_1   NA
#> 205    111-10     baseline_visit_arm_1    0
#> 206    111-10 follow_up_visit_da_arm_1   NA
#> 207     112-6     baseline_visit_arm_1    0
#> 208     112-6 follow_up_visit_da_arm_1   NA
#> 209     112-7     baseline_visit_arm_1    0
#> 210    112-10     baseline_visit_arm_1    0
#> 211    112-10 follow_up_visit_da_arm_1   NA
#> 212    112-11     baseline_visit_arm_1    0
#> 213    112-11 follow_up_visit_da_arm_1   NA
#> 214    112-12     baseline_visit_arm_1    0
#> 215    112-12 follow_up_visit_da_arm_1   NA
#> 216    112-14     baseline_visit_arm_1    0
#> 217    112-14 follow_up_visit_da_arm_1   NA
#> 218    112-15     baseline_visit_arm_1    0
#> 219    112-15 follow_up_visit_da_arm_1   NA
#> 220    112-18     baseline_visit_arm_1    0
#> 221    112-18 follow_up_visit_da_arm_1   NA
#> 222    112-19     baseline_visit_arm_1    0
#> 223    112-19 follow_up_visit_da_arm_1   NA
#> 224    112-20     baseline_visit_arm_1    0
#> 225    112-20 follow_up_visit_da_arm_1   NA
#> 226    112-21     baseline_visit_arm_1    0
#> 227    112-21 follow_up_visit_da_arm_1   NA
#> 228     113-9     baseline_visit_arm_1    0
#> 229     113-9 follow_up_visit_da_arm_1   NA
#> 230    113-10     baseline_visit_arm_1    0
#> 231    113-10 follow_up_visit_da_arm_1   NA
#> 232    113-11     baseline_visit_arm_1    0
#> 233    113-11 follow_up_visit_da_arm_1   NA
#> 234    113-12     baseline_visit_arm_1    0
#> 235    113-12 follow_up_visit_da_arm_1   NA
#> 236    113-13     baseline_visit_arm_1    0
#> 237    113-13 follow_up_visit_da_arm_1   NA
#> 238    113-16     baseline_visit_arm_1    0
#> 239    113-16 follow_up_visit_da_arm_1   NA
#> 240    113-21     baseline_visit_arm_1    0
#> 241    113-21 follow_up_visit_da_arm_1   NA
#> 242    113-23     baseline_visit_arm_1    0
#> 243    113-23 follow_up_visit_da_arm_1   NA
#> 244    113-24     baseline_visit_arm_1    0
#> 245    113-24 follow_up_visit_da_arm_1   NA
#> 246    113-27     baseline_visit_arm_1    0
#> 247    113-27 follow_up_visit_da_arm_1   NA
#> 248    113-31     baseline_visit_arm_1    0
#> 249    113-31 follow_up_visit_da_arm_1   NA
#> 250    113-32     baseline_visit_arm_1    0
#> 251    113-32 follow_up_visit_da_arm_1   NA
#> 252    113-36     baseline_visit_arm_1    0
#> 253    113-36 follow_up_visit_da_arm_1   NA
#> 254    113-37     baseline_visit_arm_1    0
#> 255    113-37 follow_up_visit_da_arm_1   NA
#> 256    113-38     baseline_visit_arm_1    0
#> 257    113-38 follow_up_visit_da_arm_1   NA
#> 258    113-40     baseline_visit_arm_1    0
#> 259    113-40 follow_up_visit_da_arm_1   NA
#> 260    113-41     baseline_visit_arm_1    0
#> 261    113-41 follow_up_visit_da_arm_1   NA
#> 262    113-44     baseline_visit_arm_1    0
#> 263    113-44 follow_up_visit_da_arm_1   NA
#> 264    114-13     baseline_visit_arm_1    0
#> 265    114-13 follow_up_visit_da_arm_1   NA
#> 266    114-17     baseline_visit_arm_1    0
#> 267    114-17 follow_up_visit_da_arm_1   NA
#> 268    114-19     baseline_visit_arm_1    0
#> 269    114-19 follow_up_visit_da_arm_1   NA
#> 270     115-8     baseline_visit_arm_1    0
#> 271     115-8 follow_up_visit_da_arm_1   NA
#> 272    115-13     baseline_visit_arm_1    0
#> 273    115-13 follow_up_visit_da_arm_1   NA
#> 274    115-14     baseline_visit_arm_1    0
#> 275    115-14 follow_up_visit_da_arm_1   NA
#> 276    115-24     baseline_visit_arm_1    0
#> 277    115-24 follow_up_visit_da_arm_1   NA
#> 278     116-6     baseline_visit_arm_1    0
#> 279     116-6 follow_up_visit_da_arm_1   NA
#> 280    116-12     baseline_visit_arm_1    0
#> 281    116-12 follow_up_visit_da_arm_1   NA
#> 282    116-14     baseline_visit_arm_1    0
#> 283    116-14 follow_up_visit_da_arm_1   NA
#> 284     117-6     baseline_visit_arm_1    0
#> 285     117-6 follow_up_visit_da_arm_1   NA
#> 286    117-11     baseline_visit_arm_1    1
#> 287    117-12     baseline_visit_arm_1    0
#> 288    117-12 follow_up_visit_da_arm_1   NA
#> 289    117-22     baseline_visit_arm_1    1
#> 290    117-25     baseline_visit_arm_1    0
#> 291    117-25 follow_up_visit_da_arm_1   NA
#> 292    117-26     baseline_visit_arm_1    0
#> 293    117-26 follow_up_visit_da_arm_1   NA
#> 294     118-7     baseline_visit_arm_1    0
#> 295    118-10     baseline_visit_arm_1    0
#> 296    118-10 follow_up_visit_da_arm_1   NA
#> 297    118-12     baseline_visit_arm_1    0
#> 298     119-6     baseline_visit_arm_1    0
#> 299     119-6 follow_up_visit_da_arm_1   NA
#> 300     119-7     baseline_visit_arm_1    0
#> 301     119-7 follow_up_visit_da_arm_1   NA
#> 302    119-14     baseline_visit_arm_1    0
#> 303     120-6     baseline_visit_arm_1    0
#> 304     120-6 follow_up_visit_da_arm_1   NA
#> 305    120-10     baseline_visit_arm_1    0
#> 306    120-13     baseline_visit_arm_1    0
#> 307    120-13 follow_up_visit_da_arm_1   NA
#> 308    120-15     baseline_visit_arm_1    0
#> 309    120-15 follow_up_visit_da_arm_1   NA
#> 310    120-26     baseline_visit_arm_1    0
#> 311     121-7     baseline_visit_arm_1    0
#> 312     121-7 follow_up_visit_da_arm_1   NA
#> 313    121-10     baseline_visit_arm_1    0
#> 314    121-10 follow_up_visit_da_arm_1   NA
#> 315     122-8     baseline_visit_arm_1    0
#> 316     122-8 follow_up_visit_da_arm_1   NA
#> 317    122-12     baseline_visit_arm_1    0
#> 318    122-12 follow_up_visit_da_arm_1   NA
#> 319    122-13     baseline_visit_arm_1    0
#> 320    122-13 follow_up_visit_da_arm_1   NA
#> 321    122-18     baseline_visit_arm_1    0
#> 322    122-18 follow_up_visit_da_arm_1   NA
#> 323    122-19     baseline_visit_arm_1    0
#> 324    122-19 follow_up_visit_da_arm_1   NA
#> 325    122-23     baseline_visit_arm_1    0
#> 326    122-23 follow_up_visit_da_arm_1   NA
#> 327    122-30     baseline_visit_arm_1    0
#> 328    122-30 follow_up_visit_da_arm_1   NA
#> 329    122-33     baseline_visit_arm_1    0
#> 330    122-33 follow_up_visit_da_arm_1   NA
#> 331    122-35     baseline_visit_arm_1    0
#> 332    122-35 follow_up_visit_da_arm_1   NA
#> 333    123-10     baseline_visit_arm_1    0
#> 334    123-10 follow_up_visit_da_arm_1   NA
#> 335     124-7     baseline_visit_arm_1    0
#> 336     124-7 follow_up_visit_da_arm_1   NA
#> 337     124-9     baseline_visit_arm_1    0
#> 338     124-9 follow_up_visit_da_arm_1   NA
#> 339     125-6     baseline_visit_arm_1    0
#> 340     125-6 follow_up_visit_da_arm_1   NA
#> 341    125-10     baseline_visit_arm_1    0
#> 342    125-10 follow_up_visit_da_arm_1   NA
#> 
```
