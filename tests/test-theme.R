library(unittest)
library(hafroreports)

options(hr.lang = "en")

ok(ut_cmp_equal(hr_red_dot_number(123456), "123 456"), "Formatted number, including small space")
ok(ut_cmp_equal(hr_red_dot_number(1.1234), "1.1234"), "In english, used decimal point")

options(hr.lang = "is")

ok(ut_cmp_equal(hr_red_dot_number(123456), "123 456"), "Formatted number, including small space")
ok(ut_cmp_equal(hr_red_dot_number(1.1234), "1,1234"), "In Icelandic, used comma")
