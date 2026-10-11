test_that('nested unit keys preserve labels containing separators', {
  d <- data.frame(study = c('a / b', 'a', 'a%2fb', 'a'),
                  case = c('c', 'b / c', 'c', 'NA'))
  keys <- .std_key(d, c('study', 'case'))
  expect_equal(length(unique(keys)), nrow(d))
  expect_identical(.std_key(d[4:1, ], c('study', 'case')), rev(keys))
  expect_identical(.std_key(d[2, , drop = FALSE], c('study', 'case')), keys[2])
  missing <- data.frame(case = c(NA_character_, 'NA', '%NA'))
  expect_equal(length(unique(.std_key(missing, 'case'))), 3L)
  expect_identical(.std_key(data.frame(case = c('C1', 'C2')), 'case'), c('C1', 'C2'))
})

test_that('BC-SMD keeps distinct nested participant labels', {
  d <- data.frame(study = c('a.b', 'a'), case = c('c', 'b.c'), time = 1:2)
  levels <- .bc_smd_levels(d, 'case', 'time')
  expect_equal(length(unique(levels$participant)), 2L)
  expect_equal(length(unique(levels$tier)), 2L)
})

test_that('nested tier identities remain separate throughout analysis', {
  d <- data.frame(study = rep(c('A.B', 'A'), each = 6),
                  case = rep(c('C', 'B.C'), each = 6), time = rep(1:6, 2),
                  phase = c(rep('A', 3), rep('B', 3), rep('A', 4), rep('B', 2)),
                  outcome = c(1:6, 101:106))
  clean <- d
  clean$study <- rep(c('S1', 'S2'), each = 6)
  clean$case <- rep(c('C1', 'C2'), each = 6)
  expect_length(.tier_split(d, list(), 'time'), 2)
  expect_identical(detect_design_type(d), detect_design_type(clean))
  ppc <- .ppc_meta(d, 'outcome', 'case', 'phase', 'time')
  expect_length(ppc$case_levels, 2)
  expect_length(ppc$pairs, 2)
  expect_equal(.n_units(d), 2L)
  expect_silent(.ar_spacing_warning(d, 'outcome ~ time + ar(time = time, gr = study:case)'))
  expect_equal(.pooled_within_sd(d), .pooled_within_sd(clean))
})

test_that('delimiter collisions cannot hide staggered nested onsets', {
  d <- data.frame(study = 's', case = rep(c('A.B', 'A', 'A.B'), each = 6),
                  nest = rep(c('C', 'B.C', 'D'), each = 6), time = rep(1:6, 3),
                  phase = c(rep('A', 3), rep('B', 3), rep(c('A', rep('B', 5)), 2)))
  expect_true(.onsets_differ(d, 'nest', c('study', 'case'), 'time'))
})
test_that('factor unit keys retain ordinary labels and exclude missing fields', {
  d <- data.frame(study = c('S1', 'S2', NA, 'NA'), case = c('C1', 'C2', 'C3', 'C4'))
  key <- .std_factor(d, c('study', 'case'))
  expect_identical(as.character(key), c('S1.C1', 'S2.C2', NA_character_, 'NA.C4'))
  expect_equal(nlevels(key), 3L)
  expect_equal(sum(lengths(split(seq_len(nrow(d)), key))), 3L)
  expect_identical(.std_factor(d, character()), factor(rep('all', 4)))
})

