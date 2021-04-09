library("dplyr")

w_cc_cw <- read.csv(file = "output/w_cc_cw.csv") %>%
  rename(jar_no = jar_w_cc_cw)
no_cc_cw <- read.csv(file = "output/no_cc_cw.csv") %>%
  rename(jar_no = jar_no_cc_cw)

# Select 10 random jars for initial sampling
initial_sample_cw_w_cc <- sample_n(w_cc_cw, 5, replace = FALSE)
initial_sample_cw_no_cc <- sample_n(no_cc_cw, 5, replace = FALSE)

initial_sample_cw_all <- initial_sample_cw_w_cc %>%
  rbind(initial_sample_cw_no_cc) %>%
  select(jar_no) %>%
  arrange(jar_no)

write.csv(initial_sample_cw_all, file = "output/initial_sample_cw_all")


# Select 20 random jars at 1 week (prewet)
# Choose 10 consistent watering jars
wk1_sample_cw_w_cc <- sample_n(subset(
  w_cc_cw, !(jar_no %in% initial_sample_cw_w_cc)), 5, replace = FALSE)
wk1_sample_cw_no_cc <- sample_n(subset(
  no_cc_cw, !(jar_no %in% initial_sample_cw_no_cc)), 5, replace = FALSE)

# Choose 10 pre-wet 1 week drought jars
w_cc_1wk <- read.csv(file = "output/w_cc_1wk.csv") %>%
  rename(jar_no = jar_w_cc_1wk)
no_cc_1wk <- read.csv(file = "output/no_cc_1wk.csv") %>%
  rename(jar_no = jar_no_cc_1wk)

wk1_sample_prewet_w_cc <- sample_n(w_cc_1wk, 5, replace = FALSE)
wk1_sample_prewet_no_cc <- sample_n(no_cc_1wk, 5, replace = FALSE)

all_1wk_prewet <- wk1_sample_prewet_w_cc %>%
  rbind(wk1_sample_prewet_no_cc) %>%
  rbind(wk1_sample_cw_no_cc) %>%
  rbind(wk1_sample_cw_w_cc) %>%
  select(jar_no) %>%
  arrange(jar_no)

# Select 10 random jars at 1 week (postwet)
wk1_sample_postwet_w_cc <- sample_n(subset(
  w_cc_1wk, !(jar_no %in% wk1_sample_prewet_w_cc)), 5, replace = FALSE)
wk1_sample_postwet_no_cc <- sample_n(subset(
  no_cc_1wk, !(jar_no %in% wk1_sample_prewet_no_cc)), 5, replace = FALSE)

all_1wk_postwet <- wk1_sample_postwet_w_cc %>%
  rbind(wk1_sample_postwet_no_cc) %>%
  select(jar_no) %>%
  arrange(jar_no)

# Select 20 random jars at 2 weeks (prewet)
# Choose 10 consistent watering jars
wk2_sample_cw_w_cc <- sample_n(subset(
  w_cc_cw, !((jar_no %in% initial_sample_cw_w_cc) |
               (jar_no %in% wk1_sample_cw_w_cc))), 5, replace = FALSE)
wk2_sample_cw_no_cc <- sample_n(subset(
  no_cc_cw, !((jar_no %in% initial_sample_cw_no_cc) |
                (jar_no %in% wk1_sample_cw_no_cc))), 5, replace = FALSE)

# Choose 10 pre-wet 2 week drought jars
w_cc_2wk <- read.csv(file = "output/w_cc_2wk.csv") %>%
  rename(jar_no = jar_w_cc_2wk)
no_cc_2wk <- read.csv(file = "output/no_cc_2wk.csv") %>%
  rename(jar_no = jar_no_cc_2wk)




#Update these
wk1_sample_prewet_w_cc <- sample_n(w_cc_1wk, 5, replace = FALSE)
wk1_sample_prewet_no_cc <- sample_n(no_cc_1wk, 5, replace = FALSE)

all_1wk_prewet <- wk1_sample_prewet_w_cc %>%
  rbind(wk1_sample_prewet_no_cc) %>%
  rbind(wk1_sample_cw_no_cc) %>%
  rbind(wk1_sample_cw_w_cc) %>%
  select(jar_no) %>%
  arrange(jar_no)
