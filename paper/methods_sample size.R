source("analysis/pbc01_train and validation split.R")

require(ggpubr)

train_ids_female %>% 
  group_by(iaea_country) %>% 
  tally()
train_female %>% 
  group_by(iaea_country) %>% 
  tally()

train_ids_male %>% 
  group_by(iaea_country) %>% 
  tally()

train_male %>% 
  group_by(iaea_country) %>% 
  tally()

# Validation ----------

validation_ids_female %>% 
  group_by(iaea_country) %>% 
  tally()
validation_female %>% 
  group_by(iaea_country) %>% 
  tally()

validation_ids_male %>% 
  group_by(iaea_country) %>% 
  tally()

validation_male %>% 
  group_by(iaea_country) %>% 
  tally()


# Test ----------
test_female %>% 
  distinct(iaea_country,record_id) %>% 
  tally()
test_male %>% 
  distinct(iaea_country,record_id) %>% 
  tally()

test_female %>% 
  distinct(iaea_country,record_id) %>% 
  group_by(iaea_country) %>% 
  tally()
test_female %>% 
  group_by(iaea_country) %>% 
  tally()

test_male %>% 
  distinct(iaea_country,record_id) %>% 
  group_by(iaea_country) %>% 
  tally()

test_male %>% 
  group_by(iaea_country) %>% 
  tally()
