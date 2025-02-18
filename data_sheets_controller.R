as.integer() # 12, 31, 331
as.numeric() # 12.5, 31.5, 331.5
as.character() # "words"
as.Date(Sys.time()) # "YYYY-MM-DD"

# rosterDF()
# teamsDF()
# classList()
# posList()
# sportList()

# Last Sync
df <- data.frame(
  lastHawkinSync = as.numeric(),
  lastRosterSync = as.numeric()
)

# Roster

df <- data.frame(
  id = as.character(),
  name = as.character(),
  teams = as.character(),
  groups = as.character(),
  active = as.character(),
  email = as.character(),
  position = as.character(),
  class = as.character(),
  sport = as.character(),
  updated = as.numeric()
)

# Anthropometrics
df <- data.frame(
  timestamp = as.numeric(),
  date = as.character(),
  name = as.character(),
  athleteId = as.character(),
  teams = as.character(),
  groups = as.character(),
  active = as.character(),
  email = as.character(),
  position = as.character(),
  class = as.character(),
  sport = as.character(),
  height_ft = as.integer(),
  height_in = as.numeric(),
  wingspan = as.integer(),
  reach = as.integer()
)

googlesheets4::write_sheet(df, ss = gsheetId, sheet = "Anthropometrics")

# Countermovement Jump
df <- data.frame(
  testId = as.character(),
  timestamp = as.numeric(),
  date = as.character(),
  type = as.character(),
  tags = as.character(),
  name = as.character(),
  athleteId = as.character(),
  teams = as.character(),
  groups = as.character(),
  active = as.character(),
  email = as.character(),
  position = as.character(),
  class = as.character(),
  sport = as.character(),
  jump_height_in = as.numeric(),
  l_r_peak_landing_force = as.numeric(),
  l_r_peak_propulsive_force = as.numeric(),
  avg_prop_velocity = as.numeric()
)

googlesheets4::write_sheet(df, ss = gsheetId, sheet = "Countermovement Jump")

# Squat Jump
df <- data.frame(
  testId = as.character(),
  timestamp = as.numeric(),
  date = as.character(),
  type = as.character(),
  tags = as.character(),
  name = as.character(),
  athleteId = as.character(),
  teams = as.character(),
  groups = as.character(),
  active = as.character(),
  email = as.character(),
  position = as.character(),
  class = as.character(),
  sport = as.character(),
  jump_height_in = as.numeric(),
  eur = as.numeric()
)

googlesheets4::write_sheet(df, ss = gsheetId, sheet = "Squat Jump")

# Multi-Rebound Jump
df <- data.frame(
  testId = as.character(),
  timestamp = as.numeric(),
  date = as.character(),
  type = as.character(),
  tags = as.character(),
  name = as.character(),
  athleteId = as.character(),
  teams = as.character(),
  groups = as.character(),
  active = as.character(),
  email = as.character(),
  position = as.character(),
  class = as.character(),
  sport = as.character(),
  peakRSI = as.numeric(),
  top3_avgRSI = as.numeric(),
  top5_avgRSI = as.numeric()
)

googlesheets4::write_sheet(df, ss = gsheetId, sheet = "Multi-Rebound")

#------------------------------------------------------------------#
#------------------------------------------------------------------#
#------------------------------------------------------------------#

# Lane Agility
df <- data.frame(
  timestamp = as.numeric(),
  date = as.character(),
  name = as.character(),
  athleteId = as.character(),
  teams = as.character(),
  groups = as.character(),
  active = as.character(),
  email = as.character(),
  position = as.character(),
  class = as.character(),
  sport = as.character(),
  surface = as.character(),
  time = as.numeric()
)

googlesheets4::write_sheet(df, ss = gsheetId, sheet = "Lane Agility")

# 5-10-5 Pro Agility
df <- data.frame(
  timestamp = as.numeric(),
  date = as.character(),
  name = as.character(),
  athleteId = as.character(),
  teams = as.character(),
  groups = as.character(),
  active = as.character(),
  email = as.character(),
  position = as.character(),
  class = as.character(),
  sport = as.character(),
  surface = as.character(),
  direction = as.character(),
  time = as.numeric()
)

googlesheets4::write_sheet(df, ss = gsheetId, sheet = "5-10-5 Pro Agility")

# 40 Yard Dash
df <- data.frame(
  timestamp = as.numeric(),
  date = as.character(),
  name = as.character(),
  athleteId = as.character(),
  teams = as.character(),
  groups = as.character(),
  active = as.character(),
  email = as.character(),
  position = as.character(),
  class = as.character(),
  sport = as.character(),
  surface = as.character(),
  time_10y = as.numeric(),
  time_20y = as.numeric(),
  time_40y = as.numeric()
)

googlesheets4::write_sheet(df, ss = gsheetId, sheet = "40 Yard Dash")

# 3/4 Quarter Court Sprint
df <- data.frame(
  timestamp = as.numeric(233924239842),
  date = as.character('2021-09-01'),
  name = as.character('John Doe'),
  athleteId = as.character('123456'),
  teams = as.character('Team A'),
  groups = as.character('Group 1'),
  active = as.character('Yes'),
  email = as.character('fake@email.com'),
  position = as.character('QB'),
  class = as.character('2023'),
  sport = as.character('Football'),
  surface = as.character('Turf'),
  time = as.numeric(4.5)
)

update_gsheet(sheet = "3/4 Quarter Court", data = df)

googlesheets4::write_sheet(df, ss = gsheetId, sheet = "3/4 Quarter Court")

# Broad Jump
df <- data.frame(
  timestamp = as.numeric(),
  date = as.character(),
  name = as.character(),
  athleteId = as.character(),
  teams = as.character(),
  groups = as.character(),
  active = as.character(),
  email = as.character(),
  position = as.character(),
  class = as.character(),
  sport = as.character(),
  distance_ft_in = as.character(),
  distance_in = as.integer()
)

googlesheets4::write_sheet(df, ss = gsheetId, sheet = "Broad Jump")

# Vertical Jump
df <- data.frame(
  timestamp = as.numeric(),
  date = as.character(),
  name = as.character(),
  athleteId = as.character(),
  teams = as.character(),
  groups = as.character(),
  active = as.character(),
  email = as.character(),
  position = as.character(),
  class = as.character(),
  sport = as.character(),
  test = as.character(),
  height_ft_in = as.character(),
  height_in = as.integer()
)

googlesheets4::write_sheet(df, ss = gsheetId, sheet = "Vertical Jump")
