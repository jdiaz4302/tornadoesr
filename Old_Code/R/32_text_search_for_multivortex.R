


# For easier text searching, make all capital letters
tor_df$EVENT_NARRATIVE <- toupper(tor_df$EVENT_NARRATIVE)


# Different ways to search for multivortex tornadoes
tor_df$proxy1 <- grepl("MULTIPLE VORTEX", tor_df$EVENT_NARRATIVE)

tor_df$proxy2 <- grepl("MULTIPLE-VORTEX", tor_df$EVENT_NARRATIVE)

tor_df$proxy3 <- grepl("MULTI-VORTEX", tor_df$EVENT_NARRATIVE)

tor_df$proxy4 <- grepl("MULTIVORTEX", tor_df$EVENT_NARRATIVE)

tor_df$proxy5 <- grepl("MULTIPLE VORTICES", tor_df$EVENT_NARRATIVE)

tor_df$proxy6 <- grepl("MULTIPLE-VORTICES", tor_df$EVENT_NARRATIVE)


# If any search yielded a find, then this new variable will be > 0
tor_df$MULTI_VORT_IND <- tor_df$proxy1 + tor_df$proxy2 + tor_df$proxy3 +
  tor_df$proxy4 + tor_df$proxy5 + tor_df$proxy6


# If there was a find, the event was a multivortex tornado
tor_df$MULTI_VORT_IND <- tor_df$MULTI_VORT_IND > 0


