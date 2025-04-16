#datos duplicados
df_duplicated <- df[duplicated(df), ]
df_no_duplicated <- df[!duplicated(df$customer_id, fromLast = FALSE),]