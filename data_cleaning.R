#Read in Data


# DEPENDENCIES IMPORTED PRIOR TO CALL

raw = read_csv("prac_data.csv")

raw_data = read_csv("prac_data.csv") %>%
  mutate(
    price = unlist(read_csv("price.csv")),
    job = recode(job,unknown = "Unknown"),
    job = as.factor(replace_na(job,"Unknown")),
    education = replace_na(education,"Unknown"),
    education = replace(education,which(education == "unknown"),"Unknown"),
    contact = replace_na(contact,"Unknown"),
    contact = replace(contact,which(contact == "unknown"),"Unknown"),
    pdays = replace_na(pdays,-1),
    default = replace_na(default,"Unknown"),
    marital = replace_na(marital,"Unkown"),
    housing = replace_na(housing,"Unkown"),
    loan = replace_na(loan,"Unkown"),
    previous = replace_na(previous,0),
    poutcome = replace_na(poutcome,"Unknown"),
    numMissing = rowSums(across(everything(), ~is.na(.))),
    y = if_else(y == "ye","yes","no"),
    y = as.factor(y)
  )

medianBalance = median(raw_data$balance,na.rm = TRUE)
medianCampaign = median(raw_data$campaign, na.rm = TRUE)
medianAge = median(raw_data$age, na.rm = TRUE)
medianDuration = median(raw_data$duration, na.rm = TRUE)
imputedData = raw_data %>%
  mutate(
    balance = replace_na(balance,medianBalance),
    campaign = replace_na(campaign,medianCampaign),
    age = replace_na(age,medianAge),
    duration = replace_na(duration,medianDuration),
    log_balance = log (balance - (-8019)),
    job_reduced = recode(job,Unknown = "unknown",
                                 student = "student",
                                 housemaid = "blue-collar",
                                 unemployed = "unemployed",
                                 entrepreneur = "white-collar",
                                 `self-employe` = "white-collar",
                                 retired = "unemployed",
                                 services = "blue-collar",
                                 admin = "white-collar",
                                 technician = "blue-collar",
                                 management = "white-collar"
                                 )
  )
imputedData = imputedData %>%
  filter(previous != max(raw_data$previous)) %>%
  mutate(
    campaign_binned = cut(campaign,
                          breaks = c(-Inf,5,10,15,Inf),
                          labels = c("<5","5-10","10-15",">15"))
  )
raw_cont = raw_data %>%
  select(balance,campaign,age,duration)
imputed_cont =imputedData %>%
  select(balance,campaign,age,duration)
raw_cont$set = "original"
imputed_cont$set = "Imputed"
final_cont = rbind(raw_cont,imputed_cont)
numericColumns = c(1,6,10,12,13,14,15,18)
categoricalColumns = c(2,3,4,5,7,8,9)
df_colnames = colnames(raw_data)
