
# loading the data from CSV file
data <- read.csv('C:\\Users\\WILLIAM\\Documents\\HKU\\FINA4354\\Mid-term\\Data.csv', header = TRUE)

#initial value setting
SCVA = 0
total = 0
ocva_1 =0
ocva_2 = 0
final_cva = 0
number = nrow(data)

#loop through each counterparty
for (i in 1:number) {
  #extracting the data from each counterparty
  risk_weight = data[i ,"Risk.Weight"]
  eff = data[i,'Effective.Maturity']
  exposure = data[i,'Exposure']
  recovery = data[i,'Recovery.Rate']
  #assuming 5% discount rate, the same rate provided in the BASEL example
  discount_rate = ((1-exp(-0.05*eff))/0.05*eff)
  alpha = 1.4
  
  #the fair value before adjustments
  total = total + data[i,'Exposure']
  
  #calculating the CVA based of the formula from BASEL
  SCVA = risk_weight*eff*(exposure*(i-recovery))*discount_rate*1/alpha
  
  #calculating the first part
  ocva_1 = ocva_1 + SCVA
  #calculating the second part
  ocva_2 = ocva_2 + SCVA^2
  
  }

final_cva = sqrt((0.5 * ocva_1)^2 + (1-0.5^2)*ocva_2)

cat("The CVA Value is",final_cva)

cat('The Present Value', total)

cat('The Present Value that is credit risk free', total - final_cva)









