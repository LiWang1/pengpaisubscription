
# 1) vips 2) exsisting customers 3) new customers 

# loss of vips 
# new vips from the existing costomers and existed existing customers 
# new vips from the new costomers and new exsisting costomers from new customers 

exist_vin = 0 
new_vin = 100
vip_vin = 0 

loss_vip <- function(vip_vin, loss_rate){
  lossvip = vip_vin*loss_rate
  return(lossvip)
}

transfer_vip <- function(exist_vin, transfer_rate){
 tranfer_vin = exist_vin*transfer_rate
 return(tranfer_vin)
}
addnew_vip <- function(new_vin, buy_rate){
  new_vin = new_vin*buy_rate
  return(new_vin)
}




pengpai_subscription <- function(new_vin_vec, date_vec, loss_rate, transfer_rate, buy_rate){
  num_vip = 0 
  num_exised = 0
  for( i in 1:length(new_vin_vec)){
    # loss and transfer of vips 
    num_vip = num_vip - loss_vip(num_vip, loss_rate) + transfer_vip(num_exised, transfer_rate)
    num_exised = num_exised + loss_vip(num_vip, loss_rate) - transfer_vip(num_exised, transfer_rate)
    
    # new members from new customers 
    num_vip = num_vip+ addnew_vip(new_vin_vec[i], buy_rate)
    num_exised = num_exised + (new_vin_vec[i]-addnew_vip(new_vin_vec[i], buy_rate))
    print(paste(date_vec[i], num_vip, num_exised))
  }
}

num_vip = 0 
num_existed = 0 
loss_rate = 0.01
transfer_rate = 15/100
buy_rate = 0.5
new_vin_vec = c(2000, 2000, 2000)
date_vec = c('1 mon', '2 mon', '3 mon')

pengpai_subscription(new_vin_vec, date_vec, loss_rate, transfer_rate, buy_rate)
