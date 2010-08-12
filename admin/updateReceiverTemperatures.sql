-- use temperature value at 39.5 GHz for odd value at 40.0 GHz for Rcvr26_40
update receiver_temperatures set temperature=63.8999996185303 where receiver_id = 14 and frequency = 40;
