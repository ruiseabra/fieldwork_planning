# check tides
future.tides$best$Ev
future.tides$ok$Bi

chosen.height <- c(
  Ev = 0.67,
  Al = 0.59,
  Sl = 0.47,
  Mi = 0.50,
  Mo = 0.46,
  To = 0.18,
  Lc = 0.25,
  Sv = 0.50,
  Bi = 0.41,
  Rn = 0.80,
  Cc = 0.75,
  La = 0.65,
  We = 0.43,
  Sc = 0.14,
  An = 0.26,
  Mc = 0.17,
  Em = 0.47)

chosen.time <- c(
  Ev = "2013-05-24 20:01:00",
  Al = "2013-05-24 07:56:00",
  Sl = "2013-05-26 09:10:00",
  Mi = "2013-07-24 09:46:00",
  Mo = "2013-05-27 10:16:00",
  To = "2013-04-26 09:54:00",
  Lc = "2013-04-27 11:24:00",
  Sv = "2013-04-28 11:54:00",
  Bi = "2013-04-29 12:02:00",
  Rn = "2013-04-25 09:25:00",
  Cc = "2013-04-26 10:01:00",
  La = "2013-04-28 12:28:00",
  We = "2013-04-27 12:43:00",
  Sc = "2013-05-27 06:45:00",
  An = "2013-05-26 16:53:00",
  Mc = "2013-05-10 10:26:00",
  Em = "2013-05-09 10:44:00")
  
chosen <- foreach(i = 1:length(chosen.height)) %do% zoo(chosen.height[i], chosen.time[i])
names(chosen) <- names(chosen.height)

#put in INFO
INFO$time$chosen <- chosen

#save data
rm(list=ls()[which(!ls() %in% c("previous.tides", "future.tides", "INFO"))])
save.image()