hr <- 13
min <- 30

mins <- hr * 60 + 30

getTimeSlot <- function (mins) {
  # 00:00 - 3:00
  if(mins >= 0 && mins <= 3 * 60) {
    print ('Late night')
  } else if (mins > (3 * 60) && mins <= (6 * 60)) {
    print ('Early morning')
  } else if (mins > 6 * 60 && mins <= 12 * 60) {
    print ('Morning')
  } else if (mins > 12 * 60 && mins <= 15 * 60) {
    print ('Afternoon')
  }
}

getTimeSlot(mins)
