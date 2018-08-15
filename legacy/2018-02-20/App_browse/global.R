shores <- names(tides)

SHORES <- c('(UK) South Cairn', '(UK) Isle Anglesey', '(IE) Emlagh', '(IE) Minard Castle', '(UK) Wembury', '(FR) Landunvez', '(FR) Le Croisic', '(FR) Royan', '(FR) Biarritz', '(ES) SV Barquera', '(ES) La Caridad', '(ES) Cabo Touriñan', '(PT) Moledo', '(PT) S. Lourenço', '(PT) Alteirinhos', '(PT) Evaristo')
names(SHORES) <- c("Sc", "An", "Em", "Mc", "We", "La", "Cc", "Rn", "Bi", "Sv", "Lc", "To", "Mo", "Sl", "Al", "Ev")

res <- 0 # 1 = high, 0 = low
sampling <- 1 # fractions of hours, i.e., half hour = 0.5, 10 mins = 1/6
max_readings <- 8192 # maximum number of readings at low res
# in seconds (-1 day, to make it a little bit conservative)
lifeSpan <- (max_readings/(1+res))*(3600*sampling) - (3600*24)

unlink(dir(dirname(getwd()), pattern='plot_', full.names=T))

