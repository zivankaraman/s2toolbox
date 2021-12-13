library(sp)
library(rgeos)

gIsValid(S2_realtive_orbits, byid = TRUE, reason = TRUE)

o21 <- S2_realtive_orbits[21, ]
gIsValid(o21, byid = TRUE, reason = TRUE)

length(o21@polygons)
length(o21@polygons[[1]]@Polygons)

o21@polygons[[1]]@Polygons[[3]] <- NULL
o21b <- createSPComment(o21)
sapply(o21@polygons, slot, 'comment')
sapply(o21b@polygons, slot, 'comment')

gIsValid(o21b, byid = TRUE, reason = TRUE)

# o21@polygons[[1]]@Polygons <- o21@polygons[[1]]@Polygons[1:2]

tmp <- rbind(S2_realtive_orbits[1:20, ], o21b, S2_realtive_orbits[22:143, ])
table(gIsValid(tmp, byid = TRUE, reason = TRUE))

S2_realtive_orbits <- tmp
save(S2_realtive_orbits, file = "./data/S2_realtive_orbits.rda")
