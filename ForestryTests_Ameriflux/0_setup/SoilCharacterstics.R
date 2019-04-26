# Pulling values from Web Soil Survey
# - entered lat lon and then drew a box around the area
# - used the surface layer with the weighted average aggregation method

# --------------------------
# Willow Creek (WCr); 45.805822, -90.079722
# --------------------------
wcr.areas <- c(0.041, 0.247, 0.230, 0.269, 0.007, 0.207)
sum(wcr.areas)
wcr.clay <- c(0.046, 0.041, 0.090, 0.090, 0.032, 0.110)
wcr.sand <- c(0.793, 0.714, 0.628, 0.628, 0.893, 0.525)
wcr.depth <- c(2.00, 0.84, 1.17, 1.17, 2.00, 1.14)
sum(wcr.clay*wcr.areas) # Percent Clay
sum(wcr.sand*wcr.areas) # Percent Sand
sum(wcr.depth*wcr.areas) # Percent Sand
# --------------------------


# --------------------------
# Harvard Forest (Ha1) 42.5378, -72.1715
# --------------------------
ha1.areas <- c(0.102, 0.109, 0.672, 0.118)
ha1.clay <- c(0.95, 0.030, 0.030, 0.065)
ha1.sand <- c(0.386, 0.575, 0.689, 0.571)
ha1.depth <- c(2.00, 0.54, 0.84, 0.61)
sum(ha1.clay*ha1.areas)
sum(ha1.sand*ha1.areas)
sum(ha1.depth*ha1.areas)
# --------------------------


# --------------------------
# Niwot Ridge (NR1) 40.0329, -105.5464
# --------------------------
nr1.areas <- c(0.178, 0.744, 0.008, 0.070)
nr1.clay <- c(0.136, 0.057, 0.099, 0.077)
nr1.sand <- c(0.465, 0.659, 0.557, 0.662)
nr1.depth <- c(0.71, 0.71, 0.71, 0.50)
sum(nr1.areas)

sum(nr1.clay*nr1.areas)
sum(nr1.sand*nr1.areas)
sum(nr1.depth*nr1.areas)
# --------------------------


# --------------------------
# Austin Cary (SP1) 29.73807, -82.21877
# --------------------------
sp1.areas <- c(0.832, 0.008, 0.096, 0.012, 0.037); sp1.areas <- sp1.areas/sum(sp1.areas)
sp1.clay <- c(0.126, 0.102, 0.093, 0.170, 0.094)
sp1.sand <- c(0.786, 0.811, 0.843, 0.724, 0.836)
sp1.depth <- c(2.00, 2.00, 2.00, 2.00, 2.00)

sum(sp1.clay*sp1.areas)
sum(sp1.sand*sp1.areas)
sum(sp1.depth*sp1.areas)
# --------------------------


# --------------------------
# Metolius (Me2) 44.4523, -121.5574
# --------------------------
me2.areas <- c(100); me2.areas <- me2.areas/sum(me2.areas)
me2.clay <- c(22.4)
me2.sand <- c(42.9)
me2.depth <- c(115)

sum(me2.clay*me2.areas)
sum(me2.sand*me2.areas)
sum(me2.depth*me2.areas)
# --------------------------

# --------------------------
# Wind River (Wrc) 45.8205, -121.9519 -- had to go north of site for data availability
# --------------------------
wrc.areas <- c(0.064, 0.717, 0.079, 0.012, 0.110); wrc.areas <- wrc.areas/sum(wrc.areas)
wrc.clay <- c(0.072, 0.128, 0.128, 0.112, 0.112)
wrc.sand <- c(0.650, 0.517, 0.517, 0.489, 0.489)
wrc.depth <- c(1.14, 2.00, 2.00, 2.00, 2.00)

sum(wrc.clay*wrc.areas)
sum(wrc.sand*wrc.areas)
sum(wrc.depth*wrc.areas)
# --------------------------

# --------------------------
# Morgan Monroe
# --------------------------
mms.areas <- c(0.274, 0.645, 0.080); mms.areas <- mms.areas/sum(mms.areas)
mms.clay <- c(0.383, 0.135, 0.216)
mms.sand <- c(0.060, 0.199, 0.272)
mms.depth <- c(0.61, 0.91, 1.37)

sum(mms.clay*mms.areas)
sum(mms.sand*mms.areas)
sum(mms.depth*mms.areas)
# --------------------------

# --------------------------
# UMBS
# --------------------------
umbs.areas <- c(0.517, 0.242, 0.094, 0.041, 0.106); umbs.areas <- umbs.areas/sum(umbs.areas)
umbs.clay <- c(0.013, 0.088, 0.088, 0.118, 0.118)
umbs.sand <- c(0.959, 0.882, 0.882, 0.726, 0.726)
umbs.depth <- c(2.00, 2.00, 2.00, 0.71, 0.71)

sum(umbs.clay*umbs.areas)
sum(umbs.sand*umbs.areas)
sum(umbs.depth*umbs.areas)

# --------------------------

# --------------------------
# Sylvania
# --------------------------
syl.areas <- c(27.6, 18.0, 19.1, 16.1); syl.areas <- syl.areas/sum(syl.areas)
syl.clay <- c(0.038, 0.032, 0.010, 0.010)
syl.sand <- c(0.616, 0.675, 0.906, 0.906)
syl.depth <- c(0.51, 0.51, 2.00, 2.00)

sum(syl.clay*syl.areas)
sum(syl.sand*syl.areas)
sum(syl.depth*syl.areas)

# --------------------------
