# Load required packages
library(yaml)
library(writexl)  # for optional Excel export

# Read constants and parameters from the YAML file
params <- yaml::read_yaml("ConstLDC.yaml")
const_list <- params$constants  # list of c01..c57
# Convert constants to a numeric vector indexed by constant number
c_names <- names(const_list)
c_index <- order(as.numeric(sub("c", "", c_names)))
c_vals <- as.numeric(const_list[c_index])
# For convenience, use 'const' as the numeric vector of constants (1-indexed: const[1] = c1, etc.)
const <- c_vals

# Extract other parameters
ss_values <- params$ss         # site index values for boniteit classes
nt <- params$nt               # maximum age for the yield table
nb <- params$nb               # number of boniteit classes

# Define descriptive names for thinning categories 1–6
category_labels <- c("No thinning", "Light thinning", "Moderate thinning",
                     "Strong thinning", "Very strong thinning", "Open stand")

# Initialize a list to collect scenario data frames
results_list <- list()

# Loop over thinning categories (ktgr) and boniteit classes (kbon)
for (ktgr in 1:6) {
  for (kbon in 1:nb) {
    # Site parameter for this boniteit (e.g., dominant height at reference age 70)
    s <- ss_values[kbon]

    # **Height curve calculation (subroutine HTOPT equivalent)**:contentReference[oaicite:4]{index=4}:contentReference[oaicite:5]{index=5}
    c1 <- const[1]
    c2 <- const[2] - const[3] * s
    c4 <- const[4]
    # Calculate reference heights and times
    h70 <- s * (1 - exp(-c1 * 70))^c2                     # expected top height at age 70
    t7  <- log(1 - (7.0 / s)^(1 / c2)) / -c1              # age at which height = 7.0 m
    ak  <- c1 * c2 * (s / 7.0) * exp(-c1 * t7) * (1 - exp(-c1 * t7))^(c2 - 1) /
           (c4 * t7^(-c4 - 1))                            # growth rate parameter for juvenile phase
    t130 <- ((t7^(-c4) - log(1.3/7.0) / ak))^(-1 / c4)    # age at which height = 1.3 m (breast height)
    # Generate height trajectory ht[1..(nt+2)] for ages 1 through nt+2
    ht <- numeric(nt + 2)
    for (i in 1:(nt + 2)) {
      t_age <- as.double(i)
      # Juvenile phase height (if stand height < 7 m) and normal phase height
      hj <- 7.0 * exp(-ak * t_age^(-c4)) / exp(-ak * t7^(-c4))
      ho <- s * (1 - exp(-c1 * t_age))^c2
      ht[i] <- if (ho > 7.0) ho else hj  # use juvenile curve until height 7, then switch to main curve
    }

    # **Initialize state variables for this scenario (subroutine PERK setup)**:contentReference[oaicite:6]{index=6}:contentReference[oaicite:7]{index=7}
    pi_val <- pi
    tgr <- as.double(ktgr)                       # thinning intensity index as double
    sper <- 3.0 * tgr + 10.0                     # target spacing (% of height * 100)
    # Dunning parameters: special case for no thinning vs others:contentReference[oaicite:8]{index=8}
    if (sper == 13.0) {  
      c19_eff <- -0.0353472    # override c19 for no-thinning scenario:contentReference[oaicite:9]{index=9}
      itmin   <- 30
    } else {
      c19_eff <- const[19]
      itmin   <- 50
    }
    fnv <- const[45]                            # initial number of stems per ha
    if (ktgr == 6) fnv <- 0.6 * const[45]       # open stand starts with 60% of normal stems:contentReference[oaicite:10]{index=10}
    fn0 <- fnv                                  
    fnn <- fnv                                 # fnn = current number of stems (after thinning)
    # Pre-calculate values at threshold height 7 m
    fff <- 5000.0
    str7 <- 10746.0 / (21 * sqrt(fn0)) - 10746.0 / (21 * sqrt(fff))
    d7   <- const[13] * (1 + const[14] * str7)  # diameter at 7 m top height:contentReference[oaicite:11]{index=11}
    g7   <- fnv * pi_val * (d7 / 200.0)^2       # basal area at threshold (all trees)
    # Initialize accumulators and other variables
    sgd <- 0.0    # cumulative basal area removed (m^2/ha)
    svd <- 0.0    # cumulative volume removed (m^3/ha)
    tgrold <- tgr  # previous cycle's growth index
    jeugd  <- 0    # flag for juvenile phase (0 until stand height reaches 7 m)

    # Prepare matrix for yearly values (rows 1..nt+1, columns 1..26):contentReference[oaicite:12]{index=12}
    x <- matrix(0.0, nrow = nt + 1, ncol = 26)

    # **Year 1 initialization**:contentReference[oaicite:13]{index=13}
    i <- 1
    t_year <- 1.0
    h <- ht[1]
    hdom <- h - const[24] * (h^const[25])        # initial "dominant height" (reduced top height)
    # Compute initial spacing index (relative spacing %):contentReference[oaicite:14]{index=14}
    sp_index <- 100 * 107.457 / (h * sqrt(fnn))
    # Set initial heights of removed and remaining fractions (no thinning yet, use 0.8 * h):contentReference[oaicite:15]{index=15}
    hgv <- h * const[49]   # c49 = 0.8
    hgn <- h * const[49]
    # Fill in known columns for year 1
    x[1, 1]  <- t_year    # Age
    x[1, 2]  <- h         # Top height
    x[1, 3]  <- hdom      # "Dominant" or mean height
    x[1, 5]  <- sp_index  # Relative spacing (%)
    x[1, 6]  <- fnv       # Stems/ha before thinning
    x[1, 9]  <- hgv       # Height of removed trees (none removed, so =0.8*h)
    x[1, 15] <- fnn       # Stems/ha after thinning (same as initial)
    x[1, 18] <- hgn       # Height of remaining trees (≈0.8*h)
    x[1, 26] <- t_year    # Using age as a placeholder for total volume at age 1 (negligible volume)
    # (Basal area and volume columns remain 0 at age 1)

    # **Yearly simulation loop (ages 2 through nt+1)** 
    for (i in 2:(nt + 1)) {
      t_year <- as.double(i)
      # Carry over surviving stems and basal area from previous year:contentReference[oaicite:16]{index=16}
      fnv <- fnn            # stems at start of year = stems after last year's thinning
      gv_prev <- x[i-1, 16] # basal area at start = basal area remaining last year (gn from prev year)
      # Current year heights
      h  <- ht[i]
      h1 <- h               # current top height
      h2 <- ht[i + 1]       # next year's top height (for increment calculations)
      # Update juvenile phase flag if threshold height is reached
      if (h >= const[50]) jeugd <- 1  # c50 = 7.0, mark end of juvenile phase:contentReference[oaicite:17]{index=17}

      # Determine if a thinning is scheduled this year
      kpr <- if (i %% 5 == 0) 1 else 0       # kpr = 1 if year is multiple of 5 (thinning interval):contentReference[oaicite:18]{index=18}

      # Adjust growth rate for thinning (subroutine DUNNING):contentReference[oaicite:19]{index=19}
      tgrnew <- tgrold
      if (ht[i] > 7.0 && kpr == 1) {
        # Calculate new spacing factor spern and adjusted growth index tgrnew:contentReference[oaicite:20]{index=20}
        if (i <= itmin) {
          spern <- sper 
        } else {
          spern <- sper + c19_eff * (i - itmin)
        }
        tgrnew <- (spern - 10.0) / 3.0
        if (tgrnew >= 6.5) tgrnew <- 6.5
        if (tgrnew <= 0.5) tgrnew <- 0.5
      } else if (ht[i] > 7.0 && kpr == 0) {
        # Between thinning years in adult phase: keep previous growth index
        tgrnew <- tgrold
      }
      # Note: if ht[i] <= 7.0 (juvenile phase), tgrnew remains tgrold

      # Compute "dominant height" hdom based on current density:contentReference[oaicite:21]{index=21}
      # (hdom ~ h reduced by a factor if stand is dense)
      if (fnn <= 100) {
        hdom <- h 
      } else if (fnn <= 250) {
        hdom <- (h - const[24] * h^const[25]) * ((fnn - 100) / 150) + 
                h * ((250 - fnn) / 150)
      } else {
        hdom <- h - const[24] * (h^const[25])
      }

      # Initialize variables for this year
      dv <- 0.0; dn <- 0.0   # quadratic mean diameters (gross and net)
      gv_gross <- 0.0        # gross basal area before thinning
      gd <- 0.0; dd <- 0.0   # basal area removed, and mean diam of removed trees
      fnd <- 0.0             # number of stems removed
      vv <- 0.0; vn <- 0.0; vd <- 0.0  # volumes (gross, net, removed)
      tgrcor <- 1.0; deltaG <- 0.0

      if (h <= 1.3) {
        # **Establishment phase (stand height <= 1.3 m)**: no significant growth yet
        dn <- 0.0
        dv <- 0.0
        # No basal area growth or thinning; carry over values (gn remains gv_prev, which is ~0)
        gv_gross <- gv_prev
        gn_new   <- gv_prev
        fnn      <- fnv    # no change in stem count
        # Heights of removed/remaining (no removal, use juvenile approximation)
        hgv <- h * 0.8
        hgn <- h * 0.8
        # No increment in basal area
        deltaG <- 0.0
        gd <- 0.0; fnd <- 0.0; dd <- 0.0; vd <- 0.0
        vn <- 0.0; vv <- 0.0
      } else if (jeugd == 0) {
        # **Juvenile growth phase (stand height < 7 m, no thinning yet)**
        if (h <= 7.0) {
          # Below threshold (no thinning, natural mortality model):contentReference[oaicite:22]{index=22}:contentReference[oaicite:23]{index=23}
          # Estimate diameter corresponding to current height in juvenile phase
          dv <- d7 * exp(-const[11] * exp(-const[12] * (h - 1.3))) / 
                    exp(-const[11] * exp(-const[12] * (7.0 - 1.3)))
          gv_gross <- fnv * pi_val * (dv / 200.0)^2   # basal area if no thinning
          gn_new   <- gv_gross                       # gn = gv (no removal):contentReference[oaicite:24]{index=24}
          dn <- dv                                   # QMD of stand (all trees)
          # Basal area increment calculation:contentReference[oaicite:25]{index=25}
          if (tgrnew <= const[54]) {                 # const[54] = dgrmin
            tgrcor <- 1.0 
          } else {
            tgrcor <- 1.0 - const[6] * (tgrnew - const[54])^const[7]
          }
          hmadj <- const[53] - h1                    # hmin1 = const[53], adjust for height deficit
          if (hmadj <= 0) hmadj <- 0.0
          c8_val <- const[8] + const[9] * sqrt(hmadj)
          dt7 <- (i + 1 - t7)                        # time interval until threshold (if crosses within a year)
          termh <- 0.0
          if (h2 > 7.0) {
            termh <- ((h2 - 1.3)^c8_val - (7.0 - 1.3)^c8_val) / dt7
          }
          if (h2 > 7.0) {
            deltaG <- dt7 * const[52] * tgrcor * (const[10] + const[5] * termh)  # c52 = cf80:contentReference[oaicite:26]{index=26}
          } else {
            deltaG <- 0.0
          }
          # No thinning removal in this phase
          fnd <- 0.0; gd <- 0.0; dd <- 0.0
          fnn <- fnv    # no change in stem count due to thinning (only natural mortality modeled, which here leaves fnn = fnv)
          # (fnn remains the same; any actual mortality would be reflected in fnv if model reduced it, but here fnv->fnn unchanged)
          # Height of remaining = h (no thinning, all trees remain), removed = none
          # (hgn/hgv will be computed later)
          gn_new <- gv_gross
        } else {
          # **Crossing the threshold (h just exceeded 7 m)**:contentReference[oaicite:27]{index=27}:contentReference[oaicite:28]{index=28}
          # Incorporate last year's basal area and ensure continuity at threshold
          deltaG <- const[52] * (if (tgrnew <= const[54]) 1.0 else (1.0 - const[6] * (tgrnew - const[54])^const[7])) *
                    (const[10] + const[5] * ((h2 - 1.3)^(const[8] + const[9]*sqrt(max(const[53] - h1, 0))) - (h1 - 1.3)^(const[8] + const[9]*sqrt(max(const[53] - h1, 0)))))
          # ^ (Above, we combined steps for tgrcor, hmadj, c8, termh, deltaG for brevity)
          # Compute gross basal area including this year's growth
          gv_gross <- gv_prev + deltaG
          # If previous year was below 7 m and now above, use g7 to avoid discontinuity:contentReference[oaicite:29]{index=29}
          if (ht[i-1] < 7.0) {
            gv_gross <- g7 + deltaG 
          }
          gn_new <- gv_gross   # no thinning removal at the moment of crossing
          dn <- 200.0 * sqrt(gv_gross / (fnv * pi_val))  # QMD of stand (all trees)
          # No thinning this year (though kpr might be 1, we skip removal at the threshold crossing)
          fnd <- 0.0; gd <- 0.0; dd <- 0.0
          fnn <- fnv           # no change in stem count
        }
        # After juvenile phase calculations, mark that stand is now in adult phase if height >= 7
        if (h >= 7.0) jeugd <- 1
      } else {
        # **Adult phase (stand height >= 7 m, thinning may occur)**
        # Basal area growth for this year:contentReference[oaicite:30]{index=30}:contentReference[oaicite:31]{index=31}
        if (tgrnew <= const[54]) {
          tgrcor <- 1.0 
        } else {
          tgrcor <- 1.0 - const[6] * (tgrnew - const[54])^const[7]
        }
        hmadj <- const[53] - h1
        if (hmadj <= 0) hmadj <- 0.0
        c8_val <- const[8] + const[9] * sqrt(hmadj)
        # Height-based term for basal area increment:contentReference[oaicite:32]{index=32}
        termh <- (h2 - 1.3)^c8_val - (h1 - 1.3)^c8_val
        deltaG <- const[52] * tgrcor * (const[10] + const[5] * termh)
        # Update gross basal area with growth
        gv_gross <- gv_prev + deltaG                      # basal area before thinning:contentReference[oaicite:33]{index=33}
        # By default, assume no thinning (gn = gv); adjust if thinning occurs
        gn_new <- gv_gross
        # Compute stand QMD before thinning
        dv <- 200.0 * sqrt(gv_gross / (fnv * pi_val))
        dn <- dv  # will update if thinning changes stems
        # **Thinning event**: apply removal if kpr == 1
        if (kpr == 1) {
          # Calculate target spacing and diameter after thinning:contentReference[oaicite:34]{index=34}:contentReference[oaicite:35]{index=35}
          akv <- 107.457 / sqrt(fnv)               # current spacing factor before thinning
          akn_target <- h * (spern / 100.0)        # target average spacing (= h * spern/100)
          # Compute target diameter factor c20:contentReference[oaicite:36]{index=36}
          c20 <- const[20] + const[21] * h70 + const[22] * sqrt(tgrnew) + const[23] * t_year
          # Predict post-thinning quadratic diameter and stem count
          dn_target <- dv * (c20 * (akn_target / akv) + 1 - c20)    # target QMD after thinning
          fnn_pred <- (107.457 / akn_target)^2                     # predicted remaining stems
          if (fnn_pred > fnv) {
            # If target stems exceed current stems, skip thinning (no removal):contentReference[oaicite:37]{index=37}
            fnn <- fnv
            gn_new <- gv_gross
            dn <- dv
            gd <- 0.0; fnd <- 0.0; dd <- 0.0
          } else {
            # Thinning occurs: compute actual removals
            fnn <- fnn_pred                          # new number of stems after thinning
            fnd <- fnv - fnn                         # number removed:contentReference[oaicite:38]{index=38}
            # Recalculate c20 with final values (as in FORTRAN code) and final post-thin QMD
            c20 <- const[20] + const[21] * h70 + const[22] * sqrt(tgrnew) + const[23] * t_year
            dn <- dv * (c20 * (akn_target / akv) + 1 - c20)   # actual QMD of remaining stand after thinning
            # Basal areas after and removed
            gn_new <- fnn * pi_val * (dn / 200.0)^2          # basal area remaining (Gn):contentReference[oaicite:39]{index=39}
            gd <- gv_gross - gn_new                         # basal area removed
            # Average diameter of removed trees:contentReference[oaicite:40]{index=40}
            if (gd > 0 && fnd > 0) {
              dd <- 200.0 * sqrt(gd / (fnd * pi_val))
            } else {
              dd <- 0.0
            }
          }
        } else {
          # No thinning this year
          fnn <- fnv     # stem count remains unchanged
          fnd <- 0.0
          gd  <- 0.0
          dd  <- 0.0
        }
      }  # end of adult phase

      # **Post-calculation for all phases: compute tree heights and volumes** 
      # Calculate heights of remaining (hgn) and removed (hgv) fractions:contentReference[oaicite:41]{index=41}:contentReference[oaicite:42]{index=42}
      # termtgr: additional term based on growth index for height adjustment
      termtgr <- 0.0
      if (tgrnew <= 6.5) {
        termtgr <- (6.5 - tgrnew)^2
      }
      # Average height of remaining trees (hgn):contentReference[oaicite:43]{index=43}
      hgn <- h - (const[32] + const[33] * termtgr + const[34] * h70) / (h^const[35])
      hg0 <- const[49] * h            # 0.8 * h
      if (hgn <= hg0) {
        hgn <- hg0    # ensure remaining height >= 0.8 * top height:contentReference[oaicite:44]{index=44}
      }
      # Average height of removed trees (hgv)
      hgv <- hgn
      if (kpr == 1 && fnd > 0) {
        hgv <- const[36] * hgn   # slightly lower than hgn if thinning occurred:contentReference[oaicite:45]{index=45}
      }

      # Volume calculations (per hectare) for gross, net, and removed wood:contentReference[oaicite:46]{index=46}:contentReference[oaicite:47]{index=47}
      # First, use volume allometric equations
      vv <- fnv * exp(const[39]) * (dv^const[37]) * (hgv^const[38]) / 1000.0   # gross volume before thinning
      vn <- fnn * exp(const[39]) * (dn^const[37]) * (hgn^const[38]) / 1000.0   # net volume after thinning
      vd <- vv - vn                                                          # volume removed this year
      # If the volume formula gives non-physical result (vd < 0), use an alternative model:contentReference[oaicite:48]{index=48}:contentReference[oaicite:49]{index=49}
      if (vd < 0) {
        vv <- gv_gross * (h^(const[40] + const[41] * t_year)) * exp(const[42] + const[43] * t_year)
        vn <- gn_new  * (h^(const[40] + const[41] * t_year)) * exp(const[42] + const[43] * t_year)
        vd <- vv - vn
      }

      # Compute dominant diameter (ddom) of the stand:contentReference[oaicite:50]{index=50}
      t0 <- t_year - t130
      if (h <= 1.3 || t0 <= 0) {
        ddom <- 0.0
      } else {
        # Weibull distribution parameters for diameter
        term1 <- (dn / const[28])^(const[29] - 1) * exp(- (dn / const[28])^const[29])
        ddom <- dn + (const[31] + const[26] * (h70^const[27]) * term1) * (1 - const[30] * tgrnew)
      }

      # **Update accumulators for total removed basal area and volume**
      sgd <- sgd + gd   # add this year's removed basal area to cumulative:contentReference[oaicite:51]{index=51}
      svd <- svd + vd   # add this year's removed volume to cumulative
      # Compute total accumulated basal area and volume produced to date
      Gtot <- gn_new + sgd   # living basal area + all removed = total produced:contentReference[oaicite:52]{index=52}
      Vtot <- vn + svd       # living volume + removed volume = total produced

      # **Store results for this year** (match columns to FORTRAN output):contentReference[oaicite:53]{index=53}:contentReference[oaicite:54]{index=54}
      x[i, 1]  <- t_year       # Age
      x[i, 2]  <- h            # Top height (m)
      x[i, 3]  <- hdom         # Mean stand height (m)
      x[i, 4]  <- ddom         # Dominant diameter (cm)
      x[i, 5]  <- 100 * 107.457 / (h * sqrt(fnn))  # Relative spacing (%):contentReference[oaicite:55]{index=55}
      x[i, 6]  <- fnv          # Stems/ha before thinning
      x[i, 7]  <- gv_gross     # Basal area gross (m^2/ha) before thinning
      x[i, 8]  <- dv           # QMD gross (cm) before thinning
      x[i, 9]  <- hgv          # Mean height of removed trees (m)
      x[i, 10] <- vv           # Volume gross (m^3/ha) before thinning
      x[i, 11] <- fnd          # Stems removed in thinning
      x[i, 12] <- gd           # Basal area removed (m^2/ha)
      x[i, 13] <- dd           # QMD of removed trees (cm)
      x[i, 14] <- vd           # Volume removed (m^3/ha)
      x[i, 15] <- fnn          # Stems/ha after thinning
      x[i, 16] <- gn_new       # Basal area net (m^2/ha) after thinning
      x[i, 17] <- dn           # QMD net (cm) after thinning
      x[i, 18] <- hgn          # Mean height of remaining trees (m)
      x[i, 19] <- vn           # Volume net (m^3/ha) after thinning
      x[i, 24] <- t_year       # Age (duplicated as integer field for output alignment)
      x[i, 25] <- Gtot         # Total basal area produced (m^2/ha) to date
      x[i, 26] <- Vtot         # Total volume produced (m^3/ha) to date

      # Prepare for next year
      tgrold <- tgrnew   # update growth index for next iteration
    }  # end yearly loop

    # **Compute annual increments for basal area and volume**:contentReference[oaicite:56]{index=56}:contentReference[oaicite:57]{index=57}
    # Use central differences to approximate current annual increment (CAI) at each year
    for (i in 2:nt) {
      x[i, 20] <- (x[i+1, 25] - x[i-1, 25]) / 2.0   # CAI of basal area (m^2/ha/yr)
      x[i, 21] <- if (x[i, 1] > 0) x[i, 25] / x[i, 1] else NA  # MAI of basal area (m^2/ha/yr)
      x[i, 22] <- (x[i+1, 26] - x[i-1, 26]) / 2.0   # CAI of volume (m^3/ha/yr)
      x[i, 23] <- if (x[i, 1] > 0) x[i, 26] / x[i, 1] else NA  # MAI of volume (m^3/ha/yr)
    }

    # **Extract the 5-year interval table (LD_C1 yield table)**:contentReference[oaicite:58]{index=58}
    # Determine last age to output based on category (no thinning and open stand end at 70, light thinning at 90, others at 120):contentReference[oaicite:59]{index=59}
    if (ktgr == 1 || ktgr == 6) {
      max_age <- min(nt, 70)
    } else if (ktgr == 2) {
      max_age <- min(nt, 90)
    } else {
      max_age <- nt
    }
    output_ages <- seq(5, max_age, by = 5)  # 5-year steps

    # Create a data frame for this scenario
    scenario_df <- data.frame(
      Category  = factor(category_labels[ktgr], levels = category_labels),
      Boniteit  = kbon,
      Age       = output_ages,
      TopHeight_m       = x[output_ages, 2],
      MeanHeight_m      = x[output_ages, 3],
      DominantDiam_cm   = x[output_ages, 4],
      RelSpacing_pct    = x[output_ages, 5],
      Stems_before_ha   = x[output_ages, 6],
      BasalArea_gross_m2ha = x[output_ages, 7],
      QMD_gross_cm      = x[output_ages, 8],
      Height_removed_m  = x[output_ages, 9],
      Volume_gross_m3ha = x[output_ages, 10],
      Stems_removed_ha  = x[output_ages, 11],
      BasalArea_removed_m2ha = x[output_ages, 12],
      QMD_removed_cm    = x[output_ages, 13],
      Volume_removed_m3ha = x[output_ages, 14],
      Stems_after_ha    = x[output_ages, 15],
      BasalArea_net_m2ha   = x[output_ages, 16],
      QMD_net_cm        = x[output_ages, 17],
      Height_net_m      = x[output_ages, 18],
      Volume_net_m3ha   = x[output_ages, 19],
      BasalArea_CAI_m2ha_yr = x[output_ages, 20],
      BasalArea_MAI_m2ha_yr = x[output_ages, 21],
      Volume_CAI_m3ha_yr    = x[output_ages, 22],
      Volume_MAI_m3ha_yr    = x[output_ages, 23]
    )
    results_list[[paste(ktgr, kbon, sep = "_")]] <- scenario_df
  }
}

# Combine all scenario results into one data frame
ldc1_table <- do.call(rbind, results_list)

# View the first rows of the resulting data frame
head(ldc1_table, 10)

# Optionally, write the data frame to an Excel file
write_excel <- TRUE
if (write_excel) {
  write_xlsx(ldc1_table, "LD_C1_table.xlsx")
}

