library(yaml)
library(openxlsx)
library(dplyr)

# Set parameters
t_max <- 100    # max age in yield table
S_i <- 36.1377  # Site index, selected from table 5 in Jansen et al, p36, top height 36.1377=III
tgr_0 <- 2      # Select from table 2. 2=zwakke laagdunning
N_0 <- 5000

# Load constants
param <- yaml::read_yaml(system.file("parameters.yaml", package = "douglasJansen"))

# Output storage
yield_table <- data.frame(
  t = integer(),
  s_percent = numeric(),
  h_top = numeric(),
  h_dom = numeric(),
  n_bt = numeric(),
  d_bt = numeric(),
  g_bt = numeric(),
  n_at = numeric(),
  g_at = numeric(),
  hg_at = numeric(),
  ic_g = numeric()
)

# set_counters
g_at <- 0  # set to zero initially
ic_g <- 0
im_g <- 0
#ic_v <- 0
im_v <- 0

#print(param$c1)
t7 <- with(param, calculate_t7(S_i, c1, c2, c3))
cat("t7\t", t7, "\n")

# TODO CHECK of nodig
# initialize variable.
tgrnew <-0

for (t in 1:(t_max + 1)) {

  # misschien moet ik s_percent_current en S-Percent_goal ofzo gaan gebruiken?
  #s_percent <- with(param, calculate_s_percent_(t, tgr_0, c19))
  s_percent <- with(param, calculate_tgr(t, tgr_0, c19))
  h1 <- with(param, calculate_h_top(t, S_i, c1, c2, c3, c4))
  h2 <- with(param, calculate_h_top(t+1, S_i, c1, c2, c3, c4))
  h_top <- h1

  #if (tgrnew <= const[54]) {                 # const[54] = dgrmin
  #  tgrcor <- 1.0
  #} else {
  #  tgrcor <- 1.0 - const[6] * (tgrnew - const[54])^const[7]
  #}


  n_bt <- if (t > 1) n_at else N_0

  # Cycle h_top < 7, if t is smaller then t7, then h_top below 7 ---------------
  if (t < floor(t7)) {

    n_at <- n_bt
    d_bt <- with(param, calculate_d_bt(h_top=h_top, N_0, c11, c12, c13, c14))
    g_bt <- calculate_g(N=N_0, d=d_bt)
    g_i <- g_bt - g_at
    g_at <- g_bt

  # TODO CHECK moet misschien net FLOOR zijn
  # Cycle h_top passes 7 in thuis year ------------- ----------------------------
  } else if (t == floor(t7)) {
    n_at <- n_bt
    ic_g <- with(param, calculate_ic_g_t7(t=t, h_top_t1=h1, h_top_t2=h2, tgr_0, c=param))


  # h_top > 7, thinning possible -----------------------------------------------
  } else {
    n_at <- n_bt
    ic_g <- with(param, calculate_ic_g(h_top_t1=h1, h_top_t2=h2, dt=1, tgr_0=tgr_0, x80=1, x_HD=0, c=param))
  }

  h_dom <- with(param, calculate_h_dom(h_top=h_top, N_at=n_at, c24, c25))
  hg_at <- h_dom*0.8

  # Save to yield table
  new_row <- data.frame(
    t = t,
    s_percent = s_percent,
    h_top = h_top,
    h_dom = h_dom,
    n_bt = n_bt,
    d_bt = d_bt,
    g_bt = g_bt,
    n_at = n_at,
    g_at = g_at,
    hg_at = hg_at,
    ic_g = ic_g

  )

  # Append the row
  yield_table <- bind_rows(yield_table, new_row)
}


# Create output dir and write
out_path <- file.path("out", "yield_output.xlsx")
if (!dir.exists("out")) dir.create("out")
openxlsx::write.xlsx(yield_table, out_path)
message("Saved to: ", out_path)
