library(ggplot2)
library(Rcpp)
library(camcorder)

gg_record(here::here("artist-temp"), width = 8, height = 8, dpi = 320)

set.seed(99)

# --- parameters ---
n_steps           <- 1500
step_length       <- 1.5
wobble_sd         <- 0.01      # radians, angular noise per step
branch_prob       <- 0.02      # chance a tip branches each step
branch_angle_min  <- pi/4      # 45 degrees
branch_angle_max  <- pi/2      # 90 degrees
min_dist          <- 0.9       # collision threshold

# --- simulation (C++) ---
cppFunction(includes = "#include <unordered_map>", code = '
NumericMatrix grow_filaments(int n_steps, double step_length, double wobble_sd,
                             double branch_prob, double branch_angle_min,
                             double branch_angle_max, double min_dist) {
  const double md2  = min_dist * min_dist;
  const double cell = min_dist;

  std::vector<double> tip_x(1, 0.0), tip_y(1, 0.0), tip_a(1, 0.0);
  std::vector<int> tip_gen(1, 0);   // branch generation (root = 0)
  std::vector<int> tip_fil(1, 0);   // filament id (each branch child gets a new one)
  int next_fil = 1;
  std::vector<double> x0, y0, x1, y1;                    // segments
  std::vector<int> seg_step, seg_gen, seg_fil;
  std::vector<double> ep_x, ep_y;                        // endpoints
  std::unordered_map<long long, std::vector<int>> grid;  // cell -> endpoint ids

  auto cell_of = [cell](double v) { return (long long) std::floor(v / cell); };
  auto key_of  = [](long long cx, long long cy) { return cx * 73856093LL + cy; };

  for (int step = 0; step < n_steps; ++step) {
    int n_tips = tip_x.size();
    if (n_tips == 0) break;

    std::vector<double> nx_t, ny_t, na_t;   // next generation of tips
    std::vector<int> ng_t, nf_t;
    std::vector<double> pend_x, pend_y;     // endpoints laid down this step

    for (int i = 0; i < n_tips; ++i) {
      double a  = tip_a[i] + R::rnorm(0.0, wobble_sd);
      double nx = tip_x[i] + step_length * std::cos(a);
      double ny = tip_y[i] + step_length * std::sin(a);

      bool hit = false;
      long long cx = cell_of(nx), cy = cell_of(ny);
      for (long long dx = -1; dx <= 1 && !hit; ++dx) {
        for (long long dy = -1; dy <= 1 && !hit; ++dy) {
          auto it = grid.find(key_of(cx + dx, cy + dy));
          if (it == grid.end()) continue;
          for (int id : it->second) {
            double ddx = ep_x[id] - nx, ddy = ep_y[id] - ny;
            if (ddx * ddx + ddy * ddy < md2) { hit = true; break; }
          }
        }
      }
      if (hit) continue;  // tip stops growing

      x0.push_back(tip_x[i]); y0.push_back(tip_y[i]);
      x1.push_back(nx);       y1.push_back(ny);
      seg_step.push_back(step + 1);
      seg_gen.push_back(tip_gen[i]);
      seg_fil.push_back(tip_fil[i]);
      pend_x.push_back(nx);   pend_y.push_back(ny);
      nx_t.push_back(nx); ny_t.push_back(ny); na_t.push_back(a);
      ng_t.push_back(tip_gen[i]); nf_t.push_back(tip_fil[i]);

      if (R::unif_rand() < branch_prob) {
        double sgn    = (R::unif_rand() < 0.5) ? -1.0 : 1.0;
        double offset = R::runif(branch_angle_min, branch_angle_max) * sgn;
        nx_t.push_back(nx); ny_t.push_back(ny); na_t.push_back(a + offset);
        ng_t.push_back(tip_gen[i] + 1); nf_t.push_back(next_fil++);
      }
    }

    for (size_t j = 0; j < pend_x.size(); ++j) {
      ep_x.push_back(pend_x[j]); ep_y.push_back(pend_y[j]);
      grid[key_of(cell_of(pend_x[j]), cell_of(pend_y[j]))].push_back(ep_x.size() - 1);
    }
    tip_x = std::move(nx_t); tip_y = std::move(ny_t); tip_a = std::move(na_t);
    tip_gen = std::move(ng_t); tip_fil = std::move(nf_t);
  }

  int n = x0.size();
  NumericMatrix out(n, 7);
  for (int i = 0; i < n; ++i) {
    out(i, 0) = x0[i]; out(i, 1) = y0[i];
    out(i, 2) = x1[i]; out(i, 3) = y1[i];
    out(i, 4) = seg_step[i]; out(i, 5) = seg_gen[i]; out(i, 6) = seg_fil[i];
  }
  colnames(out) = CharacterVector::create("x0", "y0", "x1", "y1",
                                          "step", "generation", "filament");
  return out;
}')

segments <- as.data.frame(grow_filaments(n_steps, step_length, wobble_sd,
                                         branch_prob, branch_angle_min,
                                         branch_angle_max, min_dist))

# --- plot ---
ggplot(segments) +
  geom_segment(aes(x = x0, y = y0, xend = x1, yend = y1, linewidth = generation), color = "grey95", lineend = "round") +
  scale_linewidth_continuous(range = c(0.1, 1.3)) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#2E2E2E", color = NA)
  )
