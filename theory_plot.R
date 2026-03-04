library(reticulate)

py_run_string("
import matplotlib.pyplot as plt
import numpy as np

# === Data Generation for Two Groups (A and B) ===

N = 1000
x = np.linspace(0, 10, N)

# Group A: lower alpha and beta, decreasing heteroskedasticity
alpha_a = 1
beta_a = 0.2
gamma_a = 0.5
delta_a = -0.1
mu_a = alpha_a + beta_a * x
sigma_a = np.exp(gamma_a + delta_a * x)
y_upper_a = mu_a + sigma_a
y_lower_a = mu_a - sigma_a
line_45 = x

# Group B: higher alpha and beta, increasing heteroskedasticity
alpha_b = 3
beta_b = 0.5
gamma_b = -1.5
delta_b = 0.2
mu_b = alpha_b + beta_b * x
sigma_b = np.exp(gamma_b + delta_b * x)
y_upper_b = mu_b + sigma_b
y_lower_b = mu_b - sigma_b

# === Plotting ===

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 8), sharex=True, sharey=True)

for ax, mu, y_lower, y_upper, alpha, beta, sigma, label, suffix in zip(
    [ax1, ax2],
    [mu_a, mu_b],
    [y_lower_a, y_lower_b],
    [y_upper_a, y_upper_b],
    [alpha_a_updated, alpha_b],
    [beta_a, beta_b],
    [sigma_a, sigma_b],
    ['Group A', 'Group B'],
    ['a', 'b']
):
    # Use fewer hachure lines for spacing effect by reducing fill_between calls
    ax.fill_between(x[::2], line_45[::2], 10, where=(line_45[::2] < 10),
                    hatch='|', facecolor='none', edgecolor='lightgray', linewidth=0.5, zorder=0)
    ax.fill_between(x[::2], 0, line_45[::2], where=(line_45[::2] > 0),
                    hatch='-', facecolor='none', edgecolor='lightgray', linewidth=0.5, zorder=0)

    ax.fill_between(x, y_lower, y_upper, color='gray', alpha=0.3, zorder=1)
    ax.plot(x, mu, color='black', linewidth=2, zorder=2)
    ax.plot(x, line_45, linestyle='dotted', color='black', linewidth=1, zorder=1)

    ax.text(0.5, alpha + 0.7, fr'$\alpha_{{{suffix}}}$', fontsize=22, fontweight='bold')
    ax.annotate(fr'$\beta_{{{suffix}}}$',
                xy=(6, mu[600]), xytext=(6.5, mu[600] - 1),
                arrowprops=dict(arrowstyle="->", lw=1), fontsize=22, fontweight='bold')
    ax.annotate(fr'$\sigma_{{{suffix}}}(x)$',
                xy=(7, y_upper[700]), xytext=(6.5, y_upper[700] + 1),
                arrowprops=dict(arrowstyle="->", lw=1), fontsize=22, fontweight='bold')

    ax.text(0.5, 9.5, "Upward absolute mobility", fontsize=20, fontweight='bold', ha='left')
    ax.text(3.1, 0.5, "Downward absolute mobility", fontsize=20, fontweight='bold', ha='left')

    ax.set_xlim(0, 10)
    ax.set_ylim(0, 10)
    ax.set_xlabel("origins (x)", fontsize=20)
    ax.set_ylabel("destination (y)", fontsize=20)
    ax.set_title(label, fontsize=22, fontweight='bold')
    ax.set_aspect('equal')
    ax.spines['top'].set_visible(True)
    ax.spines['right'].set_visible(True)
    ax.grid(False)

plt.tight_layout()
plt.show()

")
