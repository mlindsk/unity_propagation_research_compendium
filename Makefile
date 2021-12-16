R=Rscript
P=prediction
M=message
T=triang
G=graph

# ------------------------------------------
all: install_sparta install_jti install_others make_graphs make_prediction make_message make_triang make_cpt_sparsities
install_libraries: install_sparta install_jti install_others
make_benchmarks: make_prediction make_message make_triang
# ------------------------------------------

# ------------------------------------------
# INSTALL LIBRARIES
# ------------------------------------------
install_sparta:
	R CMD INSTALL software/sparta_0.8.2.9999.tar.gz

install_jti:
	R CMD INSTALL software/jti_0.8.0-21.tar.gz

install_others:
	$(R) -e "install.libraries('ess')"; \
	$(R) -e "install.libraries('tictoc')"; \
	$(R) -e "install.libraries('ggrepel')"; \
	$(R) -e "install.libraries('dplyr')"; \
	$(R) -e "install.libraries('glue')"; \
	$(R) -e "install.libraries('stringr')"; \
	$(R) -e "install.libraries('readr')"; \
	$(R) -e "install.libraries('ggplot2')"; \
	$(R) -e "install.libraries('tidyr')"; \
	$(R) -e "install.libraries('fs')"; \

# ---------------------------------------------------
# LEARN GRAPH STRUCTURES:
# ---------------------------------------------------
make_graphs:
	cd $(G); \
	$(R) make_graphs.R

# ---------------------------------------------------
# PREDICTION ACCURACY FOR EPSILON-SMOOTHING:
# ---------------------------------------------------
# Since we are only interested in the prediction here
# we use unity propagation in both cases since it is
# faster
# ---------------------------------------------------
make_prediction:
	cd $(P); \
	nice $(R) make.R

# ------------------------------------------------------------
# UNITY MESSAGE PROPAGATION TIMING 
# ------------------------------------------------------------
# Benchmarking the timings with and
# without avoidining mulitplication with unities. Also
# for non-unity, the full unity tables are created,
# ------------------------------------------------------------
make_message:
	cd $(M); \
	nice $(R) make.R

# --------------------------------------
# TRIANGULATION UNITY PROPAGATION TIMING
# --------------------------------------
# Bayesian networks where unity cliques
# occur due to triangulation. Timing with
# and without unity_msg
# --------------------------------------
make_triang:
	cd $(T); \
	nice $(R) make.R

# ---------------------------------------------------
# CPT SPARSITIES
# ---------------------------------------------------
# The densities shown in Table 3.
# ---------------------------------------------------
make_cpt_sparsities:
	cd $(G); \
	nice $(R) graphs_cpts_sparsity_imgs.R
