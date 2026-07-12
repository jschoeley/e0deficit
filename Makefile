RSCRIPT ?= Rscript

.PHONY: skeleton-and-downloads harmonize assemble-analysis-input counterfactual-projection estimate-and-cluster plots tables checks-and-counts

skeleton-and-downloads:
	$(RSCRIPT) src/10-make_skeleton.R
	$(RSCRIPT) src/11-download_population_and_rates.R
	$(RSCRIPT) src/12-download_death.R
	$(RSCRIPT) src/13-download_netmigration.R
	$(RSCRIPT) src/14-download_lifeexpectancy.R

harmonize:
	$(RSCRIPT) src/20-harmonize_population_and_rates.R
	$(RSCRIPT) src/21-harmonize_death_with_exposures.R
	$(RSCRIPT) src/22-harmonize_netmigration.R
	$(RSCRIPT) src/23-harmonize_lifeexpectancy.R

assemble-analysis-input:
	$(RSCRIPT) src/30-join_data.R
	$(RSCRIPT) src/31-rate_bias_correction.R

counterfactual-projection:
	$(RSCRIPT) src/40-counterfactual_projection.R

estimate-and-cluster:
	$(RSCRIPT) src/50-estimation_and_inference.R
	$(RSCRIPT) src/51-assign_clusters.R

plots:
	$(RSCRIPT) src/60-plot_e0_trends.R
	$(RSCRIPT) src/61-plot_e0_deficits.R
	$(RSCRIPT) src/62-plot_e0_deficits_age_decomposition.R

tables:
	$(RSCRIPT) src/71-tabulate_e0_deficits.R
	$(RSCRIPT) src/72-tabulate_data_sources.R

checks-and-counts:
	$(RSCRIPT) src/90-sensitivity_check_e0_exposures.R
	$(RSCRIPT) src/91-benchmark_check_e0.R
	$(RSCRIPT) src/92-sensitivity_check_excess_measures.R
	$(RSCRIPT) src/94-count_countries_by_type.R
