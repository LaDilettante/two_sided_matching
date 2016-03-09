# Download data
raw_data/autocratic_regimes.zip:
	curl -o raw_data/autocratic_regimes.zip 'http://sites.psu.edu/dictators/wp-content/uploads/sites/12570/2015/04/GWF-Autocratic-Regimes-1.2.zip'

raw_data/GWF\ Autocratic\ Regimes\ 1.2: raw_data/autocratic_regimes.zip
	unzip raw_data/autocratic_regimes.zip -d raw_data
	touch raw_data/GWF\ Autocratic\ Regimes\ 1.2

# Cleaning data
clean_data/JapanFDI_wide.RData: code/11_clean_japanfdi.R raw_data/2003_entry_exit_matrix_all_data.sav
	cd code;Rscript 11_clean_japanfdi.R

clean_data/JapanFDI_long.RData: code/12_format_japanfdi_to_long.R clean_data/JapanFDI_wide.RData
	cd code;Rscript 12_format_japanfdi_to_long.R
