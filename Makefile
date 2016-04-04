# Download data
raw_data/autocratic_regimes.zip:
	curl -o raw_data/autocratic_regimes.zip 'http://sites.psu.edu/dictators/wp-content/uploads/sites/12570/2015/04/GWF-Autocratic-Regimes-1.2.zip'

raw_data/GWF\ Autocratic\ Regimes\ 1.2: raw_data/autocratic_regimes.zip
	unzip raw_data/autocratic_regimes.zip -d raw_data
	touch raw_data/GWF\ Autocratic\ Regimes\ 1.2
	rm raw_data/autocratic_regimes.zip

raw_data/Eora26_2013.zip:
	curl -o raw_data/Eora26_2013.zip 'http://worldmrio.com/ComputationsM/Phase199/Loop082/simplified/Eora26_2013_bp.zip'

raw_data/Eora26_2013: raw_data/Eora26_2013.zip
	unzip raw_data/Eora26_2013.zip -d raw_data/Eora26_2013
	touch raw_data/Eora26_2013
	rm raw_data/Eora26_2013.zip

raw_data/StandardizedNew-2006_2015--core4-.zip:
	wget -O raw_data/StandardizedNew-2006_2015--core4-.zip 'http://www.enterprisesurveys.org/Portal/Documents/Comprehensive%201/StandardizedNew-2006_2015--core4-.zip'

raw_data/EnterpriseSurvey_core_2006_2015: raw_data/StandardizedNew-2006_2015--core4-.zip
	unzip raw_data/StandardizedNew-2006_2015--core4-.zip -d raw_data/EnterpriseSurvey_core_2006_2015
	touch raw_data/EnterpriseSurvey_core_2006_2015
	rm raw_data/StandardizedNew-2006_2015--core4-.zip
	
# Cleaning data
clean_data/JapanFDI_wide.RData: code/11_clean_japanfdi.R raw_data/2003_entry_exit_matrix_all_data.sav
	cd code;Rscript 11_clean_japanfdi.R

clean_data/JapanFDI_labels.RData: code/11_clean_japanfdi.R raw_data/2003_entry_exit_matrix_all_data.sav
	cd code;Rscript 11_clean_japanfdi.R

clean_data/JapanFDI_long.RData: code/12_format_japanfdi_to_long.R clean_data/JapanFDI_wide.RData
	cd code;Rscript 12_format_japanfdi_to_long.R
