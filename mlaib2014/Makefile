raw_files := $(wildcard ebsco/*.ris)
agg_script := aggregate.py

ebsco.csv: $(raw_files) $(agg_script)
	python $(agg_script) $(raw_files) > $@

