ELPA_DEPENDENCIES=json-mode json-reformat json-snatcher package-lint libelcouch

ELPA_ARCHIVES=melpa

LINT_CHECKDOC_FILES=$(wildcard *.el)
LINT_CHECKDOC_OPTIONS=--eval "(setq checkdoc-arguments-in-order-flag nil)"
LINT_PACKAGE_LINT_FILES=$(wildcard *.el)
LINT_COMPILE_FILES=$(wildcard *.el)

makel.mk:
	# Download makel
	@if [ -f ../makel/makel.mk ]; then \
		ln -s ../makel/makel.mk .; \
	else \
		curl \
		--fail --silent --show-error --insecure --location \
		--retry 9 --retry-delay 9 \
		-O https://gitlab.petton.fr/DamienCassou/makel/raw/v0.2.0/makel.mk; \
	fi

# Include emake.mk if present
-include makel.mk
