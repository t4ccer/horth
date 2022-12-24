NIX_SOURCES := $(shell fd -enix)
HS_SOURCES := $(shell fd -ehs)
FORMAT_EXTENSIONS := -o -XQuasiQuotes -o -XTemplateHaskell -o -XTypeApplications -o -XImportQualifiedPost -o -XPatternSynonyms -o -XOverloadedRecordDot

format:
	alejandra $(NIX_SOURCES)
	fourmolu --mode inplace $(FORMAT_EXTENSIONS) $(HS_SOURCES)

formatCheck:
	alejandra --check $(NIX_SOURCES)
	fourmolu --mode check $(FORMAT_EXTENSIONS) $(HS_SOURCES)

