IDRIS		= idris
IPKGS		= codenames-duet.ipkg codenames.ipkg

.phony: build clean

build:
	$(IDRIS) $(foreach IPKG, $(IPKGS), --build $(IPKG))

clean:
	$(IDRIS) $(foreach IPKG, $(IPKGS), --clean $(IPKG))
