IDRIS		= idris
IPKG		= codenames-duet.ipkg

.phony: build clean

build:
	$(IDRIS) --build $(IPKG)

clean:
	$(IDRIS) --clean $(IPKG)
