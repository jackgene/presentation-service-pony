build:
	corral run -- ponyc -Dopenssl_0.9.0 -o build/release presentation_service

default: build

.PHONY: build