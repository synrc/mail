build:
	mkdir -p build && cd build && cmake -DOPENSSL_ROOT_DIR=/usr/local/opt/openssl .. && make

echo:
	wscat -l 9002

clean:
	rm -rf build/

run:
	./build/clu

asn1:
	cd asn; asn1c ../../src/ROSTER.asn1

deps:
	brew install cmake openssl asn1c
