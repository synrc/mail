build:
	mkdir -p build && cd build && cmake -DOPENSSL_ROOT_DIR=/usr/local/opt/openssl .. && make

echo:
	wscat -l 9002

clean:
	rm -rf build/

run:
	./build/clu

asn: ../../src/ROSTER.asn1
	mkdir -p asn && cd asn && asn1c ../$< && rm -f Makefile.am.sample converter-sample.c

deps:
	brew install cmake openssl asn1c glfw
