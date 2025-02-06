
all:
	make build
	make install

build:
	cabal build test-client plain-server snoop-server socketfilter-server snoop-filter-server printk-server

install:
	cabal install test-client plain-server snoop-server socketfilter-server snoop-filter-server printk-server --installdir=. --overwrite-policy=always --install-method=copy

clean:
	rm test-client plain-server snoop-server socketfilter-server snoop-filter-server printk-server
