CC = elm make
PACKAGE = elm package
INSTALL = install
SRC = src
TEST = test
OPEN = xdg-open
NODE = node
BASH = bash

PACKAGE_FLAGS = -y

BUILD_DIR = build
RESOURCES_DIR = resources

.PHONY: test quickcheck publish

all: package/install clean compile

compile: sudoku

elm.js = $(BUILD_DIR)/elm.js
Main.elm = $(SRC)/Main.elm
sudoku: $(elm.js)
$(elm.js): $(Main.elm)
	$(CC) --output $(elm.js) $(Main.elm)

clean:
	rm -rf $(BUILD_DIR)

package/install:
	$(PACKAGE) $(INSTALL) $(PACKAGE_FLAGS)

#### TEST ####################

TestRunner.elm = ${TEST}/TestRunner.elm
test: 
	elm-test ${TestRunner.elm}


quickcheck:
	$(CC) test/TestRunner.elm --output build/quickcheck.html

publish:
	cp style.css bvdeenen.github.io/sudoku
	cp build/elm.js bvdeenen.github.io/sudoku/build
