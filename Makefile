# Specify the name of the output executable
OUTFILE := main

# Specify the source file(s)
SRC := main.rs

# Specify any additional flags for the Rust compiler
RUSTFLAGS := 

FIND_ALL_ERROR_FLAG := --cfg find_all_error

# Flags for the release build (e.g., optimizations)
RELEASE_FLAGS := -O -C panic=abort

all: $(OUTFILE)

$(OUTFILE): $(SRC)
	rustc $(RUSTFLAGS) -o $(OUTFILE) $(SRC)

release: $(SRC)
	rustc $(RUSTFLAGS) $(RELEASE_FLAGS) -o $(OUTFILE) $(SRC)

find_all: $(SRC)
	rustc $(RUSTFLAGS) $(FIND_ALL_ERROR_FLAG) $(RELEASE_FLAGS) -o $(OUTFILE) $(SRC)

clean:
	rm -f $(OUTFILE)

.PHONY: all release clean
