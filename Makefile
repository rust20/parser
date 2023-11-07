# Specify the name of the output executable
OUTFILE := main

# Specify the source file(s)
SRC := main.rs

# Specify any additional flags for the Rust compiler
RUSTFLAGS := -C link-args=-Wl,-zstack-size=16194304

# Flags for the release build (e.g., optimizations)
RELEASE_FLAGS := -O -C panic=abort

all: $(OUTFILE)

$(OUTFILE): $(SRC)
	rustc $(RUSTFLAGS) -o $(OUTFILE) $(SRC)

release: $(SRC)
	rustc $(RUSTFLAGS) $(RELEASE_FLAGS) -o $(OUTFILE) $(SRC)

clean:
	rm -f $(OUTFILE)

.PHONY: all release clean
