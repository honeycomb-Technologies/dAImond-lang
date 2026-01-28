# dAImond Runtime Makefile

CC = cc
CFLAGS = -Wall -Wextra -std=c11 -O2
DEBUG_FLAGS = -g -O0 -DDEBUG

# Source files
RUNTIME_SRC = daimond_runtime.c
RUNTIME_HDR = daimond_runtime.h
TEST_SRC = test_runtime.c

# Output files
RUNTIME_OBJ = daimond_runtime.o
TEST_BIN = test_runtime

# Library name
LIB_NAME = libdaimond_runtime.a

.PHONY: all clean test debug lib

all: $(RUNTIME_OBJ)

# Compile runtime object file
$(RUNTIME_OBJ): $(RUNTIME_SRC) $(RUNTIME_HDR)
	$(CC) $(CFLAGS) -c $(RUNTIME_SRC) -o $(RUNTIME_OBJ)

# Build static library
lib: $(LIB_NAME)

$(LIB_NAME): $(RUNTIME_OBJ)
	ar rcs $(LIB_NAME) $(RUNTIME_OBJ)

# Build and run tests
test: $(TEST_BIN)
	./$(TEST_BIN)

$(TEST_BIN): $(TEST_SRC) $(RUNTIME_SRC) $(RUNTIME_HDR)
	$(CC) $(CFLAGS) -o $(TEST_BIN) $(TEST_SRC) $(RUNTIME_SRC) -lm

# Debug build
debug: CFLAGS += $(DEBUG_FLAGS)
debug: clean $(TEST_BIN)
	./$(TEST_BIN)

# Clean build artifacts
clean:
	rm -f $(RUNTIME_OBJ) $(TEST_BIN) $(LIB_NAME)

# Install header and library (requires specifying PREFIX)
install: lib
ifndef PREFIX
	$(error PREFIX is not set. Usage: make install PREFIX=/usr/local)
endif
	install -d $(PREFIX)/include $(PREFIX)/lib
	install -m 644 $(RUNTIME_HDR) $(PREFIX)/include/
	install -m 644 $(LIB_NAME) $(PREFIX)/lib/
