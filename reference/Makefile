# Makefile to compile and link the dinoxor assembly code with a C wrapper for macOS Apple Silicon

# Compiler and assembler definitions
CC = clang
AS = clang
LD = clang

# Output executable name
TARGET = dinoxor_exe

# Source files
SOURCES = dinoxor.s main.c

# Object files
OBJECTS = $(SOURCES:.c=.o)
OBJECTS := $(OBJECTS:.s=.o)

# Compile C source files
%.o: %.c
	$(CC) -c $< -o $@

# Assemble assembly source files
%.o: %.s
	$(AS) -c $< -o $@

# Link object files into a single executable
$(TARGET): $(OBJECTS)
	$(LD) $(OBJECTS) -o $(TARGET)

# Default make target
all: $(TARGET)

# Clean build artifacts
clean:
	rm -f $(OBJECTS) $(TARGET)

.PHONY: all clean
