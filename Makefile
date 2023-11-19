SRC_DIR=./src
BUILD_DIR=./build
PROG_NAME=webserver

all: $(BUILD_DIR)/$(PROG_NAME)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c | $(BUILD_DIR)
	gcc -O2 -c -o $@ $^

$(BUILD_DIR)/$(PROG_NAME): $(SRC_DIR)/webserver.cob $(SRC_DIR)/sigint_handler.cob $(SRC_DIR)/sigalrm_handler.cob $(BUILD_DIR)/file-helper.o | $(BUILD_DIR)
	cobc -Wall -Wextra -fstatic-call -x -O2 -o $@ $^

clean:
	rm -rf $(BUILD_DIR)