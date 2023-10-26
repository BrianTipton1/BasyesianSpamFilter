HC = ghc
SRC_DIR = src
APP_DIR = app
TARGET = BasyesianSpamFilter

all: build cleanup

build:
	$(HC) --make -dynamic -i$(SRC_DIR) $(APP_DIR)/Main.hs -o $(TARGET)

cleanup:
	find $(SRC_DIR) $(APP_DIR) -type f \( -name "*.o" -o -name "*.hi" \) -delete

clean:
	find $(SRC_DIR) $(APP_DIR) -type f \( -name "*.o" -o -name "*.hi" \) -delete
	rm -f $(TARGET)

.PHONY: all build cleanup clean
