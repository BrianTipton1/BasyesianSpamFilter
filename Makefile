HC = ghc
SRC_DIR = src
APP_DIR = app
TARGET = BasyesianSpamFilter

all: $(TARGET)
	rm -f $(SRC_DIR)/*.o $(SRC_DIR)/*.hi $(APP_DIR)/*.o $(APP_DIR)/*.hi

$(TARGET):
	$(HC) --make -dynamic -i$(SRC_DIR) $(APP_DIR)/Main.hs -o $(TARGET)

clean:
	rm -f $(SRC_DIR)/*.o $(SRC_DIR)/*.hi $(APP_DIR)/*.o $(APP_DIR)/*.hi $(TARGET)

.PHONY: all clean
