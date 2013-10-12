.PHONY: all clean install uninstall

all: install 

install:
	@echo Installing electron core
	@cd electron && ${MAKE} install
	@echo Installing raw shell
	@cd cmd/raw && ${MAKE} install 
	@echo Installing srv/efs 
	@cd srv/efs && ${MAKE} install
	@echo Installing platform shell
	@cd cmd/platform && ${MAKE} install


clean:
	@echo Cleaning electron core
	@cd electron && ${MAKE} clean
	@echo Cleaning raw shell
	@cd cmd/raw && ${MAKE} clean 
	@echo Cleaning srv/efs 
	@cd srv/efs && ${MAKE} clean
	@echo Cleaning platform shell
	@cd cmd/platform && ${MAKE} clean

uninstall:
	@echo Uninstalling electron core
	@cd electron && ${MAKE} uninstall
	@echo Uninstalling raw shell
	@cd cmd/raw && ${MAKE} uninstall 
	@echo Uninstalling srv/efs 
	@cd srv/efs && ${MAKE} uninstall
	@echo Uninstalling platform shell
	@cd cmd/platform && ${MAKE} uninstall
