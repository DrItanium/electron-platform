.PHONY: all clean install uninstall

all: install 

install:
	@echo Installing electron core
	@cd electron && ${MAKE} install
	@echo Installing raw shell
	@cd raw && ${MAKE} install 
	@echo Installing srv/efs 
	@cd srv/efs && ${MAKE} install
	@echo Installing chicanery shell
	@cd chicanery && ${MAKE} install


clean:
	@echo Cleaning electron core
	@cd electron && ${MAKE} clean
	@echo Cleaning raw shell
	@cd raw && ${MAKE} clean 
	@echo Cleaning srv/efs 
	@cd srv/efs && ${MAKE} clean
	@echo Cleaning chicanery shell
	@cd chicanery && ${MAKE} clean

uninstall:
	@echo Uninstalling electron core
	@cd electron && ${MAKE} uninstall
	@echo Uninstalling raw shell
	@cd raw && ${MAKE} uninstall 
	@echo Uninstalling srv/efs 
	@cd srv/efs && ${MAKE} uninstall
	@echo Uninstalling chicanery shell
	@cd chicanery && ${MAKE} uninstall
