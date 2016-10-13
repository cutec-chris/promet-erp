Die automatische Build Umgebung sieht im Moment so aus:
Eine GitLab instanz steuert die Builds. Die entsprechende .yml Datei liegt im root Verzeichnis
Ein 64Bit Runner mit Docker installiert baut die Tests und führt sie aus
Danach bauen jewails Linux,Linux64,Windows,RaspberryPi Hosts mit gitlab Runnern und jewails Lazarus installiert alle Executables für die einzelnen Plattformen
Danach werden alle gebauten Binarys per ssh auf den Webserver hochgeladen
Danach wird eine Alpha Wikiseite erstellt die alle Executables listet

Bei neuen Versionen werden die regulären Downloadseiten angepasst

MacOS: MacOS 10.5 Client der minimac heisst hat im home Verzeichnis des Benuters das git unter promet und lazarus unter lazarus liegen ssh key muss vorhanden sein