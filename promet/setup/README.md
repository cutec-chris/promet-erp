Die automatische Build Umgebung sieht im Moment so aus:
Master: Linux Host
Windows: wine verzeichnis lazarus_wine im promet root verzeichnis des git repositores, inno setup installiert (ispack tut gute dienste)
Linux i386: es wird in einer VM gebaut, im homeverzeichnis des bauenden benutzers ist lazarus und promet-erp.git ausgecheckt
Linux x86_64: der Master baut direkt
MacOS: MacOS 10.5 Client der minimac heisst hat im home Verzeichnis des Benuters das git unter promet und lazarus unter lazarus liegen ssh key muss vorhanden sein