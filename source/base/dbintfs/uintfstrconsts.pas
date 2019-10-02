{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
Created 01.06.2006
*******************************************************************************}
unit uIntfStrConsts;
{$mode objfpc}{$H+}
interface
resourcestring
  strSchemeList                 = 'Schema';
  strSpreetsheet                = 'Tabellen';
  strSentenceNotValid           = 'Das wurde leider nicht verstanden !';
  strChangeNumer                = 'Nummer ändern';
  strFailed                     = 'fehlgeschlagen !';
  strRunsOnEveryMashine         = 'läuft auf allen Maschienen';
  strItemOpened                 = 'Eintrag "%s" geöffnet';
  strMeetingUsers               = 'Teilnehmer';
  strAllElements                = 'Elemente';
  strMeasurement                = 'Messungen';
  strStructure                  = 'Struktur';
  strSearchFromProjects         = 'Mit Öffnen wird das gewählte Projekt als Vorfahr übernommen';
  strCommandline                = 'Komandozeile';
  strRepaired                   = 'repariert';
  strunRepaired                 = 'unrepariert';
  strDiscarded                  = 'entsorgt';
  strWaitingforCustomer         = 'wartet auf Kunden';
  strAssemblyexchanged          = 'Baugruppentausch';
  strGoodwillRepair             = 'Kulanzreparatur';
  strGoodwillExchange           = 'Kulanzaustausch';
  strGoodwillCredit             = 'Kulanzgutschrift';
  strIsNew                      = 'neuwertig';
  strSearchFromMailSelect       = 'Hier könenn Sie einen Benutzer oder Kontakt zum informieren suchen, wenn Sie die Suche abbrechen müssen Sie den zu Informierenden im e-Mail Programm suchen';
  strLocation                   = 'Ort';
  strImportExport               = 'Import/Export';
  strSearchFromOrder            = 'Mit Öffnen wird der gewählte Eintrag in den Auftrag übernommen';
  strCombiteItems               = 'Sollen die Elemente "%s" und "%s" wirklich verschmolzen werden ?';
  strClipboard                  = 'Zwischenablage';
  strCategoryChanged            = 'Kategorie geändetr auf %s';
  strDueDateChanged             = 'Zieldatum geändert zu %s';
  strDueDateDeleted             = 'Zieldatum entfernt';
  strCantAccessFile             = 'Auf die Datei "%s" kann nicht zugegriffen werden,'+lineending+'kopieren Sie diese Datei ggf erst auf Ihren Computer oder mounten Sie die Quelle';
  strTimetools                  = 'Zeiterfassung';
  strTimeregistration           = 'Zeitaufschreibung';
  strCosts                      = 'Kosten';
  strTargetCosts                = 'Zielkosten';
  strFullPercent                = 'Auslastung:%d%%';
  strGanttView                  = 'Gantt Ansicht';
  strTaskPlan                   = 'Aufgabenplan';
  strAttPlan                    = 'Anwesenheitsplan';
  strDate                       = 'Datum';
  strWorker                     = 'Bearbeiter';
  strResponsable                = 'Verantwortlicher';
  strMeetingList                = 'Besprechungsliste';
  strMeetings                   = 'Besprechungen';
  strKeywords                   = 'Suchbegriffe';
  strProperties                 = 'Eigenschaften';
  strTexts                      = 'Texte';
  strRepairParts                = 'Reparaturteile';
  strRepairAssembly             = 'Baugruppen';
  strPrices                     = 'Preise';
  strSupplier                   = 'Lieferanten';
  strStorage                    = 'Lager';
  strPayGroups                  = 'Zahlungsgruppen';
  strMandantDetails             = 'Mandantendetails';
  strPermissions                = 'Zugriffsrechte';
  strUserRights                 = 'Benutzerrechte';
  strTree                       = 'Baum';
  strReports                    = 'Belege';
  strNumbersets                 = 'Nummernkreise';
  strInventory                  = 'Inventuren';
  strInventoryPos               = 'Inventurpositionen';
  strTexttypes                  = 'Texttypen';
  strRepairProblems             = 'Reparaturprobleme';
  strDispatchtyoes              = 'Versandarten';
  strPositionCalc               = 'Positionskalkulation';
  strPriceTypes                 = 'Preistypen';
  strPositionType               = 'Positionstypen';
  strPaymentTargets             = 'Zahlungsziele';
  strCurrency                   = 'Währung';
  strLanguages                  = 'Sprachen';
  strStandard                   = 'Standard';
  strObject                     = 'Objekt';
  strRefObject                  = 'Referenzobjekt';
  strStatistics                 = 'Statistiken';
  strOverview                   = 'Übersicht';
  strStartingNextTimeout        = 'Der process wird demnächst wieder gestartet';
  strStartingProcessTimeout     = 'Timeout überschritten';
  strStartingProcess            = 'Der process wird (neu) gestartet';
  strExitted                    = 'quitted';
  strRevision                   = 'Revision';
  strFavourites                 = 'Favoriten';
  strPositions                  = 'Positionen';
  strAttatchments               = 'Anhänge';
  strSync                       = 'Synchronisation';
  strProcesses                  = 'Prozesse';
  strProcess                    = 'Prozess';
  strMandant                    = 'Mandant';
  strEnterAnFilterName          = 'geben Sie einen Namen für den neuen Filter an';
  strNoRightsToSaveComplexFilers= 'Sie haben nicht die erforderliche Berechtigungstufe um komplexe Filter zu speichern !';
  strFilterNotFound             = 'Der Filter "%s" wurde nicht gefunden !';
  strNoSelectFilter             = '<kein Filter>';
  strNewList                    = 'Neue Liste';
  strLists                      = 'Listen';
  strCreatedby                  = 'erstellt von';
  strDue                        = 'Bis';
  strTask                       = 'Aufgabe';
  strStart                      = 'Start';
  strSummary                    = 'Zusammenfassung';
  strStatusChanged              = 'Status geändert von %s zu %s';
  strNumberChanged              = 'Nummer geändert zu %s';
  strPriorityChanged            = 'Priorität geändert zu %s';
  strFiles                      = 'Dateien';
  strDocuments                  = 'Dokumente/Bilder';
  strDocumentsOnly              = 'Dokumente';
  strFailedtoLoadMandants       = 'Mandanten konnten nicht gelanden werden !';
  strMandantnotSelected         = 'kein Mandant gewählt (--mandant) !';
  strDestinationnotSelected     = 'kein Zielmandant gewählt (--destination) !';
  strLoginFailed                = 'Login fehlgeschlagen';
  strProjectList                = 'Projektliste';
  strObjectList                 = 'Elementliste';
  strStatisticList              = 'Statistikliste';
  strSalesList                  = 'Zahlungsliste';
  strVoucher                    = 'Beleg';
  strNewVoucher                 = 'neuer Beleg';
  strNewOrder                   = 'als ';
  strWiki                       = 'Wiki';
  strUsers                      = 'Benutzer';
  strLinks                      = 'Verweise';
  strItem                       = 'Eintrag';
  strDescription                = 'Beschreibung';
  strNewDir                     = 'Neues Verzeichnis';
  strNoDirectorySelected        = 'Sie müssen ein Verzeichnis auswählen,'+lineending+'bevor Sie den Eintrag zu einem diesem zuordnen können.';
  strGotoFeed                   = 'vollen Artikel anzeigen';
  strActionMessageReceived      = '%s - empfangen';
  strMessageSpamPoints          = 'Spampunkte: ';
  strWrongPassword              = 'Das eingegebene Passwort stimmt nicht mit dem von Ihnen erstellten überein';
  strCustomers                  = 'Kontakte';
  strAdresses                   = 'Adressen';
  strMasterdata                 = 'Artikel';
  strCustomerCont               = 'Kontaktdaten';
  strTimeList                   = 'Time List';
  strOrders                     = 'Aufträge';
  strProduction                 = 'Fertigung';
  strProductionOrders           = 'Fertigungsaufträge';
  strTasks                      = 'Aufgaben';
  strRealDelete                 = 'möchten Sie diesen Eintrag wirklich löschen ?';
  strRename                     = 'Umbenennen';
  strNewName                    = 'neuer Name';
  strCreatingForms              = 'erstelle Formulare ...';
  strClose                      = 'Schliessen';
  strServer                     = 'Server';
  strPleaseWait                 = 'Bitte warten';
  strName                       = 'Name';
  strItemnotSaved               = 'Das Objekt ist nicht gespeichert, jetzt speichern ?';
  strAbort                      = 'Abbrechen';
  strThisAccountdontExists      = 'Dieses Konto scheint auf diesem Computer nicht eingerichtet zu sein, richten Sie es zuerst ein !';
  strAccount                    = 'Konto';
  strSortCode                   = 'Bankleitzahl';
  strType                       = 'Typ';
  strCalendar                   = 'Kalender';
  strUnknown                    = 'Unbekannt';
  strnewNumber                  = 'neue Nummer';
  strDatasetmustbeinInsertMode  = 'Sie können lediglich in neue Einträge Daten aus der Zwischenablage einfügen, bitte legen Sie zuvor einen neuen Eintrag an.';
  strCantgetClipboardContents   = 'Konnte keine Zwischenablagedaten holen.';
  strEdited                     = 'bearbeitet';
  strCustomerlist               = 'Kontaktliste';
  strNoDataFound                = 'kein Eintrag gefunden !';
  strArticleList                = 'Artikelliste';
  strSearch                     = 'Suche';
  strContinueSearch             = 'weitersuchen';
  strSerial                     = 'Serienummer';
  strBarcode                    = 'Barcode/PLZ';
  strCommission                 = 'Komission';
  strID                         = 'ID';
  strShortname                  = 'Kurztext';
  strMessages                   = 'Nachrichten';
  strArchive                    = 'Archiv';
  strClosingPleaseWait          = 'wird beendet...';
  streMail                      = 'e-Mail';
  strExterneMail                = 'externes e-Mail Programm';
  strOrderList                  = 'Auftragsliste';
  strQuantity                   = 'Menge';
  strOrder                      = 'Auftrag';
  strAlternativeRowcolor        = 'Alternative Zeilenfarbe';
  strInfoColor                  = 'Farbe für aktives Feld';
  strNoFilter                   = '*';
  strAccounts                   = 'Konten';
  strVersion                    = 'Version';
  strFinish                     = 'Fertigstellen';
  strPrev                       = 'Zurück';
  strNext                       = 'Weiter';
  strUnknownDbType              = 'Unbekannter Datenbanktyp';
  strProject                    = 'Projekt';
  strProjectNumber              = 'Projektnummer';
  strUserNotFound               = 'benutzer nicht gefunden !';
  strErrorDatabaseStructurenotActual = 'Die Struktur Ihrer Datenbank ist nicht aktuell, bitte wenden Sie sich an Ihren Administrator order updaten Sie diese mit dem Updatedatabase Tool.';
  strNotSupported               = 'nicht unterstützt ! (Interner Fehler)';
  strFinished                   = 'beendet';
  strContact                    = 'Kontakt';
  strFile                       = 'Datei';
  strCall                       = 'Anruf';
  strMessage                    = 'Nachricht';
  strCheckingforUpdate          = 'prüfe auf Update...';
  strreadingOptions             = 'lese Optionen ...';
  strProjects                   = 'Projekte';
  strEdit                       = 'bearbeiten';
  strWikiMainPageText           = '==Willkommen zur Promet Wiki==<br>'+lineending+'<br>'+lineending+'Sie können dieses Wiki als zentralen Wissenspool verstehen. Sie können hier Informationen jeglicher Art strukturiert ablegen.<br>'+lineending+'<br>'+lineending+'Dazu stehen Ihnen interne Links [[Ihre erste Seite]] durch die Sie auch ganz einfach neue Sieten anlegen könenn einfach indem Sie, sie anlegen und draufklicken. Externe Links, zu beliebigen Websites [http://www.ullihome.de www.ullihome.de], Aufzählungen'+lineending+'*1'+lineending+'*2'+lineending+'*3'+lineending+'===Überschriften==='+lineending+'====In verschiedenen Größen===='+lineending+'und ähnliche Elemente zur Verfügung.';
  strWikiPage                   = 'Wiki Seite';
  strAccountexchange            = 'Bankbuchungen';
  strHistory                    = 'Verlauf';
  strImages                     = 'Bilder';
  strImage                      = 'Bild';
  strFinance                    = 'Finanzen';
  strSearchfromTimeregisteringMode = 'Diese Suche wurde aus der Zeiterfassung gestartet, wenn Sie einen Eintrag öffnen, wird dieser automatisch in die Zeiterfassung übernommen.';
  strSearchfromDocumentsMode    =  'Diese Suche wurde aus der Dokumentenverwaltung gestartet, wenn Sie einen Eintrag öffnen, wird dieser automatisch mit dem Dokument verknüpft.';
  strSearchfromHistoryMode      =  'Diese Suche wurde aus einem Verlauf gestartet, wenn Sie einen Eintrag öffnen, wird dieser automatisch mit dem Verlaufseintrag verknüpft.';
  strError                      = 'Error';

  strIADAddress                 = 'IAD Rechnungsadresse';
  strDADAddress                 = 'DAD Lieferadresse';
  strBADAddress                 = 'BAD Geschäftsadresse';
  strPADAddress                 = 'PAD Privatadresse';

  vInfo                         = 'www:  http://www.cu-tec.de'+lineending
                                +'mail: info@cu-tec.de'+lineending
                                +lineending
                                +'Lizenz:'+lineending
                                +'Die Software und ihre Dokumentation wird wie sie ist zur'+lineending
                                +'Verfuegung gestellt. Da Fehlfunktionen auch bei ausfuehrlich'+lineending
                                +'getesteter Software durch die Vielzahl an verschiedenen'+lineending
                                +'Rechnerkonfigurationen niemals ausgeschlossen werden koennen,'+lineending
                                +'uebernimmt der Autor keinerlei Haftung fuer jedwede Folgeschaeden,'+lineending
                                +'die sich durch direkten oder indirekten Einsatz der Software'+lineending
                                +'oder der Dokumentation ergeben. Uneingeschraenkt ausgeschlossen'+lineending
                                +'ist vor allem die Haftung fuer Schaeden aus entgangenem Gewinn,'+lineending
                                +'Betriebsunterbrechung, Verlust von Informationen und Daten und'+lineending
                                +'Schaeden an anderer Software, auch wenn diese dem Autor bekannt'+lineending
                                +'sein sollten. Ausschliesslich der Benutzer haftet fuer Folgen der'+lineending
                                +'Benutzung dieser Software.'+lineending
                                +lineending
                                +'erstellt mit Freepascal + Lazarus'+lineending
                                +'http://www.freepascal.org, http://lazarus.freepascal.org'+lineending
                                +'Iconset von:'+lineending
                                +'http://www.famfamfam.com/lab/icons/silk/'+lineending
                                +'Rem Objects PascalScript von:'+lineending
                                +'https://github.com/remobjects/pascalscript'+lineending
                                ;

implementation
end.
