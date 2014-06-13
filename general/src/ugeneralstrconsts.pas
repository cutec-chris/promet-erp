unit uGeneralStrConsts; 

{$mode objfpc}{$H+}

interface

resourcestring
  strRefresh                    = 'Aktualisieren';
  strCancelEdit                 = 'Änderungen verwerfen';
  strSave                       = 'Speichern';
  strEdit                       = 'bearbeiten';
  strDelete                     = 'löschen';
  strSNew                       = 'Neu';
  strRecord                     = 'Datensatz';
  strBacktrace                  = 'Fehlerherkunft';
  strAddBugToTracker            = 'Fehler berichten';
  strStackTrace                 = 'Stackverfolgung:';
  strError                      = 'Error';
  strExceptionclass             = 'Errorclass: ';
  strOriginalException          = 'Original exception: ';
  strExceptObject               = 'Errorobject: ';
  strExceptObjectclass          = 'Errorobjectclass: ';
{$ifdef CPU32}
  strExceptPointer              = 'Error adress: %.4x';
{$endif}
{$ifdef CPU64}
  strExceptPointer              = 'Error adress: %.8x';
{$endif}
  strAdd                        = 'Add';
  strStatus                     = 'Status';
  strFilter                     = 'Filter';
  strNotice                     = 'Notice';
  strLogintoBugtrackerfailed    = 'Login on bugtracker failed!';
  strAllOpen                    = 'All open entrys';
  strAllOpenFeaturerequests     = 'All open featurerequests';
  strAllOpenBugs                = 'All open bugreports';
  strAllClosed                  = 'All closed entrys';
  strAllAll                     = 'All entrys';
  strAbort                      = 'Abort';
  strBack                       = 'Back';
  strNext                       = 'Next';
  strHelpIndex                  = 'Help index';
  strHelp                       = 'Help';
  strNoHelpAvalible             = 'No help avalible';
  strHome                       = 'Index';
  strSearch                     = 'Search';
  strSearchresults              = 'Searchresults';
  strMatch                      = 'Match';
  strNothingFound               = 'No entrys found !';
  strFilenotinrepositore        = 'Your File: %s is not in the repositore ! Maybe its is external modified !';
  strUpdate                     = 'Update';
  strCheckingforUpdate          = 'Checking for update ...';
  strDoUpdate                   = 'Automatic online update active';
  strPleaseWait                 = 'Please wait ...';
  strFailedPatchingFile         = 'Error patching the file';
  strGettingUpdateFile          = 'Getting update index ...';
  strUpdateSourcenotthere       = 'The update source dont anwer or isnt there';
  strWaitingforFileAccess       = 'Waiting for exclusive fileaccess';
  strUpdatingFile               = 'Updating file %s';
  strCheckingForrevisions       = 'Checking for new revisions';
  strFileIsNewer                = 'Your file is newer than the file on server, this error schouldnt be there, please inform the author';
  strActualRevisionNotFound     = 'Your file is newer than the file on server, Virus ??';
  strTargetRevisionnotFound     = 'Target revision not found';
  strRevisions                  = 'Patching file from revision %d to %d';
  strGettingRevision            = 'Getting revision %d';
  strErrorGettingFile           = 'Coudnt load file from server';
  strPatchingFile               = 'Patching file';
  strDone                       = 'Done.';
  strPatchExedontexists         = 'Bspatch or patch program dosend exists !';
  strNoNetavalible              = 'No network avalible, the online update is disabled !';
  strAnNewUpdateisAvalible      = 'Thers an new update avalible, download ind install now ?';
  strDescription                = 'Description';
  strFieldReplicablecannotbeclear='Field reproduceable cannot be clear !';
  strFieldSeveritycannotbeclear = 'Field severity cannot be clear';
  strFieldDescriptioncannotbeclear='Field description cannot be clear !';
  strFieldSummarycannotbeclear  = 'Field summary cannot be clear !';
  strFieldNamecannotbeclear     = 'Field submittername cannot be clear !';
  strFieldMailcannotbeclear     = 'Field submittermail cannot be clear !';
  strSubmittername              = 'Submittername';
  strSubmittermail              = 'Submittermail';
  strSummary                    = 'Summary';
  strChange                     = 'Change';
  strDate                       = 'Date';
  strReplicable                 = 'Reproduceable';
  strID                         = 'ID';
  strSeverity                   = 'Severity';
  strHistory                    = 'History';
  strSubmit                     = 'Submit';
  strAddNote                    = 'Add notice';
  strClose                      = 'Close';
  strAdditionalInfos            = 'Additional information';
  strViewFeatureRequestorBug    = 'Show featurerequest/bugreport';
  strAddFeatureRequestorBug     = 'Add featurerequest/bugreport';
  strBugtracker                 = 'Featurerequests/bugreports';
  strMaximize                   = 'Maximize panel';
  strMinimize                   = 'Minimize panel';
  strRestore                    = 'Restore panel';
  strUndock                     = 'Undock panel';
  strDock                       = 'Dock panel';
  strYes                        = 'Yes';
  strNo                         = 'No';
  strOK                         = 'OK';
  strLanguage                   = 'Language';
  strShortCuts                  = 'Shortcuts';
  strShortCut                   = 'Shortcut';
  strFunction                   = 'Function';
  strGetKey                     = 'Get Key';
  strDeletekey                  = 'Delete key';
  strSystem                     = 'System';
  strUnknownOS                  = 'Unknown OS';
  strNox86CPU                   = 'No x86 compatible CPU';
  strNottested                  = 'not tested';
  strValue                      = 'Value';
  strOperatingSystem            = 'Operating System';
  strCPU                        = 'CPU';
  strHarddisk                   = 'Harddisk';
  strType                       = 'Typ';
  strRating                     = 'Rating';
  strMemory                     = 'Memory';
  strBenchmark                  = 'Benchmark';
  strLicense                    = 'License';
  strChanges                    = 'Changes';
  strInfo                       = 'Info';
  strTimedOut                   = 'Programtimeout';

implementation
end.

