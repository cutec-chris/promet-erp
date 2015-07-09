#/bin/bash
cp help/help.db /tmp
sqlite3 /tmp/help.db "delete from DOCUMENTS where TYPE<>'W';delete from HISTORY;delete from REPORTS;delete from ACCHISTORY;delete from TEMPLATES;delete from DOCPAGES;delete from DELETEDITEMS;delete from THUMBNAILS;delete from TASKS;delete from OPTIONS;vacuum;"
cp /tmp/help.db ./help.db
