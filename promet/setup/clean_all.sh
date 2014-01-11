#!/bin/bash
rm -f -r ../source/base/lib
rm -f -r ../source/base/dbintfs/lib
rm -f -r ../source/promet.erp/lib
rm -f -r ../source/tools/lib
rm -f -r ../source/projectmanagement/lib
rm -f -r ../source/clientmanagement/lib
rm -f -r ../source/messageimport/lib
rm -f -r ../source/archivestore/lib
rm -f -r ../source/base_calendar/lib
rm -f -r ../source/base_documents/lib
rm -f -r ../source/base_calendar/lib
rm -f -r ../source/base_erp/lib
rm -f -r ../source/base_messaging/lib
rm -f -r ../source/base_ocr/lib
rm -f -r ../source/base_office/lib
rm -f -r ../source/base_phone/lib
rm -f -r ../source/base_pm/lib
rm -f -r ../source/base_repair/lib
rm -f -r ../source/base_wiki/lib
rm -f -r ../source/base_help/lib
rm -f -r ../source/base_frames/lib
rm -f -r ../source/base_forms/lib
rm -f -r ../source/base_inet/lib
rm -f -r ../source/base_docmanage/lib
rm -f -r ../source/meeting/lib
rm -f -r ../source/testcases/lib
rm -f -r ../source/webstat/lib
rm -f -r ../source/checkout/lib
rm -f -r ../source/webservers/lib
rm -f -r ../source/import/lib
rm -f -r ../source/pos/lib
rm -f -r ../source/statistics/lib
rm -f -r ../source/sync/lib
rm -f -r ../source/timeregistering/lib
rm -f -r ../source/tools/lib
find ../ -type f -iname "*.compiled" -exec rm -f {} \;
find ../ -type f -iname "*.bak" -exec rm -f {} \;