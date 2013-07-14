#!/bin/bash
Checkin="../output/x86_64-linux/checkin"
Mandant=Stora


$Checkin --mandant=$Mandant DOCUMENTS.ID@5469702{base} base
$Checkin --mandant=$Mandant DOCUMENTS.ID@5603045{base_erp} base_erp
$Checkin --mandant=$Mandant DOCUMENTS.ID@5603041{base_documents} base_documents
$Checkin --mandant=$Mandant DOCUMENTS.ID@5603034{base_repair} base_repair
$Checkin --mandant=$Mandant DOCUMENTS.ID@5603028{base_wiki} base_wiki
$Checkin --mandant=$Mandant DOCUMENTS.ID@5603025{base_ocr} base_ocr
$Checkin --mandant=$Mandant DOCUMENTS.ID@5603022{base_office} base_office
$Checkin --mandant=$Mandant DOCUMENTS.ID@5603019{base_phone} base_phone
$Checkin --mandant=$Mandant DOCUMENTS.ID@5603013{base_calendar} base_calendar
$Checkin --mandant=$Mandant DOCUMENTS.ID@5603008{base_messaging} base_messaging
$Checkin --mandant=$Mandant DOCUMENTS.ID@5602687{base_pm} base_pm
$Checkin --mandant=$Mandant DOCUMENTS.ID@5540023{messageimport} messageimport
$Checkin --mandant=$Mandant DOCUMENTS.ID@5482273{tools} tools
$Checkin --mandant=$Mandant DOCUMENTS.ID@5482143{testcases} testcases
$Checkin --mandant=$Mandant DOCUMENTS.ID@5481999{sync} sync
$Checkin --mandant=$Mandant DOCUMENTS.ID@5481949{import}import
$Checkin --mandant=$Mandant DOCUMENTS.ID@5471899{clientmanagement} clientmanagement
$Checkin --mandant=$Mandant DOCUMENTS.ID@92043999{archivestore} archivestore
$Checkin --mandant=$Mandant DOCUMENTS.ID@5490250{statistics} statistics
$Checkin --mandant=$Mandant DOCUMENTS.ID@5483201{promet.erp} promet.erp
$Checkin --mandant=$Mandant DOCUMENTS.ID@5555654{timeregistering} timeregistering
$Checkin --mandant=$Mandant DOCUMENTS.ID@5484739{projectmanagement} projectmanagement
$Checkin --mandant=$Mandant DOCUMENTS.ID@5484739{projectmanagement} meeting
$Checkin --mandant=$Mandant DOCUMENTS.ID@92304844{base_docmanage}



