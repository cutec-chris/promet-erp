#!/bin/bash
CheckOut="../output/x86_64-linux/checkout"
Mandant=Stora


$CheckOut --mandant=$Mandant DOCUMENTS.ID@5469702{base} base
$CheckOut --mandant=$Mandant DOCUMENTS.ID@5603045{base_erp} base_erp
$CheckOut --mandant=$Mandant DOCUMENTS.ID@5603041{base_documents} base_documents
$CheckOut --mandant=$Mandant DOCUMENTS.ID@5603034{base_repair} base_repair
$CheckOut --mandant=$Mandant DOCUMENTS.ID@5603028{base_wiki} base_wiki
$CheckOut --mandant=$Mandant DOCUMENTS.ID@5603025{base_ocr} base_ocr
$CheckOut --mandant=$Mandant DOCUMENTS.ID@5603022{base_office} base_office
$CheckOut --mandant=$Mandant DOCUMENTS.ID@5603019{base_phone} base_phone
$CheckOut --mandant=$Mandant DOCUMENTS.ID@5603013{base_calendar} base_calendar
$CheckOut --mandant=$Mandant DOCUMENTS.ID@5603008{base_messaging} base_messaging
$CheckOut --mandant=$Mandant DOCUMENTS.ID@5602687{base_pm} base_pm
$CheckOut --mandant=$Mandant DOCUMENTS.ID@5540023{messageimport} messageimport
$CheckOut --mandant=$Mandant DOCUMENTS.ID@5482273{tools} tools
$CheckOut --mandant=$Mandant DOCUMENTS.ID@5482143{testcases} testcases
$CheckOut --mandant=$Mandant DOCUMENTS.ID@5481999{sync} sync
$CheckOut --mandant=$Mandant DOCUMENTS.ID@5481949{import}import
$CheckOut --mandant=$Mandant DOCUMENTS.ID@5471899{clientmanagement} clientmanagement
$CheckOut --mandant=$Mandant DOCUMENTS.ID@92043999{archivestore} archivestore

$CheckOut --mandant=$Mandant DOCUMENTS.ID@5490250{statistics} statistics
$CheckOut --mandant=$Mandant DOCUMENTS.ID@5483201{promet.erp} promet.erp
$CheckOut --mandant=$Mandant DOCUMENTS.ID@5555654{timeregistering} timeregistering
$CheckOut --mandant=$Mandant DOCUMENTS.ID@5484739{projectmanagement} projectmanagement
$CheckOut ..mandant=$Mandant DOCUMENTS.ID@5484739{projectmanagement} meeting


