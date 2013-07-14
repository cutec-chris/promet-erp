echo "Copying Files"
cp ../../languages/*.po $1/languages
cp ../../languages/*.txt $1/languages
mkdir $1/importdata
cp -r ../../importdata/* $1/importdata
cp ../../source/base/changes.txt $1
mkdir $1/resources
install -m 644 ../../resources/world_icon32.png $1/resources/$Program.png
cp ../executables/$3/$2/prometerp*.exe $1
cp ../executables/$3/$2/sync_*.exe $1
cp ../executables/$3/$2/pstarter.exe $1
cp ../executables/$3/$2/statistics.exe $1
cp ../executables/$3/$2/clientmanagement.exe $1
cp ../executables/$3/$2/wizardmandant.exe $1
cp ../$2-win32/sqlite3.dll $1
mkdir $1/tools
cp ../executables/$3/$2/processmanager.exe $1/tools
cp ../executables/$3/$2/*receiver.exe $1/tools
cp ../executables/$3/$2/*sender.exe $1/tools
cp ../executables/$3/$2/*receiver.exe $1/tools
cp ../$2-win32/tools/* $1/tools
mkdir $1/plugins
cp ../executables/$3/$2/shipping_*.exe $1/plugins/
cp ../../resources/prometerp_font.png $1/prometerp.png
cp ../../resources/deb_ubuntu.png $1/resources/setup.png

