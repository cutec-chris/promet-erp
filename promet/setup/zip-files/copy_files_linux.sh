echo "Copying Files"
cp ../executables/$3/$2/prometerp $1
cp ../executables/$3/$2/sync_db $1
cp ../executables/$3/$2/pstarter $1
cp ../executables/$3/$2/statistics $1
cp ../executables/$3/$2/clientmanagement $1
cp ../executables/$3/$2/wizardmandant $1
mkdir $1/tools
cp ../executables/$3/$2/processmanager $1/tools
cp ../executables/$3/$2/*receiver $1/tools
cp ../executables/$3/$2/*sender $1/tools
cp ../executables/$3/$2/*receiver $1/tools
mkdir $1/plugins
cp ../../resources/prometerp_font.png $1/prometerp.png
cp ../../resources/deb_ubuntu.png $1/resources/setup.png

