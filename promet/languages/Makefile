all:    de en

clean:
	@rm -f *.mo *.pot

en:
	msgfmt English.po -o English.mo
de:
	msgfmt Deutsch.po -o Deutsch.mo
pot:
	rstconv -i ../source/base/dbintfs/lib/i386-linux-gtk2/uintfstrconsts.rst -o interface.pot
	rstconv -i ../../lkomponenten/tvplanit/packages/lib/x86_64-linux/vpsr.rst -o vpsr.pot
	rstconv -i ../../general/lib/gui/x86_64-linux-gtk2/ugeneralstrconsts.rst -o ugeneralstrconsts.pot

