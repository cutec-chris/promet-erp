SET FULLTARGET=%1
echo Ja|del ..\..\output\%FULLTARGET%\*.*
echo Ja|del ..\..\output\%FULLTARGET%\tools\*receiver.*
echo Ja|del ..\..\output\%FULLTARGET%\tools\*sender.*
echo Ja|del ..\..\output\%FULLTARGET%\tools\processmanager.*
echo Ja|del ..\..\output\%FULLTARGET%\tools\*messagemenager.*