SET FULLTARGET=%1
echo j|del ..\..\output\%FULLTARGET%\*.*
echo j|del ..\..\output\%FULLTARGET%\tools\*receiver.*
echo j|del ..\..\output\%FULLTARGET%\tools\*sender.*
echo j|del ..\..\output\%FULLTARGET%\tools\processmanager.*
echo j|del ..\..\output\%FULLTARGET%\tools\*messagemenager.*