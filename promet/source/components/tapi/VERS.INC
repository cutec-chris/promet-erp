{$IFNDEF VER80}
  {$DEFINE D2_OR_GR}
  {$IFNDEF VER90}
    {$DEFINE D3_OR_GR}
    {$IFNDEF VER100}			 
      {$DEFINE D4_OR_GR}
      {$IFNDEF VER120}
        {$DEFINE D5_OR_GR}
        {$IFNDEF VER130}
          {$DEFINE D6_OR_GR}
          {$IFNDEF VER140}
            {$DEFINE D7_OR_GR}
          {$ENDIF}
        {$ENDIF}  
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}



