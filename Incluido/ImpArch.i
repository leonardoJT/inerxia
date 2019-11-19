  /***************************************************************
   archivo incluido
  ***************************************************************/ 
  DEFINE VAR W_Dispositivo AS CHARACTER INITIAL "A".
  DEFINE VAR W_sw          AS LOGICAL. 
  
  RUN _SetCurs.p ("WAIT").
  OUTPUT TO VALUE({1}) NO-ECHO PAGED PAGE-SIZE 81.
  RUN ProcesoImprimir.
  OUTPUT CLOSE.        
  RUN _SetCurs.p ("ARROW").

