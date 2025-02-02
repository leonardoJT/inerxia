  /***************************************************************
   Procedimiento: Imprimir.I
  ***************************************************************/ 
  DEFINE VARIABLE W_Dispositivo AS CHARACTER INITIAL " ".
  DEFINE VARIABLE W_sw          AS LOGICAL. 
  
  RUN P-DisPos IN W_Manija (INPUT-OUTPUT {1},INPUT-OUTPUT W_Dispositivo).
  IF W_Dispositivo EQ "" THEN
     RETURN.

  RUN _SetCurs.r ("WAIT").
  OUTPUT TO VALUE({1}) NO-ECHO PAGED PAGE-SIZE 81.
  RUN ProcesoImprimir.
  OUTPUT CLOSE.        
  RUN _SetCurs.r ("ARROW").
  IF W_Dispositivo = "P" THEN  
    RUN Pantalla IN W_Manija (INPUT {1}).
  ELSE                                                  
    IF W_Dispositivo EQ "I" THEN
      RUN adecomm/_osprint.r ( INPUT  ?, INPUT {1},INPUT {2},INPUT  1,INPUT  1,
                                         INPUT  99999,OUTPUT W_sw).
  IF W_Dispositivo NE "A" THEN
     OS-DELETE VALUE({1}).
