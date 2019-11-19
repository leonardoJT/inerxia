FIND Cfg_varios WHERE Cfg_Varios.Tipo = Inserted.Tipo NO-LOCK NO-ERROR.
IF NOT AVAILABLE(Cfg_Varios) THEN DO:
   MESSAGE "Tipo no existe en Configuración" SKIP
           "Varios. Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
END. 

IF Inserted.Codigo EQ 0 THEN DO:
   MESSAGE "Código no se puede omitir" SKIP
           "o ser nulo. Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
END.

IF Inserted.Descripcion EQ "" THEN DO:
   MESSAGE "Campo Descripción no se" SKIP
           "puede omitir. Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
END.
