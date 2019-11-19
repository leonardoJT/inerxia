IF Inserted.Indicador EQ ? OR Inserted.Indicador LE 0 THEN DO:
   MESSAGE "Código del Indicador no se puede" SKIP
           "omitir o ser nulo. Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
END.   

IF Inserted.Nombre EQ "" THEN DO:
   MESSAGE "Nombre del Indicador no se" SKIP
           "puede omitir. Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
END.

IF Inserted.Tasa EQ ? OR Inserted.Valor EQ ? OR Inserted.Base EQ ? THEN DO:
   MESSAGE "Tasa, Valor o Base no pueden" SKIP
           "ser nulo. Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
END.

IF (Inserted.Tasa EQ 0 AND Inserted.Valor EQ 0) OR (Inserted.Tasa GT 0 AND Inserted.Valor GT 0) THEN DO:
   MESSAGE "Tasa y Valor; debe existir" SKIP
           "una de las dos. Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
END.

IF Inserted.FecVcto LT Inserted.Fecha THEN DO:
   MESSAGE "Fecha Vencimiento No puede ser Mayor" SKIP
           "que Fecha Inicial . Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
END.
