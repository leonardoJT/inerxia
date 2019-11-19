IF Inserted.Cod_Producto EQ 0 THEN DO: 
   MESSAGE "Código del Producto no" SKIP
           "se puede omitir. Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
END.

