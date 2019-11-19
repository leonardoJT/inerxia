IF Inserted.Nit EQ ""
OR Inserted.Nit EQ STRING(0) THEN DO:
   MESSAGE "El Nit debe ser diferente de cero o blanco"
      VIEW-AS ALERT-BOX ERROR TITLE "ERROR ENTRADA DE DATOS".
   RETURN ERROR.
END.

IF LENGTH(Inserted.Nombre) EQ 0 THEN DO:
   MESSAGE "El Nombre debe ser diferente de blanco"
      VIEW-AS ALERT-BOX ERROR TITLE "ERROR ENTRADA DE DATOS".
   RETURN ERROR.
END.

IF Inserted.Fec_Expedicion GT TODAY THEN DO:
   MESSAGE "La Fecha de Expedición debe ser mayor que la del día"
      VIEW-AS ALERT-BOX ERROR TITLE "ERROR ENTRADA DE DATOS".
   RETURN ERROR.
END.
