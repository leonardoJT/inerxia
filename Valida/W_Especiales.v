  IF Inserted.Nit EQ ? OR LENGTH(TRIM(Inserted.Nit)) EQ 0
  THEN DO:
      MESSAGE "La Cédula del Cliente es Obligatoria." VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
  END.
  
  IF Inserted.Cuota LE 0 
  THEN DO:
      MESSAGE "El Valor de la Cuota Debe Ser Mayor que 0." VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
  END.
  
  IF Inserted.Plazo LE 0 
  THEN DO:
      MESSAGE "El Valor del Plazo Debe Ser Mayor que 0." VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
  END.  
  
  IF Inserted.Cantidad LE 0 
  THEN DO:
      MESSAGE "El Valor del Multiplicador Debe Ser Mayor que 0." VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
  END.  
