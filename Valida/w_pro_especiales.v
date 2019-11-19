  IF Inserted.Cod_Producto LE 0 THEN DO:
      MESSAGE "Código de Producto Invalido" VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
  END.

  IF LENGTH(TRIM(Inserted.Nom_Producto)) = 0 THEN DO:
      MESSAGE "Nombre de Producto Invalido" VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
  END.

  IF Inserted.Plazo_Minimo LE 0 THEN DO:
      MESSAGE "Plazo Inicial Debe Ser Mayor a 0" VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
  END.

  IF Inserted.Plazo_Maximo LT Inserted.Plazo_Minimo THEN DO:
      MESSAGE "Rango Plazo Máximo Menor que Rango Plazo Inicial" VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
  END.

  IF Inserted.Ran_IniCuota LE 0 THEN DO:
      MESSAGE "Cuota Inicial Debe Ser Mayor a 0" VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
  END.

  IF Inserted.Ran_FinCuota LT Inserted.Ran_IniCuota THEN DO:
      MESSAGE "Rango Final Cuota Menor que Rango Inicial Cuota" VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
  END.

  IF Inserted.Id_Contabiliza THEN 
     IF LENGTH(TRIM(Inserted.Cta_Cargos)) = 0 THEN DO:
        MESSAGE "La Cuenta de Cargos es Obligatoria" VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
     END.

  IF LENGTH(TRIM(Inserted.Cta_Recaudos)) = 0 THEN DO:
      MESSAGE "La Cuenta de Recaudos es Obligatoria" VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
  END.

  IF Inserted.Cuo_FijaVar AND (Inserted.Ran_FinCuota NE Inserted.Ran_IniCuota) THEN DO:
      MESSAGE "Selecciono Cuota Fija, Por lo Tanto Rango Inicial y Final de la Cuota Deben Ser Iguales" VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
  END.
