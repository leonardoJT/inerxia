IF Inserted.Cta_OrigenAj EQ "" OR Inserted.Cta_CorrAjuste EQ "" OR Inserted.Cta_Ajuste EQ "" 
   THEN DO:
   MESSAGE "Las Cuentas Origen del Ajuste y Ajustes por" SKIP
           "Inflación no se pueden omitir. Verifique"
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
END.

IF Inserted.Cta_OrigenAj EQ "?" OR Inserted.Cta_CorrAjuste   EQ "?" OR
   Inserted.Cta_Ajuste   EQ "?" OR Inserted.Cta_CorrAxIAju EQ "?" OR
   Inserted.Cta_AxIAju  EQ "?" OR Inserted.Cta_SucAge  EQ "?" OR
   Inserted.Cta_Sobra    EQ "?"                           THEN DO:
   MESSAGE "Las Cuentas Deben ser" SKIP
           "validas, no nulas. Verifique"
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
END.

IF Inserted.Base EQ ? OR Inserted.Cod_Formato EQ ? OR 
   Inserted.Por_Distribucion EQ ?               THEN DO:
   MESSAGE "Porcentaje de Distribución, Base y" SKIP
           "Formato no pueden ser nulos. Verifique"
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
END.   
