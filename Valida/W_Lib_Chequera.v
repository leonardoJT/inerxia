IF Inserted.Num_Inicial LE 0 OR Inserted.Num_Final LE 0 THEN DO:
   MESSAGE "N�mero Inicial y Final No" SKIP(0)
           "pueden ser Cero (0). Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "Error".
   RETURN ERROR.   
END.
IF Inserted.Num_Inicial GT Inserted.Num_Final THEN DO:
   MESSAGE "N�mero Final No puede ser menor" SKIP(0)
           "que N�mero Inicial. Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "Error".
   RETURN ERROR.   
END.
/*FIND FIRST Ahorros WHERE (/* %JoinFKPK(inserted,Ahorros," = "," and") */
                          /*Inserted.Agencia      = Ahorros.Agencia      AND*/
                          Inserted.Cod_Ahorro = Ahorros.Cod_Ahorro AND
                          Inserted.Cue_Ahorros  = Ahorros.Cue_Ahorros) NO-LOCK NO-ERROR.
     IF NOT AVAILABLE Ahorros THEN DO:
        MESSAGE "No puede Actualizar 'Lib_Chequera' Porque" SKIP(0)
                "'Ahorros' NO EXISTE. Verifique."
                VIEW-AS ALERT-BOX ERROR TITLE "Error".
        RETURN ERROR.
     END.
  */
