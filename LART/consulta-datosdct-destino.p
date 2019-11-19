FOR EACH ahorros WHERE Ahorros.nit EQ "32429992" AND  Ahorros.Cue_Ahorros EQ "16985" 
                       AND Ahorros.Cod_ahorro EQ 5
                   NO-LOCK  :
      /* DISPLAY Ahorros EXCEPT Ahorros.TRF_Notas   WITH 1 COL .    */
/*               ASSIGN Ahorros.Des_Intereses = 3. */
    DISPLAY Ahorros.Agencia 
            Ahorros.Cod_ahorro 
            Ahorros.Cue_Ahorros 
            Ahorros.Cue_Destino 
            Ahorros.Agencia_Destino
            Ahorros.Pro_Destino
            Ahorros.Des_Intereses
        WITH 1 COL.
END.
