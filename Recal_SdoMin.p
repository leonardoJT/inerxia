DEFI VAR W_Fin10 LIKE Mov_Ahorros.Sdo_Dispon.
DEFI VAR W_SiMov AS LOG INIT FALSE.

OUTPUT TO C:\infred\Recal_Trimest.Txt.

DISPLAY "Ced/Nit    Pdcto Cue-Ahorros                          Fin10                sdo. Minimo               SFin.MovAho       Obs"
   WITH WIDTH 150 NO-LABELS NO-BOX.
    
FOR EACH Ahorros WHERE  Ahorros.Estado EQ 1 AND Ahorros.Sdo_Dispon GT 0
                   AND (Ahorros.Cod_Ahorro EQ 2 OR 
                        Ahorros.Cod_Ahorro EQ 5 OR
                        Ahorros.Cod_Ahorro EQ 9) BY Ahorros.Nit:
    IF Ahorros.Fec_Apertura LE DATE(10,10,2006) THEN DO:
       W_Fin10 = Sdo_Minimo.
       W_SiMov = FALSE.
       FOR EACH Mov_Ahorros WHERE 
                Mov_Ahorros.Agencia      EQ Ahorros.Agencia      AND
                Mov_Ahorros.Cod_Ahorro   EQ Ahorros.Cod_Ahorro   AND
                Mov_Ahorros.Cue_Ahorros  EQ Ahorros.Cue_Ahorros  AND
                Mov_Ahorros.Fecha        LE TODAY                AND
                Mov_Ahorros.Nit          EQ Ahorros.Nit NO-LOCK     
                BREAK BY Mov_Ahorros.Nit BY Mov_Ahorros.Cue_Ahorros BY Mov_Ahorros.Fecha BY Mov_Ahorros.Hora:   
           W_SiMov = TRUE.
           IF Mov_Ahorros.Fecha LE DATE(10,10,2006) THEN
              W_Fin10 = Mov_Ahorros.Sdo_Dispon.
           ELSE 
              IF Mov_Ahorros.Sdo_Dispon LT W_Fin10 THEN
                 W_Fin10 = Mov_Ahorros.Sdo_Dispon.           

           IF LAST-OF(Mov_Ahorros.Cue_Ahorros) THEN DO:
              IF W_Fin10 GT Sdo_Minimo THEN DO:
                 DISPLAY Ahorros.Nit Ahorros.Cod_Ahorro Ahorros.Cue_Ahorro W_Fin10 Sdo_Minimo Mov_Ahorros.Sdo_Dispon "GT"
                    WITH WIDTH 150 NO-LABELS NO-BOX.
                 Ahorros.Sdo_Minimo = W_Fin10.
              END.

           END.
       END.
       IF NOT W_SiMov AND W_Fin10 NE Sdo_Minimo THEN
          DISPLAY Ahorros.Nit Ahorros.Cod_Ahorro Ahorros.Cue_Ahorro W_Fin10 Sdo_Minimo "                 "  "NE"
             WITH WIDTH 150 NO-LABELS NO-BOX.
    END.
    
END.
OUTPUT CLOSE.
