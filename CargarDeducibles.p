/* Programa para asiganar deducibles a pro_creditos */
DEFINE VAR i AS INTEGER INITIAL 0.
DEFINE TEMP-TABLE Ctas_Ded
   FIELD w_codc      LIKE  Pro_Credito.cod_credito
   FIELD w_deduc     AS CHARACTER FORMAT "X(4)"
   INDEX TmpCod w_codc w_deduc.
INPUT FROM "C:\MIGRACIONHOY\PlanosConfiguracion\deducibles.csv".
REPEAT:
  CREATE Ctas_Ded.
  IMPORT DELIMITER ","  w_codc w_deduc NO-ERROR.
END.
/* Limpiar el vector actual */
FOR EACH pro_creditos :
 REPEAT i = 1 TO 10:
   pro_creditos.deducible[i] = "".
 END.
END.
OUTPUT TO c:\incon_deducibles.txt.
FOR EACH Ctas_Ded BREAK BY ctas_ded.W_codc:
    IF FIRST-OF(Ctas_Ded.w_codc) THEN i = 1.
    FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = Ctas_ded.w_codc NO-ERROR.
    IF AVAILABLE(pro_creditos) THEN DO:
       ASSIGN pro_creditos.deducible[i] = TRIM(Ctas_Ded.w_deduc)  i = i + 1.
    END.
    ELSE
        PUT "No se encontro el cod_credito " w_codc " para aplicarle el deducible " w_deduc SKIP(0).
    IF LAST-OF(Ctas_Ded.w_codc) THEN i = 0.
END.
OUTPUT CLOSE.


FOR EACH pro_creditos WHERE cod_credito = 15:
    DISPLAY pro_creditos WITH 1 COLUMN.
END.
