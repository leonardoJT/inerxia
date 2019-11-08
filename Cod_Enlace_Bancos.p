DEFINE TEMP-TABLE Ctas_bcos
   FIELD w_cta       LIKE  Cuentas.cuenta
   FIELD w_nom       LIKE  cuentas.nombre
   FIELD w_codc      LIKE  Operacion.Cod_Compensa
   FIELD w_age       LIKE  Agencias.age
   FIELD w_flags     AS LOGICAL INITIAL FALSE
   INDEX TmpCta w_age w_cta.
INPUT FROM "c:\sps_sfg\objetos\bancos.csv".
REPEAT:
  CREATE Ctas_bcos.
  IMPORT DELIMITER ","  Ctas_bcos.w_cta Ctas_bcos.w_nom Ctas_bcos.w_age NO-ERROR.
  IF Ctas_bcos.w_cta = "" THEN DELETE Ctas_bcos.
END.
FOR EACH Ctas_Bcos:
    FIND FIRST cuentas WHERE cuentas.cuenta = Ctas_bcos.w_cta NO-ERROR.
    IF AVAILABLE(cuentas) THEN DO:
        IF cod_enlace NE 0 THEN
           MESSAGE "La cuenta " cuentas.cuenta " esta marcada tantao para la agencia " cod_enlace " y la agencia : "  w_age 
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ELSE  Cuentas.Cod_enlace = w_age .
    END.
END.
