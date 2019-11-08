/*Programa Cta_Exenta.P...Marca Cta-Exenta en ahorros*/

DEFI VAR W_SiExe AS LOG INIT FALSE.

OUTPUT TO C:\Infred\Sin_CtaExenta.Txt.

FOR EACH Ahorros WHERE Ahorros.Tip_Ahorro EQ 1 AND Estado EQ 1
                   AND Sdo_Dispon GT 0  NO-LOCK
                   BREAK BY Ahorros.Nit BY Ahorros.Cod_Ahorro:
    IF FIRST-OF(Ahorros.Nit) THEN
       W_SiExe = FALSE.

    IF Exento_3xm THEN
       W_SiExe = TRUE.

    IF LAST-OF(Ahorros.Nit) AND NOT W_SiExe THEN
       DISPLAY Ahorros.Agencia Ahorros.Nit Fec_Apertura.
END.

OUTPUT CLOSE.
