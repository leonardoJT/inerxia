DEFINE INPUT  PARAMETER Ind  LIKE Ind_PUB.Codigo.
DEFINE INPUT  PARAMETER Ent  LIKE Cfg_Pub.Ente.
DEFINE INPUT  PARAMETER Mes  AS INTEGER FORMAT "99".

DEFINE INPUT  PARAMETER Cta  LIKE Cuentas.Cuenta.
DEFINE OUTPUT PARAMETER Tot  AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE OUTPUT PARAMETER Met  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE OUTPUT PARAMETER Cum  AS DECIMAL FORMAT "->>>>.99".

DEFINE VAR i AS INTEGER.
DEFINE VAR Sf LIKE Sal_Cuenta.Sal_Inicial.
DEFINE VAR Si LIKE Sal_Cuenta.Sal_Inicial.

DEFINE VAR S1  LIKE Sal_Cuenta.Sal_Inicial.
DEFINE VAR S59 LIKE Sal_Cuenta.Sal_Inicial.
DEFINE VAR S31 LIKE Sal_Cuenta.Sal_Inicial.
DEFINE VAR S32 LIKE Sal_Cuenta.Sal_Inicial.
DEFINE VAR S33 LIKE Sal_Cuenta.Sal_Inicial.

DEFINE VAR S4210 LIKE Sal_Cuenta.Sal_Inicial.
DEFINE VAR S5140 LIKE Sal_Cuenta.Sal_Inicial.

DEFINE VAR S21   LIKE Sal_Cuenta.Sal_Inicial.
DEFINE VAR S14   LIKE Sal_Cuenta.Sal_Inicial.
DEFINE VAR S17   LIKE Sal_Cuenta.Sal_Inicial.
DEFINE VAR S5905 LIKE Sal_Cuenta.Sal_Inicial.
DEFINE VAR S4    LIKE Sal_Cuenta.Sal_Inicial.


CASE Ind:
    /*ind001 retabilidad del capital y reservas*/
    WHEN 01 THEN DO:
        RUN Proc_Cuenta (INPUT "59", OUTPUT S59).
        RUN Proc_Cuenta (INPUT "31", OUTPUT S31).
        RUN Proc_Cuenta (INPUT "32", OUTPUT S32).
        RUN Proc_Cuenta (INPUT "33", OUTPUT S33).
        Tot = (S59 / (S31 + S32 + S33)) * 100.
    END.
    /*ind002 retabilidad del activo total*/
    WHEN 02 THEN DO:
        RUN Proc_Cuenta (INPUT "59", OUTPUT S59).
        RUN Proc_Cuenta (INPUT "1", OUTPUT S59).
        Tot = (S59 / S1) * 100.
    END.
    /*ind003 margen financiero bruto*/
    WHEN 03 THEN DO:
        RUN Proc_Cuenta (INPUT "4210", OUTPUT S4210).
        RUN Proc_Cuenta (INPUT "5140", OUTPUT S5140).
        Tot = ((S4210 - S5140) / S4210) * 100.
    END.
    /*ind050*/
    WHEN 50 THEN DO:
        RUN Proc_Cuenta (INPUT "5905", OUTPUT S5905).
        RUN Proc_Cuenta (INPUT "4", OUTPUT S4).
        Tot = (S5905 / S4) * 100.
    END.
    /*ind052*/
    WHEN 52 THEN DO:
        RUN Proc_Cuenta (INPUT "21", OUTPUT S21).
        RUN Proc_Cuenta (INPUT "14", OUTPUT S14).
        Tot = (S21 / S14) * 100.
    END.
    /*ind053*/
    WHEN 53 THEN DO:
        RUN Proc_Cuenta (INPUT "1", OUTPUT S1).
        RUN Proc_Cuenta (INPUT "14", OUTPUT S14).
        Tot = (S14 / S1) * 100.
    END.
    /*ind054*/
    WHEN 54 THEN DO:
        RUN Proc_Cuenta (INPUT "17", OUTPUT S17).
        RUN Proc_Cuenta (INPUT "1", OUTPUT S1).
        Tot = (S17 / S1) * 100.
    END.
END CASE.


FIND Cfg_PUB WHERE 
     Cfg_PUB.Ente   EQ Ent AND
     Cfg_PUB.Codigo EQ Ind NO-LOCK NO-ERROR.
IF AVAILABLE Cfg_PUB THEN DO:
   IF Cfg_Pub.Usa_Limites THEN 
      ASSIGN Met = Cfg_PUB.Metas[Mes]
             Cum = (Tot / Met).
   ELSE 
       ASSIGN Cum = Tot / 100.
END.




PROCEDURE Proc_Cuenta:
 DEFINE INPUT  PARAMETER Ctaw LIKE Cuentas.Cuenta.
 DEFINE OUTPUT PARAMETER Sdow LIKE Sal_Cuenta.Sal_Inicial.
 FOR EACH Sal_Cuenta WHERE 
     Sal_Cuenta.Cuenta  BEGINS Ctaw AND
     Sal_Cuenta.Ano     EQ Ano NO-LOCK:
     FIND Cuentas WHERE Cuentas.Cuenta EQ Sal_Cuenta.Cuenta NO-LOCK NO-ERROR.
     IF AVAILABLE Cuentas THEN DO: 
         Sf = Sal_Cuenta.Sal_Inicial.
         DO I = 1 TO Mes:
             IF Cuentas.Naturaleza EQ "DB" THEN
                ASSIGN Sf = (Sf + Sal_Cuenta.Db[Mes] - Sal_Cuenta.Cr[Mes]).
             ELSE
                ASSIGN Sf = (Sf + Sal_Cuenta.Cr[Mes] - Sal_Cuenta.Db[Mes]).
         END.
         Sdow = Sdow + Sf.
     END.
 END.
END PROCEDURE.







