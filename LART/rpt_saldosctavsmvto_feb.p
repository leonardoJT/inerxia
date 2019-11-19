DEFINE VAR wdb LIKE Sal_Cuenta.Db[1] no-undo.
DEFINE VAR wcr LIKE Sal_Cuenta.Cr[1] NO-UNDO.
DEFINE VAR wre AS RECID.
DEFINE VAR wsalida AS CHAR FORMAT "X(200)".
DEFINE VAR wtitulo AS CHAR FORMAT "X(200)".
OUTPUT TO c:\INFO_fodun\veri_sal_cta_feb2011.csv.
ASSIGN wtitulo = "registro;agencia;Cuenta;Cen_Costos;Db[2]-Cuenta;wdb-Mvto;cr[2]-Cuenta;wcr-Movto".
        DISPLAY wtitulo
            NO-LABEL WITH WIDTH  400.

FOR EACH Sal_Cuenta WHERE Sal_Cuenta.Ano = 2011 NO-LOCK BY Sal_Cuenta.agencia 
                          BY Sal_Cuenta.Cen_Costos BY Sal_Cuenta.Cuenta  :
    ASSIGN wdb = 0.
    ASSIGN wcr = 0.
    FOR EACH    Mov_Contable WHERE Mov_Contable.agencia EQ Sal_Cuenta.agencia AND
                Mov_Contable.Cuenta EQ Sal_Cuenta.Cuenta AND Mov_Contable.Cen_Costos EQ Sal_Cuenta.Cen_Costos AND
                year(Mov_Contable.Fec_Contable) EQ 2011 AND  MONTH(Mov_Contable.Fec_Contable) EQ 2 NO-LOCK:
        ASSIGN wdb = wdb + Mov_Contable.Db 
               wcr = wcr + Mov_Contable.Cr.
    END.
    IF ( Sal_Cuenta.Db[2] NE WDB ) OR  ( Sal_Cuenta.Cr[2] NE WCR)   THEN  DO:
        wre = RECID(Sal_Cuenta).
        ASSIGN wsalida = STRING(wre,"99999999") + ";" + 
               string(Sal_Cuenta.agencia) + ";" + 
               Sal_Cuenta.Cuenta  + ";" +  string(Sal_Cuenta.Cen_Costos)  + ";" + 
               string(Sal_Cuenta.Db[2], "999999999999999.99") + ";" +
               string(wdb, "999999999999999.99") + ";" +
               string(Sal_Cuenta.cr[2], "999999999999999.99") + ";" +
               string(wcr, "999999999999999.99").
               ASSIGN wdb = 0.
               ASSIGN wcr = 0.
        DISPLAY wsalida
            NO-LABEL WITH WIDTH  400.
    END.
    ASSIGN wdb = 0.
    ASSIGN wcr = 0.

END.
OUTPUT CLOSE.
