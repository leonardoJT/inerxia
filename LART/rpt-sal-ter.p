DEFINE VAR wsalida AS CHAR FORMAT "X(300)" .
DEFINE VAR wtitulo AS CHAR FORMAT "X(300)" .
OUTPUT TO c:\INFO_fodun\sal_ter.csv.
ASSIGN wtitulo = "agencia;Cen_Costos;Sdo_Inicial;".
ASSIGN wtitulo = wtitulo + "Db[1];Db[2];Db[3];Db[4];Db[5];".
ASSIGN wtitulo = wtitulo + "Cr[1];Cr[2];Cr[3];Cr[4];Cr[5]". 
DISPLAY wtitulo WITH WIDTH 550 NO-LABEL. 
FOR EACH Anexos WHERE   Anexos.Ano EQ 2011 AND
                        Anexos.Cuenta EQ "24650501" 
     NO-LOCK:
    ASSIGN wsalida = STRING(agencia) + ";" + string(cen_Costo) + ";" + STRING(Sdo_Inicial,"999999999999.99") + ";".
    ASSIGN wsalida = wsalida + STRING(Db[1],"999999999999.99") + ";".
    ASSIGN wsalida = wsalida + STRING(Db[2],"999999999999.99") + ";".
    ASSIGN wsalida = wsalida + STRING(Db[3],"999999999999.99") + ";".
    ASSIGN wsalida = wsalida + STRING(Db[4],"999999999999.99") + ";".
    ASSIGN wsalida = wsalida + STRING(Db[5],"999999999999.99") + ";".
    ASSIGN wsalida = wsalida + STRING(Cr[1],"999999999999.99") + ";".
    ASSIGN wsalida = wsalida + STRING(Cr[2],"999999999999.99") + ";".
    ASSIGN wsalida = wsalida + STRING(Cr[3],"999999999999.99") + ";".
    ASSIGN wsalida = wsalida + STRING(Cr[4],"999999999999.99") + ";".
    ASSIGN wsalida = wsalida + STRING(Cr[5],"999999999999.99").

    DISPLAY wsalida WITH WIDTH 600 NO-LABEL. 
/*     DISPLAY Anexos.agencia                                                   */
/*             Anexos.Cen_Costos                                                */
/*             Anexos.Sdo_Inicial                                               */
/*             Anexos.Db[1] Anexos.Db[2] Anexos.Db[3] Anexos.Db[4] Anexos.Db[5] */
/*             Anexos.Cr[1] Anexos.Cr[2] Anexos.Cr[3] Anexos.Cr[4] Anexos.Cr[5] */
/*     WITH WIDTH 500.                                                          */
END.
