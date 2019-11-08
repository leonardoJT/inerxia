
/*recorre tabla de anexos y hace informe igual a balance prueba pero discriminando por nit*/
DEFINE VARIABLE W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Informe de Consecutivos de Comprobantes".

/*para archivo de excel*/
DEFINE VAR LisEx AS CHARACTER.
DEFINE VAR Ct AS DECIMAL.
DEFINE VAR Cma AS CHARACTER FORMAT "X" INITIAL ";".
DEFINE TEMP-TABLE IEx
    FIELD NLinea AS INTEGER FORMAT "999999"
    FIELD Linea  AS CHARACTER FORMAT "X(150)".
/**/

{incluido/Variable.i "SHARED"}.
{incluido/Varcon.i "SHARED"}.

DEFINE VARIABLE W_Naturaleza LIKE Cuentas.Naturaleza.
DEFINE VARIABLE W_CtrNat LIKE Cuentas.Ctr_Naturaleza.
DEFINE VARIABLE L_CC AS LOGICAL INITIAL YES.
DEFINE VARIABLE choice AS LOGICAL.
DEFINE VAR W_NitEnc LIKE Clientes.Nit.
DEFINE VAR TotDeb AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VAR TotCre AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VAR TotActIni AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR TotPasIni AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR TotPtrIni AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR TotResIni AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR TotActFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR TotPasFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR TotPtrFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR TotResFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".

DEFINE TEMP-TABLE TSDoc
    FIELD TS_Age LIKE Agencias.Agencia
    FIELD TS_Doc LIKE Mov_Contable.Num_Documento
    FIELD TS_Com LIKE Mov_Contable.Comprobante
    FIELD TS_Db AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
    FIELD TS_Cr AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
    FIELD TS_Fec AS DATE
    FIELD TS_Hor AS INTEGER
    FIELD TS_NO AS LOGICAL INITIAL YES.

{incluido/Pantalla_Validacion.i}


PROCEDURE Busca_Cuenta:
    DEFINE INPUT PARAMETER T_ConsCtai LIKE Cuentas.Cuenta.
    DEFINE OUTPUT PARAMETER T_ConsCta LIKE Cuentas.Cuenta.
    DEFINE OUTPUT PARAMETER T_ConsNom LIKE Cuentas.Nombre.

    IF T_ConsCtai NE "" THEN DO:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ T_ConsCtai
                             AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAILABLE(Cuentas) THEN
            ASSIGN T_ConsCta = Cuentas.Cuenta
                   T_ConsNom = Cuentas.Nombre.
    END.

    IF T_ConsCta NE "" OR T_ConsCta NE "?" THEN DO:
        RUN C-Cuentas.r (OUTPUT T_ConsCta,
                         OUTPUT T_ConsNom,
                         OUTPUT W_Naturaleza,
                         OUTPUT W_CtrNat,
                         INPUT "T").

        IF T_ConsCta EQ ? THEN DO:
            FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ T_ConsCta
                                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
            IF NOT AVAILABLE(Cuentas) THEN
                ASSIGN T_ConsCta = ""
                       T_ConsNom = "".
        END.
    END.
END PROCEDURE.


PROCEDURE Habilita_Deshabilita:
    /* En este procedimiento se habilitan o deshabilitan las variables a pedir en pantalla segun el informe que se vaya a ejecutar. */
    ENABLE ALL WITH FRAME F_Valida IN WINDOW W_Pantalla.

    DISABLE Cmb_CenCost
            Cmb_Nivel
            W_Usuario1
            W_Usuario2
            W_NomUsuario1
            W_NomUsuario2
            W_Cuenta1
            W_Cuenta2
            W_NomCuenta1
            W_NomCuenta2
            W_Nit1
            W_NomNit1
            W_Nit2
            W_NomNit2
            W_Base
            W_Porcentaje
        WITH FRAME F_Valida.

    ASSIGN W_Fec1:LABEL IN FRAME F_Valida = "Desde"
           W_Fec2:LABEL IN FRAME F_Valida = "Hasta".
END PROCEDURE.


PROCEDURE Tabla_Temporal:  /* TT */
    DEFINE VAR W_Consecutivo LIKE Mov_Contable.Num_Documento.

    FOR EACH Mov_Contable WHERE Mov_Contable.Agencia GE W_Ag1
                            AND Mov_Contable.Agencia LE W_Ag2
                            AND Mov_Contable.Comprobante GE W_CB1
                            AND Mov_Contable.Comprobante LE W_CB2
                            AND Mov_Contable.Fec_Contable GE W_Fec1
                            AND Mov_Contable.Fec_Contable LE W_Fec2 NO-LOCK BREAK BY Mov_Contable.Agencia
                                                                                  BY Mov_Contable.Comprobante
                                                                                  BY Mov_Contable.Num_Documento:
        IF FIRST-OF(Mov_Contable.Comprobante) THEN
            W_Consecutivo = Mov_Contable.Num_Documento.

        ASSIGN TotCre = TotCre + Mov_Contable.CR
               TotDeb = TotDeb + Mov_Contable.DB.

        IF LAST-OF(Mov_Contable.Num_Documento) THEN DO:
            REPEAT WHILE W_Consecutivo LT Mov_Contable.Num_Documento:
                CREATE TSDoc.
                ASSIGN TSDoc.TS_Com = Mov_Contable.Comprobante
                       TSDoc.TS_Age = Mov_Contable.Agencia
                       TSDoc.TS_Doc = W_Consecutivo
                       TSDoc.TS_No = NO.

                W_Consecutivo = W_Consecutivo + 1.
            END.

            IF W_Consecutivo EQ Mov_Contable.Num_Documento THEN DO:
                CREATE TSDoc.
                ASSIGN TSDoc.TS_Com = Mov_Contable.Comprobante
                       TSDoc.TS_Age = Mov_Contable.Agencia
                       TSDoc.TS_Doc = Mov_Contable.Num_Documento
                       TSDoc.TS_CR = TotCre
                       TSDoc.TS_DB = TotDeb
                       TSDoc.TS_Fec = Mov_Contable.Fec_Contable
                       TSDoc.TS_Hor = Mov_Contable.Hora.

                W_Consecutivo = W_Consecutivo + 1.
            END.

            ASSIGN TotCre = 0
                   TotDeb = 0.
        END.
    END.
END PROCEDURE.


PROCEDURE Proceso_Imprimir:
    DEFINE VAR Listado AS CHARACTER INITIAL "".

    MESSAGE "Desea Sacar las inconsistencias?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice.

    EMPTY TEMP-TABLE TSDoc.
    
    ASSIGN TotCre = 0
           TotDeb = 0
           TotActIni = 0
           TotActFin = 0
           TotPasIni = 0
           TotPasFin = 0
           TotPtrIni = 0
           TotPtrFin = 0
           TotResIni = 0
           TotResFin = 0.

    RUN Tabla_Temporal.

    Listado = w_Pathspl + "L-ENTIDA.lst".

    {incluido\IMPRIMIR.I "listado"}.
END PROCEDURE.


PROCEDURE ProcesoImprimir:
    {Incluido\RepEncabezado.i}

    DEFINE VARIABLE W_EstadoInf AS CHARACTER FORMAT "X(8)" INITIAL "".
    DEFINE VARIABLE Nom_Cencosto AS CHARACTER FORMAT "X(2)" INITIAL "".

    W_Reporte = "REPORTE   : CONSECUTIVOS ENTRE : " + STRING(W_Fec1) + " y " + STRING(W_Fec2) + " Para la Agencia: " + STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(20)").

    /*1         2         3         4         5         6         7*/
    /* 1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
    W_EncColumna = "               DOC                  DEBITO           CREDITO         FECHA     HORA".

    DEFINE VAR CtaAnt AS CHARACTER FORMAT "X".
    DEFINE VAR TT_Db AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
    DEFINE VAR TT_Cr AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
    DEFINE VAR TotIni AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
    DEFINE VAR TotFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
    DEFINE VAR Hora AS CHARACTER FORMAT "X(10)".

    VIEW FRAME F-Encabezado.
    VIEW FRAME f-ftr.

    EMPTY TEMP-TABLE IEx.
    
    CREATE IEx.
    ASSIGN Ct = Ct + 1
           IEx.NLinea = Ct
           IEx.Linea = "COMPROBANTE" + Cma + "DOCUMENTO" + Cma + "DEBITO" + Cma + "CREDITO" + Cma + "FECHA" + Cma + "HORA".

    FOR EACH TSDoc BREAK BY TSDoc.TS_Age
                         BY TSDoc.TS_Com
                         BY TSDoc.TS_Doc:
        IF FIRST-OF(TSDoc.TS_Age) THEN DO:
            FIND FIRST Agencias WHERE Agencias.Agencia EQ TSDoc.TS_Age NO-LOCK NO-ERROR.
            IF AVAILABLE Agencias THEN DO:
                DISPLAY Agencias.Agencia AT 1
                        Agencias.Nombre  AT 5
                    WITH FRAME F_Age WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.

                CREATE IEx.
                ASSIGN Ct = Ct + 1
                       IEx.NLinea = Ct
                       IEx.Linea = STRING(Agencias.Agencia) + Cma + Agencias.Nombre.
            END.
        END.

        IF FIRST-OF(TSDoc.TS_Com) THEN DO:
            FIND FIRST Comprobantes WHERE Comprobantes.Agencia EQ TSDoc.TS_Age
                                      AND Comprobantes.Comprobante EQ TSDoc.TS_Com NO-LOCK NO-ERROR.
            IF AVAILABLE Comprobantes THEN DO:
                DISPLAY Comprobantes.Comprobante AT 1
                        Comprobantes.Nombre AT 5
                    WITH FRAME F_Com WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
            END.
        END.

        ASSIGN TT_Db = TT_Db + TSDoc.TS_DB
               TT_CR = TT_CR + TSDoc.TS_CR.

        IF TSDoc.TS_No EQ YES THEN DO:
            Hora = STRING(TSDoc.TS_Hor,"hh:mm:ss").

            DISPLAY TSDoc.TS_Doc AT 15
                    TSDoc.TS_Db AT 35
                    TSDoc.TS_Cr AT 50
                    TSDoc.TS_Fec AT 70
                    Hora AT 82
                WITH WIDTH 132 FRAME F_Mov USE-TEXT STREAM-IO NO-LABELS NO-BOX.

            /*informe excel*/
            CREATE IEx.
            ASSIGN Ct = Ct + 1
                   IEx.NLinea = Ct
                   IEx.Linea = STRING(TSDoc.TS_Com) + Cma + STRING(TSDoc.TS_Doc) + Cma + STRING(TSDoc.TS_Db,"->>>>>>>>>>>9.99")  + Cma + STRING(TSDoc.TS_Cr,"->>>>>>>>>>>9.99")  + Cma +
                               STRING(TSDoc.TS_Fec) + Cma + STRING(Hora).
        END.
        ELSE DO:
            IF choice THEN DO:
                Hora = "NoExiste".

                FIND FIRST mov_contable WHERE mov_contable.num_documento = TSDoc.TS_Doc
                                          AND mov_contable.comprobante = TSDoc.TS_Com NO-LOCK NO-ERROR.
                IF AVAILABLE mov_contable THEN DO:
                    FIND FIRST agencias WHERE agencias.agencia = mov_contable.agencia NO-LOCK NO-ERROR.
                    IF AVAILABLE agencias THEN
                        Hora = SUBSTRING(agencias.nombre,6).
                END.

                DISPLAY TSDoc.TS_Doc AT 15
                        Hora AT 27
                    WITH WIDTH 132 FRAME F_Mov2 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
            END.
        END.

        IF LAST-OF(TSDoc.TS_Com) THEN DO:
            DISPLAY "Total Comprobante: " AT 1
                    TSDoc.TS_Com AT 25
                    TT_Db AT 35
                    TT_Cr AT 50 SKIP(1)
                WITH WIDTH 132 FRAME F_Cbt USE-TEXT STREAM-IO NO-LABELS NO-BOX.

            ASSIGN TT_Db = 0
                   TT_Cr = 0.
        END.
    END.

    PAGE.
OUTPUT CLOSE.
END PROCEDURE.


PROCEDURE Imprimir_Excel:
    LisEx = w_Pathspl + "RevConsecutivos.csv".

    OUTPUT TO VALUE(LisEx).
        FOR EACH IEx BY IEx.NLinea:
            PUT IEx.Linea SKIP.
        END.
    OUTPUT CLOSE.

    MESSAGE "Revision Consecutivos para Excel se encuentra en:" SKIP
            LisEx
        VIEW-AS ALERT-BOX INFORMATION.

    EMPTY TEMP-TABLE IEx.
END PROCEDURE.

