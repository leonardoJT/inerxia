{incluido\iprmt_rpt.i}
{incluido\Variable.i "SHARED"}

DEFINE VAR qbf-count AS INTEGER.
DEFINE VAR qbf-governor AS INTEGER.
DEFINE VAR qbf-govcnt AS INTEGER.
DEFINE VAR qbf-loop AS INTEGER.
DEFINE VAR qbf-time AS INTEGER.
DEFINE VAR viCpt1 AS INTEGER.
DEFINE VAR viCpt2 AS INTEGER.
DEFINE VAR viAge1 AS INTEGER.
DEFINE VAR viAge2 AS INTEGER.
DEFINE VAR viConAnt AS INTEGER.
DEFINE VAR viConAct AS INTEGER.
DEFINE VAR vcComp AS CHARACTER.

DEFINE BUFFER Mov_Contable FOR Mov_Contable.

qbf-count = 0.
qbf-governor = 0.
qbf-time = TIME.

viCpt1 = INTEGER(pc01).
viCpt2 = INTEGER(pc02).
viAge1 = INTEGER(pc03).
viAge2 = INTEGER(pc04).

DEFINE VAR W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Informe de Consecutivos de Comprobantes".
DEFINE VAR LisEx AS CHARACTER.
DEFINE VAR Ct AS DECIMAL.
DEFINE VAR Cma AS CHARACTER FORMAT "X" INITIAL ";".

DEFINE TEMP-TABLE IEx
    FIELD NLinea AS INTEGER FORMAT "999999"
    FIELD Linea  AS CHARACTER FORMAT "X(150)".

DEFINE VAR W_Naturaleza AS CHARACTER INITIAL "DB".
DEFINE VAR W_CtrNat AS LOGICAL.
DEFINE VAR L_CC AS LOGICAL INITIAL YES.
DEFINE VAR W_NitEnc AS CHARACTER.
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
    FIELD TS_Age AS INTEGER
    FIELD TS_Doc AS INTEGER
    FIELD TS_Com AS INTEGER
    FIELD TS_Db AS DECIMAL
    FIELD TS_Cr AS DECIMAL
    FIELD TS_Fec AS DATE
    FIELD TS_Hor AS INTEGER
    FIELD TS_NO AS LOGICAL INITIAL YES
    INDEX idxTSDoc TS_Age TS_Com TS_Doc.

DEFINE TEMP-TABLE tempmov
    FIELD agencia AS INTEGER
    FIELD comprobante AS INTEGER
    FIELD fec_contable AS DATE
    FIELD num_documento AS INTEGER
    FIELD db AS DECIMAL
    FIELD cr AS DECIMAL
    INDEX idxtodo agencia comprobante num_documento.

EMPTY TEMP-TABLE TSDoc.
EMPTY TEMP-TABLE tempmov.

TotCre = 0.
TotDeb = 0.
TotActIni = 0.
TotActFin = 0.
TotPasIni = 0.
TotPasFin = 0.
TotPtrIni = 0.
TotPtrFin = 0.
TotResIni = 0.
TotResFin = 0.

RUN Tabla_Temporal.

IF P_NomArchivo = "DEFAULT" THEN
    ASSIGN P_NomArchivo = W_PathSpl + "\ConseComprobantes.txt".

OUTPUT TO VALUE(P_NomArchivo) PAGED PAGE-SIZE VALUE(P_NumLineas).

VIEW FRAME F-Encabezado.
RUN ProcesoImprimir. /* oakley */

PROCEDURE Tabla_Temporal:
IF (viCpt1 = 0 AND viCpt2 = 999) AND (viAge1 = 0 AND viAge2 = 999) AND (pdt01 <> ? AND pdt02 <> ?) THEN DO:
    FOR EACH Mov_Contable WHERE Mov_Contable.Fec_Contable >= pdt01
                            AND Mov_Contable.Fec_Contable <= pdt02 NO-LOCK BY mov_contable.agencia
                                                                           BY mov_contable.comprobante
                                                                           BY mov_contable.num_documento:
        CREATE tempmov.
        tempmov.agencia = mov_contable.agencia.
        tempmov.comprobante = mov_contable.comprobante.
        tempmov.fec_contable = mov_contable.fec_contable.
        tempmov.num_documento = mov_contable.num_documento.
        tempmov.db = mov_contable.db.
        tempmov.cr = mov_contable.cr.
    END.

    FOR EACH Mov_Contable2 WHERE Mov_Contable2.Fec_Contable >= pdt01
                             AND Mov_Contable2.Fec_Contable <= pdt02 NO-LOCK BY mov_contable2.agencia
                                                                             BY mov_contable2.comprobante
                                                                             BY mov_contable2.num_documento:
        CREATE tempmov.
        tempmov.agencia = mov_contable2.agencia.
        tempmov.comprobante = mov_contable2.comprobante.
        tempmov.fec_contable = mov_contable2.fec_contable.
        tempmov.num_documento = mov_contable2.num_documento.
        tempmov.db = mov_contable2.db.
        tempmov.cr = mov_contable2.cr.
    END.

    FOR EACH tempmov BREAK BY tempmov.agencia 
                           BY tempmov.Comprobante
                           BY tempmov.Num_Documento:
        TotCre = TotCre + tempmov.CR.
        TotDeb = TotDeb + tempmov.DB.

        IF LAST-OF(tempmov.Num_Documento) THEN DO:
            CREATE TSDoc.
            TSDoc.TS_Com = tempmov.Comprobante.
            TSDoc.TS_Age = tempmov.Agencia.
            TSDoc.TS_Doc = tempmov.Num_Documento.
            TSDoc.TS_CR = TotCre.
            TSDoc.TS_DB = TotDeb.
            TSDoc.TS_Fec = tempmov.Fec_Contable.
            
            TotCre = 0.
            TotDeb = 0.
        END.
    END.
END.
ELSE DO:
    IF viCpt1 = viCpt2 AND viAge1 = viAge2 AND pdt01 = pdt02 THEN DO:
        FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante  = viCpt1
                                AND Mov_Contable.Fec_Contable = pdt01
                                AND Mov_Contable.Agencia = viAge1 NO-LOCK BREAK BY Mov_Contable.agencia
                                                                                BY Mov_Contable.Comprobante
                                                                                BY Mov_Contable.Num_Documento:
            TotCre = TotCre + Mov_Contable.CR.
            TotDeb = TotDeb + Mov_Contable.DB.

            IF LAST-OF(Mov_Contable.Num_Documento) THEN DO:
                CREATE TSDoc.
                TSDoc.TS_Com = Mov_Contable.Comprobante.
                TSDoc.TS_Age = Mov_Contable.Agencia.
                TSDoc.TS_Doc = Mov_Contable.Num_Documento.
                TSDoc.TS_CR = TotCre.
                TSDoc.TS_DB = TotDeb.
                TSDoc.TS_Fec = Mov_Contable.Fec_Contable.

                TotCre = 0.
                TotDeb = 0.
            END.
        END.

        FOR EACH Mov_Contable2 WHERE Mov_Contable2.Comprobante  = viCpt1
                                 AND Mov_Contable2.Fec_Contable = pdt01
                                 AND Mov_Contable2.Agencia = viAge1 NO-LOCK BREAK BY Mov_Contable2.agencia
                                                                                  BY Mov_Contable2.Comprobante
                                                                                  BY Mov_Contable2.Num_Documento:
            TotCre = TotCre + Mov_Contable2.CR.
            TotDeb = TotDeb + Mov_Contable2.DB.

            IF LAST-OF(Mov_Contable2.Num_Documento) THEN DO:
                CREATE TSDoc.
                TSDoc.TS_Com = Mov_Contable2.Comprobante.
                TSDoc.TS_Age = Mov_Contable2.Agencia.
                TSDoc.TS_Doc = Mov_Contable2.Num_Documento.
                TSDoc.TS_CR = TotCre.
                TSDoc.TS_DB = TotDeb.
                TSDoc.TS_Fec = Mov_Contable2.Fec_Contable.

                TotCre = 0.
                TotDeb = 0.
            END.
        END.
    END.
    ELSE DO:
        IF viCpt1 = viCpt2 AND viAge1 = viAge2 THEN DO:
            FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante = viCpt1
                                    AND Mov_Contable.Fec_Contable >= pdt01
                                    AND Mov_Contable.Fec_Contable <= pdt02
                                    AND Mov_Contable.Agencia = viAge1 NO-LOCK BREAK BY Mov_Contable.agencia
                                                                                    BY Mov_Contable.Comprobante
                                                                                    BY Mov_Contable.Num_Documento:
                TotCre = TotCre + Mov_Contable.CR.
                TotDeb = TotDeb + Mov_Contable.DB.

                IF LAST-OF(Mov_Contable.Num_Documento) THEN DO:
                    CREATE TSDoc.
                    TSDoc.TS_Com = Mov_Contable.Comprobante.
                    TSDoc.TS_Age = Mov_Contable.Agencia.
                    TSDoc.TS_Doc = Mov_Contable.Num_Documento.
                    TSDoc.TS_CR  = TotCre.
                    TSDoc.TS_DB  = TotDeb.
                    TSDoc.TS_Fec = Mov_Contable.Fec_Contable.

                    TotCre = 0.
                    TotDeb = 0.
                END.
            END.

            FOR EACH Mov_Contable2 WHERE Mov_Contable2.Comprobante = viCpt1
                                     AND Mov_Contable2.Fec_Contable >= pdt01
                                     AND Mov_Contable2.Fec_Contable <= pdt02
                                     AND Mov_Contable2.Agencia = viAge1 NO-LOCK BREAK BY Mov_Contable2.agencia
                                                                                      BY Mov_Contable2.Comprobante
                                                                                      BY Mov_Contable2.Num_Documento:
                TotCre = TotCre + Mov_Contable2.CR.
                TotDeb = TotDeb + Mov_Contable2.DB.

                IF LAST-OF(Mov_Contable2.Num_Documento) THEN DO:
                    CREATE TSDoc.
                    TSDoc.TS_Com = Mov_Contable2.Comprobante.
                    TSDoc.TS_Age = Mov_Contable2.Agencia.
                    TSDoc.TS_Doc = Mov_Contable2.Num_Documento.
                    TSDoc.TS_CR  = TotCre.
                    TSDoc.TS_DB  = TotDeb.
                    TSDoc.TS_Fec = Mov_Contable2.Fec_Contable.

                    TotCre = 0.
                    TotDeb = 0.
                END.
            END.
        END.
        ELSE DO:
            IF viAge1 = viAge2 THEN DO:
                FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante >= viCpt1
                                        AND Mov_Contable.Comprobante <= viCpt2
                                        AND Mov_Contable.Fec_Contable >= pdt01
                                        AND Mov_Contable.Fec_Contable <= pdt02
                                        AND Mov_Contable.Agencia = viAge1 NO-LOCK BY mov_contable.agencia
                                                                                  BY mov_contable.comprobante
                                                                                  BY mov_contable.num_documento:
                    CREATE tempmov.
                    tempmov.agencia = mov_contable.agencia.
                    tempmov.comprobante = mov_contable.comprobante.
                    tempmov.fec_contable = mov_contable.fec_contable.
                    tempmov.num_documento = mov_contable.num_documento.
                    tempmov.db = mov_contable.db.
                    tempmov.cr = mov_contable.cr.
                END.

                FOR EACH Mov_Contable2 WHERE Mov_Contable2.Comprobante >= viCpt1
                                         AND Mov_Contable2.Comprobante <= viCpt2
                                         AND Mov_Contable2.Fec_Contable >= pdt01
                                         AND Mov_Contable2.Fec_Contable <= pdt02
                                         AND Mov_Contable2.Agencia = viAge1 NO-LOCK BY mov_contable2.agencia
                                                                                    BY mov_contable2.comprobante
                                                                                    BY mov_contable2.num_documento:
                    CREATE tempmov.
                    tempmov.agencia = mov_contable2.agencia.
                    tempmov.comprobante = mov_contable2.comprobante.
                    tempmov.fec_contable = mov_contable2.fec_contable.
                    tempmov.num_documento = mov_contable2.num_documento.
                    tempmov.db = mov_contable2.db.
                    tempmov.cr = mov_contable2.cr.
                END.

                FOR EACH tempmov BREAK BY tempmov.agencia
                                       BY tempmov.Comprobante
                                       BY tempmov.Num_Documento:
                    TotCre = TotCre + tempmov.CR.
                    TotDeb = TotDeb + tempmov.DB.

                    IF LAST-OF(tempmov.Num_Documento) THEN DO:
                        CREATE TSDoc.
                        TSDoc.TS_Com = tempmov.Comprobante.
                        TSDoc.TS_Age = tempmov.Agencia.
                        TSDoc.TS_Doc = tempmov.Num_Documento.
                        TSDoc.TS_CR = TotCre.
                        TSDoc.TS_DB = TotDeb.
                        TSDoc.TS_Fec = tempmov.Fec_Contable.

                        TotCre = 0.
                        TotDeb = 0.
                    END.
                END.
            END.
            ELSE DO:
                IF viCpt1 = viCpt2 THEN DO:
                    FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante = viCpt1
                                            AND Mov_Contable.Fec_Contable >= pdt01
                                            AND Mov_Contable.Fec_Contable <= pdt02
                                            AND Mov_Contable.Agencia >= viAge1
                                            AND Mov_Contable.Agencia <= viAge2 NO-LOCK BREAK BY Mov_Contable.agencia
                                                                                             BY Mov_Contable.Comprobante
                                                                                             BY Mov_Contable.Num_Documento:
                        TotCre = TotCre + Mov_Contable.CR.
                        TotDeb = TotDeb + Mov_Contable.DB.

                        IF LAST-OF(Mov_Contable.Num_Documento) THEN DO:
                            CREATE TSDoc.
                            TSDoc.TS_Com = Mov_Contable.Comprobante.
                            TSDoc.TS_Age = Mov_Contable.Agencia.
                            TSDoc.TS_Doc = Mov_Contable.Num_Documento.
                            TSDoc.TS_CR = TotCre.
                            TSDoc.TS_DB = TotDeb.
                            TSDoc.TS_Fec = Mov_Contable.Fec_Contable.

                            TotCre = 0.
                            TotDeb = 0.
                        END.
                    END.

                    FOR EACH Mov_Contable2 WHERE Mov_Contable2.Comprobante = viCpt1
                                             AND Mov_Contable2.Fec_Contable >= pdt01
                                             AND Mov_Contable2.Fec_Contable <= pdt02
                                             AND Mov_Contable2.Agencia >= viAge1
                                             AND Mov_Contable2.Agencia <= viAge2 NO-LOCK BREAK BY Mov_Contable2.agencia
                                                                                               BY Mov_Contable2.Comprobante
                                                                                               BY Mov_Contable2.Num_Documento:
                        TotCre = TotCre + Mov_Contable2.CR.
                        TotDeb = TotDeb + Mov_Contable2.DB.

                        IF LAST-OF(Mov_Contable2.Num_Documento) THEN DO:
                            CREATE TSDoc.
                            TSDoc.TS_Com = Mov_Contable2.Comprobante.
                            TSDoc.TS_Age = Mov_Contable2.Agencia.
                            TSDoc.TS_Doc = Mov_Contable2.Num_Documento.
                            TSDoc.TS_CR = TotCre.
                            TSDoc.TS_DB = TotDeb.
                            TSDoc.TS_Fec = Mov_Contable2.Fec_Contable.

                            TotCre = 0.
                            TotDeb = 0.
                        END.
                    END.

                END.
            END.
        END.
    END.
END.

END PROCEDURE.

PROCEDURE ProcesoImprimir:
{Incluido\RepEncabezado.i}

DEFINE VAR W_EstadoInf AS CHARACTER FORMAT "X(8)".
DEFINE VAR Nom_Cencosto AS CHARACTER FORMAT "X(2)".

W_Reporte   = "REPORTE   : CONSECUTIVOS ENTRE : " + STRING(pdt01,"99/99/9999") + " y " + STRING(pdt02,"99/99/9999").

DEFINE VAR CtaAnt AS CHARACTER FORMAT "X".
DEFINE VAR TT_Db AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TT_Cr AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TotIni AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99".
DEFINE VAR TotFin AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99".
DEFINE VARIABLE vcNom AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcDesc AS CHARACTER NO-UNDO.

VIEW FRAME f-ftr.
    
FOR EACH TSDoc BREAK BY TSDoc.TS_Age
                     BY TSDoc.TS_Com
                     BY TSDoc.TS_Doc:
    FORM TSDoc.TS_Age   COLUMN-LABEL "Age"          FORMAT "999"
         vcComp         COLUMN-LABEL "Comprobante"  FORMAT "X(30)"
         TSDoc.TS_Doc   COLUMN-LABEL "DOCUMENTO"    FORMAT "     9999999"
         TSDoc.TS_Db    COLUMN-LABEL "DÉBITO"       FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99"
         TSDoc.TS_Cr    COLUMN-LABEL "CREDITO"      FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99"
         TSDoc.TS_Fec   COLUMN-LABEL "FECHA"        FORMAT "99/99/9999"
        WITH FRAME FRep DOWN COLUMN 1 WIDTH 132 NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.

    IF FIRST-OF(TSDoc.TS_Com) THEN DO:
        viConAnt = TSDoc.TS_Doc.

        FIND Comprobantes WHERE Comprobantes.Agencia = TSDoc.TS_Age
                            AND Comprobantes.Comprobante = TSDoc.TS_Com NO-LOCK NO-ERROR.
        IF AVAILABLE Comprobantes THEN
            vcComp = (STRING(Comprobantes.Comprobante,"999")) + " - " + Comprobantes.Nombre.
    END.

    TT_Db = TT_Db + TSDoc.TS_DB.
    TT_CR = TT_CR + TSDoc.TS_CR.

    IF viConAnt <> TSDoc.TS_Doc THEN DO:
        IF (viConAnt + 1) < TSDoc.TS_Doc THEN DO:
            REPEAT WHILE viConAnt < TSDoc.TS_Doc:
                viConAnt = viConAnt + 1.

                IF viConAnt = TSDoc.TS_Doc THEN
                    LEAVE.

                DISPLAY TSDoc.TS_Age
                        vcComp
                        viConAnt @ TSDoc.TS_Doc
                    WITH FRAME FRep.

                DOWN WITH FRAME FRep.
            END.
        END.

        viConAnt = TSDoc.TS_Doc.
    END.

    DISPLAY TSDoc.TS_Age
            vcComp
            TSDoc.TS_Doc
            TSDoc.TS_Db
            TSDoc.TS_Cr
            TSDoc.TS_Fec
        WITH FRAME FRep.

    IF LAST-OF(TSDoc.TS_Com) THEN DO:
        DOWN 1 WITH FRAME FRep.
        DISPLAY "      ---------------------" @ TSDoc.TS_Db
                "      ---------------------" @ TSDoc.TS_Cr
            WITH FRAME FRep.

        DOWN 1 WITH FRAME FRep.

        DISPLAY ("T.Comp.: " + STRING(TSDoc.TS_Com,"999")) @ TSDoc.TS_Doc
                TT_Db @ TSDoc.TS_Db
                TT_Cr @ TSDoc.TS_Cr
            WITH  FRAME FRep.

        DOWN 1 WITH FRAME FRep.

        TT_Db = 0.
        TT_Cr = 0.
    END.

    DOWN WITH FRAME FRep.
END.

PAGE.
OUTPUT CLOSE.

END PROCEDURE.

RETURN.
