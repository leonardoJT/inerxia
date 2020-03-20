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
DEFINE VAR vccuenta AS CHARACTER.

DEFINE BUFFER Mov_Contable FOR Mov_Contable.

qbf-count = 0.
qbf-governor = 0.
qbf-time = TIME.

viCpt1 = INTEGER(pc01).
viCpt2 = INTEGER(pc02).
viAge1 = INTEGER(pc03).
viAge2 = INTEGER(pc04).

DEFINE VAR vdSaldo AS DECIMAL.
DEFINE VAR vdDebito AS DECIMAL.
DEFINE VAR vdCredito AS DECIMAL.
DEFINE VAR vcNomCta AS CHARACTER FORMAT "X(40)".
DEFINE VAR vcNatura AS CHARACTER FORMAT "X(2)".

DEFINE TEMP-TABLE TempCtas
    FIELD Agen AS INTEGER
     FIELD Fecha AS DATE
     FIELD Cpte AS INTEGER
     FIELD Dcto AS INTEGER
     FIELD Debito AS DECIMAL
     FIELD Credito AS DECIMAL
     FIELD Saldo AS DECIMAL
     INDEX x3 agen fecha Cpte Dcto.

DEFINE TEMP-TABLE tempmov
    FIELD agencia AS INTEGER
    FIELD comprobante AS INTEGER
    FIELD fec_contable AS DATE
    FIELD num_documento AS INTEGER
    FIELD db AS DECIMAL
    FIELD cr AS DECIMAL
    INDEX idxtodo agencia fec_contable comprobante num_documento.

EMPTY TEMP-TABLE TempCtas.
EMPTY TEMP-TABLE tempmov.

IF (viCpt1 = 0 AND viCpt2 = 999) AND
   (viAge1 = 0 AND viAge2 = 999) AND
   (pdt01 NE ? AND pdt02 NE ?) THEN DO:
    FOR EACH Mov_Contable WHERE Mov_Contable.Fec_Contable >= pdt01
                            AND Mov_Contable.Fec_Contable <= pdt02 NO-LOCK BY mov_contable.agencia:
        CREATE tempmov.
        tempmov.agencia = mov_contable.agencia.
        tempmov.comprobante = mov_contable.comprobante.
        tempmov.fec_contable = mov_contable.fec_contable.
        tempmov.num_documento = mov_contable.num_documento.
        tempmov.db = mov_contable.db.
        tempmov.cr = mov_contable.cr.
    END.

    FOR EACH mov_contable2 WHERE mov_contable2.fec_contable >= pdt01
                             AND mov_contable2.fec_contable <= pdt02 NO-LOCK BY mov_contable2.agencia:
        CREATE tempmov.
        tempmov.agencia = mov_contable2.agencia.
        tempmov.comprobante = mov_contable2.comprobante.
        tempmov.fec_contable = mov_contable2.fec_contable.
        tempmov.num_documento = mov_contable2.num_documento.
        tempmov.db = mov_contable2.db.
        tempmov.cr = mov_contable2.cr.
    END.

    FOR EACH tempmov BREAK BY tempmov.agencia
                           BY tempmov.fec_contable
                           BY tempmov.Comprobante
                           BY tempmov.Num_Documento:
        IF FIRST-OF(tempmov.Agencia) OR
           FIRST-OF(tempmov.fec_contable) OR
           FIRST-OF(tempmov.Comprobante) OR
           FIRST-OF(tempmov.num_documento) THEN DO:
            vdSaldo = 0.
            vdDebito = 0.
            vdCredito = 0.
        END.

        vdDebito = vdDebito  + tempmov.Db.
        vdCredito = vdCredito + tempmov.Cr.

        IF LAST-OF(tempmov.Agencia) OR
           LAST-OF(tempmov.fec_contable) OR
           LAST-OF(tempmov.Comprobante) OR
           LAST-OF(tempmov.num_documento) THEN DO:
            vdSaldo = vdCredito - vdDebito.

            IF vdSaldo <> 0 THEN DO:
                CREATE TempCtas.
                TempCtas.Agen = tempmov.agencia.
                TempCtas.Fecha = tempmov.fec_contable.
                TempCtas.Cpte = tempmov.Comprobante.
                TempCtas.Dcto = tempmov.num_documento.
                TempCtas.Debito = vdDebito.
                TempCtas.Credito = vdCredito.
                TempCtas.Saldo = vdSaldo.
            END.
        END.
    END.
END.
ELSE DO:
    IF viCpt1 = viCpt2 AND
       viAge1 = viAge2 AND
       pdt01 = pdt02 THEN DO:
        FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante = viCpt1
                                AND Mov_Contable.Fec_Contable = pdt01
                                AND Mov_Contable.Agencia = viAge1 NO-LOCK BREAK BY Mov_Contable.agencia
                                                                                BY Mov_Contable.fec_contable
                                                                                BY Mov_Contable.Comprobante
                                                                                BY Mov_Contable.Num_Documento:
            IF FIRST-OF(Mov_Contable.Agencia) OR
               FIRST-OF(Mov_Contable.fec_contable) OR
               FIRST-OF(Mov_Contable.Comprobante) OR
               FIRST-OF(Mov_Contable.num_documento) THEN DO:
                vdSaldo = 0.
                vdDebito = 0.
                vdCredito = 0.
            END.

            vdDebito = vdDebito + Mov_Contable.Db.
            vdCredito = vdCredito + Mov_Contable.Cr.

            IF LAST-OF(Mov_Contable.Agencia) OR
               LAST-OF(Mov_Contable.fec_contable) OR
               LAST-OF(Mov_Contable.Comprobante) OR
               LAST-OF(Mov_Contable.num_documento) THEN DO:
                vdSaldo = vdCredito - vdDebito.

                IF vdSaldo <> 0 THEN DO:
                    CREATE TempCtas.
                    TempCtas.Agen = mov_contable.agencia.
                    TempCtas.Fecha = mov_contable.fec_contable.
                    TempCtas.Cpte = mov_contable.Comprobante.
                    TempCtas.Dcto = mov_contable.num_documento.
                    TempCtas.Debito = vdDebito.
                    TempCtas.Credito = vdCredito.
                    TempCtas.Saldo = abs(vdSaldo).
                END.
            END.
        END.

        FOR EACH Mov_Contable2 WHERE Mov_Contable2.Agencia = viAge1
                                 AND Mov_Contable2.Comprobante = viCpt1
                                 AND Mov_Contable2.Fec_Contable = pdt01 NO-LOCK BREAK BY Mov_Contable2.agencia
                                                                                      BY Mov_Contable2.fec_contable
                                                                                      BY Mov_Contable2.Comprobante
                                                                                      BY Mov_Contable2.Num_Documento:
            IF FIRST-OF(Mov_Contable2.Agencia) OR
               FIRST-OF(Mov_Contable2.fec_contable) OR
               FIRST-OF(Mov_Contable2.Comprobante) OR
               FIRST-OF(Mov_Contable2.num_documento) THEN DO:
                vdSaldo = 0.
                vdDebito = 0.
                vdCredito = 0.
            END.

            vdDebito = vdDebito + Mov_Contable2.Db.
            vdCredito = vdCredito + Mov_Contable2.Cr.

            IF LAST-OF(Mov_Contable2.Agencia) OR
               LAST-OF(Mov_Contable2.fec_contable) OR
               LAST-OF(Mov_Contable2.Comprobante) OR
               LAST-OF(Mov_Contable2.num_documento) THEN DO:
                vdSaldo = vdCredito - vdDebito.

                IF vdSaldo <> 0 THEN DO:
                    CREATE TempCtas.
                    TempCtas.Agen = mov_contable2.agencia.
                    TempCtas.Fecha = mov_contable2.fec_contable.
                    TempCtas.Cpte = mov_contable2.Comprobante.
                    TempCtas.Dcto = mov_contable2.num_documento.
                    TempCtas.Debito = vdDebito.
                    TempCtas.Credito = vdCredito.
                    TempCtas.Saldo = abs(vdSaldo).
                END.
            END.
        END.
    END.
    ELSE DO:
        IF viCpt1 = viCpt2 AND viAge1 = viAge2 THEN DO:
            FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante = viCpt1
                                    AND Mov_Contable.Fec_Contable >= pdt01
                                    AND Mov_Contable.Fec_Contable <= pdt02
                                    AND Mov_Contable.Agencia = viAge1 NO-LOCK BREAK BY Mov_Contable.agencia
                                                                                    BY Mov_Contable.fec_contable
                                                                                    BY Mov_Contable.Comprobante
                                                                                    BY Mov_Contable.Num_Documento:
                IF FIRST-OF(Mov_Contable.Agencia) OR
                   FIRST-OF(Mov_Contable.fec_contable) OR
                   FIRST-OF(Mov_Contable.Comprobante) OR
                   FIRST-OF(Mov_Contable.num_documento) THEN DO:
                    vdSaldo = 0.
                    vdDebito = 0.
                    vdCredito = 0.
                END.

                vdDebito = vdDebito + Mov_Contable.Db.
                vdCredito = vdCredito + Mov_Contable.Cr.

                IF LAST-OF(Mov_Contable.Agencia) OR
                   LAST-OF(Mov_Contable.fec_contable) OR
                   LAST-OF(Mov_Contable.Comprobante) OR
                   LAST-OF(Mov_Contable.num_documento) THEN DO:
                    vdSaldo = vdCredito - vdDebito.

                    IF vdSaldo <> 0 THEN DO:
                        CREATE TempCtas.
                        TempCtas.Agen = mov_contable.agencia.
                        TempCtas.Fecha = mov_contable.fec_contable.
                        TempCtas.Cpte = mov_contable.Comprobante.
                        TempCtas.Dcto = mov_contable.num_documento.
                        TempCtas.Debito = vdDebito.
                        TempCtas.Credito = vdCredito.
                        TempCtas.Saldo = abs(vdSaldo).
                    END.
                END.
            END.

            FOR EACH Mov_Contable2 WHERE Mov_Contable2.Agencia = viAge1
                                     AND Mov_Contable2.Comprobante = viCpt1
                                     AND Mov_Contable2.Fec_Contable >= pdt01
                                     AND Mov_Contable2.Fec_Contable <= pdt02 NO-LOCK BREAK BY Mov_Contable2.agencia
                                                                                           BY Mov_Contable2.fec_contable
                                                                                           BY Mov_Contable2.Comprobante
                                                                                           BY Mov_Contable2.Num_Documento:
                IF FIRST-OF(Mov_Contable2.Agencia) OR
                   FIRST-OF(Mov_Contable2.fec_contable) OR
                   FIRST-OF(Mov_Contable2.Comprobante) OR
                   FIRST-OF(Mov_Contable2.num_documento) THEN DO:
                    vdSaldo = 0.
                    vdDebito = 0.
                    vdCredito = 0.
                END.

                vdDebito = vdDebito + Mov_Contable2.Db.
                vdCredito = vdCredito + Mov_Contable2.Cr.

                IF LAST-OF(Mov_Contable2.Agencia) OR
                   LAST-OF(Mov_Contable2.fec_contable) OR
                   LAST-OF(Mov_Contable2.Comprobante) OR
                   LAST-OF(Mov_Contable2.num_documento) THEN DO:
                    vdSaldo = vdCredito - vdDebito.

                    IF vdSaldo <> 0 THEN DO:
                        CREATE TempCtas.
                        TempCtas.Agen = mov_contable2.agencia.
                        TempCtas.Fecha = mov_contable2.fec_contable.
                        TempCtas.Cpte = mov_contable2.Comprobante.
                        TempCtas.Dcto = mov_contable2.num_documento.
                        TempCtas.Debito = vdDebito.
                        TempCtas.Credito = vdCredito.
                        TempCtas.Saldo = abs(vdSaldo).
                    END.
                END.
            END.
        END.
        ELSE DO:
            IF viAge1 = viAge2 THEN DO:
                FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante >= viCpt1
                                        AND Mov_Contable.Comprobante <= viCpt2
                                        AND Mov_Contable.Fec_Contable >= pdt01
                                        AND Mov_Contable.Fec_Contable <= pdt02
                                        AND Mov_Contable.Agencia = viAge1 NO-LOCK BREAK BY Mov_Contable.agencia
                                                                                        BY Mov_Contable.fec_contable
                                                                                        BY Mov_Contable.Comprobante
                                                                                        BY Mov_Contable.Num_Documento:
                    IF FIRST-OF(Mov_Contable.Agencia) OR
                       FIRST-OF(Mov_Contable.fec_contable) OR
                       FIRST-OF(Mov_Contable.Comprobante) OR
                       FIRST-OF(Mov_Contable.num_documento) THEN DO:
                        vdSaldo = 0.
                        vdDebito = 0.
                        vdCredito = 0.
                    END.

                    vdDebito = vdDebito + Mov_Contable.Db.
                    vdCredito = vdCredito + Mov_Contable.Cr.

                    IF LAST-OF(Mov_Contable.Agencia) OR
                       LAST-OF(Mov_Contable.fec_contable) OR
                       LAST-OF(Mov_Contable.Comprobante) OR
                       LAST-OF(Mov_Contable.num_documento) THEN DO:
                        vdSaldo = vdCredito - vdDebito.

                        IF vdSaldo <> 0 THEN DO:
                            CREATE TempCtas.
                            TempCtas.Agen = mov_contable.agencia.
                            TempCtas.Fecha = mov_contable.fec_contable.
                            TempCtas.Cpte = mov_contable.Comprobante.
                            TempCtas.Dcto = mov_contable.num_documento.
                            TempCtas.Debito = vdDebito.
                            TempCtas.Credito = vdCredito.
                            TempCtas.Saldo = abs(vdSaldo).
                        END.
                    END.
                END.

                FOR EACH Mov_Contable2 WHERE Mov_Contable2.Agencia = viAge1
                                         AND Mov_Contable2.Comprobante >= viCpt1
                                         AND Mov_Contable2.Comprobante <= viCpt2
                                         AND Mov_Contable2.Fec_Contable >= pdt01
                                         AND Mov_Contable2.Fec_Contable <= pdt02 NO-LOCK BREAK BY Mov_Contable2.agencia
                                                                                               BY Mov_Contable2.fec_contable
                                                                                               BY Mov_Contable2.Comprobante
                                                                                               BY Mov_Contable2.Num_Documento:
                    IF FIRST-OF(Mov_Contable2.Agencia) OR
                       FIRST-OF(Mov_Contable2.fec_contable) OR
                       FIRST-OF(Mov_Contable2.Comprobante) OR
                       FIRST-OF(Mov_Contable2.num_documento) THEN DO:
                        vdSaldo = 0.
                        vdDebito = 0.
                        vdCredito = 0.
                    END.

                    vdDebito = vdDebito + Mov_Contable2.Db.
                    vdCredito = vdCredito + Mov_Contable2.Cr.

                    IF LAST-OF(Mov_Contable2.Agencia) OR
                       LAST-OF(Mov_Contable2.fec_contable) OR
                       LAST-OF(Mov_Contable2.Comprobante) OR
                       LAST-OF(Mov_Contable2.num_documento) THEN DO:
                        vdSaldo = vdCredito - vdDebito.

                        IF vdSaldo <> 0 THEN DO:
                            CREATE TempCtas.
                            TempCtas.Agen = mov_contable2.agencia.
                            TempCtas.Fecha = mov_contable2.fec_contable.
                            TempCtas.Cpte = mov_contable2.Comprobante.
                            TempCtas.Dcto = mov_contable2.num_documento.
                            TempCtas.Debito = vdDebito.
                            TempCtas.Credito = vdCredito.
                            TempCtas.Saldo = abs(vdSaldo).
                        END.
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
                                                                                             BY Mov_Contable.fec_contable
                                                                                             BY Mov_Contable.Comprobante
                                                                                             BY Mov_Contable.Num_Documento:
                        IF FIRST-OF(Mov_Contable.Agencia) OR
                           FIRST-OF(Mov_Contable.fec_contable) OR
                           FIRST-OF(Mov_Contable.Comprobante) OR
                           FIRST-OF(Mov_Contable.num_documento) THEN DO:
                            vdSaldo = 0.
                            vdDebito = 0.
                            vdCredito = 0.
                        END.

                        vdDebito = vdDebito + Mov_Contable.Db.
                        vdCredito = vdCredito + Mov_Contable.Cr.

                        IF LAST-OF(Mov_Contable.Agencia) OR
                           LAST-OF(Mov_Contable.fec_contable) OR
                           LAST-OF(Mov_Contable.Comprobante) OR
                           LAST-OF(Mov_Contable.num_documento) THEN DO:
                            vdSaldo = vdCredito - vdDebito.

                            IF vdSaldo <> 0 THEN DO:
                                CREATE TempCtas.
                                TempCtas.Agen = mov_contable.agencia.
                                TempCtas.Fecha = mov_contable.fec_contable.
                                TempCtas.Cpte = mov_contable.Comprobante.
                                TempCtas.Dcto = mov_contable.num_documento.
                                TempCtas.Debito = vdDebito.
                                TempCtas.Credito = vdCredito.
                                TempCtas.Saldo = abs(vdSaldo).
                            END.
                        END.
                    END.

                    FOR EACH Mov_Contable2 WHERE Mov_Contable2.Comprobante = viCpt1
                                             AND Mov_Contable2.Fec_Contable >= pdt01
                                             AND Mov_Contable2.Fec_Contable <= pdt02
                                             AND Mov_Contable2.Agencia >= viAge1
                                             AND Mov_Contable2.Agencia <= viAge2 NO-LOCK BREAK BY Mov_Contable2.agencia
                                                                                               BY Mov_Contable2.fec_contable
                                                                                               BY Mov_Contable2.Comprobante
                                                                                               BY Mov_Contable2.Num_Documento:
                        IF FIRST-OF(Mov_Contable2.Agencia) OR
                           FIRST-OF(Mov_Contable2.fec_contable) OR
                           FIRST-OF(Mov_Contable2.Comprobante) OR
                           FIRST-OF(Mov_Contable2.num_documento) THEN DO:
                            vdSaldo = 0.
                            vdDebito = 0.
                            vdCredito = 0.
                        END.

                        vdDebito = vdDebito + Mov_Contable2.Db.
                        vdCredito = vdCredito + Mov_Contable2.Cr.

                        IF LAST-OF(Mov_Contable2.Agencia) OR
                           LAST-OF(Mov_Contable2.fec_contable) OR
                           LAST-OF(Mov_Contable2.Comprobante) OR
                           LAST-OF(Mov_Contable2.num_documento) THEN DO:
                            vdSaldo = vdCredito - vdDebito.

                            IF vdSaldo <> 0 THEN DO:
                                CREATE TempCtas.
                                TempCtas.Agen = mov_contable2.agencia.
                                TempCtas.Fecha = mov_contable2.fec_contable.
                                TempCtas.Cpte = mov_contable2.Comprobante.
                                TempCtas.Dcto = mov_contable2.num_documento.
                                TempCtas.Debito = vdDebito.
                                TempCtas.Credito = vdCredito.
                                TempCtas.Saldo = abs(vdSaldo).
                            END.
                        END.
                    END.
                END.
            END.
        END.
    END.
END.

RUN PValida_Salida NO-ERROR.

PROCEDURE PValida_Salida:
    IF P_NomArchivo = "DEFAULT" THEN
        P_NomArchivo = W_PathSpl + "\CptesDescuadrados.txt".

    OUTPUT TO VALUE(P_NomArchivo) PAGED PAGE-SIZE VALUE(P_NumLineas).
    FOR EACH TempCtas:
        FORM TempCtas.Agen    COLUMN-LABEL "Age"         FORMAT "999"
             TempCtas.Fecha   COLUMN-LABEL "Fecha"       FORMAT "99/99/9999"
             TempCtas.Cpte    COLUMN-LABEL "Comprobante" FORMAT "999"
             TempCtas.Dcto    COLUMN-LABEL "Documento"   FORMAT "999999999"
             TempCtas.Debito  COLUMN-LABEL "Debito"      FORMAT "->>>,>>>,>>>,>>9.99"
             TempCtas.Credito COLUMN-LABEL "Credito"     FORMAT "->>>,>>>,>>>,>>9.99"
             TempCtas.Saldo   COLUMN-LABEL "Diferencia"  FORMAT "->>>,>>>,>>>,>>9.99"
            WITH FRAME FCptDesc DOWN COLUMN 1 WIDTH 160 NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.

        DISPLAY TempCtas.Agen
                TempCtas.Fecha
                TempCtas.Cpte
                TempCtas.Dcto
                TempCtas.Debito
                TempCtas.Credito
                TempCtas.Saldo
            WITH FRAME FCptDesc.

        DOWN WITH FRAME FCptDesc.
    END.

    VIEW FRAME F-Ftr.
    OUTPUT CLOSE.

END PROCEDURE.

RETURN.
