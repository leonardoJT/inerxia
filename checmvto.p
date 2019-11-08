/*
    nombre: checmvto.p
    Descripción: Checa integrida de los saldos Vs Movimiento
    Programadores: Ing. Edilberto Mariño Moya(EMM)
    Log: Creado, 28 sep 2007, EMM
*/
current-WINDOW:WIDTH = 132.
DEF VAR cLsta AS CHAR NO-UNDO.
DEF VAR daFchaIncial AS DATE NO-UNDO.
DEF VAR daFchaFnal AS DATE NO-UNDO.
DEF VAR dedb AS DECIMAL NO-UNDO.
DEF VAR decr AS DECIMAL NO-UNDO.
DEF VAR dedb1 AS DECIMAL NO-UNDO.
DEF VAR decr1 AS DECIMAL NO-UNDO.
DEF VAR dedb2 AS DECIMAL NO-UNDO.
DEF VAR decr2 AS DECIMAL NO-UNDO.
DEF TEMP-TABLE Tano NO-UNDO
    FIELD ano AS INTEGER
    INDEX ano ano.
DEF VAR it AS INTEGER NO-UNDO.
DISPLAY STRING(TIME,"hh:mm").
it = TIME.
/* Años a analizar */
DEF VAR i AS INTEGER NO-UNDO.
DO i = 2007 TO 2007 BY 1.
    CREATE Tano.
    Tano.ano = i.
END.
/* fin Años a analizar */

FUNCTION fdbcrCuenta RETURN CHAR(iagncia AS INTEGER,ccuenta AS CHAR,iano AS INTEGER):
    /* Retorna una lista separa por chr(1)
        calcula en sal_cuenta
       Entrada 1: vrDb
       Entrada 2: VrCr
    */
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR crtrno AS CHAR NO-UNDO.
    DEF VAR dedb AS DECIMAL NO-UNDO.
    DEF VAR decr AS DECIMAL NO-UNDO.
    FOR EACH sal_cuenta fields(agencia cuenta ano db cr) NO-LOCK
        WHERE
            sal_cuenta.agencia = iagncia
        AND sal_cuenta.cuenta = ccuenta
        AND sal_cuenta.ano = iano:
        dedb = 0.
        decr = 0.
        DO i = 1 TO 12:
            dedb = dedb + db[i].
            decr = decr + cr[i].
        END.
        ACCUMULATE dedb(TOTAL).
        ACCUMULATE decr(TOTAL).
    END.
    crtrno = string(ACCUM TOTAL dedb) + CHR(1) + string(ACCUM TOTAL decr).
    RETURN crtrno.
END FUNCTION.

FUNCTION fdbcrAnexo RETURN CHAR(iagncia AS INTEGER,ccuenta AS CHAR,iano AS INTEGER):
    /* Retorna una lista separa por chr(1)
        calcula en anexos
       Entrada 1: vrDb
       Entrada 2: VrCr
    */
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR crtrno AS CHAR NO-UNDO.
    DEF VAR dedb AS DECIMAL NO-UNDO.
    DEF VAR decr AS DECIMAL NO-UNDO.
    FOR EACH anexos fields(agencia cuenta ano db cr) NO-LOCK
        WHERE
            anexos.agencia = iagncia
        AND anexos.cuenta = ccuenta
        AND anexos.ano = iano:
        dedb = 0.
        decr = 0.
        DO i = 1 TO 12:
            dedb = dedb + db[i].
            decr = decr + cr[i].
        END.
        ACCUMULATE dedb(TOTAL).
        ACCUMULATE decr(TOTAL).
    END.
    crtrno = string(ACCUM TOTAL dedb) + CHR(1) + string(ACCUM TOTAL decr).
    RETURN crtrno.
END FUNCTION.

OUTPUT TO c:\tmp\exito.txt.
EXPORT DELIMITER ";" 
    "AGENCIA"
    "CUENTA"
    "DB MOV"
    "CR MOV"
    "DB SAL"
    "CR SAL"
    "DB ANX"
    "CR ANX".

/* RECORRE AÑOS */
FOR EACH Tano NO-LOCK:
    daFchaIncial = DATE(1,1,tano.ano). /* primer dia del año */
    daFchaFnal = DATE(12,31,tano.ano). /* ultimo dia del año */
    /* RECORRE AGENCIAS */
    FOR EACH agencias FIELDS(agencia) NO-LOCK
        BREAK
            BY agencias.agencia:
        /* RECORRE CUENTAS DEFINIDAS */             
        FOR EACH cuentas fields(cuenta) NO-LOCK
            BY cuenta:
            /* RECORRE MOVIMIENTO */
            CURRENT-WINDOW:TITLE = "AGENCIA: " + string(agencias.agencia) + " / " + "CUENTA: " + cuentas.cuenta.
            FOR EACH mov_contable fields(db cr agencia cuenta nit fec_contable) NO-LOCK
                WHERE
                    mov_contable.agencia = agencias.agencia
                AND mov_contable.cuenta = cuentas.cuenta
                AND mov_contable.fec_contable >= dafchaincial AND mov_contable.fec_contable <= dafchafnal
                BREAK
                    BY mov_contable.agencia
                    BY mov_contable.cuenta
                    BY mov_contable.nit:
                ACCUMULATE mov_contable.db (TOTAL BY agencia BY cuenta BY nit ).
                ACCUMULATE mov_contable.cr (TOTAL BY agencia BY cuenta BY nit ).

                
                
                IF LAST-OF(mov_contable.cuenta)
                THEN DO:
                    /* valores en saldos cuentas */
                    cLsta = fdbcrCuenta(agencias.agencia,cuentas.cuenta,tano.ano).
                    dedb = DECIMAL(entry(1,cLsta,CHR(1))).
                    decr = DECIMAL(entry(2,cLsta + CHR(1),CHR(1))).

                    /* valores en anexos */
                    cLsta = fdbcrAnexo(agencias.agencia,cuentas.cuenta,tano.ano).
                    dedb1 = DECIMAL(entry(1,cLsta,CHR(1))).
                    decr1 = DECIMAL(entry(2,cLsta + CHR(1),CHR(1))).

                    dedb2 = ACCUM TOTAL BY mov_contable.cuenta db.
                    decr2 = ACCUM TOTAL BY mov_contable.cuenta cr.
                    IF not(dedb = dedb1 AND dedb1 = dedb2 AND decr = decr1 AND decr1 = decr2) 
                    THEN    EXPORT DELIMITER ";" 
                                mov_contable.agencia
                                mov_contable.cuenta  
                                    (ACCUM TOTAL BY mov_contable.cuenta db)
                                    (ACCUM TOTAL BY mov_contable.cuenta cr)
                                    dedb 
                                    decr 
                                    dedb1
                                    decr1.
                END. /* IF LAST-OF(MONTH(mov_contable.fec_contable))  */
            END.
        END. /* for each cuentas */
    END. /* fin recorrido agencias */
END. /* FIN RECORRE AÑOS */
OUTPUT CLOSE.
MESSAGE "FIN: " TIME - it.
