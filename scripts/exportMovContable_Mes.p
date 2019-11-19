DEFINE TEMP-TABLE tt
    FIELD agencia AS INTEGER
    FIELD cen_costos AS INTEGER
    FIELD nom_cenCostos AS CHARACTER
    FIELD mes AS INTEGER
    FIELD cuenta AS CHARACTER
    FIELD cliente_id AS CHARACTER
    FIELD db AS DECIMAL
    FIELD cr AS DECIMAL
    INDEX idx agencia cuenta cliente_id cen_costos.

FOR EACH agencias NO-LOCK:
    FOR EACH mov_contable WHERE mov_contable.agencia = agencias.agencia
                            AND mov_contable.fec_contable >= 11/01/2018
                            AND mov_contable.fec_contable <= 12/31/2018
                            AND (SUBSTRING(mov_contable.cuenta,1,1) = "4" OR
                                 SUBSTRING(mov_contable.cuenta,1,1) = "5" OR
                                 SUBSTRING(mov_contable.cuenta,1,1) = "6") NO-LOCK:
        FIND FIRST tt WHERE tt.agencia = mov_contable.agencia
                        AND tt.cuenta = mov_contable.cuenta
                        AND tt.cliente_id = mov_contable.nit
                        AND tt.cen_costos = mov_contable.cen_costos
                        AND tt.mes = MONTH(mov_contable.fec_contable) NO-ERROR.
        IF NOT AVAILABLE tt THEN DO:
            CREATE tt.
            tt.agencia = mov_contable.agencia.
            tt.mes = MONTH(mov_contable.fec_contable).
            tt.cuenta = mov_contable.cuenta.
            tt.cliente_id = mov_contable.nit.
            tt.cen_costos = mov_contable.cen_costos.

            IF mov_contable.cen_costos = 999 THEN
                tt.nom_cenCostos = "GENERAL".
            ELSE DO:
                FIND FIRST cen_costos WHERE cen_costos.cen_costo = mov_contable.cen_costos NO-LOCK NO-ERROR.
                IF AVAILABLE cen_costos THEN
                    tt.nom_cenCostos = cen_costos.nombre.
            END.
        END.

        tt.db = tt.db + mov_contable.db.
        tt.cr = tt.cr + mov_contable.cr.
    END.
END.

OUTPUT TO d:\Leonardo\movs456_mesCenCostos.csv.
EXPORT DELIMITER ";" "AGENCIA" "MES" "CUENTA" "CLIENTE_ID" "DB" "CR".

FOR EACH tt NO-LOCK:
    EXPORT DELIMITER ";" tt.
END.
OUTPUT CLOSE.
