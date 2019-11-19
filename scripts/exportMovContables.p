DEFINE TEMP-TABLE tt
    FIELD agencia AS INTEGER
    FIELD tercero_id AS CHARACTER
    FIELD cuenta AS CHARACTER
    FIELD db AS DECIMAL
    FIELD cr AS DECIMAL
    INDEX idx agencia tercero_id cuenta.

FOR EACH agencias NO-LOCK:
    FOR EACH mov_contable WHERE mov_contable.agencia = agencias.agencia
                            AND (/*SUBSTRING(mov_contable.cuenta,1,2) = "21" OR
                                 SUBSTRING(mov_contable.cuenta,1,4) = "2430" OR
                                 SUBSTRING(mov_contable.cuenta,1,4) = "2435" OR
                                 SUBSTRING(mov_contable.cuenta,1,4) = "2625" OR
                                 SUBSTRING(mov_contable.cuenta,1,4) = "2695" OR
                                 SUBSTRING(mov_contable.cuenta,1,4) = "5105" OR
                                 SUBSTRING(mov_contable.cuenta,1,4) = "5110" OR
                                 SUBSTRING(mov_contable.cuenta,1,4) = "6150" OR
                                 SUBSTRING(mov_contable.cuenta,1,4) = "4150" OR*/
                                 SUBSTRING(mov_contable.cuenta,1,6) = "614010")
                            AND mov_contable.fec_contable >= 01/01/2018
                            AND mov_contable.fec_contable<= 12/31/2018 NO-LOCK:
        FIND FIRST tt WHERE tt.agencia = mov_contable.agencia
                        AND tt.tercero_id = mov_contable.nit
                        AND tt.cuenta = mov_contable.cuenta NO-ERROR.
        IF NOT AVAILABLE tt THEN DO:
            CREATE tt.
            tt.agencia = mov_contable.agencia.
            tt.tercero_id = mov_contable.nit.
            tt.cuenta = mov_contable.cuenta.
        END.

        tt.db = tt.db + mov_contable.db.
        tt.cr = tt.cr + mov_contable.cr.
    END.
END.

OUTPUT TO c:\Info_Fodun\MovsContables.csv.
EXPORT DELIMITER ";"
    "AGENCIA"
    "TERCERO_ID"
    "CUENTA"
    "DB"
    "CR".

FOR EACH tt NO-LOCK:
    EXPORT DELIMITER ";" tt.
END.
OUTPUT CLOSE.
