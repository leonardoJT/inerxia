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

OUTPUT TO C:\Info_Fodun\Leonardo\movs_12-2019.csv.
EXPORT DELIMITER ";" "AGENCIA" "FECHA" "COMPROBANTE" "NUM_DOCUMENTO" "CUENTA" "DESCRIPCION" "CLIENTE_ID" "USUARIO" "DOC_REF" "DB" "CR".
FOR EACH agencias NO-LOCK:
    FOR EACH mov_contable WHERE mov_contable.agencia = agencias.agencia
                            AND mov_contable.fec_contable >= 12/01/2019
                            AND mov_contable.fec_contable <= 12/31/2019
                            /*AND mov_contable.nit = "900092385"*/
                            /*AND mov_contable.cuenta = "27259502"*/
                            AND (SUBSTRING(mov_contable.cuenta,1,1) = "1" OR
                                 SUBSTRING(mov_contable.cuenta,1,1) = "2" OR
                                 SUBSTRING(mov_contable.cuenta,1,1) = "3" OR
                                 SUBSTRING(mov_contable.cuenta,1,1) = "4" OR
                                 SUBSTRING(mov_contable.cuenta,1,1) = "5" OR
                                 SUBSTRING(mov_contable.cuenta,1,1) = "6" OR
                                 SUBSTRING(mov_contable.cuenta,1,1) = "7") NO-LOCK:
        EXPORT DELIMITER ";"
            agencia
            fec_contable
            comprobante
            num_documento
            cuenta
            comentario
            nit
            usuario
            doc_ref
            db
            cr.
    END.
END.

/*FOR EACH tt NO-LOCK:
    EXPORT DELIMITER ";" tt.
END.*/
OUTPUT CLOSE.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
