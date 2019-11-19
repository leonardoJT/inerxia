DISABLE TRIGGERS FOR LOAD OF anexos.
DISABLE TRIGGERS FOR LOAD OF mov_contable.

OUTPUT TO c:\INFO_fodun\leonardo\anexosPalmira.csv.

FOR EACH mov_Contable WHERE mov_contable.agencia = 4
                        AND mov_contable.fec_contable >= 02/01/2011
                        AND mov_contable.fec_contable <= 02/28/2011:
    IF mov_contable.db MODULO 100 <> 0 OR mov_contable.cr MODULO 100 <> 0 THEN
        MESSAGE mov_contable.db mov_contable.cr
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE DO:
        EXPORT DELIMITER ";"
            mov_contable.nit
            mov_Contable.cuenta
            mov_contable.cen_costos
            mov_contable.db - mov_contable.db / 100
            mov_contable.cr - mov_contable.cr / 100.

        mov_contable.db = mov_Contable.db / 100.
        mov_contable.cr = mov_contable.cr / 100.
    END.
END.
