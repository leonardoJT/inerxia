DISABLE TRIGGERS FOR LOAD OF anexos13.
DISABLE TRIGGERS FOR LOAD OF mov_contable.

DEFINE TEMP-TABLE anx
    FIELD nit AS CHARACTER
    FIELD cuenta AS CHARACTER
    FIELD cen_costos AS INTEGER
    FIELD db AS DECIMAL FORMAT ">>>,>>>,>>>,>>9"
    FIELD cr AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".

INPUT FROM c:\INFO_fodun\leonardo\anexosPalmira.csv.
REPEAT:
    CREATE anx.
    IMPORT DELIMITER ";" anx.
END.
INPUT CLOSE.

FOR EACH anx WHERE anx.nit <> "" NO-LOCK:
    FIND FIRST mov_contable WHERE mov_contable.agencia = 4
                              AND mov_contable.nit = anx.nit
                              AND mov_contable.cuenta = anx.cuenta
                              AND mov_contable.cen_costos = 999
                              AND mov_contable.fec_contable = 12/31/2011
                              AND mov_contable.comprobante = 20
                              AND mov_contable.num_documento = 748
                              AND mov_contable.db - anx.cr > 0 NO-ERROR.
    IF AVAILABLE mov_contable THEN
        DISPLAY anx.nit anx.cuenta mov_contable.db anx.cr WITH WIDTH 200.
END.
