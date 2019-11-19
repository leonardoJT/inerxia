DISABLE TRIGGERS FOR LOAD OF anexos.
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
    FIND FIRST anexos WHERE anexos.agencia = 4
                        AND anexos.nit = anx.nit
                        AND anexos.cuenta = anx.cuenta
                        AND anexos.cen_costos = anx.cen_costos
                        AND anexos.ano = 2011 NO-ERROR.
    anexos.db[2] = anexos.db[2] - anx.db.
    anexos.cr[2] = anexos.cr[2] - anx.cr.
END.
