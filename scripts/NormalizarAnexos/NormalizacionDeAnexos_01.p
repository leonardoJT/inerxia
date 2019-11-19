/* Para revisar documentos sin nit */

DEFINE VAR tdb AS DECIMAL.
DEFINE VAR tcr AS DECIMAL.
    
OUTPUT TO d:\Leonardo\24153001_.csv.
FOR EACH mov_contable WHERE fec_contable >= 03/01/2011
                        AND cuenta = "24153001"
                        AND nit = "" NO-LOCK:
    tdb = tdb + db.
    tcr = tcr + cr.
    EXPORT DELIMITER ";" agencia fec_contable num_documento comentario db cr.
END.

MESSAGE tdb - tcr
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
