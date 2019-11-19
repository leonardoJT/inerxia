DEFINE VAR v4 AS DECIMAL.
DEFINE VAR v5 AS DECIMAL.
DEFINE VAR v6 AS DECIMAL.

FOR EACH mov_contable WHERE YEAR(fec_contable) = 2011
    AND (SUBSTRING(cuenta,1,1) = "4" OR
         SUBSTRING(cuenta,1,1) = "5" OR
         SUBSTRING(cuenta,1,1) = "6") NO-LOCK:
    IF SUBSTRING(cuenta,1,1) = "4" THEN
        v4 = v4 + cr - db.

    IF SUBSTRING(cuenta,1,1) = "5" THEN
        v5 = v5 + db - cr.

    IF SUBSTRING(cuenta,1,1) = "6" THEN
        v6 = v6 + db - cr.
END.

OUTPUT TO d:\Leonardo\saldos.csv.
EXPORT v4 SKIP
       v5 SKIP
       v6.
OUTPUT CLOSE.
