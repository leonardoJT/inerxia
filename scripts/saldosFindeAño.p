DEFINE VAR v4 AS DECIMAL.
DEFINE VAR v5 AS DECIMAL.
DEFINE VAR v6 AS DECIMAL.
DEFINE VAR vYear AS INTEGER INITIAL 2017.

61752001

FOR EACH sal_cuenta13 WHERE ano = vYear
    AND (SUBSTRING(cuenta,1,1) = "4" OR
         SUBSTRING(cuenta,1,1) = "5" OR
         SUBSTRING(cuenta,1,1) = "6") NO-LOCK:
    IF SUBSTRING(cuenta,1,1) = "4" THEN
        v4 = v4 + (cr[1] + cr[2] + cr[3] + cr[4] + cr[5] + cr[6] + cr[7] + cr[8] + cr[9] + cr[10] + cr[11] + cr[12]) -
                  (db[1] + db[2] + db[3] + db[4] + db[5] + db[6] + db[7] + db[8] + db[9] + db[10] + db[11] + db[12]).

    IF SUBSTRING(cuenta,1,1) = "5" THEN
        v5 = v5 - (cr[1] + cr[2] + cr[3] + cr[4] + cr[5] + cr[6] + cr[7] + cr[8] + cr[9] + cr[10] + cr[11] + cr[12]) +
                  (db[1] + db[2] + db[3] + db[4] + db[5] + db[6] + db[7] + db[8] + db[9] + db[10] + db[11] + db[12]).


    IF SUBSTRING(cuenta,1,1) = "6" THEN
        v6 = v6 - (cr[1] + cr[2] + cr[3] + cr[4] + cr[5] + cr[6] + cr[7] + cr[8] + cr[9] + cr[10] + cr[11] + cr[12]) +
                  (db[1] + db[2] + db[3] + db[4] + db[5] + db[6] + db[7] + db[8] + db[9] + db[10] + db[11] + db[12]).

END.

OUTPUT TO VALUE("d:\Leonardo\saldos" + string(vYear) + ".csv").
EXPORT DELIMITER ";"
    v4 v5  v6.
OUTPUT CLOSE.


MESSAGE "Fin 2"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
