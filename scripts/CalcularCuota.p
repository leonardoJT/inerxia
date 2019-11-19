DEFINE VAR cuota AS DECIMAL.
DEFINE VAR monto AS DECIMAL INITIAL 1000000.
DEFINE VAR tasa AS DECIMAL.
DEFINE VAR plazo AS INTEGER INITIAL 12.

tasa = 1 / 100.



cuota = ROUND((monto * ((tasa * EXP(tasa + 1,plazo)) / (EXP(tasa + 1,plazo) - 1))),6).

MESSAGE cuota
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

tasa = 1.1 / 100.

cuota = ROUND((monto * ((tasa * EXP(tasa + 1,plazo)) / (EXP(tasa + 1,plazo) - 1))),6).

MESSAGE cuota
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
