DEFINE VAR tea AS DECIMAL.
DEFINE VAR periodos AS INTEGER.
DEFINE VAR tn AS DECIMAL.

tea = 4. / 100.
periodos = 4.

tn = (EXP(tea + 1,1 / Periodos)) - 1.

MESSAGE tn * 100 SKIP
        tn * 100 * periodos
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
