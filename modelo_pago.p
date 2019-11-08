DEFINE VARIABLE imp_plazo AS INTEGER INITIAL 12.
DEFINE VARIABLE imp_cuota AS DECIMAL INITIAL 100000.
DEFINE VARIABLE imp_saldo AS DECIMAL INITIAL 1000000.
DEFINE VARIABLE imp_tasa  AS DECIMAL INITIAL 1.8.
DEFINE VARIABLE imp_cuoactual AS INTEGER INITIAL 8.
DEFINE VARIABLE i AS INTEGER.



DEFINE TEMP-TABLE proyec
    FIELD pnrocuo AS INTEGER
    FIELD pfecha LIKE creditos.fec_desembolso
    FIELD pcuota LIKE creditos.cuota
    FIELD pk     LIKE creditos.monto LABEL "K cuota"
    FIELD pi     LIKE creditos.INT_corriente LABEL "I Cuota"
    FIELD psal   LIKE creditos.sdo_capital
    FIELD ek     LIKE creditos.monto LABEL "Ejec K cuota"
    FIELD Ei     LIKE creditos.INT_corriente LABEL "I Cuota"
    FIELD Cc     AS INTEGER LABEL "Cuota cerrada".

DO  i = 1 TO imp_plazo:
    CREATE proyec.
    ASSIGN pnrocuo = i
           pfecha = TODAY
           pcuota = imp_cuota
           pi     = imp_saldo * (imp_tasa / 100)
           pk     = pcuota - pi
           imp_saldo = imp_saldo - pk
           psal   = imp_saldo.
END.

FOR EACH proyec WHERE pnrocuo = imp_cuoactual + 1:
    DISP proyec.
END.


