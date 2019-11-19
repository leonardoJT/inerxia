DEFINE VAR sumaInteres2 AS DECIMAL.
DEFINE VAR sumaInteres1 AS DECIMAL.
DEFINE VAR ag AS INTEGER INITIAL 4.
DEFINE VAR codProd AS INTEGER INITIAL 62.

FOR EACH creditos WHERE creditos.agencia = ag
                    AND creditos.cod_credito = codProd
                    AND creditos.dias_atraso >= 0
                    AND creditos.dias_atraso <= 30
                    NO-LOCK:
    IF creditos.FOR_pago = 2 THEN
        sumaInteres2 = sumaInteres2 + creditos.INT_corriente.

    IF creditos.FOR_pago = 1 THEN
        sumaInteres1 = sumaInteres1 + creditos.INT_corriente.
END.

MESSAGE "A" SKIP
        sumaInteres2 SKIP
        sumaInteres1
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

sumaInteres2 = 0.
sumaINteres1 = 0.

FOR EACH creditos WHERE creditos.agencia = ag
                    AND creditos.cod_credito = codProd
                    AND creditos.dias_atraso >= 31
                    AND creditos.dias_atraso <= 60
                    NO-LOCK:
    IF creditos.FOR_pago = 2 THEN
        sumaInteres2 = sumaInteres2 + creditos.INT_corriente.

    IF creditos.FOR_pago = 1 THEN
        sumaInteres1 = sumaInteres1 + creditos.INT_corriente.
END.

MESSAGE "B" SKIP
        sumaInteres2 SKIP
        sumaInteres1
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

sumaInteres2 = 0.
sumaINteres1 = 0.

FOR EACH creditos WHERE creditos.agencia = ag
                    AND creditos.cod_credito = codProd
                    AND creditos.dias_atraso >= 61
                    AND creditos.dias_atraso <= 90
                    NO-LOCK:
    IF creditos.FOR_pago = 2 THEN
        sumaInteres2 = sumaInteres2 + creditos.INT_corriente.

    IF creditos.FOR_pago = 1 THEN
        sumaInteres1 = sumaInteres1 + creditos.INT_corriente.
END.

MESSAGE "C" SKIP
        sumaInteres2 SKIP
        sumaInteres1
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

sumaInteres2 = 0.
sumaINteres1 = 0.

FOR EACH creditos WHERE creditos.agencia = ag
                    AND creditos.cod_credito = codProd
                    AND creditos.dias_atraso >= 91
                    AND creditos.dias_atraso <= 180
                    NO-LOCK:
    IF creditos.FOR_pago = 2 THEN
        sumaInteres2 = sumaInteres2 + creditos.INT_corriente.

    IF creditos.FOR_pago = 1 THEN
        sumaInteres1 = sumaInteres1 + creditos.INT_corriente.
END.

MESSAGE "D" SKIP
        sumaInteres2 SKIP
        sumaInteres1
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

sumaInteres2 = 0.
sumaINteres1 = 0.

FOR EACH creditos WHERE creditos.agencia = ag
                    AND creditos.cod_credito = codProd
                    AND creditos.dias_atraso >= 181
                    /*AND creditos.dias_atraso <= 60*/
                    NO-LOCK:
    IF creditos.FOR_pago = 2 THEN
        sumaInteres2 = sumaInteres2 + creditos.INT_corriente.

    IF creditos.FOR_pago = 1 THEN
        sumaInteres1 = sumaInteres1 + creditos.INT_corriente.
END.

MESSAGE "E" SKIP
        sumaInteres2 SKIP
        sumaInteres1
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

sumaInteres2 = 0.
sumaINteres1 = 0.
