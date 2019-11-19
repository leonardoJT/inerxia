DEFINE VAR vTotal1 AS DECIMAL.
DEFINE VAR vTotal2 AS DECIMAL.
DEFINE VAR cont AS INTEGER.

FOR EACH rep_creditos WHERE fecCorte = 09/30/2013
                        AND categoriaMes = "E"
                        AND estado = 2 NO-LOCK BY nit BY num_credito:
    vTotal1 = vTotal1 + INT_corriente.
    vTotal2 = vTotal2 + provision_interes.
    cont = cont + 1.

    DISPLAY num_credito nit INT_corriente provision_interes.
END.

MESSAGE vTotal1 cont
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
