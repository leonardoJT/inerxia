MESSAGE "Inicia"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH creditos WHERE sdo_capital > 0
                    AND plazo = 1 NO-LOCK:
    RUN \\192.168.1.100\sps\soportes\fodun\Prog\scripts\GeneraPlanDePagos3.p(INPUT creditos.nit,
                                                                             INPUT creditos.num_credito,
                                                                             INPUT creditos.tasa) NO-ERROR.
END.

MESSAGE "Finalizó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
