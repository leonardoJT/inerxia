FOR EACH rep_creditos WHERE fecCorte = 08/31/2013
                        AND estado <> 2
                        AND fec_canceTotal < 08/01/2013:
    DELETE rep_creditos.
END.

MESSAGE "fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
