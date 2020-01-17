DEFINE TEMP-TABLE ttc LIKE calendario.
DEFINE TEMP-TABLE ttp LIKE procDia.

FOR EACH calendario WHERE ano = 2019 NO-LOCK:
    CREATE ttc.
    BUFFER-COPY calendario TO ttc.
    ttc.ano = 2020.
END.

FOR EACH ttc NO-LOCK:
    CREATE calendario.
    BUFFER-COPY ttc TO calendario.
END.

FOR EACH procDia WHERE YEAR(fecha_proc) = 2019 NO-LOCK:
    CREATE ttp.
    BUFFER-COPY procDia TO ttp.
    ttp.fecha_proc = ADD-INTERVAL(ttp.fecha_proc,1,"years").
END.

FOR EACH ttp NO-LOCK:
    CREATE procDia.
    BUFFER-COPY ttp TO procDia.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
