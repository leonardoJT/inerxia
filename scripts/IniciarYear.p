DEFINE TEMP-TABLE ttc LIKE calendario.
DEFINE TEMP-TABLE ttp LIKE procDia.

FOR EACH calendario WHERE ano = 2014 NO-LOCK:
    CREATE ttc.
    BUFFER-COPY calendario TO ttc.
    ttc.ano = 2015.
    ttc.estado = 2.
END.

FOR EACH ttc NO-LOCK:
    CREATE calendario.
    BUFFER-COPY ttc TO calendario.
END.

FOR EACH procDia WHERE YEAR(procDia.fecha_proc) = 2014 NO-LOCK:
    CREATE ttp.
    BUFFER-COPY procDia TO ttp.
    ttp.fecha_proc = DATE(MONTH(procDia.fecha_proc), DAY(procDia.fecha_proc), 2015).
    ttp.estado = 1.
END.

FOR EACH ttp NO-LOCK:
    CREATE procDia.
    BUFFER-COPY ttp TO procDia.
END.


MESSAGE "Ya!"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
