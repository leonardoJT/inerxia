DEFINE TEMP-TABLE tproc LIKE procDia.

FOR EACH procdia WHERE YEAR(fecha_proc) = 2012
                   AND fecha_proc <> 02/29/2012 NO-LOCK:
    CREATE tProc.
    BUFFER-COPY procDia TO tProc.

    tProc.fecha_proc = ADD-INTERVAL(tProc.fecha_Proc,1,"years").
    tProc.estado = 1.
END.

FOR EACH tproc:
    CREATE procDia.
    BUFFER-COPY tProc TO ProcDia.
END.
