FOR EACH creditos WHERE cod_credito = 123 AND estado = 2 AND dias_atraso > 0 NO-LOCK:
    CREATE Replication.
    Replication.entry-id = NEXT-VALUE(Sec_Replicar).
    Replication.reply-date = TODAY.
    Replication.table-Name = "Creditos".
    Replication.task-id = DBTASKID(LDBNAME(BUFFER Replication)).
    Replication.repl-event = "Write".
    Replication.key-1 = creditos.agencia.
    Replication.key-2 = creditos.nit.
    Replication.key-3 = creditos.num_credito.
    replication.key-5 = creditos.num_solicitud.

    RAW-TRANSFER creditos TO Replication.record.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
