TRIGGER PROCEDURE FOR REPLICATION-WRITE OF bdcentral.Ahorros OLD BUFFER oldbuf.

CREATE Replication.
ASSIGN Replication.entry-id = NEXT-VALUE(Sec_Replicar)
       Replication.reply-date = TODAY
       Replication.table-Name = "Ahorros"
       Replication.task-id = DBTASKID(LDBNAME(BUFFER Replication))
       Replication.repl-event = "Write"
       Replication.key-1 = IF NEW(Ahorros) THEN Ahorros.agencia ELSE oldbuf.agencia
       Replication.key-3 = IF NEW(Ahorros) THEN Ahorros.Cod_ahorro ELSE oldbuf.Cod_ahorro
       Replication.key-4 = IF NEW(Ahorros) THEN Ahorros.Cue_Ahorros ELSE oldbuf.Cue_Ahorros.
RAW-TRANSFER Ahorros TO Replication.record.
