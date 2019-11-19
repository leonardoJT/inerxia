TRIGGER PROCEDURE FOR REPLICATION-WRITE OF bdcentral.ubicacion OLD BUFFER oldbuf.

CREATE Replication.
ASSIGN Replication.entry-id = NEXT-VALUE(Sec_Replicar)
       Replication.reply-date = TODAY
       Replication.table-Name = "Ubicacion"
       Replication.task-id = DBTASKID(LDBNAME(BUFFER Replication))
       Replication.repl-event = "Write"
       Replication.key-2 = IF NEW(ubicacion) THEN ubicacion.ubicacion ELSE oldbuf.ubicacion.

RAW-TRANSFER ubicacion TO Replication.record.
