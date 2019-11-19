TRIGGER PROCEDURE FOR REPLICATION-WRITE OF bdcentral.creditos OLD BUFFER oldbuf.

CREATE Replication.
ASSIGN Replication.entry-id = NEXT-VALUE(Sec_Replicar)
       Replication.reply-date = TODAY
       Replication.table-Name = "Creditos"
       Replication.task-id = DBTASKID(LDBNAME(BUFFER Replication))
       Replication.repl-event = "Write"
       Replication.key-1 = IF NEW(creditos) THEN creditos.agencia ELSE oldbuf.agencia.
       Replication.key-2 = IF NEW(creditos) THEN creditos.nit ELSE oldbuf.nit.
       Replication.key-3 = IF NEW(creditos) THEN creditos.num_credito ELSE oldbuf.num_credito.
       replication.key-5 = IF NEW(creditos) THEN creditos.num_solicitud ELSE oldbuf.num_solicitud.

RAW-TRANSFER creditos TO Replication.record.
