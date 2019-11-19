TRIGGER PROCEDURE FOR REPLICATION-WRITE OF bdcentral.agencias OLD BUFFER oldbuf.

CREATE Replication.
ASSIGN Replication.entry-id = NEXT-VALUE(Sec_Replicar)
       Replication.reply-date = TODAY
       Replication.table-Name = "agencias"
       Replication.task-id = DBTASKID(LDBNAME(BUFFER Replication))
       Replication.repl-event = "Write"
       Replication.key-1 = IF NEW(agencias) THEN agencias.agencia ELSE oldbuf.agencia.

RAW-TRANSFER agencias TO Replication.record.
