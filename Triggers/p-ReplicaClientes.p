TRIGGER PROCEDURE FOR REPLICATION-WRITE OF bdcentral.clientes OLD BUFFER oldbuf.

CREATE Replication.
ASSIGN Replication.entry-id = NEXT-VALUE(Sec_Replicar)
       Replication.reply-date = TODAY
       Replication.table-Name = "Clientes"
       Replication.task-id = DBTASKID(LDBNAME(BUFFER Replication))
       Replication.repl-event = "Write"
       Replication.key-2 = IF NEW(clientes) THEN clientes.nit ELSE oldbuf.nit.

RAW-TRANSFER clientes TO Replication.record.
