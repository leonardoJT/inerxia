TRIGGER PROCEDURE FOR REPLICATION-WRITE OF bdcentral.habiles OLD BUFFER oldbuf.

CREATE Replication.
ASSIGN Replication.entry-id = NEXT-VALUE(Sec_Replicar)
       Replication.reply-date = TODAY
       Replication.table-Name = "habiles"
       Replication.task-id = DBTASKID(LDBNAME(BUFFER Replication))
       Replication.repl-event = "Write"
       Replication.key-2 = IF NEW(habiles) THEN habiles.nit ELSE oldbuf.nit.

RAW-TRANSFER habiles TO Replication.record.
