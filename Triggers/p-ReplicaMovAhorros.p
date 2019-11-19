TRIGGER PROCEDURE FOR REPLICATION-WRITE OF bdcentral.mov_ahorros OLD BUFFER oldbuf.

CREATE Replication.
ASSIGN Replication.entry-id = NEXT-VALUE(Sec_Replicar)
       Replication.reply-date = TODAY
       Replication.table-Name = "Mov_ahorros"
       Replication.task-id = DBTASKID(LDBNAME(BUFFER Replication))
       Replication.repl-event = "Write".
       
RAW-TRANSFER mov_ahorros TO Replication.record.
