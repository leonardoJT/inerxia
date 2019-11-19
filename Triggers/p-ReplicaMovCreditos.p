TRIGGER PROCEDURE FOR REPLICATION-WRITE OF bdcentral.mov_creditos OLD BUFFER oldbuf.

CREATE Replication.
ASSIGN Replication.entry-id = NEXT-VALUE(Sec_Replicar)
       Replication.reply-date = TODAY
       Replication.table-Name = "Mov_creditos"
       Replication.task-id = DBTASKID(LDBNAME(BUFFER Replication))
       Replication.repl-event = "Write".
       
RAW-TRANSFER mov_creditos TO Replication.record.
