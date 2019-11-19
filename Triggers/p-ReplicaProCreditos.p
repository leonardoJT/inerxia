TRIGGER PROCEDURE FOR REPLICATION-WRITE OF bdcentral.pro_creditos OLD BUFFER oldbuf.

CREATE Replication.
ASSIGN Replication.entry-id = NEXT-VALUE(Sec_Replicar)
       Replication.reply-date = TODAY
       Replication.table-Name = "Pro_Creditos"
       Replication.task-id = DBTASKID(LDBNAME(BUFFER Replication))
       Replication.repl-event = "Write"
       Replication.key-1 = IF NEW(pro_creditos) THEN pro_creditos.cod_credito ELSE oldbuf.cod_credito.
       Replication.key-3 = IF NEW(pro_creditos) THEN pro_creditos.tip_credito ELSE oldbuf.tip_credito.
       
RAW-TRANSFER pro_creditos TO Replication.record.
