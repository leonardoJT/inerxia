TRIGGER PROCEDURE FOR REPLICATION-WRITE OF bdcentral.pro_ahorros OLD BUFFER oldbuf.

CREATE Replication.
ASSIGN Replication.entry-id = NEXT-VALUE(Sec_Replicar)
       Replication.reply-date = TODAY
       Replication.table-Name = "Pro_Ahorros"
       Replication.task-id = DBTASKID(LDBNAME(BUFFER Replication))
       Replication.repl-event = "Write"
       Replication.key-1 = IF NEW(pro_ahorros) THEN pro_ahorros.cod_ahorro ELSE oldbuf.cod_ahorro.
       Replication.key-3 = IF NEW(pro_ahorros) THEN pro_ahorros.tip_ahorro ELSE oldbuf.tip_ahorro.
       
RAW-TRANSFER pro_ahorros TO Replication.record.
