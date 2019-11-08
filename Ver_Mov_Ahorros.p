OUTPUT TO c:\INFO_Utrahuilca\Mov_Ahorros.
DEF VAR w_Saldo AS DEC FORMAT "->>>,>>>,>>>,>>>.99" NO-UNDO.
FOR EACH Mov_Ahorros NO-LOCK
    BREAK BY Mov_Ahorros.agencia BY Mov_Ahorros.cod_Ahorro BY Mov_Ahorros.cod_OPeracion.
    w_saldo = w_saldo + Mov_Ahorros.val_Efectivo + val_cheque.
    IF LAST-OF(Mov_Ahorros.cod_Operacion) THEN DO.
       DISP Mov_Ahorros.agencia Mov_Ahorros.cod_Ahorro  Mov_Ahorros.cod_OPeracion w_saldo.
       w_saldo = 0.
    END.
END.
