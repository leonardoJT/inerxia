OUTPUT TO c:\INFO_Utrahuilca\Creditos.
DEF VAR w_Saldo AS DEC FORMAT "->>>,>>>,>>>,>>>.99" NO-UNDO.
FOR EACH Creditos NO-LOCK
    BREAK BY Creditos.agencia BY Creditos.tip_Credito BY Creditos.cod_Credito.
    w_saldo = w_saldo + Creditos.sdo_capital.
    IF LAST-OF(Creditos.cod_Credito) THEN DO.
       DISP Creditos.agencia int(Creditos.tip_Credito) Creditos.cod_Credito w_saldo.
       w_saldo = 0.
    END.
END.
