OUTPUT TO c:\INFO_Utrahuilca\Ahorros.
DEF VAR w_Saldo AS DEC FORMAT "->>>,>>>,>>>,>>>.99" NO-UNDO.
FOR EACH Ahorros NO-LOCK
    BREAK BY Ahorros.agencia BY Ahorros.tip_Ahorro BY Ahorros.cod_Ahorro.
    w_saldo = w_saldo + Ahorros.sdo_disponible + sdo_canje.
    IF LAST-OF(Ahorros.cod_Ahorro) THEN DO.
       FIND agencias   OF ahorros NO-LOCK NO-ERROR.
       FIND pro_ahorro OF ahorros NO-LOCK NO-ERROR.
       DISP Ahorros.agencia agencias.nombre WHEN AVAIL agencias int(Ahorros.tip_Ahorro)
            Ahorros.cod_Ahorro pro_ahorros.nom WHEN AVAIL pro_ahorros w_saldo WITH WIDTH 250.
       w_saldo = 0.
    END.
END.
