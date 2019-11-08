OUTPUT TO c:\INFO_Utrahuilca\Mov_Creditos.
DEF VAR w_Saldo AS DEC NO-UNDO.
FOR EACH Mov_Creditos NO-LOCK
    BREAK BY Mov_Creditos.agencia BY Mov_Creditos.cod_Credito BY Mov_creditos.Cod_Operacion.
    w_saldo = w_saldo + Mov_Creditos.Val_Efectivo + Mov_Creditos.Val_Cheque.
    IF LAST-OF(Mov_Creditos.Cod_Operacion) THEN DO.
       FIND agencias     OF mov_creditos NO-LOCK NO-ERROR.
       FIND pro_creditos WHERE pro_creditos.cod_credito = mov_creditos.cod_credito NO-LOCK NO-ERROR.
       FIND operacion    OF mov_creditos NO-LOCK NO-ERROR.
       DISP Mov_Creditos.agencia agencias.nombre WHEN AVAIL agencias Mov_Creditos.cod_Credito pro_credito.nom WHEN AVAIL
            pro_credito Mov_creditos.Cod_Operacion operacion.nom WHEN AVAIL operacion abs(w_saldo) FORMAT "->>>,>>>,>>>,>>>.99" WITH WIDTH 250.
       w_saldo = 0.
    END.
END.
