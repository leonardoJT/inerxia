OUTPUT TO C:\INFO_utrahuilca\Cred_ReIni.Txt.
DISPLAY "COOP. UTRAHUILCA  -      CREDITOS REINICIADOS      " 
        + STRING(TODAY,"99/99/9999") FORM "X(100)" SKIP(1)
    WITH WIDTH 120 NO-LABELS.
FOR EACH PlanPagos WHERE Id_pdoMes EQ 4 NO-LOCK 
                BREAK BY PlanPagos.Num_Credito:
    IF FIRST-OF(PlanPagos.Num_Credito) THEN DO:
       FIND FIRST Creditos WHERE Creditos.Nit         EQ PlanPagos.Nit
                             AND Creditos.Num_Credito EQ PlanPagos.Num_Credito
                             AND Creditos.Estado      EQ 2 NO-LOCK NO-ERROR. 
       IF AVAIL(Creditos) THEN DO:
          FIND FIRST Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
          DISPLAY Creditos.Agencia     LABEL "Ag."
                  Creditos.Nit         LABEL "Ced./Nit"
                  Creditos.Num_Credito LABEL "Pagare No."
                  Clientes.Cod_Empresa LABEL "Empresa"
                  Creditos.Sdo_Capital LABEL "Saldo-Capital"
                  Creditos.Val_Atraso  LABEL "Saldo Vencido"
                  Creditos.Fec_pago    LABEL "Fec.ProxPago"
              WITH DOWN WIDTH 120 FRAME F1 NO-BOX NO-LABELS STREAM-IO USE-TEXT.       
       END.
    END.
END.
OUTPUT CLOSE.
