DEFI VAR Nro AS INTEG FORM "9999999" INIT 0.

OUTPUT TO C:\InfRed\Cred_Adelant.Txt.
DISPLAY "COOP.BELEN  - Creditos con Sdo.capital o Fec-Pago Posterior".    

FOR EACH Creditos WHERE Creditos.Sdo_Capital GT 0 
                    AND ((fec_pago   GE DATE(07,01,2005)) OR
                        (Capital_Acum LT Sdo_CapPag AND 
                         Sdo_CapPag - Capital_Acum GE Creditos.Cuota)) NO-LOCK
         BY Agencia BY nit:
    DISPLAY Agencia                       LABEL "Ag"
            nit                           LABEL "Ced.Nit"
            Num_credito                   LABEL "#-Crédito"
            Sdo_capital                   LABEL "Saldo-Capital"    FORM "->>>>>,>>>,>>9"
            INT_Anticip                   LABEL "Int-Anticipado"   FORM "->>>>>,>>>,>>9"
            INT_Corrient + INT_DifCob     LABEL "Int-Corrientes"   FORM "->>>>>,>>>,>>9"
            INT_MorCob + INT_MoraDifCob   LABEL "Int-Moratorio"    FORM "->>>>>,>>>,>>9"
            Fec_Desemb                    LABEL "Fec-Desemb."      
            Fec_UltPago                   LABEL "Fec-UltPago"
            Fec_pago                      LABEL "Prox.Pago"
            (Sdo_CapPag - Capital_Acum)   LABEL "Capital Adelant." FORM "->>>>>>,>>>,>>9"
            Creditos.Cuota                LABEL "Vlr.Cuota"        FORM ">>>>,>>>,>>9"
            Plazo                         LABEL "Plazo"
            Cuo_pagadas                   LABEL "CPagas"
        WITH DOWN WIDTH 180 FRAME F1 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
    Nro = Nro + 1.
END.

DISPLAY "   Nro.TOTAL : " Nro NO-LABELS.

OUTPUT CLOSE.
