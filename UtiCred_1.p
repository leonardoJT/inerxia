/*Programa UtiCred_1.P...Créditos Adelantados sin Int_Anticipado"
  Julio 7/05 GAER*/

DEFI VAR Tot AS INTEG FORM "9999999" LABEL "No.Total : ".

OUTPUT TO C:\InfRed\CrAdel_SinAntic.Txt.

DISPLAY "COOP.BELEN     -    Créditos Adelantados sin Int-Anticipado - " +
        STRING (TODAY,"99/99/9999") FORM "X(150)" SKIP(1) 
    WITH WIDTH 200 FRAME FEnc NO-LABELS.

FOR EACH creditos WHERE creditos.sdo_capit GT 0     AND
                        fec_pago - TODAY   GT 45    AND
                        INT_Anticip        LE 0 NO-LOCK:
    Tot = Tot + 1.
    DISPLAY Agencia     LABEL "Ag."
            Nit         LABEL "Ced/Nit"
            Cod_Cred    LABEL "Pdcto"
            Num_credito LABEL "No.Crédito"
            Fec_Desem   LABEL "F-Desemb."
            Fec_UltPago LABEL "F-UltPago"
            Fec_pago    LABEL "F-Px.Pago"
            For_pago    LABEL "FP"
            Per_Pago    LABEL "PP"
            Plazo       LABEL "Plazo"
            Cuo_pagad   LABEL "C.Pagas"
            Monto       LABEL "Monto Desembolso" FORM "->>>>,>>>,>>9"
            Cuota       LABEL "Vr.de Cuota"      FORM "->>>,>>>,>>9"
            Sdo_Capital LABEL "Sdo.de Capital"   FORM "->>>>,>>>,>>9"
            INT_Corrien LABEL "Int-Corriente"    FORM "->>>>,>>>,>>9"
            (Capital_Acum - Sdo_CapPag) LABEL "Vr.Vdo/Adelant" FORM "->>>>,>>>,>>9"
        WITH DOWN WIDTH 200 FRAME F1 NO-BOX NO-LABELS STREAM-IO USE-TEXT.
END.
DISPLAY Tot.
OUTPUT CLOSE.

