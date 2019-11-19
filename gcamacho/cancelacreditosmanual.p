
OUTPUT TO "c:\INFO_fodun\creditosCancelados.csv".
PUT "agencia ;nit ;tip_credito ;cod_credito ;num_credito ;estado ;Fec_UltPago ;Fec_CanceTotal" SKIP.
FOR EACH creditos WHERE sdo_capital EQ 0 AND estado EQ 2 EXCLUSIVE-LOCK:
    IF Sdo_CapPag + Sdo_IntMor + Sdo_IntPag + Int_Anticipado + Int_Corrientes + 
        Int_DifCobro + Int_MoraDifCob + Int_MorCobrar EQ 0 THEN DO:

        EXPORT DELIMITER ";" agencia nit tip_credito cod_credito num_credito estado Fec_UltPago Fec_CanceTotal.

        UPDATE Creditos.Fec_CanceTotal = TODAY - 1
            Creditos.Fec_UltPago = TODAY - 1
            Creditos.estado = 3.
    END.
END.
OUTPUT CLOSE.
