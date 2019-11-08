DEFINE VARIABLE tda AS INTEGER. 
FOR EACH creditos WHERE dias_atraso GT 0 AND estado = 2 AND sdo_capital GT 0 AND agencia = 1:
    ASSIGN tda = TODAY - fec_pago.
    IF tda LT 0 THEN tda = 0.
    /*IF tda GT 0 THEN   */
     /*DISP fec_pago fec_ultpago dias_atraso tda val_atraso cuota.*/
     ASSIGN dias_atraso = tda
            val_atraso = 0.
     IF tda GT 0 THEN 
     ASSIGN val_atraso =  (1 + TRUNC(dias_atraso / 30,0))  * cuota.
     IF val_atraso GT sdo_capital THEN val_atraso = sdo_capital.
     

    

 END.
