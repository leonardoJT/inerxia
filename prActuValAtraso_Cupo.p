DEFINE VARIABLE wperiodo       AS INTEGER.
DEFINE VARIABLE wfec_Pproxpago AS DATE.

FIND FIRST per_facturacion WHERE per_facturacion.estado = 1 NO-LOCK NO-ERROR.
IF AVAILABLE(per_facturacion) THEN DO:
   ASSIGN wperiodo = per_facturacion.per_factura +  1.  
   FIND FIRST per_facturacion WHERE per_facturacion.per_factura = wperiodo NO-LOCK NO-ERROR.
   IF AVAILABLE(per_facturacion) THEN
      ASSIGN wfec_Pproxpago  = per_facturacion.fec_limpago.
END.
                
FOR EACH creditos WHERE 
    creditos.sdo_capital  EQ 0   AND creditos.val_atraso GT 0 AND
    (creditos.cod_credito EQ 570 OR creditos.cod_credito EQ 870):
    UPDATE creditos.fec_pago = wfec_Pproxpago
           creditos.dias_atraso = 0
           creditos.val_atraso  = 0.
END.

/* DEFINE VARIABLE wperiodo AS INTEGER.                                                          */
/* FIND FIRST per_facturacion WHERE per_facturacion.estado = 1 NO-LOCK NO-ERROR.                 */
/* IF AVAILABLE(per_facturacion) THEN DO:                                                        */
/*    wperiodo = per_facturacion.per_factura +  1.                                               */
/*    FIND FIRST per_facturacion WHERE per_facturacion.per_factura = wperiodo NO-LOCK NO-ERROR.  */
/*    IF AVAILABLE(per_facturacion) THEN                                                         */
/*        DISPLAY per_facturacion.fec_limpago.                                                   */
/* END.                                                                                          */

