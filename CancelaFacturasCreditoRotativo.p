/* Cancelación completa de facturas de cupos rotativos */

DEFINE INPUT PARAMETER pNit AS CHARACTER.
DEFINE INPUT PARAMETER pNumCredito AS INTEGER.

FOR EACH facturacion WHERE facturacion.nit = pNit
                       AND facturacion.num_credito = pNumCredito
                       AND facturacion.estado = 1:
    facturacion.pago_capital = facturacion.capital.
    facturacion.pago_intCorriente = facturacion.int_corriente.
    facturacion.pago_intDifCobro = facturacion.int_difCobro.
    facturacion.pago_mora = facturacion.int_mora.
    facturacion.estado = 2.
END.
