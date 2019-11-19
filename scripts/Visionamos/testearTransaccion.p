DEFINE TEMP-TABLE ecg
        FIELD fecha AS CHARACTER /* Fecha */
    FIELD hora AS CHARACTER /* Hora */
    FIELD comercio AS CHARACTER /* Comercio */
    FIELD sucursal AS CHARACTER /* Sucursal */
    FIELD vTerminal AS CHARACTER /* Terminal */
    FIELD grupoTransaccional AS CHARACTER /* Grupo transaccional */
    FIELD transaccion AS CHARACTER /* Transacción */
    FIELD secuencia AS CHARACTER /* Secuencia */
    FIELD origen AS CHARACTER /* Origen */
    FIELD documentoCliente AS CHARACTER /* Documento Cliente */
    FIELD tipoDocumentoCliente AS CHARACTER /* Tipo de documento */
    FIELD documentoClienteDestino AS CHARACTER /* Documento Cliente Destino */
    FIELD tipoDocumentoClienteDestino AS CHARACTER /* Tipo de documento cliente destino */
    FIELD causalTransaccion AS CHARACTER /* Causal de la Transacción */
    FIELD adquiriente AS CHARACTER /* Adquieriente */
    FIELD numeroDeAutorizacion AS CHARACTER
    FIELD terminalUbicacion AS CHARACTER /* Terminal - Ubicación */
    FIELD PCI_TerminalSerial AS CHARACTER /* PCI Terminal Serial */
    FIELD TerminalTipo AS CHARACTER /* Terminal - Tipo */
    FIELD cuentaOrigen AS CHARACTER /* Cuenta 1 / Origen */
    FIELD cuentaDestino AS CHARACTER /* Cuenta 2 / Destino */
    FIELD entidadDuenaCuentaOrigen AS CHARACTER /* Entidad dueña cuenta origen */
    FIELD entidadDuenaCuentaDestino AS CHARACTER /* Entidad dueña cuenta destino */
    FIELD entidadDuenaTerminal AS CHARACTER /* Entidad dueña terminal */
    FIELD entidadDuenaTerminalNit AS CHARACTER /* Comercio - Terminal - NIT/CC */
    FIELD entidadDuenaTerminalDescripcion AS CHARACTER /* Comercio - Terminal - Descripción */
    FIELD chequeCodigoDeBanco AS CHARACTER /* Cheque - Código de banco */
    FIELD chequeCuentaDeBanco AS CHARACTER /* Cheque - Cuenta de banco */
    FIELD chequeNumero AS CHARACTER /* Cheque - Número */
    FIELD chequeTotalCheques AS CHARACTER /* Cheque - Total cheques */
    FIELD codigoDeBarras AS CHARACTER /* Código de barras */
    FIELD numeroTarjeta AS CHARACTER /* Número de tarjeta */
    FIELD fechaContable AS CHARACTER /* Fecha contable */
    FIELD valor AS CHARACTER /* Valor */
    FIELD valorBase AS CHARACTER /* Valor base */
    FIELD valorImpuesto AS CHARACTER /* Valor impuesto */
    FIELD valorRetencion AS CHARACTER /* Valor reteción */
    FIELD valorPropina AS CHARACTER /* Valor propina */
    FIELD valorComision AS CHARACTER /* Valor comisión */
    FIELD cuotas AS CHARACTER /* Cuotas */
    FIELD numeroDeFactura AS CHARACTER /* Número de factura */
    FIELD tipoCuentaOrigen AS CHARACTER /* Tipo de cuenta 1 / Origen */
    FIELD tipoCuentaDestino AS CHARACTER /* Tipo de cuenta 2 / Destino */
    FIELD secuenciaReversar AS CHARACTER /* Tipo de cuenta 3 */
    FIELD usuarioAutorizador AS CHARACTER /* Usuario / Autorizador */
    FIELD usuarioCaja AS CHARACTER /* Usuariol / Caja */
    FIELD codigoDeError AS CHARACTER /* codigo error */
    FIELD CONTABILIZADO AS LOGICAL.

INPUT FROM d:\Leonardo\test.aut.
REPEAT:
    CREATE ecg.
    IMPORT DELIMITER "," ecg.
END.

FOR EACH ecg NO-LOCK:
    FIND FIRST aplicarVisionamos WHERE aplicarVisionamos.fecha = ecg.fecha
                                   AND aplicarVisionamos.grupo_transaccional = ecg.grupotransaccional
                                   AND aplicarVisionamos.transaccion = ecg.transaccion
                                   AND aplicarVisionamos.secuencia = ecg.secuencia
                                   AND aplicarVisionamos.origen = ecg.origen
                                   AND aplicarVisionamos.doc_cliente = ecg.documentoCliente
                                   AND aplicarVisionamos.tipo_terminal = ecg.terminalTipo
                                   AND aplicarVisionamos.cuenta1_origen = ecg.cuentaOrigen
                                   AND aplicarVisionamos.cuenta2_destino = ecg.cuentaDestino
                                   AND aplicarVisionamos.num_tarjeta = ecg.numeroTarjeta
                                   AND DECIMAL(aplicarVisionamos.valor) = DECIMAL(ecg.valor)
                                   AND aplicarVisionamos.tipoCuenta1_origen = ecg.tipoCuentaOrigen
                                   AND aplicarVisionamos.tipoCuenta2_destino = ecg.tipoCuentaDestino
                                   AND aplicarVisionamos.estado = 2 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE aplicarVisionamos THEN DO:
        DISPLAY ecg WITH WIDTH 300 1 COL.
    END.
END.
