{Incluido\variable.i "NEW GLOBAL SHARED"}

Def var ObjRecordSet as com-handle no-undo.
Def var ObjConnection as com-handle no-undo.
Def var ObjCommand as com-handle no-undo.
Def var ODBC-DSN as character no-undo.
Def var ODBC-SERVER as character no-undo.

/* oakley */

Def var ODBC-USERID as character no-undo.
Def var ODBC-PASSWD as character no-undo.
Def var ODBC-QUERY as character no-undo.
Def var ODBC-STATUS as character no-undo.
Def var ODBC-RECCOUNT as integer no-undo.
Def var ODBC-NULL as character no-undo.
Def var ODBC-CURSOR as integer no-undo.

DEFINE VAR flag AS LOGICAL INITIAL FALSE.

DEFINE TEMP-TABLE tt
    FIELD indx AS CHARACTER /* Índice */
    FIELD s001 AS CHARACTER /* Fecha */
    FIELD s002 AS CHARACTER /* Hora */
    FIELD s003 AS CHARACTER /* Comercio */
    FIELD s004 AS CHARACTER /* Sucursal */
    FIELD s005 AS CHARACTER /* Terminal */
    FIELD s007 AS CHARACTER /* Grupo Transaccional */
    FIELD s008 AS CHARACTER /* Transacción */
    FIELD s009 AS CHARACTER /* Secuencia */
    FIELD s010 AS CHARACTER /* Origen' */
    FIELD s011 AS CHARACTER /* Documento Cliente */
    FIELD s012 AS CHARACTER /* Tipo Documento */
    FIELD s015 AS CHARACTER /* Causal transacción */
    FIELD s016 AS CHARACTER /* Adquiriente */
    FIELD s018 AS CHARACTER /* Canal */
    FIELD s01f AS CHARACTER /* Serial Terminal */
    FIELD s01s AS CHARACTER /* Terminal - Ubicación */
    FIELD s01t AS CHARACTER /* Tipo Terminal */
    FIELD s020 AS CHARACTER /* Cuenta 1 / Origen */
    FIELD s021 AS CHARACTER /* Cuenta 2 / Destino */
    FIELD s025 AS CHARACTER /* Entidad Dueña Cuenta Origen */
    FIELD s026 AS CHARACTER /* Entidad dueña Cuenta Destino */
    FIELD s027 AS CHARACTER /* Entidad dueña Terminal */
    FIELD s02a AS CHARACTER /* Cheque - Código de Banco */
    FIELD s02b AS CHARACTER /* Cheque - Cuenta de Banco */
    FIELD s02c AS CHARACTER /* Cheque - Número */
    FIELD s030 AS CHARACTER /* Número de Tarjeta */
    FIELD s031 AS CHARACTER /* Fecha contable */
    FIELD s032 AS DECIMAL /* Valor */
    FIELD s033 AS DECIMAL /* Valor Base */
    FIELD s034 AS DECIMAL /* Valor Impuesto */
    FIELD s035 AS DECIMAL /* Valor Retención */
    FIELD s036 AS DECIMAL /* Valor propina */
    FIELD s037 AS DECIMAL /* Valor comisión */
    FIELD s03a AS CHARACTER /* Tipo de Cuenta 1 / Origen  - 10 - Ahorros, 50 - Cupos */
    FIELD s03b AS CHARACTER /* Tipo de Cuenta 2 / Destino */
    FIELD s03x AS CHARACTER /* Usuario / Caja */
    FIELD s03k AS CHARACTER /* Referencia */
    FIELD s050 AS INTEGER /* Código de error */
    FIELD s056 AS DECIMAL /* Valor comisión (aplicada) */
    FIELD gmf0 AS DECIMAL /* Valor GMF (APlicada) */
    FIELD s201 AS CHARACTER /* Flag Reverso / 0- Automático, 1- Manual */
    FIELD s203 AS CHARACTER /* Inter Entidad / 0- No, 1- Si */
    FIELD s204 AS CHARACTER /* Transacción gratis / 0- No, 1- Si */
    FIELD s204r AS CHARACTER /* Transacción gratis (Externa) / 0- No, 1- Si */
    FIELD sx03 AS CHARACTER /* Canal a Activar/Bloquear */
    FIELD sx04 AS CHARACTER /* Códigos de bloqueos */

    FIELD sx07 AS CHARACTER /* Tipo de tarjeta / 0- Privada, 1- Visa, 2- Master */
    INDEX idx1 indx.

/* Variables de salida para el reporte de las transacciones a Visionamos */
DEFINE VAR v_INDX AS CHARACTER.                       /* Índice */
DEFINE VAR v_NTRY AS INTEGER INITIAL 0.               /* Número de intentos */
DEFINE VAR v_s001 AS CHARACTER.                       /* Fecha */
DEFINE VAR v_s002 AS CHARACTER.                       /* Hora */
DEFINE VAR v_s003 AS CHARACTER INITIAL "00000018".    /* Comercio */
DEFINE VAR v_s004 AS CHARACTER.                       /* Sucursal */
DEFINE VAR v_s005 AS CHARACTER INITIAL "00".          /* Terminal */
DEFINE VAR v_s007 AS CHARACTER.                       /* Grupo Transaccional */
DEFINE VAR v_s008 AS CHARACTER.                       /* Transacción */
DEFINE VAR v_s009 AS CHARACTER.                       /* Secuencia */
DEFINE VAR v_s010 AS CHARACTER INITIAL "06".          /* Origen */
DEFINE VAR v_s011 AS CHARACTER.                       /* Documento Cliente */
DEFINE VAR v_s012 AS CHARACTER.                       /* Tipo documento */
DEFINE VAR v_s015 AS CHARACTER INITIAL "000000".      /* Causal de transacción */
DEFINE VAR v_s018 AS CHARACTER INITIAL "00".          /* Canal */
DEFINE VAR v_s01f AS CHARACTER.                       /* Serial Terminal */
DEFINE VAR v_s01t AS CHARACTER INITIAL "00".          /* Tipo Terminal */
DEFINE VAR v_s020 AS CHARACTER.                       /* Cuenta 1 / Origen */
DEFINE VAR v_s021 AS CHARACTER.                       /* Cuenta 2 / Destino */
DEFINE VAR v_s025 AS CHARACTER INITIAL "00000018".    /* Entidad Dueña Cuenta Origen */
DEFINE VAR v_s026 AS CHARACTER.                       /* Entidad dueña Cuenta Destino */
DEFINE VAR v_s027 AS CHARACTER INITIAL "00000018".    /* Entidad dueña Terminal */
DEFINE VAR v_s030 AS CHARACTER.                       /* Número de Tarjeta */
DEFINE VAR v_s031 AS CHARACTER.                       /* Fecha contable */
DEFINE VAR v_s032 AS DECIMAL.                         /* Valor */
DEFINE VAR v_s033 AS DECIMAL.                         /* Valor Base */
DEFINE VAR v_s034 AS DECIMAL.                         /* Valor Impuesto */
DEFINE VAR v_s035 AS DECIMAL.                         /* Valor Retención */
DEFINE VAR v_s036 AS DECIMAL.                         /* Valor propina */
DEFINE VAR v_s037 AS DECIMAL.                         /* Valor comisión */
DEFINE VAR v_s03a AS CHARACTER.                       /* Tipo de Cuenta 1 / Origen */
DEFINE VAR v_s03b AS CHARACTER.                       /* Tipo de Cuenta 2 / Destino */
DEFINE VAR v_s03x AS CHARACTER.                       /* Usuario / Caja */
DEFINE VAR v_s03k AS CHARACTER.                       /* Referencia */
DEFINE VAR v_gmf3 AS INTEGER.                           /* Aplica Saldo Acumulado GMF */
DEFINE VAR v_s050 AS INTEGER.                         /* Código de error */
/* --------------------------------------------------------------------- */

DEFINE VAR vMonto AS DECIMAL.
DEFINE VAR vComision AS DECIMAL.
DEFINE VAR pNumDocumento AS INTEGER.
DEFINE VAR cuentaSyA AS CHARACTER.
DEFINE VAR cuentaRotativo AS CHARACTER.
DEFINE VAR cuentaAhorro AS CHARACTER.
DEFINE VAR vRev AS CHARACTER.
DEFINE VAR vAgencia AS INTEGER.

OUTPUT TO VALUE("c:\Visionamos\LOG_" + STRING(DAY(TODAY),"99") + STRING(MONTH(TODAY),"99") + STRING(YEAR(TODAY),"9999") + "_" + string(time) + ".txt").
REPEAT:
    IF TIME > 84600 THEN DO:
        RUN desconexion.

        DISCONNECT bdcentral.
        QUIT.
    END.

    PUT "Write_" + STRING(TIME,"HH:MM:SS") FORMAT "X(15)" SKIP.

    FOR EACH reportarVisionamos WHERE reportarVisionamos.fecha = TODAY AND reportarVisionamos.estado = 1:
        EXPORT DELIMITER ";"
               reportarVisionamos.nit
               reportarVisionamos.numCuenta
               reportarVisionamos.tarjeta
               reportarVisionamos.valor
               reportarVisionamos.tipoTrans.

        v_INDX = STRING(YEAR(reportarVisionamos.fecha),"9999") + STRING(MONTH(reportarVisionamos.fecha),"99") + STRING(DAY(reportarVisionamos.fecha),"99") + ":" +
                 reportarVisionamos.nit + ":" +
                 SUBSTRING(reportarVisionamos.tarjeta,1,16) + ":".

        CASE reportarVisionamos.tipoTrans:
            WHEN 'R' THEN v_INDX = v_INDX + "01:".
        END CASE.

        v_INDX = v_INDX + "0200:01:$FLG:" + STRING(TIME,"99999") + ":$ADQ:".
        v_s001 = STRING(YEAR(reportarVisionamos.fecha),"9999") + STRING(MONTH(reportarVisionamos.fecha),"99") + STRING(DAY(reportarVisionamos.fecha),"99").
        v_s002 = REPLACE(STRING(TIME,"HH:MM:SS"),":","").
        v_s004 = STRING(reportarVisionamos.agencia,"9999").
        v_s007 = "0220".

        CASE reportarVisionamos.tipoTrans:
            WHEN 'R' THEN v_s008 = "01".
            WHEN 'C' THEN v_s008 = "21".
        END CASE.

        v_s009 = STRING(DAY(reportarVisionamos.fecha),"99") + STRING(MONTH(reportarVisionamos.fecha),"99") + STRING(YEAR(reportarVisionamos.fecha),"9999") + REPLACE(STRING(TIME,"HH:MM:SS"),":","").
        v_s011 = reportarVisionamos.nit.

        CASE reportarVisionamos.tipoDoc:
            WHEN 'T.I' THEN v_s012 = '0'.
            WHEN 'C.C' THEN v_s012 = '1'.
            WHEN 'NIT' THEN v_s012 = '2'.
            WHEN 'C.E' THEN v_s012 = '3'.
        END CASE.

        v_s020 = reportarVisionamos.numCuenta.
        v_s030 = TRIM(reportarVisionamos.tarjeta).
        
        v_s031 = STRING(YEAR(reportarVisionamos.fecha),"9999") + STRING(MONTH(reportarVisionamos.fecha),"99") + STRING(DAY(reportarVisionamos.fecha),"99").
        v_s032 = reportarVisionamos.valor * 100.

        CASE reportarVisionamos.tipoCuenta:
            WHEN "AH" THEN v_s03a = "10".
            WHEN "CR" THEN v_s03a = "50".
        END CASE.

        v_s03x = reportarVisionamos.usuario.

        ODBC-QUERY = "INSERT INTO dbo.trnout(indx,ntry,s001,s002,s003,s004,s005,s007,s008,s009,s010,s011,s012,s015,s018,s01F,s01T,s020,s025,s027,s030,s031,s032,s03a,s03x,s03k,gmf3,s050) VALUES (" +
                     "'" + v_INDX + "'" + "," +
                     "'" + STRING(v_ntry) + "'" + "," +
                     "'" + v_S001 + "'" + "," +
                     "'" + v_S002 + "'" + "," +
                     "'" + v_S003 + "'" + "," +
                     "'" + v_S004 + "'" + "," +
                     "'" + v_S005 + "'" + "," +
                     "'" + v_S007 + "'" + "," +
                     "'" + v_S008 + "'" + "," +
                     "'" + v_S009 + "'" + "," +
                     "'" + v_S010 + "'" + "," +
                     "'" + v_S011 + "'" + "," +
                     "'" + v_S012 + "'" + "," +
                     "'" + v_S015 + "'" + "," +
                     "'" + v_S018 + "'" + "," +
                     "'" + v_S01F + "'" + "," +
                     "'" + v_S01T + "'" + "," +
                     "'" + v_S020 + "'" + "," +
                     "'" + v_S025 + "'" + "," +
                     "'" + v_S027 + "'" + "," +
                     "'" + v_S030 + "'" + "," +
                     "'" + v_S031 + "'" + "," +
                     "'" + STRING(v_S032) + "'" + "," +
                     "'" + v_S03A + "'" + "," +
                     "'" + v_S03X + "'" + "," +
                     "'" + v_S03K + "'" + "," +
                     "'" + STRING(v_gmf3) + "'" + "," +
                     "'" + STRING(v_S050) + "'" + ");".

        RUN conexion.

        /* Check for connection errors */
        If (error-status:num-messages <= 0) THEN
            reportarVisionamos.estado = 2.

        RUN desconexion.
    END.

    PUT "Read_" + STRING(TIME,"HH:MM:SS") FORMAT "X(15)" SKIP.

    EMPTY TEMP-TABLE tt.

    ODBC-QUERY = "SELECT * FROM dbo.trninp WHERE S001 = '" + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + "'".
    /*ODBC-QUERY = "SELECT * FROM dbo.trninp WHERE S001 = '20151129'".*/

    RUN conexion.
    ODBC-RECCOUNT = ObjRecordSet:RecordCount.

    If (error-status:num-messages <= 0) THEN DO:
        If (ODBC-RECCOUNT > 0) and not (ODBC-RECCOUNT = ?) then Do:
            odbc-cursor = 0.
            
            ObjRecordSet:MoveFirst no-error.

            Do while ODBC-CURSOR < ODBC-RECCOUNT:
                CREATE tt.
                ASSIGN tt.indx = ObjRecordSet:Fields("indx"):Value
                       tt.s001 = ObjRecordSet:Fields("s001"):VALUE
                       tt.s002 = ObjRecordSet:Fields("s002"):VALUE
                       tt.s003 = ObjRecordSet:FIELDS("s003"):VALUE
                       tt.s004 = ObjRecordSet:FIELDS("s004"):VALUE
                       tt.s005 = ObjRecordSet:FIELDS("s005"):VALUE
                       tt.s007 = ObjRecordSet:FIELDS("s007"):VALUE
                       tt.s008 = ObjRecordSet:FIELDS("s008"):VALUE
                       tt.s009 = ObjRecordSet:FIELDS("s009"):VALUE
                       tt.s010 = ObjRecordSet:FIELDS("s010"):VALUE
                       tt.s011 = ObjRecordSet:FIELDS("s011"):VALUE
                       tt.s012 = ObjRecordSet:FIELDS("s012"):VALUE
                       tt.s015 = ObjRecordSet:FIELDS("s015"):VALUE
                       tt.s016 = ObjRecordSet:FIELDS("s016"):VALUE
                       tt.s018 = ObjRecordSet:FIELDS("s018"):VALUE
                       tt.s01f = ObjRecordSet:FIELDS("s01f"):VALUE
                       tt.s01s = ObjRecordSet:FIELDS("s01s"):VALUE
                       tt.s01t = ObjRecordSet:FIELDS("s01t"):VALUE
                       tt.s020 = ObjRecordSet:FIELDS("s020"):VALUE
                       tt.s021 = ObjRecordSet:FIELDS("s021"):VALUE
                       tt.s025 = ObjRecordSet:FIELDS("s025"):VALUE
                       tt.s026 = ObjRecordSet:FIELDS("s026"):VALUE
                       tt.s027 = ObjRecordSet:FIELDS("s027"):VALUE
                       tt.s02a = ObjRecordSet:FIELDS("s02a"):VALUE
                       tt.s02b = ObjRecordSet:FIELDS("s02b"):VALUE
                       tt.s02c = ObjRecordSet:FIELDS("s02c"):VALUE
                       tt.s030 = ObjRecordSet:FIELDS("s030"):VALUE
                       tt.s031 = ObjRecordSet:FIELDS("s031"):VALUE
                       tt.s032 = ObjRecordSet:FIELDS("s032"):VALUE
                       tt.s033 = ObjRecordSet:FIELDS("s033"):VALUE
                       tt.s034 = ObjRecordSet:FIELDS("s034"):VALUE
                       tt.s035 = ObjRecordSet:FIELDS("s035"):VALUE
                       tt.s036 = ObjRecordSet:FIELDS("s036"):VALUE
                       tt.s037 = ObjRecordSet:FIELDS("s037"):VALUE
                       tt.s03a = ObjRecordSet:FIELDS("s03a"):VALUE
                       tt.s03b = ObjRecordSet:FIELDS("s03b"):VALUE
                       tt.s03x = ObjRecordSet:FIELDS("s03x"):VALUE
                       tt.s03k = ObjRecordSet:FIELDS("s03k"):VALUE
                       tt.s050 = ObjRecordSet:FIELDS("s050"):VALUE
                       tt.s056 = ObjRecordSet:FIELDS("s056"):VALUE
                       tt.gmf0 = ObjRecordSet:FIELDS("gmf0"):VALUE
                       tt.s201 = ObjRecordSet:FIELDS("s201"):VALUE
                       tt.s203 = ObjRecordSet:FIELDS("s203"):VALUE
                       tt.s204 = ObjRecordSet:FIELDS("s204"):VALUE
                       tt.s204r = ObjRecordSet:FIELDS("s204r"):VALUE
                       tt.sx03 = ObjRecordSet:FIELDS("sx03"):VALUE
                       tt.sx04 = ObjRecordSet:FIELDS("sx04"):VALUE
                       tt.sx07 = ObjRecordSet:FIELDS("sx07"):VALUE.

                ODBC-CURSOR = ODBC-CURSOR + 1.
                ObjRecordSet:MoveNext no-error.
            End. /* retrieved a single data row */
        End. /* retrieved all data rows */
        ELSE
            ODBC-STATUS = "No records found.".
    End.

    RUN desconexion.

    FOR EACH tt:
        FIND FIRST aplicarVisionamos WHERE aplicarVisionamos.indx = tt.indx NO-ERROR.
        IF NOT AVAILABLE aplicarVisionamos OR (AVAILABLE aplicarVisionamos AND aplicarVisionamos.estado = 1) THEN DO:
            IF NOT AVAILABLE aplicarVisionamos THEN DO:
                CREATE aplicarVisionamos.
                ASSIGN aplicarVisionamos.indx = tt.indx
                       aplicarVisionamos.fecha = tt.s001
                       aplicarVisionamos.hora = tt.s002
                       aplicarVisionamos.comercio = tt.s003
                       aplicarVisionamos.sucursal = tt.s004
                       aplicarVisionamos.terminal_ = tt.s005
                       aplicarVisionamos.grupo_Transaccional = tt.s007
                       aplicarVisionamos.transaccion = tt.s008
                       aplicarVisionamos.secuencia = tt.s009
                       aplicarVisionamos.origen = tt.s010
                       aplicarVisionamos.doc_Cliente = tt.s011
                       aplicarVisionamos.tipo_doc = tt.s012
                       aplicarVisionamos.causal_transaccion = tt.s015
                       aplicarVisionamos.adquiriente = tt.s016
                       aplicarVisionamos.canal = tt.s018
                       aplicarVisionamos.serial_terminal = tt.s01f
                       aplicarVisionamos.terminal_ubicacion = tt.s01s
                       aplicarVisionamos.tipo_terminal = tt.s01t
                       aplicarVisionamos.cuenta1_origen = tt.s020
                       aplicarVisionamos.cuenta2_destino = tt.s021
                       aplicarVisionamos.entidadDuenaCuentaOrigen = tt.s025
                       aplicarVisionamos.entidadDuenaCuentaDestino = tt.s026
                       aplicarVisionamos.entidadDuenaTerminal = tt.s027
                       aplicarVisionamos.chequeCodigoBanco = tt.s02a
                       aplicarVisionamos.chequeCuentaBanco = tt.s02b
                       aplicarVisionamos.chequeNumero = tt.s02c
                       aplicarVisionamos.num_tarjeta = tt.s030
                       aplicarVisionamos.fec_contable = tt.s031
                       aplicarVisionamos.valor = tt.s032
                       aplicarVisionamos.valorBase = tt.s033
                       aplicarVisionamos.valorImpuesto = tt.s034
                       aplicarVisionamos.valorRetencion = tt.s035
                       aplicarVisionamos.valorPropina = tt.s036
                       aplicarVisionamos.valorComision = tt.s037
                       aplicarVisionamos.tipoCuenta1_origen = tt.s03a
                       aplicarVisionamos.tipoCuenta2_destino = tt.s03b
                       aplicarVisionamos.usuario = tt.s03x
                       aplicarVisionamos.cod_error = tt.s050
                       aplicarVisionamos.valorComisionAplicada = tt.s056
                       aplicarVisionamos.flagreverso = tt.s201
                       aplicarVisionamos.interEntidad = tt.s203
                       aplicarVisionamos.transaccionGratis = tt.s204
                       aplicarVisionamos.transaccionGratisExterna = tt.s204r
                       aplicarVisionamos.canalActivarBloquear = tt.sx03
                       aplicarVisionamos.cod_bloqueo = tt.sx04
                       aplicarVisionamos.estado = 1.
            END.
            
            EXPORT DELIMITER ";"
                aplicarVisionamos.doc_cliente
                aplicarVisionamos.cuenta1_origen
                aplicarVisionamos.num_tarjeta
                aplicarVisionamos.valor / 100
                aplicarVisionamos.valorComision / 100
                aplicarVisionamos.grupo_Transaccional
                aplicarVisionamos.transaccion.

            FIND FIRST clientes WHERE clientes.nit = aplicarVisionamos.doc_cliente NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                FIND FIRST ahorros WHERE ahorros.nit = clientes.nit
                                     AND ahorros.cue_ahorros = aplicarVisionamos.cuenta1_origen
                                     AND ahorros.estado = 1 NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ahorros THEN
                    FIND FIRST ahorros WHERE ahorros.nit = clientes.nit
                                         AND ahorros.cod_ahorro = 4
                                         AND ahorros.estado = 1 NO-LOCK NO-ERROR.

                FIND FIRST creditos WHERE creditos.nit = clientes.nit
                                                AND creditos.num_credito = DECIMAL(aplicarVisionamos.cuenta1_origen)
                                                AND creditos.estado = 2 NO-LOCK NO-ERROR.
                IF NOT AVAILABLE creditos THEN
                    FIND FIRST creditos WHERE creditos.nit = clientes.nit
                                          AND creditos.cod_credito = 123
                                          AND creditos.estado = 2 NO-LOCK NO-ERROR.

                IF AVAILABLE ahorros OR AVAILABLE creditos THEN DO:
                    /* Buscamos una tarjeta activa diferente a la tarjeta con que fue realizada la transacción, y si existe la bloqueamos */
                    FIND FIRST tarjetas WHERE tarjetas.nit = clientes.nit
                                          AND trim(tarjetas.tarjetaDB) <> trim(aplicarVisionamos.num_tarjeta)
                                          AND tarjetas.estado = '01' NO-ERROR.
                    IF AVAILABLE tarjetas THEN DO:
                        ASSIGN tarjetas.estado = "62"
                               tarjetas.fec_bloqueo = TODAY
                               tarjetas.hora_bloqueo = TIME.
                    END.

                    /* Buscamos que la tarjeta exista ya matriculada y activa. De lo contrario se crea */
                    FIND FIRST tarjetas WHERE tarjetas.nit = clientes.nit
                                          AND trim(tarjetas.tarjetaDB) = trim(aplicarVisionamos.num_tarjeta)
                                          AND tarjetas.estado = '01' NO-ERROR.
                    IF NOT AVAILABLE tarjetas THEN DO:
                        CREATE tarjetas.
                        ASSIGN tarjetas.nit = clientes.nit
                               tarjetas.tarjetaDB = trim(aplicarVisionamos.num_tarjeta)
                               tarjetas.estado = "01"
                               tarjetas.fec_cargue = TODAY
                               tarjetas.hora_cargue = TIME
                               tarjetas.agencia = clientes.agencia
                               tarjetas.usuario = "TD".
                    END.

                    ASSIGN tarjetas.fec_ulttransac = TODAY
                           tarjetas.hora_ulttrans = TIME
                           tarjetas.cue_ahorros = ahorros.cue_ahorros WHEN AVAILABLE ahorros
                           tarjetas.num_credito = creditos.num_Credito WHEN AVAILABLE creditos.

                    FIND FIRST cfg_tarjetaDB NO-LOCK NO-ERROR.
                    IF AVAILABLE cfg_tarjetaDB THEN DO:
                        tarjetas.operMaxCaj = cfg_tarjetaDB.numeroRetirosCajeroDia.
                        tarjetas.operMaxPOS = cfg_tarjetaDB.numeroRetirosPOSDia.
                        tarjetas.montoMaxCaj = cfg_tarjetaDB.montoRetirosCajeroDia.
                        tarjetas.montoMaxPOS = cfg_tarjetaDB.montoRetirosPOSDia.
                    END.
                END.
            END.
            /* -*-*-*-*-*-*-*-*-*-*-*- */

            vMonto = aplicarVisionamos.valor / 100.
            vComision = aplicarVisionamos.valorComision / 100.

            /* Si es una notificación de transacción declinada, el monto se pone en $0 */
            IF aplicarVisionamos.grupo_transaccional = "0220" THEN
                vMonto = 0.

            /* Si trae código de error significa que no fue exitosa */
            IF aplicarVisionamos.cod_error <> 0 THEN
                vMonto = 0.

            /* Si la transacción es una consulta de saldo teléfonica (IVR), se debe cargar el valor de la comisión */
            IF aplicarVisionamos.transaccion = "30" AND aplicarVisionamos.origen = "90" THEN
                vComision = 464.

            /* Revisamos si hay algún valor para debitar, para seguir con el procedimiento, o de lo contrario continuar con la siguiente transacción */
            IF vMonto + vComision > 0 THEN DO:
                /* RE - Retiro en efectivo */
                IF (aplicarVisionamos.grupo_transaccional = "0200" OR aplicarVisionamos.grupo_transaccional = "0220") AND
                   aplicarVisionamos.transaccion = "01" AND
                   aplicarVisionamos.origen = "06" AND
                   aplicarVisionamos.canal = "00" AND
                   aplicarVisionamos.tipo_terminal = "10" AND
                   INTEGER(aplicarVisionamos.tipoCuenta1_origen) = 10 /*AND flag = TRUE*/ THEN DO:
                    RUN retiroEnEfectivo.
                END.
                ELSE DO:
                    /* 2. Realizamos la transacción */
                    CASE aplicarVisionamos.grupo_transaccional:
                        /* aplicarVisionamos.transaccion
                        00 - Compra en Redes
                        01 - Retiro en efectivo
                        02 - Ajuste débito
                        20 - Anulación de compra en redes
                        21 - Consignación en efectivo
                        22 - Ajuste crédito
                        30 - Consulta de Saldo
                        35 - Consulta de movimientos
                        40 - Transferencia
                        89 - Consulta Costo de transacción
                        90 - Cambio de Clave (PIN) */
                    
                        WHEN "0200" OR
                        WHEN "0220" THEN DO:
                            vRev = "".
                    
                            IF aplicarVisionamos.transaccion = "20" OR
                               aplicarVisionamos.transaccion = "21" OR
                               aplicarVisionamos.transaccion = "22" OR
                               (aplicarVisionamos.transaccion = "40" AND aplicarVisionamos.entidadDuenaCuentaOrigen = "00000018") THEN DO:
                                RUN consignacion.
                            END.
                    
                            IF aplicarVisionamos.transaccion = "00" OR
                               aplicarVisionamos.transaccion = "01" OR
                               aplicarVisionamos.transaccion = "02" OR
                               aplicarVisionamos.transaccion = "30" OR
                               aplicarVisionamos.transaccion = "35" OR
                               aplicarVisionamos.transaccion = "90" OR
                               (aplicarVisionamos.transaccion = "40" AND aplicarVisionamos.entidadDuenaCuentaDestino = "00000018") THEN DO:
                                RUN retiroAvance.
                            END.
                        END.
                    
                        WHEN "0400" THEN DO:
                            vRev = "Rev-".
                            IF aplicarVisionamos.transaccion = "20" OR
                               aplicarVisionamos.transaccion = "21" OR
                               aplicarVisionamos.transaccion = "22" OR
                               (aplicarVisionamos.transaccion = "40" AND aplicarVisionamos.entidadDuenaCuentaOrigen = "00000018") THEN DO:
                                RUN retiroAvance.
                            END.
                    
                            IF aplicarVisionamos.transaccion = "00" OR
                               aplicarVisionamos.transaccion = "01" OR
                               aplicarVisionamos.transaccion = "02" OR
                               aplicarVisionamos.transaccion = "30" OR
                               aplicarVisionamos.transaccion = "35" OR
                               aplicarVisionamos.transaccion = "90" OR
                               (aplicarVisionamos.transaccion = "40" AND aplicarVisionamos.entidadDuenaCuentaDestino = "00000018") THEN DO:
                                RUN consignacion.
                            END.
                        END.
                    END.
                END.
            END.
            ELSE
                aplicarVisionamos.estado = 2.
        END.
    END.
END.

PROCEDURE consignacion:
    DEFINE VARIABLE vdb AS DECIMAL.
    DEFINE VARIABLE vcr AS DECIMAL.
    DEFINE VAR P_Poliza AS DECIMAL.
    DEFINE VAR P_Honora AS DECIMAL.
    DEFINE VAR P_Costas AS DECIMAL.
    DEFINE VAR P_SeguroVida AS DECIMAL.
    DEFINE VAR P_SeguroDeudor AS DECIMAL.
    DEFINE VAR P_IMorDifC AS DECIMAL.
    DEFINE VAR P_IMora AS DECIMAL.
    DEFINE VAR P_IDifCob AS DECIMAL.
    DEFINE VAR P_ICte AS DECIMAL.
    DEFINE VAR P_IAntic AS DECIMAL.
    DEFINE VAR P_Capit AS DECIMAL.
    DEFINE VAR P_VlrNoDist AS DECIMAL.
    DEFINE VAR P_IAC AS DECIMAL.
    
    IF INTEGER(aplicarVisionamos.tipoCuenta1_origen) = 10 THEN DO:
        FIND FIRST ahorros WHERE ahorros.nit = aplicarVisionamos.doc_cliente
                             AND ahorros.cod_ahorro = 4
                             AND INTEGER(ahorros.cue_ahorros) = INTEGER(aplicarVisionamos.cuenta1_origen) NO-ERROR.
        IF AVAILABLE(ahorros) THEN DO:
            FIND FIRST comprobante WHERE comprobante.comprobante = 22 NO-ERROR.
            IF AVAILABLE(comprobante) THEN
                ASSIGN pNumDocumento = comprobante.secuencia + 1
                       comprobantes.secuencia = comprobante.secuencia + 1.

            ASSIGN vcr = vMonto - vComision
                   vdb = 0.

            CREATE mov_ahorro.
            RUN movAhorro.
            
            IF INTEGER(aplicarVisionamos.causal_transaccion) = 4 THEN DO: /* Cheque */
                ASSIGN mov_ahorros.val_cheque = vcr
                       mov_ahorros.cod_operacion = 010101002
                       ahorros.sdo_disponible = ahorros.sdo_canje + vcr
                       aplicarVisionamos.estado = 2.

                CREATE che_transito.
                ASSIGN Che_Transito.Agencia = ahorros.agencia
                       Che_Transito.Cheque = aplicarVisionamos.chequeNumero
                       Che_Transito.Cod_Compensa = INTEGER(aplicarVisionamos.chequeCodigoBanco)
                       Che_Transito.Cod_Producto = ahorros.cod_ahorro
                       Che_Transito.Fec_Canje = TODAY
                       Che_Transito.Num_Cuenta = ahorros.cue_ahorros
                       Che_Transito.Tip_Producto = '1'
                       che_transito.estado = 1
                       Che_Transito.Valor = vcr.
            END. 
            ELSE /* Efectivo */
                ASSIGN mov_ahorros.val_efectivo = vcr
                       mov_ahorros.cod_operacion = 010101001
                       ahorros.sdo_disponible = ahorros.sdo_disponible + vcr
                       aplicarVisionamos.estado = 2.

            mov_ahorros.sdo_disponible = ahorros.sdo_disponible.

            CREATE mov_contable.
            RUN movContable.

            Mov_Contable.Agencia = ahorros.agencia.
            Mov_Contable.Destino = ahorros.agencia.
            Mov_Contable.Cuenta = "21050501".
            Mov_Contable.Nit = ahorros.nit.
            Mov_contable.db = vdb.
            mov_contable.cr = vcr.

            CREATE mov_contable.
            RUN movContable.

            Mov_Contable.Agencia = ahorros.agencia.
            Mov_Contable.Destino = ahorros.agencia.
            Mov_Contable.Cuenta = "24459550".
            Mov_Contable.Nit = "800112808".
            Mov_contable.db = vcr.
            mov_contable.cr = vdb.

            /* Para Sucursales y Agencias */
            IF ahorros.agencia <> 1 THEN DO:
                RUN cuentaSucursales&agencias.

                mov_contable.cuenta = cuentaSyA.
                mov_contable.nit = "001".

                FIND FIRST comprobante WHERE comprobante.comprobante = 22
                                         AND comprobantes.agencia = 1 NO-ERROR.
                IF AVAILABLE(comprobante) THEN
                    ASSIGN pNumDocumento = comprobante.secuencia + 1
                           comprobantes.secuencia = comprobante.secuencia + 1.

                CREATE mov_contable.
                RUN movContable.

                ASSIGN Mov_Contable.Agencia = 1
                       Mov_Contable.Destino = 1
                       Mov_Contable.Cuenta = cuentaSyA
                       Mov_Contable.Nit = STRING(ahorros.agencia,"999")
                       Mov_contable.db = vdb
                       mov_contable.cr = vcr.

                CREATE mov_contable.
                RUN movContable.

                ASSIGN Mov_Contable.Agencia = 1
                       Mov_Contable.Destino = 1
                       Mov_Contable.Cuenta = "24459550"
                       Mov_Contable.Nit = "800112808"
                       Mov_contable.db = vcr
                       mov_contable.cr = vdb.
            END.

            RELEASE ahorros.
        END.
    END.

    IF INTEGER(aplicarVisionamos.tipoCuenta1_origen) = 50 THEN DO: /* Consignación a Cupos Rotativos */
        FIND FIRST creditos WHERE creditos.nit = aplicarVisionamos.doc_cliente
                              AND creditos.cod_credito = 123
                              AND creditos.num_credito = INTEGER(aplicarVisionamos.cuenta1_origen) NO-ERROR.
        IF AVAILABLE(creditos) THEN DO:
            FIND FIRST comprobante WHERE comprobante.comprobante = 22 NO-ERROR.
            IF AVAILABLE(comprobante) THEN
                ASSIGN pNumDocumento = comprobante.secuencia + 1
                       comprobantes.secuencia = comprobante.secuencia + 1.

            RELEASE comprobante.

            ASSIGN vcr = vMonto - vComision
                   vdb = 0.

            IF aplicarVisionamos.grupo_Transaccional <> "0400" THEN DO:
                RUN abocredito3.r(INPUT YES,
                                  INPUT Creditos.Agencia,
                                  INPUT Creditos.Cod_Credito,
                                  INPUT Creditos.Nit,
                                  INPUT Creditos.Num_Credito,
                                  INPUT vcr,
                                  INPUT Comprobantes.Comprobante,
                                  INPUT pNumDocumento,
                                  INPUT 0,
                                  INPUT 1,
                                  INPUT 0 /*btcontrol_pagos.Nro_Cuota*/,
                                  INPUT 1,
                                  INPUT YES /*P_Benefi*/,
                                  OUTPUT P_Poliza,
                                  OUTPUT P_Honora,
                                  OUTPUT P_Costas,
                                  OUTPUT P_SeguroVida,
                                  OUTPUT P_SeguroDeudor,
                                  OUTPUT P_IMorDifC,
                                  OUTPUT P_IMora,
                                  OUTPUT P_IDifCob,
                                  OUTPUT P_ICte,
                                  OUTPUT P_IAntic,
                                  OUTPUT P_Capit,
                                  OUTPUT P_VlrNoDist,
                                  OUTPUT P_IAC).

                aplicarVisionamos.estado = 2.
            END.
            ELSE DO:
                ASSIGN vcr = vMonto + vComision
                       vdb = 0.

                creditos.sdo_capital = creditos.sdo_capital - vcr.
                aplicarVisionamos.estado = 2.

                IF vMonto > 0 THEN DO:
                    CREATE Mov_Creditos.
                    RUN movCreditos.

                    ASSIGN Mov_Creditos.Cod_Operacion = 020101001
                           Mov_Creditos.Num_Documento = STRING(pNumDocumento)
                           Mov_Creditos.Val_Efectivo = vMonto
                           Mov_Creditos.Descrip = vRev + aplicarVisionamos.TERMINAL_ubicacion.
                END.

                IF vComision> 0 THEN DO:
                    CREATE Mov_Creditos.
                    RUN movCreditos.

                    ASSIGN Mov_Creditos.Cod_Operacion = 020101001
                           Mov_Creditos.Num_Documento = STRING(pNumDocumento)
                           Mov_Creditos.Val_Efectivo = vComision
                           Mov_Creditos.Descrip = vRev + aplicarVisionamos.TERMINAL_ubicacion.
                END.

                CREATE mov_contable.
                RUN movContable.

                Mov_Contable.Agencia = Creditos.agencia.
                Mov_Contable.Destino = Creditos.agencia.
                Mov_Contable.Cuenta = "14420505".
                Mov_Contable.Nit = creditos.nit.
                Mov_contable.db = vdb.
                mov_contable.cr = vcr.
            END.

            CREATE mov_contable.
            RUN movContable.

            Mov_Contable.Agencia = Creditos.agencia.
            Mov_Contable.Destino = Creditos.agencia.
            Mov_Contable.Cuenta = "24459550".
            Mov_Contable.Nit = "800112808".
            Mov_contable.db = vcr.
            mov_contable.cr = vdb.

            /* Para Sucursales y Agencias */
            IF creditos.agencia <> 1 THEN DO:
                RUN cuentaSucursales&agencias.

                mov_contable.cuenta = cuentaSyA.
                mov_contable.nit = "001".

                FIND FIRST comprobante WHERE comprobante.comprobante = 22
                                         AND comprobantes.agencia = 1 NO-ERROR.
                IF AVAILABLE(comprobante) THEN
                    ASSIGN pNumDocumento = comprobante.secuencia + 1
                           comprobantes.secuencia = comprobante.secuencia + 1.

                RELEASE comprobante.

                CREATE mov_contable.
                RUN movContable.

                ASSIGN Mov_Contable.Agencia = 1
                       Mov_Contable.Destino = 1
                       Mov_Contable.Cuenta = cuentaSyA
                       Mov_Contable.Nit = STRING(creditos.agencia,"999")
                       Mov_contable.db = vdb
                       mov_contable.cr = vcr.

                CREATE mov_contable.
                RUN movContable.

                ASSIGN Mov_Contable.Agencia = 1
                       Mov_Contable.Destino = 1
                       Mov_Contable.Cuenta = "24459550"
                       Mov_Contable.Nit = "800112808"
                       Mov_contable.db = vcr
                       mov_contable.cr = vdb.
            END.

            RELEASE creditos.
        END.
    END.
END PROCEDURE.

PROCEDURE retiroAvance:
    DEFINE VARIABLE vdb AS DECIMAL.
    DEFINE VARIABLE vcr AS DECIMAL.
    DEFINE VAR tasaCredito AS DECIMAL.
    DEFINE VAR gmfAplicado AS DECIMAL.

    IF INTEGER(aplicarVisionamos.tipoCuenta1_origen) = 10 THEN DO:
        FIND FIRST ahorros WHERE ahorros.nit = aplicarVisionamos.doc_cliente
                             AND ahorros.cod_ahorro = 4
                             AND INTEGER(ahorros.cue_ahorros) = INTEGER(aplicarVisionamos.cuenta1_origen) NO-ERROR.
        IF AVAILABLE(ahorros) THEN DO:
            FIND FIRST comprobante WHERE comprobante.comprobante = 22 NO-ERROR.
            IF AVAILABLE(comprobante) THEN
                ASSIGN pNumDocumento = comprobante.secuencia + 1
                       comprobantes.secuencia = comprobante.secuencia + 1.

            ASSIGN vcr = 0
                   vdb = vMonto + vComision.

            IF vMonto > 0 THEN DO:
                CREATE mov_ahorro.
                RUN movAhorro.

                ASSIGN mov_ahorros.val_efectivo = vMonto
                       mov_ahorros.cod_operacion = 010102001
                       ahorros.sdo_disponible = ahorros.sdo_disponible - vMonto.

                mov_ahorros.sdo_disponible = ahorros.sdo_disponible.
            END.

            IF vComision > 0 THEN DO:
                CREATE mov_ahorro.
                RUN movAhorro.

                ASSIGN mov_ahorros.val_efectivo = vComision
                       mov_ahorros.cod_operacion = 010102001
                       mov_ahorros.descrip = "Comisión Tx"
                       ahorros.sdo_disponible = ahorros.sdo_disponible - vComision.

                mov_ahorros.sdo_disponible = ahorros.sdo_disponible.
            END.

            aplicarVisionamos.estado = 2.

            CREATE mov_contable.
            RUN movContable.

            Mov_Contable.Agencia = ahorros.agencia.
            Mov_Contable.Destino = ahorros.agencia.
            Mov_Contable.Cuenta = "21050501".
            Mov_Contable.Nit = ahorros.nit.
            Mov_contable.db = vdb.
            mov_contable.cr = vcr.

            CREATE mov_contable.
            RUN movContable.

            Mov_Contable.Agencia = ahorros.agencia.
            Mov_Contable.Destino = ahorros.agencia.
            Mov_Contable.Cuenta = "24459550".
            Mov_Contable.Nit = "800112808".
            Mov_contable.db = vcr.
            mov_contable.cr = vdb.

            IF aplicarVisionamos.entidadDuenaTerminal = "00000018" AND aplicarVisionamos.entidadDuenaCuentaOrigen = "00000018" THEN DO:
                ASSIGN mov_contable.cuenta = "11050501"
                       mov_contable.nit = ahorros.nit.

                FIND FIRST usuarios WHERE usuarios.usuario = aplicarVisionamos.usuario NO-LOCK NO-ERROR.
                IF AVAILABLE usuarios THEN
                    mov_contable.agencia = usuarios.agencia.
            END.

            RUN RutGMF.R (INPUT TRUE,
                          INPUT ahorros.agencia,
                          INPUT Ahorros.Agencia,
                          INPUT 1,
                          INPUT Ahorros.Cod_Ahorro,
                          INPUT Ahorros.Nit,
                          INPUT Ahorros.Cue_Ahorros,
                          INPUT 010102001,
                          INPUT vdb,
                          INPUT 22,
                          INPUT STRING(pNumDocumento),
                          INPUT "RetiroTarjeta",
                          INPUT 0,
                          INPUT 0,
                          OUTPUT gmfAplicado).

            /* Para Sucursales y Agencias */
            IF ahorros.agencia <> 1 AND mov_contable.cuenta <> "11050501" THEN DO:
                RUN cuentaSucursales&agencias.

                mov_contable.cuenta = cuentaSyA.
                mov_contable.nit = "001".

                FIND FIRST comprobante WHERE comprobante.comprobante = 22
                                         AND comprobantes.agencia = 1 NO-ERROR.
                IF AVAILABLE(comprobante) THEN
                    ASSIGN pNumDocumento = comprobante.secuencia + 1
                           comprobantes.secuencia = comprobante.secuencia + 1.

                CREATE mov_contable.
                RUN movContable.

                ASSIGN Mov_Contable.Agencia = 1
                       Mov_Contable.Destino = 1
                       Mov_Contable.Cuenta = cuentaSyA
                       Mov_Contable.Nit = STRING(ahorros.agencia,"999")
                       Mov_contable.db = vdb
                       mov_contable.cr = vcr.

                CREATE mov_contable.
                RUN movContable.

                ASSIGN Mov_Contable.Agencia = 1
                       Mov_Contable.Destino = 1
                       Mov_Contable.Cuenta = "24459550"
                       Mov_Contable.Nit = "800112808"
                       Mov_contable.db = vcr
                       mov_contable.cr = vdb.
            END.

            RELEASE ahorros.
        END.
    END.

    IF INTEGER(aplicarVisionamos.tipoCuenta1_origen) = 50 THEN DO: /* Retiro de Ahorros o Avance de Cupos Rotativos */
        FIND FIRST creditos WHERE creditos.nit = aplicarVisionamos.doc_cliente
                              AND creditos.cod_credito = 123
                              AND creditos.num_credito = INTEGER(aplicarVisionamos.cuenta1_origen) NO-ERROR.
        IF AVAILABLE(creditos) THEN DO:
            FIND FIRST comprobante WHERE comprobante.comprobante = 22 NO-ERROR.
            IF AVAILABLE(comprobante) THEN
                ASSIGN pNumDocumento = comprobante.secuencia + 1
                       comprobantes.secuencia = comprobante.secuencia + 1.

            RELEASE comprobante.

            ASSIGN vdb = vMonto + vComision
                   vcr = 0.

            creditos.Sdo_capital = Creditos.Sdo_capital + vdb.
            aplicarVisionamos.estado = 2.

            IF vMonto > 0 THEN DO:
                CREATE Mov_Creditos.
                RUN movCreditos.

                ASSIGN Mov_Creditos.Cod_Operacion = 020102001
                       Mov_Creditos.Num_Documento = STRING(pNumDocumento)
                       Mov_Creditos.Val_Efectivo = vMonto
                       Mov_Creditos.Descrip = vRev + aplicarVisionamos.TERMINAL_ubicacion.
            END.

            IF vComision > 0 THEN DO:
                CREATE Mov_Creditos.
                RUN movCreditos.

                ASSIGN Mov_Creditos.Cod_Operacion = 020102001
                       Mov_Creditos.Num_Documento = STRING(pNumDocumento)
                       Mov_Creditos.Val_Efectivo = vComision
                       Mov_Creditos.Descrip = vRev + aplicarVisionamos.TERMINAL_ubicacion.
            END.


            FIND FIRST pro_creditos WHERE pro_credito.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.
            IF AVAILABLE(pro_creditos) THEN
                FIND FIRST indicadores WHERE Indicadores.indicador = pro_creditos.cod_tasa  NO-LOCK NO-ERROR.

            IF AVAILABLE(indicadores) THEN DO:
                tasaCredito = (((EXP((indicadores.tasa / 100) + 1,1 / 12)) - 1) * 100) * 12.

                IF STRING(tasaCredito,">>9.99") <> STRING(creditos.tasa,">>9.99") THEN DO:
                    CREATE Mov_Creditos.
                    RUN movCreditos.

                    ASSIGN Mov_Creditos.Cod_Operacion = 999999999
                           Mov_Creditos.Descrip = "Cambio de Tasa " + STRING(creditos.tasa,">>9.99") + "-->" + STRING(tasaCredito,">>9.99").

                    creditos.tasa = tasaCredito.
                END.
            END.

            CREATE mov_contable.
            RUN movContable.

            Mov_Contable.Agencia = Creditos.agencia.
            Mov_Contable.Destino = Creditos.agencia.
            Mov_Contable.Cuenta = "14420505".
            Mov_Contable.Nit = creditos.nit.
            Mov_contable.db = vdb.
            mov_contable.cr = vcr.

            CREATE mov_contable.
            RUN movContable.

            Mov_Contable.Agencia = Creditos.agencia.
            Mov_Contable.Destino = Creditos.agencia.
            Mov_Contable.Cuenta = "24459550".
            Mov_Contable.Nit = "800112808".
            Mov_contable.db = vcr.
            mov_contable.cr = vdb.

            IF aplicarVisionamos.entidadDuenaTerminal = "00000018" AND aplicarVisionamos.entidadDuenaCuentaOrigen = "00000018" THEN DO:
                ASSIGN mov_contable.cuenta = "11050501"
                       mov_contable.nit = creditos.nit.

                FIND FIRST usuarios WHERE usuarios.usuario = aplicarVisionamos.usuario NO-LOCK NO-ERROR.
                IF AVAILABLE usuarios THEN
                    mov_contable.agencia = usuarios.agencia.
            END.

            /* Para Sucursales y Agencias */
            IF creditos.agencia <> 1 AND mov_contable.cuenta <> "11050501" THEN DO:
                RUN cuentaSucursales&agencias.

                mov_contable.cuenta = cuentaSyA.
                mov_contable.nit = "001".

                FIND FIRST comprobante WHERE comprobante.comprobante = 22
                                         AND comprobantes.agencia = 1 NO-ERROR.
                IF AVAILABLE(comprobante) THEN
                    ASSIGN pNumDocumento = comprobante.secuencia + 1
                           comprobantes.secuencia = comprobante.secuencia + 1.

                RELEASE comprobante.

                CREATE mov_contable.
                RUN movContable.

                ASSIGN Mov_Contable.Agencia = 1
                       Mov_Contable.Destino = 1
                       Mov_Contable.Cuenta = cuentaSyA
                       Mov_Contable.Nit = STRING(creditos.agencia,"999")
                       Mov_contable.db = vdb
                       mov_contable.cr = vcr.

                CREATE mov_contable.
                RUN movContable.

                ASSIGN Mov_Contable.Agencia = 1
                       Mov_Contable.Destino = 1
                       Mov_Contable.Cuenta = "24459550"
                       Mov_Contable.Nit = "800112808"
                       Mov_contable.db = vcr
                       mov_contable.cr = vdb.
            END.

            RELEASE creditos.
        END.
    END.
END PROCEDURE.

PROCEDURE movContable:
    ASSIGN mov_contable.cen_costos = 999
           Mov_Contable.Comprobante = 22
           Mov_Contable.Num_Documento = pNumDocumento
           Mov_contable.Doc_referencia = aplicarVisionamos.cuenta1_origen
           Mov_Contable.Fec_Contable = TODAY
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Comentario = vRev + aplicarVisionamos.terminal_ubicacion
           Mov_Contable.Usuario = "TD"
           Mov_Contable.Estacion = "005".

END PROCEDURE.

PROCEDURE cuentaSucursales&Agencias:
    /* Capturamos la cuenta de Sucursales y Agencias */
    IF INTEGER(aplicarVisionamos.tipoCuenta1_origen) = 10 THEN DO:
        FIND FIRST pro_ahorros WHERE pro_ahorros.cod_ahorro = 4 NO-LOCK NO-ERROR.
        IF AVAILABLE pro_ahorros THEN DO:
            FIND FIRST cortoLargo WHERE cortoLargo.agencia = ahorros.agencia
                                    AND cortoLargo.clase = 1
                                    AND cortoLargo.cod_producto = pro_ahorros.cod_ahorro NO-LOCK NO-ERROR.
            IF AVAILABLE cortoLargo THEN DO:
                cuentaAhorro = cortoLargo.Cta_AsoAd.

                FIND FIRST liqui_int WHERE liqui_int.clase = 1
                                       AND liqui_int.cod_producto = 4 NO-LOCK NO-ERROR.
                IF AVAILABLE liqui_int THEN
                    cuentaSyA = liqui_Int.Cta_SucYAge.
            END.
        END.
    END.

    IF INTEGER(aplicarVisionamos.tipoCuenta1_origen) = 50 THEN DO:
        FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = 123 NO-LOCK NO-ERROR.
        IF AVAILABLE pro_creditos THEN DO:
            FIND FIRST cortoLargo WHERE cortoLargo.agencia = creditos.agencia
                                    AND cortoLargo.clase = 2
                                    AND cortoLargo.cod_producto = pro_creditos.cod_credito NO-LOCK NO-ERROR.
            IF AVAILABLE cortoLargo THEN DO:
                cuentaRotativo = cortoLargo.Cta_AsoAd.

                FIND FIRST liqui_int WHERE liqui_int.clase = 2
                                       AND liqui_int.cod_producto = 123 NO-LOCK NO-ERROR.
                IF AVAILABLE liqui_int THEN
                    cuentaSyA = liqui_Int.Cta_SucYAge.
            END.
        END.
    END.
END PROCEDURE.

PROCEDURE movAhorro:
    IF aplicarVisionamos.fecha <> tt.s001 THEN
        vRev = vRev + aplicarVisionamos.fecha + "-".

    ASSIGN mov_ahorros.Agencia = ahorros.agencia
           mov_ahorros.Cod_Ahorro = ahorros.cod_ahorro
           mov_ahorros.cpte = comprobantes.comprobante
           mov_ahorros.Cue_Ahorros = ahorros.cue_ahorros
           mov_ahorros.descrip = vRev + aplicarVisionamos.terminal_ubicacion
           mov_ahorros.Fecha = TODAY
           mov_ahorros.Hora = TIME
           mov_ahorros.nit = ahorros.nit
           mov_ahorros.Nro_Auditoria = aplicarVisionamos.secuencia
           mov_ahorros.Num_Documento = STRING(pNumDocumento)
           mov_ahorros.Usuario = "TD".

END PROCEDURE.

PROCEDURE movCreditos:
    ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
           Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
           Mov_Creditos.Nit = Creditos.Nit
           Mov_Creditos.Num_Credito = Creditos.Num_Credito
           Mov_Creditos.Ofi_Destino = Creditos.Agencia
           Mov_Creditos.Ofi_Fuente = 1
           Mov_Creditos.Pagare = Creditos.Pagare
           Mov_Creditos.Fecha = TODAY
           Mov_Creditos.Hora = TIME
           Mov_Creditos.Usuario = "TD"
           Mov_Creditos.Int_Corriente = Creditos.Int_Corriente
           Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
           Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
           Mov_Creditos.Cpte = 22.
END.

PROCEDURE conexion:
    Create "ADODB.Connection" ObjConnection.
    Create "ADODB.RecordSet" ObjRecordSet.
    Create "ADODB.Command" ObjCommand.

    /* Remoto */
    /*ASSIGN ODBC-DSN = "visionamos" /* The ODBC DSN */
           ODBC-SERVER = "192.168.1.100\SQLEXPRESS,1433" /* The name of the server hosting the SQL DB and DSN */
           ODBC-USERID = "visionamos" /* The user id for access to the SQL Database */
           ODBC-PASSWD = "visionamos". /* Password required by above user-id */*/

    /* Local */
    Assign ODBC-DSN = "visionamos" /* The ODBC DSN */
           ODBC-SERVER = "localhost" /* The name of the server hosting the SQL DB and DSN */
           ODBC-USERID = "visionamos" /* The user id for access to the SQL Database */
           ODBC-PASSWD = "visionamos". /* Password required by above user-id */

    /* Open up the connection to the ODBC Layer */
    ObjConnection:Open ( "data source=" + ODBC-DSN + ";server=" + ODBC-SERVER, ODBC-USERID, ODBC-PASSWD, 0). /* SQL */

    If (error-status:num-messages > 0) THEN
        ODBC-STATUS = "Error: Could not establish connection.".
    Else DO:
        Assign ObjCommand:ActiveConnection = ObjConnection
               ObjCommand:CommandText = ODBC-QUERY
               ObjCommand:CommandType = 1 /* adCmdText */
               ObjConnection:CursorLocation = 3 /* adUseClient */
               ObjRecordSet:CursorType = 3 /* adOpenStatic */
               ObjRecordSet = ObjCommand:Execute (output ODBC-NULL,"", 32).
    END.

END PROCEDURE.

PROCEDURE desconexion:
    /* Close the ADO connection */
    ObjConnection:Close no-error.

    /* Don't forget to release the memory!! */
    Release object ObjConnection no-error.
    Release object ObjCommand no-error.
    Release object ObjRecordSet no-error.

    Assign ObjConnection = ?
           ObjCommand = ?
           ObjRecordSet = ?.

END PROCEDURE.


PROCEDURE retiroEnEfectivo:
    DEFINE VAR vValor AS DECIMAL.
    DEFINE VAR gmfAplicado AS DECIMAL.
    DEFINE VAR cuentaDB AS CHARACTER.
    DEFINE VAR cuentaCR AS CHARACTER.
    DEFINE VAR agenciaDB AS INTEGER.
    DEFINE VAR agenciaCR AS INTEGER.

    cuentaDB = "21050501".
    cuentaCR = "11050501".

    FIND FIRST usuarios WHERE usuarios.usuario = aplicarVisionamos.usuario NO-LOCK NO-ERROR.

    FIND FIRST ahorros WHERE ahorros.nit = aplicarVisionamos.doc_cliente
                         AND ahorros.cod_ahorro = 4
                         AND INTEGER(ahorros.cue_ahorros) = INTEGER(aplicarVisionamos.cuenta1_origen) NO-ERROR.
    IF AVAILABLE(ahorros) THEN DO:
        FIND FIRST comprobante WHERE comprobante.comprobante = 22 NO-ERROR.
        IF AVAILABLE(comprobante) THEN
            ASSIGN pNumDocumento = comprobante.secuencia + 1
                   comprobantes.secuencia = comprobante.secuencia + 1.

        ASSIGN vValor = vMonto + vComision.

        IF vMonto > 0 THEN DO:
            CREATE mov_ahorro.
            RUN movAhorro.

            ASSIGN mov_ahorros.val_efectivo = vMonto
                   mov_ahorros.cod_operacion = 010102001
                   ahorros.sdo_disponible = ahorros.sdo_disponible - vMonto.

            mov_ahorros.sdo_disponible = ahorros.sdo_disponible.
        END.

        IF vComision > 0 THEN DO:
            CREATE mov_ahorro.
            RUN movAhorro.

            ASSIGN mov_ahorros.val_efectivo = vComision
                   mov_ahorros.cod_operacion = 010102001
                   mov_ahorros.descrip = "Comisión Tx"
                   ahorros.sdo_disponible = ahorros.sdo_disponible - vComision.

            mov_ahorros.sdo_disponible = ahorros.sdo_disponible.
        END.

        CREATE mov_contable.
        RUN movContable.

        Mov_Contable.Agencia = ahorros.agencia.
        Mov_Contable.Destino = usuarios.agencia.
        Mov_Contable.Cuenta = cuentaDB.
        Mov_Contable.Nit = ahorros.nit.
        Mov_contable.db = vValor.
        
        RUN RutGMF.R (INPUT TRUE,
                      INPUT ahorros.agencia,
                      INPUT Ahorros.Agencia,
                      INPUT 1,
                      INPUT Ahorros.Cod_Ahorro,
                      INPUT Ahorros.Nit,
                      INPUT Ahorros.Cue_Ahorros,
                      INPUT 010102001,
                      INPUT vValor,
                      INPUT 22,
                      INPUT STRING(pNumDocumento),
                      INPUT "RetiroTarjeta",
                      INPUT 0,
                      INPUT 0,
                      OUTPUT gmfAplicado).

        /* Para Sucursales y Agencias */
        IF ahorros.agencia <> usuarios.agencia THEN DO:
            RUN cuentaSucursales&agencias.

            CREATE mov_contable.
            RUN movContable.

            Mov_Contable.Agencia = ahorros.agencia.
            Mov_Contable.Destino = usuarios.agencia.
            Mov_Contable.Cuenta = cuentaSyA.
            Mov_Contable.Nit = STRING(usuarios.agencia,"999").
            Mov_contable.cr = vValor.

            FIND FIRST comprobante WHERE comprobante.comprobante = 22
                                     AND comprobantes.agencia = usuarios.agencia NO-ERROR.
            IF AVAILABLE(comprobante) THEN
                ASSIGN pNumDocumento = comprobante.secuencia + 1
                       comprobantes.secuencia = comprobante.secuencia + 1.

            CREATE mov_contable.
            RUN movContable.

            ASSIGN Mov_Contable.Agencia = usuarios.agencia
                   Mov_Contable.Destino = ahorros.agencia
                   Mov_Contable.Cuenta = cuentaSyA
                   Mov_Contable.Nit = STRING(ahorros.agencia,"999")
                   Mov_contable.db = vValor.
        END.

        CREATE mov_contable.
        RUN movContable.

        Mov_Contable.Agencia = usuarios.agencia.
        Mov_Contable.Destino = ahorros.agencia.
        Mov_Contable.Cuenta = cuentaCR.
        Mov_Contable.Nit = ahorros.nit.
        mov_contable.cr = vValor.

        RELEASE ahorros.

        aplicarVisionamos.estado = 2.
    END.

END PROCEDURE.
