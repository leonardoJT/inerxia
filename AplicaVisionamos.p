Def var ObjRecordSet as com-handle no-undo.
Def var ObjConnection as com-handle no-undo.
Def var ObjCommand as com-handle no-undo.
Def var ODBC-DSN as character no-undo.
Def var ODBC-SERVER as character no-undo.
Def var ODBC-USERID as character no-undo.
Def var ODBC-PASSWD as character no-undo.
Def var ODBC-QUERY as character no-undo.
Def var ODBC-STATUS as character no-undo.
Def var ODBC-RECCOUNT as integer no-undo.
Def var ODBC-NULL as character no-undo.
Def var ODBC-CURSOR as integer no-undo.
DEFINE VAR ODBC-DRV AS CHARACTER NO-UNDO.

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
    FIELD s03a AS CHARACTER /* Tipo de Cuenta 1 / Origen */
    FIELD s03b AS CHARACTER /* Tipo de Cuenta 2 / Destino */
    FIELD s03x AS CHARACTER /* Usuario / Caja */
    FIELD s050 AS INTEGER /* Código de error */
    FIELD s056 AS INTEGER /* Valor comisión (aplicada) */
    FIELD s201 AS CHARACTER /* Flag Reverso / 0- Automático, 1- Manual */
    FIELD s203 AS CHARACTER /* Inter Entidad / 0- No, 1- Si */
    FIELD s204 AS CHARACTER /* Transacción gratis / 0- No, 1- Si */
    FIELD s204r AS CHARACTER /* Transacción gratis (Externa) / 0- No, 1- Si */
    FIELD sx03 AS CHARACTER /* Canal a Activar/Bloquear */
    FIELD sx04 AS CHARACTER /* Códigos de bloqueos */
    FIELD sx07 AS CHARACTER /* Tipo de tarjeta / 0- Privada, 1- Visa, 2- Master */
    INDEX idx1 indx.

/* Create the connection object for the link to SQL */
Create "ADODB.Connection" ObjConnection.
/* Create a recordset object ready to return the data */
Create "ADODB.RecordSet" ObjRecordSet.
/* Create a command object for sending the SQL statement */
Create "ADODB.Command" ObjCommand.

    Assign ODBC-DRV = "SQL Server Native Client 10.0"
           ODBC-DSN = "visionamos" /* The ODBC DSN */
           ODBC-SERVER = "localhost" /* The name of the server hosting the SQL DB and DSN */
           ODBC-USERID = "visionamos" /* The user id for access to the SQL Database */
           ODBC-PASSWD = "visionamos". /* Password required by above user-id */

ODBC-QUERY = "SELECT * FROM dbo.trninp".

/* Open up the connecti.on to the ODBC Layer */
/*ObjConnection:Open ("data source=" + ODBC-DSN + ";server=" + ODBC-SERVER, ODBC-USERID, ODBC-PASSWD, 0) no-error.*/ /* Progress */
ObjConnection:Open ("data source=" + ODBC-DSN + ";server=" + ODBC-SERVER, ODBC-USERID, ODBC-PASSWD, 0). /* SQL */

/* Check for connection errors */
If (error-status:num-messages > 0) THEN DO:
    ODBC-STATUS = "Error: Could not establish connection.".
    MESSAGE 'Paila!' ODBC-STATUS
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
Else DO:
    Assign ObjCommand:ActiveConnection = ObjConnection
           ObjCommand:CommandText = ODBC-QUERY
           ObjCommand:CommandType = 1 /* adCmdText */
           ObjConnection:CursorLocation = 3 /* adUseClient */
           ObjRecordSet:CursorType = 3 /* adOpenStatic */
           ObjRecordSet = ObjCommand:Execute ( output ODBC-NULL, "", 32 )
           ODBC-RECCOUNT = ObjRecordSet:RecordCount.

    /* Have we returned any rows ? */
    If ( ODBC-RECCOUNT > 0 ) and not ( ODBC-RECCOUNT = ? ) then Do:
        ObjRecordSet:MoveFirst no-error.

        Do while ODBC-CURSOR < ODBC-RECCOUNT:
            /* Display the data from the query (or create a Progress temp-table for future use) */
            /* Display ObjRecordSet:Fields ("name"):Value format "x(20)". */
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
                   tt.s050 = ObjRecordSet:FIELDS("s050"):VALUE
                   tt.s056 = ObjRecordSet:FIELDS("s056"):VALUE
                   tt.s201 = ObjRecordSet:FIELDS("s201"):VALUE
                   tt.s203 = ObjRecordSet:FIELDS("s203"):VALUE
                   tt.s204 = ObjRecordSet:FIELDS("s204"):VALUE
                   tt.s204r = ObjRecordSet:FIELDS("s204r"):VALUE
                   tt.sx03 = ObjRecordSet:FIELDS("sx03"):VALUE
                   tt.sx04 = ObjRecordSet:FIELDS("sx04"):VALUE
                   tt.sx07 = ObjRecordSet:FIELDS("sx07"):VALUE.

            Assign ODBC-CURSOR = ODBC-CURSOR + 1.

            ObjRecordSet:MoveNext no-error.
        End. /* retrieved a single data row */
    End. /* retrieved all data rows */
    Else
        Assign ODBC-STATUS = "No records found.".

    /* Close the ADO connection */
    ObjConnection:Close no-error.
End. /* The connection opened correctly */

/* Don't forget to release the memory!! */
Release object ObjConnection no-error.
Release object ObjCommand no-error.
Release object ObjRecordSet no-error.

Assign ObjConnection = ?
       ObjCommand = ?
       ObjRecordSet = ?.

FOR EACH tt:
    FIND FIRST aplicarVisionamos WHERE aplicarVisionamos.indx = tt.indx NO-LOCK NO-ERROR.
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
               aplicarVisionamos.cod_bloqueo = tt.sx04.
    END.
END.

FOR EACH aplicarVisionamos NO-LOCK:
    /* Retiros */
    RUN AplicarTransaccionesTarjetas.r (INPUT aplicarVisionamos.doc_cliente,
                                        INPUT aplicarVisionamos.cuenta1_origen,
                                        INPUT aplicarVisionamos.num_tarjeta).
END.

