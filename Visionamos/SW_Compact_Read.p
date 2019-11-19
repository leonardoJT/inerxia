DEFINE INPUT PARAMETER NombreArchivoEntrada AS CHARACTER.
DEFINE OUTPUT PARAMETER fecha AS INTEGER.
DEFINE OUTPUT PARAMETER hora AS INTEGER.
DEFINE OUTPUT PARAMETER vTerminal AS INTEGER.
DEFINE OUTPUT PARAMETER grupoTransaccional AS INTEGER.
DEFINE OUTPUT PARAMETER transaccion AS CHARACTER.
DEFINE OUTPUT PARAMETER secuencia AS CHARACTER.
DEFINE OUTPUT PARAMETER origen AS CHARACTER.
DEFINE OUTPUT PARAMETER documento_cliente AS CHARACTER.
DEFINE OUTPUT PARAMETER documento_cliente_destino AS CHARACTER.
DEFINE OUTPUT PARAMETER Cuenta1_origen AS CHARACTER.
DEFINE OUTPUT PARAMETER cuenta2_destino AS CHARACTER.
DEFINE OUTPUT PARAMETER entidad_duena_cuenta_origen AS INTEGER.
DEFINE OUTPUT PARAMETER entidad_duena_cuenta_destino AS INTEGER.
DEFINE OUTPUT PARAMETER entidad_duena_terminal AS INTEGER.
DEFINE OUTPUT PARAMETER cheque_codigo_banco AS INTEGER.
DEFINE OUTPUT PARAMETER cheque_cuenta_banco AS CHARACTER.
DEFINE OUTPUT PARAMETER cheque_numero AS CHARACTER.
DEFINE OUTPUT PARAMETER cheque_total_cheques AS INTEGER.
DEFINE OUTPUT PARAMETER codigo_barras AS CHARACTER.
DEFINE OUTPUT PARAMETER numero_tarjeta AS CHARACTER.
DEFINE OUTPUT PARAMETER fecha_contable AS INTEGER.
DEFINE OUTPUT PARAMETER valor AS DECIMAL.
DEFINE OUTPUT PARAMETER valor_base AS DECIMAL.
DEFINE OUTPUT PARAMETER valor_impuesto AS DECIMAL.
DEFINE OUTPUT PARAMETER valor_retencion AS DECIMAL.
DEFINE OUTPUT PARAMETER valor_propina AS DECIMAL.
DEFINE OUTPUT PARAMETER valor_transaccion AS DECIMAL.
DEFINE OUTPUT PARAMETER cuotas AS INTEGER.
DEFINE OUTPUT PARAMETER numero_factura AS INTEGER.
DEFINE OUTPUT PARAMETER tipo_cuenta1_origen AS CHARACTER.
DEFINE OUTPUT PARAMETER tipo_cuenta2_destino AS CHARACTER.
DEFINE OUTPUT PARAMETER secuencia_reversar AS CHARACTER.
DEFINE OUTPUT PARAMETER usuario_caja AS CHARACTER.
DEFINE OUTPUT PARAMETER ECG_STR0 AS CHARACTER.

/* Variables representing parameter values in the READ-XML( ), READ-XMLSCHEMA( ), WRITE-XML( ), and WRITE-XMLSCHEMA( ) methods. */
DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTargetType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReadMode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOverrideDefaultMapping AS LOGICAL NO-UNDO.
DEFINE VARIABLE cFieldTypeMapping AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVerifySchemaMode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEncoding AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFormatted AS LOGICAL NO-UNDO.
DEFINE VARIABLE lWriteSchema AS LOGICAL NO-UNDO.
DEFINE VARIABLE lMinSchema AS LOGICAL NO-UNDO.
DEFINE VARIABLE lWriteBeforeImage AS LOGICAL NO-UNDO.

/* Tabla temporal para recibir las transacciones XML */
DEFINE TEMP-TABLE ECG
    /* Tags de transacción (lectura) */
    FIELD S001 AS CHARACTER /* Fecha */
    FIELD S002 AS CHARACTER /* Hora */
    FIELD S003 AS CHARACTER /* Comercio */
    FIELD S004 AS CHARACTER /* Sucursal */
    FIELD S005 AS CHARACTER /* Terminal */
    FIELD S006 AS CHARACTER /* Aplicativo */
    FIELD S007 AS CHARACTER /* Grupo transaccional */
    FIELD S008 AS CHARACTER /* Transacción */
    FIELD S009 AS CHARACTER /* Secuencia */
    FIELD S010 AS CHARACTER /* Origen */
    FIELD S011 AS CHARACTER /* Documento Cliente */
    FIELD S012 AS CHARACTER /* Tipo de documento */
    FIELD S013 AS CHARACTER /* Documento Cliente Destino */
    FIELD S014 AS CHARACTER /* Tipo de documento cliente destino */
    FIELD S015 AS CHARACTER /* Causal de la Transacción */
    FIELD S016 AS CHARACTER /* Adquieriente */
    FIELD S017 AS CHARACTER /* Flag de verificación de mensaje */
    FIELD S018 AS CHARACTER /* Canal */
    FIELD S019 AS CHARACTER /* Número de autorización / origen */
    FIELD S01A AS CHARACTER /* Track de la tarjeta */
    FIELD S01B AS CHARACTER /* Pin Block */
    FIELD S01C AS CHARACTER /* PCI Terminal Vendor */
    FIELD S01D AS CHARACTER /* PCI Terminal Model */
    FIELD S01E AS CHARACTER /* PCI Terminal Firmware */
    FIELD S01F AS CHARACTER /* PCI Terminal Serial */
    FIELD S01G AS CHARACTER /* PCI Terminal Vendor II */
    FIELD S01H AS CHARACTER /* PCI Terminal Model II */
    FIELD S01I AS CHARACTER /* PCI Terminal Firmware II */
    FIELD S01J AS CHARACTER /* PCI Terminal Serial II */
    FIELD S01K AS CHARACTER /* PCI Terminal Vendor III */
    FIELD S01L AS CHARACTER /* PCI Terminal Model III */
    FIELD S01M AS CHARACTER /* PCI Terminal Firmare III */
    FIELD S01N AS CHARACTER /* PCI Terminal Serial III */
    FIELD S01S AS CHARACTER /* Terminal - Ubicación */
    FIELD S01T AS CHARACTER /* Terminal - Tipo */
    FIELD S01U AS CHARACTER /* Terminal - País */
    FIELD S01V AS CHARACTER /* Terminal - Departamento */
    FIELD S01W AS CHARACTER /* Terminal - Ciudad */
    FIELD S020 AS CHARACTER /* Cuenta 1 / Origen */
    FIELD S021 AS CHARACTER /* Cuenta 2 / Destino */
    FIELD S022 AS CHARACTER /* Cuenta 3 */
    FIELD S023 AS CHARACTER /* Cuenta 4 */
    FIELD S024 AS CHARACTER /* Cuenta 5 */
    FIELD S025 AS CHARACTER /* Entidad dueña cuenta origen */
    FIELD S026 AS CHARACTER /* Entidad dueña cuenta destino */
    FIELD S027 AS CHARACTER /* Entidad dueña terminal */
    FIELD S028 AS CHARACTER /* Comercio - Terminal - NIT/CC */
    FIELD S029 AS CHARACTER /* Comercio - Terminal - Descripción */
    FIELD S02A AS CHARACTER /* Cheque - Código de banco */
    FIELD S02B AS CHARACTER /* Cheque - Cuenta de banco */
    FIELD S02C AS CHARACTER /* Cheque - Número */
    FIELD S02D AS CHARACTER /* Cheque - Total cheques */
    FIELD S02E AS CHARACTER /* Código de barras */
    FIELD S02F AS CHARACTER /* Código de transacción para cálculo de comisión */
    FIELD S030 AS CHARACTER /* Número de tarjeta */
    FIELD S031 AS CHARACTER /* Fecha contable */
    FIELD S032 AS CHARACTER /* Valor */
    FIELD S033 AS CHARACTER /* Valor base */
    FIELD S034 AS CHARACTER /* Valor impuesto */
    FIELD S035 AS CHARACTER /* Valor reteción */
    FIELD S036 AS CHARACTER /* Valor propina */
    FIELD S037 AS CHARACTER /* Valor comisión */
    FIELD S038 AS CHARACTER /* Cuotas */
    FIELD S039 AS CHARACTER /* Número de factura */
    FIELD S03A AS CHARACTER /* Tipo de cuenta 1 / Origen */
    FIELD S03B AS CHARACTER /* Tipo de cuenta 2 / Destino */
    FIELD S03C AS CHARACTER /* Tipo de cuenta 3 */
    FIELD S03D AS CHARACTER /* Tipo de cuenta 4 */
    FIELD S03E AS CHARACTER /* Tipo de cuenta 5 */
    FIELD S03F AS CHARACTER /* Secuencia a reversar */
    FIELD S03G AS CHARACTER /* Transacción para cobro comisión */
    FIELD S03H AS CHARACTER /* Código del equipo. Ej.: Número de teléfono desde donde se llamó al IVR */
    FIELD S03V AS CHARACTER /* Usuario / Autorizador */
    FIELD S03X AS CHARACTER /* Usuariol / Caja */
    FIELD S03Z AS CHARACTER /* Sesión IVR o WEB */
    FIELD S040 AS CHARACTER /* Documento */
    FIELD S041 AS CHARACTER /* Tipo de documento */
    FIELD S042 AS CHARACTER /* Nombres */
    FIELD S043 AS CHARACTER /* Apellidos */
    FIELD S044 AS CHARACTER /* Dirección residencia */
    FIELD S045 AS CHARACTER /* Dirección trabajo */
    FIELD S046 AS CHARACTER /* Teléfono residencia */
    FIELD S047 AS CHARACTER /* Teléfono trabajo */
    FIELD S048 AS CHARACTER /* Teléfono móvil */
    FIELD S049 AS CHARACTER /* Fecha de nacimiento */
    FIELD S04A AS CHARACTER /* Sexo del cliente, F o M */
    FIELD S04B AS CHARACTER /* Correo electrónico */
    FIELD S04C AS CHARACTER /* País de nacimiento */
    FIELD S04D AS CHARACTER /* Departamento de nacimiento */
    FIELD S04E AS CHARACTER /* Ciudad de nacimiento */
    FIELD S04F AS CHARACTER /* País de residencia */
    FIELD S04G AS CHARACTER /* Departamento de residencia */
    FIELD S04H AS CHARACTER /* Ciudad de residencia */
    FIELD S04I AS CHARACTER /* Tipo de cliente = ENROLAMIENTO BIOMETRÍA */
    FIELD S04J AS CHARACTER /* Información adicional cliente */
    FIELD STR0 AS CHARACTER.

DEFINE VARIABLE returnValue AS LOGICAL NO-UNDO.

ASSIGN cSourceType = /*"LONGCHAR"*/ "FILE"
       cFile = /*"tramaXML"*/ nombreArchivoEntrada
       cReadMode = "EMPTY"
       cSchemaLocation = ?
       lOverrideDefaultMapping = ?
       cFieldTypeMapping = ?
       cVerifySchemaMode = ?.

returnValue = TEMP-TABLE ECG:READ-XML(cSourceType,
                                      cFile,
                                      cReadMode,
                                      cSchemaLocation,
                                      lOverrideDefaultMapping,
                                      cFieldTypeMapping,
                                      cVerifySchemaMode).
IF returnValue THEN DO:
    FIND FIRST ecg NO-ERROR.
    IF AVAILABLE ecg THEN DO:
        Fecha = INTEGER(ECG.S001).      /* Fecha */
        Hora = INTEGER(ECG.S002).       /* Hora */
        vTerminal = INTEGER(ECG.S005).  /* No_Serie_RockPos */

        IF ECG.S007 = "0400" OR ECG.S007 = "0420" THEN
            GrupoTransaccional = 2.     /* Señal_reversion */

        CASE ECG.S008:  /* Codigo_trans */
            WHEN "00" THEN transaccion = "CR".
            WHEN "01" THEN transaccion = "RE".
            WHEN "20" THEN transaccion = "ACR".
            WHEN "21" THEN transaccion = "CE".
            WHEN "30" THEN transaccion = "CS".
            WHEN "35" THEN transaccion = "CM".
            WHEN "40" THEN transaccion = "TF".
            WHEN "89" THEN transaccion = "CCT".
        END.

        secuencia = ECG.S009.   /* N_trans */

        CASE ECG.S010:  /* Entidad_origen */
            WHEN "01" OR
            WHEN "02" OR
            WHEN "04" OR
            WHEN "05" THEN DO:
                origen = "3".
                IF transaccion = "RE" THEN
                    transaccion = "RR".
            END.
            WHEN "06" THEN origen = "T".
            WHEN "99" THEN DO:
                /* Banco de Bogotá */
                /* Códigos origen (<S016>) para el Banco de Bogotá
                    1 - Servibanca
                    2 - Redeban
                    3 - Credibanco
                    4 - Megabanco
                    5 - RedExpress
                    6 - ATH Banco de Bogotá
                    7 - ATH Aval
                    8 - Otras redes recibidas por ATH */
                origen = "T".   /* Temporal, mientras se desarrolla el tema de las comisiones */
                FIND FIRST ahorros WHERE ahorros.tarjetaDB = ECG.S030 NO-LOCK NO-ERROR.
                IF AVAILABLE ahorros THEN DO:
                    ECG.S011 = ahorros.nit.
                END.
            END.
        END CASE.

        documento_cliente = ECG.S011.           /* nit */
        documento_cliente_destino = ECG.S013.   /* nit_destino */

        CASE ECG.S015:  /* Causal de la transacción */
            WHEN "000003" THEN
                IF transaccion = "RE" THEN transaccion = "RC".
            WHEN "000004" THEN
                IF transaccion = "CE" THEN transaccion = "CC".
        END CASE.

        /* Definición de transacciones Intercooperativas */
        /* CEC - CCC - TFC */
        IF (/*transaccion = "CE" OR transaccion = "CC" OR*/ transaccion = "TF") AND INTEGER(ECG.S026) <> 3 THEN DO: /* Entidad dueña de la cuenta destino */
            CASE transaccion:
                WHEN "CE" THEN transaccion = "CEC".
                WHEN "CC" THEN transaccion = "CCC".
                WHEN "TF" THEN transaccion = "TFC".
            END CASE.
        END.
        
        /* REO - RCO - TFO */
        IF (transaccion = "RE" OR transaccion = "RC" OR transaccion = "TF") AND INTEGER(ECG.S025) <> 3 THEN DO: /* Entidad dueña de la cuenta origen */
            CASE transaccion:
                WHEN "RE" THEN transaccion = "REO".
                WHEN "RC" THEN transaccion = "RCO".
                WHEN "TF" THEN transaccion = "TFO".
            END CASE.
        END.

        /* CEO - CCO */
        IF (transaccion = "CE" OR transaccion = "CC") AND INTEGER(ECG.S027) <> 3 THEN DO: /* Entidad dueña terminal */
            CASE transaccion:
                WHEN "CE" THEN transaccion = "CEO".
                WHEN "CC" THEN transaccion = "CCO".
            END CASE.
        END.

        /* REC - RCC */
        IF (transaccion = "RE" OR transaccion = "RC") AND INTEGER(ECG.S027) <> 3 THEN DO: /* Entidad dueña terminal */
            CASE transaccion:
                WHEN "RE" THEN transaccion = "REC".
                WHEN "RC" THEN transaccion = "RCC".
            END CASE.
        END.
        /* ----- */

        CASE ECG.S01T:  /* Terminal - Tipo */
            WHEN "04" THEN
                IF transaccion = "CR" THEN transaccion = "CP".
            WHEN "23" OR
            WHEN "24" THEN origen = "2".
            WHEN "23" THEN origen = "4".
            WHEN "40" THEN origen = "1".
        END CASE.

        cuenta1_Origen = ECG.S020.                          /* Cuenta_origen */
        cuenta2_destino = ECG.S021.                         /* Cuenta_destino */
        entidad_duena_cuenta_origen = INTEGER(ECG.S025).    /* codigo_cooperativa_origen */ 
        entidad_duena_cuenta_destino = INTEGER(ECG.S026).   /* codigo_cooperativa_origen */
        entidad_duena_terminal = INTEGER(ECG.S027).         /* Cod_Coop_RockPos */
        cheque_codigo_banco = INTEGER(ECG.S02A).            /* Codigo_banco_cheque */
        cheque_cuenta_banco = ECG.S02B.                     /* Numero_Cuenta_Cheque */
        cheque_numero = ECG.S02C.                           /* Numero_Cheque */
        cheque_total_cheques = INTEGER(ECG.S02D).           /* cantidad_cheque */
        codigo_barras = ECG.S02E.                           /* codigo_barras */
        numero_tarjeta = ECG.S030.                          /* tarjeta_cliente */
        fecha_contable = INTEGER(ECG.S031).                 /* fecha_negocio */
        valor = DECIMAL(ECG.S032).                          /* Valor */
        valor_base = DECIMAL(ECG.S033).                     /* Base */
        valor_impuesto = DECIMAL(ECG.S034).                 /* IVA */
        valor_retencion = DECIMAL(ECG.S035).                /* Retencion */
        valor_propina = DECIMAL(ECG.S036).                  /* Propina */
        valor_transaccion = DECIMAL(ECG.S037).              /* valor_cobro */
        cuotas = INTEGER(ECG.S038).                         /* cuotas */
        numero_factura = INTEGER(ECG.S039).                 /* Referencia */

        CASE ECG.S03A:  /* Codigo_Producto_Origen */
            WHEN "10" THEN tipo_cuenta1_origen = "AH".
            WHEN "50" THEN tipo_cuenta1_origen = "CR".
            WHEN "60" THEN tipo_cuenta1_origen = "CT".
        END CASE.

        CASE ECG.S03B:  /* Codigo_producto_destino */
            WHEN "10" THEN tipo_cuenta2_destino = "AH".
            WHEN "50" THEN tipo_cuenta2_destino = "CR".
            WHEN "60" THEN tipo_cuenta2_destino = "CT".
        END.

        secuencia_reversar = ECG.S03F.  /* Nro_trans_reversar */
        usuario_caja = ECG.S03X. /* Nit_empleado */
        ECG_STR0 = ECG.STR0.

    END.
END.
