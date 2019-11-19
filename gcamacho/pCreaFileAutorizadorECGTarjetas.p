
/*
    Generación archivos planos, proceso autorizador del ECG
*/



                                  
/**************************** FUNCIONES ************************************/
FUNCTION getFecToInt RETURNS INT64
    (INPUT ipdaFecha    AS DATE):

    DEFINE VARIABLE vi6Fecha AS INT64       NO-UNDO.

    ASSIGN vi6Fecha = INT64(STRING(YEAR(ipdaFecha),"9999") + STRING(MONTH(ipdaFecha),"99") + STRING(DAY(ipdaFecha),"99")).

    RETURN vi6Fecha.
    
END FUNCTION.


FUNCTION decToChar RETURNS CHARACTER
	(INPUT ipdeValor AS DECIMAL):
    /*------------------------------------------------------------------------------
      Purpose:  Convertir un decimal de dos decimales en character
        Notes:  toma un decimal y lo convierte a character, dejando en los dos ultimos
                caracteres, los decimales
                Eje: 12345.78  --> queda como --> "1234578"
    --------------------------------------------- ---- -----------------------------*/
    
	DEFINE VARIABLE vcValor AS CHARACTER NO-UNDO.

	ASSIGN 
		vcValor = REPLACE(STRING(TRUNCATE(ipdeValor,2),"->>>>>>>>>>>9.99"),".","").

	RETURN vcValor.

END FUNCTION.


/*************************************************************/


DEFINE VARIABLE vdtIni AS DATETIME    NO-UNDO.
DEFINE VARIABLE ipdaFecAct AS DATE INITIAL TODAY NO-UNDO.

DEFINE VARIABLE vcCodEntidad  AS CHARACTER INITIAL "00000005"  NO-UNDO.
DEFINE VARIABLE vclCadena AS LONGCHAR NO-UNDO.

DEFINE VARIABLE viCntClientes   AS INTEGER     NO-UNDO. /*Contador registros a cargar Clientes */
DEFINE VARIABLE viCntCuentas    AS INTEGER     NO-UNDO. /*Contador registros a cargar Cuentas  */
DEFINE VARIABLE viCntCupos      AS INTEGER     NO-UNDO. /*Contador registros a cargar Cupos X canal- Cuenta  */
DEFINE VARIABLE viCntTarjetas   AS INTEGER     NO-UNDO. /*Contador registros a cargar Tarjetas */
DEFINE VARIABLE viCntTarCuenta  AS INTEGER     NO-UNDO. /*Contador registros a cargar Tarjeta Vs. Cuenta*/

DEFINE TEMP-TABLE TPrint
    FIELDS  tipo    AS INTEGER
    FIELDS  cadena  AS CHARACTER FORMAT "X(500)"
    FIELDS  fecha   AS CHARACTER FORMAT "99999999"
    INDEX idx tipo.

DEFINE TEMP-TABLE TClientes
    FIELDS  Signo       AS CHARACTER    INITIAL "+"
    FIELDS  nit         LIKE    clientes.nit
    FIELDS  tpNit       AS INTEGER /*0. Cédula, 1. Ced. Extranjeria, 2. Tarj. Identidad 3. Pasaporte*/
    FIELDS  nombre      LIKE clientes.nombre
    FIELDS  nombre2     LIKE clientes.nombre
    FIELDS  apellido1   LIKE clientes.apellido1
    FIELDS  apellido2   LIKE clientes.apellido2
    FIELDS  dirCasa     AS CHARACTER
    FIELDS  dirTrabajo  AS CHARACTER
    FIELDS  telCasa     AS INT64
    FIELDS  telTrabajo  AS INT64
    FIELDS  telMovil    AS INT64
    FIELDS  fecNacim    AS CHARACTER
    FIELDS  genero      AS CHARACTER /*F o M*/
    FIELDS  correoe     AS CHARACTER
    FIELDS  paisResid   AS CHARACTER
    FIELDS  DeptoResid  AS CHARACTER
    FIELDS  CiudadResid AS CHARACTER
    FIELDS  paisNacim   AS CHARACTER
    FIELDS  DeptoNacim  AS CHARACTER
    FIELDS  CiudadNacim AS CHARACTER
    FIELDS  nivelAcceso AS CHARACTER INITIAL "0000"
    FIELDS  fecAct      AS INT64  FORMAT "99999999"      /*Fecha de actualización*/
    INDEX  nit IS UNIQUE nit.

DEFINE TEMP-TABLE TCuentas
    FIELDS Signo        AS CHARACTER INITIAL "+"
    FIELDS codEntidad   AS CHARACTER 
    FIELDS nit          LIKE ahorros.nit
    FIELDS cuenta       AS CHARACTER FORMAT "999999999999"
    FIELDS tpProd       AS INTEGER /*10. Ahorros; 50. Cupo Rotativo*/
    FIELDS CtaDefault   AS INTEGER  /*0. No; 1. Si*/
    FIELDS Constante    AS INTEGER INITIAL 170
    FIELDS sdoTotal     AS CHARACTER 
    FIELDS sdoDispon    AS CHARACTER
/*     FIELDS NumTxMaxATM  AS CHARACTER */
/*     FIELDS NumTxMaxPOS  AS CHARACTER */
/*     FIELDS cupoATM      AS CHARACTER */
/*     FIELDS cupoPOS      AS CHARACTER */
    FIELDS fecAct       AS INT64  FORMAT "99999999"      /*Fecha de actualización*/
    INDEX idx nit cuenta.

DEFINE TEMP-TABLE TCupos
    FIELDS Signo        AS CHARACTER INITIAL "+"
    FIELDS codEntidad   AS CHARACTER 
    FIELDS nit          LIKE ahorros.nit
    FIELDS cuenta       AS CHARACTER FORMAT "999999999999"
    FIELDS canal        AS CHARACTER
    FIELDS NumTxCanal   AS CHARACTER
    FIELDS ValTxCanal   AS CHARACTER
    FIELDS fecAct       AS INT64  FORMAT "99999999"      /*Fecha de actualización*/
    INDEX idx nit cuenta.

DEFINE TEMP-TABLE TTarjetas
    FIELDS Signo        AS CHARACTER INITIAL "+"
    FIELDS codEntidad   AS CHARACTER INITIAL "00000005"
    FIELDS nit          LIKE ahorros.nit
    FIELDS tarjetaDB    LIKE ahorros.tarjetaDB
    FIELDS estadoTDB    AS CHARACTER INITIAL "00"       
    FIELDS fecAct       AS INT64  FORMAT "99999999"      /*Fecha de actualización*/
    INDEX idx nit tarjetaDB .

DEFINE TEMP-TABLE TTDBCta
    FIELDS Signo        AS CHARACTER INITIAL "+"
    FIELDS codEntidad   AS CHARACTER INITIAL "00000005"
    FIELDS nit          LIKE ahorros.nit
    FIELDS tarjetaDB    LIKE ahorros.tarjetaDB
    FIELDS cuenta       AS CHARACTER FORMAT "999999999999"
    FIELDS tpProd       AS INTEGER /*10. Ahorros; 50. Cupo Rotativo*/
    FIELDS fecAct       AS INT64  FORMAT "99999999"      /*Fecha de actualización*/
    INDEX idx nit tarjetaDB .

ASSIGN vdtIni = NOW.

/*******************************/
DEFINE TEMP-TABLE TC
    FIELDS tarjetaDB    LIKE ahorros.tarjetaDB
    FIELDS cuenta       LIKE ahorros.cue_ahorros
    FIELDS nit          LIKE ahorros.nit
    FIELDS cue_ahorros  AS CHARACTER FORMAT "X(12)"
    INDEX nit nit.

INPUT FROM "c:\info_juriscoop\TarjetasJuriscoop061010.csv".
REPEAT:
    CREATE TC.
    IMPORT DELIMITER ";" TC.
END.
INPUT CLOSE.

/*********************************************/


RUN creaTT.
RUN creaFiles.

MESSAGE "FIN"
    "Tiempo " INTERVAL(NOW,vdtIni,"minutes") " Minutos"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

PROCEDURE creaTT:
    DEFINE VARIABLE viNumTXATM AS CHARACTER /* FORMAT "99999999.99" */ NO-UNDO.
    DEFINE VARIABLE viValTXATM AS CHARACTER /* FORMAT "99999999.99" */ NO-UNDO.
    DEFINE VARIABLE viNumTXPOS AS CHARACTER /* FORMAT "99999999.99" */ NO-UNDO.
    DEFINE VARIABLE viValTXPOS AS CHARACTER /* FORMAT "99999999.99" */ NO-UNDO.

    FOR EACH TC WHERE nit NE "" NO-LOCK:
        IF LENGTH(TC.cuenta) EQ 12 THEN NEXT.


    /*     FOR EACH ahorros WHERE ahorros.tarjetaDB NE "" AND tarjetaDB BEGINS "5907" AND estado EQ 1 AND */
    /*         ahorros.agencia EQ 31 AND */
    /*         ahorros.nit EQ "80374147" AND */
    /*         TRUE NO-LOCK: */
    
        FIND FIRST ahorros WHERE ahorros.nit EQ TC.Nit AND 
                                ahorros.tip_ahorro EQ 1 AND 
                                ahorros.cue_ahorro NE "216" AND 
                                ahorros.estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAILABLE ahorros THEN DO:
            IF LENGTH(ahorros.tarjetaDB) NE 16 THEN NEXT.
    
            FIND FIRST clientes WHERE clientes.nit EQ ahorros.nit NO-LOCK NO-ERROR.
            /*CLientes*/
            IF AVAILABLE clientes THEN DO:
    
                IF Clientes.Tipo_Identificacion EQ "nit" THEN NEXT.
    
                FIND FIRST tClientes WHERE tClientes.nit EQ clientes.nit NO-LOCK NO-ERROR.
                IF NOT AVAILABLE tClientes THEN DO:
                    CREATE tClientes.
                    BUFFER-COPY clientes TO TClientes.
                    UPDATE  TClientes.apellido1 = IF TClientes.apellido1 EQ "" THEN TClientes.nombre ELSE TClientes.Apellido1
                            TClientes.fecAct = getFecToInt(ipdaFecAct).
                   CASE Clientes.Tipo_Identificacion:
                       WHEN "t.i" THEN UPDATE TClientes.tpNit = 2.
                       WHEN "nit" THEN UPDATE TClientes.tpNit = 9.
                       WHEN "r.c" THEN UPDATE TClientes.tpNit = 4.
                       OTHERWISE       UPDATE TClientes.tpNit = 0.
                   END CASE.
    
    
                END.
            END.
    
            /*Cuenta Ahorros*/
            CREATE TCuentas.
            BUFFER-COPY ahorros TO TCuentas.
            UPDATE  TCuentas.CodEntidad     = vcCodEntidad
                    TCuentas.cuenta         = STRING(ahorros.agencia,"999") + STRING(ahorros.cod_ahorro,"999") + STRING(INTEGER(ahorros.cue_ahorros),"999999")
                    TCuentas.tpProd         = 10
                    TCuentas.CtaDefault     = 1
                    TCuentas.sdoTotal       = TRIM(STRING(decToChar(sdo_canje + sdo_Disponible)))
                    TCuentas.sdoDispon      = TRIM(STRING(decToChar(sdo_Disponible)))
    /*                 TCuentas.NumTxMaxATM    = TRIM(STRING(decToChar(100.00)))     */
    /*                 TCuentas.NumTxMaxPOS    = TRIM(STRING(decToChar(100.00)))     */
    /*                 TCuentas.cupoATM        = TRIM(STRING(decToChar(5000000.00))) */
    /*                 TCuentas.cupoPOS        = TRIM(STRING(decToChar(5000000.00))) */
    
    /*                 TCuentas.NumTxMaxATM    = viNumTXATM */
    /*                 TCuentas.NumTxMaxPOS    = viNumTXPOS */
    /*                 TCuentas.cupoATM        = viValTXATM */
    /*                 TCuentas.cupoPOS        = viValTXPOS */
    
                    TCuentas.fecAct         = getFecToInt(ipdaFecAct).
    
            /*Crear cupos por canal - Cuenta Ahorros*/
            FOR EACH canales NO-LOCK:
                CREATE TCupos.
                UPDATE 
                    TCupos.CodEntidad   = vcCodEntidad
                    TCupos.nit          = ahorros.nit
                    TCupos.cuenta       = STRING(ahorros.agencia,"999") + STRING(ahorros.cod_ahorro,"999") + STRING(INTEGER(ahorros.cue_ahorros),"999999")
                    TCupos.canal        = canales.cod_canal
                    TCupos.NumTxCanal   = TRIM(STRING(canales.NmaxTxDia))
                    TCupos.ValTxCanal   = TRIM(STRING(decToChar(canales.VmaxTxDia)))
                    TCupos.fecAct       = getFecToInt(ipdaFecAct).
            END.
    
            /*Tarjetas DB*/
            FIND FIRST TTarjetas WHERE TTarjetas.nit EQ ahorros.nit AND  TTarjetas.tarjeta EQ ahorros.tarjetaDB NO-LOCK NO-ERROR.
            IF NOT AVAILABLE TTarjetas THEN DO:
                CREATE TTarjetas.
                BUFFER-COPY ahorros TO TTarjetas.
                UPDATE  TTarjetas.CodEntidad   = vcCodEntidad
                        TTarjetas.fecAct = getFecToInt(ipdaFecAct).
            END.
    
            /*Tarjetas Vs. Cuentas*/
            CREATE TTDBCta.
            BUFFER-COPY ahorros TO TTDBCta.
            UPDATE 
                TTDBCta.CodEntidad   = vcCodEntidad
                TTDBCta.cuenta  = STRING(ahorros.agencia,"999") + STRING(ahorros.cod_ahorro,"999") + STRING(INTEGER(ahorros.cue_ahorros),"999999")
                TTDBCta.tpProd  = 10
                TTDBCta.fecAct  = getFecToInt(ipdaFecAct).
    
            /*Cuenta Credito Rotativo*/
            FIND FIRST creditos WHERE creditos.nit EQ ahorros.nit AND (creditos.cod_credito EQ 570 OR creditos.cod_credito EQ 870) AND creditos.estado EQ 2 NO-LOCK NO-ERROR.
            IF AVAILABLE creditos THEN DO:
                CREATE TCuentas.
                UPDATE TCuentas.codEntidad  = vcCodEntidad
                       TCuentas.nit         = ahorros.nit
                       TCuentas.cuenta      = STRING(creditos.agencia,"999") + STRING(creditos.cod_credito,"999") + STRING(creditos.num_credito,"999999")
                       TCuentas.tpProd      = 50
                       TCuentas.CtaDefault  = 0
                       TCuentas.sdoTotal    = TRIM(STRING(decToChar(Creditos.Monto - Creditos.Sdo_Capital)))
                       TCuentas.sdoDispon   = TRIM(STRING(decToChar(Creditos.Monto - Creditos.Sdo_Capital)))
    /*                    TCuentas.NumTxMaxATM = TRIM(STRING(decToChar(100.00)))     */
    /*                    TCuentas.NumTxMaxPOS = TRIM(STRING(decToChar(100.00)))     */
    /*                    TCuentas.cupoATM     = TRIM(STRING(decToChar(5000000.00))) */
    /*                    TCuentas.cupoPOS     = TRIM(STRING(decToChar(5000000.00))) */
    
    /*                 TCuentas.NumTxMaxATM    = viNumTXATM */
    /*                 TCuentas.NumTxMaxPOS    = viNumTXPOS */
    /*                 TCuentas.cupoATM        = viValTXATM */
    /*                 TCuentas.cupoPOS        = viValTXPOS */
    
                       TCuentas.fecAct      = getFecToInt(ipdaFecAct).
    
                /*Tarjetas Vs. Cuentas*/
                CREATE TTDBCta.
                UPDATE 
                    TTDBCta.CodEntidad   = vcCodEntidad
                    TTDBCta.cuenta  = STRING(creditos.agencia,"999") + STRING(creditos.cod_credito,"999") + STRING(INTEGER(creditos.num_credito),"999999")
                    TTDBCta.tpProd  = 50
                    TTDBCta.fecAct  = getFecToInt(ipdaFecAct)
                    TTDBCta.nit     = creditos.nit
                    TTDBCta.TarjetaDB = ahorros.tarjetaDB.
    
                /*Crear cupos por canal - Cuenta CR*/
                FOR EACH canales NO-LOCK:
                    CREATE TCupos.
                    UPDATE 
                        TCupos.CodEntidad   = vcCodEntidad
                        TCupos.nit          = ahorros.nit
                        TCupos.cuenta       = STRING(creditos.agencia,"999") + STRING(creditos.cod_credito,"999") + STRING(creditos.num_credito,"999999")
                        TCupos.canal        = canales.cod_canal
                        TCupos.NumTxCanal   = TRIM(STRING(decToChar(canales.NmaxTxDia))) 
                        TCupos.ValTxCanal   = TRIM(STRING(decToChar(canales.VmaxTxDia)))
                        TCupos.fecAct       = getFecToInt(ipdaFecAct).
                END.
    
    /*                 CASE canales.cod_canal:                                                  */
    /*                     WHEN "01" THEN /*ATM*/                                               */
    /*                         ASSIGN viNumTXATM = TRIM(STRING(decToChar(canales.NmaxTxDia)))   */
    /*                                viValTXATM = TRIM(STRING(decToChar(canales.VmaxTxDia))).  */
    /*                                                                                          */
    /*                     WHEN "02" THEN /*POS*/                                               */
    /*                         ASSIGN viNumTXPOS = TRIM(STRING(decToChar(canales.NmaxTxDia)))   */
    /*                                viValTXPOS = TRIM(STRING(decToChar(canales.VmaxTxDia))).  */
    /*                 END CASE.                                                                */
    /*             END.                                                                         */
    
            END. /*IF AVAILABLE creditos THEN DO:*/
        END. /*FOR EACH ahorros */
    END.
END PROCEDURE.


PROCEDURE creaFiles:
    DEFINE VARIABLE vcNomPath AS CHARACTER INITIAL "c:\info_Juriscoop\" NO-UNDO.
    DEFINE VARIABLE vcNomFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vcNomFileCl     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcNomFileTar    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcNomFileCta    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcNomFileCupos  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcNomFileTarCta AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcNomFileCtrl   AS CHARACTER   NO-UNDO.


    ASSIGN vcNomFile = vcNomFile + STRING(getFecToInt(ipdaFecAct)) + "." + vcCodEntidad + "."
        vcNomFileCl     = vcNomFile + "FCL"
        vcNomFileTar    = vcNomFile + "FCA"
        vcNomFileCta    = vcNomFile + "FAC"
        vcNomFileCupos  = vcNomFile + "FACH"
        vcNomFileTarCta = vcNomFile + "FCVA"
        vcNomFileCtrl   = vcNomFile + "F".

OUTPUT TO VALUE(vcNomPath + vcNomFileCl).
    FOR EACH  TClientes NO-LOCK:
        CREATE TPrint.
        UPDATE tipo = 1
               cadena = TRIM(STRING(Signo))         + "," +
                        TRIM(STRING(nit,"x(15)"))   + "," +
                        TRIM(STRING(tpNit))         + "," +
/*                         TRIM(STRING(nombre))        + "," + */
                        TRIM(REPLACE(STRING(nombre),    ",", " "))        + "," +
                        TRIM(REPLACE(STRING(nombre2),   ",", " "))       + "," +
                        TRIM(REPLACE(STRING(apellido1), ",", " "))     + "," +
                        TRIM(REPLACE(STRING(apellido2), ",", " "))     + "," +

/*                         TRIM(STRING(nombre2))       + "," + */
/*                         TRIM(STRING(apellido1))     + "," + */
/*                         TRIM(STRING(apellido2))     + "," + */
                        TRIM(STRING(dirCasa))       + "," +
                        TRIM(STRING(dirTrabajo))    + "," +
                        TRIM(STRING(telCasa))       + "," +
                        TRIM(STRING(telTrabajo))    + "," +
                        TRIM(STRING(telMovil))      + "," +
                        TRIM(STRING(fecNacim))      + "," +
                        TRIM(STRING(genero))        + "," +
                        TRIM(STRING(correoe))       + "," +
                        TRIM(STRING(paisResid))     + "," +
                        TRIM(STRING(DeptoResid))    + "," +
                        TRIM(STRING(CiudadResid))   + "," +
                        TRIM(STRING(paisNacim))     + "," +
                        TRIM(STRING(DeptoNacim))    + "," +
                        TRIM(STRING(CiudadNacim))   + "," +
                        TRIM(STRING(nivelAcceso))   + "," +
                        TRIM(STRING(fecAct)).
        PUT UNFORMATTED TPrint.cadena SKIP.
        ASSIGN viCntClientes = viCntClientes + 1.
    END.
    OUTPUT CLOSE.

    


    OUTPUT TO VALUE(vcNomPath + vcNomFileCta).
    FOR EACH TCuentas NO-LOCK:
        CREATE TPrint.
        UPDATE tipo = 2
            cadena = TRIM(STRING(TCuentas.Signo))        + "," +
                     TRIM(STRING(TCuentas.codEntidad))   + "," +
                     TRIM(STRING(TCuentas.nit))          + "," +
                     TRIM(STRING(TCuentas.cuenta))       + "," +
                     TRIM(STRING(TCuentas.tpProd))       + "," +
                     TRIM(STRING(TCuentas.CtaDefault))   + "," +
                     TRIM(STRING(TCuentas.Constante))    + "," +
                     TRIM(STRING(TCuentas.sdoTotal))     + "," +
                     TRIM(STRING(TCuentas.sdoDispon))    + "," +
/*                      TRIM(STRING(TCuentas.NumTxMaxATM))  + "," + */
/*                      TRIM(STRING(TCuentas.NumTxMaxPOS))  + "," + */
/*                      TRIM(STRING(TCuentas.cupoATM))      + "," + */
/*                      TRIM(STRING(TCuentas.cupoPOS))      + "," + */
                     TRIM(STRING(TCuentas.fecAct)).
        PUT UNFORMATTED (TPrint.cadena) SKIP.
        ASSIGN viCntCuentas = viCntCuentas + 1.
    END.
    OUTPUT CLOSE.

    /*Cupos*/
    OUTPUT TO VALUE(vcNomPath + vcNomFileCupos).
    FOR EACH TCupos NO-LOCK:
        CREATE TPrint.
        UPDATE tipo = 2
            cadena = TRIM(STRING(TCupos.Signo))        + "," +
                     TRIM(STRING(TCupos.codEntidad))   + "," +
                     TRIM(STRING(TCupos.nit))          + "," +
                     TRIM(STRING(TCupos.cuenta))       + "," +
                     TRIM(STRING(TCupos.canal))       + "," +
                     TRIM(STRING(TCupos.NumTxCanal))   + "," +
                     TRIM(STRING(TCupos.ValTxCanal))    + "," +
                     TRIM(STRING(TCupos.fecAct)).
        PUT UNFORMATTED (TPrint.cadena) SKIP.
        ASSIGN viCntCupos = viCntCupos + 1.
    END.
    OUTPUT CLOSE.


    OUTPUT TO VALUE(vcNomPath + vcNomFileTar).
    FOR EACH TTarjetas NO-LOCK:
        CREATE TPrint.
        UPDATE  tipo = 3
                cadena = TRIM(STRING(TTarjetas.Signo))      + "," +
                         TRIM(STRING(TTarjetas.codEntidad)) + "," +
                         TRIM(STRING(TTarjetas.nit))        + "," +
                         TRIM(STRING(TTarjetas.tarjetaDB))  + "," +
                         TRIM(STRING(TTarjetas.estadoTDB))  + "," +
                         TRIM(STRING(TTarjetas.fecAct)).
        PUT UNFORMATTED (TPrint.cadena) SKIP.
        ASSIGN viCntTarjetas = viCntTarjetas + 1.
    END.
    OUTPUT CLOSE.

    OUTPUT TO VALUE(vcNomPath + vcNomFileTarCta).
    FOR EACH TTDBCta NO-LOCK:
        CREATE TPrint.
        UPDATE  tipo = 4 
                cadena = TRIM(STRING(TTDBCta.Signo))        + "," +
                         TRIM(STRING(TTDBCta.codEntidad))   + "," +
                         TRIM(STRING(TTDBCta.nit))          + "," +
                         TRIM(STRING(TTDBCta.tarjetaDB))    + "," +
                         TRIM(STRING(TTDBCta.cuenta))       + "," +
                         TRIM(STRING(TTDBCta.tpProd))       + "," +
                         TRIM(STRING(TTDBCta.fecAct)).  

        PUT UNFORMATTED (TPrint.cadena) SKIP.
        ASSIGN viCntTarCuenta = viCntTarCuenta + 1.        
    END.
    OUTPUT CLOSE.

    OUTPUT TO VALUE(vcNomPath + vcNomFileCtrl).
        CREATE TPrint.
        UPDATE tipo = 5
                cadena = TRIM(STRING(vcNomFileCl))      + "," + TRIM(STRING(viCntClientes)).
        PUT UNFORMATTED (TPrint.cadena) SKIP.
        CREATE TPrint.
        UPDATE tipo = 5
            cadena = TRIM(STRING(vcNomFileCta))     + "," + TRIM(STRING(viCntCuentas)).
        PUT UNFORMATTED (TPrint.cadena) SKIP.
        CREATE TPrint.
        UPDATE tipo = 5
            cadena = TRIM(STRING(vcNomFileTar))     + "," + TRIM(STRING(viCntTarjetas)).            
        PUT UNFORMATTED (TPrint.cadena) SKIP.
        CREATE TPrint.
        UPDATE tipo = 5
            cadena = TRIM(STRING(vcNomFileTarCta))  + "," + TRIM(STRING(viCntTarCuenta)).
        PUT UNFORMATTED (TPrint.cadena) SKIP.

        UPDATE tipo = 5
            cadena = TRIM(STRING(vcNomFileCupos))  + "," + TRIM(STRING(viCntCupos)).
        PUT UNFORMATTED (TPrint.cadena) SKIP.
        
     OUTPUT CLOSE.

END PROCEDURE.


