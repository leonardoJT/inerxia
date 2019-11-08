/*--------------------------------------------------------------------------
 Formatos globales:
 1.CDAT    2.NOTAMULTIPLE  3.TAC  4.LIBRANZA  5.PAGARE
---------------------------------------------------------------------------*/                  
{INCLUIDO\VARIABLE.I " shared"}
DEFINE INPUT  PARAMETER P_Formato  AS CHARACTER FORMAT "X(10)".
DEFINE INPUT  PARAMETER WNit      LIKE Clientes.Nit.
DEFINE INPUT  PARAMETER WNumpag   LIKE Creditos.num_credito.

DEFINE VAR W_ArcSalida AS CHARACTER FORMAT "X(80)" INITIAL "".

DEFI TEMP-TABLE salidaImp
     FIELD detalle AS CHARACTER FORMAT "x(172)".

IF TRIM(P_Formato) EQ 'PAGARE' THEN
   RUN Pagare.


PROCEDURE pagare.
    /* Recibe como parametros el nit en W_ArcSalida y Nro Pagare en W_Transac */
    DEFINE VAR W_Cadena     AS CHARACTER FORMAT "X(90)" INITIAL "".
    DEFINE VAR NomMes AS CHARACTER FORMAT "X(12)" EXTENT 12
           INITIAL ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                    "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"].
    DEFINE VAR W_Municipio    AS CHARACTER FORMAT "X(60)".
    DEFINE VAR W_Puntero      AS ROWID.
    DEFINE VAR W_NomAgencia   AS CHARACTER FORMAT "X(45)".
    DEFINE VAR W_NomPagador   AS CHARACTER FORMAT "X(45)".
    DEFINE VAR W_NomEmpresa   AS CHARACTER FORMAT "X(45)".
    DEFINE VAR W_NomEmpresa2  AS CHARACTER FORMAT "X(45)".
    DEFINE VAR W_NomDeudor    AS CHARACTER FORMAT "X(45)".
    DEFINE VAR W_NomDeudor2   AS CHARACTER FORMAT "X(45)".
    DEFINE VAR W_NomDeudor3   AS CHARACTER FORMAT "X(45)".
    DEFINE VAR W_NomDeudor4   AS CHARACTER FORMAT "X(45)".
    DEFINE VAR W_NomDirAgenc  AS CHARACTER FORMAT "X(90)".
    DEFINE VAR W_NomDireccion AS CHARACTER FORMAT "X(90)".
    DEFINE VAR W_MontoLetras  AS CHARACTER FORMAT "X(90)".
    DEFINE VAR W_NomMes       AS CHARACTER FORMAT "X(12)".
    DEFINE VAR W_CiuAgencia   AS CHARACTER FORMAT "X(35)".
    DEFINE VAR W_MontoIzq     AS CHARACTER FORMAT "X(20)".
    DEFINE VAR W_fecVcto      LIKE PlanPagos.Fec_Inic. 

    DEFINE VAR AH_Atermino     LIKE Ahorros.Sdo_Disponible.
    DEFINE VAR AH_AlaVista     LIKE Ahorros.Sdo_Disponible.
    DEFINE VAR AH_Contractual  LIKE Ahorros.Sdo_Disponible.
    DEFINE VAR AH_Aportes      LIKE Ahorros.Sdo_Disponible.


    DEFINE VAR CR_Comercial    LIKE Ahorros.Sdo_Disponible.
    DEFINE VAR CR_Consumo      LIKE Ahorros.Sdo_Disponible.
    DEFINE VAR CR_Hipotecario  LIKE Ahorros.Sdo_Disponible.
    DEFINE VAR CR_MicroCredito LIKE Ahorros.Sdo_Disponible.

    DEFINE VAR AHT LIKE Ahorros.Sdo_Disponible.
    DEFINE VAR CRT LIKE Ahorros.Sdo_Disponible.
    DEFINE VAR TOT LIKE Ahorros.Sdo_Disponible.
    DEFI   VAR W_NPagare LIKE Creditos.Num_Credito.


    FOR EACH Ahorros WHERE
             Ahorros.Nit             EQ WNit AND
             Ahorros.Estado          EQ 1    AND
             Ahorros.Fec_Cancelacion EQ ?    AND
             Ahorros.FOR_Pago        EQ 2 NO-LOCK:
       CASE Ahorros.Tip_Ahorro:
         WHEN 1 THEN AH_AlaVista    = AH_AlaVista    + Ahorros.Cuota.
         WHEN 2 THEN AH_Contractual = AH_Contractual + Ahorros.Cuota.
         WHEN 3 THEN AH_ATermino    = AH_ATermino    + Ahorros.Cuota.
         WHEN 4 THEN AH_Aportes     = AH_Aportes     + Ahorros.Cuota.
       END CASE.
       ASSIGN AHT = AHT + Ahorros.Cuota.
              TOT = TOT + Ahorros.Cuota.
    END.

    FOR EACH Creditos WHERE
             Creditos.Nit         EQ WNit AND
             Creditos.Estado      EQ 2 AND
             Creditos.Sdo_Capital GT 0 AND
             Creditos.FOR_Pago    EQ 2 NO-LOCK:
       CASE Creditos.Tip_Credito:
         WHEN 1 THEN CR_Consumo      = CR_Consumo      + Creditos.Cuota.
         WHEN 2 THEN CR_Comercial    = CR_Comercial    + Creditos.Cuota.
         WHEN 3 THEN CR_Hipotecario  = CR_Hipotecario  + Creditos.Cuota.
         WHEN 4 THEN CR_Microcredito = CR_Microcredito + Creditos.Cuota.
       END CASE.
       ASSIGN CRT = CRT + Creditos.Cuota.
              TOT = TOT + Creditos.Cuota.
    END.

    FIND Clientes WHERE Clientes.Nit EQ WNit NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Clientes THEN DO:
       MESSAGE "No se encontró el cliente: " Clientes.nit
           VIEW-AS ALERT-BOX ERROR TITLE "Error pagare".
       RETURN ERROR.
    END.
    W_NomDireccion = Clientes.DIR_Comercial.
    IF W_NomDireccion EQ "" THEN W_NomDireccion = Clientes.DIR_Residencia.
    W_NomDireccion = W_NomDireccion + ",".

    FIND Creditos WHERE Creditos.Num_credito EQ WNumpag AND
                        Creditos.Nit         EQ WNit NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Creditos THEN DO:
       MESSAGE "No se ha encontrado el crédito con Numero: " WNumpag SKIP
               "del Nit: " WNit  SKIP(1)
               "Se cancela la operación de desembolso" VIEW-AS ALERT-BOX ERROR  TITLE "Error pagare".
       RETURN ERROR.
    END.
   /*
    FIND planpagos WHERE planpagos.agencia     EQ creditos.agencia     AND
                         planpagos.nit         EQ creditos.nit         AND
                         planpagos.num_credito EQ Creditos.num_credito AND 
                         planpagos.nro_cuota   EQ creditos.plazo NO-LOCK NO-ERROR.
    IF AVAILABLE(planpagos) THEN
       w_fecvcto = PlanPagos.Fec_Inic.
    ELSE
       MESSAGE "NO SE ENCONTRO EL ULTIMO PLAN PAGOS"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */
    RUN MontoEsc.r (INPUT Creditos.Monto,INPUT 0, OUTPUT W_MontoLetras).

    FIND Agencias WHERE Agencias.Agencia EQ W_Agencia AND Agencias.Estado EQ 1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Agencias THEN DO:
       MESSAGE "La Agencia para el desembolso no se encuentra" SKIP
               "disponible. se cancela la operación de desembolso" VIEW-AS ALERT-BOX.
    END.


    ASSIGN W_NomAgencia   = Agencias.Nombre  + " - " + STRING(Agencias.Agencia,"999")
           W_NPagare      = Creditos.Num_Credito
           W_NomDirAgenc = TRIM(Agencias.direccion).

    FIND FIRST Ubicacion WHERE SUBSTR(Ubicacion.Ubicacion,1,5) EQ SUBSTR(Agencia.Ciudad,1,5) NO-LOCK NO-ERROR.
    IF AVAILABLE Ubicacion THEN
       W_CiuAgencia = Ubicacion.Nombre.
    ELSE
        W_CiuAgencia = ".".

    /* PAGARE */
    W_ArcSalida = "\\172.28.1.201\d\tempo\pagare" + TRIM(W_Usuario) + ".txt".
    OUTPUT TO VALUE(W_ArcSalida).    
    PUT "InIcIo" SKIP(1).
    W_Cadena = "@p51 " + TRIM(STRING(W_NPagare)).
    PUT W_cadena SKIP(1).
/* POR EL MOMENTO NO SE REQUIERE DE ESTAS FECHAS AUNQUE SE PUEDE IMPLEMENTAR LUEGO 
   CAMBIO SOLICITADO POR EL CONTRALOR EL 25 DE FEB DE 2005 */
/*  W_Cadena = "@p52 " + TRIM(STRING(DAY(Creditos.fec_desembolso))) + ' de ' + NomMes[MONTH(Creditos.fec_Desembolso)] + ' de ' + TRIM(STRING(YEAR(Creditos.fec_desembolso))).
    PUT W_Cadena SKIP(0).
    W_Cadena = "@p53 " + TRIM(STRING(DAY(W_FecVcto))) + ' de ' + NomMes[MONTH(W_FecVcto)] + " de " + TRIM(STRING(YEAR(W_FecVcto))).
    PUT W_Cadena SKIP(0). */
    W_Cadena = "@p55 " + TRIM(W_NomAgencia).
    PUT W_Cadena SKIP(0).
    W_Cadena = "@p56 " + TRIM(W_CiuAgencia) .
    PUT W_Cadena SKIP(0).
    W_Cadena = "@p57 " + W_MontoLetras.
    PUT W_Cadena SKIP(0).
    W_Cadena = "@p58 " + TRIM(STRING(Creditos.Monto,">>>,>>>,>>>,>>9")).
    PUT W_Cadena SKIP(1).


    W_Cadena = "@co01 " + CAPS(TRIM(clientes.nombre)) + ' ' + CAPS(TRIM(clientes.apellido1)) + ' ' + CAPS(TRIM(clientes.apellido2)).
    PUT W_Cadena skip(0).
    W_Cadena = "@co02 " + Clientes.Nit.
    PUT W_Cadena SKIP(0).
    /* Codeudores - todos          */
    
    FOR EACH relaciones WHERE relaciones.Nit EQ  Creditos.Nit  AND 
      TRIM(relaciones.cuenta)   EQ TRIM(string(Creditos.num_credito)) AND
      relaciones.estado         EQ 1   AND       
      relaciones.COD_relacion   EQ 11  AND
      relaciones.clase_producto EQ 2   NO-LOCK:
      FOR EACH clientes WHERE clientes.nit EQ relaciones.nit_relacion NO-LOCK:
        W_Cadena = "@co01 " + CAPS(TRIM(clientes.nombre)) + ' ' + CAPS(TRIM(clientes.apellido1)) + ' ' + CAPS(TRIM(clientes.apellido2)).
        PUT W_Cadena skip(0).
        W_Cadena = "@co02 " + Clientes.Nit.
        PUT W_Cadena SKIP(0).
      END.
    END. 
    PUT " " SKIP(0).
    FIND Clientes WHERE Clientes.Nit EQ WNit NO-LOCK NO-ERROR.


    W_Cadena = "@p65 " + TRIM(STRING(Creditos.Monto,">>>,>>>,>>9")).
    PUT W_Cadena SKIP(0).
    W_Cadena = "@p66 " + TRIM(STRING(Creditos.Cuota,">>>,>>>,>>9")).
    PUT W_Cadena SKIP(0).
    /*
    W_Cadena = "@p67 " + TRIM(STRING(DAY(Creditos.Fec_pago))).
    PUT W_Cadena SKIP(0).
    W_Cadena = "@p68 " + TRIM(NomMes[MONTH(Creditos.Fec_pago)]).
    PUT W_Cadena SKIP(0).
    W_Cadena = "@p69 " + TRIM(STRING(YEAR(Creditos.Fec_pago))).
    PUT W_Cadena SKIP(0). */

    /* @p70 MENSUALES    - La forma de Pago- */
    IF creditos.per_pago EQ 1 THEN
       w_cadena = '@p70 Semanales'.
    ELSE
        IF creditos.per_pago EQ 2 THEN
           w_cadena = '@p70 Decadales'.
        ELSE
            IF creditos.per_pago EQ 3 THEN
               w_cadena = '@p70 Quincenales'.
            ELSE
                IF creditos.per_pago EQ 4 THEN
                   w_cadena = '@p70 Mensuales'.
                ELSE
                    IF creditos.per_pago EQ 5 THEN
                       w_cadena = '@p70 Bimestrales'.
                    ELSE
                        IF creditos.per_pago EQ 6 THEN
                           w_cadena = '@p70 Trimestrales'.
                        ELSE
                            IF creditos.per_pago EQ 7 THEN
                               w_cadena = '@p70 Cada cuatro meses'.
                            ELSE
                                IF creditos.per_pago EQ 8 THEN
                                   w_cadena = '@p70 Semestrales'.
                                ELSE
                                    IF creditos.per_pago EQ 9 THEN
                                       w_cadena = '@p70 Anuales'.
                                    ELSE
                                    DO:
                                        MESSAGE "No se encontro, definicion de los periodos de pago"
                                            VIEW-AS ALERT-BOX INFO BUTTONS OK.
                                        RETURN ERROR.
                                    END.
    PUT W_Cadena SKIP(0).
    W_Cadena = "@p71 " + TRIM(STRING(ROUND( Creditos.Tasa / 12,3),">>9.999")).
    PUT W_Cadena SKIP(0).
/* POR EL MOMENTO NO SE REQUIERE DE ESTAS FECHAS AUNQUE SE PUEDE IMPLEMENTAR LUEGO 
   CAMBIO SOLICITADO POR EL CONTRALOR EL 25 DE FEB DE 2005 */
/*    W_Cadena = "@p75 " + TRIM(STRING(DAY(Creditos.fec_desembolso))).
    PUT W_Cadena SKIP(0).
    W_Cadena = "@p76 " + NomMes[MONTH(Creditos.fec_desembolso)].
    PUT W_Cadena SKIP(0).
    W_Cadena = "@p77 " + TRIM(STRING(YEAR(Creditos.fec_desembolso))).
    PUT W_Cadena SKIP(0). */
        
        
    PUT "FinDeDocumento" SKIP(1).
    OUTPUT CLOSE.

    RUN imp_formato.
END PROCEDURE.

PROCEDURE imp_formato.
    FOR EACH SalidaImp:
        DELETE SalidaImp.
    END.
    DEFINE VARIABLE plano1 AS CHARACTER FORMAT "x(172)".
    DEFINE VARIABLE plano2 AS CHARACTER FORMAT "x(172)".

    IF TRIM(P_Formato) EQ 'PAGARE' THEN 
       /*INPUT FROM VALUE(SEARCH("\\172.28.1.201\d\sicobel\objetos\CPcx20050217.ps")).*/
       INPUT FROM VALUE(SEARCH("\\172.28.1.201\d\sicobel\objetos\CPcx20050311.ps")). /* CPcx20050225.ps */
    ELSE
       MESSAGE "No se encuentra plantilla - CPcx20050311.ps -"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
    REPEAT:
        CREATE SalidaImp.
        IMPORT UNFORMATTED plano1.
        ASSIGN SalidaImp.Detalle = TRIM(plano1).
      END.
    
    INPUT CLOSE.
    INPUT FROM VALUE(W_ArcSalida).
    REPEAT:
        CREATE SalidaImp.
        IMPORT UNFORMATTED plano2.
        ASSIGN SalidaImp.Detalle = TRIM(plano2).
      END.
    INPUT CLOSE.

    DEFINE VAR salida AS CHARACTER FORMAT "X(150)".
    salida = "\\172.28.1.201\d\tempo\pag" + TRIM(W_Usuario) + ".ps".
    OUTPUT TO VALUE(salida).
    FOR EACH salidaImp:
       PUT UNFORMATTED salidaImp.detalle AT 1.
    END.
    OUTPUT CLOSE.

    DEFINE VAR comando  AS CHARACTER FORMAT "X(150)".
    DEFINE VAR lineabat AS CHARACTER FORMAT "X(150)".
    comando = "\\172.28.1.201\d\tempo\imp" + TRIM(W_Usuario) + ".bat" .
    OUTPUT TO VALUE(comando).
    lineabat = "\\172.28.1.201\d\tempo\prfile32.exe /q " + TRIM(salida).
    PUT lineabat SKIP(0).
    PUT "EXIT"   SKIP(0).
    OUTPUT CLOSE.
    OS-COMMAND VALUE(comando).
END PROCEDURE.

/*******************************************************************************
                      FIN DE LA LIBRERIA DE FORMATOS
*******************************************************************************/
