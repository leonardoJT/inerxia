/*--------------------------------------------------------------------------
 Formatos globales:
 1.CDAT    2.NOTAMULTIPLE  3.TAC  4.LIBRANZA
---------------------------------------------------------------------------*/                  
{INCLUIDO\VARIABLE.I " shared"}
DEFINE INPUT  PARAMETER P_Formato  AS CHARACTER FORMAT "X(10)".
DEFINE INPUT  PARAMETER WNit      LIKE Clientes.Nit.
DEFINE INPUT  PARAMETER WNumpag   LIKE Creditos.num_Credito.

DEFINE VAR W_ArcSalida AS CHARACTER FORMAT "X(80)" INITIAL "".
DEFINE VAR W_CuotaPer  LIKE Creditos.Cuota.

DEFI TEMP-TABLE salidaImp
     FIELD detalle AS CHARACTER FORMAT "x(172)".

IF TRIM(P_Formato) EQ 'LIBRANZA' THEN
   RUN Lib_Pagare.


PROCEDURE Lib_pagare.
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
    DEFINE VAR W_NomDirAgenc  AS CHARACTER FORMAT "X(90)".
    DEFINE VAR W_NomDireccion AS CHARACTER FORMAT "X(90)".
    DEFINE VAR W_MontoLetras  AS CHARACTER FORMAT "X(90)".
    DEFINE VAR W_NomMes       AS CHARACTER FORMAT "X(12)".
    DEFINE VAR W_CiuAgencia   AS CHARACTER FORMAT "X(35)".
    DEFINE VAR W_MontoIzq     AS CHARACTER FORMAT "X(20)".
    DEFINE VAR W_NitEmpresa   LIKE Clientes.nit                 INITIAL ".".
    DEFINE VAR W_Tipo_idePag  LIKE Clientes.Tipo_Identificacion INITIAL ".".
    DEFINE VAR W_NitPag       LIKE Clientes.nit                 INITIAL ".". 
    
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
    DEFINE VAR W_desc_Per AS CHARACTER FORMAT "X(20)".
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

    FIND FIRST Creditos WHERE Creditos.Num_Credito EQ WNumpag AND
                        Creditos.Nit         EQ WNit NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Creditos THEN DO:
       MESSAGE "No se ha encontrado el crédito con Numero: " WNumpag SKIP
               "del Nit: " WNit  SKIP(1)
               "Se cancela la operación de desembolso" VIEW-AS ALERT-BOX ERROR  TITLE "Error pagare".
       RETURN ERROR.
    END.
    ELSE DO:
      IF Creditos.For_Pago EQ 2 THEN DO:
         FIND Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Empresas THEN DO:
            MESSAGE "Aunque el crédito es por nómina, el cliente" SKIP
                    "no se encuentra matriculado a ninguna empresa" SKIP(1)
                    "Se cancela la operación de desembolso" VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
         END.
         ELSE
             ASSIGN W_NitEmpresa   =  Empresas.Nit.


      END.
    END.
    RUN MontoEsc.r (INPUT Creditos.Monto,INPUT 0, OUTPUT W_MontoLetras).

    FIND Agencias WHERE Agencias.Agencia EQ W_Agencia AND Agencias.Estado EQ 1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Agencias THEN DO:
       MESSAGE "La Agencia para el desembolso no se encuentra" SKIP
               "disponible. se cancela la operación de desembolso" VIEW-AS ALERT-BOX.
    END.
    DEFINE VAR w_periodicidad LIKE Creditos.Per_Pago.
    w_periodicidad = Creditos.Per_Pago.

    ASSIGN W_NomAgencia   = Agencias.Nombre  + " - " + STRING(Agencias.Agencia,"999")
           W_NPagare      = Creditos.Num_Credito
           W_NomDirAgenc = TRIM(Agencias.direccion).

    FIND FIRST Ubicacion WHERE SUBSTR(Ubicacion.Ubicacion,1,5) EQ SUBSTR(Agencia.Ciudad,1,5) NO-LOCK NO-ERROR.
    IF AVAILABLE Ubicacion THEN
       W_CiuAgencia = Ubicacion.Nombre.
    ELSE
        W_CiuAgencia = ".".

    /* LIBRANZA */
    W_ArcSalida = "\\172.28.1.201\d\tempo\libranza" + TRIM(W_Usuario) + ".txt".
    OUTPUT TO VALUE(W_ArcSalida).    
    PUT "InIcIo" SKIP(1).
    W_Cadena = "@p01 " + TRIM(STRING(W_NPagare)).
    PUT W_cadena SKIP(0).
    W_Cadena = "@p02 " + TRIM(W_NomAgencia).
    PUT W_Cadena SKIP(0).
    W_Cadena = "@p03 " + TRIM(W_CiuAgencia) .
    PUT W_Cadena SKIP(0).
    W_Cadena = "@p05 " + TRIM(STRING(YEAR(W_Fecha))) + "       " + TRIM(STRING(MONTH(W_Fecha)))    + "    " + TRIM(STRING(DAY(W_Fecha))).
    PUT W_Cadena SKIP(0).
    W_Cadena = "@p06 " + "($" + TRIM(STRING(Creditos.Monto,">>>,>>>,>>9")) + ")".
    PUT W_Cadena SKIP(0).
    W_Cadena = "@p07 " + TRIM(STRING(Creditos.Tasa,">>9.999")) + "AV".
    PUT W_Cadena SKIP(0).
    W_Cadena = "@p08 " + TRIM(STRING(Creditos.Plazo,">,>>9")). /*antes decia Meses - 26 mayo 2005 jjmp */
    PUT W_Cadena SKIP(0).
    

    /* IF Creditos.FOR_Pago EQ 2 THEN DO:*/
        W_MontoIzq = TRIM(STRING(Creditos.Monto,">>>,>>>,>>>,>>9")).
        W_Puntero = ROWID(Clientes).
        W_NomPagadoR = ".".
        W_NomEmpresa = ".".
        FIND empresas WHERE empresas.cod_empresa EQ clientes.cod_empresa NO-LOCK NO-ERROR.
        IF AVAILABLE(empresas) THEN
        DO:
          FIND Clientes WHERE Clientes.Nit EQ Empresas.Nit_Pagador NO-LOCK NO-ERROR.
          IF AVAILABLE Clientes THEN
             ASSIGN W_NomPagador  = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
                    W_NitEmpresa  = Empresas.Nit     
                    W_Tipo_idePag = Clientes.Tipo_Identificacion
                    W_NitPag      = Empresas.Nit_Pagador.
          FIND Clientes WHERE Clientes.Nit EQ Empresas.Nit NO-LOCK NO-ERROR.
          IF AVAILABLE Clientes THEN
             ASSIGN W_NomEmpresa  = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
                    W_NomEmpresa2 = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
        END.

        FIND Clientes WHERE Clientes.Nit EQ Empresas.Nit_Pagador NO-LOCK NO-ERROR.
        W_NomPagadoR = "".
        IF AVAILABLE Clientes THEN
           ASSIGN W_NomPagador = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
                 /* W_Periodicidad = Empresas.For_Pago.*/
        ELSE
           W_NomPagador = '.'.
        FIND Clientes WHERE Clientes.Nit EQ Empresas.Nit NO-LOCK NO-ERROR.
        W_NomEmpresa = "".
        IF AVAILABLE Clientes THEN
           ASSIGN W_NomEmpresa  = SUBSTR(TRIM(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2),1,38)
                  W_NomEmpresa2 = SUBSTR(TRIM(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2),1,38).
        ELSE
           ASSIGN W_NomEmpresa  = ".".

        FIND Clientes WHERE ROWID(Clientes) EQ W_Puntero NO-LOCK NO-ERROR.
        ASSIGN W_NomDeudor  = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
               W_NomDeudor2 = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
        
/* -   @p14 para el NIT de la Empresa
-      @p15 para el tipo de documento el pagador o jefe nomina
-      @p16 para el No. Del documento.
*/

        CASE W_Periodicidad:
            WHEN 1 THEN
                W_Desc_Per = "SEMANAL".
            WHEN 2 THEN
                W_Desc_Per = "DECADAL".
            WHEN 3 THEN
                W_Desc_Per = "QUINCENAL".
            WHEN 4 THEN
                W_Desc_Per = "MENSUAL".
            WHEN 5 THEN
                W_Desc_Per = "BIMESTRAL".
            WHEN 6 THEN
                W_Desc_Per = "TRIMESTRAL".
            WHEN 7 THEN
                W_Desc_Per = "CUATRIMESTRAL".
            WHEN 8 THEN
                W_Desc_Per = "SEMESTRAL".
            WHEN 9 THEN
                W_Desc_Per = "ANUAL".
            OTHERWISE 
                W_Desc_Per = "NO DEFINIDO".
        END CASE.

        W_Cadena = "@p09 " + TRIM(W_NomPagador).
        PUT W_Cadena SKIP(0).
        W_Cadena = "@p10 " + TRIM(W_NomEmpresa).
        PUT W_Cadena SKIP(0).
        W_Cadena = "@p11 " + TRIM(W_NomDeudor).
        PUT W_Cadena SKIP(0).
        W_Cadena = "@p12 " + Clientes.tipo_ide.
        PUT W_Cadena SKIP(0).
        W_Cadena = "@p13 " + TRIM(Clientes.nit).
        PUT W_Cadena SKIP(0).   
        W_Cadena = "@p14 " + TRIM(W_NitEmpresa).  /*  Nuevo formato al 27 de abril de 2005 */
        PUT W_Cadena SKIP(0).   
        W_Cadena = "@p15 " + TRIM(W_Tipo_IdePag). /*  Nuevo formato al 27 de abril de 2005 */
        PUT W_Cadena SKIP(0).   
        W_Cadena = "@p16 " + TRIM(W_NitPag).      /*  Nuevo formato al 27 de abril de 2005 */
        PUT W_Cadena SKIP(0).   
        W_Cadena = "@p20 " + TRIM(W_NomDeudor).
        PUT W_Cadena SKIP(0).
        W_Cadena = "@p21 " + TRIM(W_NomEmpresa).
        PUT W_Cadena SKIP(0).
        W_Cadena = "@co01 CREDITO".
        PUT W_Cadena SKIP(0).
        W_Cadena = "@co02 " + STRING(Creditos.Num_Credito).
        PUT W_Cadena SKIP(0).
        W_CuotaPer = Creditos.Cuota.
        W_Cadena = "@co03 " + TRIM(STRING(W_CuotaPer,">>>,>>>,>>9")).
        PUT W_Cadena SKIP(0).
        W_Cadena = "@co04 " + trim(STRING(Creditos.plazo, ">>9" )).
        PUT W_Cadena SKIP(0).
        W_Cadena = "@co05 " + w_desc_per.
        PUT W_Cadena SKIP(0).

        W_Cadena = "@p30 .".
        PUT W_Cadena SKIP(0).
        W_Cadena = "@p31 " + TRIM(STRING(W_CuotaPer,">>>,>>>,>>9")).
        PUT W_Cadena SKIP(0).
        W_Cadena = "@p32 .".
        PUT W_Cadena SKIP(0).
        W_Cadena = "@p33 .".
        PUT W_Cadena SKIP(0).

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

    IF TRIM(P_Formato) EQ 'LIBRANZA' THEN 
        INPUT FROM VALUE(SEARCH("\\172.28.1.201\d\sicobel\objetos\COcx20050607.ps")). /* COcx20050427.ps */

    /*INPUT FROM VALUE(SEARCH("CDAT20041118.ps")). */
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
    salida = "\\172.28.1.201\d\tempo\lib" + TRIM(W_Usuario) + ".ps".
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
