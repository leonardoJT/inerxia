DEFINE INPUT PARAMETER P_Formato AS CHARACTER.
DEFINE INPUT PARAMETER WNit AS CHARACTER.
DEFINE INPUT PARAMETER WNumpag AS INTEGER.

{INCLUIDO\VARIABLE.I " shared"}

DEFINE VAR W_ArcSalida AS CHARACTER FORMAT "X(80)".

DEFINE TEMP-TABLE salidaImp
    FIELD detalle AS CHARACTER FORMAT "X(172)".

IF TRIM(P_Formato) = 'PAGARE' THEN
    RUN Pagare.

PROCEDURE pagare:
    DEFINE VAR W_Cadena AS CHARACTER FORMAT "X(90)".
    DEFINE VAR W_NomAgencia AS CHARACTER FORMAT "X(45)".
    DEFINE VAR W_MontoLetras AS CHARACTER FORMAT "X(90)".
    DEFINE VAR W_CiuAgencia AS CHARACTER FORMAT "X(35)".
    DEFINE VAR W_NPagare AS INTEGER.

    FIND FIRST Clientes WHERE Clientes.Nit = WNit NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Clientes THEN DO:
        MESSAGE "No se encontró el cliente:" Clientes.nit
            VIEW-AS ALERT-BOX ERROR TITLE "Error pagaré".

        RETURN ERROR.
    END.

    FIND FIRST Creditos WHERE Creditos.Num_credito = WNumpag
                          AND Creditos.Nit = WNit NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Creditos THEN DO:
        MESSAGE "No se ha encontrado el crédito con Numero:" WNumpag SKIP
                "del titular:" WNit  SKIP(1)
                "Se cancela la operación de desembolso"
            VIEW-AS ALERT-BOX ERROR  TITLE "Error pagaré".
        
        RETURN ERROR.
    END.

    /* oakley */

    
    RUN MontoEsc.r (INPUT Creditos.Monto,INPUT 0, OUTPUT W_MontoLetras).

    FIND Agencias WHERE Agencias.Agencia EQ W_Agencia AND Agencias.Estado EQ 1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Agencias THEN DO:
       MESSAGE "La Agencia para el desembolso no se encuentra" SKIP
               "disponible. se cancela la operación de desembolso" VIEW-AS ALERT-BOX.
    END.


    ASSIGN W_NomAgencia   = Agencias.Nombre  + " - " + STRING(Agencias.Agencia,"999")
           W_NPagare      = Creditos.Num_Credito.

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
