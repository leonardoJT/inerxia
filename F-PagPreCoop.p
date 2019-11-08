DEFINE INPUT PARAMETER WNit LIKE Clientes.Nit.
DEFINE INPUT PARAMETER WNumCre LIKE Creditos.Num_Credito.
DEFINE INPUT PARAMETER WFche AS DATE.

DEFINE TEMP-TABLE Ttexto
    FIELD Tlinea AS CHARACTER FORMAT "X(110)".

{INCLUIDO\VARIABLE.I "SHARED"}

DEFINE VAR NomMes AS CHARACTER FORMAT "X(12)" EXTENT 12 INITIAL ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"].

DEFI VAR W_FecVcto LIKE Creditos.Fec_Desembolso.
DEFINE VAR W_NomPagador AS CHARACTER FORMAT "X(45)".
DEFINE VAR W_NitPagador AS INTEGER FORMAT "999999999999".
DEFINE VAR W_MontoLetras AS CHARACTER FORMAT "X(90)".

/* oakley */

DEFINE VAR W_LetrasDias   AS CHARACTER FORMAT "X(90)".

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
DEFINE VARIABLE W_LineaCredito AS CHARACTER   NO-UNDO.
DEFINE VARIABLE W_CuotaLetras AS CHARACTER   NO-UNDO.
DEFINE VARIABLE W_Periodo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE W_Periodo2 AS CHARACTER   NO-UNDO.  
DEFINE VARIABLE W_Periodo3 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE W_Tasa AS DECIMAL     NO-UNDO.
    

/*FOR EACH Ahorros WHERE
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
          TOT = TOT + Creditos.
          Cuota.
END. */

FIND Clientes WHERE Clientes.Nit EQ WNit NO-LOCK NO-ERROR.
IF NOT AVAILABLE Clientes THEN DO:
   MESSAGE "No se encontró el cliente" VIEW-AS ALERT-BOX ERROR TITLE "Error pagare".
   RETURN ERROR.
END.

/********************************* William ****************************************/
ASSIGN 
        W_NomPagador = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2           /* Captura el nombre del cliente */
        W_NitPagador = integer(WNit).
  

FIND Creditos WHERE Creditos.Num_Credito EQ WNumCre AND
                    Creditos.Nit         EQ WNit NO-LOCK NO-ERROR.
IF NOT AVAILABLE Creditos THEN DO:
   MESSAGE "No se ha Encontrado el Crédito al cual se le" SKIP
           "imprimirá el pagare." SKIP(1)
           "Se Cancela la Operación." VIEW-AS ALERT-BOX ERROR  TITLE "Error pagare".
   RETURN ERROR.
END.
ELSE DO:
  IF Creditos.For_Pago EQ 2 THEN DO:
     /* RUN MontoEsc.r (INPUT DAY(WFche),INPUT 0, OUTPUT W_LetrasDIAS). */
     RUN MontoEsc.r (INPUT DAY(W_fecha),INPUT 0, OUTPUT W_LetrasDIAS).
     W_LetrasDias = TRIM(W_LetrasDias) + " (" + TRIM(STRING( DAY(W_fecha),">9")) + ") ".

     FIND Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
     IF NOT AVAILABLE Empresas THEN DO:
        MESSAGE "Aunque el crédito es por nómina, el cliente" SKIP
                "no se encuentra matriculado a ninguna empresa" 
             VIEW-AS ALERT-BOX TITLE "INFORMATIVO".
        /*RETURN ERROR.*/
     END.
  END.
END.


W_FecVcto = WFche + (Creditos.Plazo * 30).
IF Creditos.Per_Pago EQ 3 THEN
   W_FecVcto = WFche + (Creditos.Plazo * 15).

    
/*********************** William - Halla codigo producto************************/
FIND FIRST Pro_Creditos WHERE creditos.Cod_Credito = Pro_Creditos.Cod_Credito.
W_LineaCredito = Pro_Creditos.Nom_Producto.
    
RUN MontoEsc.r (INPUT Creditos.Monto,INPUT 0, OUTPUT W_MontoLetras).
RUN MontoEsc.r (INPUT Creditos.cuota,INPUT 0, OUTPUT W_CuotaLetras).

CASE creditos.Per_Pago:
    WHEN 1 THEN DO:
        ASSIGN W_Periodo = "Semanales".
        ASSIGN W_Periodo2 = "Semana".
        ASSIGN W_Periodo3 = "Semanal".
        ASSIGN W_Tasa = creditos.tasa / 52.
    END.
    WHEN 2 THEN DO: 
        ASSIGN W_Periodo = "Decadales".
        ASSIGN W_Periodo2 = "Decada".
        ASSIGN W_Periodo3 = "Decadal".
        ASSIGN W_Tasa = creditos.tasa / 36.
    END.
    WHEN 3 THEN DO: 
        ASSIGN W_Periodo = "Quincenales".
        ASSIGN W_Periodo2 = "Quincena".
        ASSIGN W_Periodo3 = "Quincenal".
        ASSIGN W_Tasa = creditos.tasa / 24.
    END.
    WHEN 4 THEN DO: 
        ASSIGN W_Periodo = "Mensuales".
        ASSIGN W_Periodo2 = "Mes".
        ASSIGN W_Periodo3 = "Mensual".
        ASSIGN W_Tasa = creditos.tasa / 12.
    END.
    WHEN 5 THEN DO: 
        ASSIGN W_Periodo = "Bimestrales".
        ASSIGN W_Periodo2 = "Bimestre".
        ASSIGN W_Periodo3 = "Bimestral".
        ASSIGN W_Tasa = creditos.tasa / 6.
    END.
    WHEN 6 THEN DO: 
        ASSIGN W_Periodo = "Trimestrales".
        ASSIGN W_Periodo2 = "Trimestre".
        ASSIGN W_Periodo3 = "Trimestral".
        ASSIGN W_Tasa = creditos.tasa / 4.
    END.
    WHEN 7 THEN DO: 
        ASSIGN W_Periodo = "Cuatrimestrales".
        ASSIGN W_Periodo2 = "Cuatrimestre".
        ASSIGN W_Periodo3 = "Cuatrimestral".
        ASSIGN W_Tasa = creditos.tasa / 3.
    END.
    WHEN 8 THEN DO: 
        ASSIGN W_Periodo = "Semestrales".
        ASSIGN W_Periodo2 = "Semestre".
        ASSIGN W_Periodo3 = "Semestral".
        ASSIGN W_Tasa = creditos.tasa / 2.
    END.
    WHEN 9 THEN DO: 
        ASSIGN W_Periodo = "Anuales".
        ASSIGN W_Periodo2 = "Año".
        ASSIGN W_Periodo3 = "Anual".
        ASSIGN W_Tasa = creditos.tasa.
    END.
END CASE.


/* W_MontoLetras = W_MontoLetras + "  ($" + STRING(Creditos.Monto,">>>,>>>,>>>,>>9") + ")". */      /* No agrega el valor */

FIND FIRST Agencias WHERE Agencias.Agencia EQ Creditos.Agencia AND Agencias.Estado NE 3 NO-LOCK NO-ERROR.

DEFINE VAR Listado AS CHARACTER INITIAL "".
listado = W_PathSpl + "L_Pagare.Lst".
{incluido\Imprim90.I "listado"}
/* {incluido\Imprimir.I "listado"} */

PROCEDURE ProcesoImprimir:
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "                          " + STRING(Creditos.Num_Credito, "9999999").
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "                                " + caps(W_LineaCredito).                             /* Linea Credito */
 CREATE Ttexto. tlinea = "                                " + string(W_FecVcto, "99/99/9999").                  /* Fecha Vencimiento */
 CREATE Ttexto. tlinea = "                                $" + TRIM(STRING(creditos.monto,">>>,>>>,>>>,>>9")).  /* Valor */
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "       ".
 CREATE Ttexto. tlinea = "        " + W_NomPagador.  /* Nombre Asociado*/
 /*CREATE Ttexto. tlinea = W_MontoLetras.*/
 /*CREATE Ttexto. tlinea = "A cancelarse totalmente en la Fecha " + STRING (W_FecVcto,"99/99/9999") + " En las Oficinas de la COOPERATIVA*********". */
 CREATE Ttexto. tlinea = "                                      " + string(W_NitPagador, "999999999999") + "                     " + caps(Clientes.Lugar_expedicion).
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "                                                          ".
 CREATE Ttexto. tlinea = "                                                          ". /* Inicia Cantidad */
 CREATE Ttexto. tlinea = "        " + caps(W_MontoLetras).
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "         $" + TRIM(STRING(creditos.monto,">>>,>>>,>>>,>>9")).
 CREATE Ttexto. tlinea = "              " + string(W_Tasa, ">9.99") + " " + caps(W_Periodo3).
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "                                                    " + string(creditos.Plazo) + " C. " + STRING(CAPS(W_Periodo)). /* Periodicidad */
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "        " + caps(W_CuotaLetras).
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "          $" + trim(STRING(creditos.cuota, ">>>,>>>,>>9")).
 CREATE Ttexto. tlinea = "                                 " + STRING(WFche, "99/99/9999") + "                                   " + caps(W_Periodo2).
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "            " + STRING(DAY(TODAY)) + "                   " + trim(CAPS(nommes[MONTH(TODAY)])) + "                    " + STRING(YEAR(TODAY)).
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "". 
 CREATE Ttexto. tlinea = "". 
 CREATE Ttexto. tlinea = "".
  /*CREATE Ttexto. tlinea = "Impresión Pagaré Reverso...".*/
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "                                                                                                        FCR-17.01".
  
  FOR EACH Ttexto:
    IF Ttexto.Tlinea NE "Impresión Pagaré Reverso..." THEN
      PUT "     " Ttexto.Tlinea SKIP(0).
    ELSE DO: 
     /*MESSAGE "Impresión Pagaré Reverso..." VIEW-AS ALERT-BOX INFO BUTTONS OK. */
       PAGE.
    END.
  END.
  PAGE.

END PROCEDURE.
