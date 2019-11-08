/*PROGRAMA QUE GENERA LA LIBRANZA GENERAL DE CREDITOS*/
DEFINE INPUT PARAMETER WNit LIKE Creditos.Nit.
DEFINE INPUT PARAMETER WNcre LIKE Creditos.Num_credito.
DEFINE INPUT PARAMETER WInd AS INTEGER FORMAT "9".
DEFINE INPUT PARAMETER WFche AS DATE.
DEFINE VAR w_per AS CHARACTER FORMAT "X(20)".

DEFINE TEMP-TABLE Ttexto
    FIELD Tlinea AS CHARACTER FORMAT "X(110)".

{INCLUIDO\VARIABLE.I "SHARED"}

DEFINE VAR NomMes AS CHARACTER FORMAT "X(12)" EXTENT 12 INITIAL ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"].

DEFINE VAR W_Puntero AS ROWID.
DEFINE VAR W_NomPagador AS CHARACTER FORMAT "X(45)".
DEFINE VAR W_NomEmpresa AS CHARACTER FORMAT "X(45)".
DEFINE VAR W_NomEmpresa2 AS CHARACTER FORMAT "X(45)".
DEFINE VAR W_NomDeudor AS CHARACTER FORMAT "X(45)".
DEFINE VAR W_LetrasMonto AS CHARACTER FORMAT "X(90)".
DEFINE VAR W_LetrasCuota AS CHARACTER FORMAT "X(90)".
DEFINE VAR W_LetrasPlazo AS CHARACTER FORMAT "X(90)".
DEFINE VAR W_LetrasDias AS CHARACTER FORMAT "X(90)".
DEFINE VAR W_LetrasTotPro AS CHARACTER FORMAT "X(90)".
DEFINE VAR W_LetrasExtra AS CHARACTER FORMAT "X(120)".

/* oakley */


DEFINE VAR W_NomMes        AS CHARACTER FORMAT "X(12)".
DEFINE VAR W_CiuAgencia    AS CHARACTER FORMAT "X(35)".
DEFINE VAR W_MontoIzq      AS CHARACTER FORMAT "X(20)".
DEFINE VAR wnomfecha       AS CHARACTER FORMAT "X(30)".
DEFINE VAR wcuotasExtra    AS INTEGER INITIAL 0.

DEFINE VAR i           AS INTEGER INITIAL 0.
DEFINE VAR wnomCod1    AS CHARACTER FORMAT "X(40)" INITIAL "".
DEFINE VAR wnomCod2    AS CHARACTER FORMAT "X(40)" INITIAL "".
DEFINE VAR wnomCod3    AS CHARACTER FORMAT "X(40)" INITIAL "".
DEFINE VAR wnomCod4    AS CHARACTER FORMAT "X(40)" INITIAL "".
DEFINE VAR wconta      AS INTEGER INITIAL 0.
DEFINE VAR wvalorfinal AS DECIMAL INITIAL 0.
DEFINE VAR wnomlinea   AS CHARACTER FORMAT "X(80)" INITIAL "".
DEFINE VAR wvlrExtras  AS DECIMAL INITIAL 0.
DEFINE VAR wmesesExt   AS CHARACTER FORMAT "X(120)".


FIND Clientes WHERE Clientes.Nit EQ WNit NO-LOCK NO-ERROR.
IF NOT AVAILABLE Clientes THEN DO:
   MESSAGE "No se encontró el cliente" VIEW-AS ALERT-BOX ERROR TITLE "Error pagare".
   RETURN ERROR.
END.


FIND creditos WHERE creditos.Num_Credito   EQ WNcre AND
                    creditos.Nit           EQ WNit  NO-LOCK NO-ERROR.
IF NOT AVAILABLE creditos THEN DO:
   MESSAGE "No se ha encontrado el Credito al cual se le" SKIP
           "imprimirá La Libranza." SKIP(1)
           "#Credito : " Wncre  "  C.C. " Wnit SKIP(1)
           "Se cancela la operación de Solicitud" VIEW-AS ALERT-BOX ERROR  TITLE "Error pagare".
   RETURN ERROR.
END.
ELSE DO:
  FIND pro_creditos WHERE pro_creditos.cod_credito = creditos.cod_Credito NO-LOCK NO-ERROR.
  IF AVAILABLE(creditos) THEN wnomlinea = pro_creditos.nom_producto.
  IF Creditos.For_Pago EQ 2 THEN DO:
     FIND Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
     IF NOT AVAILABLE Empresas THEN DO:
        MESSAGE "Aunque el crédito es por nómina, el cliente" SKIP
                "no se encuentra matriculado a ninguna empresa" SKIP(1)
                VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
     END.
  END.
END.

/* nuevo */
FOR EACH relaciones WHERE relaciones.Nit EQ  wNit  AND 
    TRIM(relaciones.cuenta)   EQ TRIM(string(Creditos.num_credito)) AND
    relaciones.estado         EQ 1   AND       
    relaciones.COD_relacion   EQ 11  AND
    relaciones.clase_producto EQ 2   NO-LOCK:
    FOR EACH clientes WHERE clientes.nit EQ relaciones.nit_relacion NO-LOCK:
      WConta  = Wconta + 1.
      CASE wconta:
          WHEN 1 THEN WnomCod1 = CAPS(TRIM(clientes.nombre)) + ' ' + CAPS(TRIM(clientes.apellido1)) + ' ' + CAPS(TRIM(clientes.apellido2)) + " " + Clientes.Tipo_Identificacion + " " + TRIM(clientes.nit).
          WHEN 2 THEN WnomCod2 = CAPS(TRIM(clientes.nombre)) + ' ' + CAPS(TRIM(clientes.apellido1)) + ' ' + CAPS(TRIM(clientes.apellido2)) + " " + Clientes.Tipo_Identificacion + " " + TRIM(clientes.nit).
          WHEN 3 THEN WnomCod3 = CAPS(TRIM(clientes.nombre)) + ' ' + CAPS(TRIM(clientes.apellido1)) + ' ' + CAPS(TRIM(clientes.apellido2)) + " " + Clientes.Tipo_Identificacion + " " + TRIM(clientes.nit).
          WHEN 4 THEN WnomCod4 = CAPS(TRIM(clientes.nombre)) + ' ' + CAPS(TRIM(clientes.apellido1)) + ' ' + CAPS(TRIM(clientes.apellido2)) + " " + Clientes.Tipo_Identificacion + " " + TRIM(clientes.nit).
      END CASE.
      /* WnomCod[wconta] = CAPS(TRIM(clientes.nombre)) + ' ' + CAPS(TRIM(clientes.apellido1)) + ' ' + CAPS(TRIM(clientes.apellido2)) + " " + Clientes.Tipo_Identificacion + " " + TRIM(clientes.nit). */
    END.
END.  
FIND Clientes WHERE Clientes.Nit EQ WNit NO-LOCK NO-ERROR.

FIND FIRST Ubicacion WHERE SUBSTR(Ubicacion.Ubicacion,1,5) EQ SUBSTR(Clientes.lugar_comercial,1,5) NO-LOCK NO-ERROR.
IF AVAILABLE Ubicacion THEN
   W_CiuAgencia = Ubicacion.Nombre.
ELSE
    W_CiuAgencia = ".".

FOR EACH extras WHERE extras.agencia       = creditos.agencia       AND 
                      extras.nit           = creditos.nit           AND
                      extras.cod_credito   = creditos.cod_credito   AND 
                      extras.num_solicitud = creditos.num_solicitud AND 
                      extras.estado = 1 NO-LOCK:
   ASSIGN wvlrextras = wvlrextras + extras.vr_cuoExtra
          wcuotasExtra = wcuotasExtra + 1.
   wmesesExt  = wmesesExt  + (NomMes[MONTH(fec_vcto)]) + "/" + STRING(YEAR(fec_vcto),"9999")+ "  ".
END.

/* termina */
RUN MontoEsc.r (INPUT Creditos.Monto,INPUT 0, OUTPUT W_LetrasMonto).
W_LetrasMonto = TRIM(W_LetrasMonto) + " ($" + TRIM(STRING(Creditos.Monto,">>>,>>>,>>>,>>9")) + ")  M/L".
RUN MontoEsc.r (INPUT Creditos.cuota,INPUT 0, OUTPUT W_LetrasCuota).
W_LetrasCuota = TRIM(W_LetrasCuota) + " ($" + TRIM(STRING(Creditos.Cuota,">>>,>>>,>>>,>>9")) + ")  M/L".
RUN MontoEsc.r (INPUT Creditos.plazo,INPUT 0, OUTPUT W_LetrasPlazo).
W_LetrasPlazo = TRIM(W_LetrasPlazo) + " (" + TRIM(STRING(Creditos.Plazo,">>>9")) + ") ".
/* RUN MontoEsc.r (INPUT DAY(WFche),INPUT 0, OUTPUT W_LetrasDias). */
RUN MontoEsc.r (INPUT DAY(W_Fecha),INPUT 0, OUTPUT W_LetrasDias).
W_LetrasDias = TRIM(W_LetrasDias) + " (" + TRIM(STRING( DAY(w_fecha),">9")) + ") ".
RUN MontoEsc.r (INPUT ROUND(Creditos.cuota * Creditos.plazo,0),INPUT 0, OUTPUT W_LetrasTotPro).
W_LetrasTotPro = TRIM(W_LetrasTotPro) + " (" + TRIM(STRING( ROUND(Creditos.cuota * Creditos.plazo,0) ,">>>,>>>,>>>,>>9")) + ")  M/L" .
RUN MontoEsc.r (INPUT wvlrExtras,INPUT 0, OUTPUT W_LetrasExtra).
W_LetrasExtra = TRIM(W_LetrasExtra) + " (" + TRIM(STRING( wvlrExtras ,">>>,>>>,>>>,>>9")) + ")  M/L" .

CASE Creditos.per_pago:
    WHEN 1 THEN w_per = "Semanal".
    WHEN 2 THEN w_per = "Decadal".
    WHEN 3 THEN w_per = "Quincenal".
    WHEN 4 THEN w_per = "Mensual".
    WHEN 5 THEN w_per = "Bimestral".
    WHEN 6 THEN w_per = "Trimestral".
    WHEN 7 THEN w_per = "Cuatrimestral".
    WHEN 8 THEN w_per = "Semestral ".
    WHEN 9 THEN w_per = "Anual".
    OTHERWISE 
        DO:
           MESSAGE "No se encontro, definicion de los periodos de pago" VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN ERROR.
        END.        
END CASE.


IF WInd EQ 1 THEN DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  listado = W_PathSpl + "L_Pagare_" + w_usuario + STRING(RANDOM(2000,10000)) + ".Lst".
  {incluido\Imprim90.I "listado"}
END.
ELSE DO:
  RUN ProcesoImprimir.
END.
 

FIND Agencias WHERE Agencias.Agencia EQ W_Agencia AND Agencias.Estado EQ 1 NO-LOCK NO-ERROR.
IF NOT AVAILABLE Agencias THEN DO:
   MESSAGE "La Agencia para el desembolso no se encuentra" SKIP
           "disponible. se cancela la operación de desembolso" VIEW-AS ALERT-BOX.
   RETURN ERROR.
END.

PROCEDURE ProcesoImprimir:
    ASSIGN W_MontoIzq = STRING(Creditos.Monto,">>>,>>>,>>>,>>9")
           W_Puntero = ROWID(Clientes).
    FIND Clientes WHERE Clientes.Nit EQ Empresas.Nit_Pagador NO-LOCK NO-ERROR.
    W_NomPagadoR = "".
    IF AVAILABLE Clientes THEN
       W_NomPagador = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
    FIND Clientes WHERE Clientes.Nit EQ Empresas.Nit NO-LOCK NO-ERROR.
    W_NomEmpresa = "".
    DEFINE VAR W_MontoLetras  AS CHARACTER FORMAT "X(90)".
    IF AVAILABLE Clientes THEN
       ASSIGN W_NomEmpresa  = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
              W_NomEmpresa2 = TRIM(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2).
    FIND Clientes WHERE ROWID(Clientes) EQ W_Puntero NO-LOCK NO-ERROR.
    ASSIGN W_NomDeudor = trim(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + Clientes.Tipo_Identificacion + " " + TRIM(clientes.nit)
    wnomfecha = STRING(DAY(WFche))  + " de "  + nommes[month(WFche)] + " de " + STRING(YEAR(WFche)).
    /* DISPLAY SKIP(1) */
    CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = " ". CREATE Ttexto. tlinea = " ". 
    CREATE Ttexto. tlinea = "LIBRANZA - RETENCIÓN No. " + TRIM(string(NEXT-VALUE(Sec_Libranza),"9999999999")) + "  CIUDAD :  " + TRIM(W_CiuAgencia).
    CREATE Ttexto. tlinea = "FECHA EN QUE SE LIBRA   :  " + STRING(w_fecha,"99/99/9999")  + "  NRO SOLIC : " + STRING(Creditos.Num_Solicitud,"9999999999").
    CREATE Ttexto. tlinea = "EMPRESA                 :  " + SUBSTRING(W_NomEmpresa2,1,37).
    /* CREATE Ttexto. tlinea = "         PAGADOR O JEFE DE NOMINA:  " + TRIM(W_NomPagador). */
    CREATE Ttexto. tlinea = "NOMBRES Y APELLIDOS     :  " + TRIM(W_NomDeudor) +  "*". 
    CREATE Ttexto. tlinea = "LINEA DE CRÉDITO        :  " + TRIM(wnomlinea) +  "  TASA ANUAL : " + STRING(ROUND(Creditos.Tasa,1),"ZZ9.9") + "NMV".
    CREATE Ttexto. tlinea = "VALOR DEL CRÉDITO      $:  " + TRIM(W_Letrasmonto).
    CREATE Ttexto. tlinea = "". 
    CREATE Ttexto. tlinea = "Yo (Nosotros) "    + W_NomDeudor. 
    /*CREATE Ttexto. tlinea = "". */

                   /*       1         2         3         4         5         6         7         8         9  
                             123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
    /*CREATE Ttexto. tlinea = "".*/
    IF wnomcod1 GT "" THEN CREATE Ttexto. tlinea = "         " + wnomcod1. CREATE Ttexto. tlinea = "".
    IF wnomcod2 GT "" THEN CREATE Ttexto. tlinea = "         " + wnomcod2. CREATE Ttexto. tlinea = "".
    IF wnomcod3 GT "" THEN CREATE Ttexto. tlinea = "         " + wnomcod3. CREATE Ttexto. tlinea = "".
    IF wnomcod4 GT "" THEN CREATE Ttexto. tlinea = "         " + wnomcod4.
    CREATE Ttexto. Tlinea = "".                                                                                  
    CREATE Ttexto. tlinea = "Por  medio  del  presente   documento  autorizo(amos)  irrevocablemente  a   la    pagaduría  de   la:".
    CREATE Ttexto. tlinea = SUBSTRING(W_NomEmpresa2,1,37) + "* para la cual laboro(amos), retener y pagar a favor de la Cooperativa".
    CREATE Ttexto. tlinea = "JURISCOOP, en forma " + TRIM(w_per) +  " de mi salario, la cantidad de " + TRIM(REPLACE(W_LetrasPlazo,'Pesos ML','')) + " cuotas, cada una por valor de ".
    CREATE Ttexto. tlinea = trim(w_letrasCuota) + " hasta completar".
    CREATE Ttexto. tlinea = "la suma de " + TRIM(W_LetrasTotPro). 
    CREATE Ttexto. tlinea = "la primera cuota  deberá ser descontada  en  la fecha " + trim(wnomfecha) + " y girada a JURISCOOP, a mas tardar".
    CREATE Ttexto. tlinea = "dentro  de   los   diez (10) días  siguientes   al descuento  y  así sucesivamente   cada uno  de  los meses".
    IF wcuotasExtra GT 0 THEN DO:
      CREATE Ttexto. tlinea = "siguientes    hasta   completar la totalidad de las cuotas. De igual forma  autorizo(amos) el descuento   de".
      CREATE Ttexto. tlinea = "(" + STRING(wcuotasExtra)  + ") cuotas extraordinarias, por valor total de " + TRIM(W_LetrasExtra).
      CREATE Ttexto. tlinea = ", en los meses de: " + TRIM(wmesesExt) + ".".                                                      
    END.
    ELSE DO:
      CREATE Ttexto. tlinea = "siguientes    hasta   completar la totalidad de las cuotas.".
    END.
    CREATE Ttexto. tlinea = "".
    CREATE Ttexto. tlinea = "Así mismo autorizo (amos) el descuento de las cuotas correspondientes, durante el tiempo que permanezca(mos)".
    CREATE Ttexto. tlinea = "en vacaciones,  licencias  e  incapacidades,  por el  valor  de las cuotas que  deban pagarse  a  JURISCOOP.".
    CREATE Ttexto. tlinea = "Igualmente, autorizo (amos) para  que  en el evento  en  que  se modifiquen las condiciones del crédito, los".
    CREATE Ttexto. tlinea = "descuentos mensuales sean los que  JURISCOOP  notifique  a  la  pagaduría. En caso  de  mora  o de darse por".
    CREATE Ttexto. tlinea = "terminado   mi (nuestro)  contrato de  trabajo  o  relación  laboral con la entidad  del deudor o codeudores".
    CREATE Ttexto. tlinea = "solidarios del  préstamo que garantiza  esta libranza, autorizo (amos) para que las cuotas restantes, que se".
    CREATE Ttexto. tlinea = "harán exigibles de manera inmediata y sin que sea necesario requerimiento de ninguna índole al cual renuncio".
    CREATE Ttexto. tlinea = "(amos) de manera expresa, intereses o cualquier gasto que  se  ocasione,  sea descontado de las prestaciones".
    CREATE Ttexto. tlinea = "sociales,  salarios,  indemnizaciones  o  cualquier otra acreencia laboral a  que tengo (amos) derecho.  Doy".
    CREATE Ttexto. tlinea = "(damos)  pleno  poder a  JURISCOOP,  para  que en  nuestro  nombre  y  representación y con  las más amplias".
    CREATE Ttexto. tlinea = "facultades tramite todo lo referente al reconocimiento y pago  de dichas obligaciones, y reciba, con cargo a".
    CREATE Ttexto. tlinea = "mis (nuestras) deudas, tales dineros. En caso que mi (nuestra) obligación presente más de una cuota  vencida".
    CREATE Ttexto. tlinea = "autorizo (amos) el envío de la novedad  para descontar como  valor adicional, hasta  quedar la obligación al".
    CREATE Ttexto. tlinea = "día.  El  no  descuento de  las cuotas  por nómina  en   las fechas   estipuladas, no  me (nos)  exime de la".
    CREATE Ttexto. tlinea = "responsabilidad   de cancelarlas en forma oportuna, y por lo tanto, me  (nos) comprometo  (comprometemos)  a".
    CREATE Ttexto. tlinea = "cancelar en las oficinas de JURISCOOP, dentro de los plazos previstos. En caso de incumplimiento en el  pago".
    CREATE Ttexto. tlinea = "de  la  obligación  reconozco (cemos)  y  pagaré(mos) intereses  de  mora a la tasa máxima legal autorizada.".
    CREATE Ttexto. tlinea = "Además, JURISCOOP, queda autorizado  para  solicitar  el descuento por  nómina a los codeudores solidarios a".
    CREATE Ttexto. tlinea = "través  de  las  pagadurías  donde  estos laboren, cuando  lo  estime conveniente y  sin que  sea  necesario".
    CREATE Ttexto. tlinea = "requerimiento de ninguna índole. Para constancia se firma en la ciudad de " +  trim(W_CiuAgencia).
 /* CREATE Ttexto. tlinea = "a los " + TRIM(REPLACE(w_letrasDias,'Pesos ML','')) +  " días del mes de " + trim(nommes[MONTH(wfche)]) + " del año (" + trim(STRING(YEAR(wfche))) + ").". */
    CREATE Ttexto. tlinea = "a los " + TRIM(REPLACE(w_letrasDias,'Pesos ML','')) +  " días del mes de " + trim(nommes[MONTH(w_fecha)]) + " del año (" + trim(STRING(YEAR(w_fecha))) + ").". 
    CREATE Ttexto. tlinea = "".
    CREATE Ttexto. tlinea = "LIBRAMOS, ACEPTAMOS Y AUTORIZAMOS COMO DEUDORES SOLIDARIOS.".
    CREATE Ttexto. tlinea = "". 
    CREATE Ttexto. tlinea = "              FIRMA DEUDOR                                    FIRMA CODEUDOR SOLIDARIO     ". 
    CREATE Ttexto. tlinea = "". 
    CREATE Ttexto. tlinea = "Firma____________________________________________            Firma__________________________________________".
    CREATE Ttexto. tlinea = "Nombre __________________________________________            Nombre_________________________________________".
    CREATE Ttexto. tlinea = "C.C.   __________________________________________            C.C   _________________________________________".
    CREATE Ttexto. tlinea = "". 
    CREATE Ttexto. tlinea = "               FIRMA CODEUDOR SOLIDARIO                        FIRMA CODEUDOR SOLIDARIO     ".
    CREATE Ttexto. tlinea = "". 
    CREATE Ttexto. tlinea = "Firma____________________________________________            Firma__________________________________________".
    CREATE Ttexto. tlinea = "Nombre __________________________________________            Nombre_________________________________________".
    CREATE Ttexto. tlinea = "C.C.   __________________________________________            C.C   _________________________________________".
    CREATE Ttexto. tlinea = "".
    CREATE Ttexto. tlinea = "LEY 79 DE 1988, Art. 142/143/144".
    CREATE Ttexto. tlinea = "". 
    CREATE Ttexto. tlinea = "_____________________________       ______________________________        _________________________________".
    CREATE Ttexto. tlinea = "   PAGADOR FIRMA Y SELLO             JEFE PERSONAL FIRMA Y SELLO          FUNCIONARIO JURISCOOP QUE ELABORO".
    CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "". 
    CREATE Ttexto. tlinea = "Artículo 142. Toda persona, empresa o entidad pública o privada estará obligada a deducir y retener de".
    CREATE Ttexto. tlinea = "cualquier cantidad que haya de pagar a sus trabajadores o pensionados, las sumas  que estos  adeuden a".
    CREATE Ttexto. tlinea = "la cooperativa, y que la obligación conste en libranza, títulos valores, o  cualquier  otro  documento".
    CREATE Ttexto. tlinea = "suscrito por el deudor, quién para el efecto deberá dar su consentimiento previo.". 
    CREATE Ttexto. tlinea = "Parágrafo. Las personas, empresas o entidades obligadas a retener  deben entregar las sumas retenidas.".
    CREATE Ttexto. tlinea = "La cooperativa, simultáneamente  con el pago que hace el trabajador o pensionado. Si  por culpa  no lo". 
    CREATE Ttexto. tlinea = "hicieren, serán responsables ante la cooperativa de su omisión y quedarán solidariamente deudoras ante".
    CREATE Ttexto. tlinea = "ésta de las sumas dejadas de retener o entregar, junto con los intereses  de  la obligación  contraída".
    CREATE Ttexto. tlinea = "por el deudor.".
    CREATE Ttexto. tlinea = "Artículo 143. Para   los  efectos del artículo  anterior, prestará  mérito  ejecutivo  la  relación de".
    CREATE Ttexto. tlinea = "asociados deudores, con la prueba de haber sido entregada para el  descuento  con  antelacion  de  por".
    CREATE Ttexto. tlinea = "los menos diez (10) días hábiles.".
    CREATE Ttexto. tlinea = "Artículo 144.  Las deducciones a favor  de las cooperativas  tendrán  prelación sobre  cualquier  otro".
    CREATE Ttexto. tlinea = "descuento por obligaciones civiles, salvo las judiciales por alimentos.".
    CREATE Ttexto. tlinea = "Impresión Libranza Reverso...".
    CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "".
    CREATE Ttexto. tlinea = " 1  CONSIDERACIONES ADICIONALES ".
    CREATE Ttexto. tlinea = " 2  ___________________________________________________________________________________________________".
    CREATE Ttexto. tlinea = " 3  ___________________________________________________________________________________________________".
    CREATE Ttexto. tlinea = " 4  ___________________________________________________________________________________________________".
    CREATE Ttexto. tlinea = " 5  ___________________________________________________________________________________________________".
    CREATE Ttexto. tlinea = " 6  ___________________________________________________________________________________________________".
    CREATE Ttexto. tlinea = " 7  ___________________________________________________________________________________________________".
    CREATE Ttexto. tlinea = " 8  ___________________________________________________________________________________________________".
    CREATE Ttexto. tlinea = " 9  ___________________________________________________________________________________________________".
    CREATE Ttexto. tlinea = "10  ___________________________________________________________________________________________________".
    CREATE Ttexto. tlinea = "".     CREATE Ttexto. tlinea = "".     CREATE Ttexto. tlinea = "".     CREATE Ttexto. tlinea = "". 
    CREATE Ttexto. tlinea = "              FIRMA DEUDOR                                    FIRMA CODEUDOR SOLIDARIO     ". 
    CREATE Ttexto. tlinea = "". 
    CREATE Ttexto. tlinea = "Firma____________________________________________            Firma__________________________________________".
    CREATE Ttexto. tlinea = "Nombre __________________________________________            Nombre_________________________________________".
    CREATE Ttexto. tlinea = "C.C.   __________________________________________            C.C   _________________________________________".
    CREATE Ttexto. tlinea = "". 
    CREATE Ttexto. tlinea = "               FIRMA CODEUDOR SOLIDARIO                        FIRMA CODEUDOR SOLIDARIO     ".
    CREATE Ttexto. tlinea = "". 
    CREATE Ttexto. tlinea = "Firma____________________________________________            Firma__________________________________________".
    CREATE Ttexto. tlinea = "Nombre __________________________________________            Nombre_________________________________________".
    CREATE Ttexto. tlinea = "C.C.   __________________________________________            C.C   _________________________________________".
    CREATE Ttexto. tlinea = "".
    FOR EACH Ttexto:
      IF Ttexto.Tlinea NE "Impresión Libranza Reverso..." THEN
        PUT "     " Ttexto.Tlinea SKIP(0).
      ELSE DO: 
     /*  MESSAGE "Impresión Libranza Reverso..." VIEW-AS ALERT-BOX INFO BUTTONS OK. */
         PAGE.
      END.
    END.
    PAGE.

END PROCEDURE.
