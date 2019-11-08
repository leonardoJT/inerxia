DEFINE INPUT PARAMETER WNit    LIKE Clientes.Nit.
DEFINE INPUT PARAMETER WNumCre LIKE Creditos.Num_Credito.
DEFINE INPUT PARAMETER WFche AS DATE.

DEFINE TEMP-TABLE Ttexto
    FIELD Tlinea  AS CHARACTER FORMAT "X(110)".

{INCLUIDO\VARIABLE.I "SHARED"}

DEFINE VAR NomMes AS CHARACTER FORMAT "X(12)" EXTENT 12
       INITIAL ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"].

DEFI VAR W_FecVcto LIKE Creditos.Fec_Desembolso.
DEFINE VAR W_Puntero      AS ROWID.
DEFINE VAR W_NomAgencia   AS CHARACTER FORMAT "X(45)".
DEFINE VAR W_NomPagador   AS CHARACTER FORMAT "X(45)".
DEFINE VAR W_NomEmpresa   AS CHARACTER FORMAT "X(45)".
DEFINE VAR W_NomEmpresa2  AS CHARACTER FORMAT "X(45)".
DEFINE VAR W_NomDeudor    AS CHARACTER FORMAT "X(45)".
DEFINE VAR W_NomDeudor2   AS CHARACTER FORMAT "X(45)".
DEFINE VAR W_NomDireccion AS CHARACTER FORMAT "X(90)".
DEFINE VAR W_MontoLetras  AS CHARACTER FORMAT "X(90)".
DEFINE VAR W_NomMes       AS CHARACTER FORMAT "X(12)".
DEFINE VAR W_CiuAgencia   AS CHARACTER FORMAT "X(35)".
DEFINE VAR W_MontoIzq     AS CHARACTER FORMAT "X(20)".
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
DEFI   VAR W_NPagare LIKE Creditos.Pagare.

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
   MESSAGE "No se encontr� el cliente" VIEW-AS ALERT-BOX ERROR TITLE "Error pagare".
   RETURN ERROR.
END.

W_NomDireccion = Clientes.DIR_Comercial.
IF W_NomDireccion EQ "" THEN W_NomDireccion = Clientes.DIR_Residencia.
W_NomDireccion = W_NomDireccion + ",".

FIND Creditos WHERE Creditos.Num_Credito EQ WNumCre AND
                    Creditos.Nit         EQ WNit NO-LOCK NO-ERROR.
IF NOT AVAILABLE Creditos THEN DO:
   MESSAGE "No se ha Encontrado el Cr�dito al cual se le" SKIP
           "imprimir� el pagare." SKIP(1)
           "Se Cancela la Operaci�n." VIEW-AS ALERT-BOX ERROR  TITLE "Error pagare".
   RETURN ERROR.
END.
ELSE DO:
  IF Creditos.For_Pago EQ 2 THEN DO:
     /* RUN MontoEsc.r (INPUT DAY(WFche),INPUT 0, OUTPUT W_LetrasDIAS). */
     RUN MontoEsc.r (INPUT DAY(W_fecha),INPUT 0, OUTPUT W_LetrasDIAS).
     W_LetrasDias = TRIM(W_LetrasDias) + " (" + TRIM(STRING( DAY(W_fecha),">9")) + ") ".

     FIND Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
     IF NOT AVAILABLE Empresas THEN DO:
        MESSAGE "Aunque el cr�dito es por n�mina, el cliente" SKIP
                "no se encuentra matriculado a ninguna empresa" 
             VIEW-AS ALERT-BOX TITLE "INFORMATIVO".
        /*RETURN ERROR.*/
     END.
  END.
END.

W_NPagare    = Creditos.Pagare.
W_FecVcto = WFche + (Creditos.Plazo * 30).
IF Creditos.Per_Pago EQ 3 THEN
   W_FecVcto = WFche + (Creditos.Plazo * 15).

RUN MontoEsc.r (INPUT Creditos.Monto,INPUT 0, OUTPUT W_MontoLetras).
W_MontoLetras = W_MontoLetras + "  ($" + STRING(Creditos.Monto,">>>,>>>,>>>,>>9") + ")".

FIND FIRST Agencias WHERE Agencias.Agencia EQ Creditos.Agencia AND Agencias.Estado NE 3 NO-LOCK NO-ERROR.
IF AVAILABLE(agencias) AND agencias.ciudad NE "" THEN DO:
   ASSIGN W_NomAgencia = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
   FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "B" AND Ubicacion.Ubicacion EQ Agencia.Ciudad NO-LOCK NO-ERROR.
   IF AVAILABLE Ubicacion THEN ASSIGN W_CiuAgencia  = Ubicacion.Nombre.
   FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Agencia.Ciudad,1,5) NO-LOCK NO-ERROR.
   IF AVAILABLE Ubicacion THEN ASSIGN W_CiuAgencia  = W_CiuAgencia + " - " + Ubicacion.Nombre.
   FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "D" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Agencia.Ciudad,1,2) NO-LOCK NO-ERROR.
   IF AVAILABLE Ubicacion THEN ASSIGN W_CiuAgencia  = W_CiuAgencia + " - " + Ubicacion.Nombre.
   W_CiuAgencia = TRIM(LC(W_CiuAgencia)).
END.
ELSE
  W_CiuAgencia = "_________________________________________".

/*FIND Ubicacion WHERE Ubicacion.Ubicacion EQ Agencia.Ciudad AND Ubicacion.Tipo EQ "C" NO-LOCK NO-ERROR.
IF AVAILABLE Ubicacion THEN
   W_CiuAgencia = Ubicacion.Nombre.*/

DEFINE VAR Listado AS CHARACTER INITIAL "".
listado = W_PathSpl + "L_Pagare.Lst".
{incluido\Imprim90.I "listado"}
/* {incluido\Imprimir.I "listado"} */

PROCEDURE ProcesoImprimir:
 CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "                               PAGAR� No." + STRING(WNumCre,"99999999").
 CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "LUGAR: " + w_ciuAgencia.
 CREATE Ttexto. tlinea = "FECHA: " + TRIM(REPLACE(w_letrasDias,'Pesos ML','')) +  " d�as del mes de " + trim(nommes[MONTH(W_Fecha)]) + " del a�o (" + trim(STRING(YEAR(W_Fecha))) + ").".
 CREATE Ttexto. tlinea = "El(los) abajo firmante(s), mayor(es) de edad, identificado(s) como aparece al  pie de mi(nuestras)  firma(s),".
 CREATE Ttexto. tlinea = "qui�n(es) en  adelante  me(nos) denominar�(mos) EL(LOS) DEUDOR(ES), por  medio  del presente  pagar�  hacemos".
 CREATE Ttexto. tlinea = "constar: PRIMERO. Que me(nos) obligo(amos) a pagar a la orden  de  la  Cooperativa  de Ahorro y Credito - ".
 CREATE Ttexto. tlinea = "UTRAHUILCA, o su  cesionario o  qui�n  represente  sus  derechos  y  qui�n  en lo  sucesivo  seguir�".
 CREATE Ttexto. tlinea = "denomin�ndose LA COOPERATIVA, en forma incondicional, indivisible y solidaria la suma de ____________________".
 CREATE Ttexto. tlinea = "___________________________________ PESOS MONEDA CORRIENTE ($________________), el d�a ______________________".
 CREATE Ttexto. tlinea = "(_____) del mes ________________________ de _______________(______) en las oficinas de  la  COOPERATIVA en la".
 CREATE Ttexto. tlinea = "ciudad de ________________________________________________________________.".
 /*CREATE Ttexto. tlinea = W_MontoLetras.*/
 /*CREATE Ttexto. tlinea = "A cancelarse totalmente en la Fecha " + STRING (W_FecVcto,"99/99/9999") + " En las Oficinas de la COOPERATIVA*********". */
 CREATE Ttexto. tlinea = "SEGUNDO. Que en caso de mora me(nos) obligo(amos) a pagar  intereses a  la tasa moratoria m�xima que permitan".
 CREATE Ttexto. tlinea = "las disposiciones legales vigentes. TERCERO. Que  expresamente  declaro(amos) excusada la  presentaci�n  para".
 CREATE Ttexto. tlinea = "el pago, el  aviso de rechazo  y el protesto. CUARTO. Que en caso de cobro judicial  o extrajudicial  de este".
 CREATE Ttexto. tlinea = "pagar�, ser�n  de  mi(nuestra) cuenta los gastos y costos que se ocasionen por la cobranza. Por lo  tanto, en".
 CREATE Ttexto. tlinea = "caso de cobro judicial, los gastos  no  se limitar�n a las costas judiciales que decrete el juez,sino tambi�n".
 CREATE Ttexto. tlinea = "ser�n de mi(nuestro) cargo :  el valor  del  impuesto de  timbre,  los honorarios del abogado de acuerdo a la".
 CREATE Ttexto. tlinea = "instrucci�n que he(mos) impartido a  la COOPERATIVA, as�  como todos los dem�s valores que se causen  por  la".
 CREATE Ttexto. tlinea = "gesti�n de cobro hasta el momento del  pago liberatorio. QUINTO. Que  reconozco (reconocemos) de  antemano el".
 CREATE Ttexto. tlinea = "derecho que le asiste a LA COOPERATIVA, para que en los eventos que a  continuaci�n se se�alan pueda declarar".
 CREATE Ttexto. tlinea = "extinguido el plazo y  de esta manera exigir  anticipadamente,  judicial o extrajudicialmente,  sin necesidad".
 CREATE Ttexto. tlinea = "de requerimiento  alguno,  el pago de la totalidad  del  saldo insoluto  de la obligaci�n  incorporada  en el".
 CREATE Ttexto. tlinea = "presente pagar�, as�  como sus intereses, los gastos  de cobranza,  incluyendo  los honorarios de  abogados y".
 CREATE Ttexto. tlinea = "dem�s obligaciones a mi(nuestro)  cargo constituidas a favor  de LA COOPERATIVA: A) Si se presenta mora en el".
 CREATE Ttexto. tlinea = "cumplimiento de cualquiera de las obligaciones que tenga(mos) con LA COOPERATIVA.En este caso, LA COOPERATIVA".
 CREATE Ttexto. tlinea = "podr� restituirme(restituirnos) el plazo, para  lo  cual podr� exigir el pago de las cuotas  vencidas,  junto".
 CREATE Ttexto. tlinea = "con la totalidad de  los intereses causados  hasta la fecha en la que se haga el respectivo pago,as� como los".
 CREATE Ttexto. tlinea = "gastos de  honorarios  de  abogados y dem�s dineros que por mi (nuestra) cuenta  haya (n) sido pagados por LA".
 CREATE Ttexto. tlinea = "COOPERATIVA.B)Si abro(abrimos) o se me(nos) inicia proceso de concurso de acreedores, concordato, liquidaci�n".
 CREATE Ttexto. tlinea = "oferta de cesi�n de  bienes,  cierre  o  abandono de los  negocios o en el  evento en que me (nos)  encuentre".
 CREATE Ttexto. tlinea = "(encontremos) en notorio  estado  de insolvencia. C) Si los bienes  dados en  garant�a se  demeritan,  gravan".
 CREATE Ttexto. tlinea = "enajenan, en todo o en parte, o dejan de ser  garant�a  suficiente. D) Si existen inexactitudes en  balances,".
 CREATE Ttexto. tlinea = "informes,declaraciones o documentos que haya(mos) presentado  a  LA COOPERATIVA. E)En los dem�s casos de ley.".
 CREATE Ttexto. tlinea = "SEXTO.Que los intereses pendientes producir�n intereses  en  los t�rminos establecidos en el Art886 del C.Co.".
 CREATE Ttexto. tlinea = "SEPTIMO. Que este pagar� podr� ser llenado por  LA COOPERATIVA,  seg�n las  instrucciones  impartidas  por mi".
 CREATE Ttexto. tlinea = "(nosotros) en la carta  de instrucciones  que se encuentra adjunta al presente  t�tulo valor,  de conformidad".
 CREATE Ttexto. tlinea = "con lo  dispuesto en el  inciso segundo del Art.622 del C.Co. OCTAVO. Que la  solidaridad  e  indivisibilidad".
 CREATE Ttexto. tlinea = "subsisten en caso de pr�rroga,  pago parcial, novaci�n o cualquier modificaci�n a lo estipulado inicialmente.".
 CREATE Ttexto. tlinea = "NOVENO.Que expresamente faculto(facultamos)  a  LA COOPERATIVA para compensar los saldos pendientes por pagar".
 CREATE Ttexto. tlinea = "a mi(nuestro) cargo, con los dineros  que  tenga(mos) bajo cualquier t�tulo en  LA  COOPERATIVA.  D�CIMO. Que".
 CREATE Ttexto. tlinea = "autorizo(autorizamos)  expresamente a  LA COOPERATIVA para que contrate abogados y acuerde sus honorarios  en".
 CREATE Ttexto. tlinea = "caso de cobranza judicial  o  extrajudicial del  cr�dito a  mi(nuestro) cargo.  D�CIMO PRIMERO.  Que autorizo".
 CREATE Ttexto. tlinea = "(autorizamos) expresamente  a  LA COOPERATIVA para que a cualquier t�tulo endose el presente pagar� o ceda el".
 CREATE Ttexto. tlinea = "cr�dito incorporado en el mismo, a  favor de cualquier tercero sin necesidad de notificaci�n. D�CIMO SEGUNDO.".
 CREATE Ttexto. tlinea = "Ser�n de mi(nuestro) cargo los gastos  que conlleve  la aprobaci�n  del cr�dito,  entre otros,  de consulta a".
 CREATE Ttexto. tlinea = "centrales de riego,  estudio de t�tulos,  legalizaci�n de las garant�as, papeler�a,  gastos notariales,  etc.".
 CREATE Ttexto. tlinea = "D�CIMO TERCERO.  Autorizo(amos) irrevocablemente a LA COOPERATIVA,  para que obtenga de cualquier fuente y se".
 CREATE Ttexto. tlinea = "reporte a   cualquier  Banco  de  datos, las   informaciones y  referencias  relativas  a mis(nuestros) datos".
 CREATE Ttexto. tlinea = "personales,  comportamiento  de  cr�dito,  h�bitos  de  pago,  manejo  de  cuentas bancarias y en general, al".
 CREATE Ttexto. tlinea = "cumplimiento de mis(nuestras) obligaciones pecunarias.".
 CREATE Ttexto. tlinea = "".
 
 /*
 Creditos.Tasa                           AT 71 FORMAT                 ">9.9999" 
 "   % Anual"                            AT 80
 "pagaderos por per�odos mensuales vencidos que comienzan a causarse desde la fecha de est�" AT 1
 "pagar�, El pago de la expresada cantidad, junto con los intereses se har� en" AT 1
 Creditos.Plazo                          AT 80 FORMAT "99"
 "cuotas"                                AT 84
 "fijas consecutivas de $"               AT 1
 Creditos.Cuota                          AT 27 FORMAT ">>>,>>>,>>9"
 "cada una, comenzando el pago de la  primera el  d�a" AT 39
 DAY(Creditos.Fec_Pago)                  AT 1 FORMAT "99"
 "del mes de "                           AT 4
 NomMes[MONTH(Creditos.Fec_Pago)]        AT 17 FORMAT "X(12)"
 "del a�o "                              AT 30
 YEAR(Creditos.Fec_Pago)                 AT 40 FORMAT "9999" SKIP(2)
 */
 
 /*HOJA DE FIRMAS Y HUELLAS*/
  CREATE Ttexto. tlinea = "              FIRMA DEUDOR                                     FIRMA CODEUDOR SOLIDARIO".
  CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "Firma _________________________________                 Firma __________________________________".
  CREATE Ttexto. tlinea = "Nombre ________________________________                 Nombre _________________________________".
  CREATE Ttexto. tlinea = "C.C.   ________________________________                 C.C.   _________________________________".
  CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "".        
  CREATE Ttexto. tlinea = "      FIRMA CODEUDOR SOLIDARIO                                 FIRMA CODEUDOR SOLIDARIO".
  CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "Firma _________________________________                 Firma __________________________________".
  CREATE Ttexto. tlinea = "Nombre ________________________________                 Nombre _________________________________".
  CREATE Ttexto. tlinea = "C.C.   ________________________________                 C.C.   _________________________________".
  CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "      FIRMA CODEUDOR SOLIDARIO                                 FIRMA CODEUDOR SOLIDARIO".
  CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "Firma _________________________________                 Firma __________________________________". 
  CREATE Ttexto. tlinea = "Nombre ________________________________                 Nombre _________________________________". 
  CREATE Ttexto. tlinea = "C.C.   ________________________________                 C.C.   _________________________________".
  CREATE Ttexto. tlinea = "Impresi�n Pagar� Reverso...".
  CREATE Ttexto. tlinea = "                                  CARTA DE INSTRUCCIONES".
  CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "Se�ores".
  CREATE Ttexto. tlinea = "U T R A H U I L C A".
  CREATE Ttexto. tlinea = "Apreciados Se�ores:". CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "El(los) firmante(s), mayor(es) de edad, identificado(s) como aparece al pie de mi(nuestra)  firma(s),  qui�n".
  CREATE Ttexto. tlinea = "(es) en adelante me(nos) denominare(mos)  EL(LOS)  DEUDOR(ES),  me(nos) permito(permitimos)  manifertar  que".
  CREATE Ttexto. tlinea = "autorizo(autorizamos) en forma irrevocable a la Cooperativa de Ahorro y credito - UTRAHUILCA  que".
  CREATE Ttexto. tlinea = "en lo sucesivo se denominar� LA COOPERATIVA o a su cesionario o a qui�n represente sus derechos,para  llenar".
  CREATE Ttexto. tlinea = "sin previo aviso el pagar� a la orden con espacios en blanco que he(mos) suscrito a favor de la COOPERATIVA,".
  CREATE Ttexto. tlinea = "Conforme a las siguientes instrucciones:".
  CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "1) Elpagar� podr� ser llenado por LA COOPERATIVA a partir de cualquiera de los siguientes  eventos: A) Si se".
  CREATE Ttexto. tlinea = "presenta mora en el cumplimiento de cualquiera de las obligaciones  que  tenga(mos) con  LA  COOPERATIVA. En".
  CREATE Ttexto. tlinea = "este caso, LA COOPERATIVA podr� restituirme(restituirnos) el plazo para lo cual podr� exigir el pago de  las".
  CREATE Ttexto. tlinea = "cuotas vencidas,junto con la totalidad de los intereses causados hasta la fecha en que se haga el respectivo".
  CREATE Ttexto. tlinea = "pago, as� como los gastos de honorarios de abogados y dem�s dineros que por mi(nuestra) cuenta haya(n)  sido".
  CREATE Ttexto. tlinea = "pagados por LA COOPERATIVA. B)  Si abro(abrimos)  o  se me(nos) inicia proceso  de  concurso de  acreedores,".
  CREATE Ttexto. tlinea = "concordato, liquidaci�n, oferta de cesi�n de  bienes, cierre o  abandono de los  negocios o en el  evento en".
  CREATE Ttexto. tlinea = "que me (nos) encuentre (encontremos) en notorio estado de insolvencia.C)Si los bienes  dados en  garant�a se".
  CREATE Ttexto. tlinea = "demeritan, gravan,enajenan,en todo o en parte,o dejan de ser garant�a suficiente. D)Si existen inexactitudes".
  CREATE Ttexto. tlinea = "en balances, informes,declaraciones o documentos que haya(mos) presentado a  LA COOPERATIVA.  E)En los dem�s".
  CREATE Ttexto. tlinea = "casos de ley.".
  CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "2) El valor del pagar� ser� igual al monto de las sumas que le adeude(mos) a LA COOPERATIVA por concepto del".
  CREATE Ttexto. tlinea = "mutuo comercial que con ella he(mos) celebrado y todas las obligaciones que de este contrato se deriven, las".
  CREATE Ttexto. tlinea = "cuales se predican de capital, intereses, comisiones, gastos, honorarios, costas judiciales,o cualquier otro".
  CREATE Ttexto. tlinea = "concepto que tenga el deber de pagar a LA COOPERATIVA.". CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "3) La fecha de vencimiento ser� la misma en que sea llenado el pagar� por  LA  COOPERATIVA y ser�n exigibles".
  CREATE Ttexto. tlinea = "inmediatamente todas las obligaciones en �l contenidas a mi(nuestro) cargo, sin necesidad de que se me (nos)".
  CREATE Ttexto. tlinea = "requiera, judicial o extrajudicialmente, en tal sentido. Adem�s por el hecho de ser utilizado el  pagar�, LA".
  CREATE Ttexto. tlinea = "COOPERATIVA podr� declarar de plazo vencido todas y cada una de las obligaciones que adicionalmente tengamos".
  CREATE Ttexto. tlinea = "a nuestro cargo, a�n cuando respecto de ellas se hubiera pactado alg�n plazo para su exigibilidad y el mismo".
  CREATE Ttexto. tlinea = "estuviere pendiente, sin necesidad de requerimiento, al cual renuncio(amos) expresamente.".
  CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "4) El lugar de pago del pagar� ser� aquel donde se efect�e el cobro.".
  CREATE Ttexto. tlinea = "El pagar� llenado de acuerdo a las anteriores instrucciones presta m�rito ejecutivo al tenor de lo dispuesto".
  CREATE Ttexto. tlinea = "por el Art.488 del C.P.C., por lo que LA  COOPERATIVA  podr� exigir  su  cancelaci�n por  v�a  judicial, sin".
  CREATE Ttexto. tlinea = "perjuicio de las dem�s acciones legales que pueda llegar a tener.". CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "La presente carta de instrucciones es impartida de conformidad con lo dispuesto por  el  inciso  segundo del".
  CREATE Ttexto. tlinea = "Art.622 del C.Co.,para todos los efectos all� previstos.Igualmente,dejo(dejamos) constancia de que recibimos".
  CREATE Ttexto. tlinea = "copia del pagar� y de la presente carta de instrucciones.". CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "". 

  CREATE Ttexto. tlinea = "Para constancia se firma en la ciudad de ________________________, a los ___________________ (____) d�as del".
  CREATE Ttexto. tlinea = "mes de ___________________ de _______________________ (_________ )".  /* + TRIM(W_CiuAgencia).*/
  /*CREATE Ttexto. tlinea = "En la Fecha :" + TRIM(REPLACE(w_letrasDias,'Pesos ML','')) +  " d�as del mes de " + trim(nommes[MONTH(creditos.fec_aprobacion)]) + " del a�o (" + trim(STRING(YEAR(creditos.fec_aprobacion))) + ").".  */
  CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "              FIRMA DEUDOR                                     FIRMA CODEUDOR SOLIDARIO".
  CREATE Ttexto. tlinea = "".  CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "Firma _________________________________                 Firma __________________________________".
  CREATE Ttexto. tlinea = "Nombre ________________________________                 Nombre _________________________________".
  CREATE Ttexto. tlinea = "C.C.   ________________________________                 C.C.   _________________________________".
  CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "". 
  CREATE Ttexto. tlinea = "      FIRMA CODEUDOR SOLIDARIO                                 FIRMA CODEUDOR SOLIDARIO".
  CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "Firma _________________________________                 Firma __________________________________".
  CREATE Ttexto. tlinea = "Nombre ________________________________                 Nombre _________________________________".
  CREATE Ttexto. tlinea = "C.C.   ________________________________                 C.C.   _________________________________".
  CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "".
                                                                                                             
  CREATE Ttexto. tlinea = "      FIRMA CODEUDOR SOLIDARIO                                 FIRMA CODEUDOR SOLIDARIO".
  CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "".  CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "Firma _________________________________                 Firma __________________________________".
  CREATE Ttexto. tlinea = "Nombre ________________________________                 Nombre _________________________________".
  CREATE Ttexto. tlinea = "C.C.   ________________________________                 C.C.   _________________________________".
  CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "".

  FOR EACH Ttexto:
    IF Ttexto.Tlinea NE "Impresi�n Pagar� Reverso..." THEN
      PUT "     " Ttexto.Tlinea SKIP(0).
    ELSE DO: 
     /*MESSAGE "Impresi�n Pagar� Reverso..." VIEW-AS ALERT-BOX INFO BUTTONS OK. */
       PAGE.
    END.
  END.
  PAGE.

END PROCEDURE.
