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
   MESSAGE "No se encontró el cliente" VIEW-AS ALERT-BOX ERROR TITLE "Error pagare".
   RETURN ERROR.
END.

W_NomDireccion = Clientes.DIR_Comercial.
IF W_NomDireccion EQ "" THEN W_NomDireccion = Clientes.DIR_Residencia.
W_NomDireccion = W_NomDireccion + ",".

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
 CREATE Ttexto. tlinea = "                               PAGARÉ No." + STRING(WNumCre,"99999999").
 CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "".
 CREATE Ttexto. tlinea = "LUGAR: " + w_ciuAgencia.
 CREATE Ttexto. tlinea = "FECHA: " + TRIM(REPLACE(w_letrasDias,'Pesos ML','')) +  " días del mes de " + trim(nommes[MONTH(W_Fecha)]) + " del año (" + trim(STRING(YEAR(W_Fecha))) + ").".
 CREATE Ttexto. tlinea = "El(los) abajo firmante(s), mayor(es) de edad, identificado(s) como aparece al  pie de mi(nuestras)  firma(s),".
 CREATE Ttexto. tlinea = "quién(es) en  adelante  me(nos) denominaré(mos) EL(LOS) DEUDOR(ES), por  medio  del presente  pagaré  hacemos".
 CREATE Ttexto. tlinea = "constar: PRIMERO. Que me(nos) obligo(amos) a pagar a la orden  de  la  Cooperativa  de Ahorro y Credito - ".
 CREATE Ttexto. tlinea = "UTRAHUILCA, o su  cesionario o  quién  represente  sus  derechos  y  quién  en lo  sucesivo  seguirá".
 CREATE Ttexto. tlinea = "denominándose LA COOPERATIVA, en forma incondicional, indivisible y solidaria la suma de ____________________".
 CREATE Ttexto. tlinea = "___________________________________ PESOS MONEDA CORRIENTE ($________________), el día ______________________".
 CREATE Ttexto. tlinea = "(_____) del mes ________________________ de _______________(______) en las oficinas de  la  COOPERATIVA en la".
 CREATE Ttexto. tlinea = "ciudad de ________________________________________________________________.".
 /*CREATE Ttexto. tlinea = W_MontoLetras.*/
 /*CREATE Ttexto. tlinea = "A cancelarse totalmente en la Fecha " + STRING (W_FecVcto,"99/99/9999") + " En las Oficinas de la COOPERATIVA*********". */
 CREATE Ttexto. tlinea = "SEGUNDO. Que en caso de mora me(nos) obligo(amos) a pagar  intereses a  la tasa moratoria máxima que permitan".
 CREATE Ttexto. tlinea = "las disposiciones legales vigentes. TERCERO. Que  expresamente  declaro(amos) excusada la  presentación  para".
 CREATE Ttexto. tlinea = "el pago, el  aviso de rechazo  y el protesto. CUARTO. Que en caso de cobro judicial  o extrajudicial  de este".
 CREATE Ttexto. tlinea = "pagaré, serán  de  mi(nuestra) cuenta los gastos y costos que se ocasionen por la cobranza. Por lo  tanto, en".
 CREATE Ttexto. tlinea = "caso de cobro judicial, los gastos  no  se limitarán a las costas judiciales que decrete el juez,sino también".
 CREATE Ttexto. tlinea = "serán de mi(nuestro) cargo :  el valor  del  impuesto de  timbre,  los honorarios del abogado de acuerdo a la".
 CREATE Ttexto. tlinea = "instrucción que he(mos) impartido a  la COOPERATIVA, así  como todos los demás valores que se causen  por  la".
 CREATE Ttexto. tlinea = "gestión de cobro hasta el momento del  pago liberatorio. QUINTO. Que  reconozco (reconocemos) de  antemano el".
 CREATE Ttexto. tlinea = "derecho que le asiste a LA COOPERATIVA, para que en los eventos que a  continuación se señalan pueda declarar".
 CREATE Ttexto. tlinea = "extinguido el plazo y  de esta manera exigir  anticipadamente,  judicial o extrajudicialmente,  sin necesidad".
 CREATE Ttexto. tlinea = "de requerimiento  alguno,  el pago de la totalidad  del  saldo insoluto  de la obligación  incorporada  en el".
 CREATE Ttexto. tlinea = "presente pagaré, así  como sus intereses, los gastos  de cobranza,  incluyendo  los honorarios de  abogados y".
 CREATE Ttexto. tlinea = "demás obligaciones a mi(nuestro)  cargo constituidas a favor  de LA COOPERATIVA: A) Si se presenta mora en el".
 CREATE Ttexto. tlinea = "cumplimiento de cualquiera de las obligaciones que tenga(mos) con LA COOPERATIVA.En este caso, LA COOPERATIVA".
 CREATE Ttexto. tlinea = "podrá restituirme(restituirnos) el plazo, para  lo  cual podrá exigir el pago de las cuotas  vencidas,  junto".
 CREATE Ttexto. tlinea = "con la totalidad de  los intereses causados  hasta la fecha en la que se haga el respectivo pago,así como los".
 CREATE Ttexto. tlinea = "gastos de  honorarios  de  abogados y demás dineros que por mi (nuestra) cuenta  haya (n) sido pagados por LA".
 CREATE Ttexto. tlinea = "COOPERATIVA.B)Si abro(abrimos) o se me(nos) inicia proceso de concurso de acreedores, concordato, liquidación".
 CREATE Ttexto. tlinea = "oferta de cesión de  bienes,  cierre  o  abandono de los  negocios o en el  evento en que me (nos)  encuentre".
 CREATE Ttexto. tlinea = "(encontremos) en notorio  estado  de insolvencia. C) Si los bienes  dados en  garantía se  demeritan,  gravan".
 CREATE Ttexto. tlinea = "enajenan, en todo o en parte, o dejan de ser  garantía  suficiente. D) Si existen inexactitudes en  balances,".
 CREATE Ttexto. tlinea = "informes,declaraciones o documentos que haya(mos) presentado  a  LA COOPERATIVA. E)En los demás casos de ley.".
 CREATE Ttexto. tlinea = "SEXTO.Que los intereses pendientes producirán intereses  en  los términos establecidos en el Art886 del C.Co.".
 CREATE Ttexto. tlinea = "SEPTIMO. Que este pagaré podrá ser llenado por  LA COOPERATIVA,  según las  instrucciones  impartidas  por mi".
 CREATE Ttexto. tlinea = "(nosotros) en la carta  de instrucciones  que se encuentra adjunta al presente  título valor,  de conformidad".
 CREATE Ttexto. tlinea = "con lo  dispuesto en el  inciso segundo del Art.622 del C.Co. OCTAVO. Que la  solidaridad  e  indivisibilidad".
 CREATE Ttexto. tlinea = "subsisten en caso de prórroga,  pago parcial, novación o cualquier modificación a lo estipulado inicialmente.".
 CREATE Ttexto. tlinea = "NOVENO.Que expresamente faculto(facultamos)  a  LA COOPERATIVA para compensar los saldos pendientes por pagar".
 CREATE Ttexto. tlinea = "a mi(nuestro) cargo, con los dineros  que  tenga(mos) bajo cualquier título en  LA  COOPERATIVA.  DÉCIMO. Que".
 CREATE Ttexto. tlinea = "autorizo(autorizamos)  expresamente a  LA COOPERATIVA para que contrate abogados y acuerde sus honorarios  en".
 CREATE Ttexto. tlinea = "caso de cobranza judicial  o  extrajudicial del  crédito a  mi(nuestro) cargo.  DÉCIMO PRIMERO.  Que autorizo".
 CREATE Ttexto. tlinea = "(autorizamos) expresamente  a  LA COOPERATIVA para que a cualquier título endose el presente pagaré o ceda el".
 CREATE Ttexto. tlinea = "crédito incorporado en el mismo, a  favor de cualquier tercero sin necesidad de notificación. DÉCIMO SEGUNDO.".
 CREATE Ttexto. tlinea = "Serán de mi(nuestro) cargo los gastos  que conlleve  la aprobación  del crédito,  entre otros,  de consulta a".
 CREATE Ttexto. tlinea = "centrales de riego,  estudio de títulos,  legalización de las garantías, papelería,  gastos notariales,  etc.".
 CREATE Ttexto. tlinea = "DÉCIMO TERCERO.  Autorizo(amos) irrevocablemente a LA COOPERATIVA,  para que obtenga de cualquier fuente y se".
 CREATE Ttexto. tlinea = "reporte a   cualquier  Banco  de  datos, las   informaciones y  referencias  relativas  a mis(nuestros) datos".
 CREATE Ttexto. tlinea = "personales,  comportamiento  de  crédito,  hábitos  de  pago,  manejo  de  cuentas bancarias y en general, al".
 CREATE Ttexto. tlinea = "cumplimiento de mis(nuestras) obligaciones pecunarias.".
 CREATE Ttexto. tlinea = "".
 
 /*
 Creditos.Tasa                           AT 71 FORMAT                 ">9.9999" 
 "   % Anual"                            AT 80
 "pagaderos por períodos mensuales vencidos que comienzan a causarse desde la fecha de esté" AT 1
 "pagaré, El pago de la expresada cantidad, junto con los intereses se hará en" AT 1
 Creditos.Plazo                          AT 80 FORMAT "99"
 "cuotas"                                AT 84
 "fijas consecutivas de $"               AT 1
 Creditos.Cuota                          AT 27 FORMAT ">>>,>>>,>>9"
 "cada una, comenzando el pago de la  primera el  día" AT 39
 DAY(Creditos.Fec_Pago)                  AT 1 FORMAT "99"
 "del mes de "                           AT 4
 NomMes[MONTH(Creditos.Fec_Pago)]        AT 17 FORMAT "X(12)"
 "del año "                              AT 30
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
  CREATE Ttexto. tlinea = "Impresión Pagaré Reverso...".
  CREATE Ttexto. tlinea = "                                  CARTA DE INSTRUCCIONES".
  CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "Señores".
  CREATE Ttexto. tlinea = "U T R A H U I L C A".
  CREATE Ttexto. tlinea = "Apreciados Señores:". CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "El(los) firmante(s), mayor(es) de edad, identificado(s) como aparece al pie de mi(nuestra)  firma(s),  quién".
  CREATE Ttexto. tlinea = "(es) en adelante me(nos) denominare(mos)  EL(LOS)  DEUDOR(ES),  me(nos) permito(permitimos)  manifertar  que".
  CREATE Ttexto. tlinea = "autorizo(autorizamos) en forma irrevocable a la Cooperativa de Ahorro y credito - UTRAHUILCA  que".
  CREATE Ttexto. tlinea = "en lo sucesivo se denominará LA COOPERATIVA o a su cesionario o a quién represente sus derechos,para  llenar".
  CREATE Ttexto. tlinea = "sin previo aviso el pagaré a la orden con espacios en blanco que he(mos) suscrito a favor de la COOPERATIVA,".
  CREATE Ttexto. tlinea = "Conforme a las siguientes instrucciones:".
  CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "1) Elpagaré podrá ser llenado por LA COOPERATIVA a partir de cualquiera de los siguientes  eventos: A) Si se".
  CREATE Ttexto. tlinea = "presenta mora en el cumplimiento de cualquiera de las obligaciones  que  tenga(mos) con  LA  COOPERATIVA. En".
  CREATE Ttexto. tlinea = "este caso, LA COOPERATIVA podrá restituirme(restituirnos) el plazo para lo cual podrá exigir el pago de  las".
  CREATE Ttexto. tlinea = "cuotas vencidas,junto con la totalidad de los intereses causados hasta la fecha en que se haga el respectivo".
  CREATE Ttexto. tlinea = "pago, así como los gastos de honorarios de abogados y demás dineros que por mi(nuestra) cuenta haya(n)  sido".
  CREATE Ttexto. tlinea = "pagados por LA COOPERATIVA. B)  Si abro(abrimos)  o  se me(nos) inicia proceso  de  concurso de  acreedores,".
  CREATE Ttexto. tlinea = "concordato, liquidación, oferta de cesión de  bienes, cierre o  abandono de los  negocios o en el  evento en".
  CREATE Ttexto. tlinea = "que me (nos) encuentre (encontremos) en notorio estado de insolvencia.C)Si los bienes  dados en  garantía se".
  CREATE Ttexto. tlinea = "demeritan, gravan,enajenan,en todo o en parte,o dejan de ser garantía suficiente. D)Si existen inexactitudes".
  CREATE Ttexto. tlinea = "en balances, informes,declaraciones o documentos que haya(mos) presentado a  LA COOPERATIVA.  E)En los demás".
  CREATE Ttexto. tlinea = "casos de ley.".
  CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "2) El valor del pagaré será igual al monto de las sumas que le adeude(mos) a LA COOPERATIVA por concepto del".
  CREATE Ttexto. tlinea = "mutuo comercial que con ella he(mos) celebrado y todas las obligaciones que de este contrato se deriven, las".
  CREATE Ttexto. tlinea = "cuales se predican de capital, intereses, comisiones, gastos, honorarios, costas judiciales,o cualquier otro".
  CREATE Ttexto. tlinea = "concepto que tenga el deber de pagar a LA COOPERATIVA.". CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "3) La fecha de vencimiento será la misma en que sea llenado el pagaré por  LA  COOPERATIVA y serán exigibles".
  CREATE Ttexto. tlinea = "inmediatamente todas las obligaciones en él contenidas a mi(nuestro) cargo, sin necesidad de que se me (nos)".
  CREATE Ttexto. tlinea = "requiera, judicial o extrajudicialmente, en tal sentido. Además por el hecho de ser utilizado el  pagaré, LA".
  CREATE Ttexto. tlinea = "COOPERATIVA podrá declarar de plazo vencido todas y cada una de las obligaciones que adicionalmente tengamos".
  CREATE Ttexto. tlinea = "a nuestro cargo, aún cuando respecto de ellas se hubiera pactado algún plazo para su exigibilidad y el mismo".
  CREATE Ttexto. tlinea = "estuviere pendiente, sin necesidad de requerimiento, al cual renuncio(amos) expresamente.".
  CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "4) El lugar de pago del pagaré será aquel donde se efectúe el cobro.".
  CREATE Ttexto. tlinea = "El pagaré llenado de acuerdo a las anteriores instrucciones presta mérito ejecutivo al tenor de lo dispuesto".
  CREATE Ttexto. tlinea = "por el Art.488 del C.P.C., por lo que LA  COOPERATIVA  podrá exigir  su  cancelación por  vía  judicial, sin".
  CREATE Ttexto. tlinea = "perjuicio de las demás acciones legales que pueda llegar a tener.". CREATE Ttexto. tlinea = "".
  CREATE Ttexto. tlinea = "La presente carta de instrucciones es impartida de conformidad con lo dispuesto por  el  inciso  segundo del".
  CREATE Ttexto. tlinea = "Art.622 del C.Co.,para todos los efectos allí previstos.Igualmente,dejo(dejamos) constancia de que recibimos".
  CREATE Ttexto. tlinea = "copia del pagaré y de la presente carta de instrucciones.". CREATE Ttexto. tlinea = "". CREATE Ttexto. tlinea = "". 

  CREATE Ttexto. tlinea = "Para constancia se firma en la ciudad de ________________________, a los ___________________ (____) días del".
  CREATE Ttexto. tlinea = "mes de ___________________ de _______________________ (_________ )".  /* + TRIM(W_CiuAgencia).*/
  /*CREATE Ttexto. tlinea = "En la Fecha :" + TRIM(REPLACE(w_letrasDias,'Pesos ML','')) +  " días del mes de " + trim(nommes[MONTH(creditos.fec_aprobacion)]) + " del año (" + trim(STRING(YEAR(creditos.fec_aprobacion))) + ").".  */
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
    IF Ttexto.Tlinea NE "Impresión Pagaré Reverso..." THEN
      PUT "     " Ttexto.Tlinea SKIP(0).
    ELSE DO: 
     /*MESSAGE "Impresión Pagaré Reverso..." VIEW-AS ALERT-BOX INFO BUTTONS OK. */
       PAGE.
    END.
  END.
  PAGE.

END PROCEDURE.
