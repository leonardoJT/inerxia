 {Incluido\VARIABLE.I "SHARED"}
 RUN Rutinas.r PERSISTENT SET W_Manija.

 DEFINE INPUT PARAMETER W_Nit   LIKE Clientes.Nit.
 DEFINE INPUT PARAMETER W_Cue   LIKE Ahorros.Cue_Ahorros.

 DEFINE VAR W_NomCli           AS CHARACTER FORMAT "X(30)".
 DEFINE VAR W_Rpta             AS LOGICAL. 
 DEFINE VAR W_MonEs            AS CHARACTER FORMAT "X(80)".
 DEFINE VAR W_MonEs1           AS CHARACTER FORMAT "X(80)".
 DEFINE VAR W_MonEs2           AS CHARACTER FORMAT "X(80)".
 DEFINE VAR W_MonEs3           AS CHARACTER FORMAT "X(80)".

 DEFINE VAR W_Tipo    AS CHARACTER FORMAT "X(6)".
 DEFINE VAR Mensaje   AS CHARACTER INITIAL "".  
 DEFINE VAR procname  AS CHARACTER FORMAT "X(132)" INITIAL "temporal.txt".
 DEFINE VAR W_Titulo  AS CHARACTER FORMAT "X(40)".
 DEFINE VAR Listado   AS CHARACTER INITIAL "".


  
 /* DEFINE FRAME F-Encabezado
    HEADER
      W_nit AT ROW 1 COL 2
  WITH DOWN WIDTH 300 FRAME F-Encabezado NO-LABEL NO-BOX NO-UNDERLINE PAGE-TOP 
                              USE-TEXT.*/

  DEFINE FRAME F_Movimiento
  WITH 10 DOWN size 150 by 10 FRAME F_Movimiento USE-TEXT NO-BOX NO-LABEL STREAM-IO.

/*  DEFINE FRAME F-Totales
    HEADER
      W_Nit
  WITH WIDTH 150 FRAME F-Totales PAGE-BOTTOM NO-LABEL NO-BOX NO-UNDERLINE USE-TEXT.*/

  
  /*Listado = W_PathSpl + "L_Ingreso.Lst".*/
  
  Listado = W_pathspl + "Informe" + TRIM(string(W_cue)).


  DEFINE VAR W_Dispositivo AS CHARACTER INITIAL "P".  
  RUN P-DisPos IN W_Manija (INPUT-OUTPUT Listado, INPUT-OUTPUT W_Dispositivo).
  IF W_Dispositivo = "" THEN
     RETURN.
  OUTPUT TO VALUE(listado) NO-ECHO PAGED PAGE-SIZE 22.

/*  CASE W_Dispositivo:
 *     WHEN "I" THEN
 *       OUTPUT TO PRINTER        NO-ECHO PAGE-SIZE 28.
 *     OTHERWISE 
 *       OUTPUT TO VALUE(listado) NO-ECHO PAGED PAGE-SIZE 28.
 *   END CASE.*/
  /* Sirve para armar el comentario de la transaccion */

  
  /*VIEW FRAME F-Encabezado.
  VIEW FRAME F-Totales.  */
  W_NomCli = "".
  FIND FIRST Clientes WHERE Clientes.nit EQ W_Nit NO-LOCK NO-ERROR.
  IF AVAILABLE(Clientes) THEN 
    W_Nomcli = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.

  FIND Ahorros WHERE Ahorros.Nit EQ W_Nit AND Ahorros.Cue_Ahorros EQ W_Cue NO-LOCK NO-ERROR.
  IF AVAILABLE Ahorros THEN DO:
      RUN MontoEsc.r (INPUT ROUND(Ahorros.sdo_disponible,0),INPUT 0,OUTPUT W_MonEs).
      RUN PartirValor IN W_Manija (INPUT W_MonEs,INPUT 60,OUTPUT W_MonEs1,OUTPUT W_MonEs2,OUTPUT W_MonEs3). 
  END.

  DEFINE VAR Pagadero AS CHARACTER FORMAT "X(30)".
  DEFINE VAR Tasa2    LIKE Ahorros.Tasa.
  
  CASE Ahorros.Per_Liquidacion:
      WHEN 1 THEN Pagadero = "Dia".
      WHEN 2 THEN Pagadero = "Mes".
      WHEN 3 THEN Pagadero = "Trimestre".
      WHEN 4 THEN Pagadero = "Semestre".
      WHEN 5 THEN Pagadero = "Anual".
      WHEN 6 THEN Pagadero = "Al Vencimiento".
  END CASE.
  Tasa2 = Ahorros.Tasa.

  IF Ahorros.FOR_Liquidacion EQ 1 THEN
     Pagadero = Pagadero + " Anticipado".
  ELSE
     Pagadero = Pagadero + " Vencido".

  IF W_MonEs2 EQ "" AND W_MonEs3 EQ "" THEN
     W_MonEs1 = W_MonEs1 + " *******************************************************************************************************".
  ELSE DO:
     IF W_MonEs2 NE "" AND W_MonEs3 EQ "" THEN
        W_MonEs2 = W_MonEs2 + " *******************************************************************************************************".
  END.
  DEFINE VAR NomCiudad AS CHARACTER FORMAT "X(25)".
  DEFINE VAR FecApeD AS CHARACTER FORMAT "X(2)".
  DEFINE VAR FecApeM AS CHARACTER FORMAT "X(2)".
  DEFINE VAR FecApeA AS CHARACTER FORMAT "X(4)".
  DEFINE VAR FecVenD AS CHARACTER FORMAT "X(2)".
  DEFINE VAR FecVenM AS CHARACTER FORMAT "X(2)".
  DEFINE VAR FecVenA AS CHARACTER FORMAT "X(4)".
  DEFINE VAR W_Monto AS CHARACTER FORMAT "X(15)".

  ASSIGN FecApeD = SUBSTRING(STRING(Ahorros.Fec_Apertura),1,2) 
         FecApeM = SUBSTRING(STRING(Ahorros.Fec_Apertura),4,5) 
         FecApeA = "20" + SUBSTRING(STRING(Ahorros.Fec_Apertura),7,10)
         FecVenD = SUBSTRING(STRING(Ahorros.Fec_Vencimiento),1,2) 
         FecVenM = SUBSTRING(STRING(Ahorros.Fec_Vencimiento),4,5) 
         FecVenA = "20" + SUBSTRING(STRING(Ahorros.Fec_Vencimiento),7,10)
         W_Monto = TRIM(STRING(Ahorros.Sdo_disponible,">>>,>>>,>>>,>>9.99")).


  FIND Agencias WHERE Agencias.Agencia EQ Ahorros.Agencia NO-LOCK NO-ERROR.
  IF AVAILABLE Agencias THEN DO:
      
      /*FIND  Ubicacion WHERE Ubicacion.Ubicacion BEGINS SUBSTRING("05001000",1,5) AND Ubicacion.Tipo EQ "C" NO-LOCK NO-ERROR.
      IF AVAILABLE Ubicacion THEN NomCiudad = Ubicacion.Nombre.*/
      FIND Ubicacion WHERE Agencias.Ciudad EQ Ubicacion.Ubicacion.
 /**/ IF AVAILABLE (Ubicacion) THEN 
          ASSIGN NomCiudad = Ubicacion.Nombre.  /**/

  END.

      DISPLAY
          /*Ahorros.Cue_Ahorros AT ROW 5 COL 85 /*9,100*/
          W_NomCli AT ROW 7 COL 15      /*12,19*/
          W_Nit    AT ROW 9 COL 83 
          Clientes.Tel_Residencia AT ROW 10.5 COL 19
          Clientes.DIR_Residencia AT ROW 10.5 COL 70
          W_MonEs1  AT ROW 12 COL 20
          W_MonEs2 AT ROW 13 COL 5
          Ahorros.sdo_disponible AT ROW 14 COL 77
          Ahorros.Tasa  AT ROW 16 COL 63
          Tasa2         AT ROW 17.6 COL 1
          Pagadero      AT ROW 17.6 COL 39
          Ahorros.Fec_Apertura AT ROW 19 COL 20
          Ahorros.Fec_Vencimiento AT ROW 19 COL 65
          Ahorros.Plazo           AT ROW 19 COL 90
          NomCiudad     AT ROW 21 COL 10
          W_Nom_Agencia AT ROW 21 COL 50*/

          NomCiudad     AT ROW 9.5  COL 1
          FecApeD       AT ROW 9.5  COL 56
          FecApeM       AT ROW 9.5  COL 61
          FecApeA       AT ROW 9.5  COL 68
          FecVenD       AT ROW 9.5  COL 82
          FecVenM       AT ROW 9.5  COL 87
          FecVenA       AT ROW 9.5  COL 94
          W_Monto       AT ROW 12.5 COL 1
          W_MonEs1      AT ROW 12.5 COL 24
          W_MonEs2      AT ROW 13.5 COL 24
          Ahorros.Plazo AT ROW 20   COL 1
          Ahorros.Tasa  AT ROW 20   COL 12
          Tasa2         AT ROW 20   COL 41
          Pagadero      AT ROW 20   COL 75
          W_NomCli      AT ROW 23   COL 1

      WITH /*10 down*/ FRAME F_Movimiento.

  OUTPUT CLOSE.
  IF W_Dispositivo = "P" THEN  
     RUN Pantalla2 IN W_Manija (INPUT Listado).
     /*RUN Ptlla_Imprimir.R(INPUT listado).*/
  IF W_Dispositivo = "I" THEN DO:
      RUN adecomm/_osprint.r ( INPUT  ?, INPUT listado,INPUT 8,INPUT  1,INPUT  1,
                               INPUT  99999,OUTPUT W_Rpta).
  END.
  ELSE
    IF W_Dispositivo <> "A" THEN
       OS-DELETE VALUE(LISTADO).
