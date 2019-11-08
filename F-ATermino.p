 {Incluido\VARIABLE.I "SHARED"}
 RUN Rutinas.r PERSISTENT SET W_Manija.

 DEFINE INPUT PARAMETER W_Nit   LIKE Clientes.Nit.
 DEFINE INPUT PARAMETER W_Cue   LIKE Ahorros.Cue_Ahorros.
 DEFINE INPUT PARAMETER W_xtas  LIKE ahorros.tasa.

 DEFINE VARIABLE W_TasPer LIKE Ahorros.Tasa.

 DEFINE VAR W_NomCli           AS CHARACTER FORMAT "X(60)".
 DEFINE VAR W_Rpta             AS LOGICAL. 
 DEFINE VAR W_MonEs            AS CHARACTER FORMAT "X(200)".
 DEFINE VAR W_MonEs1           AS CHARACTER FORMAT "X(80)".
 DEFINE VAR W_MonEs2           AS CHARACTER FORMAT "X(80)".
 DEFINE VAR W_MonEs3           AS CHARACTER FORMAT "X(80)".
 DEFINE VARIABLE W_DirCli AS CHARACTER FORMAT "X(80)".
 DEFINE VARIABLE W_TelCli AS CHARACTER FORMAT "X(20)".
 

 DEFINE VAR W_Tipo    AS CHARACTER FORMAT "X(6)".
 DEFINE VAR Mensaje   AS CHARACTER INITIAL "".  
 DEFINE VAR procname  AS CHARACTER FORMAT "X(132)" INITIAL "temporal.txt".
 DEFINE VAR W_Titulo  AS CHARACTER FORMAT "X(40)".
 DEFINE VAR Listado   AS CHARACTER INITIAL "".
 DEFINE VARIABLE W_NomBen AS CHARACTER FORMAT "X(60)".
 DEFINE VARIABLE W_TelBen AS CHARACTER FORMAT "X(20)".
 DEFINE VARIABLE W_NitBen AS CHARACTER FORMAT "X(60)".
 DEFINE VARIABLE W_DirBen AS CHARACTER FORMAT "X(60)".

  
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
    W_DirCli = clientes.Dir_Residencia.
    W_TelCli = clientes.Tel_Residencia.

  FIND Ahorros WHERE Ahorros.Nit EQ W_Nit AND Ahorros.Cue_Ahorros EQ W_Cue NO-LOCK NO-ERROR.
  IF AVAILABLE Ahorros THEN DO:
      RUN MontoEsc.r (INPUT ROUND(Ahorros.sdo_disponible,0),INPUT 0,OUTPUT W_MonEs).
      W_MonEs = W_MonEs + " * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *".
      RUN PartirValor IN W_Manija (INPUT W_MonEs,INPUT 60,OUTPUT W_MonEs1,OUTPUT W_MonEs2,OUTPUT W_MonEs3). 
      
      DEFINE VARIABLE W_TasNom LIKE Ahorros.Tasa.
      DEFINE VARIABLE W_Per AS INTEGER INITIAL 0 NO-UNDO.

     CASE Ahorros.Per_Liquidacion:
          WHEN 1 THEN
              ASSIGN W_TasNom = (Ahorros.Tasa / 100) / 365
                     W_Per = 365.
          WHEN 2 THEN
              ASSIGN W_TasNom = (Ahorros.Tasa / 100) / 12
                     W_Per = 12.
          WHEN 3 THEN
              ASSIGN W_TasNom = (Ahorros.Tasa / 100) / 4
                     W_Per    = 4.
          WHEN 4 THEN
              ASSIGN W_TasNom = (Ahorros.Tasa / 100) / 2
                     W_per = 2.
          WHEN 5 THEN
              ASSIGN W_TasNom = (Ahorros.Tasa / 100) 
                     W_Per = 1.
/******************************************************************************/
/** Modificado: AGordon 18/04/2008                                           **/
/** Inclusion de Funcion para calculo de Tasas de Interes                    **/
/******************************************************************************/
          WHEN 6 THEN DO:
              IF Ahorros.Plazo GE 360 THEN
                  ASSIGN W_TasNom = (Ahorros.Tasa / 100) 
                         W_Per = 1.
              ELSE 
                  ASSIGN W_TasNom = (Ahorros.Tasa / 100) / (360 / Ahorros.Plazo) 
                         W_Per = (360 / Ahorros.Plazo).
          END.
      END CASE.
      ASSIGN W_TasPer = EXP((1 + (W_TasNom)),W_Per) - 1.
      /*IF Ahorros.For_Liquidacion EQ 1 THEN
          RUN NAEF IN W_ManFin (W_TasNom / 100,INPUT W_Per, OUTPUT W_Tasper).
      ELSE 
          RUN NVEF IN W_ManFin (W_TasNom / 100,INPUT W_Per ,OUTPUT W_Tasper).*/
          /*ASSIGN W_TasPer = W_TasPer * 100.*/
/******************************************************************************/
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
  ASSIGN Tasa2 = W_TasPer * 100.
  IF ahorros.per_liquidacion = 5 THEN tasa2 = W_xtas.

  IF Ahorros.FOR_Liquidacion EQ 1 THEN
     Pagadero = Pagadero + " Anticipado".
  ELSE
     Pagadero = Pagadero + " Vencido".

  /*
  IF W_MonEs2 EQ "" AND W_MonEs3 EQ "" THEN
     W_MonEs1 = W_MonEs1 + " ******************************************************".
  ELSE DO:
     IF W_MonEs2 NE "" AND W_MonEs3 EQ "" THEN
        W_MonEs2 = W_MonEs2 + " ***************************************************".
  END.
  
  */

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

/*      HALLA LOS BENEFICIARIOS, DESHABILITADO POR SOLICITUD 02/11/2008 WILLIAM MARTINEZ
  FIND FIRST relaciones WHERE relaciones.nit = W_Nit AND W_Cue = relaciones.cuenta AND relaciones.Cod_relacion = 6.
       ASSIGN W_NitBen = relaciones.nit_Relacion.


  FIND FIRST clientes WHERE clientes.nit = W_NitBen NO-ERROR.
       IF AVAILABLE clientes THEN DO:
           ASSIGN W_NomBen = trim(clientes.Nombre + " " + clientes.Apellido1 + " " + clientes.Apellido2).
           ASSIGN W_TelBen = clientes.Tel_Residencia.
           ASSIGN W_DirBen = clientes.Dir_Residencia.
       END.
*/       

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

            
          W_Cue                     AT ROW 2      COL 94
          NomCiudad                 AT ROW 8.5    COL 8
          W_NomCli                  AT ROW 11.5   COL 19
          W_nit                     AT ROW 11.5   COL 88
          W_DirCli                  AT ROW 14     COL 19
          W_TelCli                  AT ROW 14     COL 88
          
  /*        /* BENEFICIARIO */
          W_NomBen                  AT ROW 16.5   COL 8
          W_NitBen                  AT ROW 16.5   COL 88
          W_DirBen                  AT ROW 18   COL 8
          W_TelBen                  AT ROW 18   COL 88
   */

          FecApeD                   AT ROW 8.5    COL 101
          FecApeM                   AT ROW 8.5    COL 96
          FecApeA                   AT ROW 8.5    COL 89
            
          W_Monto                   AT ROW 25.5   COL 14
          W_MonEs1                  AT ROW 25.5   COL 45
          W_MonEs2                  AT ROW 27.5   COL 10
          
          Ahorros.Plazo             AT ROW 31     COL 13
          Ahorros.Tasa              AT ROW 31     COL 25 FORMAT ">>>.99"
          Tasa2                     AT ROW 31     COL 43 FORMAT ">>>.99"
          Pagadero                  AT ROW 31     COL 58
          FecVenD                   AT ROW 31     COL 101
          FecVenM                   AT ROW 31     COL 96
          FecVenA                   AT ROW 31     COL 89

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
