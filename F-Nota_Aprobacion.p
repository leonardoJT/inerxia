 {Incluido\VARIABLE.I "SHARED"}
 DEFINE INPUT PARAMETER W_NumSol LIKE Solicitud.Num_Solicitud.
 DEFINE INPUT PARAMETER W_NitSol LIKE Solicitud.Nit.
 DEFINE INPUT PARAMETER W_UsuSol LIKE Usuarios.Usuario.
 DEFINE INPUT PARAMETER W_InsSol LIKE Mov_Instancias.Instancia.
 
 DEFINE VAR W_Rpta             AS   LOGICAL. 
 DEFINE VAR W_Tipo    AS CHARACTER FORMAT "X(6)".
 DEFINE VAR Mensaje   AS CHARACTER INITIAL "".  
 DEFINE VAR procname  AS CHARACTER FORMAT "X(132)" INITIAL "temporal.txt".
 DEFINE VAR W_Titulo  AS CHARACTER FORMAT "X(40)".
 DEFINE VAR Listado   AS CHARACTER INITIAL "".

 DEFINE VAR W_NomGar AS CHARACTER FORMAT "X(15)".
 DEFINE VAR W_NomUsu LIKE Usuarios.Nombre.
 DEFINE VAR W_DesSol AS CHARACTER FORMAT "X(500)".
 DEFINE VAR W_Nom AS CHARACTER FORMAT "X(55)".
 DEFINE VAR W_Tip AS CHARACTER FORMAT "X(30)".
 DEFINE VAR W_Per AS CHARACTER FORMAT "X(30)".
 DEFINE VAR W_Pla AS CHARACTER FORMAT "X(30)".
 DEFINE VAR W_TPr AS CHARACTER FORMAT "X(30)".
 DEFINE VAR lin_1 AS CHARACTER FORMAT "X(100)".
 DEFINE VAR lin_2 AS CHARACTER FORMAT "X(100)".
 DEFINE VAR lin_3 AS CHARACTER FORMAT "X(100)".
 DEFINE VAR lin_4 AS CHARACTER FORMAT "X(100)".
 DEFINE VAR lin_5 AS CHARACTER FORMAT "X(100)".

 FIND Usuarios WHERE Usuarios.Usuario EQ W_UsuSol NO-LOCK NO-ERROR.
 IF AVAILABLE Usuarios THEN W_NomUsu = W_UsuSol + " - " + Usuarios.Nombre.
 
 FIND Solicitud WHERE
      Solicitud.Num_Solicitud EQ W_NumSol AND 
      Solicitud.Nit           EQ W_NitSol
      NO-LOCK NO-ERROR.
 IF NOT AVAILABLE Solicitud THEN DO:
    MESSAGE "No se encuentra la solicitud a la cual" SKIP
            "se le sacará la nota de aprobación." SKIP(1)
            "Consulte con el administrador"
            VIEW-AS ALERT-BOX TITLE "Nota de Aprobación".
    RETURN ERROR.
 END.
 ELSE DO:
   CASE Solicitud.Per_Pago:
     WHEN 1 THEN W_Pla = STRING(Solicitud.Plazo) + " Semanas".
     WHEN 3 THEN W_Pla = STRING(Solicitud.Plazo) + " Quincenas".
     WHEN 4 THEN W_Pla = STRING(Solicitud.Plazo) + " Meses".
   END CASE.
   IF Solicitud.FOR_Interes EQ 1 THEN W_Tpr = "Interes Vencido".
   ELSE W_Tpr = "Interes Anticipado".
   FIND Mov_Instancias WHERE 
        Mov_Instancias.Instancia EQ W_InsSol AND
        Mov_Instancias.Nit       EQ W_NitSol AND
        Mov_Instancias.Num_Solicitud EQ W_NumSol NO-LOCK NO-ERROR.
   IF AVAILABLE Mov_Instancias THEN 
      ASSIGN Lin_1 = SUBSTRING(Mov_Instancias.Descripcion,1,100)
             Lin_2 = SUBSTRING(Mov_Instancias.Descripcion,101,100)
             Lin_3 = SUBSTRING(Mov_Instancias.Descripcion,201,100)
             Lin_4 = SUBSTRING(Mov_Instancias.Descripcion,301,100)
             Lin_5 = SUBSTRING(Mov_Instancias.Descripcion,401,100).
   FIND Pro_Creditos WHERE
        Pro_Creditos.Cod_Credito EQ Solicitud.Cod_Credito AND
        Pro_Creditos.Tip_Credito EQ Solicitud.Tip_Credito AND 
        Pro_Creditos.Estado EQ 1 NO-LOCK NO-ERROR.
   IF AVAILABLE Pro_Creditos THEN DO:
      CASE Pro_Creditos.Tip_Credito:
         WHEN 1 THEN W_Tip = "Consumo".
         WHEN 2 THEN W_Tip = "Comercial".
         WHEN 3 THEN W_Tip = "Hipotecario".
         WHEN 4 THEN W_Tip = "Microcredito".
      END CASE.
   END.
 END.
 FIND Clientes WHERE Clientes.Nit EQ Solicitud.Nit NO-LOCK NO-ERROR.
 IF NOT AVAILABLE Clientes THEN  DO:
    MESSAGE "No se encuentra el cliente al cual" SKIP
            "se le sacará la nota de aprobación." SKIP(1)
            "Consulte con el administrador"
            VIEW-AS ALERT-BOX TITLE "Nota de Aprobación".
    RETURN ERROR.
 END.
 W_Nom = Clientes.Nit + " - " + Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
 Listado = W_PathSpl + "L_Aprobacion.Lst".

  DEFINE FRAME F-Encabezado
    HEADER
        "Cliente       : "            AT COL 2  ROW 2
        W_Nom                         AT COL 20 ROW 2
        "Fecha         : "            AT COL 65 ROW 2
        TODAY                         AT COL 85 ROW 2
        "Tel.Residencia: "            AT COL 2  ROW 3
        Clientes.Tel_Residencia       AT COL 20 ROW 3
        "Hora          : "            AT COL 65 ROW 3 STRING(TIME,"HH:MM:SS") SKIP
        "Dir.Residencia: "            AT COL 2  ROW 4
        Clientes.Dir_Residencia       AT COL 20 ROW 4
        "Tel.Comercial : "            AT COL 2  ROW 5
        Clientes.Tel_Comercial        AT COL 20 ROW 5
        "Usuario Respo.: "            AT COL 2  ROW 6
        W_NomUsu                      AT COL 20 ROW 6
     "-------------------------------------------------------------------------------------------------------------" AT COL 1 ROW 7
/*     "Numero Solicitud: "      AT COL 1  ROW 8
     Solicitud.Num_Solicitud   AT COL 20 ROW 8 
     "Producto Crédito: "      AT COL 1  ROW 9
     Pro_Creditos.Nom_Producto AT COL 20 ROW 9 FORMAT "X(40)"
     "Tipo de Producto: "      AT COL 65 ROW 9
     W_Tip                     AT COL 85 ROW 9 FORMAT "X(30)" SKIP
     "Monto Solicitado: "      AT COL 1  ROW 10
     Solicitud.Monto           AT COL 20 ROW 10
     "Plazo           : "      AT COL 65 ROW 10
     W_Pla                     AT COL 85 ROW 10
     "Tasa del Periodo: "      AT COL 1  ROW 11
     Solicitud.Tasa            AT COL 20 ROW 11
     "Tipo Interes    : "      AT COL 65 ROW 11
     W_Tpr                     AT COL 85 ROW 11
     "-------------------------------------------------------------------------------------------------------------" AT COL 1 ROW 12
     "Nota de Aprobación de la Solicitud" AT COL 1 ROW 13
     Lin_1 AT COL 1 ROW 14
     Lin_2 AT COL 1 ROW 15
     Lin_3 AT COL 1 ROW 16
     Lin_4 AT COL 1 ROW 17 
     Lin_5 AT COL 1 ROW 18
     "-------------------------------------------------------------------------------------------------------------" AT COL 1 ROW 19
     "GARANTIAS DE LA SOLICITUD" AT COL 1 ROW 20
     "Tip.Garantia      Id.Garantia              Nombre Garantia                                 Val.Garantia" AT COL 1 ROW 21*/
  WITH 1 DOWN WIDTH 300 FRAME F-Encabezado NO-LABEL NO-BOX NO-UNDERLINE PAGE-TOP 
                              USE-TEXT.
 
/*  DEFINE FRAME F_Garantias 
     W_NomGar                       AT 1
     Garantias.Identificacion_Bien  AT 17 FORMAT "X(12)"
     Garantias.Nom_Bien             AT 40 FORMAT "X(40)"
     Garantias.Val_Bien             AT 100
  WITH 10 DOWN size 150 by 10 FRAME F_Garantias USE-TEXT NO-BOX NO-LABEL STREAM-IO.*/

  DEFINE FRAME F-Totales
    HEADER
    "Aprobo :______________________"  AT COL 2  ROW 4
    "Firma  :______________________"  AT COL 42 ROW 4
    "C.C. NIT."                       AT COL 48 ROW 5
  WITH WIDTH 150 FRAME F-Totales PAGE-BOTTOM NO-LABEL NO-BOX NO-UNDERLINE USE-TEXT.


  DEFINE VAR W_Dispositivo AS CHARACTER INITIAL "P".  
  RUN P-DisPos IN W_Manija (INPUT-OUTPUT Listado, INPUT-OUTPUT W_Dispositivo).
  IF W_Dispositivo = "" THEN
     RETURN.
     
  OUTPUT TO VALUE(listado) NO-ECHO PAGED PAGE-SIZE 22.
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Totales.  
  DISPLAY 
       "Numero Solicitud: "      AT COL 1  ROW 8
     Solicitud.Num_Solicitud   AT COL 20 ROW 8 
     "Producto Crédito: "      AT COL 1  ROW 9
     Pro_Creditos.Nom_Producto AT COL 20 ROW 9 FORMAT "X(40)"
     "Tipo de Producto: "      AT COL 65 ROW 9
     W_Tip                     AT COL 85 ROW 9 FORMAT "X(30)" SKIP
     "Monto Solicitado: "      AT COL 1  ROW 10
     Solicitud.Monto           AT COL 20 ROW 10
     "Plazo           : "      AT COL 65 ROW 10
     W_Pla                     AT COL 85 ROW 10
     "Tasa del Periodo: "      AT COL 1  ROW 11
     Solicitud.Tasa            AT COL 20 ROW 11
     "Tipo Interes    : "      AT COL 65 ROW 11
     W_Tpr                     AT COL 85 ROW 11
     "-------------------------------------------------------------------------------------------------------------" AT COL 1 ROW 12
     "Nota de Aprobación de la Solicitud" AT COL 1 ROW 13
     Lin_1 AT COL 1 ROW 14
     Lin_2 AT COL 1 ROW 15
     Lin_3 AT COL 1 ROW 16
     Lin_4 AT COL 1 ROW 17 
     Lin_5 AT COL 1 ROW 18
     "-------------------------------------------------------------------------------------------------------------" AT COL 1 ROW 19
     "GARANTIAS DE LA SOLICITUD" AT COL 1 ROW 20
     "Tip.Garantia      Id.Garantia              Nombre Garantia                                 Val.Garantia" AT COL 1 ROW 21
  WITH FRAME F_SOl NO-LABELS USE-TEXT NO-BOX WIDTH 132.
  RUN Garantias_List.
  OUTPUT CLOSE.
  IF W_Dispositivo = "P" THEN  
    RUN Pantalla IN W_Manija (INPUT listado).
  IF W_Dispositivo = "I" THEN
      RUN adecomm/_osprint.r ( INPUT  ?, INPUT listado,INPUT 2,INPUT  1,INPUT  1,
                               INPUT  99999,OUTPUT W_Rpta).
  ELSE
    IF W_Dispositivo <> "A" THEN
       OS-DELETE VALUE(LISTADO).

PROCEDURE Garantias_List:
  FOR EACH Garantias WHERE
           Garantias.Cod_Credito EQ Solicitud.Cod_Credito AND
           Garantias.Tip_Credito EQ Solicitud.Tip_Credito AND
           Garantias.Num_Solicitud EQ Solicitud.Num_Solicitud AND
           Garantias.Nit           EQ Solicitud.Nit
           NO-LOCK:
     CASE Garantias.Tipo_Garantia:
       WHEN 1 THEN W_NomGar = "Propiedad".
       WHEN 2 THEN W_NomGar = "Vehículo".
       WHEN 3 THEN W_NomGar = "Inversión".
     END CASE.
     DISPLAY W_NomGar                       AT 1
             Garantias.Identificacion_Bien  AT 17 FORMAT "X(12)"
             Garantias.Nom_Bien             AT 40 FORMAT "X(40)"
             Garantias.Val_Bien             AT 100
       WITH FRAME F_12 NO-LABELS WIDTH 132. /* 10 DOWN size 150 by 10 FRAME F_Garantias USE-TEXT NO-BOX NO-LABEL STREAM-IO.*/
  END.
END PROCEDURE.
