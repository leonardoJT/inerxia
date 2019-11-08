 
 
 /*********************************************************************
 *                                                                    *
 *                                                                    *
 *                                                                    *
 *              CREADO POR: WILLIAM MARTINEZ RUIZ                     *
 *                          31/10/2008                                *
 *                                                                    *
 *                                                                    *
 *                                                                    *
 **********************************************************************/
 
 {Incluido\VARIABLE.I "SHARED"}
 RUN Rutinas.r PERSISTENT SET W_Manija.

 DEFINE INPUT PARAMETER W_Nit   LIKE Clientes.Nit.
 DEFINE INPUT PARAMETER W_Cue   LIKE Ahorros.Cue_Ahorros.

 DEFINE VARIABLE W_TasPer LIKE Ahorros.Tasa.
 DEFINE VARIABLE W_Rpta             AS LOGICAL. 

 /* TITULAR */
 DEFINE VARIABLE W_NomCli       AS CHARACTER FORMAT "X(60)".
 DEFINE VARIABLE W_RolCli       AS CHARACTER INITIAL "T"  NO-UNDO.
 DEFINE VARIABLE W_NitCli       AS CHARACTER FORMAT "99999999999" NO-UNDO.
 DEFINE VARIABLE W_TiDocCli     AS CHARACTER FORMAT "X(5)".
 DEFINE VARIABLE W_TelCli       AS CHARACTER FORMAT "X(20)".
 /* AUTORIZADO 1 */
 DEFINE VARIABLE W_NomAut1      AS CHARACTER FORMAT "x(60)"  NO-UNDO.
 DEFINE VARIABLE W_RolAut1      AS CHARACTER INITIAL ""  NO-UNDO.
 DEFINE VARIABLE W_TipDocAut1   AS CHARACTER NO-UNDO.
 DEFINE VARIABLE W_NitAut1      AS CHARACTER FORMAT "x(14)" NO-UNDO.
 DEFINE VARIABLE W_TelAut1      AS CHARACTER FORMAT "x(14)" NO-UNDO.
 /* AUTORIZADO 2 */
 DEFINE VARIABLE W_NomAut2      AS CHARACTER FORMAT "x(60)"  NO-UNDO.
 DEFINE VARIABLE W_RolAut2      AS CHARACTER INITIAL ""  NO-UNDO.
 DEFINE VARIABLE W_TipDocAut2   AS CHARACTER NO-UNDO.
 DEFINE VARIABLE W_NitAut2      AS CHARACTER FORMAT "x(14)" NO-UNDO.
 DEFINE VARIABLE W_TelAut2      AS CHARACTER FORMAT "x(14)" NO-UNDO.
 /* AUTORIZADO 3 */
 DEFINE VARIABLE W_NomAut3      AS CHARACTER FORMAT "x(60)"  NO-UNDO.
 DEFINE VARIABLE W_RolAut3      AS CHARACTER INITIAL ""  NO-UNDO.
 DEFINE VARIABLE W_TipDocAut3   AS CHARACTER NO-UNDO.
 DEFINE VARIABLE W_NitAut3      AS CHARACTER FORMAT "x(14)" NO-UNDO.
 DEFINE VARIABLE W_TelAut3      AS CHARACTER FORMAT "x(14)" NO-UNDO.

 DEFINE VAR W_Tipo    AS CHARACTER FORMAT "X(6)".
 DEFINE VAR Mensaje   AS CHARACTER INITIAL "".  
 DEFINE VAR procname  AS CHARACTER FORMAT "X(132)" INITIAL "temporal.txt".
 DEFINE VAR W_Titulo  AS CHARACTER FORMAT "X(40)".
 DEFINE VAR Listado   AS CHARACTER INITIAL "".
 DEFINE VAR FecApeD   AS CHARACTER FORMAT "X(2)".
 DEFINE VAR FecApeM   AS CHARACTER FORMAT "X(2)".
 DEFINE VAR FecApeA   AS CHARACTER FORMAT "X(4)".
 DEFINE VARIABLE FecModD AS CHARACTER FORMAT "X(2)".
 DEFINE VARIABLE FecModA AS CHARACTER FORMAT "X(4)".
 DEFINE VARIABLE FecModM AS CHARACTER FORMAT "X(2)".
 DEFINE VARIABLE W_Notas AS CHARACTER FORMAT "X(90)"  NO-UNDO.
 DEFINE VARIABLE W_NroTRF AS CHARACTER   NO-UNDO.

 DEFINE VARIABLE W_TipTRF AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE W_Ind AS CHARACTER INITIAL " " NO-UNDO.
 DEFINE VARIABLE W_Con AS CHARACTER INITIAL " " NO-UNDO.
 DEFINE VARIABLE W_Alt AS CHARACTER INITIAL " " NO-UNDO.
  
 DEFINE FRAME F_Movimiento
  WITH 10 DOWN size 150 by 10 FRAME F_Movimiento USE-TEXT NO-BOX NO-LABEL STREAM-IO.
  
  Listado = W_pathspl + "Informe" + TRIM(string(W_cue)).

  ASSIGN W_NitCli = W_Nit.

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

  
  /* HALLA INFORMACION CLIENTE */
  W_NomCli = "".
  FIND FIRST Clientes WHERE Clientes.nit EQ W_Nit NO-LOCK NO-ERROR.
  IF AVAILABLE(Clientes) THEN 
    W_Nomcli = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
    W_TelCli = clientes.Tel_Residencia.
    W_TiDocCli = string(clientes.Tipo_Identificacion).


  /* HALLA FECHA APERTURA */
  FIND Ahorros WHERE Ahorros.Nit EQ W_Nit AND Ahorros.Cue_Ahorros EQ W_Cue NO-LOCK NO-ERROR.
  IF AVAILABLE Ahorros THEN DO:
    
  ASSIGN FecApeD = SUBSTRING(STRING(Ahorros.Fec_Apertura),1,2) 
         FecApeM = SUBSTRING(STRING(Ahorros.Fec_Apertura),4,5) 
         FecApeA = STRING(YEAR(Ahorros.Fec_Apertura), "9999")
         W_TipTRF = STRING(ahorros.trf)
         W_Notas = Ahorros.TRF_Notas
         W_NroTRF = SUBSTRING(TRF_Notas, 1, 4).
  END. 
  

   /* HALLA TIPO TRF */
  IF W_TipTRF = "1" THEN
    W_Ind = "X".
  IF W_TipTRF = "2" THEN
    W_Con = "X".
  IF W_TipTRF = "3" THEN
    W_Alt = "X".


  /* HALLA FECHA MODIFICACION (IMPRESION) */
              
    ASSIGN FecModD = STRING(DAY(TODAY), "99").
    ASSIGN FecModA = /*"20" + */STRING(YEAR(TODAY), "9999").
    ASSIGN FecModM = STRING(MONTH(TODAY), "99").


  /*  HALLA LOS AUTORIZADOS  */
  DEFINE VARIABLE W_Control AS INTEGER     NO-UNDO.
  REPEAT W_Control = 1 TO 3:
        FIND NEXT relaciones WHERE relaciones.nit = W_Nit AND W_Cue = relaciones.cuenta AND relaciones.Cod_relacion = 7 NO-ERROR.
        IF AVAILABLE relaciones THEN DO:
            CASE W_Control:
                WHEN 1 THEN DO:
                    ASSIGN W_NitAut1 = relaciones.nit_Relacion.
                    FIND FIRST clientes WHERE clientes.nit = W_NitAut1 NO-ERROR.
                         IF AVAILABLE clientes THEN DO:
                             ASSIGN W_NomAut1 = trim(clientes.Nombre + " " + clientes.Apellido1 + " " + clientes.Apellido2).
                             ASSIGN W_RolAut1 = relaciones.Descripcion.
                             ASSIGN W_TipDocAut1 = string(clientes.Tipo_Identificacion).
                             ASSIGN W_TelAut1 = clientes.Tel_Residencia.
                         END.
                END.
                WHEN 2 THEN DO:
                    ASSIGN W_NitAut2 = relaciones.nit_Relacion.
                    FIND FIRST clientes WHERE clientes.nit = W_NitAut2 NO-ERROR.
                         IF AVAILABLE clientes THEN DO:
                             ASSIGN W_NomAut2 = trim(clientes.Nombre + " " + clientes.Apellido1 + " " + clientes.Apellido2).
                             ASSIGN W_RolAut2 = relaciones.Descripcion.
                             ASSIGN W_TipDocAut2 = string(clientes.Tipo_Identificacion).
                             ASSIGN W_TelAut2 = clientes.Tel_Residencia.
                         END.
                END.
                WHEN 3 THEN DO:
                    ASSIGN W_NitAut3 = relaciones.nit_Relacion.
                    FIND FIRST clientes WHERE clientes.nit = W_NitAut3 NO-ERROR.
                         IF AVAILABLE clientes THEN DO:
                             ASSIGN W_NomAut3 = trim(clientes.Nombre + " " + clientes.Apellido1 + " " + clientes.Apellido2).
                             ASSIGN W_RolAut3 = relaciones.Descripcion.
                             ASSIGN W_TipDocAut3 = string(clientes.Tipo_Identificacion).
                             ASSIGN W_TelAut3 = clientes.Tel_Residencia.
                         END.
                END.
            END CASE.
        END.
  END.

  /*
  FIND FIRST relaciones WHERE relaciones.nit = W_Nit AND W_Cue = relaciones.cuenta AND relaciones.Cod_relacion = 7.
       ASSIGN W_NitBen = relaciones.nit_Relacion.

  FIND FIRST clientes WHERE clientes.nit = W_NitBen NO-ERROR.
       IF AVAILABLE clientes THEN DO:
           ASSIGN W_NomBen = trim(clientes.Nombre + " " + clientes.Apellido1 + " " + clientes.Apellido2).
           ASSIGN W_TelBen = clientes.Tel_Residencia.
           ASSIGN W_DirBen = clientes.Dir_Residencia.
       END.
   */      



      DISPLAY
         
            W_Cue                       AT ROW 4     COL 67

            W_NroTRF                    AT ROW 11    COL 79

            W_Ind                       AT ROW 17    COL 45
            W_Con                       AT ROW 17    COL 54
            W_Alt                       AT ROW 17    COL 63
            
            FecApeD                     AT ROW 17    COL 70
            FecApeM                     AT ROW 17    COL 74
            FecApeA                     AT ROW 17    COL 79
            
            FecModD                     AT ROW 17    COL 85
            FecModM                     AT ROW 17    COL 90
            FecModA                     AT ROW 17    COL 94

            W_NomCli                    AT ROW 20    COL 14
            W_RolCli                    AT ROW 20    COL 64
            W_TiDocCli                  AT ROW 20    COL 70
            W_NitCli                    AT ROW 20    COL 76
            W_TelCli                    AT ROW 20    COL 86
            
               
            W_NomAut1                   AT ROW 21.5  COL 14
            W_RolAut1                   AT ROW 21.5  COL 64
            W_TipDocAut1                AT ROW 21.5  COL 70
            W_NitAut1                   AT ROW 21.5  COL 76
            W_TelAut1                   AT ROW 21.5  COL 86
            
            W_NomAut2                   AT ROW 23.5    COL 14
            W_RolAut2                   AT ROW 23.5    COL 64
            W_TipDocAut2                AT ROW 23.5    COL 70
            W_NitAut2                   AT ROW 23.5    COL 76
            W_TelAut2                   AT ROW 23.5    COL 86
                                        
            W_NomAut3                   AT ROW 24.5    COL 14
            W_RolAut3                   AT ROW 24.5    COL 64
            W_TipDocAut3                AT ROW 24.5    COL 70
            W_NitAut3                   AT ROW 24.5    COL 76
            W_TelAut3                   AT ROW 24.5    COL 86

            W_Notas                     AT ROW 27      COL 14


      WITH /*10 down*/ FRAME F_Movimiento.

  OUTPUT CLOSE.
  IF W_Dispositivo = "P" THEN  
     RUN Pantalla2 IN W_Manija (INPUT Listado).
     /*RUN Ptlla_Imprimir.R(INPUT listado).*/
  IF W_Dispositivo = "I" THEN DO:
      RUN _osprint.r ( INPUT  ?, INPUT listado,INPUT 8,INPUT  1,INPUT  1,
                               INPUT  99999,OUTPUT W_Rpta).
  END.
  ELSE
    IF W_Dispositivo <> "A" THEN
       OS-DELETE VALUE(LISTADO).
