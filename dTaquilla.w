&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dTables 
/*------------------------------------------------------------------------

  File:  

  Description: from DATA.W - Template For SmartData objects in the ADM

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Modified:     February 24, 1999
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */



{incluido\iGetSdo.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Global-define DATA-LOGIC-PROCEDURE .p

&Scoped-define PROCEDURE-TYPE SmartDataObject
&Scoped-define DB-AWARE yes

&Scoped-define ADM-SUPPORTED-LINKS Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target


/* Db-Required definitions. */
&IF DEFINED(DB-REQUIRED) = 0 &THEN
    &GLOBAL-DEFINE DB-REQUIRED TRUE
&ENDIF
&GLOBAL-DEFINE DB-REQUIRED-START   &IF {&DB-REQUIRED} &THEN
&GLOBAL-DEFINE DB-REQUIRED-END     &ENDIF


&Scoped-define QUERY-NAME Query-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Taquilla

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Agencia Age_Destino Age_Fuente Autorizo Cod_Compensa Cod_Operacion~
 Cod_Producto Cod_Segmento Contabilizar Cta_Contra Cuenta Descripcion~
 Duracion Estacion Estado Est_Linea Fec_Exonerado Fec_GNU Fec_GNUM~
 Fec_Reportada Fec_Sospechosa Fec_Transaccion Hora_Transaccion Id_Exonerada~
 Id_NUD Id_NUM Id_NUS Id_RepFiscal Id_Sospechosa Naturaleza Nit Nro_Cuenta~
 Nro_Transaccion Num_Documento Num_Retcheque Tip_Producto Ult_Instancia~
 Usuario Usu_Exonerado Usu_GNU Usu_GNUM Usu_Reportada Usu_Sospechosa~
 Val_Cheque Val_Efectivo
&Scoped-define ENABLED-FIELDS-IN-Taquilla Agencia Age_Destino Age_Fuente ~
Autorizo Cod_Compensa Cod_Operacion Cod_Producto Cod_Segmento Contabilizar ~
Cta_Contra Cuenta Descripcion Duracion Estacion Estado Est_Linea ~
Fec_Exonerado Fec_GNU Fec_GNUM Fec_Reportada Fec_Sospechosa Fec_Transaccion ~
Hora_Transaccion Id_Exonerada Id_NUD Id_NUM Id_NUS Id_RepFiscal ~
Id_Sospechosa Naturaleza Nit Nro_Cuenta Nro_Transaccion Num_Documento ~
Num_Retcheque Tip_Producto Ult_Instancia Usuario Usu_Exonerado Usu_GNU ~
Usu_GNUM Usu_Reportada Usu_Sospechosa Val_Cheque Val_Efectivo 
&Scoped-Define DATA-FIELDS  Agencia Age_Destino Age_Fuente Autorizo Cod_Compensa Cod_Operacion~
 Cod_Producto Cod_Segmento Contabilizar Cta_Contra Cuenta Descripcion~
 Duracion Estacion Estado Est_Linea Fec_Exonerado Fec_GNU Fec_GNUM~
 Fec_Reportada Fec_Sospechosa Fec_Transaccion Hora_Transaccion Id_Exonerada~
 Id_NUD Id_NUM Id_NUS Id_RepFiscal Id_Sospechosa Naturaleza Nit Nro_Cuenta~
 Nro_Transaccion Num_Documento Num_Retcheque Tip_Producto Ult_Instancia~
 Usuario Usu_Exonerado Usu_GNU Usu_GNUM Usu_Reportada Usu_Sospechosa~
 Val_Cheque Val_Efectivo FEfeChe FValConsignacion FValRetiro
&Scoped-define DATA-FIELDS-IN-Taquilla Agencia Age_Destino Age_Fuente ~
Autorizo Cod_Compensa Cod_Operacion Cod_Producto Cod_Segmento Contabilizar ~
Cta_Contra Cuenta Descripcion Duracion Estacion Estado Est_Linea ~
Fec_Exonerado Fec_GNU Fec_GNUM Fec_Reportada Fec_Sospechosa Fec_Transaccion ~
Hora_Transaccion Id_Exonerada Id_NUD Id_NUM Id_NUS Id_RepFiscal ~
Id_Sospechosa Naturaleza Nit Nro_Cuenta Nro_Transaccion Num_Documento ~
Num_Retcheque Tip_Producto Ult_Instancia Usuario Usu_Exonerado Usu_GNU ~
Usu_GNUM Usu_Reportada Usu_Sospechosa Val_Cheque Val_Efectivo 
&Scoped-Define MANDATORY-FIELDS  Estado
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dtaquilla.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH Taquilla NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Taquilla NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Taquilla
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Taquilla


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValConsignacion dTables  _DB-REQUIRED
FUNCTION getValConsignacion RETURNS DECIMAL
    (INPUT piCodOpe AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValRetiro dTables  _DB-REQUIRED
FUNCTION getValRetiro RETURNS DECIMAL
    (INPUT piCodOpe AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Taquilla SCROLLING.
&ANALYZE-RESUME
{&DB-REQUIRED-END}


/* ************************  Frame Definitions  *********************** */


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataObject
   Allow: Query
   Frames: 0
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER DB-AWARE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW dTables ASSIGN
         HEIGHT             = 1.62
         WIDTH              = 46.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB dTables 
/* ************************* Included-Libraries *********************** */

{src/adm2/data.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW dTables
  VISIBLE,,RUN-PERSISTENT                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY Query-Main
/* Query rebuild information for SmartDataObject Query-Main
     _TblList          = "bdcentral.Taquilla"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.Taquilla.Agencia
"Agencia" "Agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 7.29 yes "Agencia"
     _FldNameList[2]   > bdcentral.Taquilla.Age_Destino
"Age_Destino" "Age_Destino" ? ? "integer" ? ? ? ? ? ? yes ? no 14.14 yes "Ofc. Destino"
     _FldNameList[3]   > bdcentral.Taquilla.Age_Fuente
"Age_Fuente" "Age_Fuente" ? ? "integer" ? ? ? ? ? ? yes ? no 13.29 yes "Ofc. Fuente"
     _FldNameList[4]   > bdcentral.Taquilla.Autorizo
"Autorizo" "Autorizo" ? ? "character" ? ? ? ? ? ? yes ? no 7.57 yes "Autorizo"
     _FldNameList[5]   > bdcentral.Taquilla.Cod_Compensa
"Cod_Compensa" "Cod_Compensa" ? ? "integer" ? ? ? ? ? ? yes ? no 23.57 yes ?
     _FldNameList[6]   > bdcentral.Taquilla.Cod_Operacion
"Cod_Operacion" "Cod_Operacion" ? ? "integer" ? ? ? ? ? ? yes ? no 16.43 yes ?
     _FldNameList[7]   > bdcentral.Taquilla.Cod_Producto
"Cod_Producto" "Cod_Producto" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 yes ?
     _FldNameList[8]   > bdcentral.Taquilla.Cod_Segmento
"Cod_Segmento" "Cod_Segmento" ? ? "integer" ? ? ? ? ? ? yes ? no 19.72 yes ?
     _FldNameList[9]   > bdcentral.Taquilla.Contabilizar
"Contabilizar" "Contabilizar" ? ? "logical" ? ? ? ? ? ? yes ? no 10.43 yes ?
     _FldNameList[10]   > bdcentral.Taquilla.Cta_Contra
"Cta_Contra" "Cta_Contra" ? ? "character" ? ? ? ? ? ? yes ? no 19.43 yes ?
     _FldNameList[11]   > bdcentral.Taquilla.Cuenta
"Cuenta" "Cuenta" ? ? "character" ? ? ? ? ? ? yes ? no 15.14 yes ?
     _FldNameList[12]   > bdcentral.Taquilla.Descripcion
"Descripcion" "Descripcion" ? ? "character" ? ? ? ? ? ? yes ? no 80 yes ?
     _FldNameList[13]   > bdcentral.Taquilla.Duracion
"Duracion" "Duracion" ? ? "integer" ? ? ? ? ? ? yes ? no 8.29 yes ?
     _FldNameList[14]   > bdcentral.Taquilla.Estacion
"Estacion" "Estacion" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ?
     _FldNameList[15]   > bdcentral.Taquilla.Estado
"Estado" "Estado" ? ? "integer" ? ? ? ? ? ? yes ? yes 6.43 yes ?
     _FldNameList[16]   > bdcentral.Taquilla.Est_Linea
"Est_Linea" "Est_Linea" ? ? "integer" ? ? ? ? ? ? yes ? no 15 yes ?
     _FldNameList[17]   > bdcentral.Taquilla.Fec_Exonerado
"Fec_Exonerado" "Fec_Exonerado" ? ? "date" ? ? ? ? ? ? yes ? no 14.43 yes ?
     _FldNameList[18]   > bdcentral.Taquilla.Fec_GNU
"Fec_GNU" "Fec_GNU" ? ? "date" ? ? ? ? ? ? yes ? no 10.29 yes ?
     _FldNameList[19]   > bdcentral.Taquilla.Fec_GNUM
"Fec_GNUM" "Fec_GNUM" ? ? "date" ? ? ? ? ? ? yes ? no 10.43 yes ?
     _FldNameList[20]   > bdcentral.Taquilla.Fec_Reportada
"Fec_Reportada" "Fec_Reportada" ? ? "date" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[21]   > bdcentral.Taquilla.Fec_Sospechosa
"Fec_Sospechosa" "Fec_Sospechosa" ? ? "date" ? ? ? ? ? ? yes ? no 15.86 yes ?
     _FldNameList[22]   > bdcentral.Taquilla.Fec_Transaccion
"Fec_Transaccion" "Fec_Transaccion" ? ? "date" ? ? ? ? ? ? yes ? no 22.57 yes ?
     _FldNameList[23]   > bdcentral.Taquilla.Hora_Transaccion
"Hora_Transaccion" "Hora_Transaccion" ? ? "integer" ? ? ? ? ? ? yes ? no 16.29 yes ?
     _FldNameList[24]   > bdcentral.Taquilla.Id_Exonerada
"Id_Exonerada" "Id_Exonerada" ? ? "logical" ? ? ? ? ? ? yes ? no 12.86 yes ?
     _FldNameList[25]   > bdcentral.Taquilla.Id_NUD
"Id_NUD" "Id_NUD" ? ? "logical" ? ? ? ? ? ? yes ? no 7.14 yes ?
     _FldNameList[26]   > bdcentral.Taquilla.Id_NUM
"Id_NUM" "Id_NUM" ? ? "logical" ? ? ? ? ? ? yes ? no 7.43 yes ?
     _FldNameList[27]   > bdcentral.Taquilla.Id_NUS
"Id_NUS" "Id_NUS" ? ? "logical" ? ? ? ? ? ? yes ? no 7 yes ?
     _FldNameList[28]   > bdcentral.Taquilla.Id_RepFiscal
"Id_RepFiscal" "Id_RepFiscal" ? "Enviada/No Enviada" "logical" ? ? ? ? ? ? yes ? no 12.14 yes ?
     _FldNameList[29]   > bdcentral.Taquilla.Id_Sospechosa
"Id_Sospechosa" "Id_Sospechosa" ? "Si/No" "logical" ? ? ? ? ? ? yes ? no 14.29 yes ?
     _FldNameList[30]   > bdcentral.Taquilla.Naturaleza
"Naturaleza" "Naturaleza" ? ? "character" ? ? ? ? ? ? yes ? no 10.14 yes ?
     _FldNameList[31]   > bdcentral.Taquilla.Nit
"Nit" "Nit" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[32]   > bdcentral.Taquilla.Nro_Cuenta
"Nro_Cuenta" "Nro_Cuenta" ? ? "character" ? ? ? ? ? ? yes ? no 17.14 yes ?
     _FldNameList[33]   > bdcentral.Taquilla.Nro_Transaccion
"Nro_Transaccion" "Nro_Transaccion" ? ? "integer" ? ? ? ? ? ? yes ? no 15.14 yes ?
     _FldNameList[34]   > bdcentral.Taquilla.Num_Documento
"Num_Documento" "Num_Documento" ? ? "character" ? ? ? ? ? ? yes ? no 21.72 yes ?
     _FldNameList[35]   > bdcentral.Taquilla.Num_Retcheque
"Num_Retcheque" "Num_Retcheque" ? ? "character" ? ? ? ? ? ? yes ? no 27.86 yes ?
     _FldNameList[36]   > bdcentral.Taquilla.Tip_Producto
"Tip_Producto" "Tip_Producto" ? ? "integer" ? ? ? ? ? ? yes ? no 15.57 yes ?
     _FldNameList[37]   > bdcentral.Taquilla.Ult_Instancia
"Ult_Instancia" "Ult_Instancia" ? ? "integer" ? ? ? ? ? ? yes ? no 8.43 yes ?
     _FldNameList[38]   > bdcentral.Taquilla.Usuario
"Usuario" "Usuario" ? ? "character" ? ? ? ? ? ? yes ? no 7.29 yes ?
     _FldNameList[39]   > bdcentral.Taquilla.Usu_Exonerado
"Usu_Exonerado" "Usu_Exonerado" ? ? "character" ? ? ? ? ? ? yes ? no 14.86 yes ?
     _FldNameList[40]   > bdcentral.Taquilla.Usu_GNU
"Usu_GNU" "Usu_GNU" ? ? "character" ? ? ? ? ? ? yes ? no 9.14 yes ?
     _FldNameList[41]   > bdcentral.Taquilla.Usu_GNUM
"Usu_GNUM" "Usu_GNUM" ? ? "character" ? ? ? ? ? ? yes ? no 10.86 yes ?
     _FldNameList[42]   > bdcentral.Taquilla.Usu_Reportada
"Usu_Reportada" "Usu_Reportada" ? ? "character" ? ? ? ? ? ? yes ? no 14.43 yes ?
     _FldNameList[43]   > bdcentral.Taquilla.Usu_Sospechosa
"Usu_Sospechosa" "Usu_Sospechosa" ? ? "character" ? ? ? ? ? ? yes ? no 16.29 yes ?
     _FldNameList[44]   > bdcentral.Taquilla.Val_Cheque
"Val_Cheque" "Val_Cheque" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[45]   > bdcentral.Taquilla.Val_Efectivo
"Val_Efectivo" "Val_Efectivo" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.14 yes ?
     _FldNameList[46]   > "_<CALC>"
"getCtrlEfeChe(INPUT RowObject.Cod_Operacion)" "FEfeChe" "Efe/Che" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no "Efec/Cheq"
     _FldNameList[47]   > "_<CALC>"
"getValConsignacion(INPUT RowObject.Cod_Operacion)" "FValConsignacion" "Val. Consig." "->>>,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 14.86 no "Val. Consig."
     _FldNameList[48]   > "_<CALC>"
"getValRetiro(INPUT RowObject.Cod_Operacion)" "FValRetiro" "Val. Retiro" "->>>,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 14.86 no "Val. Retiro"
     _Design-Parent    is WINDOW dTables @ ( 1.15 , 2.57 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dTables 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DATA.CALCULATE dTables  DATA.CALCULATE _DB-REQUIRED
PROCEDURE DATA.CALCULATE :
/*------------------------------------------------------------------------------
  Purpose:     Calculate all the Calculated Expressions found in the
               SmartDataObject.
  Parameters:  <none>
------------------------------------------------------------------------------*/
      ASSIGN 
         rowObject.FEfeChe = (getCtrlEfeChe(INPUT RowObject.Cod_Operacion))
         rowObject.FValConsignacion = (getValConsignacion(INPUT RowObject.Cod_Operacion))
         rowObject.FValRetiro = (getValRetiro(INPUT RowObject.Cod_Operacion))
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI dTables  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValConsignacion dTables  _DB-REQUIRED
FUNCTION getValConsignacion RETURNS DECIMAL
    (INPUT piCodOpe AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  Devuelve el Consignado
            Tipo_Operacion = 1 es Consignacion
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE vdeValor AS DECIMAL INITIAL 0 NO-UNDO.

    FIND Operacion WHERE Operacion.Cod_Operacion EQ piCodOpe NO-LOCK NO-ERROR.
    IF AVAILABLE Operacion THEN DO:
        IF Operacion.Tipo_Operacion EQ 1 THEN /*Consignacion*/
            ASSIGN vdeValor = RowObject.Val_Efectivo + RowObject.Val_Cheque.
    END.

    RETURN vdeValor.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValRetiro dTables  _DB-REQUIRED
FUNCTION getValRetiro RETURNS DECIMAL
    (INPUT piCodOpe AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  Devuelve el valor de Retiro
            Tipo_Operacion = 2 es Retiro
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE vdeValor AS DECIMAL INITIAL 0 NO-UNDO.

    FIND Operacion WHERE Operacion.Cod_Operacion EQ piCodOpe NO-LOCK NO-ERROR.
    IF AVAILABLE Operacion THEN DO:
        IF Operacion.Tipo_Operacion EQ 2 THEN /*Retiro*/
            ASSIGN vdeValor = RowObject.Val_Efectivo + RowObject.Val_Cheque.
    END.
    
    RETURN vdeValor.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

