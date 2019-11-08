&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"d-Cliente_Sencillo.i"}.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS vTableWin 
/*------------------------------------------------------------------------

  File:

  Description: from viewer.w - Template for SmartDataViewer objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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
   DEFINE VARIABLE W_Estado   AS LOGICAL.
   DEFINE VARIABLE W_Cencos   AS LOGICAL.
   DEFINE VARIABLE W_IngUsu   AS LOGICAL.
   DEFINE VARIABLE W_String   AS CHARACTER.
   DEFINE VAR W_Ok AS LOGICAL.
   DEFINE VARIABLE W_OfiUsu LIKE Agencias.Agencia.

  /* para llamado a Ubicacion*/
  DEFINE VAR P_Ubi LIKE Ubicacion.Ubicacion.
  DEFINE VAR P_NUbi AS CHARACTER FORMAT "X(80)".

       {incluido\Variable.i "SHARED"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "d-Cliente_Sencillo.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Nit RowObject.tipo_identificacion ~
RowObject.Nombre RowObject.Apellido1 RowObject.Apellido2 ~
RowObject.Tipo_Vinculo RowObject.Tel_comercial RowObject.Dir_comercial ~
RowObject.Agencia RowObject.Tipo_cliente 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS Cmb_Agencia Btn_Lugar 
&Scoped-Define DISPLAYED-FIELDS RowObject.Nit RowObject.tipo_identificacion ~
RowObject.Nombre RowObject.Apellido1 RowObject.Apellido2 ~
RowObject.Tipo_Vinculo RowObject.Tel_comercial RowObject.Dir_comercial ~
RowObject.Agencia RowObject.Fec_Ingreso RowObject.Lugar_comercial ~
RowObject.Tipo_cliente 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS W_NomComercial Cmb_Agencia 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Lugar 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 1" 
     SIZE 3 BY .54.

DEFINE VARIABLE Cmb_Agencia AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     DROP-DOWN-LIST
     SIZE 51 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE W_NomComercial AS CHARACTER FORMAT "X(50)":U 
     LABEL "Ciudad" 
     VIEW-AS FILL-IN 
     SIZE 42 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     W_NomComercial AT ROW 9.62 COL 12 COLON-ALIGNED
     Cmb_Agencia AT ROW 1.15 COL 12 COLON-ALIGNED
     RowObject.Nit AT ROW 2.08 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 30 BY .81
          BGCOLOR 15 
     RowObject.tipo_identificacion AT ROW 2.08 COL 57 COLON-ALIGNED
          LABEL "Tipo de Documento"
          VIEW-AS COMBO-BOX INNER-LINES 4
          LIST-ITEMS "C.C","C.E","T.I","NIT","R.C" 
          DROP-DOWN-LIST
          SIZE 6.14 BY 1
          BGCOLOR 15 
     RowObject.Nombre AT ROW 3.96 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 50 BY .81
          BGCOLOR 15 
     RowObject.Apellido1 AT ROW 5.04 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 33 BY .81
          BGCOLOR 15 
     RowObject.Apellido2 AT ROW 6.12 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 33 BY .81
          BGCOLOR 15 
     RowObject.Tipo_Vinculo AT ROW 5.04 COL 48 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Asociado", 1,
"Cliente No Asociado", 2,
"Tercero", 3,
"Proveedor", 4
          SIZE 17.29 BY 3.23
     RowObject.Tel_comercial AT ROW 7.46 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     RowObject.Dir_comercial AT ROW 8.54 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY .81
          BGCOLOR 15 
     Btn_Lugar AT ROW 9.62 COL 56
     RowObject.Agencia AT ROW 10.15 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
     RowObject.Usuario AT ROW 11.23 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.29 BY .81
          BGCOLOR 17 FGCOLOR 17 
     RowObject.Fec_Ingreso AT ROW 10.69 COL 12 COLON-ALIGNED
          LABEL "Fecha Ingreso"
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Lugar_comercial AT ROW 11.23 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 17 FGCOLOR 17 
     RowObject.Tipo_cliente AT ROW 3.12 COL 3 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Natural Mayor de Edad", 1,
"Natural Menor de Edad", 2,
"Jurídica S.A", 3,
"Jurídica C.A", 4
          SIZE 61 BY .69
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "d-Cliente_Sencillo.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {d-Cliente_Sencillo.i}
      END-FIELDS.
   END-TABLES.
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
  CREATE WINDOW vTableWin ASSIGN
         HEIGHT             = 11.38
         WIDTH              = 65.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTableWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       RowObject.Agencia:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Fec_Ingreso IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.Lugar_comercial IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX RowObject.tipo_identificacion IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Usuario IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN W_NomComercial IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main vTableWin
ON GO OF FRAME F-Main
DO:
  APPLY 'leave' TO RowObject.Lugar_Comercial.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Apellido1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Apellido1 vTableWin
ON LEAVE OF RowObject.Apellido1 IN FRAME F-Main /* Primer Apellido */
DO:
    SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Apellido2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Apellido2 vTableWin
ON LEAVE OF RowObject.Apellido2 IN FRAME F-Main /* Segundo Apellido */
DO:
    SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Lugar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Lugar vTableWin
ON CHOOSE OF Btn_Lugar IN FRAME F-Main /* Button 1 */
DO:
  DO WITH FRAME F-Main:
    RUN C-Ubicacion.r (OUTPUT P_Ubi,OUTPUT P_NUbi).
    ASSIGN W_NomComercial:SCREEN-VALUE            = LC(P_NUbi)
           RowObject.Lugar_Comercial:SCREEN-VALUE = P_Ubi.
    RUN ValueChanged.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Agencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencia vTableWin
ON VALUE-CHANGED OF Cmb_Agencia IN FRAME F-Main /* Agencia */
DO:
  ASSIGN W_OfiUsu = INTEGER(SUBSTRING(Cmb_Agencia,1,3)).
         RowObject.Agencia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(W_OfiUsu).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Nombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Nombre vTableWin
ON LEAVE OF RowObject.Nombre IN FRAME F-Main /* Nombre */
DO:
  SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addRecord vTableWin 
PROCEDURE addRecord :
RUN SUPER.
  ENABLE Cmb_Agencia RowObject.Apellido1 RowObject.Apellido2 WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI vTableWin  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayFields vTableWin 
PROCEDURE displayFields :
DEFINE INPUT PARAMETER pcColValues AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
   RUN SUPER( INPUT pcColValues).
   FIND Agencias WHERE Agencias.Agencia EQ INTEGER(RowObject.Agencia:SCREEN-VALUE IN FRAME f-main) NO-LOCK NO-ERROR.
   IF AVAILABLE Agencias THEN DO:
      ASSIGN W_String = STRING(Agencias.Agencia, "999") + "-" + TRIM(Agencias.Nombre)
             Cmb_Agencia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = W_String.
      DISABLE Cmb_Agencia WITH FRAME F-Main.
   END.
   ELSE DO:
     IF Cmb_Agencia:NUM-ITEMS IN FRAME {&FRAME-NAME} GT 0 THEN
       ASSIGN Cmb_Agencia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = Cmb_Agencia:ENTRY(1).
   END.
   APPLY "VALUE-CHANGED":U TO Cmb_Agencia.

 /*ciudad */
 DEFINE VARIABLE W_NUbicacion AS CHARACTER FORMAT "X(50)".
 FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "B" AND Ubicacion.Ubicacion EQ RowObject.Lugar_Comercial:SCREEN-VALUE NO-LOCK NO-ERROR.
 IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = Ubicacion.Nombre.
 FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(RowObject.Lugar_Comercial:SCREEN-VALUE,1,5) NO-LOCK NO-ERROR.
 IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = W_NUbicacion + " - " + Ubicacion.Nombre.
 FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "D" AND Ubicacion.Ubicacion BEGINS SUBSTRING(RowObject.Lugar_Comercial:SCREEN-VALUE,1,2) NO-LOCK NO-ERROR.
 IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = W_NUbicacion + " - " + Ubicacion.Nombre.
 W_NomComercial:SCREEN-VALUE IN FRAME F-Main = LC(W_NUbicacion).

 IF INTEGER(RowObject.Tipo_Cliente:SCREEN-VALUE) GT 2 THEN
    DISABLE RowObject.Apellido1 RowObject.Apellido2 WITH FRAME F-Main.
 ELSE
    ENABLE RowObject.Apellido1 RowObject.Apellido2 WITH FRAME F-Main.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTableWin 
PROCEDURE initializeObject :
/* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN Cmb_Agencia:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
  FOR EACH Agencias WHERE Agencias.Estado NE 3 NO-LOCK
                       BY Agencia.Agencia:
      ASSIGN W_String = STRING(Agencias.Agencia, "999") + "-" + TRIM(Agencias.Nombre)
             W_Estado = Cmb_Agencia:ADD-LAST(W_String).
      IF Agencias.Agencia EQ W_Agencia THEN
         Cmb_Agencia:SCREEN-VALUE = W_String.
  END.
  RUN SUPER.
  W_Ok = RowObject.Tipo_Vinculo:DISABLE("Asociado").
  W_Ok = RowObject.Tipo_Vinculo:DISABLE("Cliente No Asociado").
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateRecord vTableWin 
PROCEDURE updateRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  IF RowObject.Nit:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "" THEN DO:
     MESSAGE "Debe Ingresar un Nit" SKIP
             "No se graba el registro" VIEW-AS ALERT-BOX TITLE "Falta información".
     RETURN ERROR.
  END.
  IF RowObject.Nombre:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "" THEN DO:
     MESSAGE "Debe Ingresar el Nombre del Cliente!" SKIP
             "No se graba el registro" VIEW-AS ALERT-BOX TITLE "Falta información".
     RETURN ERROR.
  END.
  IF RowObject.Apellido1:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "" AND
     INTEGER(RowObject.Tipo_Cliente:SCREEN-VALUE) LE 2 THEN DO:
     MESSAGE "Debe Ingresar al menos un Apellido!" SKIP
             "No se graba el registro" VIEW-AS ALERT-BOX TITLE "Falta información".
     RETURN ERROR.
  END.

  ASSIGN RowObject.Agencia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3)
         RowObject.Usuario:SCREEN-VALUE = W_Usuario
         RowObject.Lugar_Comercial:SCREEN-VALUE = Lugar:SCREEN-VALUE.
  RUN SUPER.
  DISABLE Cmb_Agencia WITH FRAME F-Main.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valueChanged vTableWin 
PROCEDURE valueChanged :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

