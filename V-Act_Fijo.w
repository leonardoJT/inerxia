&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"d-act_fijos.i"}.


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
{incluido\Variable.i "SHARED"}
DEFINE VARIABLE W_TipAct AS INTEGER INITIAL 7.    
/* ***************************  Definitions  ************************** */

DEFINE VAR W_Nuevo AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "d-act_fijos.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Grupo RowObject.Agencia 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.Estado RowObject.Codigo ~
RowObject.Nombre RowObject.Nit_Responsable RowObject.Descripcion ~
RowObject.Val_Compra RowObject.Fec_Avaluo RowObject.Fec_Garantia ~
RowObject.Val_Garantia RowObject.Fec_Compra RowObject.Fec_IniDepre ~
RowObject.Val_Avaluo RowObject.Fec_Contrato RowObject.Fec_IniPignora ~
RowObject.Val_Arriendo RowObject.Fec_debaja RowObject.Fec_FinPignora ~
RowObject.Val_Comercial RowObject.Fec_Retiro RowObject.Fec_Venta ~
RowObject.Fec_Asignacion RowObject.Val_Provision RowObject.Val_Valorizacion ~
RowObject.Vto_Seguro RowObject.Val_Reposicion RowObject.Neto ~
RowObject.Mejoras RowObject.Val_Salvamento RowObject.Nro_Factura ~
RowObject.Fec_VctoMante RowObject.Id_Pignorado RowObject.Id_Prestamo ~
RowObject.Grupo RowObject.Agencia 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS Oficinas Tipo_Act nomres 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE Oficinas AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 Ninguno" 
     DROP-DOWN-LIST
     SIZE 35.29 BY .81
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE Tipo_Act AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tipo de Activo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00000 Ninguno" 
     DROP-DOWN-LIST
     SIZE 75.57 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE nomres AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 59.72 BY .81
     BGCOLOR 18 FONT 4 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.Estado AT ROW 1.19 COL 47.29 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Retirado", 2,
"Depreciado", 3
          SIZE 45 BY 1.08
          BGCOLOR 17 FGCOLOR 0 
     Oficinas AT ROW 1.23 COL 8.72 COLON-ALIGNED
     Tipo_Act AT ROW 2.46 COL 5.85
     RowObject.Codigo AT ROW 3.88 COL 15.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     RowObject.Nombre AT ROW 3.88 COL 41.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 47.14 BY .81
          BGCOLOR 15 
     RowObject.Nit_Responsable AT ROW 5.08 COL 15.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.86 BY .81
          BGCOLOR 15 FGCOLOR 0 FONT 4
     RowObject.Descripcion AT ROW 7.12 COL 6 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 85 BY 3.77
          BGCOLOR 15 FONT 4
     RowObject.Val_Compra AT ROW 11.35 COL 77 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     RowObject.Fec_Avaluo AT ROW 11.42 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     RowObject.Fec_Garantia AT ROW 11.42 COL 47.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     RowObject.Val_Garantia AT ROW 12.31 COL 77 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Fec_Compra AT ROW 12.38 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     RowObject.Fec_IniDepre AT ROW 12.38 COL 47.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Val_Avaluo AT ROW 13.27 COL 77 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     RowObject.Fec_Contrato AT ROW 13.35 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     RowObject.Fec_IniPignora AT ROW 13.35 COL 47.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Val_Arriendo AT ROW 14.19 COL 77 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     RowObject.Fec_debaja AT ROW 14.31 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     RowObject.Fec_FinPignora AT ROW 14.31 COL 47.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     RowObject.Val_Comercial AT ROW 15.19 COL 77 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     RowObject.Fec_Retiro AT ROW 15.23 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     RowObject.Fec_Venta AT ROW 15.27 COL 47.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     RowObject.Fec_Asignacion AT ROW 16.15 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.Val_Provision AT ROW 16.15 COL 77 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     RowObject.Val_Valorizacion AT ROW 16.23 COL 47.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     RowObject.Vto_Seguro AT ROW 17.08 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     RowObject.Val_Reposicion AT ROW 17.15 COL 47.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Neto AT ROW 17.15 COL 77 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Mejoras AT ROW 18.04 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Val_Salvamento AT ROW 18.12 COL 47.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     RowObject.Nro_Factura AT ROW 18.12 COL 77 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Fec_VctoMante AT ROW 19.08 COL 77 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     RowObject.Id_Pignorado AT ROW 19.31 COL 5.72
          VIEW-AS TOGGLE-BOX
          SIZE 13 BY .77
     RowObject.Id_Prestamo AT ROW 19.31 COL 28.72
          VIEW-AS TOGGLE-BOX
          SIZE 11.57 BY .77
     nomres AT ROW 5.12 COL 31.29 COLON-ALIGNED NO-LABEL
     RowObject.Grupo AT ROW 7.73 COL 92 NO-LABEL
           VIEW-AS TEXT 
          SIZE 5 BY .62
          BGCOLOR 17 FGCOLOR 17 FONT 4
     RowObject.Agencia AT ROW 9.35 COL 93 NO-LABEL
           VIEW-AS TEXT 
          SIZE 4 BY .62
          BGCOLOR 17 FGCOLOR 17 FONT 4
     "Decripción del Activo" VIEW-AS TEXT
          SIZE 20.29 BY .62 AT ROW 6.35 COL 6.29
          FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "d-act_fijos.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {d-act_fijos.i}
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
         HEIGHT             = 19.65
         WIDTH              = 96.
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Agencia IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.Codigo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR RowObject.Descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.Descripcion:RETURN-INSERTED IN FRAME F-Main  = TRUE.

/* SETTINGS FOR RADIO-SET RowObject.Estado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Fec_Asignacion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Fec_Avaluo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Fec_Compra IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Fec_Contrato IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Fec_debaja IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Fec_FinPignora IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Fec_Garantia IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Fec_IniDepre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Fec_IniPignora IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Fec_Retiro IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Fec_VctoMante IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Fec_Venta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Grupo IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR TOGGLE-BOX RowObject.Id_Pignorado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.Id_Prestamo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Mejoras IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Neto IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Nit_Responsable IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Nombre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nomres IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Nro_Factura IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Oficinas IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Tipo_Act IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN RowObject.Val_Arriendo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Val_Avaluo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Val_Comercial IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Val_Compra IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Val_Garantia IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Val_Provision IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Val_Reposicion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Val_Salvamento IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Val_Valorizacion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Vto_Seguro IN FRAME F-Main
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

&Scoped-define SELF-NAME RowObject.Nit_Responsable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Nit_Responsable vTableWin
ON LEAVE OF RowObject.Nit_Responsable IN FRAME F-Main /* Responsable */
DO:
  IF SELF:SCREEN-VALUE NE ' ' THEN DO:
    FIND Clientes WHERE Clientes.Nit = TRIM(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN
      NomRes:SCREEN-VALUE = TRIM(Clientes.Nombre) + ' ' + TRIM(Clientes.Apellido1).
     ELSE DO:
       MESSAGE 'Nit No Existe...' VIEW-AS ALERT-BOX.
       RETURN NO-APPLY. 
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Oficinas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Oficinas vTableWin
ON VALUE-CHANGED OF Oficinas IN FRAME F-Main /* Agencia */
DO:
   RowObject.Agencia:SCREEN-VALUE = SUBSTRING(SELF:SCREEN-VALUE,1,3,"CHARACTER").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tipo_Act
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tipo_Act vTableWin
ON VALUE-CHANGED OF Tipo_Act IN FRAME F-Main /* Tipo de Activo */
DO:
   RowObject.Grupo:SCREEN-VALUE = SUBSTRING(SELF:SCREEN-VALUE,1,5,"CHARACTER").  
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
  ENABLE ALL EXCEPT RowObject.Estado WITH FRAME {&FRAME-NAME}.
  W_Nuevo = YES.
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
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcColValues AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcColValues).
  FIND FIRST Agencias WHERE Agencias.Agencia = INTEGER(RowObject.Agencia:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
  IF AVAILABLE Agencias THEN DO:
     Oficinas:ADD-LAST(STRING(Agencias.Agencia,'999') + ' ' + TRIM(Agencias.Nombre)).
     Oficinas:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(Agencias.Agencia,'999') + ' ' + TRIM(Agencias.Nombre).
    END.
   ELSE
    Oficinas:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '000 Ninguno'.  
  FIND FIRST Varios WHERE Varios.Codigo = INTEGER(RowObject.Grupo:SCREEN-VALUE IN FRAME {&FRAME-NAME}) AND Varios.Tipo = W_TipAct NO-LOCK NO-ERROR.
  IF AVAILABLE Varios THEN DO:
      Tipo_Act:ADD-LAST(STRING(Varios.Codigo,'99999') + ' ' + TRIM(Varios.Descripcion)).
      Tipo_Act:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(Varios.Codigo,'99999') + ' ' + TRIM(Varios.Descripcion).
    END.
   ELSE
    Tipo_Act:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '00000 Ninguno'.  
   FIND Clientes WHERE Clientes.Nit = RowObject.Nit_Responsable:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF AVAILABLE Clientes THEN
     NomRes:SCREEN-VALUE = TRIM(Clientes.Nombre) + ' ' + TRIM(Clientes.Apellido1).
    ELSE 
     IF RowObject.Nit_Responsable:SCREEN-VALUE NE ' ' THEN
       NomRes:SCREEN-VALUE = 'Nit No Existe...'.              
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

 RUN SUPER.
 
 FOR EACH Agencias WHERE TRIM(Agencias.Nombre) NE ' ' BY Agencias.Agencia:
    Oficinas:ADD-LAST(STRING(Agencias.Agencia,'999') + ' ' + TRIM(Agencias.Nombre)) IN FRAME {&FRAME-NAME}.
 END.
 
 FOR EACH Varios WHERE TRIM(Varios.Descripcion) NE ' ' AND Varios.Tipo = W_TipAct BY Varios.Codigo:
    Tipo_Act:ADD-LAST(STRING(Varios.Codigo,'99999') + ' ' + TRIM(Varios.Descripcion)) IN FRAME {&FRAME-NAME}.
 END.
/* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetRecord vTableWin 
PROCEDURE resetRecord :
RUN SUPER.
  W_Nuevo = NO.
  DISABLE ALL WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateRecord vTableWin 
PROCEDURE updateRecord :
RUN SUPER.
  IF W_Nuevo THEN
     RUN P-GraLog IN W_Manija (INPUT "NOR: CREA Registro, Act_Fijo. Cod.Activo: " + STRING(RowObject.Codigo:SCREEN-VALUE IN FRAME F-Main)).
  ELSE   
     RUN P-GraLog IN W_Manija (INPUT "NOR: SALVA Registro, Act_Fijo. Cod.Activo: " + STRING(RowObject.Codigo:SCREEN-VALUE IN FRAME F-Main)).
  W_Nuevo = NO.
  DISABLE ALL WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

