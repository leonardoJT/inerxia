&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{Incluido/Variable.I "SHARED"}
DEFINE VAR W_ok AS LOGICAL.
DEFINE VAR i    AS INTEGER.

DEFINE VARIABLE P_Cod   LIKE Empresas.Cod_Empresa.
DEFINE VARIABLE P_AgeEmp LIKE Agencias.Agencia.

DEFINE VARIABLE P_Nit      LIKE Clientes.Nit.
DEFINE VARIABLE p_Nombre   LIKE Clientes.Nombre.
DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
DEFINE VARIABLE P_CliAge   LIKE Clientes.Agencia.

DEFINE VARIABLE W_Puntero AS ROWID.
DEFINE VARIABLE W_Crear   AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Empresas

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Empresas.Alias_Empresa Empresas.For_Pago ~
Empresas.Dias_Gracia Empresas.Nit_RepLegal Empresas.Nit_Pagador ~
Empresas.Per_Juridica Empresas.Fec_Constitucion Empresas.Observaciones ~
Empresas.Cupo_Asignado 
&Scoped-define ENABLED-TABLES Empresas
&Scoped-define FIRST-ENABLED-TABLE Empresas
&Scoped-Define ENABLED-OBJECTS Btn_Informacion BUTTON-3 BUTTON-4 BUTTON-1 ~
Btn_Salvar Btn_Deshacer Btn_Ingresar Btn_Borrar Cmb_ForImportacion ~
Cmb_ForLibranza Btn_Cancelar Btn_Salir BUTTON-11 RECT-1 RECT-2 RECT-3 
&Scoped-Define DISPLAYED-FIELDS Empresas.Cod_Empresa Empresas.Nit ~
Empresas.Alias_Empresa Empresas.For_Pago Empresas.Dias_Gracia ~
Empresas.Nit_RepLegal Empresas.Nit_Pagador Empresas.Per_Juridica ~
Empresas.Fec_Ingreso Empresas.Fec_Constitucion Empresas.Fec_Retiro ~
Empresas.Estado Empresas.Observaciones Empresas.Consecutivo_Pago ~
Empresas.Cupo_Asignado Empresas.Num_Empleados Empresas.Cupo_Actual 
&Scoped-define DISPLAYED-TABLES Empresas
&Scoped-define FIRST-DISPLAYED-TABLE Empresas
&Scoped-Define DISPLAYED-OBJECTS Cmb_Agencia Nom_Empresa Nom_RepLegal ~
Nom_Pagador ActEconomica Cmb_ForImportacion Cmb_ForLibranza 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Btn_Informacion BUTTON-3 BUTTON-4 Btn_Salvar ~
Btn_Deshacer Btn_Ingresar Btn_Borrar Btn_Cancelar Btn_Salir BUTTON-11 
&Scoped-define List-2 Empresas.Nit Empresas.Alias_Empresa Empresas.For_Pago ~
Empresas.Dias_Gracia Empresas.Nit_RepLegal Empresas.Nit_Pagador BUTTON-1 ~
Empresas.Per_Juridica Empresas.Fec_Constitucion Empresas.Observaciones ~
Cmb_ForImportacion Cmb_ForLibranza 
&Scoped-define List-3 Cmb_Agencia Empresas.Cod_Empresa Empresas.Nit 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Borrar 
     LABEL "Borrar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Cancelar 
     LABEL "Cancelar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Deshacer 
     LABEL "Deshacer" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Informacion 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 2" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Ingresar 
     LABEL "Ingresar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Salir 
     LABEL "Salir" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Salvar 
     LABEL "Salvar" 
     SIZE 11 BY 1.62
     FGCOLOR 0 .

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 1" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-11 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 11" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BUTTON-3 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 3" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 4" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE Cmb_Agencia AS CHARACTER FORMAT "X(50)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 41 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_ForImportacion AS CHARACTER FORMAT "X(256)":U INITIAL "00 - No Asignado" 
     LABEL "Formato Importación de Información" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00 - No Asignado" 
     DROP-DOWN-LIST
     SIZE 38 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_ForLibranza AS CHARACTER FORMAT "X(256)":U INITIAL "00 - No Asignado" 
     LABEL "Formato Orden Retención" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00 - No Asignado" 
     DROP-DOWN-LIST
     SIZE 38 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ActEconomica AS CHARACTER FORMAT "X(70)":U 
     LABEL "Actividad Económica" 
     VIEW-AS FILL-IN 
     SIZE 60 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_Empresa AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 60 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_Pagador AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 60 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_RepLegal AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 60 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 79 BY 1.62.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 13 BY 5.38.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 13 BY 10.23.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Empresas
     Cmb_Agencia AT ROW 1.81 COL 14 COLON-ALIGNED
     Empresas.Cod_Empresa AT ROW 1.81 COL 84.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Btn_Informacion AT ROW 1.81 COL 100
     Empresas.Nit AT ROW 2.88 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 FGCOLOR 0 
     Nom_Empresa AT ROW 2.88 COL 30 COLON-ALIGNED NO-LABEL
     BUTTON-3 AT ROW 3.42 COL 100
     Empresas.Alias_Empresa AT ROW 3.96 COL 30 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 60 BY .81
          BGCOLOR 15 
     BUTTON-4 AT ROW 5.04 COL 100
     Empresas.For_Pago AT ROW 5.42 COL 16 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Semanal", 1,
"Decadal", 2,
"Quincenal", 3,
"Mensual", 4
          SIZE 47 BY 1.08
     Empresas.Dias_Gracia AT ROW 5.46 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.43 BY .81
          BGCOLOR 15 
     Empresas.Nit_RepLegal AT ROW 6.92 COL 14 COLON-ALIGNED
          LABEL "Representante"
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     Nom_RepLegal AT ROW 6.92 COL 30 COLON-ALIGNED NO-LABEL
     Empresas.Nit_Pagador AT ROW 8 COL 14 COLON-ALIGNED
          LABEL "Pagador"
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
          BGCOLOR 15 
     Nom_Pagador AT ROW 8 COL 30 COLON-ALIGNED NO-LABEL
     ActEconomica AT ROW 9.08 COL 30 COLON-ALIGNED
     BUTTON-1 AT ROW 9.08 COL 92
     Empresas.Per_Juridica AT ROW 10.15 COL 30 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 
     Empresas.Fec_Ingreso AT ROW 10.15 COL 77 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Btn_Salvar AT ROW 10.42 COL 100
     Empresas.Fec_Constitucion AT ROW 11.23 COL 30 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 
     Empresas.Fec_Retiro AT ROW 11.23 COL 77 COLON-ALIGNED
          LABEL "Fecha de Retiro"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Btn_Deshacer AT ROW 12.04 COL 100
     Empresas.Estado AT ROW 12.31 COL 70 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 22 BY .81
     Empresas.Observaciones AT ROW 13.12 COL 32 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 60 BY 2.15
          BGCOLOR 15 
     Btn_Ingresar AT ROW 13.65 COL 100
     Btn_Borrar AT ROW 15.27 COL 100
     Cmb_ForImportacion AT ROW 15.54 COL 52 COLON-ALIGNED
     Cmb_ForLibranza AT ROW 16.62 COL 52 COLON-ALIGNED
     Btn_Cancelar AT ROW 16.88 COL 100
     Empresas.Consecutivo_Pago AT ROW 17.69 COL 52 COLON-ALIGNED
          LABEL "Consecutivo de Pago"
          VIEW-AS FILL-IN 
          SIZE 5.43 BY 1
          BGCOLOR 18 FGCOLOR 15 
     Empresas.Cupo_Asignado AT ROW 17.69 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.43 BY 1
          BGCOLOR 15 FGCOLOR 0 
     Btn_Salir AT ROW 18.5 COL 100
     Empresas.Num_Empleados AT ROW 18.77 COL 52 COLON-ALIGNED
          LABEL "Número de Empleados"
          VIEW-AS FILL-IN 
          SIZE 5.43 BY 1
          BGCOLOR 18 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.86 BY 21.12
         BGCOLOR 17 FONT 5.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Empresas
     Empresas.Cupo_Actual AT ROW 18.77 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.43 BY 1
          BGCOLOR 18 FGCOLOR 15 
     BUTTON-11 AT ROW 20.65 COL 104
     RECT-1 AT ROW 5.04 COL 14
     RECT-2 AT ROW 1.54 COL 99
     RECT-3 AT ROW 10.15 COL 99
     "Observaciones" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 12.31 COL 32
          FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.86 BY 21.12
         BGCOLOR 17 FONT 5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Empresas que Manejan Deducción de Nómina"
         HEIGHT             = 21.12
         WIDTH              = 113.86
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Empresas
                                                                        */
/* SETTINGS FOR FILL-IN ActEconomica IN FRAME F_Empresas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Empresas.Alias_Empresa IN FRAME F_Empresas
   2                                                                    */
/* SETTINGS FOR BUTTON Btn_Borrar IN FRAME F_Empresas
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME F_Empresas
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Deshacer IN FRAME F_Empresas
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Informacion IN FRAME F_Empresas
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Ingresar IN FRAME F_Empresas
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Salir IN FRAME F_Empresas
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Salvar IN FRAME F_Empresas
   1                                                                    */
/* SETTINGS FOR BUTTON BUTTON-1 IN FRAME F_Empresas
   2                                                                    */
/* SETTINGS FOR BUTTON BUTTON-11 IN FRAME F_Empresas
   1                                                                    */
/* SETTINGS FOR BUTTON BUTTON-3 IN FRAME F_Empresas
   1                                                                    */
/* SETTINGS FOR BUTTON BUTTON-4 IN FRAME F_Empresas
   1                                                                    */
/* SETTINGS FOR COMBO-BOX Cmb_Agencia IN FRAME F_Empresas
   NO-ENABLE 3                                                          */
/* SETTINGS FOR COMBO-BOX Cmb_ForImportacion IN FRAME F_Empresas
   2                                                                    */
/* SETTINGS FOR COMBO-BOX Cmb_ForLibranza IN FRAME F_Empresas
   2                                                                    */
/* SETTINGS FOR FILL-IN Empresas.Cod_Empresa IN FRAME F_Empresas
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN Empresas.Consecutivo_Pago IN FRAME F_Empresas
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Empresas.Cupo_Actual IN FRAME F_Empresas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Empresas.Dias_Gracia IN FRAME F_Empresas
   2                                                                    */
/* SETTINGS FOR RADIO-SET Empresas.Estado IN FRAME F_Empresas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Empresas.Fec_Constitucion IN FRAME F_Empresas
   2                                                                    */
/* SETTINGS FOR FILL-IN Empresas.Fec_Ingreso IN FRAME F_Empresas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Empresas.Fec_Retiro IN FRAME F_Empresas
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR RADIO-SET Empresas.For_Pago IN FRAME F_Empresas
   2                                                                    */
/* SETTINGS FOR FILL-IN Empresas.Nit IN FRAME F_Empresas
   NO-ENABLE 2 3                                                        */
/* SETTINGS FOR FILL-IN Empresas.Nit_Pagador IN FRAME F_Empresas
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Empresas.Nit_RepLegal IN FRAME F_Empresas
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Nom_Empresa IN FRAME F_Empresas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_Pagador IN FRAME F_Empresas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_RepLegal IN FRAME F_Empresas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Empresas.Num_Empleados IN FRAME F_Empresas
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR EDITOR Empresas.Observaciones IN FRAME F_Empresas
   2                                                                    */
/* SETTINGS FOR FILL-IN Empresas.Per_Juridica IN FRAME F_Empresas
   2                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Empresas
/* Query rebuild information for FRAME F_Empresas
     _Query            is NOT OPENED
*/  /* FRAME F_Empresas */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Empresas que Manejan Deducción de Nómina */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Empresas que Manejan Deducción de Nómina */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar wWin
ON CHOOSE OF Btn_Cancelar IN FRAME F_Empresas /* Cancelar */
DO:
  W_Crear = NO.
  DISABLE {&List-3} WITH FRAME F_Empresas.
  ENABLE Btn_Salvar Btn_Salir Btn_Ingresar Btn_Borrar WITH FRAME F_Empresas.
  RUN Inicializar_Pantalla.
  IF W_Puntero NE ? THEN DO:
     FIND Empresas WHERE ROWID(Empresas) EQ W_Puntero NO-ERROR.
     IF AVAILABLE Empresas THEN DO:
        RUN Mostrar_Empresa.
     END.
  END.

  Button-4:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Deshacer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deshacer wWin
ON CHOOSE OF Btn_Deshacer IN FRAME F_Empresas /* Deshacer */
DO:
  W_Crear = NO.
  IF AVAILABLE Empresas THEN
     RUN Mostrar_Empresa.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ingresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ingresar wWin
ON CHOOSE OF Btn_Ingresar IN FRAME F_Empresas /* Ingresar */
DO:
DO WITH FRAME F_Empresas:
  IF AVAILABLE Empresas THEN
     W_Puntero = ROWID(Empresas).
     
  ENABLE {&List-2}.
  DISABLE Btn_Ingresar Btn_Borrar Btn_Salir.
  /*valida usuarios.all_agencias para habilitar cmb_agencia*/
  W_Crear = YES.

  RUN Inicializar_Pantalla.
  FIND LAST Empresas NO-LOCK NO-ERROR.
  IF AVAILABLE Empresas THEN
     Empresas.Cod_Empresa:SCREEN-VALUE = STRING(Empresas.Cod_Empresa + 1).
  ELSE Empresas.Cod_Empresa:SCREEN-VALUE IN FRAME F_Empresas = "1".

  RELEASE Empresas.

  Button-4:SENSITIVE = FALSE.

  APPLY "entry" TO Empresas.Nit.
  RETURN NO-APPLY.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON CHOOSE OF Btn_Salir IN FRAME F_Empresas /* Salir */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar wWin
ON CHOOSE OF Btn_Salvar IN FRAME F_Empresas /* Salvar */
DO:
  IF INTEGER(Empresas.Cod_Empresa:SCREEN-VALUE) LE 0 THEN DO:
     MESSAGE "Falta Cod_empresa ... No se permite la operaciòn." VIEW-AS ALERT-BOX.
     RETURN.
  END.
    
  IF W_Crear THEN DO:
     CREATE Empresas.
     ASSIGN Empresas.Agencia     = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3))
            Empresas.Cod_Empresa = INTEGER(Empresas.Cod_Empresa:SCREEN-VALUE).
  END.
  ELSE FIND CURRENT Empresas NO-ERROR.

  RUN Grabar.  

  ASSIGN W_Crear            = NO
         Button-4:SENSITIVE = TRUE.

  ENABLE Btn_Ingresar WITH FRAME F_Empresas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME F_Empresas /* Button 1 */
DO:

  DEFINE VAR P_Gru LIKE Ciiu.Grupo.
  DEFINE VAR P_Sub LIKE Ciiu.Subgrupo.
  DEFINE VAR P_Cod LIKE Ciiu.Codigo.
  DEFINE VAR P_Nom AS CHARACTER FORMAT "X(50)".

  RUN C-Ciiu.r (OUTPUT P_Gru, OUTPUT P_Sub, OUTPUT P_Cod, OUTPUT P_Nom).
  IF P_Gru EQ 0 THEN MESSAGE "No fue escogido ningún código" VIEW-AS ALERT-BOX.
  ELSE
    ASSIGN ActEconomica:SCREEN-VALUE = STRING(P_Gru,"99") + STRING(P_Sub,"99") + STRING(P_Cod,"9999") + " - " + LOWER(P_Nom).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 wWin
ON CHOOSE OF BUTTON-3 IN FRAME F_Empresas /* Button 3 */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
  
  listado = W_PathSpl + "L_Empresas.Lst".
  {Incluido\Imprimir.i "Listado" Tamano}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 wWin
ON CHOOSE OF BUTTON-4 IN FRAME F_Empresas /* Button 4 */
DO:
  WWin:SENSITIVE = FALSE.

  RUN C-Empresas.r (INPUT W_Agencia, OUTPUT P_Cod, OUTPUT P_Nit, OUTPUT P_AgeEmp, OUTPUT Nom_Empresa).

  IF P_Cod NE 0 THEN DO:
     FIND Empresas WHERE Empresas.Cod_Empresa EQ P_Cod NO-LOCK NO-ERROR.
     IF AVAILABLE Empresas THEN 
        RUN Mostrar_Empresa.
  END.  

  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP ( ).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Agencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencia wWin
ON VALUE-CHANGED OF Cmb_Agencia IN FRAME F_Empresas /* Agencia */
DO:
  ASSIGN FRAME F_Empresas Cmb_Agencia.
  FOR EACH Formatos WHERE Formatos.Agencia EQ INTEGER(SUBSTRING(Cmb_Agencia,1,3)) NO-LOCK:
    W_ok = Cmb_ForImportacion:ADD-LAST(STRING(Formatos.Cod_Formato,"99") + " - " + Formatos.Nom_Formato).
    W_ok = Cmb_ForLibranza:ADD-LAST(STRING(Formatos.Cod_Formato,"99") + " - " + Formatos.Nom_Formato). 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Empresas.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Empresas.Nit wWin
ON ENTRY OF Empresas.Nit IN FRAME F_Empresas /* Nit */
DO:
  ENABLE Btn_Salvar WITH FRAME F_Empresas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Empresas.Nit wWin
ON LEAVE OF Empresas.Nit IN FRAME F_Empresas /* Nit */
DO:
  DO WITH FRAME Empresas:
     FIND Clientes WHERE Clientes.Nit EQ Empresas.Nit:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAILABLE(Clientes) THEN
        Nom_Empresa:SCREEN-VALUE = TRIM(Clientes.Nombre) + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     ELSE DO:
        RUN C-Clientes.R(INPUT 1, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_CliAge).
        ASSIGN Nom_Empresa:SCREEN-VALUE    = TRIM(P_Nombre) + " " + P_Apellido
               Empresas.Nit:SCREEN-VALUE = P_Nit.
        FIND Clientes WHERE Clientes.Agencia EQ P_CliAge AND Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
     END.
  IF AVAILABLE(Clientes) THEN DO:
   IF Clientes.Tipo_Identificacion NE "NIT" THEN DO:
       MESSAGE "Debe ser escogida una Persona Jurídica" SKIP
               "si este campo es inconsistente. no se " SKIP
               "permitira grabar el registro. Rectifique el nit!" VIEW-AS ALERT-BOX WARNING.
       DISABLE Btn_Salvar WITH FRAME F_Empresas.
   END.
  END.
  IF Empresas.Nit:SCREEN-VALUE EQ "" THEN DO:
       MESSAGE "Debe ser escogido un Nit diferente de espacios para" SKIP
               "La empresa, de lo contrario no se permitira" SKIP
               "grabar el registro. Rectifique el nit!" VIEW-AS ALERT-BOX WARNING.
       DISABLE Btn_Salvar WITH FRAME F_Empresas.
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Empresas.Nit_Pagador
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Empresas.Nit_Pagador wWin
ON LEAVE OF Empresas.Nit_Pagador IN FRAME F_Empresas /* Pagador */
DO:
  DO WITH FRAME Empresas:
     FIND Clientes WHERE Clientes.Nit EQ Empresas.Nit_Pagador:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAILABLE(Clientes) THEN
        Nom_Pagador:SCREEN-VALUE = TRIM(Clientes.Nombre) + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     ELSE DO:
        RUN C-Clientes.R(INPUT 1, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_CliAge).
        ASSIGN Nom_Pagador:SCREEN-VALUE    = TRIM(P_Nombre) + " " + P_Apellido
               Empresas.Nit_Pagador:SCREEN-VALUE = P_Nit.
        FIND Clientes WHERE Clientes.Agencia EQ P_CliAge AND Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
     END.
  IF AVAILABLE(Clientes) AND Clientes.Tipo_Identificacion EQ "NIT" THEN DO:
       MESSAGE "Debe ser escogida una Persona Natural" VIEW-AS ALERT-BOX WARNING.
       ASSIGN Empresas.Nit_Pagador:SCREEN-VALUE = ""
              Nom_Pagador:SCREEN-VALUE = "".
  END.
END. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Empresas.Nit_RepLegal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Empresas.Nit_RepLegal wWin
ON LEAVE OF Empresas.Nit_RepLegal IN FRAME F_Empresas /* Representante */
DO:
  DO WITH FRAME Empresas:
     FIND Clientes WHERE Clientes.Nit EQ Empresas.Nit_RepLegal:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAILABLE(Clientes) THEN
        Nom_RepLegal:SCREEN-VALUE = TRIM(Clientes.Nombre) + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     ELSE DO:
        RUN C-Clientes.R(INPUT 1, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_CliAge).
        ASSIGN Nom_RepLegal:SCREEN-VALUE    = TRIM(P_Nombre) + " " + P_Apellido
               Empresas.Nit_RepLegal:SCREEN-VALUE = P_Nit.
        FIND Clientes WHERE Clientes.Agencia EQ P_CliAge AND Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
     END.
  IF AVAILABLE(Clientes) AND Clientes.Tipo_Identificacion EQ "NIT" THEN DO:
       MESSAGE "Debe ser escogida una Persona Natural" VIEW-AS ALERT-BOX WARNING.
       ASSIGN Empresas.Nit_RepLegal:SCREEN-VALUE = ""
              Nom_RepLegal:SCREEN-VALUE = "".
  END.
END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY Cmb_Agencia Nom_Empresa Nom_RepLegal Nom_Pagador ActEconomica 
          Cmb_ForImportacion Cmb_ForLibranza 
      WITH FRAME F_Empresas IN WINDOW wWin.
  IF AVAILABLE Empresas THEN 
    DISPLAY Empresas.Cod_Empresa Empresas.Nit Empresas.Alias_Empresa 
          Empresas.For_Pago Empresas.Dias_Gracia Empresas.Nit_RepLegal 
          Empresas.Nit_Pagador Empresas.Per_Juridica Empresas.Fec_Ingreso 
          Empresas.Fec_Constitucion Empresas.Fec_Retiro Empresas.Estado 
          Empresas.Observaciones Empresas.Consecutivo_Pago 
          Empresas.Cupo_Asignado Empresas.Num_Empleados Empresas.Cupo_Actual 
      WITH FRAME F_Empresas IN WINDOW wWin.
  ENABLE Btn_Informacion BUTTON-3 Empresas.Alias_Empresa BUTTON-4 
         Empresas.For_Pago Empresas.Dias_Gracia Empresas.Nit_RepLegal 
         Empresas.Nit_Pagador BUTTON-1 Empresas.Per_Juridica Btn_Salvar 
         Empresas.Fec_Constitucion Btn_Deshacer Empresas.Observaciones 
         Btn_Ingresar Btn_Borrar Cmb_ForImportacion Cmb_ForLibranza 
         Btn_Cancelar Empresas.Cupo_Asignado Btn_Salir BUTTON-11 RECT-1 RECT-2 
         RECT-3 
      WITH FRAME F_Empresas IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Empresas}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar wWin 
PROCEDURE Grabar :
DO WITH FRAME F_Empresas:
   ASSIGN Empresas.Agencia              = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3))
          Empresas.Cod_Empresa          = INTEGER(Empresas.Cod_Empresa:SCREEN-VALUE)
          Empresas.Nit                  
          Empresas.FOR_Pago             
          Empresas.Dias_Gracia          
          Empresas.Nit_RepLegal         
          Empresas.Nit_Pagador          
          Empresas.Per_Juridica         
          Empresas.Fec_Constitucion     
          Empresas.Fec_Ingreso          
          Empresas.Fec_Retiro           
          Empresas.Estado               
          Empresas.Observaciones        
          Empresas.Consecutivo_Pago     
          Empresas.Num_Empleados
          Empresas.ALIAS_Empresa
          Empresas.Cupo_Asignado
          Empresas.Cod_Formato          = INTEGER(SUBSTRING(Cmb_ForImportacion:SCREEN-VALUE,1,2))
          Empresas.Cod_Orden            = INTEGER(SUBSTRING(Cmb_ForLibranza:SCREEN-VALUE,1,2))
          Empresas.Actividad_Economica  = INTEGER(SUBSTRING(ActEconomica:SCREEN-VALUE,1,8)).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Pantalla wWin 
PROCEDURE Inicializar_Pantalla :
DO WITH FRAME F_Empresas:
   FIND Agencias WHERE Agencias.Agencia EQ W_Agencia NO-LOCK NO-ERROR.
   IF AVAILABLE Agencias THEN
      Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
   ASSIGN Cmb_ForImportacion:SCREEN-VALUE = Cmb_ForImportacion:ENTRY(1)
          Cmb_ForLibranza:SCREEN-VALUE = Cmb_ForLibranza:ENTRY(1)
          Empresas.Cod_Empresa:SCREEN-VALUE    = "0"
          Empresas.Nit:SCREEN-VALUE            = ""
          Nom_Empresa:SCREEN-VALUE             = ""
          Empresas.FOR_Pago:SCREEN-VALUE       = "4"
          Empresas.Dias_Gracia:SCREEN-VALUE    = "0"
          Empresas.Nit_RepLegal:SCREEN-VALUE   = ""
          Nom_RepLegal:SCREEN-VALUE            = ""
          Empresas.Nit_Pagador:SCREEN-VALUE    = ""
          Nom_Pagador:SCREEN-VALUE             = ""
          ActEconomica:SCREEN-VALUE            = ""
          Empresas.Per_Juridica:SCREEN-VALUE   = ""
          Empresas.Fec_Constitucion:SCREEN-VALUE = ""
          Empresas.Fec_Ingreso:SCREEN-VALUE    = STRING(TODAY)
          Empresas.Fec_Retiro:SCREEN-VALUE     = ""
          Empresas.Estado:SCREEN-VALUE         = "1"
          Empresas.Observaciones:SCREEN-VALUE  = ""
          Cmb_ForImportacion:SCREEN-VALUE      = Cmb_ForImportacion:ENTRY(1)
          Cmb_ForLibranza:SCREEN-VALUE         = Cmb_ForLibranza:ENTRY(1)
          Empresas.Consecutivo_Pago:SCREEN-VALUE = "0"
          Empresas.Num_Empleados:SCREEN-VALUE  = "0"
          Empresa.ALIAS_Empresa:SCREEN-VALUE   = ""
          Empresas.Cupo_Asignado:SCREEN-VALUE  = "0"
          Empresas.Cupo_Actual:SCREEN-VALUE    = "0".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
DO WITH FRAME F_Empresas:
  RUN SUPER.
  FOR EACH Agencias NO-LOCK:
    W_Ok = Cmb_Agencia:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
  END.
  FIND FIRST Empresas WHERE Empresas.Agencia EQ W_Agencia NO-ERROR.
  IF AVAILABLE Empresas THEN  DO:
     FOR EACH Formatos WHERE Formatos.Agencia EQ W_Agencia NO-LOCK:
       W_ok = Cmb_ForImportacion:ADD-LAST(STRING(Formatos.Cod_Formato,"99") + " - " + Formatos.Nom_Formato).
       W_ok = Cmb_ForLibranza:ADD-LAST(STRING(Formatos.Cod_Formato,"99") + " - " + Formatos.Nom_Formato). 
     END.
     RUN Mostrar_Empresa.
  END.
  ELSE DO:
    FIND FIRST Empresas NO-ERROR.
    IF AVAILABLE Empresas THEN DO:
      FOR EACH Formatos WHERE Formatos.Agencia EQ Empresas.Agencia NO-LOCK:
        W_ok = Cmb_ForImportacion:ADD-LAST(STRING(Formatos.Cod_Formato,"99") + " - " + Formatos.Nom_Formato).
        W_ok = Cmb_ForLibranza:ADD-LAST(STRING(Formatos.Cod_Formato,"99") + " - " + Formatos.Nom_Formato). 
      END.
      RUN Mostrar_Empresa.
    END.
    ELSE DO:
      DISABLE ALL WITH FRAME F_Empresas.
      ENABLE {&List-1} WITH FRAME F_Empresas.
      RUN Inicializar_Pantalla.
    END.
  END.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Empresa wWin 
PROCEDURE Mostrar_Empresa :
DO WITH FRAME F_Empresas:
   FIND Agencias WHERE Agencias.Agencia EQ Empresas.Agencia NO-LOCK NO-ERROR.
   IF AVAILABLE Agencias THEN
      Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.

   ASSIGN Empresas.Cod_Empresa:SCREEN-VALUE    = STRING(Empresas.Cod_Empresa)
          Empresas.Nit:SCREEN-VALUE            = Empresas.Nit
          Empresas.FOR_Pago:SCREEN-VALUE       = STRING(Empresas.FOR_Pago)
          Empresas.Dias_Gracia:SCREEN-VALUE    = STRING(Empresas.Dias_Gracia)
          Empresas.Nit_RepLegal:SCREEN-VALUE   = Empresas.Nit_RepLegal
          Empresas.Nit_Pagador:SCREEN-VALUE    = Empresas.Nit_Pagador
          Empresas.Per_Juridica:SCREEN-VALUE   = Empresas.Per_Juridica
          Empresas.Fec_Constitucion:SCREEN-VALUE = STRING(Empresas.Fec_Constitucion)
          Empresas.Fec_Ingreso:SCREEN-VALUE    = STRING(Empresas.Fec_Ingreso)
          Empresas.Fec_Retiro:SCREEN-VALUE     = STRING(Empresas.Fec_Retiro)
          Empresas.Estado:SCREEN-VALUE         = STRING(Empresas.Estado)
          Empresas.Observaciones:SCREEN-VALUE  = Empresas.Observaciones
          Empresas.Consecutivo_Pago:SCREEN-VALUE = STRING(Empresas.Consecutivo_Pago)
          Empresas.Num_Empleados:SCREEN-VALUE  = STRING(Empresas.Num_Empleados)
          Empresas.ALIAS_Empresa:SCREEN-VALUE  = Empresas.ALIAS_Empresa
          Empresas.Cupo_Asignado:SCREEN-VALUE  = STRING(Empresas.Cupo_Asignado)
          Empresas.Cupo_Actual:SCREEN-VALUE    = STRING(Empresas.Cupo_Actual).

    IF Empresas.Cod_Formato NE 0 THEN DO:
       FIND Formatos WHERE
            Formatos.Agencia EQ Empresas.Agencia AND
            Formatos.Cod_Formato EQ Empresas.Cod_Formato NO-LOCK NO-ERROR.
       IF AVAILABLE Formatos THEN
          Cmb_ForImportacion:SCREEN-VALUE = STRING(Formatos.Cod_Formato,"99") + " - " + Formatos.Nom_Formato.
    END.
    ELSE
      Cmb_ForImportacion:SCREEN-VALUE = "00 - No Asignado".                    
    IF Empresas.Cod_Orden NE 0 THEN DO:
       FIND Formatos WHERE
            Formatos.Agencia EQ Empresas.Agencia AND
            Formatos.Cod_Formato EQ Empresas.Cod_Formato NO-LOCK NO-ERROR.
       IF AVAILABLE Formatos THEN
          Cmb_ForLibranza:SCREEN-VALUE = STRING(Formatos.Cod_Formato,"99") + " - " + Formatos.Nom_Formato.
    END.
    ELSE
      Cmb_ForLibranza:SCREEN-VALUE = "00 - No Asignado".                    
    FIND Clientes WHERE Clientes.Nit EQ Empresas.Nit NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN
       ASSIGN Nom_Empresa:SCREEN-VALUE             = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
    ELSE
       ASSIGN Nom_Empresa:SCREEN-VALUE             = "No Asignado".
    FIND Clientes WHERE Clientes.Nit EQ Empresas.Nit_RepLegal NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN
       ASSIGN Nom_RepLegal:SCREEN-VALUE             = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
    ELSE
       ASSIGN Nom_RepLegal:SCREEN-VALUE             = "No Asignado".    
    FIND Clientes WHERE Clientes.Nit EQ Empresas.Nit_Pagador NO-LOCK NO-ERROR.
    IF AVAILABLE Clientes THEN
       ASSIGN Nom_Pagador:SCREEN-VALUE             = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
    ELSE
       ASSIGN Nom_Pagador:SCREEN-VALUE             = "No Asignado".
    FIND Ciiu WHERE 
         Ciiu.Grupo EQ INTEGER(SUBSTRING(STRING(Empresas.Actividad_Economica,"99999999"),1,2)) AND
         Ciiu.Subgrupo EQ INTEGER(SUBSTRING(STRING(Empresas.Actividad_Economica,"99999999"),3,2)) AND 
         Ciiu.Codigo_Ciiu EQ INTEGER(SUBSTRING(STRING(Empresas.Actividad_Economica,"99999999"),5,4)) 
         NO-LOCK NO-ERROR.
    IF AVAILABLE Ciiu THEN
       ActEconomica:SCREEN-VALUE = LOWER(STRING(Empresas.Actividad_Economica,"99999999") + " - " + Ciiu.Descripcion).
    ELSE ActEconomica:SCREEN-VALUE = "Grupo No definido - ".
          
       
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{INCLUIDO\RepEncabezado.I}    
  W_Reporte    = "REPORTE   : EMPRESAS DEL SISTEMA - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = " Cod  Age  Nit            Descripcion                      Period     Nit Pagad   Nit Rep.Legal".
  DEFINE VAR Des AS CHARACTER FORMAT "X(10)".
    
  DEFINE FRAME F-Detalle
     Empresas.Cod_Empresa       AT  2 FORMAT "z9"
     Empresas.Agencia           AT  5 FORMAT "zzz9"
     Empresas.Nit               AT 12 FORMAT "X(12)"
     Empresas.Alias_Empresa     AT 27 FORMAT "X(30)"
     des                        AT 60 FORMAT "X(10)"
     Empresas.Nit_Pagador       AT 72 FORMAT "X(12)"
     Empresas.Nit_RepLegal      AT 87 FORMAT "X(12)"
  WITH WIDTH 132 USE-TEXT NO-BOX STREAM-IO NO-LABELS FRAME F-Detalle.
  
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  FOR EACH Empresas BY Empresas.cod_empresa BY Empresas.Agencia :
     CASE Empresas.For_Pago:
        WHEN 1 THEN des = "Semanal  ".   
        WHEN 2 THEN des = "Decadal  ".   
        WHEN 3 THEN des = "Quincenal".   
        WHEN 4 THEN des = "Mensual  ".   
     END.
     DISPLAY Empresas.Agencia         
             Empresas.Cod_Empresa    
             Empresas.Nit            
             Empresas.Alias_Empresa   
             des        
             Empresas.Nit_Pagador  
             Empresas.Nit_RepLegal 
     WITH FRAME F-Detalle WIDTH 132 NO-BOX.
  END.  
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

