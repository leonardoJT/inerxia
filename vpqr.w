&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dpqr.i"}.



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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}

DEF VAR W_Ok           AS LOG NO-UNDO.
DEF VAR P_Nit          AS CHAR NO-UNDO.
DEF VAR w_agencia       AS CHAR NO-UNDO.
DEF VAR w_usuario       AS CHAR NO-UNDO.
DEF VAR BW_agencia       AS CHAR NO-UNDO.
DEF VAR BW_usuario       AS CHAR NO-UNDO.


DEF VAR P_Cla_Pro      AS INT NO-UNDO. 
DEF VAR P_Tip_pro      AS INT NO-UNDO. 
DEF VAR P_Cod-pro      AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dpqr.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Pqr

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Descrip_PQR RowObject.Num_PQR 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-318 RECT-319 RECT-321 RECT-327 RECT-328 
&Scoped-Define DISPLAYED-FIELDS RowObject.Nit RowObject.Fec_Solicitud ~
RowObject.Agencia RowObject.Usuario RowObject.Clase_Producto ~
RowObject.Tip_Credito RowObject.Cod_Producto RowObject.Cod_Proceso ~
RowObject.Area_Resp RowObject.Cod_Req RowObject.Per_Resp RowObject.Estado ~
RowObject.Descrip_PQR RowObject.Descrip_Resp RowObject.Num_PQR 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS nom_cliente Nom_Agencia Nom_Usuario ~
Cmb_Proceso Cmb_Producto Cmb_AreasPQR Cmb_Requerimientos Nom_UsuarioResp ~
Nom_Estado 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dvarios1 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvarios2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect1 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect5 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE Cmb_AreasPQR AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 39 BY 1
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE Cmb_Proceso AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Producto AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 39 BY 1
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE Cmb_Requerimientos AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 39 BY 1
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE Nom_Agencia AS CHARACTER FORMAT "X(70)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE nom_cliente AS CHARACTER FORMAT "X(60)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 50 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE Nom_Estado AS CHARACTER FORMAT "X(70)":U 
     VIEW-AS FILL-IN 
     SIZE 49 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE Nom_Usuario AS CHARACTER FORMAT "X(70)":U 
     VIEW-AS FILL-IN 
     SIZE 45.43 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE Nom_UsuarioResp AS CHARACTER FORMAT "X(70)":U 
     VIEW-AS FILL-IN 
     SIZE 49 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-318
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY 2.62.

DEFINE RECTANGLE RECT-319
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 29 BY 8.31.

DEFINE RECTANGLE RECT-321
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 57.43 BY 8.31.

DEFINE RECTANGLE RECT-327
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 43.29 BY 6.73.

DEFINE RECTANGLE RECT-328
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 43.29 BY 6.73.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Pqr
     RowObject.Nit AT ROW 1.46 COL 61.43 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 19.57 BY .92
          BGCOLOR 18 FGCOLOR 15 
     nom_cliente AT ROW 2.62 COL 18 COLON-ALIGNED WIDGET-ID 4
     RowObject.Fec_Solicitud AT ROW 4 COL 31 COLON-ALIGNED HELP
          "Ingrese la Fecha Solicitud" NO-LABEL WIDGET-ID 28 FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .92
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Agencia AT ROW 4.88 COL 16 COLON-ALIGNED HELP
          "Ingrese la Oficina donde se Origino el PQR" NO-LABEL WIDGET-ID 108 FORMAT "999"
          VIEW-AS FILL-IN 
          SIZE 4.43 BY .81
          BGCOLOR 10 
     Nom_Agencia AT ROW 4.08 COL 45 COLON-ALIGNED NO-LABEL WIDGET-ID 110
     RowObject.Usuario AT ROW 4.88 COL 22 COLON-ALIGNED HELP
          "Usuario de Grabacion del PQR" NO-LABEL WIDGET-ID 106 FORMAT "X(4)"
          VIEW-AS FILL-IN 
          SIZE 5.43 BY .81
          BGCOLOR 10 
     Nom_Usuario AT ROW 5.15 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 96
     RowObject.Clase_Producto AT ROW 4.88 COL 83 NO-LABEL WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 2.57 BY 1
          BGCOLOR 10 
     Cmb_Proceso AT ROW 6.08 COL 56.86 COLON-ALIGNED NO-LABEL WIDGET-ID 114
     RowObject.Tip_Credito AT ROW 4.88 COL 84 COLON-ALIGNED NO-LABEL WIDGET-ID 92
          VIEW-AS FILL-IN 
          SIZE 2.57 BY 1
          BGCOLOR 10 
     RowObject.Cod_Producto AT ROW 6.92 COL 24 COLON-ALIGNED NO-LABEL WIDGET-ID 90
          VIEW-AS FILL-IN 
          SIZE 4.43 BY .81
     RowObject.Cod_Proceso AT ROW 7.73 COL 78 COLON-ALIGNED HELP
          "Area Responsable PQR" NO-LABEL WIDGET-ID 120 FORMAT "99999"
          VIEW-AS FILL-IN 
          SIZE 6.43 BY 1
          BGCOLOR 10 
     Cmb_Producto AT ROW 7.08 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 116
     Cmb_AreasPQR AT ROW 8.08 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 126
     RowObject.Area_Resp AT ROW 8.12 COL 22 COLON-ALIGNED NO-LABEL WIDGET-ID 118
          VIEW-AS FILL-IN 
          SIZE 6.43 BY .81
     Cmb_Requerimientos AT ROW 9.08 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 128
     RowObject.Cod_Req AT ROW 9.23 COL 22 COLON-ALIGNED HELP
          "Requerimientos" NO-LABEL WIDGET-ID 122 FORMAT "99999"
          VIEW-AS FILL-IN 
          SIZE 6.43 BY .81
     Nom_UsuarioResp AT ROW 10.12 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 134
     RowObject.Per_Resp AT ROW 10.27 COL 22.57 COLON-ALIGNED HELP
          "Persona Responsable PQR" NO-LABEL WIDGET-ID 132 FORMAT "X(4)"
          VIEW-AS FILL-IN 
          SIZE 5.43 BY .73
     RowObject.Estado AT ROW 10.96 COL 24 COLON-ALIGNED HELP
          "Estado PQR" NO-LABEL WIDGET-ID 138 FORMAT "9"
          VIEW-AS FILL-IN 
          SIZE 3.72 BY .81
          BGCOLOR 15 
     Nom_Estado AT ROW 11.04 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 140
     RowObject.Descrip_PQR AT ROW 12.85 COL 4 HELP
          "Descripcion detallada del PQR" NO-LABEL WIDGET-ID 58
          VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
          SIZE 40.86 BY 5.38
          BGCOLOR 15 
     RowObject.Descrip_Resp AT ROW 13.08 COL 47 HELP
          "Descripcion detallada de la Respuesta del PQR" NO-LABEL WIDGET-ID 104
          VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
          SIZE 40.86 BY 5.38
          BGCOLOR 15 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 89.57 BY 17.92
         BGCOLOR 17 FONT 4 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Pqr
     RowObject.Num_PQR AT ROW 2.62 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 142
          VIEW-AS FILL-IN 
          SIZE 9.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     "Estado" VIEW-AS TEXT
          SIZE 9.43 BY .77 TOOLTIP "Estado" AT ROW 11.04 COL 4.57 WIDGET-ID 136
     "Sucursal" VIEW-AS TEXT
          SIZE 8 BY .92 TOOLTIP "Sucursal" AT ROW 4 COL 20.29 WIDGET-ID 70
     "Producto" VIEW-AS TEXT
          SIZE 17 BY .92 TOOLTIP "Producto" AT ROW 7.08 COL 4.57 WIDGET-ID 72
     "Usuario Responsable" VIEW-AS TEXT
          SIZE 19.43 BY .92 TOOLTIP "Usuario Responsable" AT ROW 10.12 COL 4.57 WIDGET-ID 130
     "Requerimiento" VIEW-AS TEXT
          SIZE 17.43 BY .92 TOOLTIP "Requerimiento" AT ROW 9.08 COL 4.57 WIDGET-ID 124
     "Área Responsable" VIEW-AS TEXT
          SIZE 17.43 BY .92 TOOLTIP "Área Responsable" AT ROW 8.08 COL 4.57 WIDGET-ID 78
     "Descripcion PQR" VIEW-AS TEXT
          SIZE 21 BY .73 TOOLTIP "Descripcion" AT ROW 12.12 COL 4.57 WIDGET-ID 80
     "Canal Recepción ~\ Proceso Misional" VIEW-AS TEXT
          SIZE 26.29 BY .92 TOOLTIP "Canal Recepción" AT ROW 6.08 COL 4.57 WIDGET-ID 74
          FONT 4
     "Usuario" VIEW-AS TEXT
          SIZE 11.43 BY .92 TOOLTIP "Usuario" AT ROW 5.15 COL 4.57 WIDGET-ID 94
     "Fecha De PQR ~\" VIEW-AS TEXT
          SIZE 15.29 BY .92 TOOLTIP "Fecha De Solicitud" AT ROW 4 COL 4.57 WIDGET-ID 68
     "Descripcion Respuesta" VIEW-AS TEXT
          SIZE 21 BY .92 TOOLTIP "Descripcion" AT ROW 12.15 COL 47.29 WIDGET-ID 102
     RECT-318 AT ROW 1.08 COL 3 WIDGET-ID 60
     RECT-319 AT ROW 3.73 COL 3 WIDGET-ID 62
     RECT-321 AT ROW 3.73 COL 31.57 WIDGET-ID 66
     RECT-327 AT ROW 11.92 COL 3 WIDGET-ID 98
     RECT-328 AT ROW 11.92 COL 45.72 WIDGET-ID 100
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 89.57 BY 17.92
         BGCOLOR 17 FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dpqr.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dpqr.i}
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
         HEIGHT             = 17.92
         WIDTH              = 89.57.
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
/* SETTINGS FOR FRAME F-Pqr
   NOT-VISIBLE FRAME-NAME Custom                                        */
ASSIGN 
       FRAME F-Pqr:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Agencia IN FRAME F-Pqr
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
ASSIGN 
       RowObject.Agencia:HIDDEN IN FRAME F-Pqr           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Area_Resp IN FRAME F-Pqr
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.Area_Resp:HIDDEN IN FRAME F-Pqr           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Clase_Producto IN FRAME F-Pqr
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       RowObject.Clase_Producto:HIDDEN IN FRAME F-Pqr           = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_AreasPQR IN FRAME F-Pqr
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_Proceso IN FRAME F-Pqr
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_Producto IN FRAME F-Pqr
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_Requerimientos IN FRAME F-Pqr
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Cod_Proceso IN FRAME F-Pqr
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
ASSIGN 
       RowObject.Cod_Proceso:HIDDEN IN FRAME F-Pqr           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Cod_Producto IN FRAME F-Pqr
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.Cod_Producto:HIDDEN IN FRAME F-Pqr           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Cod_Req IN FRAME F-Pqr
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
ASSIGN 
       RowObject.Cod_Req:HIDDEN IN FRAME F-Pqr           = TRUE.

/* SETTINGS FOR EDITOR RowObject.Descrip_PQR IN FRAME F-Pqr
   EXP-LABEL EXP-HELP                                                   */
ASSIGN 
       RowObject.Descrip_PQR:RETURN-INSERTED IN FRAME F-Pqr  = TRUE.

/* SETTINGS FOR EDITOR RowObject.Descrip_Resp IN FRAME F-Pqr
   NO-ENABLE EXP-LABEL EXP-HELP                                         */
ASSIGN 
       RowObject.Descrip_Resp:RETURN-INSERTED IN FRAME F-Pqr  = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Estado IN FRAME F-Pqr
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
ASSIGN 
       RowObject.Estado:HIDDEN IN FRAME F-Pqr           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Fec_Solicitud IN FRAME F-Pqr
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.Nit IN FRAME F-Pqr
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_Agencia IN FRAME F-Pqr
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nom_cliente IN FRAME F-Pqr
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_Estado IN FRAME F-Pqr
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_Usuario IN FRAME F-Pqr
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_UsuarioResp IN FRAME F-Pqr
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Per_Resp IN FRAME F-Pqr
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
ASSIGN 
       RowObject.Per_Resp:HIDDEN IN FRAME F-Pqr           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Tip_Credito IN FRAME F-Pqr
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.Tip_Credito:HIDDEN IN FRAME F-Pqr           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Usuario IN FRAME F-Pqr
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
ASSIGN 
       RowObject.Usuario:HIDDEN IN FRAME F-Pqr           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Pqr
/* Query rebuild information for FRAME F-Pqr
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Pqr */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Cmb_AreasPQR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_AreasPQR vTableWin
ON VALUE-CHANGED OF Cmb_AreasPQR IN FRAME F-Pqr
DO:
   
  DO WITH FRAME {&FRAME-NAME}:  
      ASSIGN RowObject.Area_Resp:SCREEN-VALUE = STRING(SUBSTRING(Cmb_AreasPQR:SCREEN-VALUE,1,5))
             Cmb_Requerimientos:LIST-ITEMS = ''.
    
    FOR EACH varios WHERE varios.tipo   = 46 
          NO-LOCK:
          FIND FIRST Req_PQR WHERE Req_PQR.Cod_Proceso = INT(RowObject.Cod_Proceso:SCREEN-VALUE)
                               AND Req_PQR.Area_PQR    = INT(RowObject.Area_Resp:SCREEN-VALUE)
                               AND Req_PQR.Cod_Req     = varios.codigo 
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE Req_PQR THEN NEXT.          
          W_Ok = Cmb_Requerimientos:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion).

       
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Proceso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Proceso vTableWin
ON VALUE-CHANGED OF Cmb_Proceso IN FRAME F-Pqr
DO:
   
  DO WITH FRAME {&FRAME-NAME}:
    
    ASSIGN RowObject.Cod_Proceso:SCREEN-VALUE = SUBSTRING(cmb_proceso:SCREEN-VALUE,1,5).
    Cmb_Producto:LIST-ITEMS = ''.


    /* En la tabla varios 01 es captacion  */
   IF RowObject.Cod_Proceso:SCREEN-VALUE = STRING(1,'99999') THEN DO:
      FOR EACH pro_ahorros NO-LOCK:
         W_Ok = Cmb_Producto:ADD-LAST(STRING(Pro_ahorros.Cod_Ahorro,"999") + " - " + Pro_ahorros.Nom_Producto).
         ASSIGN RowObject.Clase_Producto:SCREEN-VALUE = STRING(1). 
      END.
    END.
    ELSE /* En la Tabla Varios 2 es Colocacion */
    IF RowObject.Cod_Proceso:SCREEN-VALUE = STRING(2,'99999') THEN DO:
       FOR EACH pro_Creditos NO-LOCK:
         W_Ok = Cmb_Producto:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto).
         ASSIGN RowObject.Clase_Producto:SCREEN-VALUE = STRING(2). 
       END.
    END.
    ELSE DO:    /*  En la tabla varios 4 es Vinculacion, incluye productos de Credito y Ahorros */
        IF RowObject.Cod_Proceso:SCREEN-VALUE = STRING(4,'99999')  THEN DO:
           FOR EACH pro_Creditos NO-LOCK:
             W_Ok = Cmb_Producto:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto).
             ASSIGN RowObject.Clase_Producto:SCREEN-VALUE = STRING(2). 
           END.
           FOR EACH pro_ahorros NO-LOCK:
              W_Ok = Cmb_Producto:ADD-LAST(STRING(Pro_ahorros.Cod_Ahorro,"999") + " - " + Pro_ahorros.Nom_Producto).
              ASSIGN RowObject.Clase_Producto:SCREEN-VALUE = STRING(1). 
           END.
        END.
    END.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Producto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Producto vTableWin
ON LEAVE OF Cmb_Producto IN FRAME F-Pqr
DO:
  

  IF RowObject.Clase_Producto:SCREEN-VALUE = STRING(2) THEN DO:
     FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = INT(RowObject.Cod_Producto:SCREEN-VALUE)
     NO-LOCK NO-ERROR.
     IF AVAILABLE pro_creditos THEN
        ASSIGN RowObject.Tip_Credito:SCREEN-VALUE = STRING(pro_creditos.Tip_Credito).

  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Producto vTableWin
ON VALUE-CHANGED OF Cmb_Producto IN FRAME F-Pqr
DO:

  ASSIGN RowObject.Cod_Producto:SCREEN-VALUE = SUBSTRING(cmb_producto:SCREEN-VALUE,1,3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Requerimientos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Requerimientos vTableWin
ON VALUE-CHANGED OF Cmb_Requerimientos IN FRAME F-Pqr
DO:
  DO WITH FRAME {&FRAME-NAME}:


     ASSIGN RowObject.Cod_Req:SCREEN-VALUE = SUBSTRING(Cmb_Requerimientos:SCREEN-VALUE,1,5).

     FIND FIRST Req_PQR WHERE Req_PQR.Cod_Proceso = INT(RowObject.Cod_Proceso:SCREEN-VALUE)
                          AND Req_PQR.Area_PQR    = INT(RowObject.Area_Resp:SCREEN-VALUE)
                          AND Req_PQR.Cod_Req     = INT(RowObject.Cod_Req:SCREEN-VALUE) 
     NO-LOCK NO-ERROR.
     IF AVAILABLE Req_PQR THEN DO:

       ASSIGN RowObject.Per_Resp:SCREEN-VALUE = Req_PQR.Usu_Resp.

       FIND usuarios WHERE Usuarios.Usuario = Req_PQR.Usu_Resp NO-LOCK NO-ERROR.
       IF AVAILABLE usuarios THEN DO:

           DISPLAY Usuarios.Usuario + '-' + Usuarios.Nombre @ Nom_UsuarioResp. 
       END.

     END.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Descrip_Resp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Descrip_Resp vTableWin
ON LEAVE OF RowObject.Descrip_Resp IN FRAME F-Pqr
DO:
  IF rowobject.Descrip_Resp NE "" THEN DO:
      ASSIGN rowobject.estado = 2.
  END.
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
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DEF VAR cNew  AS CHAR.

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    { GET NewRecord cNew }.


    IF cnew = 'ADD' THEN DO:
      RUN enableField IN h_dynselect1.
      RUN enableField IN h_dynselect5.
      ENABLE Cmb_Proceso
             Cmb_Producto
             Cmb_AreasPQR
             Cmb_Requerimientos
             /*RowObject.Descrip_PQR
             RowObject.Descrip_Resp */ .
    

      IF RowObject.Per_Resp EQ W_Usuario AND RowObject.estado EQ 1 THEN DO:
          ENABLE RowObject.Descrip_Resp.
      END.
          


      ASSIGN RowObject.Estado:SCREEN-VALUE = STRING(1).
    
    END.

    RUN Nombres_Estados.
    RUN busqueda_usuario(Bw_Usuario).
    RUN busqueda_Agencia(Bw_Agencia).
    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects vTableWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'dvarios.wDB-AWARE':U ,
             INPUT  FRAME F-Pqr:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedvariosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch10RebuildOnReposyesToggleDataTargetsyes':U ,
             OUTPUT h_dvarios1 ).
       RUN repositionObject IN h_dvarios1 ( 1.00 , 84.00 ) NO-ERROR.
       /* Size in AB:  ( 1.35 , 6.43 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Pqr:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldDescripcionDataSourceFilterTipo = 42NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelTipo de SolicitudSortyesViewAsCombo-box:drop-down-listToolTipFormat?HelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameTip_PQRDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldCodigo':U ,
             OUTPUT h_dynselect1 ).
       RUN repositionObject IN h_dynselect1 ( 1.46 , 20.57 ) NO-ERROR.
       RUN resizeObject IN h_dynselect1 ( 0.92 , 23.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dvarios.wDB-AWARE':U ,
             INPUT  FRAME F-Pqr:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedvariosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch10RebuildOnReposyesToggleDataTargetsyes':U ,
             OUTPUT h_dvarios2 ).
       RUN repositionObject IN h_dvarios2 ( 2.35 , 84.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 6.29 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Pqr:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldDescripcionDataSourceFilterTipo = 43NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelSortyesViewAsCombo-box:drop-down-listToolTipFormat?HelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameCanal_ServicioDisplayFieldyesEnableFieldnoLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldCodigo':U ,
             OUTPUT h_dynselect5 ).
       RUN repositionObject IN h_dynselect5 ( 6.08 , 33.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect5 ( 0.92 , 25.00 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect1. */
       RUN addLink ( h_dvarios1 , 'Data':U , h_dynselect1 ).

       /* Links to SmartDataField h_dynselect5. */
       RUN addLink ( h_dvarios2 , 'Data':U , h_dynselect5 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect1 ,
             RowObject.Nit:HANDLE IN FRAME F-Pqr , 'BEFORE':U ).
       RUN adjustTabOrder ( h_dynselect5 ,
             Nom_Usuario:HANDLE IN FRAME F-Pqr , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Busqueda_Agencia vTableWin 
PROCEDURE Busqueda_Agencia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER PW_Agencia AS CHAR.
   
   ASSIGN W_agencia = PW_Agencia.

   DO WITH FRAME {&FRAME-NAME}:
       
       RowObject.agencia:SCREEN-VALUE = PW_Agencia.
       FIND agencias WHERE agencias.agencia = INT(PW_Agencia) NO-LOCK NO-ERROR.
       IF AVAILABLE agencias
       THEN DISPLAY STRING(agencias.agencia) + '-' + agencias.nombre @ Nom_agencia.
       
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Busqueda_Nit vTableWin 
PROCEDURE Busqueda_Nit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER FP_Nit  AS CHAR.
    
    ASSIGN P_Nit = FP_Nit.
       
    DO WITH FRAME {&FRAME-NAME}:
        RowObject.Nit:SCREEN-VALUE = p_nit.
        FIND cliente WHERE cliente.nit = p_nit NO-LOCK NO-ERROR.
        IF AVAILABLE cliente 
           THEN DISPLAY clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.Nombre @ Nom_Cliente.
    END.

   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Busqueda_Usuario vTableWin 
PROCEDURE Busqueda_Usuario :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER PW_Usuario AS CHAR.
    
    ASSIGN W_Usuario = PW_Usuario.

    DO WITH FRAME {&FRAME-NAME}:
        RowObject.usuario:SCREEN-VALUE = PW_Usuario.
        
        FIND usuarios WHERE Usuarios.Usuario = PW_Usuario NO-LOCK NO-ERROR.
        IF AVAILABLE usuarios 
        THEN DISPLAY Usuarios.Usuario + '-' + Usuarios.Nombre @ Nom_Usuario.

    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancelRecord vTableWin 
PROCEDURE cancelRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  DO WITH FRAME {&FRAME-NAME}:
      RUN disableField IN h_dynselect1.
      RUN disableField IN h_dynselect5.
      DISABLE Cmb_Proceso
              Cmb_Producto
              Cmb_AreasPQR
              Cmb_Requerimientos
              RowObject.Descrip_PQR
              RowObject.Descrip_Resp.
  END.
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
  HIDE FRAME F-Pqr.
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

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
      IF p_nit <> "" THEN
     RowObject.Nit:SCREEN-VALUE = p_nit.
     /*
     RowObject.Agencia:SCREEN-VALUE = W_Agencia.
     RowObject.Usuario:SCREEN-VALUE = W_Usuario.
     */
     FIND cliente WHERE cliente.nit = p_nit NO-LOCK NO-ERROR.
     IF AVAILABLE cliente 
       THEN DISPLAY clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.Nombre @ Nom_Cliente.
        
     IF RowObject.Clase_Producto:SCREEN-VALUE = STRING(1,'9') THEN DO:

     END.
     ELSE
     IF RowObject.Clase_Producto:SCREEN-VALUE = STRING(2,'9') THEN DO:

     END.
     
     ASSIGN Cmb_Proceso:LIST-ITEMS = ''
            Cmb_AreasPQR:LIST-ITEMS = ''
            Cmb_Requerimientos:LIST-ITEMS = ''.
     
     FOR EACH Varios WHERE Varios.Tipo = 44 NO-LOCK :
        W_Ok = Cmb_Proceso:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion).
     END.
     FOR EACH Varios WHERE Varios.Tipo = 45 NO-LOCK :
       W_Ok = Cmb_AreasPQR:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion).
     END.

     FOR EACH varios WHERE varios.tipo   = 46 NO-LOCK:
         FIND FIRST Req_PQR WHERE Req_PQR.Cod_Proceso = INT(RowObject.Cod_Proceso:SCREEN-VALUE)
                              AND Req_PQR.Area_PQR    = INT(RowObject.Area_Resp:SCREEN-VALUE)
                              AND Req_PQR.Cod_Req     = varios.codigo 
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Req_PQR THEN NEXT.
               W_Ok = Cmb_Requerimientos:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion).
     END.

        /* En la tabla varios 01 es captacion  */
     IF RowObject.Cod_Proceso:SCREEN-VALUE = STRING(1,'99999') THEN DO:
          FOR EACH pro_ahorros NO-LOCK:
             W_Ok = Cmb_Producto:ADD-LAST(STRING(Pro_ahorros.Cod_Ahorro,"999") + " - " + Pro_ahorros.Nom_Producto).
             ASSIGN RowObject.Clase_Producto:SCREEN-VALUE = STRING(1). 
          END.
     END.
     ELSE /* En la Tabla Varios 2 es Colocacion */
     IF RowObject.Cod_Proceso:SCREEN-VALUE = STRING(2,'99999') THEN DO:
           FOR EACH pro_Creditos NO-LOCK:
             W_Ok = Cmb_Producto:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto).
             ASSIGN RowObject.Clase_Producto:SCREEN-VALUE = STRING(2). 
           END.
     END.
     ELSE DO:    /*  En la tabla varios 4 es Vinculacion, incluye productos de Credito y Ahorros */
            IF RowObject.Cod_Proceso:SCREEN-VALUE = STRING(4,'99999')  THEN DO:
               FOR EACH pro_Creditos NO-LOCK:
                 W_Ok = Cmb_Producto:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto).
                 ASSIGN RowObject.Clase_Producto:SCREEN-VALUE = STRING(2). 
               END.
               FOR EACH pro_ahorros NO-LOCK:
                  W_Ok = Cmb_Producto:ADD-LAST(STRING(Pro_ahorros.Cod_Ahorro,"999") + " - " + Pro_ahorros.Nom_Producto).
                  ASSIGN RowObject.Clase_Producto:SCREEN-VALUE = STRING(1). 
               END.
            END.
     END.

     FIND FIRST Varios WHERE Varios.tipo = 44 AND /* Proceso*/
                             Varios.Codigo = INT(RowObject.Cod_Proceso:SCREEN-VALUE) NO-LOCK NO-ERROR.
     ASSIGN Cmb_Proceso:SCREEN-VALUE = (IF AVAILABLE Varios THEN STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion ELSE "").
     
     FIND FIRST Varios WHERE Varios.tipo = 45 AND /* Area Responsable */
                             Varios.Codigo = INT(RowObject.Area_resp:SCREEN-VALUE) NO-LOCK NO-ERROR.
     ASSIGN Cmb_AreasPQR:SCREEN-VALUE = (IF AVAILABLE Varios THEN STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion ELSE "").
     
     /* Producto - Captacion */
     IF RowObject.Cod_Proceso:SCREEN-VALUE = STRING(1,'99999') THEN DO:
        FIND FIRST Pro_ahorros WHERE Pro_ahorros.Cod_Ahorro = INT(RowObject.Cod_producto:SCREEN-VALUE) NO-LOCK NO-ERROR.
        ASSIGN Cmb_Producto:SCREEN-VALUE = (IF AVAILABLE Pro_ahorros THEN STRING(Pro_ahorros.Cod_Ahorro,"999") + " - " + Pro_ahorros.Nom_Producto ELSE "").
     END.
     ELSE /* Producto 2 - Colocacion */
     IF RowObject.Cod_Proceso:SCREEN-VALUE = STRING(2,'99999') THEN DO:
        FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito = INT(RowObject.Cod_producto:SCREEN-VALUE) NO-LOCK NO-ERROR.
        ASSIGN Cmb_Producto:SCREEN-VALUE = (IF AVAILABLE Pro_Creditos THEN STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto ELSE "").
     END.
     
     FIND usuarios WHERE Usuarios.Usuario = RowObject.Per_Resp:SCREEN-VALUE NO-LOCK NO-ERROR.
     DISPLAY (IF AVAILABLE usuarios THEN Usuarios.Usuario + '-' + Usuarios.Nombre ELSE "") @ Nom_UsuarioResp. 

      ASSIGN RowObject.Area_Resp:SCREEN-VALUE = STRING(SUBSTRING(Cmb_AreasPQR:SCREEN-VALUE,1,5)).

      FIND FIRST Req_PQR WHERE Req_PQR.Cod_Proceso = INT(RowObject.Cod_Proceso:SCREEN-VALUE)
                           AND Req_PQR.Area_PQR    = INT(RowObject.Area_Resp:SCREEN-VALUE) NO-LOCK NO-ERROR.
      FIND FIRST varios WHERE varios.tipo   = 46 AND 
                              varios.codigo = Req_PQR.Cod_Req NO-LOCK NO-ERROR.
      ASSIGN Cmb_Requerimientos:SCREEN-VALUE = (IF AVAILABLE varios THEN STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion ELSE "").

     RUN nombres_estados.
     RUN busqueda_usuario(RowObject.Usuario:SCREEN-VALUE).
     RUN busqueda_agencia(RowObject.Agencia:SCREEN-VALUE).
        
  END.

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

  /* Code placed here will execute AFTER standard behavior.    */

  
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN Cmb_Proceso:LIST-ITEMS = ''
            Cmb_AreasPQR:LIST-ITEMS = ''.

    FOR EACH Varios WHERE Varios.Tipo = 44 NO-LOCK :
       W_Ok = Cmb_Proceso:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion).
    END.
    FOR EACH Varios WHERE Varios.Tipo = 45 NO-LOCK :
       W_Ok = Cmb_AreasPQR:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion).
    END.

  END.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Nombres_Estados vTableWin 
PROCEDURE Nombres_Estados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    IF RowObject.Estado:SCREEN-VALUE = STRING(1) THEN
       DISPLAY "Por Tramitar" @ Nom_Estado.
    ELSE 
    IF RowObject.Estado:SCREEN-VALUE = STRING(2) THEN
       DISPLAY "Solucionado" @ Nom_Estado.
    ELSE
    IF RowObject.Estado:SCREEN-VALUE = STRING(3) THEN
       DISPLAY "Anulado" @ Nom_Estado.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRcbeDtos vTableWin 
PROCEDURE pRcbeDtos :
/*------------------------------------------------------------------------------
  Purpose:    pRcbeDtos 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER cnit AS CHAR NO-UNDO.
    DEF INPUT PARAMETER cusu AS CHAR NO-UNDO.
    DEF INPUT PARAMETER cage AS CHAR NO-UNDO.
    P_Nit     = cnit.
    W_Usuario = cusu.
    W_Agencia = cage.
    BW_Usuario = cusu.
    BW_Agencia = cage.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

