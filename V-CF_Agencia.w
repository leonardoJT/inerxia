&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*-----------------------------------------------------------------------------

  ARCHIVO    : V-Oficin.W
  DESCRIPCION: Mantenimiento de las Agencias.
  
-------------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
  {Incluido/Variable.I}
   DEFINE BUFFER Tmp_Agencias FOR Agencias.
   DEFINE VARIABLE W_TipAux  LIKE Agencias.Tip_agencia.
   DEFINE VARIABLE W_Rpta      AS LOGICAL.
   DEFINE VARIABLE W_Rowid     AS ROWID.
   DEFINE VARIABLE Tmp_Ciu   LIKE Agencia.Ciudad.
   DEFINE VARIABLE W_CbStr1    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE W_Metodo1   AS LOGICAL NO-UNDO.

   DEFINE VARIABLE P_Nit      LIKE Clientes.Nit.
   DEFINE VARIABLE p_Nombre   LIKE Clientes.Nombre.
   DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
   DEFINE VARIABLE P_CliAge   LIKE Clientes.Agencia.
   DEFINE VAR W_Ok AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Agencia

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES Agencia
&Scoped-define FIRST-EXTERNAL-TABLE Agencia


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Agencia.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Agencias.Datafono[1] Agencias.Datafono[2] ~
Agencias.Datafono[3] Agencias.Datafono[4] Agencias.Estado Agencias.Nombre ~
Agencias.Nit_Director Agencias.Direccion Agencias.email Agencias.Telefono ~
Agencias.Fax Agencias.Modem Agencias.Mensaje Agencias.Tip_Agencia ~
Agencias.Fec_Creacion Agencias.PorMax_ConcentracionInv ~
Agencias.ValMax_Bancos Agencias.ValMax_Caja Agencias.ValMax_Inversiones 
&Scoped-define ENABLED-TABLES Agencias
&Scoped-define FIRST-ENABLED-TABLE Agencias
&Scoped-Define ENABLED-OBJECTS TB-COficinas Tgl-Cerrar W_Centidad Btn_Lugar ~
Cmb_Zonas RECT-126 RECT-127 
&Scoped-Define DISPLAYED-FIELDS Agencias.Datafono[1] Agencias.Datafono[2] ~
Agencias.Datafono[3] Agencias.Datafono[4] Agencias.Agencia Agencias.Estado ~
Agencias.Nombre Agencias.Nit_Director Agencias.Direccion Agencias.email ~
Agencias.Telefono Agencias.Fax Agencias.Modem Agencias.Mensaje ~
Agencias.Ciudad Agencias.Zona Agencias.Tip_Agencia Agencias.Fec_Trabajo ~
Agencias.Fec_Creacion Agencias.Fec_Retiro Agencias.PorMax_ConcentracionInv ~
Agencias.ValMax_Bancos Agencias.ValMax_Caja Agencias.ValMax_Inversiones 
&Scoped-define DISPLAYED-TABLES Agencias
&Scoped-define FIRST-DISPLAYED-TABLE Agencias
&Scoped-Define DISPLAYED-OBJECTS TB-COficinas Tgl-Cerrar W_Centidad ~
W_NomLugar Cmb_Zonas Nom_Director 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Lugar 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 119" 
     SIZE 3 BY .54.

DEFINE VARIABLE Cmb_Zonas AS CHARACTER FORMAT "X(256)":U 
     LABEL "Zonas" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 36 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Centidad AS CHARACTER FORMAT "X(40)":U 
     LABEL "Entidad" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 38 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom_Director AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomLugar AS CHARACTER FORMAT "X(50)":U 
     LABEL "Ciudad" 
     VIEW-AS FILL-IN 
     SIZE 36 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-126
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50 BY 8.35.

DEFINE RECTANGLE RECT-127
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 8.35.

DEFINE VARIABLE TB-COficinas AS LOGICAL INITIAL no 
     LABEL "Cerrar Oficinas Condicional" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE Tgl-Cerrar AS LOGICAL INITIAL no 
     LABEL "Cerrar Todas las agencias" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Agencia
     TB-COficinas AT ROW 4.23 COL 14 WIDGET-ID 22
     Agencias.Datafono[1] AT ROW 14.46 COL 57 COLON-ALIGNED NO-LABEL WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
     Agencias.Datafono[2] AT ROW 15.54 COL 57 COLON-ALIGNED NO-LABEL WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
     Agencias.Datafono[3] AT ROW 14.46 COL 76.72 COLON-ALIGNED NO-LABEL WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
     Agencias.Datafono[4] AT ROW 15.54 COL 76.72 COLON-ALIGNED NO-LABEL WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
     Tgl-Cerrar AT ROW 5.04 COL 14 WIDGET-ID 2
     W_Centidad AT ROW 2.08 COL 12 COLON-ALIGNED HELP
          "Seleccione la entidad a la cual pertenece la Agencia"
     Agencias.Agencia AT ROW 3.15 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
          BGCOLOR 15 
     Agencias.Estado AT ROW 3.15 COL 21 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Normal", 1,
"Cierre", 2,
"Retirada", 3
          SIZE 31 BY .81
          BGCOLOR 17 
     Agencias.Nombre AT ROW 6.12 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY .81
          BGCOLOR 15 
     Agencias.Nit_Director AT ROW 7.19 COL 12 COLON-ALIGNED
          LABEL "Cédula Director"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     Agencias.Direccion AT ROW 9.08 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 36 BY .81
          BGCOLOR 15 
     Agencias.email AT ROW 10.15 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 36 BY .81
          BGCOLOR 15 
     Btn_Lugar AT ROW 11.23 COL 50
     W_NomLugar AT ROW 11.23 COL 12 COLON-ALIGNED
     Cmb_Zonas AT ROW 12.31 COL 12 COLON-ALIGNED
     Agencias.Telefono AT ROW 13.38 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY .81
          BGCOLOR 15 
     Agencias.Fax AT ROW 14.46 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY .81
          BGCOLOR 15 
     Agencias.Modem AT ROW 15.54 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY .81
          BGCOLOR 15 
     Agencias.Mensaje AT ROW 18.23 COL 14 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 80 BY 1.88
          BGCOLOR 15 FGCOLOR 7 FONT 5
     Nom_Director AT ROW 7.19 COL 23 COLON-ALIGNED NO-LABEL
     Agencias.Ciudad AT ROW 13.38 COL 48 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.14 BY .81
          BGCOLOR 17 FGCOLOR 17 
     Agencias.Zona AT ROW 14.19 COL 48 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.29 BY .81
          BGCOLOR 17 FGCOLOR 17 
     Agencias.Tip_Agencia AT ROW 2.35 COL 70 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Remota", "D":U,
"Central", "C":U
          SIZE 21 BY .54
          BGCOLOR 17 
     Agencias.Fec_Trabajo AT ROW 3.42 COL 70 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Agencias.Fec_Creacion AT ROW 4.5 COL 70 COLON-ALIGNED
          LABEL "Fecha Apertura"
          VIEW-AS FILL-IN 
          SIZE 22 BY .81
          BGCOLOR 18 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Agencia
     Agencias.Fec_Retiro AT ROW 5.58 COL 70 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Agencias.PorMax_ConcentracionInv AT ROW 9.08 COL 77 COLON-ALIGNED
          LABEL "% Máximo Concentración"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     Agencias.ValMax_Bancos AT ROW 10.15 COL 77 COLON-ALIGNED
          LABEL "Máximo en Bancos"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     Agencias.ValMax_Caja AT ROW 11.23 COL 77 COLON-ALIGNED
          LABEL "Máximo en Caja"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     Agencias.ValMax_Inversiones AT ROW 12.31 COL 77 COLON-ALIGNED
          LABEL "Máximo de Inversión"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     "Datos Generales" VIEW-AS TEXT
          SIZE 16 BY .54 AT ROW 1.27 COL 14
          BGCOLOR 17 FGCOLOR 7 FONT 5
     " Controles" VIEW-AS TEXT
          SIZE 10 BY 1.04 AT ROW 8 COL 59
          FGCOLOR 7 FONT 5
     "                Datáfonos en la agencia" VIEW-AS TEXT
          SIZE 39 BY .81 AT ROW 13.42 COL 55.57 WIDGET-ID 4
          BGCOLOR 18 FGCOLOR 15 FONT 6
     "3ro." VIEW-AS TEXT
          SIZE 3 BY .81 AT ROW 14.46 COL 75.57 WIDGET-ID 8
          BGCOLOR 18 FGCOLOR 15 
     "4to." VIEW-AS TEXT
          SIZE 3 BY .81 AT ROW 15.5 COL 75.57 WIDGET-ID 10
          BGCOLOR 18 FGCOLOR 15 
     "1ro." VIEW-AS TEXT
          SIZE 3 BY .81 AT ROW 14.46 COL 55.72 WIDGET-ID 12
          BGCOLOR 18 FGCOLOR 15 
     "2do." VIEW-AS TEXT
          SIZE 3 BY .81 AT ROW 15.5 COL 55.72 WIDGET-ID 14
          BGCOLOR 18 FGCOLOR 15 
     "Ubicación" VIEW-AS TEXT
          SIZE 10 BY 1.08 AT ROW 8 COL 14
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Tipo de Agencia" VIEW-AS TEXT
          SIZE 15 BY 1.08 AT ROW 1 COL 73
          FGCOLOR 7 FONT 5
     "  Mensaje Agencia" VIEW-AS TEXT
          SIZE 17 BY 1.08 AT ROW 17.15 COL 13
          BGCOLOR 17 FGCOLOR 7 FONT 5
     RECT-126 AT ROW 8.54 COL 4
     RECT-127 AT ROW 8.54 COL 55
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: Agencia.Agencia
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 19.58
         WIDTH              = 104.29.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Agencia
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Agencia:SCROLLABLE       = FALSE
       FRAME F-Agencia:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Agencias.Agencia IN FRAME F-Agencia
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Agencias.Ciudad IN FRAME F-Agencia
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Agencias.Fec_Creacion IN FRAME F-Agencia
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Agencias.Fec_Retiro IN FRAME F-Agencia
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Agencias.Fec_Trabajo IN FRAME F-Agencia
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Agencias.Nit_Director IN FRAME F-Agencia
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Nom_Director IN FRAME F-Agencia
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Agencias.PorMax_ConcentracionInv IN FRAME F-Agencia
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Agencias.ValMax_Bancos IN FRAME F-Agencia
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Agencias.ValMax_Caja IN FRAME F-Agencia
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Agencias.ValMax_Inversiones IN FRAME F-Agencia
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN W_NomLugar IN FRAME F-Agencia
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Agencias.Zona IN FRAME F-Agencia
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Agencia
/* Query rebuild information for FRAME F-Agencia
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Agencia */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Agencias.Agencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Agencias.Agencia V-table-Win
ON LEAVE OF Agencias.Agencia IN FRAME F-Agencia /* Agencia */
DO:
  FIND FIRST Agencias WHERE Agencias.Agencia EQ INTEGER(Agencias.Agencia:SCREEN-VALUE IN FRAME F-Agencia) NO-LOCK NO-ERROR.
  IF AVAILABLE(Agencias) THEN DO:
     MESSAGE "Ya existe una Agencia con el Código: " Agencias.Agencia:SCREEN-VALUE IN FRAME F-Agencia SKIP
             "Digite un Código que no exista!" VIEW-AS ALERT-BOX ERROR.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Lugar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Lugar V-table-Win
ON CHOOSE OF Btn_Lugar IN FRAME F-Agencia /* Button 119 */
DO:
  DEFINE VAR P_Ubi LIKE Ubicacion.Ubicacion.
  DEFINE VAR P_NUbi AS CHARACTER FORMAT "X(80)".
  DO WITH FRAME F-Main:
     RUN C-Ubicacion.r (OUTPUT P_Ubi, OUTPUT P_NUbi).
     ASSIGN W_NomLugar:SCREEN-VALUE = LC(P_NUbi)
           Agencias.Ciudad:SCREEN-VALUE = P_Ubi.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Zonas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Zonas V-table-Win
ON VALUE-CHANGED OF Cmb_Zonas IN FRAME F-Agencia /* Zonas */
DO:
  ASSIGN Agencias.Zona:SCREEN-VALUE = SUBSTRING(Cmb_Zonas:SCREEN-VALUE,1,4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Agencias.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Agencias.Estado V-table-Win
ON MOUSE-SELECT-CLICK OF Agencias.Estado IN FRAME F-Agencia /* Estado */
DO:
  APPLY "VALUE-CHANGED" TO Agencias.Estado IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Agencias.Nit_Director
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Agencias.Nit_Director V-table-Win
ON LEAVE OF Agencias.Nit_Director IN FRAME F-Agencia /* Cédula Director */
DO:
DO WITH FRAME {&FRAME-NAME}:
     FIND Clientes WHERE Clientes.Nit EQ Agencias.Nit_Director:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAILABLE(Clientes) THEN
        Nom_Director:SCREEN-VALUE = TRIM(Clientes.Nombre) + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     ELSE DO:
        RUN C-Clientes.R(INPUT 1, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_CliAge).
        ASSIGN Nom_Director:SCREEN-VALUE    = TRIM(P_Nombre) + " " + P_Apellido
               Agencias.Nit_Director:SCREEN-VALUE = P_Nit.
    FIND Clientes WHERE Clientes.Agencia EQ P_CliAge AND Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
  END.
  IF AVAILABLE(Clientes) THEN DO:
   IF Clientes.Tipo_Identificacion EQ "NIT" THEN DO:
       MESSAGE "Debe ser escogida una Persona Natural" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Agencias.Nombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Agencias.Nombre V-table-Win
ON LEAVE OF Agencias.Nombre IN FRAME F-Agencia /* Nombre */
OR RETURN OF Agencias.Nombre DO:
  IF Agencias.Nombre:SCREEN-VALUE EQ "" THEN    
     RETURN.
  APPLY "TAB" TO SELF. 
  ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Agencias.Nombre V-table-Win
ON TAB OF Agencias.Nombre IN FRAME F-Agencia /* Nombre */
OR MOUSE-SELECT-DBLCLICK OF Agencias.Nombre
  OR RETURN OF Agencias.Nombre DO:
     ON RETURN RETURN.  
     IF Agencias.Nombre:SCREEN-VALUE EQ "" THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 204,OUTPUT W_Eleccion).
        APPLY "ENTRY" TO Agencias.Nombre.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TB-COficinas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TB-COficinas V-table-Win
ON VALUE-CHANGED OF TB-COficinas IN FRAME F-Agencia /* Cerrar Oficinas Condicional */
DO:
    DEFINE VARIABLE vlOP AS LOGICAL INITIAL FALSE NO-UNDO.
    ASSIGN TB-COficinas.      
    IF TB-COficinas THEN DO:
        MESSAGE "Desea cerrar todas las oficinas"
                "excepto Calle 53 y Direccion "
                "General"
            VIEW-AS ALERT-BOX INFO BUTTONS YES-NO SET vlOP.
        IF vlOP THEN DO:
            FOR EACH Agencias WHERE Agencias.Agencia NE 55 AND Agencias.Agencia NE 8 AND 
                                    Agencias.Agencia NE 24:
                UPDATE Agencias.Estado = 2.    
            END.
        END.
        ELSE TB-COficinas:SCREEN-VALUE = "NO".
    END.
    ELSE
        FOR EACH Agencias EXCLUSIVE-LOCK:
            UPDATE Agencias.Estado = 1.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tgl-Cerrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tgl-Cerrar V-table-Win
ON VALUE-CHANGED OF Tgl-Cerrar IN FRAME F-Agencia /* Cerrar Todas las agencias */
DO:
  ASSIGN tgl-cerrar.      
  IF tgl-cerrar THEN
     FOR EACH agencias WHERE agencia NE 55 AND agencias.estado EQ 1 :
        ASSIGN estado = 2.
     END.
  ELSE
     FOR EACH agencias WHERE agencia NE 55 AND agencias.estado EQ 2  :
        ASSIGN estado = 1.
     END.
  FIND FIRST agencias WHERE agencias.agencia EQ w_agencia NO-LOCK NO-ERROR.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Agencias.Tip_Agencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Agencias.Tip_Agencia V-table-Win
ON MOUSE-SELECT-CLICK OF Agencias.Tip_Agencia IN FRAME F-Agencia /* Tipo Agencia */
DO:
  APPLY "VALUE-CHANGED" TO Agencias.Tip_Agencia.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Agencias.Tip_Agencia V-table-Win
ON VALUE-CHANGED OF Agencias.Tip_Agencia IN FRAME F-Agencia /* Tipo Agencia */
OR RETURN OF Agencias.Tip_Agencia DO:
 DO WITH FRAME {&FRAME-NAME}:
    IF Agencias.Tip_agencia:SCREEN-VALUE = "C" THEN DO:
       W_Rowid = ROWID(Agencias).
       FIND FIRST Tmp_Agencias WHERE Tmp_Agencias.Tip_Agencia EQ "C" 
                               AND   Tmp_Agencias.Agencia     NE  INTEGER(Agencias.Agencia :SCREEN-VALUE)
                               AND   Tmp_Agencias.Estado      NE  3  NO-LOCK NO-ERROR.
       IF AVAILABLE(Tmp_Agencias) THEN DO:
          RUN MostrarMensaje IN W_Manija (INPUT 17,OUTPUT W_Eleccion).
          ASSIGN Agencias.Tip_Agencia:SCREEN-VALUE = "D".
          APPLY "ENTRY" TO Agencias.Tip_Agencia.
          RETURN NO-APPLY.
       END.
    END.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Centidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Centidad V-table-Win
ON VALUE-CHANGED OF W_Centidad IN FRAME F-Agencia /* Entidad */
DO:
  ASSIGN W_CEntidad.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "Agencia"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Agencia"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Agencia.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */

  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN Agencias.Estado:SCREEN-VALUE        = "1"
            Agencias.Tip_agencia:SCREEN-VALUE   = "D"
            Agencias.Fec_Retiro:SCREEN-VALUE    = ""
            Agencias.Fec_Creacion:SCREEN-VALUE  = STRING(TODAY,"99/99/9999").
     ASSIGN Agencias.Fec_Creacion:BGCOLOR = 15
            Agencias.Fec_Creacion:FGCOLOR = 0.
     FIND LAST Agencias NO-LOCK NO-ERROR.
     IF AVAILABLE(Agencias) THEN
        Agencias.Agencia:SCREEN-VALUE = STRING(Agencias.Agencia + 1).
     ELSE
        Agencias.Agencia:SCREEN-VALUE = "1".
  END.
  ENABLE Agencias.Agencia Agencias.Fec_Creacion WITH FRAME F-Agencia.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR W_Evento  AS   CHARACTER FORMAT "XX".
  DEFINE VAR G_Ciudad  LIKE Agencias.Ciudad.
  DEFINE VAR G_Zona    LIKE Agencias.Zona.
  DEFINE VAR G_Entidad LIKE Agencia.Entidad.
  DEFINE VAR AgeWk     LIKE Agencias.Agencia.
  AgeWk = INTEGER(Agencias.Agencia:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN W_CEntidad.
     IF Agencias.Nombre:SCREEN-VALUE EQ "" THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 204, OUTPUT W_Eleccion).
        APPLY "ENTRY" TO Agencias.Nombre. 
        RETURN ERROR.
     END.
     IF Agencias.Tip_agencia:SCREEN-VALUE = "C" THEN DO:
        FIND FIRST Tmp_Agencias WHERE Tmp_Agencias.Tip_Agencia EQ "C" 
                                AND   Tmp_Agencias.Agencia     NE  INTEGER(Agencias.Agencia:SCREEN-VALUE)
                                AND   Tmp_Agencias.Estado      NE  3  NO-LOCK NO-ERROR.
        IF AVAILABLE(Tmp_Agencias) THEN DO:
           RUN MostrarMensaje IN W_Manija (INPUT 17,OUTPUT W_Eleccion).
           RETURN ERROR.
        END.
     END.
     /*asignar zona y ciudad*/
    
     FIND FIRST Entidad WHERE Entidad.Nombre EQ TRIM(W_CEntidad) NO-LOCK NO-ERROR.
     IF AVAILABLE(Entidad) THEN DO:
        ASSIGN G_Entidad = Entidad.Entidad.
     END.
     
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ).
     Agencias.Agencia = AgeWk.
     ASSIGN Agencias.Ciudad = G_Ciudad
            Agencias.Zona   = G_Zona
            Agencia.Entidad = G_Entidad
            Agencia.Zona = INTEGER(Agencias.Zona:SCREEN-VALUE IN FRAME F-Agencia)
            Agencia.Ciudad = Agencias.Ciudad:SCREEN-VALUE IN FRAME F-Agencia.
     RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
     IF RETURN-VALUE = "YES" THEN DO:
        ASSIGN Agencias.Fec_Creacion = W_Fecha
               Agencias.Fec_Trabajo  = W_Fecha.
     END.
        ELSE ASSIGN Agencias.Fec_Creacion = DATE(Agencias.Fec_Creacion:SCREEN-VALUE). 
        
            
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
   OBSERVACIONES : Procedimiento que valida la existencia de saldos en la Agencia
                   para luego inactivarla.       
  ------------------------------------------------------------------------------*/
  
  DEFINE VAR W_OfiTmp LIKE Agencias.Agencia.
  DEFINE VARIABLE  TEXTO  AS CHAR.
  
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN W_OfiTmp = INTEGER(Agencias.Agencia:SCREEN-VALUE).
     IF Agencias.Fec_Retiro:SCREEN-VALUE NE "" THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 20,OUTPUT W_Eleccion).
     END.
     ELSE DO:
        RUN MostrarMensaje IN W_Manija (INPUT 19,OUTPUT W_Eleccion).
        IF W_Eleccion THEN DO:
           RUN Val_Agencia(INPUT W_OfiTmp) NO-ERROR.
           IF ERROR-STATUS:ERROR THEN DO:
              MESSAGE "No Es Posible Inhabilitar la Agencia."
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
              TITLE "Error En Agencias".
              RETURN ERROR.
           END.
           ELSE DO:
              RUN dispatch IN THIS-PROCEDURE ('update-record':U).
              IF AVAILABLE(Agencias) THEN DO:
                 ASSIGN  Agencias.Fec_Retiro = TODAY
                         Agencias.Estado     = 3. 
                    TEXTO = "ADV: INACTIVA Registro, Agencias. Codigo: " + STRING(Agencias.Agencia).
                 RUN P-GraLog IN W_Manija (INPUT TEXTO ).
              END.
              RUN dispatch IN THIS-PROCEDURE ('end-update':U).
              RUN dispatch IN THIS-PROCEDURE ('display-fields':U).
           END.
        END.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy V-table-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
       FIND Clientes WHERE Clientes.Nit EQ Agencias.Nit_Director NO-LOCK NO-ERROR.
        IF AVAILABLE(Clientes) THEN
            Nom_Director = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.Nombre.
        ELSE
            Nom_Director = "No existe en clientes".
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
     IF AVAILABLE(Agencias) THEN DO:
         DEFINE VARIABLE W_NUbicacion AS CHARACTER FORMAT "X(50)".
         FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "B" AND Ubicacion.Ubicacion EQ Agencias.Ciudad NO-LOCK NO-ERROR.
         IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = Ubicacion.Nombre.
         FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Agencias.Ciudad,1,5) NO-LOCK NO-ERROR.
         IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = W_NUbicacion + " - " + Ubicacion.Nombre.
         FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "D" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Agencias.Ciudad,1,2) NO-LOCK NO-ERROR.
         IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = W_NUbicacion + " - " + Ubicacion.Nombre.
         W_NomLugar:SCREEN-VALUE = LC(W_NUbicacion).

         FIND Zonas WHERE Zonas.Cod_Zona EQ Agencias.Zona NO-LOCK NO-ERROR.
         IF AVAILABLE(Zonas) THEN
           Cmb_Zonas:SCREEN-VALUE = STRING(Zonas.Cod_Zona,"9999") + " - " + Zonas.Nombre.
         ELSE
           Cmb_Zonas:SCREEN-VALUE = "0000 - Zona No Asignada".
        FIND FIRST Entidad WHERE Entidad.Entidad EQ Agencias.Entidad NO-LOCK NO-ERROR.
        IF AVAILABLE(Entidad) THEN DO:
           ASSIGN W_CEntidad:SCREEN-VALUE = Entidad.Nombre.
        END.
        ELSE ASSIGN W_CEntidad:SCREEN-VALUE = "".
        DISABLE Agencias.Agencia WITH FRAME F-Agencia.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 DO WITH FRAME {&FRAME-NAME}:
     ASSIGN W_CEntidad:LIST-ITEMS = "".
     FOR EACH Entidad WHERE Entidad.Estado EQ 1 NO-LOCK:
         W_CEntidad:ADD-LAST(STRING(Entidad.Nombre,"X(40)")).    
     END.
  END.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .
  DISABLE Agencias.Agencia WITH FRAME F-Agencia.
  W_Ok = Cmb_Zonas:ADD-LAST("0000 - Zona No Asignada").
  FOR EACH Zonas NO-LOCK:
      W_Ok = Cmb_Zonas:ADD-LAST(STRING(Zonas.Cod_Zona,"9999") + " - " + Zonas.Nombre).
  END.
  FIND Zonas WHERE Zonas.Cod_Zona EQ Agencias.Zona NO-LOCK NO-ERROR.
  IF AVAILABLE(Zonas) THEN
     Cmb_Zonas:SCREEN-VALUE = STRING(Zonas.Cod_Zona,"9999") + " - " + Zonas.Nombre.
  ELSE
     Cmb_Zonas:SCREEN-VALUE = "0000 - Zona No Asignada".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  RUN check-modified IN THIS-PROCEDURE ( INPUT 'clear':U ).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR W_Evento AS CHAR FORMAT "X(2)" INITIAL "SA".
 DEFINE VARIABLE TEXTO AS CHAR.
  /* Code placed here will execute PRIOR to standard behavior. */
  IF TRIM(Agencias.Agencia:SCREEN-VALUE IN FRAME {&FRAME-NAME}) = "000" THEN
   DO:
      RUN MostrarMensaje IN W_Manija (INPUT 18,OUTPUT W_Eleccion).
      APPLY "ENTRY" TO Agencias.Agencia.
      RETURN ERROR.
   END.  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
  RUN Get-Attribute IN THIS-PROCEDURE ("ADM-NEW-RECORD").
  ASSIGN Agencias.FEC_Creacion:BGCOLOR = 18
         Agencias.Fec_Creacion:FGCOLOR = 15.
  DISABLE Agencias.Fec_Creacion WITH FRAME {&FRAME-NAME}.
  IF RETURN-VALUE = "YES" THEN DO:
     FIND Agencias WHERE Agencias.Agencia EQ INTEGER(Agencias.Agencia:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
                   NO-ERROR.
     IF AVAILABLE(Agencias) THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 16,W_Eleccion).
        APPLY "ENTRY" TO Agencias.Agencia.
        RETURN ERROR.
     END.
    ELSE
      ASSIGN W_Evento = "AD".
   END.
  IF W_Evento EQ "AD" THEN
         TEXTO = "NOR: CREA Registro, Agencias. Codigo: " + STRING(Agencias.Agencia).
    /* RUN P-GraLog IN W_Manija (INPUT TEXTO). */
  ELSE
          TEXTO = "NOR: SALVA Registro, Agencias. Codigo: " + STRING(Agencias.Agencia).
    /* RUN P-GraLog IN W_Manija (INPUT TEXTO). */
  DISABLE Agencias.Agencia WITH FRAME F-Agencia.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "Agencia"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Val_Agencia V-table-Win 
PROCEDURE Val_Agencia :
/*------------------------------------------------------------------------------
  OBSERVACIONES : Validar Agencias.       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER W_OfiTem  LIKE Agencias.Agencia.
  DEFINE VAR   W_MesA              AS   INTEGER FORMAT "99".
  DEFINE VAR   W_AnoA              AS   INTEGER FORMAT "9999".
  
  IF MONTH(TODAY) EQ 1 THEN
     ASSIGN W_MesA = 12
            W_AnoA = YEAR(TODAY) - 1.
  ELSE
     ASSIGN W_MesA = MONTH(TODAY) - 1
            W_AnoA = YEAR(TODAY).
            
  
  FOR EACH Anexos WHERE Anexos.Agencia EQ W_OfiTem 
                  AND   Anexos.Ano     EQ YEAR(TODAY)
                  AND   Anexos.Mes     EQ MONTH(TODAY) NO-LOCK:
      IF Anexos.Sdo_Final GT 0 THEN
         RETURN ERROR.
  END.
  
  FOR EACH Sal_Cuenta WHERE Sal_Cuenta.Agencia EQ W_OfiTem 
                      AND   Sal_Cuenta.Ano     EQ YEAR(TODAY)
                      NO-LOCK:
      IF Sal_Cuenta.Sal_Inicial GT 0 THEN
         RETURN ERROR.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

