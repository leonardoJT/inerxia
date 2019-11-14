&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  File: W-Tercero_Nvo.W
  Description: Creación terceros en tabla clientes.

  Input Parameters:  Cédula/Nit
  Output Parameters: Cédula/Nit

  Author: 

  Created: Agosto 26/04.

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

 {Incluido\VARIABLE.I "SHARED"}
  DEFI INPUT-OUTPUT PARAMETER W_NitTer LIKE Clientes.Nit.

  DEFI VAR W_OfiUsu LIKE Agencias.Agencia.
  /* para llamado a Ubicacion*/
  DEFINE VAR P_Ubi        LIKE Ubicacion.Ubicacion.
  DEFINE VAR P_NUbi       AS CHARACTER FORMAT "X(80)".
  DEFINE VAR W_NUbicacion AS CHARACTER FORMAT "X(50)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Nuevos

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Clientes.Email Clientes.Estado Clientes.Nit ~
Clientes.Tipo_Identificacion Clientes.Tipo_Cliente Clientes.Nombre ~
Clientes.Apellido1 Clientes.Apellido2 Clientes.Tel_comercial ~
Clientes.Dir_comercial Clientes.Tipo_Vinculo 
&Scoped-define ENABLED-TABLES Clientes
&Scoped-define FIRST-ENABLED-TABLE Clientes
&Scoped-Define ENABLED-OBJECTS Cmb_Agencia Btn_Lugar Btn_Salvar Btn_Ing ~
Btn_Salir RECT-302 RECT-303 RECT-304 RECT-322 
&Scoped-Define DISPLAYED-FIELDS Clientes.dv Clientes.Email Clientes.Estado ~
Clientes.Nit Clientes.Tipo_Identificacion Clientes.Nombre ~
Clientes.Apellido1 Clientes.Apellido2 Clientes.Tel_comercial ~
Clientes.Dir_comercial Clientes.Tipo_Vinculo Clientes.Fec_Ingreso 
&Scoped-define DISPLAYED-TABLES Clientes
&Scoped-define FIRST-DISPLAYED-TABLE Clientes
&Scoped-Define DISPLAYED-OBJECTS Cmb_Agencia W_NomComercial 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ing 
     LABEL "INGRESAR" 
     SIZE 13.43 BY 1.62.

DEFINE BUTTON Btn_Lugar 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 1" 
     SIZE 3.57 BY .54.

DEFINE BUTTON Btn_Salir AUTO-END-KEY 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 90" 
     SIZE 11.86 BY 1.62.

DEFINE BUTTON Btn_Salvar 
     LABEL "Salvar" 
     SIZE 13.43 BY 1.62.

DEFINE VARIABLE Cmb_Agencia AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     DROP-DOWN-LIST
     SIZE 51 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE W_LugCcial AS CHARACTER FORMAT "X(80)":U 
     LABEL "Lugar CCial" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81 NO-UNDO.

DEFINE VARIABLE W_NomComercial AS CHARACTER FORMAT "X(50)":U 
     LABEL "Ciudad" 
     VIEW-AS FILL-IN 
     SIZE 41.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-302
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67.57 BY 1.35.

DEFINE RECTANGLE RECT-303
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18.57 BY 3.95.

DEFINE RECTANGLE RECT-304
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71.14 BY 2.42.

DEFINE RECTANGLE RECT-322
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20.72 BY .85.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Nuevos
     Clientes.dv AT ROW 2.54 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 2.72 BY .81
          BGCOLOR 3 
     Clientes.Email AT ROW 11.08 COL 18.43 COLON-ALIGNED WIDGET-ID 2
          LABEL "e-mail (correo electrónico)"
          VIEW-AS FILL-IN 
          SIZE 41.57 BY .81
     Clientes.Estado AT ROW 12.35 COL 33 HELP
          "Estado del cliente dentro del aplicativo" NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 19 BY .46
     W_LugCcial AT ROW 12.27 COL 62.72 COLON-ALIGNED
     Cmb_Agencia AT ROW 1.5 COL 18.14 COLON-ALIGNED
     Clientes.Nit AT ROW 2.54 COL 18.29 COLON-ALIGNED
          LABEL "Cédula/Nit"
          VIEW-AS FILL-IN 
          SIZE 17.43 BY .81
          BGCOLOR 15 
     Clientes.Tipo_Identificacion AT ROW 2.54 COL 62 COLON-ALIGNED
          LABEL "Tipo de Documento"
          VIEW-AS COMBO-BOX 
          LIST-ITEMS "C.C","C.E","T.I","NIT","R.C" 
          DROP-DOWN-LIST
          SIZE 7 BY 1
          BGCOLOR 15 
     Clientes.Tipo_Cliente AT ROW 3.77 COL 7.72 HELP
          "Tipo de persona que corresponde al cliente" NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Natural Mayor de Edad", 1,
"Natural Menor de Edad", 2,
"Jurídica S.A", 3,
"Jurídica C.A", 4
          SIZE 64.86 BY .85
     Clientes.Nombre AT ROW 5.08 COL 18.43 COLON-ALIGNED
          LABEL "Nombres"
          VIEW-AS FILL-IN 
          SIZE 50.43 BY .81
          BGCOLOR 15 
     Clientes.Apellido1 AT ROW 6.04 COL 18.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.14 BY .81
          BGCOLOR 15 
     Clientes.Apellido2 AT ROW 7 COL 18.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.14 BY .81
          BGCOLOR 15 
     Clientes.Tel_comercial AT ROW 8.12 COL 18.43 COLON-ALIGNED HELP
          "Teléfono del lugar comercial del cliente"
          LABEL "Teléfono Comercial" FORMAT "X(20)"
          VIEW-AS FILL-IN 
          SIZE 21.29 BY .81
          BGCOLOR 15 
     Clientes.Dir_comercial AT ROW 9.04 COL 18.43 COLON-ALIGNED HELP
          "Dirección comercial donde labora el cliente"
          LABEL "Dirección Comercial" FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 32.86 BY .81
          BGCOLOR 15 
     W_NomComercial AT ROW 10.15 COL 18.43 COLON-ALIGNED
     Clientes.Tipo_Vinculo AT ROW 6.5 COL 55.43 HELP
          "Identifica el vínculo que tiene con la Organización" NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Asociado", 1,
"Cliente No Asociado", 2,
"Tercero", 3,
"Proveedor", 4,
"Empleado", 5
          SIZE 16.86 BY 3.35
     Btn_Lugar AT ROW 10.15 COL 62.14
     Clientes.Fec_Ingreso AT ROW 12.15 COL 19 COLON-ALIGNED HELP
          "Fecha de ingreso del cliente a la Organización"
          LABEL "Fecha de Ingreso" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 10.57 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Btn_Salvar AT ROW 14.73 COL 33.29
     Btn_Ing AT ROW 14.77 COL 6.57
     Btn_Salir AT ROW 14.77 COL 60.43
     "Tipo de Vínculo" VIEW-AS TEXT
          SIZE 11.57 BY .54 AT ROW 5.96 COL 57.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 75.29 BY 16.04
         FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Nuevos
     RECT-302 AT ROW 3.5 COL 5.72
     RECT-303 AT ROW 6.04 COL 54.57
     RECT-304 AT ROW 14.35 COL 3
     RECT-322 AT ROW 12.15 COL 31.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 75.29 BY 16.04
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Creaciòn Terceros en Clientes, Programa W-Tercero_Nvo.W"
         HEIGHT             = 16.04
         WIDTH              = 75.29
         MAX-HEIGHT         = 21.27
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.27
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Nuevos
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN Clientes.Dir_comercial IN FRAME F_Nuevos
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN Clientes.dv IN FRAME F_Nuevos
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Clientes.Email IN FRAME F_Nuevos
   EXP-LABEL                                                            */
/* SETTINGS FOR RADIO-SET Clientes.Estado IN FRAME F_Nuevos
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN Clientes.Fec_Ingreso IN FRAME F_Nuevos
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN Clientes.Nit IN FRAME F_Nuevos
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Nombre IN FRAME F_Nuevos
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Tel_comercial IN FRAME F_Nuevos
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR RADIO-SET Clientes.Tipo_Cliente IN FRAME F_Nuevos
   NO-DISPLAY EXP-HELP                                                  */
/* SETTINGS FOR COMBO-BOX Clientes.Tipo_Identificacion IN FRAME F_Nuevos
   EXP-LABEL                                                            */
/* SETTINGS FOR RADIO-SET Clientes.Tipo_Vinculo IN FRAME F_Nuevos
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN W_LugCcial IN FRAME F_Nuevos
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN W_NomComercial IN FRAME F_Nuevos
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Nuevos
/* Query rebuild information for FRAME F_Nuevos
     _Query            is NOT OPENED
*/  /* FRAME F_Nuevos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Creaciòn Terceros en Clientes, Programa W-Tercero_Nvo.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Creaciòn Terceros en Clientes, Programa W-Tercero_Nvo.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Apellido1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Apellido1 C-Win
ON LEAVE OF Clientes.Apellido1 IN FRAME F_Nuevos /* Primer Apellido */
DO:
  ASSIGN SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Apellido2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Apellido2 C-Win
ON LEAVE OF Clientes.Apellido2 IN FRAME F_Nuevos /* Segundo Apellido */
DO:
  ASSIGN SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ing
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ing C-Win
ON CHOOSE OF Btn_Ing IN FRAME F_Nuevos /* INGRESAR */
DO:
  RELEASE Clientes.

  ASSIGN Clientes.Nit:SCREEN-VALUE                 = ""
         Clientes.Tipo_Identificacion:SCREEN-VALUE = "C.C"
         Clientes.Nombre:SCREEN-VALUE              = "" 
         Clientes.Tipo_Vinculo:SCREEN-VALUE        = "3"
         Clientes.Tipo_Cliente:SCREEN-VALUE        = "1"
         Clientes.Apellido1:SCREEN-VALUE           = ""
         Clientes.Apellido2:SCREEN-VALUE           = ""
         Clientes.Tel_comercial:SCREEN-VALUE       = ""
         Clientes.Dir_comercial:SCREEN-VALUE       = ""
         Clientes.Fec_Ingreso:SCREEN-VALUE         = STRING(W_Fecha)
         Clientes.Estado:SCREEN-VALUE              = "1"
         W_LugCcial                                = ""
         clientes.email:SCREEN-VALUE = ""
         clientes.dv:screen-VALUE = "". 

  ENABLE Clientes.Apellido1 Clientes.Apellido2 WITH FRAME F_Nuevos.  
    
  APPLY "ENTRY" TO Clientes.Nit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Lugar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Lugar C-Win
ON CHOOSE OF Btn_Lugar IN FRAME F_Nuevos /* Button 1 */
DO:
    ASSIGN C-Win:SENSITIVE = FALSE.
    RUN C-Ubicacion.R (OUTPUT P_Ubi,OUTPUT P_NUbi).
        
    ASSIGN C-Win:SENSITIVE                        = TRUE
           W_NomComercial:SCREEN-VALUE            = LC(P_NUbi)
           W_LugCcial                             = P_Ubi.  
        
   C-Win:MOVE-TO-TOP().     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir C-Win
ON CHOOSE OF Btn_Salir IN FRAME F_Nuevos /* Button 90 */
DO:
  IF AVAIL(Clientes) THEN
     ASSIGN W_NitTer = Clientes.Nit.
  ELSE W_NitTer = "".

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar C-Win
ON CHOOSE OF Btn_Salvar IN FRAME F_Nuevos /* Salvar */
DO:
    DEFINE VAR flagError AS LOGICAL.
    DEFINE VAR flagNuevo AS LOGICAL.

    IF clientes.nit:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Debe ingresar una identificación válida."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    /* Validamos la prioridad del Usuario para hacer creaciones o modificaciones */
    FIND FIRST usuarios WHERE usuarios.usuario = w_usuario NO-LOCK NO-ERROR.
    IF usuarios.prioridad < 5 THEN DO:
        MESSAGE "Su usuario no está autorizado" SKIP
                "para realizar esta operación."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    IF clientes.tipo_cliente:SCREEN-VALUE < "3" THEN DO:
        IF Clientes.Nit:SCREEN-VALUE = "0" OR
           Clientes.Tipo_Identificacion:SCREEN-VALUE LE " " OR
           Clientes.Nombre:SCREEN-VALUE LE " " OR
           clientes.apellido1:SCREEN-VALUE = "" OR
           Clientes.Tipo_Vinculo:SCREEN-VALUE LE "0" OR
           Clientes.Tipo_Cliente:SCREEN-VALUE LE "0" OR
           Clientes.Tel_comercial:SCREEN-VALUE LE "0" OR
           clientes.DIR_comercial:SCREEN-VALUE = "" OR
           clientes.email:SCREEN-VALUE = "" THEN DO:
            MESSAGE "Los siguientes datos son obligatorios:" skip
                    "Cédula/Nit" skip
                    "Tipo de identificación" skip
                    "Nombre" SKIP
                    "Primer Apellido" SKIP
                    "Teléfono" SKIP
                    "Dirección" SKIP
                    "Correo electrónico" SKIP(2)
                    "Por favor, revise y complete la información..."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN.
        END.
    END.
    ELSE DO:
        IF Clientes.Nit:SCREEN-VALUE = "0" OR
           Clientes.Tipo_Identificacion:SCREEN-VALUE LE " " OR
           Clientes.Nombre:SCREEN-VALUE LE " " OR
           Clientes.Tipo_Vinculo:SCREEN-VALUE LE "0" OR
           Clientes.Tipo_Cliente:SCREEN-VALUE LE "0" OR
           Clientes.Tel_comercial:SCREEN-VALUE LE "0" OR
           clientes.DIR_comercial:SCREEN-VALUE = "" OR
           clientes.email:SCREEN-VALUE = "" THEN DO:
            MESSAGE "Los siguientes datos son obligatorios:" skip
                    "Cédula/Nit" skip
                    "Tipo de identificación" skip
                    "Nombre" SKIP
                    "Teléfono" SKIP
                    "Dirección" SKIP
                    "Correo electrónico" SKIP(2)
                    "Por favor, revise y complete la información..."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN.
        END.
    END.

    /* Validar integridad del documento ID */
    RUN validaNIT.r(INPUT clientes.nit:SCREEN-VALUE,
                    INPUT clientes.tipo_identificacion:SCREEN-VALUE,
                    OUTPUT flagError).

    IF flagError = TRUE THEN DO:
        MESSAGE "El documento ingresado no es correcto de" SKIP
                "acuerdo al tipo de identificación seleccionado." SKIP
                "Revise por favor..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    IF AVAIL(Clientes) THEN
        FIND CURRENT Clientes NO-ERROR.
    ELSE DO:
        CREATE Clientes.
        ASSIGN Clientes.Nit.

        flagNuevo = TRUE.
    END.

    ASSIGN Clientes.Agencia = INTEG(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3))
           Clientes.Lugar_Comercial = W_LugCcial
           Clientes.Usuario = W_Usuario
           Clientes.Tipo_Identificacion
           Clientes.Nombre
           Clientes.Tipo_Vinculo
           Clientes.Apellido1
           Clientes.Apellido2
           Clientes.Tel_comercial
           Clientes.Dir_comercial
           Clientes.Fec_Ingreso
           Clientes.Estado
           Clientes.Tipo_Cliente
           clientes.email
           clientes.dv.

    FIND CURRENT Clientes NO-LOCK NO-ERROR.

    IF flagNuevo = TRUE THEN
        MESSAGE "Se realizó la creación del Tercero" SKIP
                "de forma exitosa."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE
        MESSAGE "Se realizó la modificación del Tercero" SKIP
                "de forma exitosa."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Agencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencia C-Win
ON VALUE-CHANGED OF Cmb_Agencia IN FRAME F_Nuevos /* Agencia */
DO:
   ASSIGN W_OfiUsu = INTEGER(SUBSTRING(Cmb_Agencia,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Email
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Email C-Win
ON LEAVE OF Clientes.Email IN FRAME F_Nuevos /* e-mail (correo electrónico) */
DO:
    IF INDEX(clientes.email:SCREEN-VALUE,"@") = 0 THEN DO:
        MESSAGE "El formato para el correo electrónico" SKIP
                "es inválido. Revise por favor..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        clientes.email:SCREEN-VALUE = "".

        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Nit C-Win
ON LEAVE OF Clientes.Nit IN FRAME F_Nuevos /* Cédula/Nit */
DO:
    DEFINE VAR digitoVerificacion AS INTEGER.

    FIND FIRST Clientes WHERE Clientes.Nit EQ Clientes.Nit:SCREEN-VALUE AND Clientes.Nit:SCREEN-VALUE <> "" NO-LOCK NO-ERROR.
    IF AVAIL(Clientes) THEN DO:
        MESSAGE "La Cédula/Nit ya existe...Revise la información en la ventana."
            VIEW-AS ALERT-BOX.

        DISPLAY Clientes.Nit
                Clientes.Tipo_Identificacion
                Clientes.Nombre
                Clientes.Tipo_Vinculo
                Clientes.Apellido1
                Clientes.Apellido2
                Clientes.Tel_comercial
                Clientes.Dir_comercial
                Clientes.Fec_Ingreso
                Clientes.Estado
                clientes.tipo_cliente
                clientes.email
                clientes.dv
            WITH FRAME F_Nuevos.

        FIND FIRST Agencias WHERE Agencias.Agencia EQ Clientes.Agencia NO-LOCK NO-ERROR.

        ASSIGN Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia, "999") + "-" + TRIM(Agencias.Nombre) WHEN AVAIL(Agencias).

        FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "B" AND Ubicacion.Ubicacion EQ Clientes.Lugar_Comercial NO-LOCK NO-ERROR.
        IF AVAILABLE (Ubicacion) THEN
            W_NUbicacion = Ubicacion.Nombre.

        FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Comercial,1,5) NO-LOCK NO-ERROR.
        IF AVAILABLE (Ubicacion) THEN
            W_NUbicacion = W_NUbicacion + " - " + Ubicacion.Nombre.

        FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "D" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Comercial,1,2) NO-LOCK NO-ERROR.
        IF AVAILABLE(Ubicacion) THEN
            W_NUbicacion = W_NUbicacion + " - " + Ubicacion.Nombre.

        ASSIGN W_NomComercial:SCREEN-VALUE = LC(W_NUbicacion)
               W_LugCcial = Clientes.Lugar_Comercial.

        IF Clientes.Tipo_Cliente GT 2 THEN
            DISABLE Clientes.Apellido1
                    Clientes.Apellido2
                WITH FRAME F_Nuevos.
        ELSE
            ENABLE Clientes.Apellido1
                   Clientes.Apellido2
                WITH FRAME F_Nuevos.
    END.

    IF clientes.tipo_identificacion:SCREEN-VALUE = "NIT" THEN DO:
        RUN digitoVerificacion.r(INPUT clientes.nit:SCREEN-VALUE,
                                 OUTPUT digitoVerificacion).

        clientes.dv:SCREEN-VALUE = STRING(digitoVerificacion).
    END.
    ELSE
        clientes.dv:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Nombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Nombre C-Win
ON LEAVE OF Clientes.Nombre IN FRAME F_Nuevos /* Nombres */
DO:
  ASSIGN SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Tel_comercial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tel_comercial C-Win
ON LEAVE OF Clientes.Tel_comercial IN FRAME F_Nuevos /* Teléfono Comercial */
DO:
  DEFI VAR Tel_Nume AS DEC FORM "999999999999".
  ASSIGN Tel_Nume = DEC(Clientes.Tel_Comercial:SCREEN-VALUE) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     MESSAGE "Solo Numèrico...Corrija por favor."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY "ENTRY" TO SELF.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Tipo_Cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tipo_Cliente C-Win
ON VALUE-CHANGED OF Clientes.Tipo_Cliente IN FRAME F_Nuevos /* Tipo de Cliente */
DO:
    ENABLE Clientes.Apellido1
           Clientes.Apellido2
        WITH FRAME F_Nuevos.

    IF Clientes.Tipo_Cliente:SCREEN-VALUE GT "2" THEN
        DISABLE Clientes.Apellido1
                Clientes.Apellido2
        WITH FRAME F_Nuevos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Tipo_Identificacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tipo_Identificacion C-Win
ON VALUE-CHANGED OF Clientes.Tipo_Identificacion IN FRAME F_Nuevos /* Tipo de Documento */
DO:
    DEFINE VAR digitoVerificacion AS INTEGER.

    IF clientes.tipo_identificacion:SCREEN-VALUE = "NIT" THEN DO:
        RUN digitoVerificacion.r(INPUT clientes.nit:SCREEN-VALUE,
                                 OUTPUT digitoVerificacion).

        clientes.dv:SCREEN-VALUE = STRING(digitoVerificacion).
    END.
    ELSE
        clientes.dv:SCREEN-VALUE = "".

    IF clientes.tipo_identificacion:SCREEN-VALUE <> "NIT" THEN
        ENABLE Clientes.Apellido1
               Clientes.Apellido2.
    ELSE
        DISABLE Clientes.Apellido1
                Clientes.Apellido2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Tipo_Vinculo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tipo_Vinculo C-Win
ON VALUE-CHANGED OF Clientes.Tipo_Vinculo IN FRAME F_Nuevos /* codigo Vinculo */
DO:
  IF Clientes.Tipo_Vinculo:SCREEN-VALUE LE "2" THEN DO:
     MESSAGE "Solo Proveedores y Terceros..."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     Clientes.Tipo_Vinculo:SCREEN-VALUE = "4".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  ASSIGN Cmb_Agencia:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
  
  FOR EACH Agencias WHERE Agencias.Estado NE 3 NO-LOCK
                       BY Agencia.Agencia:
      Cmb_Agencia:ADD-LAST(STRING(Agencias.Agencia, "999") + "-" + TRIM(Agencias.Nombre)).
      IF Agencias.Agencia EQ W_Agencia THEN
         Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia, "999") + "-" + TRIM(Agencias.Nombre).
  END.

  clientes.dv:SENSITIVE = FALSE.

  ON RETURN TAB.
  

  APPLY "Choose" TO Btn_Ing.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY Cmb_Agencia W_NomComercial 
      WITH FRAME F_Nuevos IN WINDOW C-Win.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.dv Clientes.Email Clientes.Estado Clientes.Nit 
          Clientes.Tipo_Identificacion Clientes.Nombre Clientes.Apellido1 
          Clientes.Apellido2 Clientes.Tel_comercial Clientes.Dir_comercial 
          Clientes.Tipo_Vinculo Clientes.Fec_Ingreso 
      WITH FRAME F_Nuevos IN WINDOW C-Win.
  ENABLE Clientes.Email Clientes.Estado Cmb_Agencia Clientes.Nit 
         Clientes.Tipo_Identificacion Clientes.Tipo_Cliente Clientes.Nombre 
         Clientes.Apellido1 Clientes.Apellido2 Clientes.Tel_comercial 
         Clientes.Dir_comercial Clientes.Tipo_Vinculo Btn_Lugar Btn_Salvar 
         Btn_Ing Btn_Salir RECT-302 RECT-303 RECT-304 RECT-322 
      WITH FRAME F_Nuevos IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Nuevos}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

