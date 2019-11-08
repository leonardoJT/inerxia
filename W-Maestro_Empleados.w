&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

{src/adm2/widgetprto.i}

{Incluido\variable.i "shared"}

DEFINE VAR WRowid AS ROWID.
DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR gTexto AS CHARACTER FORMAT "X(50)".
DEFINE VAR W_NvaNota AS LOGICAL INITIAL NO.
DEFINE VAR WNomEmp AS CHARACTER FORMAT "X(35)".
DEFINE VAR WNomDev AS CHARACTER FORMAT "X(35)".

/* oakley */

DEFINE VAR WDiasT  AS INTEGER FORMAT "999".
DEFINE VAR WBasico LIKE Empleados.Salario_Mensual.
DEFINE VAR WTipo AS INTEGER FORMAT "9".
DEFINE VAR WVAnt LIKE Cfg_Novedades.Valor.
DEFINE VAR WNvoCliente AS LOGICAL INITIAL NO.
DEFINE VAR FE1 LIKE Empleados.Estado.
DEFINE VAR FE2 LIKE Empleados.Estado.
DEFINE VAR WTipInfo AS INTEGER FORMAT "9".

DEFINE TEMP-TABLE TConceptos
    FIELD Tip    LIKE Cfg_Novedades.Tipo
    FIELD Cod    LIKE Cfg_Novedades.Codigo
    FIELD Nom    LIKE Cfg_Novedades.Nombre.

  /*tabla temporal para las relaciones del cliente en pantalla*/
  DEFINE TEMP-TABLE T_Relaciones
     FIELD R_Relacion   LIKE Varios.Descripcion
     FIELD R_AgeObjeto  LIKE Clientes.Agencia
     FIELD R_NitObjeto  LIKE Clientes.Nit
     FIELD R_NomObjeto  AS CHARACTER FORMAT "X(35)"
     FIELD R_TelObjeto  AS CHARACTER FORMAT "X(30)"
     FIELD R_NomDescri  AS CHARACTER FORMAT "X(15)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FMain
&Scoped-define BROWSE-NAME BConceptos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TConceptos Novedades_Nit Empleados ~
HVEmpleados T_Relaciones Clientes

/* Definitions for BROWSE BConceptos                                    */
&Scoped-define FIELDS-IN-QUERY-BConceptos TConceptos.Cod TConceptos.Nom   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BConceptos   
&Scoped-define SELF-NAME BConceptos
&Scoped-define QUERY-STRING-BConceptos FOR EACH TConceptos
&Scoped-define OPEN-QUERY-BConceptos OPEN QUERY {&SELF-NAME} FOR EACH TConceptos.
&Scoped-define TABLES-IN-QUERY-BConceptos TConceptos
&Scoped-define FIRST-TABLE-IN-QUERY-BConceptos TConceptos


/* Definitions for BROWSE BDeducciones                                  */
&Scoped-define FIELDS-IN-QUERY-BDeducciones Novedades_Nit.Codigo WNomDev   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BDeducciones   
&Scoped-define SELF-NAME BDeducciones
&Scoped-define QUERY-STRING-BDeducciones FOR EACH Novedades_Nit WHERE Novedades_Nit.Agencia EQ Empleados.Agencia AND                                                      Novedades_Nit.Nit     EQ Empleados.Nit AND                                                      Novedades_Nit.Tipo    EQ 2
&Scoped-define OPEN-QUERY-BDeducciones OPEN QUERY {&SELF-NAME} FOR EACH Novedades_Nit WHERE Novedades_Nit.Agencia EQ Empleados.Agencia AND                                                      Novedades_Nit.Nit     EQ Empleados.Nit AND                                                      Novedades_Nit.Tipo    EQ 2.
&Scoped-define TABLES-IN-QUERY-BDeducciones Novedades_Nit
&Scoped-define FIRST-TABLE-IN-QUERY-BDeducciones Novedades_Nit


/* Definitions for BROWSE BDevengados                                   */
&Scoped-define FIELDS-IN-QUERY-BDevengados Novedades_Nit.Codigo WNomDev   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BDevengados   
&Scoped-define SELF-NAME BDevengados
&Scoped-define QUERY-STRING-BDevengados FOR EACH Novedades_Nit WHERE Novedades_Nit.Agencia EQ Empleados.Agencia AND                                                      Novedades_Nit.Nit     EQ Empleados.Nit AND                                                      Novedades_Nit.Tipo    EQ 1
&Scoped-define OPEN-QUERY-BDevengados OPEN QUERY {&SELF-NAME} FOR EACH Novedades_Nit WHERE Novedades_Nit.Agencia EQ Empleados.Agencia AND                                                      Novedades_Nit.Nit     EQ Empleados.Nit AND                                                      Novedades_Nit.Tipo    EQ 1.
&Scoped-define TABLES-IN-QUERY-BDevengados Novedades_Nit
&Scoped-define FIRST-TABLE-IN-QUERY-BDevengados Novedades_Nit


/* Definitions for BROWSE BEmpleados                                    */
&Scoped-define FIELDS-IN-QUERY-BEmpleados Empleados.Agencia Empleados.Nit WNomEmp Empleados.Cargo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BEmpleados   
&Scoped-define SELF-NAME BEmpleados
&Scoped-define QUERY-STRING-BEmpleados FOR EACH Empleados WHERE Empleados.Estado EQ 1
&Scoped-define OPEN-QUERY-BEmpleados OPEN QUERY BEmpleados FOR EACH Empleados WHERE Empleados.Estado EQ 1.
&Scoped-define TABLES-IN-QUERY-BEmpleados Empleados
&Scoped-define FIRST-TABLE-IN-QUERY-BEmpleados Empleados


/* Definitions for BROWSE BInformativo                                  */
&Scoped-define FIELDS-IN-QUERY-BInformativo HVEmpleados.Agencia HVEmpleados.Codigo HVEmpleados.Descripcion HVEmpleados.FecIni   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BInformativo   
&Scoped-define SELF-NAME BInformativo
&Scoped-define QUERY-STRING-BInformativo FOR EACH HVEmpleados WHERE HvEmpleados.Nit EQ Empleados.Nit
&Scoped-define OPEN-QUERY-BInformativo OPEN QUERY {&SELF-NAME} FOR EACH HVEmpleados WHERE HvEmpleados.Nit EQ Empleados.Nit.
&Scoped-define TABLES-IN-QUERY-BInformativo HVEmpleados
&Scoped-define FIRST-TABLE-IN-QUERY-BInformativo HVEmpleados


/* Definitions for BROWSE Br_Relaciones                                 */
&Scoped-define FIELDS-IN-QUERY-Br_Relaciones T_Relaciones.R_Relacion T_Relaciones.R_AgeObjeto T_Relaciones.R_NitObjeto T_Relaciones.R_NomObjeto T_Relaciones.R_NomDescri T_Relaciones.R_TelObjeto   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Relaciones   
&Scoped-define SELF-NAME Br_Relaciones
&Scoped-define QUERY-STRING-Br_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Relaciones OPEN QUERY {&SELF-NAME} FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Relaciones T_Relaciones
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Relaciones T_Relaciones


/* Definitions for FRAME FConceptos                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FConceptos ~
    ~{&OPEN-QUERY-BConceptos}

/* Definitions for FRAME FConsulta                                      */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FConsulta ~
    ~{&OPEN-QUERY-BEmpleados}

/* Definitions for FRAME FHV                                            */
&Scoped-define FIELDS-IN-QUERY-FHV Clientes.Dir_Residencia ~
Clientes.Tel_Residencia Clientes.Estrato Clientes.Celular Clientes.Email ~
Clientes.Niv_Educativo Clientes.Est_Civil Clientes.Fec_Nacimiento 
&Scoped-define ENABLED-FIELDS-IN-QUERY-FHV Clientes.Dir_Residencia ~
Clientes.Tel_Residencia Clientes.Estrato Clientes.Celular Clientes.Email ~
Clientes.Niv_Educativo Clientes.Est_Civil Clientes.Fec_Nacimiento 
&Scoped-define ENABLED-TABLES-IN-QUERY-FHV Clientes
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-FHV Clientes
&Scoped-define OPEN-BROWSERS-IN-QUERY-FHV ~
    ~{&OPEN-QUERY-BInformativo}
&Scoped-define QUERY-STRING-FHV FOR EACH Clientes SHARE-LOCK
&Scoped-define OPEN-QUERY-FHV OPEN QUERY FHV FOR EACH Clientes SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-FHV Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-FHV Clientes


/* Definitions for FRAME FHV_Detalle                                    */
&Scoped-define FIELDS-IN-QUERY-FHV_Detalle HVEmpleados.FecInicial ~
HVEmpleados.Descripcion HVEmpleados.FecFinal HVEmpleados.Valor 
&Scoped-define ENABLED-FIELDS-IN-QUERY-FHV_Detalle HVEmpleados.FecInicial ~
HVEmpleados.Descripcion HVEmpleados.FecFinal HVEmpleados.Valor 
&Scoped-define ENABLED-TABLES-IN-QUERY-FHV_Detalle HVEmpleados
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-FHV_Detalle HVEmpleados
&Scoped-define QUERY-STRING-FHV_Detalle FOR EACH HVEmpleados SHARE-LOCK
&Scoped-define OPEN-QUERY-FHV_Detalle OPEN QUERY FHV_Detalle FOR EACH HVEmpleados SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-FHV_Detalle HVEmpleados
&Scoped-define FIRST-TABLE-IN-QUERY-FHV_Detalle HVEmpleados


/* Definitions for FRAME FMain                                          */
&Scoped-define FIELDS-IN-QUERY-FMain Empleados.Fec_Ingreso ~
Empleados.Fec_Retiro Empleados.Nit 
&Scoped-define ENABLED-FIELDS-IN-QUERY-FMain Empleados.Fec_Ingreso ~
Empleados.Nit 
&Scoped-define ENABLED-TABLES-IN-QUERY-FMain Empleados
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-FMain Empleados
&Scoped-define QUERY-STRING-FMain FOR EACH Empleados SHARE-LOCK, ~
      EACH Clientes OF Empleados SHARE-LOCK
&Scoped-define OPEN-QUERY-FMain OPEN QUERY FMain FOR EACH Empleados SHARE-LOCK, ~
      EACH Clientes OF Empleados SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-FMain Empleados Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-FMain Empleados
&Scoped-define SECOND-TABLE-IN-QUERY-FMain Clientes


/* Definitions for FRAME FNovedades                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FNovedades ~
    ~{&OPEN-QUERY-BDeducciones}~
    ~{&OPEN-QUERY-BDevengados}

/* Definitions for FRAME F_Relaciones                                   */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Empleados.Fec_Ingreso Empleados.Nit 
&Scoped-define ENABLED-TABLES Empleados
&Scoped-define FIRST-ENABLED-TABLE Empleados
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 FOTO C_Agencia Btn_Informacion ~
Btn_informe BUTTON-4 BtnHV BUTTON-18 Btn_SS BUTTON-17 BUTTON-15 BUTTON-201 ~
Btn_Salvar Btn_Deshacer Btn_Ingresar Btn_Borrar Btn_Cancelar Btn_Salir ~
BUTTON-11 
&Scoped-Define DISPLAYED-FIELDS Empleados.Fec_Ingreso Empleados.Fec_Retiro ~
Empleados.Nit 
&Scoped-define DISPLAYED-TABLES Empleados
&Scoped-define FIRST-DISPLAYED-TABLE Empleados
&Scoped-Define DISPLAYED-OBJECTS C_Agencia C_Nombre C_Apellido1 C_Apellido2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Btn_Informacion Btn_informe BUTTON-4 Btn_Salvar ~
Btn_Deshacer Btn_Ingresar Btn_Borrar Btn_Cancelar Btn_Salir BUTTON-11 
&Scoped-define List-5 BUTTON-18 Btn_SS BUTTON-17 BUTTON-15 BUTTON-201 
&Scoped-define List-6 C_Nombre C_Apellido1 C_Apellido2 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BDeducciones 
       MENU-ITEM m_Adicionar_Deduccion LABEL "Adicionar Deduccion"
       MENU-ITEM m_Eliminiar_Deduccion LABEL "Eliminiar Deduccion".

DEFINE MENU POPUP-MENU-BDevengados 
       MENU-ITEM m_Adicionar_Devengado LABEL "Adicionar Devengado"
       MENU-ITEM m_Eliminar_Devengado LABEL "Eliminar Devengado".

DEFINE MENU POPUP-MENU-BInformativo 
       MENU-ITEM m_Ver_Detalle_de_la_Anotacin LABEL "Ver Detalle de la Anotación"
       MENU-ITEM m_Imprimir_Hoja_de_Vida LABEL "Imprimir Hoja de Vida"
       MENU-ITEM m_Eliminar_Concepto LABEL "Eliminar Concepto".


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-37 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-38 
     LABEL "Adicionar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-21 
     LABEL "Ocultar" 
     SIZE 15 BY 3.23.

DEFINE VARIABLE Cmb_Estado AS CHARACTER FORMAT "X(256)":U INITIAL "00 - Todos" 
     LABEL "Estado" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00 - Todos","01 - Activo","02 - Retirado","03 - Lic. Maternidad","04 - Lic. Remunerada","05 - Lic. No Remunerada","06 - Vacaciones","07 - Comisión","08 - Calamidad Doméstica","09 - Incapacidad","10 - Suspendido" 
     DROP-DOWN-LIST
     SIZE 43 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WCon AS CHARACTER FORMAT "X(25)":U 
     LABEL "Consulta" 
     VIEW-AS FILL-IN 
     SIZE 43 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RCon AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todos", 0,
"Agencia", 1,
"Nit", 2
     SIZE 34 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-305
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 3.77.

DEFINE BUTTON BUTTON-20 
     LABEL "Nueva Dotación" 
     SIZE 16 BY 1.12.

DEFINE BUTTON BUTTON-22 
     LABEL "Activar" 
     SIZE 11 BY 1.12.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 6.19.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 6.19.

DEFINE VARIABLE C_Perfil AS CHARACTER FORMAT "X(256)":U INITIAL "000 - Perfil No Escogido" 
     LABEL "Perfil del Cargo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 - Perfil No Escogido" 
     DROP-DOWN-LIST
     SIZE 44 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 11.85
     BGCOLOR 17 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34 BY 2.15.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 23 BY 1.88.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34 BY 1.62.

DEFINE BUTTON BtnNvoInformativo 
     LABEL "Agregar Nuevo" 
     SIZE 19 BY .85.

DEFINE BUTTON BUTTON-190 
     LABEL "Ver información completa" 
     SIZE 20 BY .81.

DEFINE VARIABLE CInformativos AS CHARACTER FORMAT "X(256)":U INITIAL "00000 - Todos los conceptos" 
     LABEL "Conceptos Informativos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00000 - Todos los Conceptos" 
     DROP-DOWN-LIST
     SIZE 51 BY 1
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE Cumplidos AS DECIMAL FORMAT "999":U INITIAL 0 
     LABEL "Cumplidos" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RTV AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Propia", 1,
"Arrendada", 2
     SIZE 21 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 3.5.

DEFINE RECTANGLE RECT-303
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 1.62
     BGCOLOR 18 .

DEFINE BUTTON BUTTON-196 
     LABEL "Guardar Anotación" 
     SIZE 22 BY .96.

DEFINE BUTTON BUTTON-197 
     LABEL "Ocultar" 
     SIZE 22 BY .96.

DEFINE BUTTON BUTTON-199 
     LABEL "Imprimir" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-200 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE CInformativosI AS CHARACTER FORMAT "X(256)":U INITIAL "00000 - Todos los conceptos" 
     LABEL "Conceptos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00000 - Todos los Conceptos" 
     DROP-DOWN-LIST
     SIZE 51 BY 1
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE Actual AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 63 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WFecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WFecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RIHV AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Todos los Conceptos", 1,
"Un Concepto en especifico", 2
     SIZE 23 BY 1.88 NO-UNDO.

DEFINE VARIABLE RIHVEmpleado AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Empleado Actual", 1,
"Todos los Empleados", 2
     SIZE 50 BY .81
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-302
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 65 BY 1.62.

DEFINE BUTTON BtnHV 
     LABEL "Hoja de Vida" 
     SIZE 15 BY 1.12.

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

DEFINE BUTTON Btn_informe 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 3" 
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

DEFINE BUTTON Btn_SS 
     LABEL "Seguridad Social" 
     SIZE 17 BY 1.12.

DEFINE BUTTON BUTTON-11 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 11" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BUTTON-15 
     LABEL "Novedades Fijas Nomina" 
     SIZE 19 BY 1.12.

DEFINE BUTTON BUTTON-17 
     LABEL "Dotación" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-18 
     LABEL "General" 
     SIZE 11 BY 1.12.

DEFINE BUTTON BUTTON-201 
     LABEL "Relaciones" 
     SIZE 18 BY 1.12.

DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 4" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE C_Agencia AS CHARACTER FORMAT "X(256)":U INITIAL "000 - Agencia no escogida" 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 - Agencia no escogida" 
     DROP-DOWN-LIST
     SIZE 32 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE C_Apellido1 AS CHARACTER FORMAT "X(15)" 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .81
     BGCOLOR 15 .

DEFINE VARIABLE C_Apellido2 AS CHARACTER FORMAT "X(15)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 15 .

DEFINE VARIABLE C_Nombre AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .81
     BGCOLOR 15 .

DEFINE IMAGE FOTO
     FILENAME "IMAGENES/FOTOS/0.jpg":U
     SIZE 19 BY 4.58.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 13 BY 5.38.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 13 BY 10.23.

DEFINE BUTTON BUTTON-189 
     LABEL "Asignar Novedades Fijas" 
     SIZE 32 BY 1.12.

DEFINE BUTTON Btn_SC 
     LABEL "SC" 
     SIZE 15 BY .81.

DEFINE BUTTON BUTTON-205 
     LABEL "Grabar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-206 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE R_Apellido1 AS CHARACTER FORMAT "X(15)" 
     LABEL "Primer Apellido" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 15 .

DEFINE VARIABLE R_Apellido2 AS CHARACTER FORMAT "X(15)" 
     LABEL "Segundo Apellido" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 15 .

DEFINE VARIABLE R_Nit AS CHARACTER FORMAT "X(12)" 
     LABEL "Nit" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 15 .

DEFINE VARIABLE R_Nombre AS CHARACTER FORMAT "X(40)" 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 15 .

DEFINE VARIABLE R_Relacion AS CHARACTER FORMAT "X(50)":U 
     LABEL "Relacion o Parentesco" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Tel_Residencia AS CHARACTER FORMAT "X(20)" 
     LABEL "Teléfono de la Residencia" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 15 .

DEFINE BUTTON BUTTON-207 
     LABEL "Aceptar" 
     SIZE 15 BY 1.12
     FONT 4.

DEFINE VARIABLE RCiao AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Renuncia Voluntaria", 502,
"Despido", 503,
"Cumplimiento del Contrato", 504
     SIZE 24 BY 2.42
     FONT 4 NO-UNDO.

DEFINE VARIABLE C_Cesantias AS CHARACTER FORMAT "X(256)":U INITIAL "00000 - Fondo Cesantias No Seleccionado" 
     LABEL "Fondo de Cesantias" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00000 - Fondo Cesantias No Seleccionado" 
     DROP-DOWN-LIST
     SIZE 30 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE C_Eps AS CHARACTER FORMAT "X(256)":U INITIAL "00000 - EPS No Seleccionada" 
     LABEL "EPS" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00000 - EPS No Seleccionada" 
     DROP-DOWN-LIST
     SIZE 30 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE C_Pensiones AS CHARACTER FORMAT "X(256)":U INITIAL "00000 - Fondo Pensiones No Seleccionado" 
     LABEL "Fondo de Pensiones" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00000 - Fondo Pensiones No Seleccionado" 
     DROP-DOWN-LIST
     SIZE 30 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72 BY 4.31.

DEFINE BUTTON Btn_Activas 
     LABEL "Borrar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_CreRel 
     LABEL "Crear" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_SalRel 
     LABEL "Salvar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE Cmb_Relaciones AS CHARACTER FORMAT "X(50)":U 
     LABEL "Consulta Relaciones" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 49 BY 1
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE W_MenRel AS CHARACTER FORMAT "X(256)":U INITIAL "Escoja del Combo de Relaciones, el tipo de relación que quiere consultar" 
     VIEW-AS FILL-IN 
     SIZE 91 BY .81
     FGCOLOR 7 FONT 5 NO-UNDO.

DEFINE VARIABLE RActivas AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activas", 1,
"Borradas", 2
     SIZE 19 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BConceptos FOR 
      TConceptos SCROLLING.

DEFINE QUERY BDeducciones FOR 
      Novedades_Nit SCROLLING.

DEFINE QUERY BDevengados FOR 
      Novedades_Nit SCROLLING.

DEFINE QUERY BEmpleados FOR 
      Empleados SCROLLING.

DEFINE QUERY BInformativo FOR 
      HVEmpleados SCROLLING.

DEFINE QUERY Br_Relaciones FOR 
      T_Relaciones SCROLLING.

DEFINE QUERY FHV FOR 
      Clientes SCROLLING.

DEFINE QUERY FHV_Detalle FOR 
      HVEmpleados SCROLLING.

DEFINE QUERY FMain FOR 
      Empleados, 
      Clientes SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BConceptos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BConceptos wWin _FREEFORM
  QUERY BConceptos DISPLAY
      TConceptos.Cod
 TConceptos.Nom WIDTH 25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 44 BY 9.96
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE BDeducciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BDeducciones wWin _FREEFORM
  QUERY BDeducciones DISPLAY
      Novedades_Nit.Codigo
WNomDev COLUMN-LABEL "Nombre"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 45 BY 9.42
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE BDevengados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BDevengados wWin _FREEFORM
  QUERY BDevengados DISPLAY
      Novedades_Nit.Codigo
WNomDev COLUMN-LABEL "Nombre"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 45 BY 9.42
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE BEmpleados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BEmpleados wWin _FREEFORM
  QUERY BEmpleados DISPLAY
      Empleados.Agencia COLUMN-LABEL "Age" 
 Empleados.Nit
 WNomEmp COLUMN-LABEL "Nombre Empleado"
 Empleados.Cargo
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 74 BY 6.73
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE BInformativo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BInformativo wWin _FREEFORM
  QUERY BInformativo DISPLAY
      HVEmpleados.Agencia
  HVEmpleados.Codigo
  HVEmpleados.Descripcion WIDTH 65
  HVEmpleados.FecIni
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 91 BY 4.85
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.

DEFINE BROWSE Br_Relaciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Relaciones wWin _FREEFORM
  QUERY Br_Relaciones NO-LOCK DISPLAY
      T_Relaciones.R_Relacion  FORMAT "X(22)" LABEL "Relacion"
      T_Relaciones.R_AgeObjeto FORMAT "999"   LABEL "Age"
      T_Relaciones.R_NitObjeto FORMAT "X(14)" LABEL "Nit"
      T_Relaciones.R_NomObjeto FORMAT "X(35)" LABEL "Nombre"
      T_Relaciones.R_NomDescri FORMAT "X(15)" LABEL "Descripción"
      T_Relaciones.R_TelObjeto FORMAT "X(30)" LABEL "Tel.Comercial"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 91 BY 9.42
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FMain
     Empleados.Fec_Ingreso AT ROW 1.27 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 FGCOLOR 0 
     C_Agencia AT ROW 1.81 COL 30 COLON-ALIGNED
     Btn_Informacion AT ROW 1.81 COL 100
     Empleados.Fec_Retiro AT ROW 2.08 COL 83 COLON-ALIGNED
          LABEL "Fecha Retiro"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Btn_informe AT ROW 3.42 COL 100
     Empleados.Nit AT ROW 3.96 COL 30 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     C_Nombre AT ROW 3.96 COL 43 COLON-ALIGNED NO-LABEL
     C_Apellido1 AT ROW 3.96 COL 62 COLON-ALIGNED NO-LABEL
     C_Apellido2 AT ROW 3.96 COL 79 COLON-ALIGNED NO-LABEL
     BUTTON-4 AT ROW 5.04 COL 100
     BtnHV AT ROW 6.12 COL 3
     BUTTON-18 AT ROW 6.12 COL 18
     Btn_SS AT ROW 6.12 COL 29
     BUTTON-17 AT ROW 6.12 COL 46
     BUTTON-15 AT ROW 6.12 COL 61
     BUTTON-201 AT ROW 6.12 COL 80
     Btn_Salvar AT ROW 10.42 COL 100
     Btn_Deshacer AT ROW 12.04 COL 100
     Btn_Ingresar AT ROW 13.65 COL 100
     Btn_Borrar AT ROW 15.27 COL 100
     Btn_Cancelar AT ROW 16.88 COL 100
     Btn_Salir AT ROW 18.5 COL 100
     BUTTON-11 AT ROW 20.65 COL 104
     "Segundo Apellido" VIEW-AS TEXT
          SIZE 17 BY .77 AT ROW 3.15 COL 81
          FGCOLOR 7 
     "Nombre" VIEW-AS TEXT
          SIZE 18 BY .77 AT ROW 3.15 COL 45
          FGCOLOR 7 
     "Primer Apellido" VIEW-AS TEXT
          SIZE 17 BY .77 AT ROW 3.15 COL 64
          FGCOLOR 7 
     RECT-2 AT ROW 1.54 COL 99
     RECT-3 AT ROW 10.15 COL 99
     FOTO AT ROW 1.27 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112 BY 20.88
         BGCOLOR 17 FONT 4.

DEFINE FRAME FNovedades
     BDevengados AT ROW 2.35 COL 2
     BDeducciones AT ROW 2.35 COL 49
     BUTTON-189 AT ROW 12.58 COL 3
     "sobre la lista a la cual desea adicionar el concepto." VIEW-AS TEXT
          SIZE 43 BY .5 AT ROW 13.12 COL 38
     "  Deducciones" VIEW-AS TEXT
          SIZE 45 BY .81 AT ROW 1.54 COL 49
          BGCOLOR 18 FGCOLOR 15 FONT 1
     "  Devengados" VIEW-AS TEXT
          SIZE 45 BY .81 AT ROW 1.54 COL 2
          BGCOLOR 18 FGCOLOR 15 FONT 1
     "Para crear un nuevo devengado o una nueva deducción, presione click derecho" VIEW-AS TEXT
          SIZE 56 BY .81 AT ROW 12.31 COL 38
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 7.19
         SIZE 95 BY 14
         BGCOLOR 17 FONT 4
         TITLE "Novedades Fijas de Nomina".

DEFINE FRAME FRelNva
     R_Nit AT ROW 1.27 COL 20 COLON-ALIGNED HELP
          "Número documento de identificación"
     R_Nombre AT ROW 2.12 COL 20 COLON-ALIGNED HELP
          "Nombre del cliente"
     R_Apellido1 AT ROW 2.96 COL 20 COLON-ALIGNED HELP
          "Primer apellido del cliente"
     R_Apellido2 AT ROW 3.81 COL 20 COLON-ALIGNED HELP
          "Segundo apellido del cliente"
     R_Tel_Residencia AT ROW 4.65 COL 20 COLON-ALIGNED HELP
          "Teléfono de la residencia del cliente"
     R_Relacion AT ROW 5.5 COL 20 COLON-ALIGNED
     BUTTON-205 AT ROW 2.08 COL 53
     BUTTON-206 AT ROW 5.31 COL 53
     Btn_SC AT ROW 1.27 COL 53
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 19 ROW 12.58
         SIZE 69 BY 6.46
         BGCOLOR 17 FONT 4
         TITLE "Nueva Relacion".

DEFINE FRAME F_Relaciones
     Cmb_Relaciones AT ROW 1.27 COL 23 COLON-ALIGNED
     RActivas AT ROW 1.27 COL 75 NO-LABEL
     Btn_CreRel AT ROW 2.35 COL 25
     Btn_Activas AT ROW 2.35 COL 41
     Btn_SalRel AT ROW 2.35 COL 57
     Br_Relaciones AT ROW 3.69 COL 3
     W_MenRel AT ROW 13.12 COL 1 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 7.19
         SIZE 95 BY 14
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE "Relaciones".

DEFINE FRAME FRetiro
     RCiao AT ROW 1.27 COL 3 NO-LABEL
     BUTTON-207 AT ROW 3.96 COL 7
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 27 ROW 9.35
         SIZE 28 BY 5.38
         BGCOLOR 17 
         TITLE "Forma de Desvinculación".

DEFINE FRAME FDotacion
     Empleados.Fec_EntregaDotacion AT ROW 1.54 COL 77 COLON-ALIGNED
          LABEL "Fecha Entrega Última Dotacion"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     BUTTON-20 AT ROW 2.62 COL 78
     Empleados.Id_Dotacion AT ROW 3.42 COL 59
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY .65
     Empleados.Id_AuxilioDotacion AT ROW 4.5 COL 59
          VIEW-AS TOGGLE-BOX
          SIZE 21.43 BY .65
     Empleados.Id_PagaPorcentaje AT ROW 5.58 COL 59
          VIEW-AS TOGGLE-BOX
          SIZE 25.57 BY .65
     BUTTON-22 AT ROW 6.65 COL 41
     Empleados.Fec_InicioPrestamo AT ROW 8.27 COL 31 COLON-ALIGNED
          LABEL "Inicio Prestamo"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
     Empleados.Num_Camisas AT ROW 8.27 COL 65 COLON-ALIGNED
          LABEL "Camisas"
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
          BGCOLOR 15 
     Empleados.Talla_Camisas AT ROW 8.27 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
          BGCOLOR 15 
     Empleados.Prestamo_Especial AT ROW 9.35 COL 31 COLON-ALIGNED
          LABEL "Prestamo Especial"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
     Empleados.Num_Pantalones AT ROW 9.35 COL 65 COLON-ALIGNED
          LABEL "Pantalones"
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
          BGCOLOR 15 
     Empleados.Talla_Pantalon AT ROW 9.35 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
          BGCOLOR 15 
     Empleados.Plazo_Prestamo AT ROW 10.42 COL 31 COLON-ALIGNED
          LABEL "Plazo"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
     Empleados.Num_Chaquetas AT ROW 10.42 COL 65 COLON-ALIGNED
          LABEL "Chaquetas"
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
          BGCOLOR 15 
     Empleados.Talla_Chaquetas AT ROW 10.42 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
          BGCOLOR 15 
     Empleados.Num_Corbatas AT ROW 11.5 COL 65 COLON-ALIGNED
          LABEL "Corbatas"
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
          BGCOLOR 15 
     Empleados.Puntos_Adicionales AT ROW 12.04 COL 35 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 2.86 BY .81
     "  Talla" VIEW-AS TEXT
          SIZE 14 BY 1.08 AT ROW 6.65 COL 77
          BGCOLOR 18 FGCOLOR 15 
     "  Crédito Rotatorio" VIEW-AS TEXT
          SIZE 36 BY 1.08 AT ROW 6.65 COL 5
          BGCOLOR 18 FGCOLOR 15 
     "  Tipo de Dotación" VIEW-AS TEXT
          SIZE 21 BY 1.08 AT ROW 6.65 COL 56
          BGCOLOR 18 FGCOLOR 15 
     RECT-8 AT ROW 7.19 COL 56
     RECT-10 AT ROW 7.19 COL 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 7.19
         SIZE 95 BY 14
         BGCOLOR 17 FONT 4
         TITLE "Dotación y Prestamos".

DEFINE FRAME FConceptos
     BConceptos AT ROW 1.27 COL 3
     BUTTON-38 AT ROW 11.77 COL 16
     BUTTON-37 AT ROW 11.77 COL 32
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 31 ROW 7.73
         SIZE 48 BY 12.92
         BGCOLOR 17 FONT 4
         TITLE "Conceptos Disponibles".

DEFINE FRAME FGeneral
     Empleados.Estado AT ROW 2.62 COL 7 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Retirado", 2,
"Lic. Maternidad", 3,
"Lic. Remunerada", 4,
"Lic. No Remunerada", 5,
"Vacaciones", 6,
"Comisión", 7,
"Calamidad Doméstica", 8,
"Incapacidad", 9,
"Suspendido", 10
          SIZE 20 BY 10.23
     Empleados.Periodo_Pago AT ROW 2.62 COL 39.14 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Semanal", 1,
"Quincenal", 2,
"Mensual", 3
          SIZE 31 BY .81
     Empleados.Jornada_Laboral AT ROW 2.62 COL 72.14 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Diurna", 1,
"Nocturna", 2
          SIZE 21 BY .81
     Empleados.Tipo_Contrato AT ROW 5.31 COL 38 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Indefinido", 1,
"Fijo", 2,
"Integral", 3
          SIZE 32 BY .81
     Empleados.Tiempo_Contrato AT ROW 5.31 COL 83 COLON-ALIGNED
          LABEL "Diás a Contratar"
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
          BGCOLOR 15 
     Empleados.Id_Manejo AT ROW 6.65 COL 39
          LABEL "Es empleado de manejo y confianza?"
          VIEW-AS TOGGLE-BOX
          SIZE 29 BY .65
     Empleados.Cargo AT ROW 8.27 COL 48 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 44 BY .81
          BGCOLOR 15 
     C_Perfil AT ROW 9.35 COL 48 COLON-ALIGNED
     Empleados.Salario_Mensual AT ROW 10.42 COL 78.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.57 BY .81
          BGCOLOR 15 
     Empleados.PerNom_Actual AT ROW 11.5 COL 79 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.57 BY .81
          BGCOLOR 18 FGCOLOR 15 
     "  Periodicidad de Pago" VIEW-AS TEXT
          SIZE 34.14 BY 1.08 AT ROW 1.27 COL 37
          BGCOLOR 18 FGCOLOR 15 
     "  Jornada Laboral" VIEW-AS TEXT
          SIZE 23 BY 1.08 AT ROW 1.27 COL 71.14
          BGCOLOR 18 FGCOLOR 15 
     "  Tipo de Contrato" VIEW-AS TEXT
          SIZE 34 BY 1.08 AT ROW 3.96 COL 37
          BGCOLOR 18 FGCOLOR 15 
     "  Estado en el que se encuentra el usuario" VIEW-AS TEXT
          SIZE 33 BY 1.04 AT ROW 1.27 COL 3
          BGCOLOR 18 FGCOLOR 15 
     RECT-1 AT ROW 1.54 COL 3
     RECT-5 AT ROW 1.54 COL 37.14
     RECT-6 AT ROW 1.81 COL 71.14
     RECT-7 AT ROW 4.77 COL 37
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 7.19
         SIZE 95 BY 14
         BGCOLOR 17 FONT 4
         TITLE "Información General".

DEFINE FRAME FIHV
     RIHVEmpleado AT ROW 1.27 COL 11 NO-LABEL
     Actual AT ROW 2.15 COL 2 COLON-ALIGNED NO-LABEL
     RIHV AT ROW 3.42 COL 4 NO-LABEL
     CInformativosI AT ROW 5.42 COL 6.86
     WFecIni AT ROW 6.58 COL 18 COLON-ALIGNED
     BUTTON-199 AT ROW 6.65 COL 51
     WFecFin AT ROW 7.65 COL 18 COLON-ALIGNED
     BUTTON-200 AT ROW 8 COL 51
     RECT-302 AT ROW 1.54 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 22 ROW 9.88
         SIZE 68 BY 9.42
         BGCOLOR 17 FONT 4
         TITLE "Imprimir Hoja de Vida".

DEFINE FRAME FHV_Detalle
     HVEmpleados.FecInicial AT ROW 1.12 COL 70 COLON-ALIGNED
          LABEL "Fecha Inicio"
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 
     HVEmpleados.Descripcion AT ROW 1.15 COL 2 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 59 BY 2.54
          BGCOLOR 15 
     HVEmpleados.FecFinal AT ROW 2 COL 70 COLON-ALIGNED
          LABEL "Fecha Final"
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 
     HVEmpleados.Valor AT ROW 2.88 COL 70 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 
     BUTTON-196 AT ROW 3.96 COL 2
     BUTTON-197 AT ROW 3.96 COL 69
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 5 ROW 16.08
         SIZE 91 BY 4.85
         BGCOLOR 17 FONT 4
         TITLE "Anotación a Hoja de Vida".

DEFINE FRAME FConsulta
     BEmpleados AT ROW 1.27 COL 3
     BUTTON-21 AT ROW 8.81 COL 60
     RCon AT ROW 9.08 COL 5 NO-LABEL
     WCon AT ROW 10.15 COL 11 COLON-ALIGNED
     Cmb_Estado AT ROW 11.23 COL 11 COLON-ALIGNED
     RECT-305 AT ROW 8.54 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 14 ROW 7.73
         SIZE 78 BY 12.38
         BGCOLOR 17 FONT 4
         TITLE "Consulta Empleados".

DEFINE FRAME FSS
     C_Eps AT ROW 4.5 COL 26 COLON-ALIGNED
     Empleados.FecAF_EPS AT ROW 4.5 COL 58 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 
     C_Pensiones AT ROW 5.58 COL 26 COLON-ALIGNED
     Empleados.FecAF_Pensiones AT ROW 5.58 COL 58 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 
     C_Cesantias AT ROW 6.65 COL 26 COLON-ALIGNED
     Empleados.FecAF_Cesantias AT ROW 6.65 COL 58 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 
     " Fecha de Afiliación" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 3.42 COL 60
          BGCOLOR 18 FGCOLOR 15 FONT 1
     " Entidad escogida" VIEW-AS TEXT
          SIZE 29 BY .81 AT ROW 3.42 COL 29
          BGCOLOR 18 FGCOLOR 15 FONT 1
     " Tipo de Entidad" VIEW-AS TEXT
          SIZE 17 BY .81 AT ROW 3.42 COL 12
          BGCOLOR 18 FGCOLOR 15 FONT 1
     RECT-4 AT ROW 3.69 COL 11
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 7.19
         SIZE 95 BY 14
         BGCOLOR 17 FONT 4
         TITLE "Seguridad Social".

DEFINE FRAME FHV
     BUTTON-190 AT ROW 1.19 COL 72
     Clientes.Dir_Residencia AT ROW 2.08 COL 20.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 70.43 BY .81
          BGCOLOR 15 
     Clientes.Tel_Residencia AT ROW 3 COL 20.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.72 BY .81
          BGCOLOR 15 
     Clientes.Estrato AT ROW 3.04 COL 58 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "0","1","2","3","4","5","6","7" 
          DROP-DOWN-LIST
          SIZE 10 BY 1
          BGCOLOR 15 
     RTV AT ROW 3.15 COL 71 NO-LABEL
     Clientes.Celular AT ROW 3.96 COL 20.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.72 BY .81
          BGCOLOR 15 
     Clientes.Email AT ROW 3.96 COL 58 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 33 BY .81
          BGCOLOR 15 
     Clientes.Niv_Educativo AT ROW 5.31 COL 20.43 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 6
          LIST-ITEMS "Ninguno","Primaria","Bachiller","Tecnólogo","Técnico","Profesional","Post-Grado" 
          DROP-DOWN-LIST
          SIZE 15.72 BY 1
          BGCOLOR 15 
     Clientes.Est_Civil AT ROW 5.31 COL 58 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "No Aplica","Soltero","Casado","Separado","Viudo","Unión Libre" 
          DROP-DOWN-LIST
          SIZE 15.72 BY 1
          BGCOLOR 15 
     Clientes.Fec_Nacimiento AT ROW 6.23 COL 20.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.72 BY .81
          BGCOLOR 15 FGCOLOR 15 
     Cumplidos AT ROW 6.23 COL 58 COLON-ALIGNED
     CInformativos AT ROW 7.65 COL 19 COLON-ALIGNED
     BtnNvoInformativo AT ROW 7.65 COL 73
     BInformativo AT ROW 9.08 COL 3
     " Información General" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 1.23 COL 5
          FGCOLOR 7 FONT 5
     RECT-11 AT ROW 1.54 COL 3
     RECT-303 AT ROW 7.19 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 7.19
         SIZE 95 BY 14
         BGCOLOR 17 FONT 4
         TITLE "Hoja de Vida".


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
         TITLE              = "Maestros de Empleados"
         HEIGHT             = 20.88
         WIDTH              = 112
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
/* REPARENT FRAME */
ASSIGN FRAME FConceptos:FRAME = FRAME FMain:HANDLE
       FRAME FConsulta:FRAME = FRAME FMain:HANDLE
       FRAME FDotacion:FRAME = FRAME FMain:HANDLE
       FRAME FGeneral:FRAME = FRAME FMain:HANDLE
       FRAME FHV:FRAME = FRAME FMain:HANDLE
       FRAME FHV_Detalle:FRAME = FRAME FMain:HANDLE
       FRAME FIHV:FRAME = FRAME FMain:HANDLE
       FRAME FNovedades:FRAME = FRAME FMain:HANDLE
       FRAME FRelNva:FRAME = FRAME FMain:HANDLE
       FRAME FRetiro:FRAME = FRAME FMain:HANDLE
       FRAME FSS:FRAME = FRAME FMain:HANDLE
       FRAME F_Relaciones:FRAME = FRAME FMain:HANDLE.

/* SETTINGS FOR FRAME FConceptos
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BConceptos 1 FConceptos */
ASSIGN 
       FRAME FConceptos:HIDDEN           = TRUE
       FRAME FConceptos:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME FConsulta
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BEmpleados RECT-305 FConsulta */
ASSIGN 
       FRAME FConsulta:HIDDEN           = TRUE
       FRAME FConsulta:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN WCon IN FRAME FConsulta
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FDotacion
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FDotacion:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Empleados.Fec_EntregaDotacion IN FRAME FDotacion
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Empleados.Fec_InicioPrestamo IN FRAME FDotacion
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Empleados.Num_Camisas IN FRAME FDotacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Empleados.Num_Chaquetas IN FRAME FDotacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Empleados.Num_Corbatas IN FRAME FDotacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Empleados.Num_Pantalones IN FRAME FDotacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Empleados.Plazo_Prestamo IN FRAME FDotacion
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Empleados.Prestamo_Especial IN FRAME FDotacion
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FRAME FGeneral
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FGeneral:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX Empleados.Id_Manejo IN FRAME FGeneral
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Empleados.PerNom_Actual IN FRAME FGeneral
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Empleados.Tiempo_Contrato IN FRAME FGeneral
   EXP-LABEL                                                            */
/* SETTINGS FOR FRAME FHV
                                                                        */
/* BROWSE-TAB BInformativo BtnNvoInformativo FHV */
ASSIGN 
       BInformativo:POPUP-MENU IN FRAME FHV             = MENU POPUP-MENU-BInformativo:HANDLE.

/* SETTINGS FOR FILL-IN Cumplidos IN FRAME FHV
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FHV_Detalle
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FHV_Detalle:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN HVEmpleados.FecFinal IN FRAME FHV_Detalle
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN HVEmpleados.FecInicial IN FRAME FHV_Detalle
   EXP-LABEL                                                            */
/* SETTINGS FOR FRAME FIHV
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FIHV:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Actual IN FRAME FIHV
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX CInformativosI IN FRAME FIHV
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FRAME FMain
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON Btn_Borrar IN FRAME FMain
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME FMain
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Deshacer IN FRAME FMain
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Informacion IN FRAME FMain
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_informe IN FRAME FMain
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Ingresar IN FRAME FMain
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Salir IN FRAME FMain
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Salvar IN FRAME FMain
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_SS IN FRAME FMain
   5                                                                    */
/* SETTINGS FOR BUTTON BUTTON-11 IN FRAME FMain
   1                                                                    */
/* SETTINGS FOR BUTTON BUTTON-15 IN FRAME FMain
   5                                                                    */
/* SETTINGS FOR BUTTON BUTTON-17 IN FRAME FMain
   5                                                                    */
/* SETTINGS FOR BUTTON BUTTON-18 IN FRAME FMain
   5                                                                    */
/* SETTINGS FOR BUTTON BUTTON-201 IN FRAME FMain
   5                                                                    */
/* SETTINGS FOR BUTTON BUTTON-4 IN FRAME FMain
   1                                                                    */
/* SETTINGS FOR FILL-IN C_Apellido1 IN FRAME FMain
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN C_Apellido2 IN FRAME FMain
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN C_Nombre IN FRAME FMain
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN Empleados.Fec_Retiro IN FRAME FMain
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FRAME FNovedades
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BDevengados TEXT-18 FNovedades */
/* BROWSE-TAB BDeducciones BDevengados FNovedades */
ASSIGN 
       FRAME FNovedades:HIDDEN           = TRUE.

ASSIGN 
       BDeducciones:POPUP-MENU IN FRAME FNovedades             = MENU POPUP-MENU-BDeducciones:HANDLE.

ASSIGN 
       BDevengados:POPUP-MENU IN FRAME FNovedades             = MENU POPUP-MENU-BDevengados:HANDLE.

/* SETTINGS FOR FRAME FRelNva
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME FRelNva:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRetiro
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRetiro:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FSS
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FSS:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Relaciones
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Relaciones Btn_SalRel F_Relaciones */
ASSIGN 
       FRAME F_Relaciones:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON Btn_SalRel IN FRAME F_Relaciones
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_MenRel IN FRAME F_Relaciones
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BConceptos
/* Query rebuild information for BROWSE BConceptos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TConceptos.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BConceptos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BDeducciones
/* Query rebuild information for BROWSE BDeducciones
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Novedades_Nit WHERE Novedades_Nit.Agencia EQ Empleados.Agencia AND
                                                     Novedades_Nit.Nit     EQ Empleados.Nit AND
                                                     Novedades_Nit.Tipo    EQ 2.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BDeducciones */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BDevengados
/* Query rebuild information for BROWSE BDevengados
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Novedades_Nit WHERE Novedades_Nit.Agencia EQ Empleados.Agencia AND
                                                     Novedades_Nit.Nit     EQ Empleados.Nit AND
                                                     Novedades_Nit.Tipo    EQ 1.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BDevengados */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BEmpleados
/* Query rebuild information for BROWSE BEmpleados
     _START_FREEFORM
OPEN QUERY BEmpleados FOR EACH Empleados WHERE Empleados.Estado EQ 1.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BEmpleados */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BInformativo
/* Query rebuild information for BROWSE BInformativo
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH HVEmpleados WHERE HvEmpleados.Nit EQ Empleados.Nit.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BInformativo */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Relaciones
/* Query rebuild information for BROWSE Br_Relaciones
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE Br_Relaciones */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FHV
/* Query rebuild information for FRAME FHV
     _TblList          = "bdcentral.Clientes"
     _Query            is OPENED
*/  /* FRAME FHV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FHV_Detalle
/* Query rebuild information for FRAME FHV_Detalle
     _TblList          = "bdcentral.HVEmpleados"
     _Query            is OPENED
*/  /* FRAME FHV_Detalle */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FMain
/* Query rebuild information for FRAME FMain
     _TblList          = "bdcentral.Empleados,bdcentral.Clientes OF bdcentral.Empleados"
     _Query            is NOT OPENED
*/  /* FRAME FMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Maestros de Empleados */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Maestros de Empleados */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BDeducciones
&Scoped-define FRAME-NAME FNovedades
&Scoped-define SELF-NAME BDeducciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BDeducciones wWin
ON ROW-DISPLAY OF BDeducciones IN FRAME FNovedades
DO:
  FIND Cfg_Novedades WHERE
       Cfg_Novedades.Tipo   EQ Novedades_Nit.Tipo AND
       Cfg_Novedades.Codigo EQ Novedades_Nit.Codigo NO-LOCK NO-ERROR.
  IF AVAILABLE Cfg_Novedades THEN
     WNomDev = Cfg_Novedades.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BDevengados
&Scoped-define SELF-NAME BDevengados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BDevengados wWin
ON ROW-DISPLAY OF BDevengados IN FRAME FNovedades
DO:
  FIND Cfg_Novedades WHERE
       Cfg_Novedades.Tipo   EQ Novedades_Nit.Tipo AND
       Cfg_Novedades.Codigo EQ Novedades_Nit.Codigo NO-LOCK NO-ERROR.
  IF AVAILABLE Cfg_Novedades THEN
     WNomDev = Cfg_Novedades.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BEmpleados
&Scoped-define FRAME-NAME FConsulta
&Scoped-define SELF-NAME BEmpleados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BEmpleados wWin
ON MOUSE-SELECT-CLICK OF BEmpleados IN FRAME FConsulta
DO:
  RUN Mostrar_Empleados.
  OPEN QUERY BInformativo FOR EACH HVEmpleados WHERE HvEmpleados.Nit EQ Empleados.Nit INDEXED-REPOSITION.
  OPEN QUERY BR_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BEmpleados wWin
ON ROW-DISPLAY OF BEmpleados IN FRAME FConsulta
DO:
  WNomEmp = "Empleado No Encontrado en Clientes".
  FIND Clientes WHERE Clientes.Nit EQ Empleados.Nit NO-LOCK NO-ERROR.
  IF AVAILABLE Clientes THEN
     WNomEmp = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FMain
&Scoped-define SELF-NAME BtnHV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnHV wWin
ON CHOOSE OF BtnHV IN FRAME FMain /* Hoja de Vida */
DO:
  HIDE FRAME F_Relaciones.
  OPEN QUERY BInformativo FOR EACH HVEmpleados WHERE HvEmpleados.Nit EQ Empleados.Nit INDEXED-REPOSITION.
  HIDE FRAME FSS.
  HIDE FRAME FGeneral.
  HIDE FRAME FDotacion.
  HIDE FRAME FNovedades.
  VIEW FRAME FHV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FHV
&Scoped-define SELF-NAME BtnNvoInformativo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnNvoInformativo wWin
ON CHOOSE OF BtnNvoInformativo IN FRAME FHV /* Agregar Nuevo */
DO:
  ASSIGN FRAME FHV CInformativos.
  IF SUBSTRING(CInformativos,1,5) EQ "00000" THEN DO:
     MESSAGE "Debe seleccionarse el concepto que se desea crear" 
             VIEW-AS ALERT-BOX.
     APPLY "entry" TO CInformativos IN FRAME FHV.
     RETURN NO-APPLY.
  END.
  W_NvaNota = YES.
  DO WITH FRAME FHV_Detalle:
     ASSIGN HVEmpleados.Descripcion:SCREEN-VALUE = ""
            HVEmpleados.Valor:SCREEN-VALUE = "0"
            HVEmpleados.fecIni:SCREEN-VALUE = STRING(W_Fecha)
            HvEmpleados.FecFin:SCREEN-VALUE = "".
     APPLY "entry" TO HVEmpleados.Descripcion.
  END.
  
  VIEW FRAME FHV_Detalle.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Btn_Activas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Activas wWin
ON CHOOSE OF Btn_Activas IN FRAME F_Relaciones /* Borrar */
DO:
  DO WITH FRAME F_Relaciones:
  W_MenRel:SCREEN-VALUE = "Escoja del Combo de Relaciones, el tipo de relación que quiere consultar".
  IF Br_Relaciones:NUM-SELECTED-ROWS EQ 0 THEN DO:
     MESSAGE "Debe posicionarse en la relación a Borrar" SKIP
             "mediante el mouse. Rectifique!!!" VIEW-AS ALERT-BOX INFORMATION.
  END.
  ELSE
  DO:
    FIND Relaciones WHERE Relaciones.Nit EQ Empleados.Nit:SCREEN-VALUE IN FRAME FMain AND
                          Relaciones.Cod_Relacion EQ INTEGER(SUBSTRING(Cmb_Relaciones:SCREEN-VALUE,1,5)) AND
                          Relaciones.Nit_Relacion EQ T_Relaciones.R_NitObjeto NO-ERROR.
    IF AVAILABLE(Relaciones) THEN DO:
       IF RActivas:SCREEN-VALUE EQ "1" THEN
          ASSIGN Relaciones.Estado = 2
                 Relaciones.Fec_Inactividad = W_Fecha.
       ELSE
          ASSIGN Relaciones.Estado = 1
                 Relaciones.Fec_Inactividad = ?.
    END.
    APPLY 'value-changed' TO Cmb_Relaciones.
    RETURN NO-APPLY.
  END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FMain
&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar wWin
ON CHOOSE OF Btn_Cancelar IN FRAME FMain /* Cancelar */
DO:
  ENABLE Btn_Salvar Btn_Salir Btn_Ingresar Btn_Borrar WITH FRAME FMain.
  FIND LAST Empleados NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Empleados THEN DO: 
     RUN Inicializar_Pantalla.
     DISABLE C_Agencia Empleados.Nit WITH FRAME FMain.
     DISABLE ALL WITH FRAME FGeneral.
     DISABLE ALL WITH FRAME FSS.
     DISABLE ALL WITH FRAME FDotacion.
  END.
  ELSE RUN Mostrar_Empleados.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Btn_CreRel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CreRel wWin
ON CHOOSE OF Btn_CreRel IN FRAME F_Relaciones /* Crear */
DO:
  DO WITH FRAME F_Relaciones:
      IF Cmb_Relaciones:SCREEN-VALUE BEGINS "00000" OR 
         Cmb_Relaciones:SCREEN-VALUE BEGINS "99999" THEN DO:
         MESSAGE "Para crear una relacion se debe escoger el tipo" VIEW-AS ALERT-BOX.
         APPLY "entry" TO Cmb_Relaciones.
         RETURN NO-APPLY.
      END.
      W_MenRel:SCREEN-VALUE = "Escoja del Combo de consulta la relación que se le asignará a la persona o empresa".
     RActivas:SCREEN-VALUE = "1".
     ENABLE Cmb_Relaciones Btn_SalRel.
     DISABLE Btn_CreRel Btn_Activas.
     VIEW FRAME FRelNva.
     APPLY "entry" TO R_Nit IN FRAME FRelNva.
     RETURN NO-APPLY.
  END.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FMain
&Scoped-define SELF-NAME Btn_Deshacer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deshacer wWin
ON CHOOSE OF Btn_Deshacer IN FRAME FMain /* Deshacer */
DO:
  FIND CURRENT Empleados.
  IF AVAILABLE Empleados THEN
     RUN Mostrar_Empleados.
  ELSE FIND LAST Empleados.
  FIND CURRENT Empleados NO-LOCK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_informe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_informe wWin
ON CHOOSE OF Btn_informe IN FRAME FMain /* Button 3 */
DO:
  WTipInfo = 1.
  RUN Imprimir.
  FIND Empleados WHERE ROWID(Empleados) EQ WRowId NO-LOCK NO-ERROR.
  IF AVAILABLE Empleados THEN RUN Mostrar_Empleados.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ingresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ingresar wWin
ON CHOOSE OF Btn_Ingresar IN FRAME FMain /* Ingresar */
DO:
    DO WITH FRAME FNovedades:
        FIND CURRENT Empleados NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Empleados THEN DO:
            OPEN QUERY BDevengados FOR EACH Novedades_Nit WHERE Novedades_Nit.Agencia = Empleados.Agencia
                                                            AND Novedades_Nit.Nit = "".

            OPEN QUERY BDeducciones FOR EACH Novedades_Nit WHERE Novedades_Nit.Agencia = Empleados.Agencia
                                                             AND Novedades_Nit.Nit = "".
        END.
    END.

    RUN Inicializar_Pantalla.

    ENABLE C_Agencia Empleados.Nit WITH FRAME FMain.
    ENABLE ALL WITH FRAME FGeneral.
    ENABLE ALL WITH FRAME FSS.
    ENABLE ALL WITH FRAME FDotacion.
    DISABLE Btn_Ingresar WITH FRAME Fmain.
    APPLY "entry" TO C_Agencia IN FRAME FMain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON CHOOSE OF Btn_Salir IN FRAME FMain /* Salir */
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


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Btn_SalRel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalRel wWin
ON CHOOSE OF Btn_SalRel IN FRAME F_Relaciones /* Salvar */
DO:
  DO WITH FRAME F_Relaciones:
    W_MenRel:SCREEN-VALUE = "Escoja del Combo de Relaciones, el tipo de relación que quiere consultar".
    FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
    IF AVAILABLE(Clientes) THEN DO:
        CREATE T_Relaciones.
        UPDATE T_Relaciones.R_Relacion =  SUBSTRING(Cmb_Relaciones:SCREEN-VALUE,8,15)
               T_Relaciones.R_AgeObjeto = Clientes.Agencia
               T_Relaciones.R_NitObjeto = Relaciones.Nit_Relacion
               T_Relaciones.R_NomDescri = Relaciones.Descripcion
               T_Relaciones.R_NomObjeto = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
               T_Relaciones.R_TelObjeto = Clientes.Tel_Residencia.
    END.
    DISABLE Btn_SalRel.
     OPEN QUERY BR_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
     ENABLE Btn_CreRel Btn_Activas.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FMain
&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar wWin
ON CHOOSE OF Btn_Salvar IN FRAME FMain /* Salvar */
DO:
    ASSIGN FRAME FMain C_Agencia.

    IF SUBSTRING(C_Agencia,1,3) EQ "000" THEN DO:
        MESSAGE "Debe escogerse la agencia a la que pertenece" SKIP
                "el empleado. rectifique la agencia!!!!"
            VIEW-AS ALERT-BOX.

        APPLY "entry" TO C_Agencia.
        RETURN NO-APPLY.
    END.


    IF INDEX(Empleados.Nit:SCREEN-VALUE,"-") > 0 THEN DO:
        MESSAGE "El formato para el número de documento es inválido." SKIP
                "Revise por favor..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    FIND Empleados WHERE Empleados.Nit = Empleados.Nit:SCREEN-VALUE IN FRAME FMain NO-ERROR.
    IF NOT AVAILABLE Empleados THEN DO:
        CREATE Empleados.
        ASSIGN FRAME FMain Empleados.Nit Empleados.Fec_Ingreso Fec_Retiro.
    END.

    FIND Clientes WHERE Clientes.Nit = Empleados.Nit NO-ERROR.
    IF NOT AVAILABLE Clientes THEN DO:
        ENABLE {&List-5} WITH FRAME FMain.

        ASSIGN FRAME FMain C_Nombre C_Apellido1 C_Apellido2.

        CREATE Clientes.
        ASSIGN Clientes.Agencia = INTEGER(SUBSTRING(C_Agencia,1,3))
               Clientes.Nit = Empleados.Nit
               Clientes.Nombre = C_Nombre
               Clientes.Apellido1 = C_Apellido1
               Clientes.Apellido2 = C_Apellido2
               Clientes.Tipo_Identificacion = "C.C"
               Clientes.Fec_Ingreso = W_Fecha
               Clientes.Tipo_Vinculo = 3.

        DISABLE {&List-6} WITH FRAME FMain.

        WNvoCliente = NO.
    END.

    FIND Clientes WHERE Clientes.Nit EQ Empleados.Nit NO-ERROR.
    IF AVAILABLE Clientes THEN DO:
        ASSIGN FRAME FHV
               Clientes.DIR_Residencia
               Clientes.Tel_Residencia
               Clientes.email
               Clientes.Celular
               Clientes.Fec_Nacimiento
               RTV
               Clientes.Niv_Educativo
               Clientes.Estrato
               Clientes.Est_Civil
               Clientes.Tipo_Vivienda = RTV.
    END.
    ELSE
        FIND CURRENT Empleados.

    ASSIGN FRAME FMain C_Agencia Empleados.Fec_Ingreso.

    Empleados.Agencia = INTEGER(SUBSTRING(C_Agencia,1,3)).

    DO WITH FRAME FGeneral:
        ASSIGN FRAME FGeneral
               Empleados.Estado
               Empleados.Periodo_Pago
               Empleados.Jornada_Laboral
               Empleados.Tipo_Contrato
               Empleados.Tiempo_Contrato
               Empleados.Cargo
               Empleados.Salario_Mensual
               Empleados.Id_Manejo.

        Empleados.Perfil_Cargo = INTEGER(SUBSTRING(C_Perfil,1,3)).

        IF Empleados.Estado = 2 THEN
            Empleados.Fec_Retiro = W_Fecha.

        DISPLAY Empleados.Fec_Retiro WITH FRAME FMain.
    END.

    DO WITH FRAME FSS:
        ASSIGN FRAME FSS
               Empleados.FecAF_Eps
               Empleados.FecAF_Pensiones
               Empleados.FecAF_Cesantias.

        ASSIGN Empleados.Fon_Pensiones = INTEGER(SUBSTRING(C_Pensiones:SCREEN-VALUE,1,5))
               Empleados.Fon_Cesantias = INTEGER(SUBSTRING(C_Cesantias:SCREEN-VALUE,1,5))
               Empleados.EPS = INTEGER(SUBSTRING(C_EPS:SCREEN-VALUE,1,5)).
    END.

    DO WITH FRAME FDotacion:
        ASSIGN FRAME FDotacion
               Empleados.Id_Dotacion
               Empleados.Id_PagaPorcentaje
               Empleados.Fec_EntregaDotacion
               Empleados.Num_Camisas
               Empleados.Talla_Camisas
               Empleados.Id_AuxilioDotacion
               Empleados.Num_Pantalones
               Empleados.Talla_Pantalon
               Empleados.Num_Chaquetas
               Empleados.Talla_Chaquetas
               Empleados.Num_Corbatas
               Empleados.Fec_InicioPrestamo
               Empleados.Prestamo_Especial
               Empleados.Plazo_Prestamo.
    END.

    ENABLE Btn_Ingresar WITH FRAME FMain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRelNva
&Scoped-define SELF-NAME Btn_SC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SC wWin
ON CHOOSE OF Btn_SC IN FRAME FRelNva /* SC */
DO:
  R_Nit:SCREEN-VALUE = "SC_" + STRING(NEXT-VALUE(Sec_NitAuto)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FMain
&Scoped-define SELF-NAME Btn_SS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SS wWin
ON CHOOSE OF Btn_SS IN FRAME FMain /* Seguridad Social */
DO:
  HIDE FRAME F_Relaciones.
  VIEW FRAME FSS.
  HIDE FRAME FGeneral.
  HIDE FRAME FDotacion.
  HIDE FRAME FNovedades.
  HIDE FRAME FHV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-15 wWin
ON CHOOSE OF BUTTON-15 IN FRAME FMain /* Novedades Fijas Nomina */
DO:
  HIDE FRAME FSS.
  HIDE FRAME F_Relaciones.
  HIDE FRAME FGeneral.
  HIDE FRAME FDotacion.
  HIDE FRAME FHV.
  VIEW FRAME FNovedades.
  DO WITH FRAME FNovedades:
  FIND CURRENT Empleados NO-LOCK.
  OPEN QUERY BDevengados FOR EACH Novedades_Nit 
                            WHERE Novedades_Nit.Agencia EQ Empleados.Agencia AND
                                  Novedades_Nit.Nit     EQ Empleados.Nit AND
                                  Novedades_Nit.Tipo    EQ 1.
  OPEN QUERY BDeducciones FOR EACH Novedades_Nit WHERE Novedades_Nit.Agencia EQ Empleados.Agencia AND
                                                       Novedades_Nit.Nit     EQ Empleados.Nit AND
                                                       Novedades_Nit.Tipo    EQ 2.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-17 wWin
ON CHOOSE OF BUTTON-17 IN FRAME FMain /* Dotación */
DO:
  HIDE FRAME F_Relaciones.
  HIDE FRAME FSS.
  HIDE FRAME FGeneral.
  VIEW FRAME FDotacion.
  HIDE FRAME FNovedades.
  HIDE FRAME FHV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-18 wWin
ON CHOOSE OF BUTTON-18 IN FRAME FMain /* General */
DO:
  HIDE FRAME F_Relaciones.
  HIDE FRAME FSS.
  VIEW FRAME FGeneral.
  HIDE FRAME FDotacion.
  HIDE FRAME FNovedades.
  HIDE FRAME FHV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FNovedades
&Scoped-define SELF-NAME BUTTON-189
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-189 wWin
ON CHOOSE OF BUTTON-189 IN FRAME FNovedades /* Asignar Novedades Fijas */
DO:
  CASE Empleados.Periodo_Pago:
      WHEN 1 THEN WDiast = 7.
      WHEN 2 THEN WDiast = 15.
      WHEN 3 THEN WDiast = 30.
  END CASE.
  FOR EACH Cfg_Novedades WHERE 
           Cfg_Novedades.Clase    EQ 1 AND 
           Cfg_Novedades.Estado   EQ 1 AND 
           Cfg_Novedades.Id_Valor NE 3 AND 
           Cfg_Novedades.Id_Pago  EQ NO NO-LOCK: 
         FIND Novedades_Nit WHERE
              Novedades_Nit.Tipo   EQ Cfg_Novedades.Tipo AND
              Novedades_Nit.Codigo EQ Cfg_Novedades.Codigo AND
              Novedades_Nit.Nit    EQ Empleados.Nit NO-ERROR.
         IF NOT AVAILABLE Novedades_Nit THEN
            CREATE Novedades_Nit.
         ASSIGN Novedades_Nit.Agencia     = Empleados.Agencia
                Novedades_Nit.Tipo        = Cfg_Novedades.Tipo
                Novedades_Nit.Codigo      = Cfg_Novedades.Codigo
                Novedades_Nit.Nit         = Empleados.Nit.
  END.
DO WITH FRAME FNovedades:
OPEN QUERY BDevengados FOR EACH Novedades_Nit WHERE Novedades_Nit.Agencia EQ Empleados.Agencia AND
                                                     Novedades_Nit.Nit     EQ Empleados.Nit AND
                                                     Novedades_Nit.Tipo    EQ 1.
OPEN QUERY BDeducciones FOR EACH Novedades_Nit WHERE Novedades_Nit.Agencia EQ Empleados.Agencia AND
                                                     Novedades_Nit.Nit     EQ Empleados.Nit AND
                                                     Novedades_Nit.Tipo    EQ 2.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FHV
&Scoped-define SELF-NAME BUTTON-190
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-190 wWin
ON CHOOSE OF BUTTON-190 IN FRAME FHV /* Ver información completa */
DO:
  RUN W-ProClientes.r.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FHV_Detalle
&Scoped-define SELF-NAME BUTTON-196
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-196 wWin
ON CHOOSE OF BUTTON-196 IN FRAME FHV_Detalle /* Guardar Anotación */
DO:
  IF W_NvaNota THEN DO:
     CREATE HVEmpleados.
     ASSIGN HVEmpleados.Consecutivo = NEXT-VALUE(Sec_HvEmpleados)
            HVEmpleados.Nit         = Empleados.Nit
            HVEmpleados.Agencia     = Empleados.Agencia
            HVEmpleados.Codigo      = INTEGER(SUBSTRING(CInformativos,1,5)).
  END.
  FIND CURRENT HvEmpleados.
  ASSIGN FRAME FHV_Detalle HVEmpleados.Descripcion HVEmpleados.Valor HVEmpleados.FecIni HVEmpleados.FecFin.
  W_NvaNota = NO.
  APPLY "value-changed" TO CInformativos IN FRAME FHV.
  HIDE FRAME FHV_Detalle.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-197
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-197 wWin
ON CHOOSE OF BUTTON-197 IN FRAME FHV_Detalle /* Ocultar */
DO:
  W_NvaNota = NO.
  APPLY "value-changed" TO CInformativos IN FRAME FHV.
  HIDE FRAME FHV_Detalle.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FIHV
&Scoped-define SELF-NAME BUTTON-199
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-199 wWin
ON CHOOSE OF BUTTON-199 IN FRAME FIHV /* Imprimir */
DO:
  ASSIGN FRAME FIhv CInformativosi WFecIni WFecFin RIHV RihvEmpleado.
  WTipInfo  = 2.
  RUN Imprimir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FDotacion
&Scoped-define SELF-NAME BUTTON-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-20 wWin
ON CHOOSE OF BUTTON-20 IN FRAME FDotacion /* Nueva Dotación */
DO:
  MESSAGE "aqui se crea novedad informativa" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FIHV
&Scoped-define SELF-NAME BUTTON-200
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-200 wWin
ON CHOOSE OF BUTTON-200 IN FRAME FIHV /* Ocultar */
DO:
  HIDE FRAME FIHV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FMain
&Scoped-define SELF-NAME BUTTON-201
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-201 wWin
ON CHOOSE OF BUTTON-201 IN FRAME FMain /* Relaciones */
DO:
  HIDE FRAME FSS.
  HIDE FRAME FGeneral.
  HIDE FRAME FDotacion.
  HIDE FRAME FHV.
  HIDE FRAME FNovedades.
  FOR EACH T_Relaciones: DELETE T_Relaciones. END.
  OPEN QUERY BR_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
  HIDE FRAME F_segmentacion FRAME F_Ubicacion FRAME F_Economica FRAME F_Fechas FRAME F_Otros.
  Cmb_Relaciones:SCREEN-VALUE IN FRAME F_Relaciones = Cmb_Relaciones:ENTRY(1).
  VIEW FRAME F_Relaciones.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRelNva
&Scoped-define SELF-NAME BUTTON-205
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-205 wWin
ON CHOOSE OF BUTTON-205 IN FRAME FRelNva /* Grabar */
DO:
  ASSIGN FRAME FRelNva R_Nit R_Nombre R_Apellido1 R_Apellido2 R_Tel_Residencia R_Relacion.
  IF R_Nit EQ "" OR R_Nombre EQ "" OR R_Apellido1 EQ "" OR R_Relacion EQ "" THEN DO:
     MESSAGE "Debe entrarse minimo: Nit, Nombre, apellido1 y el tipo de relacion" SKIP
             "o parentesco de la persona. rectifique la informacion" VIEW-AS ALERT-BOX.
     APPLY "entry" TO R_Nit.
     RETURN NO-APPLY.
  END.
  ELSE DO:
      FIND Clientes WHERE Clientes.Nit EQ R_Nit NO-ERROR.
      IF NOT AVAILABLE Clientes THEN DO:
          IF INDEX(R_nit,"-") > 0 THEN DO:
              MESSAGE "El formato para el número de documento es inválido." SKIP
                      "Revise por favor..."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
          END.

         CREATE Clientes.
         ASSIGN Clientes.Agencia   = W_Agencia
                Clientes.Nit       = R_Nit
                Clientes.Nombre    = R_Nombre
                Clientes.Apellido1 = R_Apellido1
                Clientes.Apellido2 = R_Apellido2
                Clientes.Tel_Residencia = R_Tel_Residencia.
      END.
      DO WITH FRAME F_Relaciones:
         CREATE Relaciones.
         ASSIGN Relaciones.Nit             = Empleados.Nit:SCREEN-VALUE IN FRAME FMain
                Relaciones.Cod_Relacion    = INTEGER(SUBSTRING(Cmb_Relaciones:SCREEN-VALUE,1,5))
                Relaciones.Nit_Relacion    = R_Nit
                Relaciones.Usuario         = W_Usuario
                Relaciones.Fec_Ingreso     = W_Fecha
                Relaciones.Descripcion     = R_Relacion:SCREEN-VALUE
                Relaciones.Estado          = 1.
      END.
      DO WITH FRAME FNvaRel:
        ASSIGN R_Nit:SCREEN-VALUE = ""
               R_Nombre:SCREEN-VALUE = ""
               R_Apellido1:SCREEN-VALUE = ""
               R_Apellido2:SCREEN-VALUE = ""
               R_Tel_Residencia:SCREEN-VALUE = "".
      END.
      HIDE FRAME FRelNva.
      APPLY "choose" TO Btn_SalRel IN FRAME F_Relaciones.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-206
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-206 wWin
ON CHOOSE OF BUTTON-206 IN FRAME FRelNva /* Ocultar */
DO:
    ASSIGN R_Nit:SCREEN-VALUE = ""
         R_Nombre:SCREEN-VALUE = ""
         R_Apellido1:SCREEN-VALUE = ""
         R_Apellido2:SCREEN-VALUE = ""
         R_Tel_Residencia:SCREEN-VALUE = "".

    HIDE FRAME FRelNva.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRetiro
&Scoped-define SELF-NAME BUTTON-207
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-207 wWin
ON CHOOSE OF BUTTON-207 IN FRAME FRetiro /* Aceptar */
DO:
  ASSIGN FRAME FRetiro RCiao.
  HIDE FRAME FRetiro.
  FIND Cfg_Novedades WHERE Cfg_Novedades.Tipo EQ 3 AND Cfg_Novedades.Codigo EQ RCiao NO-LOCK NO-ERROR.
  IF AVAILABLE Cfg_Novedades THEN DO:
     CInformativos:SCREEN-VALUE IN FRAME FHV = STRING(Cfg_Novedades.Codigo,"99999") + " - " + 
                  Cfg_Novedades.Nombre.
     APPLY "choose" TO BtnNvoInformativo.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FConsulta
&Scoped-define SELF-NAME BUTTON-21
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-21 wWin
ON CHOOSE OF BUTTON-21 IN FRAME FConsulta /* Ocultar */
DO:
  HIDE FRAME FConsulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FConceptos
&Scoped-define SELF-NAME BUTTON-37
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-37 wWin
ON CHOOSE OF BUTTON-37 IN FRAME FConceptos /* Ocultar */
DO:
  HIDE FRAME FConceptos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-38
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-38 wWin
ON CHOOSE OF BUTTON-38 IN FRAME FConceptos /* Adicionar */
DO:
 DEFINE VAR WMod AS LOGICAL INITIAL NO.
 DEFINE VAR WConDv AS INTEGER FORMAT "99".
 DEFINE VAR WConDd AS INTEGER FORMAT "99".
 DEFINE VAR i AS INTEGER FORMAT "99" INITIAL 0.

 FOR EACH Novedades_Nit WHERE Novedades_Nit.Tipo EQ 1 AND Novedades_Nit.Nit EQ Empleados.Nit: WConDv = WConDv + 1. END.
 FOR EACH Novedades_Nit WHERE Novedades_Nit.Tipo EQ 2 AND Novedades_Nit.Nit EQ Empleados.Nit: WConDd = WConDd + 1. END.
 IF WTipo EQ 1 THEN DO: /*adiciona concepto devengado*/
     IF AVAILABLE TConceptos THEN DO:
        CREATE Novedades_Nit.
        ASSIGN Novedades_Nit.Agencia = Empleados.Agencia
               Novedades_Nit.Tipo    = TConceptos.Tip
               Novedades_Nit.Codigo  = TConceptos.Cod
               Novedades_Nit.Nit     = Empleados.Nit.

     END.
     OPEN QUERY BDevengados FOR EACH Novedades_Nit WHERE Novedades_Nit.Nit EQ Empleados.Nit AND Novedades_Nit.Tip EQ 1.
/*     IF WMod THEN DO:
        ENABLE BROWSE BDevengados Novedades_Nit.Val.
        w_ok = BROWSE BDevengados:SELECT-ROW(WConDv + 1).
        APPLY "entry" TO Novedades_Nit.Val IN BROWSE BDevengados.
     END.
     HIDE FRAME FConceptos.*/
 END.
 IF WTipo EQ 2 THEN DO: /*adiciona concepto deduccion*/
     IF AVAILABLE TConceptos THEN DO:
        CREATE Novedades_Nit.
        ASSIGN Novedades_Nit.Agencia = Empleados.Agencia
               Novedades_Nit.Tipo    = TConceptos.Tip
               Novedades_Nit.Codigo  = TConceptos.Cod
               Novedades_Nit.Nit     = Empleados.Nit.

     END.
     OPEN QUERY BDeducciones FOR EACH Novedades_Nit WHERE Novedades_Nit.Nit EQ Empleados.Nit AND Novedades_Nit.Tip EQ 2.
     HIDE FRAME FConceptos.
 END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FMain
&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 wWin
ON CHOOSE OF BUTTON-4 IN FRAME FMain /* Button 4 */
DO:
  HIDE FRAME F_Relaciones.
  OPEN QUERY BInformativo FOR EACH HVEmpleados WHERE HvEmpleados.Nit EQ Empleados.Nit INDEXED-REPOSITION.
  HIDE FRAME FSS.
  HIDE FRAME FGeneral.
  HIDE FRAME FDotacion.
  HIDE FRAME FNovedades.
  VIEW FRAME FHV.
    OPEN QUERY BEmpleados FOR EACH Empleados.
    VIEW FRAME FConsulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FHV
&Scoped-define SELF-NAME CInformativos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CInformativos wWin
ON VALUE-CHANGED OF CInformativos IN FRAME FHV /* Conceptos Informativos */
DO:
  ASSIGN FRAME FHV CInformativos.
  IF SUBSTRING(CInformativos,1,5) EQ "00000" THEN
     OPEN QUERY BInformativo FOR EACH HVEmpleados WHERE 
                                      HvEmpleados.Nit EQ Empleados.Nit INDEXED-REPOSITION.
  ELSE
     OPEN QUERY BInformativo FOR EACH HVEmpleados WHERE 
                                   HvEmpleados.Codigo EQ INTEGER(SUBSTRING(CInformativos,1,5)) AND
                                   HvEmpleados.Nit EQ Empleados.Nit INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FIHV
&Scoped-define SELF-NAME CInformativosI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CInformativosI wWin
ON VALUE-CHANGED OF CInformativosI IN FRAME FIHV /* Conceptos */
DO:
  ASSIGN FRAME FHV CInformativos.
  IF SUBSTRING(CInformativos,1,5) EQ "00000" THEN
     OPEN QUERY BInformativo FOR EACH HVEmpleados WHERE 
                                      HvEmpleados.Nit EQ Empleados.Nit INDEXED-REPOSITION.
  ELSE
     OPEN QUERY BInformativo FOR EACH HVEmpleados WHERE 
                                   HvEmpleados.Codigo EQ INTEGER(SUBSTRING(CInformativos,1,5)) AND
                                   HvEmpleados.Nit EQ Empleados.Nit INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FConsulta
&Scoped-define SELF-NAME Cmb_Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Estado wWin
ON VALUE-CHANGED OF Cmb_Estado IN FRAME FConsulta /* Estado */
DO:
 ASSIGN FRAME FConsulta RCon WCon Cmb_Estado.
 APPLY "value-changed" TO RCon.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Cmb_Relaciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Relaciones wWin
ON VALUE-CHANGED OF Cmb_Relaciones IN FRAME F_Relaciones /* Consulta Relaciones */
DO:
  DO WITH FRAME F_Relaciones:
  FOR EACH T_Relaciones: DELETE T_Relaciones. END.
  CASE Cmb_Relaciones:SCREEN-VALUE:
  WHEN "99999 - Todas las Relaciones" THEN DO:
      FOR EACH Relaciones WHERE Relaciones.Nit    EQ Empleados.Nit:SCREEN-VALUE IN FRAME Fmain AND 
                                Relaciones.Estado EQ INTEGER(RActivas:SCREEN-VALUE) NO-LOCK 
                          BREAK BY Relaciones.Cod_Relacion:
          IF FIRST-OF(Relaciones.Cod_Relacion) THEN
             FIND Varios WHERE Varios.Tipo EQ 3 AND Varios.Codigo EQ Relaciones.Cod_Relacion NO-LOCK NO-ERROR.
          FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
          IF AVAILABLE(Clientes) THEN DO:
              CREATE T_Relaciones.
              UPDATE T_Relaciones.R_Relacion = Varios.Descripcion
                     T_Relaciones.R_AgeObjeto = Clientes.Agencia
                     T_Relaciones.R_NitObjeto = Relaciones.Nit_Relacion
                     T_Relaciones.R_NomObjeto = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
                     T_Relaciones.R_NomDescri = Relaciones.Descripcion
                     T_Relaciones.R_TelObjeto = Clientes.Tel_Comercial.
          END.
      END.
  END.
  WHEN "00000 - Ninguna Relacion" THEN DO:
      FOR EACH T_Relaciones: DELETE T_Relaciones. END.
  END.
  OTHERWISE DO:
      FIND Varios WHERE Varios.Tipo EQ 3 AND Varios.Codigo EQ INTEGER(SUBSTRING(Cmb_Relaciones:SCREEN-VALUE,1,5)) NO-LOCK NO-ERROR.
      FOR EACH Relaciones WHERE Relaciones.Nit EQ Empleados.Nit:SCREEN-VALUE IN FRAME FMain AND 
                                Relaciones.Cod_Relacion  EQ INTEGER(SUBSTRING(Cmb_Relaciones:SCREEN-VALUE,1,5)) AND 
                                Relaciones.Estado EQ INTEGER(RActivas:SCREEN-VALUE) NO-LOCK:
         FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
         IF AVAILABLE(Clientes) THEN DO:
            CREATE T_Relaciones.
            UPDATE T_Relaciones.R_Relacion = Varios.Descripcion
                   T_Relaciones.R_AgeObjeto = Clientes.Agencia
                   T_Relaciones.R_NitObjeto = Relaciones.Nit_Relacion
                   T_Relaciones.R_NomObjeto = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
                   T_Relaciones.R_NomDescri = Relaciones.Descripcion
                   T_Relaciones.R_TelObjeto = Clientes.Tel_Comercial.
         END.
       END.
  END.
  END CASE.
  OPEN QUERY BR_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.  
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FMain
&Scoped-define SELF-NAME C_Apellido1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C_Apellido1 wWin
ON LEAVE OF C_Apellido1 IN FRAME FMain
DO:
  SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME C_Apellido2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C_Apellido2 wWin
ON LEAVE OF C_Apellido2 IN FRAME FMain
DO:
  SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME C_Nombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C_Nombre wWin
ON LEAVE OF C_Nombre IN FRAME FMain
DO:
  SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FGeneral
&Scoped-define SELF-NAME Empleados.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Empleados.Estado wWin
ON VALUE-CHANGED OF Empleados.Estado IN FRAME FGeneral /* Estado */
DO:
  IF Empleados.Estado:SCREEN-VALUE IN FRAME FGeneral EQ "2" THEN
     VIEW FRAME FRetiro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Adicionar_Deduccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Adicionar_Deduccion wWin
ON CHOOSE OF MENU-ITEM m_Adicionar_Deduccion /* Adicionar Deduccion */
DO:
  WTipo = 2.
  OPEN QUERY BConceptos FOR EACH TConceptos WHERE TConceptos.Tip EQ WTipo.
  VIEW FRAME FConceptos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Adicionar_Devengado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Adicionar_Devengado wWin
ON CHOOSE OF MENU-ITEM m_Adicionar_Devengado /* Adicionar Devengado */
DO:
  WTipo = 1.
  OPEN QUERY BConceptos FOR EACH TConceptos WHERE TConceptos.Tip EQ WTipo.
  VIEW FRAME FConceptos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Eliminar_Concepto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Eliminar_Concepto wWin
ON CHOOSE OF MENU-ITEM m_Eliminar_Concepto /* Eliminar Concepto */
DO:
  FIND CURRENT HVEmpleados NO-ERROR.
  IF AVAILABLE HVEmpleados THEN
     DELETE HVEmpleados.
  APPLY "value-changed" TO CInformativos IN FRAME FHV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Eliminar_Devengado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Eliminar_Devengado wWin
ON CHOOSE OF MENU-ITEM m_Eliminar_Devengado /* Eliminar Devengado */
DO:
    FIND CURRENT Novedades_Nit.
    DELETE Novedades_Nit.
    OPEN QUERY BDevengados 
         FOR EACH Novedades_Nit WHERE 
                  Novedades_Nit.Nit EQ Empleados.Nit AND Novedades_Nit.Tipo EQ 1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Eliminiar_Deduccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Eliminiar_Deduccion wWin
ON CHOOSE OF MENU-ITEM m_Eliminiar_Deduccion /* Eliminiar Deduccion */
DO:
    FIND CURRENT Novedades_Nit.
    DELETE Novedades_Nit.
    OPEN QUERY BDeducciones 
         FOR EACH Novedades_Nit WHERE 
                  Novedades_Nit.Nit EQ Empleados.Nit AND Novedades_Nit.Tipo EQ 2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Imprimir_Hoja_de_Vida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Imprimir_Hoja_de_Vida wWin
ON CHOOSE OF MENU-ITEM m_Imprimir_Hoja_de_Vida /* Imprimir Hoja de Vida */
DO:

  Actual:SCREEN-VALUE IN FRAME FIHV = Empleados.Nit:SCREEN-VALUE IN FRAME FMain + " - " + 
        C_Nombre:SCREEN-VALUE IN FRAME FMain +
        C_Apellido1:SCREEN-VALUE IN FRAME Fmain +
        C_Apellido2:SCREEN-VALUE IN FRAME Fmain.
  VIEW FRAME FIHV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Ver_Detalle_de_la_Anotacin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Ver_Detalle_de_la_Anotacin wWin
ON CHOOSE OF MENU-ITEM m_Ver_Detalle_de_la_Anotacin /* Ver Detalle de la Anotación */
DO:
  W_NvaNota = NO.
  DO WITH FRAME FHV_Detalle:
     ASSIGN HVEmpleados.Descripcion:SCREEN-VALUE = HVEmpleados.Descripcion
            HVEmpleados.Valor:SCREEN-VALUE = STRING(HVEmpleados.Valor)
            HVEmpleados.FecIni:SCREEN-VALUE = STRING(HVEmpleados.FecIni)
            HVEmpleados.FecFin:SCREEN-VALUE = STRING(HVEmpleados.FecFin).
  END.
  VIEW FRAME FHV_Detalle.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FMain
&Scoped-define SELF-NAME Empleados.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Empleados.Nit wWin
ON LEAVE OF Empleados.Nit IN FRAME FMain /* Nit */
DO:
    IF Empleados.Nit:SCREEN-VALUE <> "" THEN DO:
        ENABLE {&List-5} WITH FRAME FMain.

        WNvoCliente = NO.

        FIND Clientes WHERE Clientes.Nit EQ Empleados.Nit:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE(Clientes) THEN DO:
            ASSIGN Empleados.Nit:SCREEN-VALUE = Clientes.Nit
                   C_Nombre:SCREEN-VALUE = Clientes.Nombre
                   C_Apellido1:SCREEN-VALUE = Clientes.Apellido1
                   C_Apellido2:SCREEN-VALUE = Clientes.Apellido2.

            RUN Mostrar_Cliente.
            DISABLE {&List-6} WITH FRAME Fmain.
        END.
        ELSE DO:
            MESSAGE "Como este numero de identificación aún" SKIP
                    "no se encuentra registrado en el archivo" SKIP
                    "de nits, se deberán ingresar los campos" SKIP
                    "de identificación del Empleado."
                VIEW-AS ALERT-BOX.

            DISABLE {&List-5} WITH FRAME FMain.
            WNvoCliente = YES.
            ENABLE {&List-6} WITH FRAME FMain.
            APPLY "entry" TO C_Nombre IN FRAME FMain.
            RETURN NO-APPLY.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME RActivas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RActivas wWin
ON VALUE-CHANGED OF RActivas IN FRAME F_Relaciones
DO:
  DO WITH FRAME F_Relaciones:
  IF RActivas:SCREEN-VALUE EQ "1" THEN
    Btn_Activas:LABEL = "Borrar".
  ELSE
    Btn_Activas:LABEL = "Activar".
  END.
  APPLY 'value-changed' TO Cmb_Relaciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FConsulta
&Scoped-define SELF-NAME RCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RCon wWin
ON VALUE-CHANGED OF RCon IN FRAME FConsulta
DO:
  ASSIGN FRAME FConsulta RCon Cmb_Estado.
  IF Cmb_Estado = "00 - Todos" THEN ASSIGN FE1 = 0 FE2 = 20.
  ELSE ASSIGN FE1 = INTEGER(SUBSTRING(Cmb_Estado,1,2)) FE2 = FE1.

  IF SELF:SCREEN-VALUE EQ "0" THEN DO:
      OPEN QUERY BEmpleados FOR EACH Empleados WHERE Empleados.Estado GE FE1 AND Empleados.Estado LE FE2.
      WCon:SCREEN-VALUE = "".
      DISABLE WCon.
  END.
  ELSE DO:
      ENABLE WCon WITH FRAME FConsulta.
      APPLY "entry" TO WCon.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FIHV
&Scoped-define SELF-NAME RIHV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RIHV wWin
ON VALUE-CHANGED OF RIHV IN FRAME FIHV
DO:
DO WITH FRAME FIHV:
    ASSIGN FRAME FIHV RIHV.
    IF RIHV EQ 1 THEN DISABLE CInformativosI.
    ELSE ENABLE CInformativosI.

END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RIHVEmpleado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RIHVEmpleado wWin
ON VALUE-CHANGED OF RIHVEmpleado IN FRAME FIHV
DO:
  ASSIGN FRAME FIHV RIHVEmpleado.
  IF RIHVEmpleado EQ 1 THEN
     Actual:SCREEN-VALUE IN FRAME FIHV = Empleados.Nit:SCREEN-VALUE IN FRAME FMain + " - " + 
     C_Nombre:SCREEN-VALUE IN FRAME FMain +
     C_Apellido1:SCREEN-VALUE IN FRAME FMain +
     C_Apellido2:SCREEN-VALUE IN FRAME FMain.
  ELSE
     Actual:SCREEN-VALUE IN FRAME FIHV = "Todos los empleados de esta Agencia".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FConsulta
&Scoped-define SELF-NAME WCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WCon wWin
ON LEAVE OF WCon IN FRAME FConsulta /* Consulta */
DO:
  ASSIGN FRAME FConsulta RCon wcon Cmb_Estado.

  IF Cmb_Estado = "00 - Todos" THEN ASSIGN FE1 = 0 FE2 = 20.
  ELSE ASSIGN FE1 = INTEGER(SUBSTRING(Cmb_Estado,1,2)) FE2 = FE1.
  CASE RCon:
      WHEN 0 THEN
        OPEN QUERY BEmpleados FOR EACH Empleados WHERE
                    Empleados.Estado GE FE1 AND
                    Empleados.Estado LE FE2.
      WHEN 1 THEN
        OPEN QUERY BEmpleados FOR EACH Empleados WHERE
                    Empleados.Agencia EQ INTEGER(WCon) AND
                    Empleados.Estado  GE FE1 AND
                    Empleados.Estado  LE FE2.
      WHEN 2 THEN
        OPEN QUERY BEmpleados FOR EACH Empleados WHERE
                    Empleados.Nit     EQ WCon AND
                    Empleados.Estado  GE FE1 AND
                    Empleados.Estado  LE FE2.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FMain
&Scoped-define BROWSE-NAME BConceptos
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
  DISPLAY C_Agencia C_Nombre C_Apellido1 C_Apellido2 
      WITH FRAME FMain IN WINDOW wWin.
  IF AVAILABLE Empleados THEN 
    DISPLAY Empleados.Fec_Ingreso Empleados.Fec_Retiro Empleados.Nit 
      WITH FRAME FMain IN WINDOW wWin.
  ENABLE RECT-2 RECT-3 FOTO Empleados.Fec_Ingreso C_Agencia Btn_Informacion 
         Btn_informe Empleados.Nit BUTTON-4 BtnHV BUTTON-18 Btn_SS BUTTON-17 
         BUTTON-15 BUTTON-201 Btn_Salvar Btn_Deshacer Btn_Ingresar Btn_Borrar 
         Btn_Cancelar Btn_Salir BUTTON-11 
      WITH FRAME FMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FMain}
  DISPLAY Cmb_Relaciones RActivas W_MenRel 
      WITH FRAME F_Relaciones IN WINDOW wWin.
  ENABLE Cmb_Relaciones RActivas Btn_CreRel Btn_Activas Br_Relaciones 
      WITH FRAME F_Relaciones IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Relaciones}
  DISPLAY C_Eps C_Pensiones C_Cesantias 
      WITH FRAME FSS IN WINDOW wWin.
  IF AVAILABLE Empleados THEN 
    DISPLAY Empleados.FecAF_EPS Empleados.FecAF_Pensiones 
          Empleados.FecAF_Cesantias 
      WITH FRAME FSS IN WINDOW wWin.
  ENABLE RECT-4 C_Eps Empleados.FecAF_EPS C_Pensiones Empleados.FecAF_Pensiones 
         C_Cesantias Empleados.FecAF_Cesantias 
      WITH FRAME FSS IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FSS}
  IF AVAILABLE Empleados THEN 
    DISPLAY Empleados.Fec_EntregaDotacion Empleados.Id_Dotacion 
          Empleados.Id_AuxilioDotacion Empleados.Id_PagaPorcentaje 
          Empleados.Fec_InicioPrestamo Empleados.Num_Camisas 
          Empleados.Talla_Camisas Empleados.Prestamo_Especial 
          Empleados.Num_Pantalones Empleados.Talla_Pantalon 
          Empleados.Plazo_Prestamo Empleados.Num_Chaquetas 
          Empleados.Talla_Chaquetas Empleados.Num_Corbatas 
          Empleados.Puntos_Adicionales 
      WITH FRAME FDotacion IN WINDOW wWin.
  ENABLE RECT-8 RECT-10 BUTTON-20 Empleados.Id_Dotacion 
         Empleados.Id_AuxilioDotacion Empleados.Id_PagaPorcentaje BUTTON-22 
         Empleados.Num_Camisas Empleados.Talla_Camisas Empleados.Num_Pantalones 
         Empleados.Talla_Pantalon Empleados.Num_Chaquetas 
         Empleados.Talla_Chaquetas Empleados.Num_Corbatas 
         Empleados.Puntos_Adicionales 
      WITH FRAME FDotacion IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FDotacion}

  {&OPEN-QUERY-FHV}
  GET FIRST FHV.
  DISPLAY RTV Cumplidos CInformativos 
      WITH FRAME FHV IN WINDOW wWin.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Dir_Residencia Clientes.Tel_Residencia Clientes.Estrato 
          Clientes.Celular Clientes.Email Clientes.Niv_Educativo 
          Clientes.Est_Civil Clientes.Fec_Nacimiento 
      WITH FRAME FHV IN WINDOW wWin.
  ENABLE RECT-11 RECT-303 BUTTON-190 Clientes.Dir_Residencia 
         Clientes.Tel_Residencia Clientes.Estrato RTV Clientes.Celular 
         Clientes.Email Clientes.Niv_Educativo Clientes.Est_Civil 
         Clientes.Fec_Nacimiento CInformativos BtnNvoInformativo BInformativo 
      WITH FRAME FHV IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FHV}
  DISPLAY C_Perfil 
      WITH FRAME FGeneral IN WINDOW wWin.
  IF AVAILABLE Empleados THEN 
    DISPLAY Empleados.Estado Empleados.Periodo_Pago Empleados.Jornada_Laboral 
          Empleados.Tipo_Contrato Empleados.Tiempo_Contrato Empleados.Id_Manejo 
          Empleados.Cargo Empleados.Salario_Mensual Empleados.PerNom_Actual 
      WITH FRAME FGeneral IN WINDOW wWin.
  ENABLE RECT-1 RECT-5 RECT-6 RECT-7 Empleados.Estado Empleados.Periodo_Pago 
         Empleados.Jornada_Laboral Empleados.Tipo_Contrato 
         Empleados.Tiempo_Contrato Empleados.Id_Manejo Empleados.Cargo C_Perfil 
         Empleados.Salario_Mensual 
      WITH FRAME FGeneral IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FGeneral}
  ENABLE BDevengados BDeducciones BUTTON-189 
      WITH FRAME FNovedades IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FNovedades}
  DISPLAY RCon WCon Cmb_Estado 
      WITH FRAME FConsulta IN WINDOW wWin.
  ENABLE RECT-305 BEmpleados BUTTON-21 RCon Cmb_Estado 
      WITH FRAME FConsulta IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FConsulta}
  ENABLE BConceptos BUTTON-38 BUTTON-37 
      WITH FRAME FConceptos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FConceptos}
  DISPLAY RCiao 
      WITH FRAME FRetiro IN WINDOW wWin.
  ENABLE RCiao BUTTON-207 
      WITH FRAME FRetiro IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FRetiro}
  DISPLAY RIHVEmpleado Actual RIHV CInformativosI WFecIni WFecFin 
      WITH FRAME FIHV IN WINDOW wWin.
  ENABLE RECT-302 RIHVEmpleado RIHV WFecIni BUTTON-199 WFecFin BUTTON-200 
      WITH FRAME FIHV IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FIHV}
  DISPLAY R_Nit R_Nombre R_Apellido1 R_Apellido2 R_Tel_Residencia R_Relacion 
      WITH FRAME FRelNva IN WINDOW wWin.
  ENABLE R_Nit R_Nombre R_Apellido1 R_Apellido2 R_Tel_Residencia R_Relacion 
         BUTTON-205 BUTTON-206 Btn_SC 
      WITH FRAME FRelNva IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FRelNva}

  {&OPEN-QUERY-FHV_Detalle}
  GET FIRST FHV_Detalle.
  IF AVAILABLE HVEmpleados THEN 
    DISPLAY HVEmpleados.FecInicial HVEmpleados.Descripcion HVEmpleados.FecFinal 
          HVEmpleados.Valor 
      WITH FRAME FHV_Detalle IN WINDOW wWin.
  ENABLE HVEmpleados.FecInicial HVEmpleados.Descripcion HVEmpleados.FecFinal 
         HVEmpleados.Valor BUTTON-196 BUTTON-197 
      WITH FRAME FHV_Detalle IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FHV_Detalle}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir wWin 
PROCEDURE Imprimir :
DEFINE VAR Listado AS CHARACTER INITIAL "".
  DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
  
  listado = W_PathSpl + "L_Empresas.Lst".
  {Incluido\Imprimir.i "Listado" Tamano}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Pantalla wWin 
PROCEDURE Inicializar_Pantalla :
DO WITH FRAME FMain:
   ASSIGN C_Agencia:SCREEN-VALUE               = C_Agencia:ENTRY(1)
          Empleados.Fec_Ingreso:SCREEN-VALUE   = STRING(TODAY)
          Empleados.Fec_Retiro:SCREEN-VALUE    = "?"
          Empleados.Nit:SCREEN-VALUE           = "".
END.
DO WITH FRAME FGeneral:
   ASSIGN Empleados.Estado:SCREEN-VALUE          = "1"
          Empleados.Periodo_Pago:SCREEN-VALUE    = "2"
          Empleados.Jornada_Laboral:SCREEN-VALUE = "1"
          Empleados.Tipo_Contrato:SCREEN-VALUE   = "1"
          Empleados.Tiempo_Contrato:SCREEN-VALUE = "0"
          Empleados.Salario_Mensual:SCREEN-VALUE = "0"
          Empleados.Cargo:SCREEN-VALUE           = ""
          C_Perfil:SCREEN-VALUE                  = C_Perfil:ENTRY(1)
          C_Nombre:SCREEN-VALUE                  = ""
          C_Apellido1:SCREEN-VALUE               = ""
          C_Apellido2:SCREEN-VALUE               = "".
END.
DO WITH FRAME FSS:
   ASSIGN C_EPS:SCREEN-VALUE                     = C_EPS:ENTRY(1)
          C_Pensiones:SCREEN-VALUE               = C_Pensiones:ENTRY(1)
          C_Cesantias:SCREEN-VALUE               = C_Cesantias:ENTRY(1)
          Empleados.FecAF_EPS:SCREEN-VALUE       = ""
          Empleados.FecAF_Cesantias:SCREEN-VALUE = ""
          Empleados.FecAF_Pensiones:SCREEN-VALUE = "".

END.
DO WITH FRAME FDotacion:
   ASSIGN Empleados.Id_Dotacion:SCREEN-VALUE         = "No"
          Empleados.Id_PagaPorcentaje:SCREEN-VALUE   = "No"
          Empleados.Fec_EntregaDotacion:SCREEN-VALUE = ""
          Empleados.Num_Camisas:SCREEN-VALUE         = "0"
          Empleados.Talla_Camisas:SCREEN-VALUE       = "0"
          Empleados.Id_AuxilioDotacion:SCREEN-VALUE  = "No"
          Empleados.Num_Pantalones:SCREEN-VALUE      = "0"
          Empleados.Talla_Pantalon:SCREEN-VALUE      = "0"
          Empleados.Num_Chaquetas:SCREEN-VALUE       = "0"
          Empleados.Talla_Chaquetas:SCREEN-VALUE     = "0"
          Empleados.Num_Corbatas:SCREEN-VALUE        = "0"
          Empleados.Fec_InicioPrestamo:SCREEN-VALUE  = ""
          Empleados.Prestamo_Especial:SCREEN-VALUE   = "0"
          Empleados.Plazo_Prestamo:SCREEN-VALUE      = "0".
END.
DO WITH FRAME FHV:
   ASSIGN Clientes.DIR_Residencia:SCREEN-VALUE = ""
          Clientes.Tel_Residencia:SCREEN-VALUE = ""
          Clientes.email:SCREEN-VALUE          = ""
          Clientes.Celular:SCREEN-VALUE        = ""
          Clientes.Fec_Nacimiento:SCREEN-VALUE = ""
          Clientes.Niv_Educativo:SCREEN-VALUE  = ""
          Clientes.Estrato:SCREEN-VALUE        = "1"
          Clientes.Est_Civil:SCREEN-VALUE      = "Soltero"
          Cumplidos:SCREEN-VALUE               = ""
          RTV:SCREEN-VALUE                     = "2"
          CInformativos:SCREEN-VALUE            = "00000 - Todos los Conceptos".
    APPLY "value changed" TO CInformativos IN FRAME FHV.
       
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
FOR EACH Cfg_Novedades WHERE Cfg_Novedades.Tipo LT 3 AND Cfg_Novedades.Estado EQ 1 AND
           Cfg_Novedades.Clase NE 3 AND Cfg_Novedades.Clase NE 6 NO-LOCK:
      CREATE TConceptos.
      ASSIGN TConceptos.Tip = Cfg_Novedades.Tipo
             TConceptos.Cod = Cfg_Novedades.Codigo
             TConceptos.Nom = Cfg_Novedades.Nombre.
  END.
  W_Ok = Cmb_Relaciones:ADD-LAST("00000 - Ninguna Relacion") IN FRAME F_Relaciones.
  W_Ok = Cmb_Relaciones:ADD-LAST("99999 - Todas las Relaciones") IN FRAME F_Relaciones.
  FOR EACH Varios WHERE Varios.Tipo EQ 3 AND 
           Varios.Codigo NE 11 NO-LOCK:
      W_Ok = Cmb_Relaciones:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion) IN FRAME F_Relaciones.
  END.

  FOR EACH Cfg_Novedades WHERE Cfg_Novedades.Tipo EQ 3 NO-LOCK:
      W_Ok = CInformativos:ADD-LAST(STRING(Cfg_Novedades.Codigo,"99999") + " - " + Cfg_Novedades.Nombre) IN FRAME FHV.
      W_Ok = CInformativosI:ADD-LAST(STRING(Cfg_Novedades.Codigo,"99999") + " - " + Cfg_Novedades.Nombre) IN FRAME FIHV.
  END.
  FOR EACH Agencias NO-LOCK:
      W_Ok = C_Agencia:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre) IN FRAME FMain.
  END.
  FOR EACH Varios WHERE Varios.Tipo EQ 31 NO-LOCK:
      W_Ok = C_Pensiones:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion) IN FRAME FSS.
  END.
  FOR EACH Varios WHERE Varios.Tipo EQ 32 NO-LOCK:
    W_Ok = C_Cesantias:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion) IN FRAME FSS.
  END.
  FOR EACH Varios WHERE Varios.Tipo EQ 33 NO-LOCK:
      W_Ok = C_EPS:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion) IN FRAME FSS.
  END.
  RUN SUPER.
  FIND FIRST Empleados  WHERE Empleados.Nit NE "" AND Empleados.Estado NE 2 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Empleados THEN DO: 
     RUN Inicializar_Pantalla.
     DISABLE C_Agencia Empleados.Nit WITH FRAME FMain.
     DISABLE ALL WITH FRAME FGeneral.
     DISABLE ALL WITH FRAME FSS.
     DISABLE ALL WITH FRAME FDotacion.
  END.
  ELSE DO: 
    RUN Mostrar_Empleados.
    OPEN QUERY BInformativo FOR EACH HVEmpleados WHERE HvEmpleados.Nit EQ Empleados.Nit INDEXED-REPOSITION.
    OPEN QUERY BR_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
  END.
  ASSIGN WFecIni = TODAY - DAY(TODAY)
         WFecFin = TODAY.
  DISPLAY WFecIni WFecFin WITH FRAME FIHV.
  OPEN QUERY BInformativo FOR EACH HVEmpleados WHERE 
                                   HvEmpleados.Nit EQ Empleados.Nit INDEXED-REPOSITION.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Cliente wWin 
PROCEDURE Mostrar_Cliente :
DO WITH FRAME FMain:
    ASSIGN C_Nombre:SCREEN-VALUE = Clientes.Nombre
           C_Apellido1:SCREEN-VALUE = Clientes.Apellido1
           C_Apellido2:SCREEN-VALUE = Clientes.Apellido2.
END.
DO WITH FRAME FHV:
  ASSIGN Clientes.DIR_Residencia:SCREEN-VALUE = Clientes.DIR_Residencia
         Clientes.Tel_Residencia:SCREEN-VALUE = Clientes.Tel_Residencia
         Clientes.email:SCREEN-VALUE          = Clientes.Email
         Clientes.Celular:SCREEN-VALUE        = STRING(Clientes.Celular)
         Clientes.Fec_Nacimiento:SCREEN-VALUE = STRING(Clientes.Fec_Nacimiento)
         Clientes.Niv_Educativo:SCREEN-VALUE  = Clientes.Niv_Educativo
         Clientes.Estrato:SCREEN-VALUE        = STRING(Clientes.Estrato)
         Clientes.Est_Civil:SCREEN-VALUE      = STRING(Clientes.Est_Civil)
         Cumplidos:SCREEN-VALUE               = STRING(DECIMAL(W_Fecha - Clientes.Fec_Nacimiento) / 365)
         RTV:SCREEN-VALUE                     = STRING(Clientes.Tipo_Vivienda).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Empleados wWin 
PROCEDURE Mostrar_Empleados :
DO WITH FRAME FMain:
   FIND Clientes WHERE Clientes.Nit EQ Empleados.Nit NO-LOCK NO-ERROR.
   W_Ok = Foto:LOAD-IMAGE("imagenes\fotos\0.jpg") NO-ERROR.
   IF AVAILABLE Clientes THEN DO:
       IF Clientes.Fotografia THEN DO:
           gTexto = SEARCH("imagenes\fotos\" + TRIM(Empleados.Nit:SCREEN-VALUE) + ".jpg").
           IF gTexto EQ ? THEN DO:
              MESSAGE "No ha sido capturada la fotografia" SKIP
                      "del cliente." VIEW-AS ALERT-BOX INFORMATION.
              W_Ok = Foto:LOAD-IMAGE("imagenes\fotos\0.jpg") NO-ERROR.
           END.
           ELSE 
              W_Ok = Foto:LOAD-IMAGE(gTexto) NO-ERROR.
       END.
       RUN Mostrar_Cliente.
   END.

   FIND Agencias WHERE Agencias.Agencia EQ Empleados.Agencia NO-LOCK NO-ERROR.
   IF AVAILABLE Agencias THEN
      C_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
   ELSE
      C_Agencia:SCREEN-VALUE = C_Agencia:ENTRY(1).
   DISPLAY Empleados.Fec_Ingreso Empleados.Fec_Retiro Empleados.Nit.
   FIND Clientes WHERE Clientes.Nit EQ Empleados.Nit NO-LOCK NO-ERROR.
   IF AVAILABLE Clientes THEN
      ASSIGN C_Nombre:SCREEN-VALUE    = Clientes.Nombre
             C_Apellido1:SCREEN-VALUE = Clientes.Apellido1
             C_Apellido2:SCREEN-VALUE = Clientes.Apellido2.
END.
DO WITH FRAME FGeneral:
   DISPLAY Empleados.Estado Empleados.Periodo_Pago Empleados.Jornada_Laboral
           Empleados.Tipo_Contrato Empleados.Tiempo_Contrato Empleados.Salario_Mensual
           Empleados.Cargo Empleados.Id_Manejo.
   FIND Perfiles_Nomina WHERE Perfiles_Nomina.Codigo EQ Empleados.Perfil_Cargo NO-LOCK NO-ERROR.
   IF AVAILABLE Perfiles_Nomina THEN
      C_Perfil:SCREEN-VALUE = STRING(Perfiles_Nomina.Codigo,"999") + " - " + Perfiles_Nomina.Nombre.
   ELSE
      C_Perfil:SCREEN-VALUE = C_Perfil:ENTRY(1).
END.
DO WITH FRAME FSS:
   FIND Varios WHERE Varios.Tipo   EQ 31 AND Varios.Codigo EQ Empleados.Fon_Pensiones NO-LOCK NO-ERROR.
   IF AVAILABLE Varios THEN C_Pensiones:SCREEN-VALUE = STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion.
   ELSE C_Pensiones:SCREEN-VALUE = C_Pensiones:ENTRY(1).

   FIND Varios WHERE Varios.Tipo   EQ 32 AND Varios.Codigo EQ Empleados.Fon_Cesantias NO-LOCK NO-ERROR.
   IF AVAILABLE Varios THEN C_Cesantias:SCREEN-VALUE = STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion.
   ELSE C_Cesantias:SCREEN-VALUE = C_Cesantias:ENTRY(1).

   FIND Varios WHERE Varios.Tipo   EQ 33 AND Varios.Codigo EQ Empleados.EPS NO-LOCK NO-ERROR.
   IF AVAILABLE Varios THEN C_EPS:SCREEN-VALUE = STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion.
   ELSE C_EPS:SCREEN-VALUE = C_EPS:ENTRY(1).


   DISPLAY Empleados.FecAF_EPS Empleados.FecAF_Cesantias Empleados.FecAF_Pensiones.
END.

DO WITH FRAME FDotacion:
   DISPLAY Empleados.Id_Dotacion Empleados.Id_PagaPorcentaje Empleados.Fec_EntregaDotacion
           Empleados.Num_Camisas Empleados.Talla_Camisas Empleados.Id_AuxilioDotacion
           Empleados.Num_Pantalones Empleados.Talla_Pantalon Empleados.Num_Chaquetas
           Empleados.Talla_Chaquetas Empleados.Num_Corbatas Empleados.Fec_InicioPrestamo
           Empleados.Prestamo_Especial Empleados.Plazo_Prestamo.
END.
DO WITH FRAME FNovedades:
  FIND CURRENT Empleados NO-LOCK.
  OPEN QUERY BDevengados FOR EACH Novedades_Nit 
                            WHERE Novedades_Nit.Agencia EQ Empleados.Agencia AND
                                  Novedades_Nit.Nit     EQ Empleados.Nit AND
                                  Novedades_Nit.Tipo    EQ 1.
  OPEN QUERY BDeducciones FOR EACH Novedades_Nit WHERE Novedades_Nit.Agencia EQ Empleados.Agencia AND
                                                       Novedades_Nit.Nit     EQ Empleados.Nit AND
                                                       Novedades_Nit.Tipo    EQ 2.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
DEFINE VAR W_NomEmp AS CHARACTER FORMAT "X(40)".
DEFINE VAR WEmpIni LIKE Clientes.Nit.
DEFINE VAR WEmpFin LIKE Clientes.Nit.
DEFINE VAR WConIni AS INTEGER FORMAT "99999".
DEFINE VAR WConFin AS INTEGER FORMAT "99999".
DEFINE VAR WLinea1 AS CHARACTER FORMAT "X(120)".
DEFINE VAR WLinea2 AS CHARACTER FORMAT "X(120)".
DEFINE VAR WLinea3 AS CHARACTER FORMAT "X(120)".
DEFINE VAR WLinea4 AS CHARACTER FORMAT "X(120)".

WRowid = ROWID(Empleados).

{INCLUIDO\RepEncabezado.I}    

  IF WTipInfo EQ 1 THEN DO:
      W_Reporte    = "REPORTE   : EMPLEADOS DE LA COOPERATIVA - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
      W_EncColumna = "Age Cedula       Nombre                                  Basico Mes    FecIngreso".
  END.
  IF WTipInfo EQ 2 THEN DO:
      W_Reporte    = "REPORTE   : HOJA DE VIDA DE LOS EMPLEADOS - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
      W_EncColumna = "Age Cedula       Nombre                                  Basico Mes    FecIngreso".
      IF RihvEmpleado EQ 1 THEN
         ASSIGN WEmpIni = Empleados.Nit WEmpFin = WEmpIni.
      ELSE
         ASSIGN WEmpIni = "00000000000000" WEmpFin = "999999999999".
      IF rihv EQ 1 THEN
         ASSIGN WConIni = 0 WConFin = 99999.
      ELSE
         ASSIGN WConIni = INTEGER(SUBSTRING(Cinformativosi,1,5)) WConFin = WConIni.
      
  END.
  DEFINE VAR Des AS CHARACTER FORMAT "X(10)".
    
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.

  IF WTipInfo EQ 1 THEN DO:
      FOR EACH Empleados WHERE Empleados.Estado EQ 1 BREAK BY Empleados.Agencia BY Empleados.Nit:
         FIND Clientes WHERE Clientes.Nit EQ Empleados.Nit NO-LOCK NO-ERROR.
         IF AVAILABLE Clientes THEN
            W_NomEmp = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
         DISPLAY Empleados.Agencia 
                 Empleados.Nit
                 W_NomEmp
                 Empleados.Salario_Mensual
                 Empleados.Fec_Ingreso FORMAT "99/99/9999"
                 Empleados.Cargo
         WITH FRAME F-empleados WIDTH 200 NO-LABELS STREAM-IO USE-TEXT NO-BOX.
      END.  
  END.
  IF WTipInfo EQ 2 THEN DO:
     FOR EACH HVEmpleados WHERE 
              HVEmpleados.Agencia    EQ INTEGER(SUBSTRING(C_Agencia:SCREEN-VALUE IN FRAME FMain,1,3)) AND
              HvEmpleados.Nit        GE WEmpIni AND
              HvEmpleados.Nit        LE WEmpFin AND
              HVEmpleados.Codigo     GE WConIni AND
              HvEmpleados.Codigo     LE WConFin AND
              HVEmpleados.FecInicial GE WFecIni AND
              HVEmpleados.FecInicial LE WFecFin NO-LOCK BREAK BY HVEmpleados.Nit BY HVEmpleados.Codigo BY HVEmpleados.FecInicial:
         IF FIRST-OF(HVEmpleados.Nit) THEN DO:
            FIND Clientes WHERE Clientes.Nit EQ HVEmpleados.Nit NO-LOCK NO-ERROR.
            IF AVAILABLE Clientes THEN
                W_NomEmp = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
             DISPLAY Empleados.Agencia 
                     Empleados.Nit
                     W_NomEmp
                     Empleados.Salario_Mensual
                     Empleados.Fec_Ingreso FORMAT "99/99/9999"
                     Empleados.Cargo SKIP
                     "----------------------------------------------------------------------------------------------------------"
             WITH FRAME F-Detalle2 WIDTH 200 NO-LABELS STREAM-IO USE-TEXT NO-BOX.
         END.
         IF FIRST-OF(HVEmpleados.Codigo) THEN DO:
            FIND Cfg_Novedades WHERE Cfg_Novedades.Codigo EQ HVEmpleados.Codigo NO-LOCK NO-ERROR.
            IF AVAILABLE Cfg_Novedades THEN 
                DISPLAY SKIP Cfg_Novedades.Codigo Cfg_Novedades.Nombre SKIP
                "----------------------------------------------------------------------------------------------------------"
                WITH FRAME F-Detalle3 WIDTH 200 NO-LABELS STREAM-IO USE-TEXT NO-BOX.
         END.
         ASSIGN WLinea1 = STRING(HVEmpleados.FecInicial) + " - " + SUBSTRING(HVEmpleados.Descripcion,1,90)
                WLinea2 = SUBSTRING(HVEmpleados.Descripcion,91,90)
                WLinea3 = SUBSTRING(HVEmpleados.Descripcion,181,90)
                WLinea4 = SUBSTRING(HVEmpleados.Descripcion,271,90).
         DISPLAY WLinea1 SKIP WLinea2 SKIP WLinea3
             WITH FRAME F-Mvto WIDTH 132 NO-LABELS STREAM-IO USE-TEXT NO-BOX.
     END.
  END.  
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

