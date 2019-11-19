DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR W_AG1 AS INTEGER FORMAT "999".
DEFINE VAR W_AG2 AS INTEGER FORMAT "999".
DEFINE VAR W_CC1 AS INTEGER FORMAT "999".
DEFINE VAR W_CC2 AS INTEGER FORMAT "999".
DEFINE VAR W_CB1 AS INTEGER FORMAT "99".
DEFINE VAR W_CB2 AS INTEGER FORMAT "99".
DEFINE VAR W_Pantalla AS WIDGET-HANDLE NO-UNDO.
DEFINE RECTANGLE W_CuadroFechas EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL SIZE 68 BY 2.
DEFINE RECTANGLE W_CuadroLimite EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL SIZE 68 BY 6.
DEFINE RECTANGLE W_CuadroOtros EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL SIZE 68 BY 4.
DEFINE BUTTON Btn_Informacion IMAGE-UP FILE  "imagenes\informacion.bmp" SIZE 10 BY 1.58 AUTO-GO.
DEFINE BUTTON Btn_Imprimir IMAGE-UP FILE  "imagenes\impresora2.bmp"  SIZE 10 BY 1.58 AUTO-GO.
DEFINE BUTTON Btn_Salir AUTO-GO LABEL "&Salir" SIZE 10 BY 1.58 FONT 5.
DEFINE BUTTON Btn_Ayuda IMAGE-UP FILE "imagenes\interrogacion.bmp" SIZE 4 BY 1.08 FONT 4.
DEFINE VAR Cmb_Agencia AS CHARACTER FORMAT "X(40)" VIEW-AS COMBO-BOX INNER-LINES 5 FONT 5 BGCOLOR 15 LABEL "Agencia".
DEFINE VAR Cmb_CenCost AS CHARACTER FORMAT "X(40)" VIEW-AS COMBO-BOX INNER-LINES 5 FONT 5 BGCOLOR 15 LABEL "Centro de Costos".
DEFINE VAR Cmb_Comprob AS CHARACTER FORMAT "X(40)" VIEW-AS COMBO-BOX INNER-LINES 5 FONT 5 BGCOLOR 15 LABEL "Comprobantes".
DEFINE VAR Cmb_Nivel AS INTEGER FORMAT "9" VIEW-AS COMBO-BOX LIST-ITEMS 1, 2, 3, 4, 5, 6, 7, 8 INNER-LINES 5 FONT 5 BGCOLOR 15 LABEL "Nivel" INITIAL 8.
DEFINE VAR W_Usuario1 AS CHARACTER LABEL "Usuario Inicial" BGCOLOR 15.
DEFINE VAR W_NomUsuario1 AS CHARACTER FORMAT "X(54)" BGCOLOR 18 FGCOLOR 15.
DEFINE VAR W_Usuario2 AS CHARACTER LABEL "Usuario Final" BGCOLOR 15.
DEFINE VAR W_NomUsuario2 AS CHARACTER FORMAT "X(54)" BGCOLOR 18 FGCOLOR 15.
DEFINE VAR W_Fec1 AS DATE FORMAT "99/99/9999" INITIAL TODAY BGCOLOR 15 LABEL "Fecha de Trabajo" VIEW-AS FILL-IN SIZE 10 BY .81.
DEFINE VAR W_Fec2 AS DATE FORMAT "99/99/9999" INITIAL TODAY BGCOLOR 15 LABEL "Fecha Final" VIEW-AS FILL-IN SIZE 10 BY .81.
DEFINE VAR W_Cuenta1 AS CHARACTER FORMAT "X(14)" LABEL "Cuenta Inicial" BGCOLOR 15.
DEFINE VAR W_NomCuenta1 AS CHARACTER FORMAT "X(40)" BGCOLOR 18 FGCOLOR 15.
DEFINE VAR W_Cuenta2 AS CHARACTER FORMAT "X(14)" LABEL "Cuenta Final" BGCOLOR 15.
DEFINE VAR W_NomCuenta2 AS CHARACTER FORMAT "X(40)" BGCOLOR 18 FGCOLOR 15.
DEFINE VAR W_Nit1 AS CHARACTER FORMAT "X(14)" LABEL "Nit Inicial" BGCOLOR 15.
DEFINE VAR W_NomNit1 AS CHARACTER FORMAT "X(40)" BGCOLOR 18 FGCOLOR 15.
DEFINE VAR W_Nit2 AS CHARACTER FORMAT "X(14)" LABEL "Nit Final" BGCOLOR 15.
DEFINE VAR W_NomNit2 AS CHARACTER FORMAT "X(40)" BGCOLOR 18 FGCOLOR 15.
DEFINE VAR W_Base AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99" LABEL "Valor de la Base" BGCOLOR 15 VIEW-AS FILL-IN SIZE 13 BY .81.
DEFINE VAR W_Porcentaje AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99" LABEL "Porcentaje" BGCOLOR 15 VIEW-AS FILL-IN SIZE 13 BY .81.
DEFINE VAR chkComparado AS LOGICAL LABEL "Comparado" VIEW-AS TOGGLE-BOX SIZE 11.57 BY .77 NO-UNDO.


DEFINE FRAME F_Valida
        Cmb_Agencia AT ROW 1.3 COL 26
        Cmb_CenCost AT ROW 2.6 COL 20
        Cmb_Comprob AT ROW 3.7 COL 21.8
        W_Usuario1 AT ROW 05 COL 22
        W_NomUsuario1 AT ROW 05 COL 39.5 NO-LABEL
        W_Usuario2 AT ROW 06 COL 22.6
        W_NomUsuario2 AT ROW 06 COL 39.5 NO-LABEL
        "  Escoja la fecha del Informe  "  AT ROW 7 COL 19 FONT 5 FGCOLOR 7
        W_Fec1 AT ROW 8.2 COL 20
        W_Fec2 AT ROW 8.2 COL 50
        chkComparado AT ROW 8.2 COL 72
        W_CuadroFechas AT ROW 7.5 COL 17
        "  Limites de Cuentas y Nits  " AT ROW 10.5 COL 19 FONT 5 FGCOLOR 7
        W_Cuenta1 AT ROW 12 COL 24
        W_NomCuenta1 AT ROW 12 COL 45.5 NO-LABEL
        W_Cuenta2 AT ROW 13 COL 24.5
        W_NomCuenta2 AT ROW 13 COL 45.5 NO-LABEL
        W_Nit1 AT ROW 14.5 COL 27
        W_NomNit1 AT ROW 14.5 COL 45.5 NO-LABEL
        W_Nit2 AT ROW 15.5 COL 27.5
        W_NomNit2 AT ROW 15.5 COL 45.5 NO-LABEL
        W_CuadroLimite AT ROW 10.9 COL 17
        "  Otros Parametros  " AT ROW 17.5 COL 19 FONT 5 FGCOLOR 7
        W_CuadroOtros AT ROW 18   COL 17
        Cmb_Nivel AT ROW 20.5 COL 62.3
        W_Base AT ROW 18.5 COL 55
        W_Porcentaje AT ROW 19.5 COL 58.6
        Btn_Informacion AT ROW 04 COL 102 HELP "Confirma la Entrada al Sistema"
        Btn_Imprimir AT ROW 16 COL 102
        Btn_Salir AT ROW 18 COL 102 HELP "Cancela la Entrada al Sistema" SKIP(0.5)
        Btn_Ayuda AT ROW 20 COL 105
    WITH SIZE 114 BY 22 SIDE-LABELS THREE-D KEEP-TAB-ORDER FONT 4 BGCOLOR 17 FGCOLOR 0 OVERLAY NO-UNDERLINE SCROLLABLE.

CREATE WINDOW W_Pantalla  
    ASSIGN HIDDEN = YES
    TITLE = "Informe: " + W_Informe
    HEIGHT = 23.15
    WIDTH = 115
    RESIZE = NO
    SCROLL-BARS = NO
    STATUS-AREA = YES
    MESSAGE-AREA = NO
    THREE-D = YES
    SENSITIVE = YES.

W_Ok = Cmb_Agencia:ADD-LAST("000 - Todas las Agencias").

FOR EACH Agencias NO-LOCK:
    W_Ok = Cmb_Agencia:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).

    IF W_Agencia = Agencias.Agencia THEN
        Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
END.

W_Ok = Cmb_Agencia:ADD-LAST("999 - Consolidado x Agencia").

FIND FIRST Entidad WHERE Entidad.Entidad = W_Entidad NO-LOCK NO-ERROR.
IF AVAILABLE(Entidad) THEN DO:
    IF NOT Entidad.Id_CenCosto THEN
        L_CC = NO.
END.

W_Ok = Cmb_CenCost:ADD-LAST("000 - Entidad No Maneja Cen.Costos").
Cmb_CenCost:SCREEN-VALUE IN FRAME F_Valida = "000 - Entidad No Maneja Cen.Costos".

IF L_CC THEN DO:
    W_Ok = Cmb_CenCost:ADD-LAST("000 - Todos los Centros de Costo").

    FOR EACH Cen_Costos WHERE Cen_Costos.Agencia = W_Agencia NO-LOCK:
        W_Ok = Cmb_CenCost:ADD-LAST(STRING(Cen_Costos.Cen_Costos,"999") + Cen_Costos.Nombre).
    END.

    Cmb_CenCost:SCREEN-VALUE IN FRAME F_Valida = "000 - Todos los Centros de Costo".
END.

W_Ok = Cmb_Comprob:ADD-LAST("00 - Todos los Comprobantes").

FOR EACH Comprobantes WHERE comprobantes.agencia = w_agencia
                        AND Comprobantes.Estado = 1 NO-LOCK:
    W_Ok = Cmb_Comprob:ADD-LAST(STRING(Comprobantes.Comprobante,"99") + " - " + Comprobantes.Nombre).
END.

Cmb_Comprob:SCREEN-VALUE IN FRAME F_Valida = "00 - Todos los Comprobantes".

W_Usuario1:SCREEN-VALUE = "".
W_Usuario2:SCREEN-VALUE = "".
W_NomUsuario1:SCREEN-VALUE = "Todos los Usuarios".
W_NomUsuario2:SCREEN-VALUE = "Todos los Usuarios".
W_Fec1:SCREEN-VALUE = STRING(TODAY).
W_Fec2:SCREEN-VALUE = STRING(TODAY).
Cmb_Nivel:SCREEN-VALUE = STRING(8).

ON RETURN TAB.
CURRENT-WINDOW = W_Pantalla.
VIEW W_Pantalla.


ON CHOOSE OF Btn_Informacion DO:
    RUN W-InfDia.R NO-ERROR.
END.


ON 'value-changed':U OF Cmb_Agencia DO:
    RUN asignarVariables.

    IF L_CC THEN DO:
        Cmb_CenCost:LIST-ITEMS = "".
        W_Ok = Cmb_CenCost:ADD-LAST("000 - Todos los Centros de Costo").

        FOR EACH Cen_Costos WHERE Cen_Costos.Agencia >= w_ag1
                              AND Cen_costos.agencia <= w_ag2 NO-LOCK:
            W_Ok = Cmb_CenCost:ADD-LAST(STRING(Cen_Costos.Cen_Costos,"999") + Cen_Costos.Nombre).
        END.

        Cmb_CenCost:SCREEN-VALUE IN FRAME F_Valida = "000 - Todos los Centros de Costo".
    END.
END.


ON 'leave':U OF W_Usuario1 DO:
    IF W_Usuario1:SCREEN-VALUE <> "" THEN DO:
        FIND FIRST Usuarios WHERE Usuarios.Agencia = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3))
                              AND Usuarios.Usuario = W_Usuario1:SCREEN-VALUE NO-LOCK NO-ERROR.
        W_NomUsuario1:SCREEN-VALUE = "A partir de este número".

        IF AVAILABLE(Usuarios) THEN
            W_NomUsuario1:SCREEN-VALUE = Usuarios.Nombre.
    END.
    ELSE
        W_NomUsuario1:SCREEN-VALUE = "Consolidado".
END.


ON 'leave':U OF W_Usuario2 DO:
    IF W_Usuario2:SCREEN-VALUE <> "" THEN DO:
        FIND FIRST Usuarios WHERE Usuarios.Agencia = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3))
                              AND Usuarios.Usuario = W_Usuario2:SCREEN-VALUE NO-LOCK NO-ERROR.
        W_NomUsuario2:SCREEN-VALUE = "Hasta este número".

        IF AVAILABLE(Usuarios) THEN
            W_NomUsuario2:SCREEN-VALUE = Usuarios.Nombre.
    END.
    ELSE
        W_NomUsuario2:SCREEN-VALUE = "Consolidado".
END.


ON 'mouse-select-dblclick':U OF W_Cuenta1 DO:
    DEFINE VAR W_Cta AS CHARACTER.
    DEFINE VAR W_Nom AS CHARACTER.

    IF W_Cuenta1:SCREEN-VALUE = "" THEN
        RUN Busca_Cuenta (INPUT W_Cuenta1:SCREEN-VALUE,
                          OUTPUT W_Cta,
                          OUTPUT W_Nom).

    W_Cuenta1:SCREEN-VALUE = W_Cta.
    W_NomCuenta1:SCREEN-VALUE = W_Nom.
END.


ON 'leave':U OF  W_Cuenta1 DO:
    IF W_Cuenta1:SCREEN-VALUE = "" THEN DO:
        W_NomCuenta1:SCREEN-VALUE = "Todas las Cuentas".
    END.
    ELSE DO:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta = W_Cuenta1:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE Cuentas THEN
            W_NomCuenta1:SCREEN-VALUE = Cuentas.Nombre.
    END.
END.


ON 'mouse-select-dblclick':U OF W_Cuenta2 DO:
    DEFINE VAR W_Cta AS CHARACTER.
    DEFINE VAR W_Nom AS CHARACTER.

    IF W_Cuenta2:SCREEN-VALUE = "" THEN
        RUN Busca_Cuenta (INPUT W_Cuenta2:SCREEN-VALUE,
                          OUTPUT W_Cta,
                          OUTPUT W_Nom).

    W_Cuenta2:SCREEN-VALUE = W_Cta.
    W_NomCuenta2:SCREEN-VALUE = W_Nom.
END.


ON 'leave':U OF  W_Cuenta2 DO:
    IF W_Cuenta2:SCREEN-VALUE = "" THEN DO:
        W_NomCuenta2:SCREEN-VALUE = "Todos las Cuentas".
    END.
    ELSE DO:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta = W_Cuenta2:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE Cuentas THEN
            W_NomCuenta2:SCREEN-VALUE = Cuentas.Nombre.
    END.
END.


ON 'leave':U OF W_Nit2 DO:
    IF W_Nit2:SCREEN-VALUE = "" THEN
        W_NomNit2:SCREEN-VALUE = "Todas los Nits".
END.


ON 'leave':U OF Cmb_Nivel DO:
    APPLY 'entry' TO Btn_Imprimir IN FRAME F_Valida.
    RETURN NO-APPLY.
END.


ON 'entry':U OF Btn_Imprimir DO:
    ON RETURN RETURN.
END.


ON 'leave':U OF Btn_Imprimir DO:
    APPLY 'focus' TO Cmb_Agencia.
END.


ON CHOOSE OF Btn_Imprimir DO:
    RUN asignarVariables.

    ASSIGN FRAME F_Valida
        W_Fec1
        W_Fec2
        W_Cuenta1
        W_Cuenta2
        W_Nit1
        W_Nit2
        W_Usuario1
        W_Usuario2
        W_Porcentaje
        Cmb_Nivel
        W_Base.

    IF W_Cuenta1 = "" AND W_Cuenta2 = "" THEN DO:
        W_Cuenta1 = "0".
        W_Cuenta2 = "99999999999999".
    END.
    
    DEFINE VAR zcontador AS INTEGER.
    DEFINE VAR zfin_age AS INTEGER.

    FIND LAST agencias NO-LOCK NO-ERROR.
    
    zfin_age = agencias.agencia.

    IF W_AG1 = 999 AND w_ag1 = w_ag2 THEN DO:
        REPEAT zcontador = 1 TO zfin_age:
            w_contodas = TRUE.

            FIND FIRST agencias WHERE agencias.agencia = zcontador NO-LOCK NO-ERROR.
            IF AVAILABLE(agencias) THEN DO:
                w_ag1 = zcontador.
                w_ag2 = zcontador.

                RUN Proceso_Imprimir.
            END.
        END.
    END.
    ELSE DO:
        w_contodas = FALSE.
        RUN Proceso_Imprimir.
    END.

    ON RETURN TAB.
END.

ON VALUE-CHANGED OF chkComparado DO:
    ASSIGN chkComparado.

    IF chkComparado = TRUE THEN DO:
        W_Fec2:SENSITIVE = TRUE.
        w_fec2:VISIBLE = TRUE.
        W_Fec1:LABEL = "Fecha inicial".
        W_Fec2:LABEL = "Fecha final".
    END.
    ELSE DO:
        DISABLE W_Fec2.
        w_fec2:VISIBLE = FALSE.
        W_Fec1:LABEL = "Fecha de corte".
    END.
END.



RUN Habilita_Deshabilita.

WAIT-FOR CHOOSE OF Btn_Salir FOCUS Cmb_Agencia.
HIDE FRAME F_Valida NO-PAUSE IN WINDOW W_Pantalla.
APPLY "CLOSE":U TO W_Pantalla.
DELETE WIDGET W_Pantalla.
  
