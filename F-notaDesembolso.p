{Incluido\VARIABLE.I "SHARED"}

RUN Rutinas.r PERSISTENT SET W_Manija.

DEFINE INPUT PARAMETER W_Cbt1 AS INTEGER.
DEFINE INPUT PARAMETER W_Doc1 AS INTEGER.
DEFINE INPUT PARAMETER W_Doc2 AS INTEGER.
DEFINE INPUT PARAMETER W_Ofi1 AS INTEGER.
DEFINE INPUT PARAMETER pFecContable AS DATE.

DEFINE TEMP-TABLE WCom
    FIELD WCom AS CHARACTER.

DEFINE VAR W_NCli AS CHARACTER FORMAT "X(15)".
DEFINE VAR W_Enlace AS CHARACTER FORMAT "X(10)".

/* oakley */

DEFINE VAR W_NCta AS CHARACTER FORMAT "X(15)".
DEFINE VAR W_Enc1 AS CHARACTER FORMAT "X(120)".
DEFINE VAR W_Enc2 AS CHARACTER FORMAT "X(120)".
DEFINE VAR W_Enc3 AS CHARACTER FORMAT "X(120)".
DEFINE VAR W_Enc4 AS CHARACTER FORMAT "X(120)".
DEFINE VAR W_Encabezado AS CHARACTER FORMAT "X(240)".
DEFINE VAR W_NroDoc AS INTEGER INITIAL 0.
DEFINE VAR W_NomEntidad AS CHARACTER FORMAT "X(30)".
DEFINE VAR W_NitEnti LIKE Clientes.Nit.
DEFINE VAR W_ConcatEnti AS CHARACTER FORMAT "X(57)".
DEFINE VAR W_NomCli AS CHARACTER FORMAT "X(30)".
DEFINE VAR W_PrimerCom AS CHARACTER FORMAT "X(100)" INITIAL "".
DEFINE VAR W_Comentario2 AS CHARACTER FORMAT "X(100)" INITIAL "".
DEFINE VAR W_Comentario AS CHARACTER FORMAT "X(100)" INITIAL "".
DEFINE VAR W_Nomofi AS  CHARACTER FORMAT "X(30)".
DEFINE VAR Movimiento_Debito AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR Movimiento_Credito AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR Total_Debito AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR Total_Credito AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR W_NomCbt LIKE Comprobantes.Nombre.
DEFINE VAR W_NomUsu LIKE Usuarios.Nombre.
DEFINE VAR W_Rpta AS LOGICAL.
DEFINE VAR Listado AS CHARACTER INITIAL "".

W_NroDoc = W_Doc2 - W_Doc1.

FIND Comprobantes WHERE Comprobantes.Comprobante = W_Cbt1
                    AND Comprobantes.Agencia = W_Ofi1 NO-LOCK NO-ERROR.
IF AVAILABLE(Comprobantes) THEN
    W_NomCbt = STRING(Comprobantes.Comprobante,"99") + " - " + Comprobantes.Nombre.

FIND LAST Mov_Contable WHERE Mov_Contable.Agencia EQ W_Ofi1
                         AND Mov_Contable.Comprobante EQ W_Cbt1
                         AND (Mov_Contable.Num_Documento GE W_Doc1 AND Mov_Contable.Num_Documento LE W_Doc2)
                         AND mov_contable.fec_contable = pFecContable NO-LOCK NO-ERROR.
IF NOT AVAILABLE(Mov_Contable) THEN DO:
    RUN MostrarMensaje IN W_Manija (INPUT 64,
                                    OUTPUT W_Rpta).
    RETURN.
END.

w_NomCli = "".

FIND FIRST Clientes WHERE Clientes.nit EQ Mov_contable.Nit NO-LOCK NO-ERROR.
IF AVAILABLE(Clientes) THEN
    W_Nomcli = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.

FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Mov_Contable.Usuario NO-LOCK NO-ERROR.
IF AVAILABLE(Usuarios) THEN
    W_NomUsu = Usuarios.Nombre.
ELSE
    W_NomUsu = "No encontro".

W_NomOfi = "".

FIND FIRST Agencias WHERE Agencias.Agencia = Mov_Contable.Agencia NO-LOCK NO-ERROR.
IF AVAILABLE(Agencias) THEN DO:
    W_NomOfi = Agencias.Nombre.

    FIND FIRST Entidad WHERE Entidad.Entidad EQ Agencia.Entidad NO-LOCK NO-ERROR.
    IF AVAILABLE(Entidad) THEN
        ASSIGN W_NomEntidad = Entidad.Nombre
               W_NitEnti = Entidad.Nit
               w_ConcatEnti = TRIM(W_NomEntidad) + "   Nit: " + w_NitEnti.
END.

DEFINE FRAME F-Encabezado HEADER
    W_ConcatEnti                AT COL  4 ROW 1

    /* oakley */

    "Comprobante de       :"    AT COL  4 ROW 2
    W_NomCbt                    AT COL 26 ROW 2
    "Fecha:"                    AT COL 93 ROW 2
    TODAY                       AT COL 99 ROW 2
    "Agencia Nro          :"    AT COL  4 ROW 3
    Mov_contable.Agencia        AT COL 26 ROW 3
    W_NomOfi                    AT COL 31 ROW 3
    "Hora :"                    AT COL 93 ROW 3 STRING(TIME,"HH:MM:SS") SKIP
    "Pagina:"                   AT COL 93 ROW 4 PAGE-NUMBER FORMAT ">9"
    "Documento Nro        :"    AT COL  4 ROW 4
    Mov_Contable.Num_documento  AT COL 26 ROW 4
    "Fecha Contabilizacion:"    AT COL  4 ROW 5
    Mov_Contable.Fec_Contable   AT COL 26 ROW 5
    "Usuario Digitador    :"    AT COL  4 ROW 6
    W_NomUsu                    AT COL 26 ROW 6
    "Titular              :"    AT COL  4 ROW 7
    W_NomCli                    AT COL 26 ROW 7 FORMAT "X(100)"
    "Encabezado           :"    AT COL  4 ROW 8
    W_Enc1                      AT COL 26 ROW 8
    W_Enc2                      AT COL 26 ROW 9
    W_Enc3                      AT COL 26 ROW 10
    W_Enc4                      AT COL 26 ROW 11
    "Cuenta"                    AT COL  5 ROW 12
    "C_C"                       AT COL 19 ROW 12
    "NomCuenta"                 AT COL 24 ROW 12
    "Nit"                       AT COL 37 ROW 12
    "Nombre"                    AT COL 52 ROW 12
    "Enlace"                    AT COL 70 ROW 12
    "Débito"                    AT COL 84 ROW 12
    "Crédito"                   AT COL 99 ROW 12 skip(1)
    WITH DOWN WIDTH 170 FRAME F-Encabezado NO-LABEL NO-BOX NO-UNDERLINE PAGE-TOP USE-TEXT.

DEFINE FRAME F_Movimiento
    Mov_Contable.Cuenta     AT 1   FORMAT "X(14)"
    mov_contable.cen_costos AT 15 FORMAT "999"
    W_NCta                  AT 20  FORMAT "X(13)"
    Mov_Contable.Nit        AT 34  FORMAT "X(14)"
    W_NCli                  AT 49  FORMAT "X(17)"
    W_Enlace                AT 67  FORMAT "X(6)"
    Movimiento_Debito       AT 74  FORMAT "->>,>>>,>>>,>>9.99"
    Movimiento_Credito      AT 93  FORMAT "->>,>>>,>>>,>>9.99"
    WITH 10 DOWN size 170 by 10 FRAME F_Movimiento USE-TEXT NO-BOX NO-LABEL STREAM-IO.

DEFINE FRAME F-Totales HEADER
    "Sumas Iguales: "               AT COL 55 ROW 1
    Total_Debito                    AT COL 69 ROW 1
    Total_Credito                   AT COL 87 ROW 1
    "Elaboró:______________________"    AT COL 2  ROW 5
    "Aprobó:______________________"     AT COL 47 ROW 5
    "Ordenó:______________________"     AT COL 92 ROW 5
    WITH WIDTH 170 FRAME F-Totales PAGE-BOTTOM NO-LABEL NO-BOX NO-UNDERLINE USE-TEXT.

Listado = W_pathspl + "Cpte-" + TRIM(string(W_Doc1)) + "-" + TRIM(W_Usuario) + STRING(TIME) + ".Txt".

DEFINE VAR W_Dispositivo AS CHARACTER INITIAL "P".

RUN P-DisPos IN W_Manija (INPUT-OUTPUT Listado, INPUT-OUTPUT W_Dispositivo).

IF W_Dispositivo = "" THEN
    RETURN.

OUTPUT TO VALUE(listado) NO-ECHO PAGED PAGE-SIZE 42.
    /* Inicio a armar Comentario */
    W_Comentario = "".
    W_Encabezado = "".
    
    IF W_NroDoc = 0 THEN DO:
        w_comentario = "Detalle Transacción: ".

        FOR EACH Mov_Contable FIELD (Agencia Comprobante Num_Documento DB CR Enlace Doc_Refer Cuenta Cen_Costos Comentario Nit Fec_contable)
            WHERE Mov_Contable.Agencia EQ W_Ofi1
              AND Mov_Contable.Comprobante EQ W_Cbt1
              AND (Mov_Contable.Num_Documento GE W_Doc1 AND Mov_Contable.Num_Documento LE W_Doc2)
              AND mov_contable.fec_contable = pFecContable NO-LOCK BREAK BY mov_contable.fec_contable
                                                                         BY Mov_contable.comprobante
                                                                         BY Mov_contable.Num_documento
                                                                         BY ROWID(Mov_Contable):
            FIND FIRST WCom WHERE WCom EQ Mov_Contable.Comentario NO-ERROR.
            IF NOT AVAILABLE WCom THEN DO:
                CREATE WCom.
                WCom.WCom = Mov_Contable.Comentario.
            END.

            IF LAST-OF(Mov_Contable.Num_Documento) THEN DO:
                FOR EACH WCom:
                    W_Encabezado = W_Encabezado + WCom.WCom + ".".
                END.

                ASSIGN W_Enc1 = SUBSTRING(W_Encabezado,1,120)
                       W_Enc2 = SUBSTRING(W_Encabezado,121,240)
                       W_Enc3 = SUBSTRING(W_Encabezado,241,360)
                       W_Enc4 = SUBSTRING(W_Encabezado,361,480).

                FOR EACH WCom:
                    DELETE WCom.
                END.

                LEAVE.
            END.
        END.
    END.
    /* Fin Armar Comentario */

    VIEW FRAME F-Encabezado.
    VIEW FRAME F-Totales.

    FOR EACH Mov_Contable FIELD (Agencia Comprobante Num_Documento DB CR Enlace Doc_Refer Cuenta Cen_Costos Comentario Nit Fec_contable)
        WHERE Mov_Contable.Agencia EQ W_Ofi1
          AND Mov_Contable.Comprobante EQ W_Cbt1
          AND (Mov_Contable.Num_Documento GE W_Doc1 AND Mov_Contable.Num_Documento LE W_Doc2)
          AND mov_contable.fec_contable = pFecContable NO-LOCK BREAK BY mov_contable.fec_contable
                                                                     BY Mov_contable.comprobante
                                                                     BY Mov_contable.Num_documento
                                                                     BY ROWID(Mov_Contable):
        IF FIRST-OF(Mov_Contable.Num_documento) THEN
            ASSIGN w_Comentario2 = ""
                   Total_Debito = 0
                   Total_Credito = 0
                   W_Comentario2 = "Detalle Transaccion: "
                   W_Comentario2 = W_Comentario2 + TRIM(Mov_Contable.Comentario) + ", "
                   W_PrimerCom = TRIM(Mov_Contable.Comentario).

        FIND FIRST cuentas WHERE cuentas.cuenta = mov_contable.cuenta NO-LOCK NO-ERROR.
        IF AVAILABLE cuentas THEN DO:
            IF cuentas.id_cuenta = 5 THEN
                NEXT.
        END.

        IF Mov_Contable.DB GT 0 THEN
            ASSIGN Movimiento_Debito = Mov_Contable.DB
                   Movimiento_Credito = 0
                   Total_Debito = Total_Debito + Mov_Contable.DB.
        ELSE
            ASSIGN Movimiento_Credito = Mov_Contable.CR
                   Movimiento_Debito  = 0
                   Total_Credito = Total_Credito + Mov_Contable.CR.

        IF TRIM(Mov_Contable.Comentario) NE W_PrimerCom THEN
            W_Comentario2 = W_Comentario2 + TRIM(Mov_Contable.Comentario) + " ".

        W_NCli = "".

        IF Mov_Contable.Nit NE "" THEN DO:
            FIND FIRST Clientes WHERE Clientes.Nit EQ Mov_Contable.Nit NO-LOCK NO-ERROR.
            IF AVAILABLE Clientes THEN
                W_NCli = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
        END.

        W_NCta = "".

        FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Mov_Contable.Cuenta NO-LOCK NO-ERROR.
        IF AVAILABLE Cuentas THEN
            W_NCta = Cuentas.Nombre.

        W_Enlace = Mov_Contable.Doc_Refer.

        DISPLAY Mov_Contable.Cuenta
                mov_contable.cen_costos
                W_NCta
                Mov_contable.Nit
                W_NCli
                W_Enlace
                Movimiento_Debito
                Movimiento_Credito
            WITH FRAME F_Movimiento.

        IF LAST-OF(mov_contable.fec_contable) THEN
            LEAVE.
    END.
OUTPUT CLOSE.

IF W_Dispositivo = "P" THEN
    RUN Pantalla2 IN W_Manija (INPUT Listado).

IF W_Dispositivo = "I" THEN
    RUN _osprint.r (INPUT ?,
                    INPUT listado,
                    INPUT 8,
                    INPUT 1,
                    INPUT 1,
                    INPUT 99999,
                    OUTPUT W_Rpta).
ELSE
    IF W_Dispositivo <> "A" THEN
        OS-DELETE VALUE(LISTADO).
