DEFINE VARIABLE W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Movimiento Contable" NO-UNDO.

{incluido/Variable.i "SHARED"}.
{incluido/Varcon.i "SHARED"}.

DEFINE VAR L_CC AS LOGICAL INITIAL YES.
DEFINE VAR Nom_Cliente AS CHARACTER FORMAT "X(18)".

DEFINE TEMP-TABLE tmp_mvto LIKE mov_contable.

{incluido/Pantalla_Validacion2.i}

ON RETURN TAB.

VIEW W_Pantalla ACTIVE-WINDOW.

RELEASE usuarios.

ON CHOOSE OF Btn_Informacion DO:
    RUN W-InfDia.R NO-ERROR.
END.

ON 'value-changed':U OF Cmb_Agencia DO:

    IF L_CC THEN DO:
        Cmb_CenCost:LIST-ITEMS = "".

        W_Ok = Cmb_CenCost:ADD-LAST("000 - Todos los Centros de Costo").

        FOR EACH Cen_Costos WHERE Cen_Costos.Agencia = W_Agencia NO-LOCK:
            W_Ok = Cmb_CenCost:ADD-LAST(STRING(Cen_Costos.Cen_Costos,"999") + " - " + Cen_Costos.Nombre).
        END.

        Cmb_CenCost:SCREEN-VALUE IN FRAME F_Valida = "000 - Todos los Centros de Costo".
    END.
END.

ON 'leave':U OF W_Usuario1 DO:
    IF W_Usuario1:SCREEN-VALUE <> "" THEN DO:
        FIND FIRST Usuarios WHERE Usuarios.Usuario = W_Usuario1:SCREEN-VALUE NO-LOCK NO-ERROR.
        
        IF AVAILABLE(Usuarios) THEN
            W_NomUsuario1:SCREEN-VALUE = Usuarios.Nombre.
    END.
    ELSE
        W_NomUsuario1:SCREEN-VALUE = "Todos los Usuarios".
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
    IF W_Cuenta1:SCREEN-VALUE = "" THEN
        ASSIGN W_Cuenta1:SCREEN-VALUE = "0"

        /* oakley */

               W_NomCuenta1:SCREEN-VALUE = "Todas las Cuentas".
    ELSE DO:
        FIND FIRST cuentas WHERE cuentas.cuenta = w_cuenta1:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE cuentas THEN
            W_NomCuenta1:SCREEN-VALUE = cuentas.nombre.
        ELSE
            W_NomCuenta1:SCREEN-VALUE = "".
    END.
END.

ON 'mouse-select-dblclick':U OF W_Cuenta2 DO:
    DEFINE VAR W_Cta LIKE Cuentas.Cuenta NO-UNDO.
    DEFINE VAR W_Nom LIKE Cuentas.Nombre NO-UNDO.

    IF W_Cuenta2:SCREEN-VALUE EQ "" THEN
        RUN Busca_Cuenta (INPUT W_Cuenta2:SCREEN-VALUE,
                          OUTPUT W_Cta,
                          OUTPUT W_Nom).

    ASSIGN W_Cuenta2:SCREEN-VALUE = W_Cta
           W_NomCuenta2:SCREEN-VALUE = W_Nom.
END.

ON 'leave':U OF  W_Cuenta2 DO:
    IF W_Cuenta2:SCREEN-VALUE EQ "" THEN
        ASSIGN W_Cuenta2:SCREEN-VALUE = "99999999999999"
               W_NomCuenta2:SCREEN-VALUE = "Todas las Cuentas".
    ELSE DO:
        FIND FIRST cuentas WHERE cuentas.cuenta = w_cuenta2:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE cuentas THEN
            W_NomCuenta2:SCREEN-VALUE = cuentas.nombre.
        ELSE
            W_NomCuenta2:SCREEN-VALUE = "".
    END.
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

ON 'Entry':U OF W_Usuario1 DO:
    /*Disdponible*/
END.

ON 'Entry':U OF W_Fec1 DO:
    APPLY "FOCUS" TO SELF.
    RELEASE usuarios.

    /* APPLY "Entry" TO W_Usuario1.
    RETURN NO-APPLY.*/
END.

ON CHOOSE OF Btn_TotCta DO:
    W_TotCta = TRUE.
    APPLY "CHOOSE" TO Btn_Imprimir.
END.

ON CHOOSE OF Btn_TotComp DO:
    W_TotComp = TRUE.
    APPLY "CHOOSE" TO Btn_Imprimir.
END.

ON CHOOSE OF Btn_Imprimir DO:
    ASSIGN W_CC1 = INTEGER(SUBSTRING(Cmb_CenCost:SCREEN-VALUE,1,3))
           W_CC2 = INTEGER(SUBSTRING(Cmb_CenCost:SCREEN-VALUE,1,3))
           W_AG1 = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3))
           W_AG2 = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3))
           W_CB1 = INTEGER(SUBSTRING(Cmb_Comprob:SCREEN-VALUE,1,3))
           W_CB2 = INTEGER(SUBSTRING(Cmb_Comprob:SCREEN-VALUE,1,3))
           W_Ordenar.

    IF SUBSTRING(Cmb_CenCost:SCREEN-VALUE,1,3) EQ "000" THEN
        ASSIGN W_CC1 = 0
               W_CC2 = 999.

    IF SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3) EQ "000" THEN
        ASSIGN W_AG1 = 0
               W_AG2 = 999.

    IF SUBSTRING(Cmb_Comprob:SCREEN-VALUE,1,3) EQ "000" THEN
        ASSIGN W_CB1 = 0
               W_CB2 = 999.

    ASSIGN FRAME F_Valida
        W_Fec1
        W_Fec2
        W_Cuenta1
        W_Cuenta2
        W_Nit1
        W_Usuario1
        W_Porcentaje
        Cmb_Nivel
        W_Base.

    IF W_Cuenta1 EQ "" AND W_Cuenta2 EQ "" THEN
        ASSIGN W_Cuenta1 = "0"
               W_Cuenta2 = "99999999999999".

    RUN Proceso_Imprimir.

    W_TotCta = FALSE.

    ON RETURN TAB.
END.

RUN Habilita_Deshabilita.

RELEASE usuarios.
APPLY "ENTRY" TO W_Fec1.
WAIT-FOR CHOOSE OF Btn_Salir FOCUS W_Fec1.    /*Cmb_Agencia.*/
RELEASE usuarios.

ON CURSOR-DOWN CURSOR-DOWN.
ON RETURN RETURN.
HIDE FRAME F_Valida NO-PAUSE IN WINDOW W_Pantalla.
DELETE WIDGET W_Pantalla.
RELEASE usuarios.

/* fin incluido Pantalla parametros */

PROCEDURE Habilita_Deshabilita:
    /* En este procedimiento se habilitan o deshabilitan las variables a pedir en pantalla segun el informe que se vaya a ejecutar. */
    ENABLE ALL WITH FRAME F_Valida IN WINDOW W_Pantalla.

    DISABLE W_NomUsuario1
            W_NomCuenta1
            W_NomCuenta2
            W_NomNit1
            W_Nit2
            W_NomNit2
            Cmb_Nivel
            W_Base
            W_Porcentaje
        WITH FRAME F_Valida.

    IF NOT L_CC THEN /* valida si la entidad maneja centros de costos */
        DISABLE Cmb_CenCost WITH FRAME F_Valida.

    w_nit1:LABEL = "Identificación".
    w_nit2:VISIBLE = FALSE.
    W_NomNit2:VISIBLE = FALSE.
    W_usuario2:VISIBLE = FALSE.
    W_NomUsuario2:VISIBLE = FALSE.

END PROCEDURE.

PROCEDURE Proceso_Imprimir:
    DEFINE VAR Listado AS CHARACTER INITIAL "" NO-UNDO.
    Listado = W_Pathspl + "MDia" + STRING(W_Usuario,"X(4)") + STRING(TIME,"999999") + ".Txt".
    {incluido\IMPRIMIR_informe.I "listado"}.
END PROCEDURE.

PROCEDURE ProcesoImprimir:
    {Incluido\RepEncabezado.i}

    /*DEFINE VAR W_EstadoInf AS CHARACTER FORMAT "X(8)" INITIAL "" NO-UNDO.*/
    /*DEFINE VAR Nom_Cencosto AS CHARACTER FORMAT "X(2)" INITIAL "" NO-UNDO.*/

    IF W_Fec1 <> W_Fec2 AND W_Fec1 <= W_Fec2 THEN
        W_Reporte = "REPORTE   : MOVIMIENTO CONTABLE - del " + STRING(W_Fec1) + " al " + STRING(W_Fec2) + " - Agencia: " + STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(25)") + " - "+ STRING(TIME,"hh:mm am").
    ELSE
        W_Reporte = "REPORTE   : MOVIMIENTO CONTABLE - Al " + STRING(W_Fec2) + " - Agencia: " + STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(25)") + " - "+ STRING(TIME,"hh:mm am").

    W_EncColumna = "AGE NUM.DOC  CUENTA-CC         COMENTARIO           NIT            NOMBRE CLIENTE      DOC.REF          DEBITO          CREDITO   USU".

    IF W_Ordenar = 2 THEN
        W_EncColumna = "AGE COMP NUM.DOC CUENTA-CC          COMENTARIO           NIT            NOMBRE CLIENTE      DOC.REF          DEBITO          CREDITO   USU".

    DEFINE VAR TT_Db AS DECIMAL FORMAT "->,>>>,>>>,>>9".
    DEFINE VAR TT_Cr AS DECIMAL FORMAT "->,>>>,>>>,>>9".
    DEFINE VAR TC_Db AS DECIMAL FORMAT "->,>>>,>>>,>>9".
    DEFINE VAR TC_Cr AS DECIMAL FORMAT "->,>>>,>>>,>>9".
    /*DEFINE VAR W_Cta AS CHARACTER FORMAT "X(14)".*/
    /*DEFINE VAR W_Nom AS CHARACTER FORMAT "X(20)".*/
    DEFINE VAR vCuenta AS CHARACTER.
    DEFINE VAR fecCont AS DATE.
    
    /* Variables para el manejo de los filtros */
    DEFINE VAR comprobante_ini AS INTEGER INITIAL 0.
    DEFINE VAR comprobante_fin AS INTEGER INITIAL 999.
    DEFINE VAR usuario_ini AS CHARACTER INITIAL ''.
    DEFINE VAR usuario_fin AS CHARACTER INITIAL 'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzz'.
   
    
    DEFINE FRAME F_Mov
        WITH DOWN FRAME F_Mov WIDTH 150 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.

    VIEW FRAME F-Encabezado.
    VIEW FRAME f-ftr.

    EMPTY TEMP-TABLE tmp_mvto.

    IF w_nit1 = '' THEN
        w_nit2 = "999999999999999".
    ELSE
        w_nit2 = w_nit1.

    IF w_usuario1 <> '' THEN DO:
        usuario_ini = w_usuario1.
        usuario_ini = w_usuario1.
    END.

    FOR EACH agencias WHERE agencias.agencia >= W_Ag1
                        AND agencias.agencia <= W_Ag2 NO-LOCK:
        FOR EACH mov_contable WHERE mov_contable.agencia = agencias.agencia
                                AND mov_contable.fec_contable >= w_fec1
                                AND mov_contable.fec_contable <= w_fec2
                                AND mov_contable.cuenta >= w_cuenta1
                                AND mov_contable.cuenta <= w_cuenta2 NO-LOCK:
            IF mov_contable.comprobante < w_cb1 OR mov_contable.comprobante > w_cb2 THEN
                NEXT.

            IF mov_contable.cen_costos < w_cc1 OR mov_contable.cen_costos > w_cc2 THEN
                NEXT.

            IF w_usuario1 <> "" AND mov_contable.usuario <> w_usuario1 THEN
                NEXT.

            IF w_nit1 <> "" AND mov_contable.nit <> w_nit1 THEN
                NEXT.

            CREATE tmp_mvto.
            BUFFER-COPY mov_contable TO tmp_mvto.
        END.

        FOR EACH mov_contable2 WHERE mov_contable2.agencia = agencias.agencia
                                 AND mov_contable2.comprobante >= w_cb1
                                 AND mov_contable2.comprobante <= w_cb2
                                 AND mov_contable2.cen_costos >= w_cc1
                                 AND mov_contable2.cen_costos <= w_cc2
                                 AND mov_contable2.fec_contable >= w_fec1
                                 AND mov_contable2.fec_contable <= w_fec2
                                 AND mov_contable2.cuenta >= w_cuenta1
                                 AND mov_contable2.cuenta <= w_cuenta2 NO-LOCK:
            IF w_usuario1 <> "" AND mov_contable2.usuario <> w_usuario1 THEN
                NEXT.

            IF w_nit1 <> "" AND mov_contable2.cliente_id <> w_nit1 THEN
                NEXT.

            CREATE tmp_mvto.
            BUFFER-COPY mov_contable2 TO tmp_mvto.
            tmp_mvto.nit = mov_contable2.cliente_id.
        END.
    END.



    

    IF W_TotCta THEN DO:
        IF W_Usuario1 <> "" THEN
            W_EncColumna = "        Resumen Detallado - Movimiento por Cuenta   Usuario "  + trim(W_Usuario1).
        ELSE
            W_EncColumna = "        Resumen Detallado - Movimiento por Cuenta   de todos los Usuarios".

        RUN TotxCta.
    END.
    ELSE
        IF W_TotComp THEN DO:
            IF W_Usuario1 <> "" THEN
                W_EncColumna = "        Resumen Totalizado por Cuenta y Comprobante   Usuario "  + trim(W_Usuario1).
            ELSE
                W_EncColumna = "        Resumen Totalizado por Cuenta y Comprobante   de todos los Usuarios".

            RUN TotxComp.
        END.
        ELSE DO:
            IF W_Ordenar = 1 THEN DO:
                /* EMPIEZA EL INFORME  POR IMPRESORA */
                FOR EACH TMP_MVTO BREAK BY TMP_MVTO.Comprobante
                                        BY TMP_MVTO.Fec_Contable
                                        BY TMP_MVTO.Agencia
                                        BY TMP_MVTO.Num_Documento:
                    vCuenta = tmp_Mvto.cuenta.

                    IF tmp_Mvto.cen_costos <> 999 AND tmp_Mvto.cen_costos <> 0 THEN
                        vCuenta = vCuenta + "-" + STRING(tmp_Mvto.cen_costos,"999").

                    IF FIRST-OF(tmp_mvto.Comprobante) THEN DO:
                        DISPLAY "Comprobante: " AT 1
                                tmp_mvto.Comprobante AT 15
                            WITH FRAME F_Com WIDTH 150 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.
                    END.

                    IF FIRST-OF(tmp_mvto.Fec_Contable) THEN DO:
                        DISPLAY SKIP(1)
                                "Fecha Contable: " AT 1
                                tmp_mvto.Fec_Contable AT 18 SKIP(1)
                            WITH FRAME F_Fec WIDTH 150 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.
                    END.

                    Nom_Cliente = "".

                    FIND FIRST Clientes WHERE Clientes.Nit EQ tmp_mvto.Nit NO-LOCK NO-ERROR.
                    IF AVAILABLE Clientes THEN
                        Nom_Cliente = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.

                    DISPLAY tmp_mvto.Agencia    AT 1 FORMAT "99"
                            tmp_mvto.Num_Documento  AT 4 FORMAT "99999999"
                            vCuenta                 AT 13 FORMAT "X(18)"
                            tmp_mvto.Comentario     AT 32 FORMAT "X(20)" 
                            tmp_mvto.Nit            AT 53  FORMAT "X(14)" 
                            Nom_Cliente             AT 68  FORMAT "X(19)"
                            tmp_mvto.Doc_Referencia AT 88  FORMAT "X(7)"
                            tmp_mvto.Db             AT 98  FORMAT "->,>>>,>>>,>>9"
                            tmp_mvto.Cr             AT 114 FORMAT "->,>>>,>>>,>>9"
                            tmp_mvto.Usuario        AT 131 FORMAT "X(4)"
                        WITH DOWN FRAME F_Otro WIDTH 150 USE-TEXT NO-BOX STREAM-IO NO-UNDERLINE NO-LABELS.

                    ASSIGN TT_Db = TT_Db + tmp_mvto.Db
                           TT_Cr = TT_Cr + tmp_mvto.Cr
                           TC_Db = TC_Db + tmp_mvto.Db
                           TC_Cr = TC_Cr + tmp_mvto.Cr.

                    IF LAST-OF(tmp_mvto.Num_Documento) THEN DO:
                        DISPLAY "____________________________________" AT 94 SKIP(1)
                                TT_Db AT 98
                                TT_Cr AT 114 SKIP(0)
                            WITH DOWN FRAME F_Tot WIDTH 150 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.

                        ASSIGN TT_Db = 0
                               TT_Cr = 0.
                    END.

                    IF LAST-OF(tmp_mvto.Comprobante) THEN DO:
                        DISPLAY "Total Comprobante: "                  AT 44
                                tmp_mvto.Comprobante               AT 64
                                "____________________________________" AT 94 SKIP(1)
                                TC_Db AT 98
                                TC_Cr AT 114 SKIP(1)
                            WITH DOWN FRAME F_TotCom WIDTH 150 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.

                        ASSIGN TC_Db = 0
                               TC_Cr = 0.
                    END.
                END. /* WHERE */ 
            END.

            IF W_Ordenar = 2 THEN DO:
                FOR EACH TMP_MVTO BREAK BY TMP_MVTO.Agencia
                                        BY TMP_MVTO.Fec_Contable
                                        BY tmp_mvto.fec_grabacion
                                        BY TMP_MVTO.hora
                                        BY tmp_mvto.num_documento:
                    vCuenta = tmp_Mvto.cuenta.

                    IF tmp_Mvto.cen_costos <> 999 AND tmp_Mvto.cen_costos <> 0 THEN
                        vCuenta = vCuenta + "-" + STRING(tmp_Mvto.cen_costos,"999").

                    IF FIRST-OF(tmp_mvto.Fec_Contable) THEN DO:
                        DISPLAY SKIP(1) "Fecha Contable: " AT 1
                                tmp_mvto.Fec_Contable AT 18 SKIP(1)
                            WITH FRAME F_Fec2 WIDTH 150 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.
                    END.

                    Nom_Cliente = "".
            
                    FIND FIRST Clientes WHERE Clientes.Nit EQ tmp_mvto.Nit NO-LOCK NO-ERROR.
                    IF AVAILABLE Clientes THEN
                        Nom_Cliente = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.

                    DISPLAY tmp_mvto.Agencia        AT 1 FORMAT "99"
                            tmp_mvto.comprobante    AT 4 FORMAT ">999"
                            tmp_mvto.Num_Documento  AT 9 FORMAT "99999999"
                            vCuenta                 AT 18 FORMAT "X(18)"
                            tmp_mvto.Comentario     AT 37 FORMAT "X(20)" 
                            tmp_mvto.Nit            AT 58  FORMAT "X(14)" 
                            Nom_Cliente             AT 73  FORMAT "X(19)"
                            tmp_mvto.Doc_Referencia AT 93  FORMAT "X(7)"
                            tmp_mvto.Db             AT 103  FORMAT "->,>>>,>>>,>>9"
                            tmp_mvto.Cr             AT 119 FORMAT "->,>>>,>>>,>>9"
                            tmp_mvto.Usuario        AT 136 FORMAT "X(4)"
                        WITH DOWN FRAME F_Otro2 WIDTH 150 USE-TEXT NO-BOX STREAM-IO NO-UNDERLINE NO-LABELS.

                    ASSIGN TT_Db = TT_Db + tmp_mvto.Db
                           TT_Cr = TT_Cr + tmp_mvto.Cr
                           TC_Db = TC_Db + tmp_mvto.Db
                           TC_Cr = TC_Cr + tmp_mvto.Cr.

                    IF LAST-OF(tmp_mvto.Num_Documento) AND TT_Db = TT_Cr THEN DO:
                        DISPLAY "____________________________________" AT 99 SKIP(1)
                                TT_Db AT 103
                                TT_Cr AT 119 SKIP(0)
                            WITH DOWN FRAME F_Tot2 WIDTH 150 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.

                        ASSIGN TT_Db = 0
                               TT_Cr = 0.
                    END.

                    IF LAST-OF(tmp_mvto.agencia) AND TC_Db = TC_Cr THEN DO:
                        DISPLAY "Total agencia: "   AT 49
                                tmp_mvto.agencia    AT 69
                                "____________________________________" AT 99 SKIP(1)
                                TC_Db AT 103
                                TC_Cr AT 119 SKIP(1)
                            WITH DOWN FRAME F_TotAg WIDTH 150 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.

                        ASSIGN TC_Db = 0
                               TC_Cr = 0.
                    END.
                END.
            END. /* WHERE */ 

        END. /* FIN DEL ELSE  DEL INFORME */

    DISPLAY SKIP(3)
            "    _______________________           _____________________________           _________________________" SKIP
            "    Elaborado                         Revisado                                Aprobado                 " 
        WITH DOWN FRAME F_TotGen WIDTH 150 NO-BOX USE-TEXT STREAM-IO NO-LABELS.

    PAGE.
    
    OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE TotXCta:
    DEFINE VAR TT_Db AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR TT_Cr AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR TC_Db AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.  
    DEFINE VAR TC_Cr AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR TG_Db AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.  
    DEFINE VAR TG_Cr AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR vCuenta AS CHARACTER.

    IF w_ordenar = 1 THEN DO:
        FOR EACH TMP_MVTO NO-LOCK BREAK BY TMP_MVTO.Cuenta
                                        BY TMP_MVTO.Comprobante
                                        BY TMP_MVTO.Num_Documento
                                        BY TMP_MVTO.Agencia
                                        BY TMP_MVTO.Fec_Contable:
            vCuenta = tmp_Mvto.cuenta.

            IF tmp_Mvto.cen_costos <> 999 AND tmp_Mvto.cen_costos <> 0 THEN
                vCuenta = vCuenta + "-" + STRING(tmp_Mvto.cen_costos,"999").

            IF FIRST-OF(TMP_MVTO.Cuenta) THEN DO:
                FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ TMP_MVTO.Cuenta NO-LOCK NO-ERROR.
                DISPLAY "Cuenta: "      AT 1
                        TMP_MVTO.Cuenta AT 10
                        Cuentas.Nombre  AT 25 WHEN AVAIL(Cuentas)
                    WITH FRAME F_Com WIDTH 150 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.
            END.

            DISPLAY TMP_MVTO.Agencia        FORMAT "999" LABEL "Ag."
                    TMP_MVTO.Fec_Contab     FORMAT "99/99/99" LABEL "Fecha"
                    TMP_MVTO.Comprob        LABEL "CP"
                    TMP_MVTO.Num_Documento  FORMAT "999999999" LABEL "Documento"
                    vCuenta                 FORMAT "X(18)" LABEL "Cta-Contable"
                    TMP_MVTO.Comentario     FORMAT "X(20)" LABEL "Descripción Tx"
                    TMP_MVTO.Nit            FORMAT "X(12)" LABEL "Ced./ Nit"
                    TMP_MVTO.Usuario        FORMAT "X(4)"  LABEL "Usu."
                    TMP_MVTO.Doc_Referencia FORMAT "X(10)" LABEL "Docto-Ref."
                    TMP_MVTO.Db             FORMAT ">>,>>>,>>>,>>9" LABEL "Valor Débitos"
                    TMP_MVTO.Cr             FORMAT ">>>>,>>>,>>>,>>9" LABEL "Valor Créditos"
                WITH DOWN FRAME F_Mov1 WIDTH 150 NO-BOX USE-TEXT STREAM-IO NO-LABELS.

            ASSIGN TT_Db = TT_Db + TMP_MVTO.Db
                   TT_Cr = TT_Cr + TMP_MVTO.Cr
                   TC_Db = TC_Db + TMP_MVTO.Db
                   TC_Cr = TC_Cr + TMP_MVTO.Cr
                   TG_Db = TG_Db + TMP_MVTO.Db
                   TG_Cr = TG_Cr + TMP_MVTO.Cr.

            IF LAST-OF(TMP_MVTO.Comprobante) THEN DO:
                DISPLAY "                      Total Comprobante: "
                        TMP_MVTO.Comprobante
                        "                                          __________________ ________________" SKIP
                        "                                                                         "
                        "                "
                        TC_Db
                        " "
                        TC_Cr SKIP(1)
                    WITH DOWN FRAME F_TotCom WIDTH 150 NO-BOX USE-TEXT STREAM-IO NO-LABELS.

                ASSIGN TC_Db = 0
                       TC_Cr = 0.
            END.

            IF LAST-OF(TMP_MVTO.Cuenta) THEN DO:
                DISPLAY "                                                         "
                        "                                "
                        "--------------    -------------" SKIP
                        "Total Cuenta "
                        TMP_MVTO.Cuenta
                        Cuentas.Nombre WHEN AVAIL(cuentas)
                        "                    "
                        TT_Db
                        " "
                        TT_Cr  SKIP(2)
                    WITH DOWN FRAME F_Tot WIDTH 150 NO-BOX USE-TEXT STREAM-IO NO-LABELS.

                ASSIGN TT_Db = 0
                       TT_Cr = 0.
            END.
        END.  /*Fin For Each*/
    END.

    IF w_ordenar = 2 THEN DO:
        FOR EACH TMP_MVTO NO-LOCK BREAK BY TMP_MVTO.Cuenta
                                        BY tmp_mvto.fec_grabacion
                                        BY tmp_mvto.hora
                                        BY TMP_MVTO.Num_Documento
                                        BY TMP_MVTO.Agencia
                                        BY TMP_MVTO.Fec_Contable:
            vCuenta = tmp_Mvto.cuenta.

            IF tmp_Mvto.cen_costos <> 999 AND tmp_Mvto.cen_costos <> 0 THEN
                vCuenta = vCuenta + "-" + STRING(tmp_Mvto.cen_costos,"999").

            IF FIRST-OF(TMP_MVTO.Cuenta) THEN DO:
                FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ TMP_MVTO.Cuenta NO-LOCK NO-ERROR.
                DISPLAY "Cuenta: "      AT 1
                        TMP_MVTO.Cuenta AT 10
                        Cuentas.Nombre  AT 25 WHEN AVAIL(Cuentas)
                    WITH FRAME F_Com2 WIDTH 150 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.
            END.

            DISPLAY TMP_MVTO.Agencia        FORMAT "999" LABEL "Ag."
                    TMP_MVTO.Fec_Contab     FORMAT "99/99/99" LABEL "Fecha"
                    TMP_MVTO.Comprob        LABEL "CP"
                    TMP_MVTO.Num_Documento  FORMAT "999999999" LABEL "Documento"
                    vCuenta                 FORMAT "X(18)" LABEL "Cta-Contable"
                    TMP_MVTO.Comentario     FORMAT "X(20)" LABEL "Descripción Tx"
                    TMP_MVTO.Nit            FORMAT "X(12)" LABEL "Ced./ Nit"
                    TMP_MVTO.Usuario        FORMAT "X(4)"  LABEL "Usu."
                    TMP_MVTO.Doc_Referencia FORMAT "X(10)" LABEL "Docto-Ref."
                    TMP_MVTO.Db             FORMAT ">>,>>>,>>>,>>9" LABEL "Valor Débitos"
                    TMP_MVTO.Cr             FORMAT ">>>>,>>>,>>>,>>9" LABEL "Valor Créditos"
                WITH DOWN FRAME F_Mov2 WIDTH 150 NO-BOX USE-TEXT STREAM-IO NO-LABELS.

            ASSIGN TT_Db = TT_Db + TMP_MVTO.Db
                   TT_Cr = TT_Cr + TMP_MVTO.Cr
                   TC_Db = TC_Db + TMP_MVTO.Db
                   TC_Cr = TC_Cr + TMP_MVTO.Cr
                   TG_Db = TG_Db + TMP_MVTO.Db
                   TG_Cr = TG_Cr + TMP_MVTO.Cr.

            /*IF LAST-OF(TMP_MVTO.Comprobante) THEN DO:
                DISPLAY "                      Total Comprobante: "
                        TMP_MVTO.Comprobante
                        "                                          __________________ ________________" SKIP
                        "                                                                         "
                        "                "
                        TC_Db
                        " "
                        TC_Cr SKIP(1)
                    WITH DOWN FRAME F_TotCom WIDTH 150 NO-BOX USE-TEXT STREAM-IO NO-LABELS.

                ASSIGN TC_Db = 0
                       TC_Cr = 0.
            END.*/

            IF LAST-OF(TMP_MVTO.Cuenta) THEN DO:
                DISPLAY "                                                         "
                        "                                "
                        "--------------    -------------" SKIP
                        "Total Cuenta "
                        TMP_MVTO.Cuenta
                        Cuentas.Nombre WHEN AVAIL(cuentas)
                        "                    "
                        TT_Db
                        " "
                        TT_Cr  SKIP(2)
                    WITH DOWN FRAME F_Tot2 WIDTH 150 NO-BOX USE-TEXT STREAM-IO NO-LABELS.

                ASSIGN TT_Db = 0
                       TT_Cr = 0.
            END.
        END.
    END.

    DISPLAY "                                          "
            "                      ----------------    ----------------" SKIP
            "                                                TOTAL GENERAL   "
            TG_Db
            "      "
            TG_Cr
        WITH DOWN FRAME F_TotGen WIDTH 150 NO-BOX USE-TEXT STREAM-IO NO-LABELS.

     ASSIGN TG_Db = 0
            TG_Cr = 0.

END PROCEDURE.

PROCEDURE TotXComp:
    DEFINE VAR  TA_Db  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR  TA_Cr  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR  TK_Db  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.  
    DEFINE VAR  TK_Cr  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR  TC_Db  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.  
    DEFINE VAR  TC_Cr  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR  TG_Db  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.  
    DEFINE VAR  TG_Cr  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR  TU_Db  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.  
    DEFINE VAR  TU_Cr  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR  tit_c  AS CHARACTER FORM "X(40)" NO-UNDO.
    
    FOR EACH TMP_MVTO NO-LOCK 
        BREAK BY TMP_MVTO.Agencia
              BY TMP_MVTO.Cuenta
              BY TMP_MVTO.Comprobante
              BY TMP_MVTO.Fec_Contable 
              BY TMP_MVTO.Usuario:
        ASSIGN TC_Db = TC_Db + TMP_MVTO.Db
               TC_Cr = TC_Cr + TMP_MVTO.Cr
               TK_Db = TK_Db + TMP_MVTO.Db
               TK_Cr = TK_Cr + TMP_MVTO.Cr
               TG_Db = TG_Db + TMP_MVTO.Db
               TG_Cr = TG_Cr + TMP_MVTO.Cr
               TU_Db = TU_Db + TMP_MVTO.Db
               TU_Cr = TU_Cr + TMP_MVTO.Cr
               TA_Db = TA_Db + TMP_MVTO.Db
               TA_Cr = TA_Cr + TMP_MVTO.Cr.
        IF FIRST-OF(TMP_MVTO.Agencia)  THEN DO:
            FIND FIRST agencias WHERE agencias.agencia EQ TMP_MVTO.agencia NO-LOCK NO-ERROR.
            DISPLAY  TMP_MVTO.Agencia           FORMAT "999"  
                     "- "
                     Agencias.nombre      WHEN AVAIL(Agencias)          FORMAT "X(25)" SKIP(1)
                     WITH DOWN FRAME F_TotCom1 WIDTH 150 NO-BOX USE-TEXT STREAM-IO NO-LABELS.    
        END.

        IF FIRST-OF(TMP_MVTO.Cuenta) THEN DO:
           FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ TMP_MVTO.Cuenta NO-LOCK NO-ERROR.
           DISPLAY "Cuenta: "       AT 5
                    TMP_MVTO.Cuenta AT 15
                    Cuentas.Nombre      AT 30 WHEN AVAIL(Cuentas) SKIP(1)
                    WITH DOWN FRAME F_TotCom2 WIDTH 150 NO-BOX USE-TEXT STREAM-IO NO-LABELS.    
        END.

        IF LAST-OF(TMP_MVTO.Usuario) THEN DO:
            FIND FIRST comprobantes WHERE comprobantes.agencia     EQ TMP_MVTO.agencia AND 
                                          comprobantes.comprobante EQ TMP_MVTO.comprobante NO-LOCK NO-ERROR.
             DISPLAY "        "
                     TMP_MVTO.Comprobante "-" Comprobantes.nombre FORMAT "X(29)" WHEN AVAIL(Comprobantes)  LABEL "Comprobante"
                     TMP_MVTO.Fec_Contab        FORMAT "99/99/99" LABEL "Fecha"
                     "  "
                     TMP_MVTO.Usuario           FORMAT "X(4)"     LABEL "Usu."
                     "  "
                     TU_Db                          FORMAT ">>>>,>>>,>>>,>>9" LABEL "Valor Débitos"
                     "    "
                     TU_Cr                          FORMAT ">>>>,>>>,>>>,>>9" LABEL "Valor Créditos" SKIP(0) 
                     WITH DOWN FRAME F_TotCom3 WIDTH 150 NO-BOX USE-TEXT STREAM-IO NO-LABELS.    
             ASSIGN TU_Db = 0  TU_Cr = 0.

        END.  

        IF LAST-OF(TMP_MVTO.comprobante) THEN DO:
           DISPLAY
               "                                                      "
               "        -----------------     -----------------" SKIP(0)  
               "           Total Comprobante  " TMP_MVTO.Comprobante " - " Comprobantes.nombre FORMAT "X(25)" 
               TC_Db                          FORMAT ">>>>,>>>,>>>,>>9"  
               "    "
               TC_Cr                          FORMAT ">>>>,>>>,>>>,>>9" SKIP(1)
               WITH DOWN FRAME F_TotCom4 WIDTH 150 NO-BOX USE-TEXT STREAM-IO NO-LABELS.    
           ASSIGN TC_Db = 0 TC_Cr = 0.
        END.

        IF LAST-OF(TMP_MVTO.Cuenta) THEN DO:
           DISPLAY "                                                      "
                   "        -----------------     -----------------" SKIP(0)  
                   "Total por la cuenta " 
                    TMP_MVTO.Cuenta 
                   Cuentas.Nombre   FORMAT "X(27)" WHEN AVAIL(Cuentas)
                   TK_Db                          FORMAT ">>>>,>>>,>>>,>>9"  
                   "    "
                   TK_Cr                          FORMAT ">>>>,>>>,>>>,>>9" SKIP(2)
                   WITH DOWN FRAME F_TotCom5 WIDTH 150 NO-BOX USE-TEXT STREAM-IO NO-LABELS.    
           ASSIGN TK_Db = 0  TK_Cr = 0.
        END.
        IF LAST-OF(TMP_MVTO.Agencia) THEN DO:
           DISPLAY
               "                                                      "
               "       -----------------     -----------------" SKIP(1)  
               "              Total Agencia "  TMP_MVTO.agencia " - " Agencias.nombre FORMAT "X(25)" 
               TA_Db                          FORMAT ">>>>,>>>,>>>,>>9"  
               "    "
               TA_Cr                          FORMAT ">>>>,>>>,>>>,>>9" SKIP(1)
               WITH DOWN FRAME F_TotCom6 WIDTH 150 NO-BOX USE-TEXT STREAM-IO NO-LABELS.    
           ASSIGN TA_Db = 0 TA_Cr = 0.
        END.
        
     END.  /*Fin For Each*/

     DISPLAY "                                          "
             "                      ----------------    ----------------" SKIP
             "                                                TOTAL GENERAL   " 
             TG_Db
             "      "
             TG_Cr  
          WITH DOWN FRAME F_TotGen WIDTH 150 NO-BOX USE-TEXT STREAM-IO NO-LABELS.

     ASSIGN TG_Db = 0
            TG_Cr = 0.

  END PROCE.


  /***********************/
  PROCEDURE Imprimir_Excel:
      {Incluido\Def_Excel.i}
       /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
       E_NumFila = 1.
       E_NumColumn = 11.
       E_Fila      = "003" + "Age"          
                   + "003" + "Cbt"      
                   + "007" + "NumDoct"
                   + "014" + "Cuenta        "         
                   + "017" + "Comentario       "     
                   + "003" + "CCo"       
                   + "014" + "Nit           "            
                   + "010" + " DocRefere" 
                   + "014" + "Debito        "    
                   + "014" + "Credito       "    
                   + "004" + "Usua".        

       RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

      /* launch Excel so it is visible to the user */
      chExcelApp:Visible = TRUE.

      /* create a new Workbook */
      chWorkbook = chExcelApp:Workbooks:Add().

      /* get the active Worksheet */
      chWorkSheet = chExcelApp:Sheets:Item(1).

       DEFI VAR W_IdCC AS CHARACTER FORMAT "X(2)".
        
       FOR EACH Mov_Contable WHERE Mov_Contable.Agencia = W_Ag1
                               AND (IF W_CC1 = 0 THEN TRUE ELSE Mov_Contable.Cen_Costos = W_CC1)
                               AND (IF W_CB1 = 0 THEN TRUE ELSE Mov_Contable.Comprobante = W_CB1)
                               AND (Mov_Contable.Fec_Contable >=  W_Fec1 AND Mov_Contable.Fec_Contable <=  W_Fec2)
                               AND (Mov_Contable.Cuenta >= W_Cuenta1 AND Mov_Contable.Cuenta <= W_Cuenta2) NO-LOCK BREAK BY Mov_Contable.Comprobante
                                                                                                                                                                                BY Mov_Contable.Fec_Contable
                                                                                                                                                                                BY Mov_Contable.Agencia
                                                                                                                                                                                BY Mov_Contable.Num_Documento:
            E_Fila2     = "".
            E_Fila2     = "003" + STRING(Mov_Contable.Agencia,"999")
                        + "003" + STRING(Mov_Contable.Comprobante,"999")
                        + "007" + STRING(Mov_Contable.Num_Documento,"9999999")
                        + "014" + STRING(Mov_Contable.Cuenta,"X(14)")
                        + "017" + STRING(Mov_Contable.Comentario,"X(20)")
                        + "003" + STRING(Mov_Contable.Cen_Costos,"999")
                        + "014" + STRING(Mov_Contable.Nit,"X(14)")
                        + "010" + STRING(Mov_Contable.Doc_Referencia,"X(10)")
                        + "014" + STRING(Mov_Contable.Db,">>,>>>,>>>,>>9")
                        + "014" + STRING(Mov_Contable.Cr,">>,>>>,>>>,>>9")
                        + "004" + STRING(Mov_Contable.Usuario,"X(4)").
            {Incluido\imprimir_Excel.i}
  END PROCEDURE.
                                           

