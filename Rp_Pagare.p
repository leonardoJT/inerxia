/* Parámetros */
DEFINE INPUT PARAMETER pNit AS CHARACTER.
DEFINE INPUT PARAMETER pCueAhorros AS CHARACTER.

/* Variables */
/*DEFINE VAR pNit AS CHARACTER INITIAL "80504000".
DEFINE VAR pCueAhorros AS CHARACTER INITIAL "17859".*/

DEFINE VAR pFecPago AS CHARACTER.
DEFINE VAR valorEnLetras AS CHARACTER.
DEFINE VAR vPeriodicidad AS CHARACTER.
DEFINE VAR tasaMensual AS DECIMAL.
DEFINE VAR sdoDisponible AS DECIMAL.
DEFINE VAR periodosLiq AS INTEGER.
DEFINE VAR tea AS DECIMAL.

/* Variables de procedimiento */
DEFINE VAR posY AS INTEGER INITIAL 479.

FIND FIRST ahorros WHERE ahorros.nit = pNit
                     AND ahorros.cue_ahorros = pCueAhorros NO-LOCK NO-ERROR.

sdoDisponible = ahorros.sdo_disponible.

FIND FIRST clientes WHERE clientes.nit = pNit NO-LOCK NO-ERROR.

{Incluido\pdf_inc.i "NOT SUPER"}

/* Create stream for new PDF file */
RUN pdf_new IN h_PDFinc ("Spdf","Reportes\CDAT\" + pNit + "_" + pCueAhorros + ".pdf").


pdf_PageFooter ("Spdf", THIS-PROCEDURE:HANDLE, "PageFooter").
pdf_PageHeader ("Spdf", THIS-PROCEDURE:HANDLE, "PageHeader").


/* Load FODUN Logo File */
RUN pdf_load_image IN h_PDFinc ("Spdf","Logo","imagenes\logo-fodun.jpg").
RUN pdf_load_image IN h_PDFinc ("Spdf","Logo_marcaDeAgua","imagenes\logo-fodun_marcaDeAgua.jpg").
RUN pdf_load_image IN h_PDFinc ("Spdf","Firma_CDAT","imagenes\firmaCDAT.jpg").


/* Margen inferior */
RUN pdf_set_BottomMargin IN h_PDFinc ("Spdf",30).


/* Instantiate a New Page */
RUN pdf_new_page IN h_PDFinc ("Spdf").

/* Loop through appropriate record set */

/*RUN end_of_report.*/

RUN pdf_close IN h_PDFinc ("Spdf").


/* -------------------- INTERNAL PROCEDURES -------------------------- */

PROCEDURE end_of_report:
  /* Display Footer UnderLine and End of Report Tag (Centered) */
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_stroke_fill IN h_PDFinc ("Spdf",1.0,1.0,1.0).
  RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", "End of Report", 250, pdf_TextY("Spdf") - 20, pdf_Text_Width("Spdf","End of Report"), 16, "Left",1).
END.

PROCEDURE PageFooter:
    RUN pdf_skip IN h_PDFinc ("Spdf").
    RUN pdf_set_dash IN h_PDFinc ("Spdf",1,0).
    RUN pdf_line IN h_PDFinc ("Spdf", 40, pdf_TextY("Spdf") - 5, pdf_PageWidth("Spdf") - 35 , pdf_TextY("Spdf") - 5, 1).
    RUN pdf_skip IN h_PDFinc ("Spdf").
    RUN pdf_skip IN h_PDFinc ("Spdf").
    RUN pdf_text_to IN h_PDFinc ("Spdf", "Página: " + STRING(pdf_page("Spdf")) + " de " + pdf_TotalPages("Spdf"), 120).
END.

PROCEDURE PageHeader:
    /* Marca de agua */
    /*RUN pdf_watermark IN h_PDFinc ("Spdf","C D A T","Courier-Bold",75,.87,.87,.87,175,600).*/
    RUN pdf_place_image IN h_PDFinc ("Spdf",
                                     "Logo_marcaDeAgua",
                                     pdf_LeftMargin("Spdf") + 78,
                                     pdf_TopMargin("Spdf") + 190,
                                     451,
                                     138).

    
    IF pdf_Page("Spdf") = 1 THEN DO:
        RUN pdf_place_image IN h_PDFinc ("Spdf",
                                         "Logo",
                                         pdf_LeftMargin("Spdf") + 30,
                                         pdf_TopMargin("Spdf") + 10,
                                         147,
                                         43).

        /* CERTIFICADO CDAT */
        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",24).
        FIND FIRST pro_ahorros WHERE cod_ahorro = ahorros.cod_ahorro NO-LOCK NO-ERROR.
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "CERTIFICADO " + pro_ahorro.Nom_Producto, pdf_LeftMargin("Spdf") + 220, 760).

        /* Agencia y Nº de CDAT */
        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",20).
        FIND FIRST agencias WHERE agencias.agencia = ahorros.agencia NO-LOCK NO-ERROR.
        RUN pdf_text_xy IN h_PDFinc ("Spdf", SUBSTRING(agencias.nombre,1,3) + "-" + ahorros.cue_ahorros, pdf_LeftMargin("Spdf") + 400, 735).

        /* Monto y valor en letras */
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 40, /* Eje Y */ 685, /* Ancho */ 540, /* Alto */ 40, /* Justificación */ "Center", /* Ancho de línea */ 1).

        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",14).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(sdoDisponible,"$>>>,>>>,>>9"),45,697).
        
        RUN montoesc.p(INPUT sdoDisponible,
                       INPUT 0,
                       OUTPUT valorEnLetras) NO-ERROR.

        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",8).

        RUN pdf_text_xy IN h_PDFinc ("Spdf"," - " + REPLACE(valorEnLetras,"*",""),145,697).


        /* Cédula, nombre */
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 40, /* Eje Y */ 625, /* Ancho */ 540, /* Alto */ 40, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",12).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",clientes.Tipo_Identificacion + " " + clientes.nit + "       NOMBRE: " + clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2,60,647).

        /*CASE ahorros.per_liquidacion:
            WHEN 1 THEN vPeriodicidad = " Mensual Día vencido".
            WHEN 2 THEN vPeriodicidad = " Mensual Mes vencido".
            WHEN 3 THEN vPeriodicidad = " Mensual Trimestre vencido".
            WHEN 4 THEN vPeriodicidad = " Mensual Semestre vencido".
            WHEN 5 THEN vPeriodicidad = " Mensual Año vencido".
            WHEN 6 THEN vPeriodicidad = " Mensual Al Vencimiento".
        END CASE.

        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",12).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","- Interés " + STRING(Ahorros.tasa / 12,">>9.9999%") + vPeriodicidad,50,617).*/

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 40, /* Eje Y */ 608, /* Ancho */ 135, /* Alto */ 12, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",9).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","TASA EFECTIVA ANUAL",60,611).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 175, /* Eje Y */ 608, /* Ancho */ 135, /* Alto */ 12, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",9).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","TASA NOMINAL MENSUAL",195,611).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 310, /* Eje Y */ 608, /* Ancho */ 135, /* Alto */ 12, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",9).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","MODALIDAD",360,611).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 445, /* Eje Y */ 608, /* Ancho */ 135, /* Alto */ 12, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",9).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","PLAZO",495,611).

        CASE ahorros.per_liquidacion:
            WHEN 1 THEN periodosLiq = 365.
            WHEN 2 THEN periodosLiq = 12.
            WHEN 3 THEN periodosLiq = 4.
            WHEN 4 THEN periodosLiq = 2.
            WHEN 5 THEN periodosLiq = 1.
            WHEN 6 THEN DO:
                periodosLiq = 365 / ahorros.plazo.
            END.
        END CASE.

        tea = EXP(1 + ((ahorros.tasa / 100) / periodosLiq), periodosLiq) - 1.
        tea = tea * 100.
        
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 40, /* Eje Y */ 590, /* Ancho */ 135, /* Alto */ 18, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",12).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(tea,">>9.99%"),75,594).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 175, /* Eje Y */ 590, /* Ancho */ 135, /* Alto */ 18, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",12).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(ahorros.tasa / 12,">>9.99%"),210,594).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 310, /* Eje Y */ 590, /* Ancho */ 135, /* Alto */ 18, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",12).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","VENCIDO",360,594).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 445, /* Eje Y */ 590, /* Ancho */ 135, /* Alto */ 18, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",12).
        /*RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(ROUND(ahorros.plazo / 10,0) * 10) + " días",480,594).*/
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(ahorros.plazo) + " días",480,594).


        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 40, /* Eje Y */ 544, /* Ancho */ 540, /* Alto */ 41, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",7.7).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","- El Asociado poseedor de este depósito acepta las condiciones establecidas en el reglamento de ahorro de FODUN",50,572).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",12).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","FECHA APERTURA: " + STRING(ahorros.fec_apertura,"99/99/9999"),50,550).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","FECHA VENCIMIENTO: " + STRING(ahorros.fec_vencimiento,"99/99/9999"),360,550).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 40, /* Eje Y */ 484, /* Ancho */ 540, /* Alto */ 60, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",10).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Gerente:                                    Firma Titular:",50,534).

        RUN pdf_place_image IN h_PDFinc ("Spdf",
                                         "Firma_CDAT",
                                         pdf_LeftMargin("Spdf") + 50,
                                         pdf_TopMargin("Spdf") + 250,
                                         135,
                                         37).
    END.
END. /* PageHeader */

IF VALID-HANDLE(h_PDFinc) THEN
  DELETE PROCEDURE h_PDFinc.
