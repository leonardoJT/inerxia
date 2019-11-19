{Incluido\pdf_inc.i "NOT SUPER"}

DEFINE VARIABLE Vold_Y      AS INTEGER NO-UNDO.

DEFINE VARIABLE vStateBook  AS INTEGER NO-UNDO.
DEFINE VARIABLE vCustBook   AS INTEGER NO-UNDO.
DEFINE VARIABLE vNullBook   AS INTEGER NO-UNDO.

/* Create stream for new PDF file */
RUN pdf_new IN h_PDFinc ("Spdf","C:\Info_Fodun\Reportes\FacturaCupo.pdf").

pdf_PageFooter ("Spdf",
                THIS-PROCEDURE:HANDLE,
                "PageFooter").
pdf_PageHeader ("Spdf",
                THIS-PROCEDURE:HANDLE,
                "PageHeader").

/* Load FODUN Logo File */
RUN pdf_load_image IN h_PDFinc ("Spdf","Logo","imagenes\logo-fodun.jpg").

/* Margen inferior */
RUN pdf_set_BottomMargin IN h_PDFinc ("Spdf",40).

/* Instantiate a New Page */
RUN pdf_new_page IN h_PDFinc ("Spdf").

/* Loop through appropriate record set */
FOR EACH clientes WHERE clientes.nit = "75147376" NO-LOCK BREAK BY clientes.agencia:
    /* Output the appropriate Record information */
    RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Oblique",10.0).
    RUN pdf_text_at  IN h_PDFinc ("Spdf", clientes.agencia,1).
    RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",10.0).
    RUN pdf_text_at  IN h_PDFinc ("Spdf", clientes.nit,6).
    RUN pdf_text_at  IN h_PDFinc ("Spdf", clientes.nombre,12).
    RUN pdf_text_at  IN h_PDFinc  ("Spdf", STRING(clientes.tel_residencia),44).
    RUN pdf_text_to  IN h_PDFinc ("Spdf", STRING(clientes.salario),80).

    /* Skip to Next Text Line */
    RUN pdf_skip IN h_PDFinc ("Spdf").

    IF LAST-OF(clientes.agencia) THEN DO:
        /* Put a red line between each of the states */
        RUN pdf_skip IN h_PDFinc ("Spdf").
        RUN pdf_stroke_color IN h_PDFinc ("Spdf",1.0,.0,.0).
        RUN pdf_set_dash IN h_PDFinc ("Spdf",2,2).
        RUN pdf_line IN h_PDFinc ("Spdf", pdf_LeftMargin("Spdf") , pdf_TextY("Spdf") + 5, pdf_PageWidth("Spdf") - 20 , pdf_TextY("Spdf") + 5, 0.5).
        RUN pdf_stroke_color IN h_PDFinc ("Spdf",.0,.0,.0).
        RUN pdf_skip IN h_PDFinc ("Spdf").
    END. /* Last-of State */
END. /* each Customer */

RUN end_of_report.

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
    RUN pdf_line IN h_PDFinc ("Spdf", 0, pdf_TextY("Spdf") - 5, pdf_PageWidth("Spdf") - 20 , pdf_TextY("Spdf") - 5, 1).
    RUN pdf_skip IN h_PDFinc ("Spdf").
    RUN pdf_skip IN h_PDFinc ("Spdf").
    RUN pdf_text_to IN h_PDFinc ("Spdf", "Página: " + STRING(pdf_page("Spdf")) + " de " + pdf_TotalPages("Spdf"), 97).
END.

PROCEDURE PageHeader:
    /* Marca de agua */
    RUN pdf_watermark IN h_PDFinc ("Spdf","Customer List","Courier-Bold",34,.87,.87,.87,175,500).

    /* Place Logo but only on first page of Report */
    IF pdf_Page("Spdf") = 1 THEN
        RUN pdf_place_image IN h_PDFinc ("Spdf","Logo",pdf_LeftMargin("Spdf"), pdf_TopMargin("Spdf") - 20 ,179,20).
    
    /* Set Header Font Size and Colour */
    RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",10.0).
    RUN pdf_text_color IN h_PDFinc ("Spdf",1.0,.0,.0).

    /* Put a Rectangle around the Header */
    RUN pdf_stroke_color IN h_PDFinc ("Spdf", .0,.0,.0).
    RUN pdf_stroke_fill IN h_PDFinc ("Spdf", .9,.9,.9).
    RUN pdf_rect IN h_PDFinc ("Spdf",
                              pdf_LeftMargin("Spdf"), /* Distancia del margen izquierdo */
                              pdf_TextY("Spdf") - 3, /* Margen superior */
                              pdf_PageWidth("Spdf") - 30, /* Ancho */
                              12, /* Altura */
                              0.5 /* Grosor de línea */).

    /* Encabezados */
    RUN pdf_text_at IN h_PDFinc ("Spdf","St",1).
    RUN pdf_text_at IN h_PDFinc ("Spdf","Nbr",6).
    RUN pdf_text_at IN h_PDFinc ("Spdf","Customer Name",12).
    RUN pdf_text_at IN h_PDFinc ("Spdf","Phone Number",44).
    RUN pdf_text_to IN h_PDFinc ("Spdf","Balance",80).

    /* Display Header UnderLine */
    RUN pdf_skip IN h_PDFinc ("Spdf").
    RUN pdf_set_dash IN h_PDFinc ("Spdf",1,0).
    RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin("Spdf"), pdf_TextY("Spdf") + 5, pdf_PageWidth("Spdf") - 20 , pdf_TextY("Spdf") + 5, 1).
    RUN pdf_skip IN h_PDFinc ("Spdf").

    /* Set Detail Font Colour */
    RUN pdf_text_color IN h_PDFinc ("Spdf",.0,.0,.0).
END. /* PageHeader */

IF VALID-HANDLE(h_PDFinc) THEN
  DELETE PROCEDURE h_PDFinc.
