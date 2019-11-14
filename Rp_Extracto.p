/* Parámetros */
DEFINE TEMP-TABLE TImp
    FIELD Reng AS CHAR FORM "X(140)".

DEFINE INPUT PARAMETER TABLE FOR TImp.

DEFINE TEMP-TABLE Imp
    FIELD Reg AS INTEGER
    FIELD Lin AS CHARACTER FORMAT "X(160)".

DEFINE INPUT PARAMETER TABLE FOR Imp.

/* Variables de procedimiento */
DEFINE VAR iHeight AS INTEGER.
DEFINE VAR iWidth AS INTEGER.
DEFINE VAR linea AS INTEGER.
DEFINE VAR interlineado AS INTEGER.
DEFINE VAR margen AS INTEGER.
DEFINE VAR renglon AS INTEGER INITIAL 36.
linea = 18.
interlineado = 12.

{Incluido\pdf_inc.i "NOT SUPER"}

/* Create stream for new PDF file */
RUN pdf_new IN h_PDFinc ("Spdf","Extractos\Extracto.pdf").

pdf_PageFooter ("Spdf", THIS-PROCEDURE:HANDLE, "PageFooter").
pdf_PageHeader ("Spdf", THIS-PROCEDURE:HANDLE, "PageHeader").

/* Load FODUN Logo File */
RUN pdf_load_image IN h_PDFinc ("Spdf","Logo","imagenes\logo-fodun2018.jpg").
RUN pdf_load_image IN h_PDFinc ("Spdf","Tarjeta","imagenes\TarjetaFodun.jpg").
RUN pdf_load_font IN h_PDFinc ("Spdf","Futura","fonts\FuturaBookfont.ttf", "fonts\FuturaBookfont.afm","").
RUN pdf_load_font IN h_PDFinc ("Spdf","Futura-bold","fonts\futura heavy font.ttf", "fonts\futura heavy font.afm","").
RUN pdf_load_font IN h_PDFinc ("Spdf","Futura-medium","fonts\futura medium bt.ttf", "fonts\futura medium bt.afm","").
RUN pdf_load_font IN h_PDFinc ("Spdf","Futura-light","fonts\FuturaLightBT.ttf", "fonts\FuturaLightBT.afm","").

/* Margen inferior */
RUN pdf_set_BottomMargin IN h_PDFinc ("Spdf",30).

/* Instantiate a New Page */
RUN pdf_new_page IN h_PDFinc ("Spdf").

/* Loop through appropriate record set */
/*RUN end_of_report.*/
RUN pdf_text_color IN h_PDFinc ("Spdf",.50,0.0,0.0).   
RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-bold", 17.5).
RUN pdf_text_xy IN h_PDFinc ("Spdf","EXTRACTO", 190, 725).
RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-bold",9).
RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",7).
RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).  
FOR EACH Imp:
    RUN pdf_text_xy IN h_PDFinc ("Spdf", Lin, pdf_LeftMargin("Spdf") + 30, pdf_PageHeight("Spdf") - 743 + linea * renglon - interlineado).
    renglon = renglon - 1.
    IF renglon = 1 THEN DO:
        RUN pdf_skip IN h_PDFinc ("Spdf").
        RUN pdf_new_page IN h_PDFinc ("Spdf").
        renglon = 36.
    END.
END.

FOR EACH TImp:
    RUN pdf_text_xy IN h_PDFinc ("Spdf", Reng, pdf_LeftMargin("Spdf") + 30, pdf_PageHeight("Spdf") - 743 + linea * renglon - interlineado).
    renglon = renglon - 1.
    IF renglon = 1 THEN DO:
        RUN pdf_skip IN h_PDFinc ("Spdf").
        RUN pdf_new_page IN h_PDFinc ("Spdf").
        renglon = 36.
    END.
END.
RUN pdf_skip IN h_PDFinc ("Spdf").        

RUN pdf_close IN h_PDFinc ("Spdf").


/* -------------------- INTERNAL PROCEDURES -------------------------- */

PROCEDURE PageFooter:
    RUN pdf_skip IN h_PDFinc ("Spdf").
    RUN pdf_set_dash IN h_PDFinc ("Spdf",1,0).
    RUN pdf_text_xy IN h_PDFinc ("Spdf", "Página: " + STRING(pdf_page("Spdf")) + " de " + pdf_TotalPages("Spdf"), 420, 30).
END.

PROCEDURE PageHeader:
    iHeight = pdf_PageHeight("Spdf") + 20.
    iWidth = pdf_PageWidth("Spdf").
    RUN pdf_watermark IN h_PDFinc ("Spdf","FODUN","Futura-light",120,.87,.87,.87,85,330).
    RUN pdf_place_image IN h_PDFinc ("Spdf",
                                         "Logo",
                                         pdf_LeftMargin("Spdf") + 30,
                                         pdf_TopMargin("Spdf") + 36,
                                         95,
                                         73).  
    /* Set Detail Font Colour */
    RUN pdf_text_color IN h_PDFinc ("Spdf",.0,.0,.0).
END. /* PageHeader */

IF VALID-HANDLE(h_PDFinc) THEN
  DELETE PROCEDURE h_PDFinc.
