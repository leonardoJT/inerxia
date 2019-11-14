{PDFinclude3.3.3\pdf_inc.i "NOT SUPER"}
RUN pdf_open_PDF IN h_PDFinc ("Spdf", "D:\SFG\Desarrollo\Prog\formulario.pdf", "PO").
RUN pdf_fill_text IN h_PDFinc ("Spdf","nombre","prueba"). 
RUN pdf_close IN h_PDFinc ("Spdf").

IF VALID-HANDLE(h_PDFinc) THEN
  DELETE PROCEDURE h_PDFinc.

