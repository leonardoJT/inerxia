DISABLE TRIGGERS FOR LOAD OF carteraVencida.

FOR EACH carteravencida:
    DELETE carteraVencida.
END.

INPUT FROM c:\INFO_Fodun\CarteraVencida.txt.
REPEAT:
    CREATE carteraVencida.
    IMPORT carteraVencida.
END.


