set PATH=C:\Progress\OpenEdge\BIN;%PATH%
set LIB=C:\Progress\OpenEdge\LIB;%LIB%
d:
cd d:\sps\soportes\fodun\bd
proserve bdcentral -H servidor-hp -N tcp -S 5850 -L 100000
