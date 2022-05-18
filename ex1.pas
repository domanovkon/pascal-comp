program sample;
function div(a : integer, b : integer) : integer 
begin
	write(a - b);
	div := a - b;
end
function sum(a : integer, b : integer) : integer 
begin
	sum := a + b;
end
var	x, y: integer;
	z:real;
begin
	x := 2;
	y := sum(5,4);
	sum(1, 2);
	z := 2.5 + x;
	write(z, y, div(x, 99), 10, "result");
end.
