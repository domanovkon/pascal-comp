program sample;
function div(a : integer, b : integer) : integer 
begin
	write(a - b);
	div := a - b;
end
function sum2(a : integer, b : integer) : integer 
begin
	sum2 := a + b;
end
var	x, y: integer;
	z:real;
begin
	x := 2;
	y := sum2(102,4);
	sum2(1, 2);
	z := 2.5 + x;
	write(z, y, div(x, 99), 10, "result");
end.
