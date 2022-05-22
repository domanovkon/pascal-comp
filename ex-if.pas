program sample;
function div(a : integer, b : integer) : integer 
begin
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
	
	for x := 0 to 10 do begin
		write(x, ",");
		x := x + 1;
	end;

	
	if 3 > 2 then begin
		y := sum2(1000, 8);
		end;
		
	if div(3, 1) = y then begin
			z := 777.0;
		end
		else begin
			z := 999.0;
			y := 7;
			while y >= 2 do begin
			y := y - 1;
			write(x, ",");
			x := x + 1;
	end;
		end;
	write(z,y, div(x, 99), 10, "result");
end.
