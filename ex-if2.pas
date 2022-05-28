program sample;
function sum(a : real, b : real) : real 
begin
	sum := a + b;
end
var	x, y: real;
begin
	x := -2.12;
	y := 3.5;
	
	if (sum(x, y) > 0) then begin
		writeln(sum(x, y));
	end 
	else begin
		write("hi");
	end;
end.



