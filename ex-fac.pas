program sample;
function fact(x : integer) : integer 
var n, val : integer;
begin
	n := 1;
	val := 1;
	while n <= x do begin
		val := n * val;
		n := n + 1;
	end;
	fact := val;
end
var	x: integer;
begin
	write(fact(5));
end.
