program sample;
var i, j, temp : integer;
	arr : array [6] of integer = (4, -2, 12, 5, -4, -10);
begin
	writeln("Array")
	for i := 0 to 6 do begin
		writeln(arr[i]);
	end;
	for i := 0 to 5 do begin
		j := 0;
		while j < 6 do begin
			if arr[j + 1] <= arr[j] then begin
				temp := arr[j+1];
				arr[j+1] := arr[j];
				arr[j] := temp;
			end;
			j := j + 1;
		end;
	end;
	
	writeln("Sorted")
	for i := 0 to 6 do begin
		writeln(arr[i]);
	end;
end.
