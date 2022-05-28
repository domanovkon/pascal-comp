program sample;
var i, j, temp : integer;
	arr : array [10] of integer;
begin
	arr[0] := 13;
	arr[1] := 244;
	arr[2] := 4;
	arr[3] := -4;
	arr[4] := 14;
	arr[5] := -24;
	arr[6] := 41;
	arr[7] := 2;
	arr[8] := -5;
	arr[9] := 1;
	
	for i := 0 to 10 do begin
		j := 0;
		while j < 10 do begin
			if arr[j + 1] <= arr[j] then begin
				temp := arr[j+1];
				arr[j+1] := arr[j];
				arr[j] := temp;
			end;
			j := j + 1;
		end;
	end;
	
	for i := 0 to 10 do begin
		writeln(arr[i]);
	end;
end.
