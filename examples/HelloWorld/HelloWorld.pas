{$ASMMODE INTEL}
{$mode delphi} 

uses ToroVSys, BaseUnix;
var
  a: Char;
  r: PChar;
begin
  WriteLn('Hello World, I am ToroV!');
  WriteLn('Press a key and ENTER to finish');
  FpRead(0, @a, 1);
  WriteLn('You pressed: ', a);
  r := GetMem(120);
  r^ := 'a';
  FreeMem(r);
  WriteLn('Exiting');
end.
