{$ASMMODE INTEL}
{$mode delphi} 

uses ToroVSys, BaseUnix;
var
  a: Char;
begin
  WriteLn('Hello World, I am ToroV!');
  WriteLn('Press a key and ENTER to finish');
  FpRead(0, @a, 1);
  WriteLn('You pressed: ', a);
  WriteLn('Exiting');
end.
