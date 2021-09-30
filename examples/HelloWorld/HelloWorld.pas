//
// HelloWorld Example
//
// This example prints HelloWorld on the screen.
//
// Copyright (c) 2021 Matias Vara <matiasevara@torokernel.io>
// All Rights Reserved
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

uses BaseUnix;
{$ASMMODE intel}
var
  a: Char;
begin
  WriteLn('Hello World, I am ToroV!');
  WriteLn('Press a key and ENTER to finish');
  FpRead(0, @a, 1);
  WriteLn('You pressed: ', a);
end.
