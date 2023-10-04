{
               ECL Brasil - Essential Core Library for Delphi

                   Copyright (c) 2023, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers�o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos � permitido copiar e distribuir c�pias deste documento de
       licen�a, mas mud�-lo n�o � permitido.

       Esta vers�o da GNU Lesser General Public License incorpora
       os termos e condi��es da vers�o 3 da GNU General Public License
       Licen�a, complementado pelas permiss�es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{
  @abstract(ECLBr Library)
  @created(23 Abr 2023)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @Discord(https://discord.gg/S5yvvGu7)
}

unit eclbr.core;

interface

uses
  Rtti,
  SysUtils,
  Generics.Collections;

type
  Tuple = array of TValue;

  IObserverEx = interface
    ['{5887CDFF-DA23-4466-A5CB-FBA1DFEAF907}']
    procedure Update(const Progress: Integer);
  end;

  TArrayHelper = class helper for TArray
  public
    /// <summary>
    ///   Copies an open array to a dynamic array.
    /// </summary>
    class function Copy<T>(const Values: array of T): TArray<T>; static;
  end;

implementation

{ TArrayHelper }

class function TArrayHelper.Copy<T>(const Values: array of T): TArray<T>;
var
  LFor: Integer;
begin
  SetLength(Result, Length(Values));
  for LFor := Low(Values) to High(Values) do
    Result[LFor] := Values[LFor];
end;

end.


