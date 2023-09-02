{
             ECL Brasil - Essential Core Library for Delphi

                   Copyright (c) 2016, Isaque Pinheiro
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
  @Telegram(https://t.me/ormbr)
}

unit eclbr.result.pair.value;

interface

uses
  SysUtils;

type
  TResultPairValue<T> = record
  private
    FValue: T;
    FHasValue: boolean;
  public
    constructor Create(AValue: T);
    class function CreateNil: TResultPairValue<T>; static;
    function GetValue: T;
    procedure SetValue(const AValue: T);
    function HasValue: boolean;
  end;

implementation

constructor TResultPairValue<T>.Create(AValue: T);
begin
  FValue := AValue;
  FHasValue := True;
end;

class function TResultPairValue<T>.CreateNil: TResultPairValue<T>;
begin
  Result.FHasValue := False;
end;

function TResultPairValue<T>.GetValue: T;
begin
  if not FHasValue then
    raise Exception.Create('Value is nil.');
  Result := FValue;
end;

function TResultPairValue<T>.HasValue: boolean;
begin
  Result := FHasValue;
end;

procedure TResultPairValue<T>.SetValue(const AValue: T);
begin
  FValue := AValue;
end;

end.
