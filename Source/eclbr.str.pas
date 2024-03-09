{
               ECL Brasil - Essential Core Library for Delphi

                   Copyright (c) 2023, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Versão 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos é permitido copiar e distribuir cópias deste documento de
       licença, mas mudá-lo não é permitido.

       Esta versão da GNU Lesser General Public License incorpora
       os termos e condições da versão 3 da GNU General Public License
       Licença, complementado pelas permissões adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{
  @abstract(ECLBr Library)
  @created(23 Abr 2023)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @Discord(https://discord.gg/T2zJC8zX)
}

unit eclbr.str;

interface

uses
  SysUtils,
  Generics.Collections,
  eclbr.vector;

type
  TStringEx = record helper for string
  public
    function Filter(const APredicate: TFunc<Char, Boolean>): string;
    function Collect: TVector<string>;
    function Map(const ATransform: TFunc<Char, Char>): string;
    function Sum: Integer;
    function First: Char;
    function Last: Char;
    function Reduce<T>(const AInitialValue: T; const AAccumulator: TFunc<T, Char, T>): T;
    function Exists(const APredicate: TFunc<Char, Boolean>): Boolean;
    function All(const APredicate: TFunc<Char, Boolean>): Boolean;
    function Any(const APredicate: TFunc<Char, Boolean>): Boolean;
    function Sort: string;
//    function Split(const ASeparator: array of Char): TVector<string>;
    procedure Partition(APredicate: TFunc<Char, Boolean>; out Left, Right: string);
  end;

implementation

uses
  eclbr.std;

{ TStringEx }

function TStringEx.Filter(const APredicate: TFunc<Char, Boolean>): string;
var
  LChar: Char;
  LValue: String;
begin
  if not Assigned(APredicate) then
    raise Exception.Create('Invalid predicate function in TStringEx.Filter');

  LValue := EmptyStr;
  for LChar in Self do
  begin
    if APredicate(LChar) then
      LValue := LValue + LChar;
  end;
  Self := LValue;
  Result := Self;
end;

function TStringEx.First: Char;
begin
  if Length(Self) > 0 then
    Result := Self[1]
  else
    raise Exception.Create('String is empty');
end;

function TStringEx.Last: Char;
begin
  if Length(Self) > 0 then
    Result := Self[Length(Self)]
  else
    raise Exception.Create('String is empty');
end;

function TStringEx.Collect: TVector<string>;
var
  LChar: Char;
  LIndex: Integer;
begin
  Result := TVector<string>.Create([]);
  LIndex := 0;
  for LChar in Self do
    Result.Add(LChar);
end;

function TStringEx.Map(const ATransform: TFunc<Char, Char>): string;
var
  LChar: Char;
  LValue: String;
begin
  if not Assigned(ATransform) then
    raise Exception.Create('Invalid transform function in TStringEx.Map');

  LValue := EmptyStr;
  for LChar in Self do
  begin
    LValue := LValue + ATransform(LChar);
  end;
  Self := LValue;
  Result := Self;
end;

function TStringEx.Sum: Integer;
var
  LChar: Char;
begin
  Result := 0;
  for LChar in Self do
    Result := Result + StrToIntDef(LChar, 0);
end;

function TStringEx.Reduce<T>(const AInitialValue: T; const AAccumulator: TFunc<T, Char, T>): T;
var
  LChar: Char;
begin
  if not Assigned(AAccumulator) then
    raise Exception.Create('Invalid accumulator function in TStringEx.Reduce<T>');

  Result := AInitialValue;
  for LChar in Self do
    Result := AAccumulator(Result, LChar);
end;

function TStringEx.Exists(const APredicate: TFunc<Char, Boolean>): Boolean;
var
  LChar: Char;
begin
  if not Assigned(APredicate) then
    raise Exception.Create('Invalid predicate function in TStringEx.Exists');

  for LChar in Self do
    if APredicate(LChar) then
      Exit(True);

  Result := False;
end;

function TStringEx.All(const APredicate: TFunc<Char, Boolean>): Boolean;
var
  LChar: Char;
begin
  if not Assigned(APredicate) then
    raise Exception.Create('Invalid predicate function in TStringEx.All');

  for LChar in Self do
    if not APredicate(LChar) then
      Exit(False);

  Result := True;
end;

function TStringEx.Any(const APredicate: TFunc<Char, Boolean>): Boolean;
var
  LChar: Char;
begin
  if not Assigned(APredicate) then
    raise Exception.Create('Invalid predicate function in TStringEx.Any');

  for LChar in Self do
    if APredicate(LChar) then
      Exit(True);

  Result := False;
end;

function TStringEx.Sort: string;
var
  LArray: TArray<Char>;
  LChar: Char;
  LIndex: Integer;
begin
  SetLength(LArray, Length(Self));
  for LIndex := 1 to Length(Self) do
    LArray[LIndex - 1] := Self[LIndex];

  TArray.Sort<Char>(LArray);

  Result := '';
  for LChar in LArray do
    Result := Result + LChar;
end;

//function TStringEx.Split(const ASeparator: array of Char): TVector<string>;
//var
//  LItem: string;
//  LArray: TArray<string>;
//begin
//  Result := TVector<string>.Create([]);
//  LArray := TStd.Split(Self);
//  for LItem in LArray do
//    Result.Add(LItem);
//end;

procedure TStringEx.Partition(APredicate: TFunc<Char, Boolean>; out Left, Right: string);
var
  LIndex: Integer;
begin
  if not Assigned(APredicate) then
    raise Exception.Create('Invalid predicate function in TStringEx.Partition');

  LIndex := 1;
  while (LIndex <= Length(Self)) and APredicate(Self[LIndex]) do
    Inc(LIndex);

  Left := Copy(Self, 1, LIndex - 1);
  Right := Copy(Self, LIndex, Length(Self) - LIndex + 1);
end;

end.
