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
  TCharHelperEx = record helper for Char
  public
    function ToUpper: Char;
    function ToLower: Char;
    function IsLetter: Boolean;
  end;

  TStringHelperEx = record helper for String
  public
    function Filter(const APredicate: TFunc<Char, Boolean>): String;
    function Collect: TVector<String>;
    function Map(const ATransform: TFunc<Char, Char>): String;
    function Sum: Integer;
    function First: Char;
    function Last: Char;
    function Reduce<T>(const AInitialValue: T; const AAccumulator: TFunc<T, Char, T>): T;
    function Exists(const APredicate: TFunc<Char, Boolean>): Boolean;
    function All(const APredicate: TFunc<Char, Boolean>): Boolean;
    function Any(const APredicate: TFunc<Char, Boolean>): Boolean;
    function Sort: String;
    procedure Partition(APredicate: TFunc<Char, Boolean>; out Left, Right: String);
  end;

implementation

uses
  eclbr.std;

{ TStringHelperEx }

function TStringHelperEx.Filter(const APredicate: TFunc<Char, Boolean>): String;
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

function TStringHelperEx.First: Char;
begin
  if Length(Self) > 0 then
    Result := Self[1]
  else
    raise Exception.Create('String is empty');
end;

function TStringHelperEx.Last: Char;
begin
  if Length(Self) > 0 then
    Result := Self[Length(Self)]
  else
    raise Exception.Create('String is empty');
end;

function TStringHelperEx.Collect: TVector<String>;
var
  LChar: Char;
begin
  Result := TVector<String>.Create([]);
  for LChar in Self do
    Result.Add(LChar);
end;

function TStringHelperEx.Map(const ATransform: TFunc<Char, Char>): String;
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

function TStringHelperEx.Sum: Integer;
var
  LChar: Char;
begin
  Result := 0;
  for LChar in Self do
    Result := Result + StrToIntDef(LChar, 0);
end;

function TStringHelperEx.Reduce<T>(const AInitialValue: T; const AAccumulator: TFunc<T, Char, T>): T;
var
  LChar: Char;
begin
  if not Assigned(AAccumulator) then
    raise Exception.Create('Invalid accumulator function in TStringEx.Reduce<T>');

  Result := AInitialValue;
  for LChar in Self do
    Result := AAccumulator(Result, LChar);
end;

function TStringHelperEx.Exists(const APredicate: TFunc<Char, Boolean>): Boolean;
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

function TStringHelperEx.All(const APredicate: TFunc<Char, Boolean>): Boolean;
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

function TStringHelperEx.Any(const APredicate: TFunc<Char, Boolean>): Boolean;
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

function TStringHelperEx.Sort: String;
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

procedure TStringHelperEx.Partition(APredicate: TFunc<Char, Boolean>; out Left, Right: String);
var
  LIndex: Integer;
begin
  if not Assigned(APredicate) then
    raise Exception.Create('Invalid predicate function in TStringHelperEx.Partition');

  for LIndex := 1 to Length(Self) do
  begin
    if APredicate(Self[LIndex]) then
      Right := Right + Self[LIndex]
    else
      Left := Left + Self[LIndex];
  end;
end;

{ TCharHelperEx }

function TCharHelperEx.IsLetter: Boolean;
begin
  Result := CharInSet(Self, ['a'..'z', 'A'..'Z']);
end;

function TCharHelperEx.ToLower: Char;
begin
  if CharInSet(Self, ['A'..'Z']) then
    Result := Chr(Ord(Self) + 32)
  else
    Result := Self;
end;

function TCharHelperEx.ToUpper: Char;
begin
  if CharInSet(Self, ['a'..'z']) then
    Result := Chr(Ord(Self) - 32)
  else
    Result := Self;
end;

end.
