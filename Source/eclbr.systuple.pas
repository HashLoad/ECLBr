unit eclbr.systuple;

interface

uses
  SysUtils,
  Generics.Collections,
  eclbr.sysmap;

type
  TTuple<K, V> = record
  private
    FTuplas: TMap<K, V>;
  public
    function Keys(const AKeyArray: array of K): TTuple<K, V>;
    function Values(const AValueArray: array of V): TTuple<K, V>;
    function Get(const AKey: K): V;
  end;

implementation

{ TTupla<K, V> }

function TTuple<K, V>.Get(const AKey: K): V;
begin
  Result := FTuplas[Akey];
end;

function TTuple<K, V>.Keys(const AKeyArray: array of K): TTuple<K, V>;
var
  LFor: Integer;
begin
  for LFor := Low(AKeyArray) to High(AKeyArray) do
    FTuplas.Add(AKeyArray[LFor], Default(V));
  Result := Self;
end;

function TTuple<K, V>.Values(const AValueArray: array of V): TTuple<K, V>;
var
  LFor: Integer;
  LPair: TPair<K, V>;
begin
  if FTuplas.Length <> Length(AValueArray) then
    raise Exception.Create('Error Message');

  LFor := 0;
  for LPair in FTuplas.ToArray do
  begin
    FTuplas[LPair.Key] := AValueArray[LFor];
    Inc(LFor);
  end;
  Result := Self;
end;

end.
