unit eclbr.systuple;

interface

uses
  SysUtils,
  Generics.Collections,
  eclbr.sysmap;

type
  TTuple<K, V> = packed record
  private
    FTuplas: TMap<K, V>;
  public
    /// <summary>
    ///   Cria um objeto TTuple contendo as chaves especificadas no array AKeyArray.
    /// </summary>
    /// <param name="AKeyArray">Um array de chaves a serem usadas para inicializar o TTuple.</param>
    /// <returns>Um objeto TTuple contendo as chaves especificadas.</returns>
    function Keys(const AKeyArray: array of K): TTuple<K, V>;

    /// <summary>
    ///   Cria um objeto TTuple contendo os valores especificados no array AValueArray.
    /// </summary>
    /// <param name="AValueArray">Um array de valores a serem usados para inicializar o TTuple.</param>
    /// <returns>Um objeto TTuple contendo os valores especificados.</returns>
    function Values(const AValueArray: array of V): TTuple<K, V>;

    /// <summary>
    ///   Obtém o valor associado à chave especificada.
    /// </summary>
    /// <param name="AKey">A chave para a qual se deseja obter o valor correspondente.</param>
    /// <returns>O valor associado à chave especificada.</returns>
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
