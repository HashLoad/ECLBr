unit eclbr.tuple;

interface

uses
  SysUtils,
  Generics.Collections,
  Generics.Defaults;

type
  TTuple<K, V> = packed record
  private
    FTuples: TArray<TPair<K, V>>;
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

    /// <summary>
    ///   Returns the number of elements in the collection.
    /// </summary>
    /// <remarks>
    ///   This function returns the current count of elements stored in the collection.
    /// </remarks>
    /// <returns>
    ///   An integer value representing the number of elements in the collection.
    /// </returns>
    function Count: integer;
  end;

implementation

{ TTupla<K, V> }

function TTuple<K, V>.Count: integer;
begin
  Result := Length(FTuples);
end;

function TTuple<K, V>.Get(const AKey: K): V;
var
  LPair: TPair<K, V>;
begin
  for LPair in FTuples do
  begin
    if TEqualityComparer<K>.Default.Equals(LPair.Key, AKey) then
      exit(LPair.Value);
  end;
  raise Exception.Create('Key not found');
end;

function TTuple<K, V>.Keys(const AKeyArray: array of K): TTuple<K, V>;
var
  LFor: integer;
  LTuples: TArray<TPair<K, V>>;
begin
  SetLength(LTuples, Length(FTuples) + Length(AKeyArray));
  for LFor := Low(FTuples) to High(FTuples) do
    LTuples[LFor] := FTuples[LFor];
  for LFor := 0 to High(AKeyArray) do
    LTuples[High(FTuples) + LFor + 1] := TPair<K, V>.Create(AKeyArray[LFor], Default(V));
  FTuples := LTuples;
  Result := Self;
end;

function TTuple<K, V>.Values(const AValueArray: array of V): TTuple<K, V>;
var
  LFor: Integer;
begin
  if Length(FTuples) <> Length(AValueArray) then
    raise Exception.Create('Number of values does not match the number of keys');
  for LFor := Low(FTuples) to High(FTuples) do
    FTuples[LFor] := TPair<K, V>.Create(FTuples[LFor].Key, AValueArray[LFor]);
  Result := Self;
end;

end.
