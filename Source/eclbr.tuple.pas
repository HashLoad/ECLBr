unit eclbr.tuple;

interface

uses
  Rtti,
  SysUtils,
  Generics.Collections,
  Generics.Defaults;

type
  TTuple<K> = packed record
  private
    FTuples: TArray<TPair<K, TValue>>;
  private
    /// <summary>
    ///   Creates a new instance of the TTuple class with the specified key-value pairs.
    /// </summary>
    /// <param name="ATuples">
    ///   An array of TPair instances representing the key-value pairs to be included in the tuple.
    /// </param>
    constructor Create(const ATuples: TArray<TPair<K, TValue>>);
  public
    class operator Implicit(const P: TTuple<K>): TArray<TPair<K, TValue>>;
    class operator Implicit(const P: TArray<TPair<K, TValue>>): TTuple<K>;
    class operator Equal(const Left, Right: TTuple<K>): boolean;
    class operator NotEqual(const Left, Right: TTuple<K>): boolean;

    /// <summary>
    ///   Creates a new instance of the TTuple class with the specified keys and values.
    /// </summary>
    /// <param name="AKeys">
    ///   An array of keys of type K to be included in the tuple.
    /// </param>
    /// <param name="AValues">
    ///   An array of values of type V to be associated with the keys in the tuple.
    /// </param>
    /// <returns>
    ///   A new TTuple instance with the specified keys and values.
    /// </returns>
    class function New(const AKeys: array of K; const AValues: array of TValue): TTuple<K>; static;

    /// <summary>
    ///   Cria um objeto TTuple contendo as chaves especificadas no array AKeyArray.
    /// </summary>
    /// <param name="AKeyArray">Um array de chaves a serem usadas para inicializar o TTuple.</param>
    /// <returns>Um objeto TTuple contendo as chaves especificadas.</returns>
    function Keys(const AKeyArray: array of K): TTuple<K>;

    /// <summary>
    ///   Cria um objeto TTuple contendo os valores especificados no array AValueArray.
    /// </summary>
    /// <param name="AValueArray">Um array de valores a serem usados para inicializar o TTuple.</param>
    /// <returns>Um objeto TTuple contendo os valores especificados.</returns>
    function Values(const AValueArray: array of TValue): TTuple<K>;

    /// <summary>
    ///   Obt�m o valor associado � chave especificada.
    /// </summary>
    /// <param name="AKey">A chave para a qual se deseja obter o valor correspondente.</param>
    /// <returns>O valor associado � chave especificada.</returns>
    function Get<T>(const AKey: K): T;

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

{ TTupla<K, TValue> }

function TTuple<K>.Count: integer;
begin
  Result := Length(FTuples);
end;

constructor TTuple<K>.Create(const ATuples: TArray<TPair<K, TValue>>);
begin
  FTuples := ATuples;
end;

class operator TTuple<K>.Equal(const Left, Right: TTuple<K>): boolean;
var
  LFor: Integer;
begin
  Result := false;
  if Length(Left.FTuples) <> Length(Right.FTuples) then
    exit;
  for LFor := 0 to High(Left.FTuples) do
  begin
    if not TEqualityComparer<K>.Default.Equals(Left.FTuples[LFor].Key, Right.FTuples[LFor].Key) or
       not TEqualityComparer<TValue>.Default.Equals(Left.FTuples[LFor].Value, Right.FTuples[LFor].Value) then
    begin
      exit;
    end;
  end;
  Result := True;
end;

function TTuple<K>.Get<T>(const AKey: K): T;
var
  LPair: TPair<K, TValue>;
begin
  for LPair in FTuples do
  begin
    if not TEqualityComparer<K>.Default.Equals(LPair.Key, AKey) then
      continue;
    Result := LPair.Value.AsType<T>;
    exit;
  end;
  raise Exception.Create('Key not found');
end;

class operator TTuple<K>.Implicit(const P: TArray<TPair<K, TValue>>): TTuple<K>;
begin
  Result.FTuples := P;
end;

class operator TTuple<K>.Implicit(const P: TTuple<K>): TArray<TPair<K, TValue>>;
begin
  Result := P.FTuples;
end;

function TTuple<K>.Keys(const AKeyArray: array of K): TTuple<K>;
var
  LFor: integer;
  LTuples: TArray<TPair<K, TValue>>;
begin
  SetLength(LTuples, Length(FTuples) + Length(AKeyArray));
  for LFor := Low(FTuples) to High(FTuples) do
    LTuples[LFor] := FTuples[LFor];
  for LFor := 0 to High(AKeyArray) do
    LTuples[High(FTuples) + LFor + 1] := TPair<K, TValue>.Create(AKeyArray[LFor], Default(TValue));
  FTuples := LTuples;
  Result := Self;
end;

class function TTuple<K>.New(const AKeys: array of K;
  const AValues: array of TValue): TTuple<K>;
var
  LPairs: TArray<TPair<K, TValue>>;
  LFor: Integer;
begin
  if Length(AKeys) <> Length(AValues) then
    raise Exception.Create('Number of keys and values must match');
  SetLength(LPairs, Length(AKeys));
  for LFor := 0 to High(AKeys) do
  begin
    LPairs[LFor] := TPair<K, TValue>.Create(AKeys[LFor], AValues[LFor]);
  end;
  Result := TTuple<K>.Create(LPairs);
end;

class operator TTuple<K>.NotEqual(const Left, Right: TTuple<K>): boolean;
begin
  Result := not (Left = Right);
end;

function TTuple<K>.Values(const AValueArray: array of TValue): TTuple<K>;
var
  LFor: Integer;
begin
  if Length(FTuples) <> Length(AValueArray) then
    raise Exception.Create('Number of values does not match the number of keys');
  for LFor := Low(FTuples) to High(FTuples) do
  begin
    FTuples[LFor] := TPair<K, TValue>.Create(FTuples[LFor].Key, AValueArray[LFor]);
  end;
  Result := Self;
end;

end.
