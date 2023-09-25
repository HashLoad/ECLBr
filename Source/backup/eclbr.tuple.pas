{
             ECL Brasil - Essential Core Library for Delphi

                   Copyright (c) 2022, Isaque Pinheiro
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
  @Telegram(https://t.me/ormbr)
}

unit eclbr.tuple;

{$if defined(fpc)}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Rtti,
  SysUtils,
  Generics.Collections,
  Generics.Defaults;

type
  TTuple<K> = packed record
  private
    FTuplesPair: TArray<TPair<K, TValue>>;
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
    class function New(const AKeys: TArray<K>; const AValues: TArray<TValue>): TTuple<K>; static;

    /// <summary>
    ///   Obtém o valor associado à chave especificada.
    /// </summary>
    /// <param name="AKey">A chave para a qual se deseja obter o valor correspondente.</param>
    /// <returns>O valor associado à chave especificada.</returns>
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

  TTuple = packed record
  private
    FTuples: TArray<TValue>;
  private
    /// <summary>
    ///   Constructs an instance of TTuple with the values provided in ATuples.
    /// </summary>
    /// <param name="ATuples">
    ///   An array of TValue containing the values to be stored in the tuple.
    /// </param>
    constructor Create(const ATuples: TArray<TValue>);
  public
    /// <summary>
    ///   Implicitly converts an instance of TTuple into an array of TValue.
    /// </summary>
    /// <param name="P">
    ///   The TTuple instance to be converted into an array of TValue.
    /// </param>
    class operator Implicit(const P: TTuple): TArray<TValue>;

    /// <summary>
    ///   Implicitly converts an array of TValue into an instance of TTuple.
    /// </summary>
    /// <param name="P">
    ///   The array of TValue to be converted into an instance of TTuple.
    /// </param>
    class operator Implicit(const P: TArray<TValue>): TTuple;

    /// <summary>
    ///   Checks if two instances of TTuple are equal.
    /// </summary>
    /// <param name="Left">
    ///   The first TTuple instance to be compared.
    /// </param>
    /// <param name="Right">
    ///   The second TTuple instance to be compared.
    /// </param>
    class operator Equal(const Left, Right: TTuple): boolean;

    /// <summary>
    ///   Checks if two instances of TTuple are not equal.
    /// </summary>
    /// <param name="Left">
    ///   The first TTuple instance to be compared.
    /// </param>
    /// <param name="Right">
    ///   The second TTuple instance to be compared.
    /// </param>
    class operator NotEqual(const Left, Right: TTuple): boolean;

    /// <summary>
    ///   Creates a new instance of TTuple with the values provided in AValues.
    /// </summary>
    /// <param name="AValues">
    ///   An array of TValue containing the values to be stored in the new tuple.
    /// </param>
    class function New(const AValues: TArray<TValue>): TTuple; static;

    /// <summary>
    ///   Retrieves the value at the specified index as a generic type T.
    /// </summary>
    /// <param name="AIndex">
    ///   The index of the value to be retrieved from the tuple.
    /// </param>
    /// <returns>
    ///   The value stored at the specified index, converted to the generic type T.
    /// </returns>
    function Get<T>(const AIndex: integer): T;

    /// <summary>
    ///   Returns the number of elements in the tuple.
    /// </summary>
    /// <returns>
    ///   The number of elements in the tuple.
    /// </returns>
    function Count: integer;
  end;

implementation

{ TTupla<K, TValue> }

function TTuple<K>.Count: integer;
begin
  Result := Length(FTuplesPair);
end;

constructor TTuple<K>.Create(const ATuples: TArray<TPair<K, TValue>>);
begin
  FTuplesPair := ATuples;
end;

class operator TTuple<K>.Equal(const Left, Right: TTuple<K>): boolean;
var
  LFor: Integer;
begin
  Result := false;
  if Length(Left.FTuplesPair) <> Length(Right.FTuplesPair) then
    exit;
  for LFor := 0 to High(Left.FTuplesPair) do
  begin
    if not TEqualityComparer<K>.Default.Equals(Left.FTuplesPair[LFor].Key, Right.FTuplesPair[LFor].Key) or
       not TEqualityComparer<TValue>.Default.Equals(Left.FTuplesPair[LFor].Value, Right.FTuplesPair[LFor].Value) then
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
  for LPair in FTuplesPair do
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
  Result.FTuplesPair := P;
end;

class operator TTuple<K>.Implicit(const P: TTuple<K>): TArray<TPair<K, TValue>>;
begin
  Result := P.FTuplesPair;
end;

class function TTuple<K>.New(const AKeys: TArray<K>;
  const AValues: TArray<TValue>): TTuple<K>;
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

{ TTuple }

function TTuple.Count: integer;
begin
  Result := Length(FTuples);
end;

constructor TTuple.Create(const ATuples: TArray<TValue>);
begin
  FTuples := ATuples;
end;

class operator TTuple.Equal(const Left, Right: TTuple): boolean;
var
  LFor: Integer;
begin
  Result := false;
  for LFor := 0 to High(Left.FTuples) do
  begin
    if not TEqualityComparer<TValue>.Default.Equals(Left.FTuples[LFor], Right.FTuples[LFor]) then
      exit;
  end;
  Result := True;
end;

function TTuple.Get<T>(const AIndex: integer): T;
begin
  Result := FTuples[AIndex].AsType<T>;
end;

class operator TTuple.Implicit(const P: TTuple): TArray<TValue>;
begin
  Result := P.FTuples;
end;

class operator TTuple.Implicit(const P: TArray<TValue>): TTuple;
begin
  Result.FTuples := P;
end;

class function TTuple.New(const AValues: TArray<TValue>): TTuple;
begin
  Result := TTuple.Create(AValues);
end;

class operator TTuple.NotEqual(const Left, Right: TTuple): boolean;
begin
  Result := not (Left = Right);
end;

end.
