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

unit eclbr.tuple;

interface

uses
  Rtti,
  SysUtils,
  Generics.Collections,
  Generics.Defaults,
  eclbr.std;

type
  Tuple = eclbr.std.Tuple;

  TTuple<K> = record
  strict private
    FTuplesPair: TArray<TPair<K, TValue>>;
    /// <summary>
    ///   Creates a new instance of the TTuple class with the specified key-value pairs.
    /// </summary>
    /// <param name="ATuples">
    ///   An array of TPair instances representing the key-value pairs to be included in the tuple.
    /// </param>
    constructor Create(const ATuples: TArray<TPair<K, TValue>>);
  private
    function GetItem(const AKey: K): TValue;
  public
    class operator Implicit(const P: TTuple<K>): TArray<TPair<K, TValue>>; inline;
    class operator Implicit(const P: TArray<TPair<K, TValue>>): TTuple<K>; inline;
    class operator Equal(const Left, Right: TTuple<K>): Boolean; inline;
    class operator NotEqual(const Left, Right: TTuple<K>): Boolean; inline;

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
    class function New(const AKeys: TArray<K>; const AValues: TArray<TValue>): TTuple<K>; static; inline;

    /// <summary>
    ///   Retrieves the value associated with a specified key from a dictionary.
    /// </summary>
    /// <typeparam name="T">
    ///   The type of the value to retrieve.
    /// </typeparam>
    /// <param name="AKey">
    ///   The key of type K for which to retrieve the associated value.
    /// </param>
    /// <returns>
    ///   The value of type T associated with the specified key in the dictionary.
    /// </returns>
    /// <remarks>
    ///   The Get function allows you to retrieve the value associated with a specified
    ///   key from a dictionary. It returns the value of type T associated with the
    ///   specified key, or a default value if the key is not found in the dictionary.
    /// </remarks>
    function Get<T>(const AKey: K): T; inline;

    /// <summary>
    ///   Returns the number of elements in the collection.
    /// </summary>
    /// <remarks>
    ///   This function returns the current count of elements stored in the collection.
    /// </remarks>
    /// <returns>
    ///   An integer value representing the number of elements in the collection.
    /// </returns>
    function Count: Integer; inline;
    function SetTuple(const AKeys: TArray<K>; const AValues: TArray<TValue>): TTuple<K>; inline;

    property Items[const Key: K]: TValue read GetItem; default;
  end;

  PTuple = ^TTuple;
  TTuple = record
  private
    FTuples: TArray<TValue>;
  private
    function GetItem(const AIndex: Integer): TValue;
    /// <summary>
    ///   Constructs an instance of TTuple with the values provided in ATuples.
    /// </summary>
    /// <param name="ATuples">
    ///   An array of TValue containing the values to be stored in the tuple.
    /// </param>
    constructor Create(const ATuples: TArray<TValue>);
  public
    class operator Implicit(const P: TTuple): TArray<TValue>; inline;
    class operator Implicit(const P: TArray<TValue>): TTuple; inline;
    class operator Equal(const Left, Right: TTuple): Boolean; inline;
    class operator NotEqual(const Left, Right: TTuple): Boolean; inline;

    /// <summary>
    ///   Creates a new instance of TTuple with the values provided in AValues.
    /// </summary>
    /// <param name="AValues">
    ///   An array of TValue containing the values to be stored in the new tuple.
    /// </param>
    class function New(const AValues: TArray<TValue>): TTuple; static; inline;

    /// <summary>
    ///   Retrieves the value at the specified index as a generic type T.
    /// </summary>
    /// <param name="AIndex">
    ///   The index of the value to be retrieved from the tuple.
    /// </param>
    /// <returns>
    ///   The value stored at the specified index, converted to the generic type T.
    /// </returns>
    function Get<T>(const AIndex: Integer): T; inline;

    /// <summary>
    ///   Returns the number of elements in the tuple.
    /// </summary>
    /// <returns>
    ///   The number of elements in the tuple.
    /// </returns>
    function Count: Integer; inline;

    property Items[const Key: Integer]: TValue read GetItem; default;
  end;

implementation

{ TTupla<K, TValue> }

function TTuple<K>.Count: Integer;
begin
  Result := Length(FTuplesPair);
end;

constructor TTuple<K>.Create(const ATuples: TArray<TPair<K, TValue>>);
begin
  FTuplesPair := ATuples;
end;

class operator TTuple<K>.Equal(const Left, Right: TTuple<K>): Boolean;
var
  LComp1: IEqualityComparer<K>;
  LComp2: IEqualityComparer<TValue>;
  LFor: Integer;
begin
  Result := false;
  if Length(Left.FTuplesPair) <> Length(Right.FTuplesPair) then
    exit;
  LComp1 := TEqualityComparer<K>.Default;
  LComp2 := TEqualityComparer<TValue>.Default;
  for LFor := 0 to High(Left.FTuplesPair) do
  begin
    if not LComp1.Equals(Left.FTuplesPair[LFor].Key, Right.FTuplesPair[LFor].Key) or
       not LComp2.Equals(Left.FTuplesPair[LFor].Value, Right.FTuplesPair[LFor].Value) then
    begin
      exit;
    end;
  end;
  Result := True;
end;

function TTuple<K>.SetTuple(const AKeys: TArray<K>;
  const AValues: TArray<TValue>): TTuple<K>;
begin
  Result := TTuple<K>.New(AKeys, AValues);
end;

function TTuple<K>.Get<T>(const AKey: K): T;
var
  LComp: IEqualityComparer<K>;
  LPair: TPair<K, TValue>;
begin
  LComp := TEqualityComparer<K>.Default;
  for LPair in FTuplesPair do
  begin
    if not LComp.Equals(LPair.Key, AKey) then
      continue;
    Result := LPair.Value.AsType<T>;
    exit;
  end;
  raise Exception.Create('Key not found');
end;

function TTuple<K>.GetItem(const AKey: K): TValue;
var
  LComp: IEqualityComparer<K>;
  LPair: TPair<K, TValue>;
begin
  LComp := TEqualityComparer<K>.Default;
  Result := Default(TValue);
  for LPair in FTuplesPair do
  begin
    if not LComp.Equals(LPair.Key, AKey) then
      Continue;
    Result := LPair.Value;
    Break;
  end;
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
    LPairs[LFor] := TPair<K, TValue>.Create(AKeys[LFor], AValues[LFor]);
  Result := TTuple<K>.Create(LPairs);
end;

class operator TTuple<K>.NotEqual(const Left, Right: TTuple<K>): Boolean;
begin
  Result := not (Left = Right);
end;

{ TTuple }

function TTuple.Count: Integer;
begin
  Result := Length(FTuples);
end;

constructor TTuple.Create(const ATuples: TArray<TValue>);
begin
  FTuples := ATuples;
end;

class operator TTuple.Equal(const Left, Right: TTuple): Boolean;
var
  LComp: IEqualityComparer<TValue>;
  LFor: Integer;
begin
  Result := false;
  LComp := TEqualityComparer<TValue>.Default;
  for LFor := 0 to High(Left.FTuples) do
  begin
    if not LComp.Equals(Left.FTuples[LFor], Right.FTuples[LFor]) then
      exit;
  end;
  Result := true;
end;

function TTuple.Get<T>(const AIndex: Integer): T;
begin
  Result := FTuples[AIndex].AsType<T>;
end;

function TTuple.GetItem(const AIndex: Integer): TValue;
begin
  Result := FTuples[AIndex];
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

class operator TTuple.NotEqual(const Left, Right: TTuple): Boolean;
begin
  Result := not (Left = Right);
end;

end.
