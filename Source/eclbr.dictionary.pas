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

unit eclbr.dictionary;

interface

uses
  Rtti,
  SysUtils,
  Generics.Defaults,
  Generics.Collections,
  eclbr.list,
  eclbr.map,
  eclbr.std,
  eclbr.vector;

type
  TDictEx<K,V> = class(TDictionary<K,V>)
  strict private
    type
      TItemPair = TPair<K, V>;
  public
    /// <summary>
    ///   Adds the key-value pairs from another dictionary to this dictionary.
    /// </summary>
    /// <param name="ASource">The source dictionary containing key-value pairs to add.</param>
    procedure AddRange(const ASource: TDictionary<K,V>);

    /// <summary>
    ///   Iterates through each key-value pair in the dictionary and performs an action on them.
    /// </summary>
    /// <param name="AAction">The action to be performed on each key-value pair.</param>
    procedure ForEach(const AAction: TProc<K, V>);

    /// <summary>
    ///   Iterates through each key-value pair in the dictionary along with its index and performs an action on them.
    /// </summary>
    /// <param name="AAction">The action to be performed on each indexed key-value pair.</param>
    procedure ForEachIndexed(const AAction: TProc<Integer, K, V>);

    /// <summary>
    ///   Rotates the dictionary by a specified number of positions.
    /// </summary>
    /// <param name="ACount">The number of positions to rotate the dictionary.</param>
    function Rotate(const ACount: Integer): TArray<TPair<K, V>>;

    /// <summary>
    ///   Removes duplicate values from the dictionary, keeping only the first occurrence of each value.
    /// </summary>
    procedure Unique;

    /// <summary>
    ///   Returns the sorted keys of the dictionary.
    /// </summary>
    function SortedKeys: TArray<K>;

    /// <summary>
    ///   Shuffles the keys of the dictionary.
    /// </summary>
    function ShuffleKeys: TArray<K>;

    /// <summary>
    ///   Maps the values of the dictionary to a new dictionary of results.
    /// </summary>
    /// <typeparam name="R">The type of the mapped results.</typeparam>
    /// <param name="AMappingFunc">The mapping function to be applied to each value.</param>
    function Map(const AMappingFunc: TFunc<V, V>): TMap<K, V>; overload;

    /// <summary>
    ///   Creates a new map resulting from the application of a mapping function to each value in the original map.
    /// </summary>
    /// <typeparam name="R">
    ///   The type of values in the resulting new map.
    /// </typeparam>
    /// <param name="AMappingFunc">
    ///   The mapping function that transforms each value from the original map into a value of the specified type R in the new map.
    /// </param>
    /// <returns>
    ///   A new map containing the values mapped from the original map.
    /// </returns>
    /// <remarks>
    ///   This function creates a new map resulting from the application of the specified mapping function to each value
    ///   in the original map. It does not modify the original map but instead returns a new map with the mapped values.
    ///   The mapping function should accept a value of type V and return a value of the type specified by R.
    /// </remarks>
    function Map<R>(const AMappingFunc: TFunc<V, R>): TMap<K, R>; overload;

    /// <summary>
    ///   Creates a new map resulting from the application of a mapping function to each key-value pair in the original map.
    /// </summary>
    /// <param name="AMappingFunc">
    ///   The mapping function that transforms each key-value pair from the original map into a new value in the new map.
    /// </param>
    /// <returns>
    ///   A new map containing the values mapped from the original map.
    /// </returns>
    /// <remarks>
    ///   This function creates a new map resulting from the application of the specified mapping function to each key-value pair
    ///   in the original map. It does not modify the original map but instead returns a new map with the mapped values.
    ///   The mapping function should accept a key of type K, a value of type V, and return a new value of type V.
    /// </remarks>
    function Map(const AMappingFunc: TFunc<K, V, V>): TMap<K, V>; overload;

    /// <summary>
    ///   Creates a new map resulting from the application of a mapping function to each key-value pair in the original map.
    /// </summary>
    /// <param name="AMappingFunc">
    ///   The mapping function that transforms each key-value pair from the original map into a value in the new map.
    /// </param>
    /// <typeparam name="R">
    ///   The type of values in the resulting new map.
    /// </typeparam>
    /// <returns>
    ///   A new map containing the values mapped from the original map.
    /// </returns>
    /// <remarks>
    ///   This function creates a new map resulting from the application of the specified mapping function to each key-value pair
    ///   in the original map. It does not modify the original map but instead returns a new map with the mapped values.
    ///   The mapping function should accept a key of type K, a value of type V, and return a value of the type specified by R.
    /// </remarks>
    function Map<R>(const AMappingFunc: TFunc<K, V, R>): TMap<K, R>; overload;

    /// <summary>
    ///   Filters the values of the dictionary based on a specified predicate.
    /// </summary>
    /// <param name="APredicate">The predicate used to filter the values.</param>
    function Filter(const APredicate: TFunc<K, V, Boolean>): TMap<K, V>;

    /// <summary>
    ///   Reduces the values of the dictionary to a single value using an accumulator function.
    /// </summary>
    /// <typeparam name="R">The type of the reduction result.</typeparam>
    /// <param name="AAccumulator">The accumulator function used to reduce the values.</param>
    function Reduce(const AAccumulator: TFunc<V, V, V>): V;

    /// <summary>
    ///   Groups the values of the dictionary by a key selector and returns a new dictionary with grouped value lists.
    /// </summary>
    /// <typeparam name="TKey">The type of the grouping key.</typeparam>
    /// <param name="AKeySelector">The key selector function.</param>
    function GroupBy<TKey>(const AKeySelector: TFunc<V, TKey>): TMap<TKey, TVector<V>>;

    /// <summary>
    ///   Joins the values of the dictionary into a single String, separated by the specified separator.
    /// </summary>
    /// <param name="ASeparator">The separator used to join the values.</param>
    function Join(const ASeparator: String): String;

    /// <summary>
    ///   Partitions the dictionary into two dictionaries based on a given predicate.
    /// </summary>
    /// <param name="APredicate">The predicate used to partition the dictionary.</param>
    /// <returns>
    ///   A pair of dictionaries where the first dictionary contains key-value pairs that satisfy the predicate,
    ///   and the second dictionary contains key-value pairs that do not satisfy the predicate.
    /// </returns>
    function Partition(const APredicate: TPredicate<V>): TPair<TMap<K, V>, TMap<K, V>>;

    /// <summary>
    ///   Takes a specified number of key-value pairs from the beginning of the dictionary.
    /// </summary>
    /// <param name="ACount">The number of key-value pairs to take.</param>
    /// <returns>A new dictionary with the specified number of key-value pairs from the start.</returns>
    function Take(const ACount: Integer): TMap<K, V>;

    /// <summary>
    ///   Skips a specified number of key-value pairs from the beginning of the dictionary and returns the remaining pairs.
    /// </summary>
    /// <param name="ACount">The number of key-value pairs to skip.</param>
    /// <returns>A new dictionary with key-value pairs after skipping the specified number of pairs.</returns>
    function Skip(const ACount: Integer): TMap<K, V>;

    /// <summary>
    ///   Creates a new dictionary containing key-value pairs from the specified start index to the end index.
    /// </summary>
    /// <param name="AStartIndex">The starting index for slicing.</param>
    /// <param name="AEndIndex">The ending index for slicing.</param>
    /// <returns>A new dictionary with the sliced key-value pairs.</returns>
    function Slice(const AStartIndex: Integer; const AEndIndex: Integer): TMap<K, V>;

    /// <summary>
    ///   Combines two dictionaries with a specified function to create a new dictionary.
    /// </summary>
    /// <typeparam name="T1">The value type of the first dictionary.</typeparam>
    /// <typeparam name="T2">The value type of the second dictionary.</typeparam>
    /// <typeparam name="R">The value type of the result dictionary.</typeparam>
    /// <param name="AList1">The first dictionary to combine.</param>
    /// <param name="AList2">The second dictionary to combine.</param>
    /// <param name="AFunc">The function to apply to each pair of values.</param>
    /// <returns>A new dictionary with values resulting from applying the function to corresponding pairs.</returns>
    function Zip<T, R>(const AList: TDictEx<K, T>;
      const AFunc: TFunc<V, T, R>): TMap<K, R>;

    /// <summary>
    ///   Maps each value in the dictionary to an array of results and flattens the results into a single dictionary.
    /// </summary>
    /// <typeparam name="R">The value type of the result dictionary.</typeparam>
    /// <param name="AFunc">The function to map each value to an array of results.</param>
    /// <returns>A new dictionary containing flattened results.</returns>
    function FlatMap<R>(const AFunc: TFunc<TValue, TArray<R>>): TMap<K, R>;

    /// <summary>
    ///   Returns a new dictionary containing key-value pairs that are common between two dictionaries.
    /// </summary>
    /// <param name="AOtherDict">The other dictionary to intersect with.</param>
    /// <returns>A new dictionary containing common key-value pairs.</returns>
    function Intersect(const AOtherDict: TDictEx<K, V>): TMap<K, V>;

    /// <summary>
    ///   Returns a new dictionary containing key-value pairs that exist in the current dictionary but not in another dictionary.
    /// </summary>
    /// <param name="AOtherDict">The other dictionary to compare.</param>
    /// <returns>A new dictionary containing key-value pairs not present in the other dictionary.</returns>
    function &Except(const AOtherDict: TDictEx<K, V>): TMap<K, V>;

    /// <summary>
    ///   Returns the maximum key in the dictionary.
    /// </summary>
    /// <returns>The maximum key in the dictionary.</returns>
    function MaxKey: K;

    /// <summary>
    ///   Returns the minimum key in the dictionary.
    /// </summary>
    /// <returns>The minimum key in the dictionary.</returns>
    function MinKey: K;

    /// <summary>
    ///   Returns a new dictionary with distinct keys based on the selected key selector function.
    /// </summary>
    /// <typeparam name="TKey">The type of the key used for distinct selection.</typeparam>
    /// <param name="AKeySelector">The function used to select keys for distinct values.</param>
    /// <returns>A new dictionary with distinct keys.</returns>
    function DistinctBy<TKey>(const AKeySelector: TFunc<K, TKey>): TMap<TKey, V>;

    /// <summary>
    ///   Returns a new dictionary containing key-value pairs that satisfy the given predicate.
    /// </summary>
    /// <param name="APredicate">The predicate used to filter key-value pairs.</param>
    /// <returns>A new dictionary containing filtered key-value pairs.</returns>
    function FindAll(const APredicate: TPredicate<V>): TMap<K, V>;

    /// <summary>
    ///   Returns a new dictionary containing key-value pairs from the beginning of the dictionary
    ///   while the specified predicate is True.
    /// </summary>
    /// <param name="APredicate">The predicate used to take key-value pairs.</param>
    /// <returns>A new dictionary containing key-value pairs that match the predicate.</returns>
    function TakeWhile(const APredicate: TPredicate<K>): TMap<K, V>;

    /// <summary>
    ///   Skips key-value pairs from the beginning of the dictionary while the specified predicate is True
    ///   and returns the remaining key-value pairs.
    /// </summary>
    /// <param name="APredicate">The predicate used to skip key-value pairs.</param>
    /// <returns>A new dictionary containing key-value pairs after skipping while the predicate is True.</returns>
    function SkipWhile(const APredicate: TPredicate<K>): TMap<K, V>;

    /// <summary>
    ///   Partitions the dictionary into two groups based on the given predicate.
    ///   Keys that satisfy the predicate are placed in one group, and keys that do not satisfy it are placed in another group.
    /// </summary>
    /// <param name="APredicate">The predicate used for partitioning.</param>
    /// <returns>
    ///   A new dictionary with two entries: one entry for keys that satisfy the predicate (True) and another for keys that do not (False).
    /// </returns>
    function PartitionBy(const APredicate: TPredicate<V>): TMap<Boolean, TVector<V>>;

    /// <summary>
    ///   Returns the maximum value in the dictionary.
    /// </summary>
    /// <returns>The maximum value in the dictionary.</returns>
    function MaxValue: V;

    /// <summary>
    ///   Returns the minimum value in the dictionary.
    /// </summary>
    /// <returns>The minimum value in the dictionary.</returns>
    function MinValue: V;

    /// <summary>
    ///   Returns a String representation of the dictionary.
    /// </summary>
    /// <returns>A String representation of the key-value pairs in the dictionary.</returns>
    function ToString: String; override;
  end;

implementation

{ TDictionaryHelper<K, V> }

function TDictEx<K, V>.DistinctBy<TKey>(
  const AKeySelector: TFunc<K, TKey>): TMap<TKey, V>;
var
  LPair: TPair<K, V>;
  LKey: TKey;
begin
  Result := [];
  for LPair in Self do
  begin
    LKey := AKeySelector(LPair.Key);
    if not Result.Contains(LKey) then
      Result.Add(LKey, LPair.Value);
  end;
end;

function TDictEx<K, V>.&Except(
  const AOtherDict: TDictEx<K, V>): TMap<K, V>;
var
  LPair: TPair<K, V>;
begin
  Result := [];
  for LPair in Self do
  begin
    if not AOtherDict.ContainsKey(LPair.Key) then
      Result.Add(LPair.Key, LPair.Value);
  end;
end;

procedure TDictEx<K, V>.AddRange(const ASource: TDictionary<K, V>);
var
  LPair: TPair<K, V>;
begin
  for LPair in ASource do
    Self.Add(LPair.Key, LPair.Value);
end;

function TDictEx<K, V>.Filter(
  const APredicate: TFunc<K, V, Boolean>): TMap<K, V>;
var
  LPair: TPair<K, V>;
begin
  Result := [];
  for LPair in Self do
  begin
    if APredicate(LPair.Key, LPair.Value) then
      Result.Add(LPair.Key, LPair.Value);
  end;
end;

function TDictEx<K, V>.FindAll(
  const APredicate: TPredicate<V>): TMap<K, V>;
var
  LPair: TPair<K, V>;
begin
  Result := [];
  for LPair in Self do
  begin
    if APredicate(LPair.Value) then
      Result.Add(LPair.Key, LPair.Value);
  end;
end;

function TDictEx<K, V>.FlatMap<R>(
  const AFunc: TFunc<TValue, TArray<R>>): TMap<K, R>;
var
  LKey: K;
  LValue: TValue;
  LResult: R;
  LSortedKeys: TArray<K>;
  LResultArray: TArray<R>;
begin
  Result := [];
  LSortedKeys := SortedKeys;
  for LKey in LSortedKeys do
  begin
    LValue := TValue.From<V>(Self[LKey]);
    LResultArray := AFunc(LValue);
    for LResult in LResultArray do
      Result.Add(TValue.FromVariant(Result.Count + 1).AsType<K>, LResult);
  end;
end;

procedure TDictEx<K, V>.ForEach(const AAction: TProc<K, V>);
var
  LPair: TPair<K, V>;
begin
  for LPair in Self do
    AAction(LPair.Key, LPair.Value);
end;

procedure TDictEx<K, V>.ForEachIndexed(
  const AAction: TProc<Integer, K, V>);
var
  LIndex: Integer;
  LSortedKeys: TArray<K>;
  LKey: K;
begin
  LSortedKeys := SortedKeys;
  for LIndex := 0 to Length(LSortedKeys) - 1 do
  begin
    LKey := LSortedKeys[LIndex];
    AAction(LIndex, LKey, Self[LKey]);
  end;
end;

function TDictEx<K, V>.GroupBy<TKey>(
  const AKeySelector: TFunc<V, TKey>): TMap<TKey, TVector<V>>;
var
  LPair: TPair<K, V>;
  LKey: TKey;
  LList: TVector<V>;
begin
  Result := [];
  for LPair in Self do
  begin
    LKey := AKeySelector(LPair.Value);
    if not Result.TryGetValue(LKey, LList) then
    begin
      LList := TVector<V>.Create([]);
      Result.Add(LKey, LList);
    end;
    LList.Add(LPair.Value);
    Result[LKey] := LList;
  end;
end;

function TDictEx<K, V>.Intersect(
  const AOtherDict: TDictEx<K, V>): TMap<K, V>;
var
  LPair: TPair<K, V>;
begin
  Result := [];
  for LPair in Self do
  begin
    if AOtherDict.ContainsKey(LPair.Key) then
      Result.Add(LPair.Key, LPair.Value);
  end;
end;

function TDictEx<K, V>.Join(const ASeparator: String): String;
var
  LSortKeys: TArray<K>;
  LFor: Integer;
begin
  Result := '';
  LSortKeys := SortedKeys;
  for LFor := Low(LSortKeys) to High(LSortKeys) do
  begin
    if Result <> '' then
      Result := Result + ASeparator;
    Result := Result + TValue.From<K>(LSortKeys[LFor]).ToString + ': ' + TValue.From<V>(Self[LSortKeys[LFor]]).ToString;
  end;
end;

function TDictEx<K,V>.MaxValue: V;
var
  LPair: TPair<K, V>;
  LMaxValue: V;
  LIsFirst: Boolean;
begin
  LMaxValue := Default(V);
  LIsFirst := True;
  for LPair in Self do
  begin
    if LIsFirst or (TComparer<V>.Default.Compare(LPair.Value, LMaxValue) > 0) then
    begin
      LMaxValue := LPair.Value;
      LIsFirst := False;
    end;
  end;
  if LIsFirst then
    raise Exception.Create('The dictionary is empty.');
  Result := LMaxValue;
end;

function TDictEx<K, V>.MinValue: V;
var
  LPair: TPair<K, V>;
  LMinValue: V;
  LIsFirst: Boolean;
begin
  if Count = 0 then
    raise Exception.Create('The dictionary is empty.');

  LMinValue := Default(V);
  LIsFirst := True;
  for LPair in Self do
  begin
    if LIsFirst or (TComparer<V>.Default.Compare(LPair.Value, LMinValue) < 0) then
    begin
      LMinValue := LPair.Value;
      LIsFirst := False;
    end;
  end;
  if LIsFirst then
    raise Exception.Create('No minimum key found in the dictionary.');
  Result := LMinValue;
end;

function TDictEx<K, V>.Map(
  const AMappingFunc: TFunc<V, V>): TMap<K, V>;
var
  LPair: TPair<K, V>;
begin
  Result := [];
  for LPair in Self do
    Result.Add(LPair.Key, AMappingFunc(LPair.Value));
end;

function TDictEx<K, V>.Map(
  const AMappingFunc: TFunc<K, V, V>): TMap<K, V>;
var
  LPair: TPair<K, V>;
begin
  Result := [];
  for LPair in Self do
    Result.Add(LPair.Key, AMappingFunc(LPair.Key, LPair.Value));
end;

function TDictEx<K, V>.Map<R>(
  const AMappingFunc: TFunc<K, V, R>): TMap<K, R>;
var
  LPair: TPair<K, V>;
begin
  Result := [];
  for LPair in Self do
    Result.Add(LPair.Key, AMappingFunc(LPair.Key, LPair.Value));
end;

function TDictEx<K, V>.Map<R>(
  const AMappingFunc: TFunc<V, R>): TMap<K, R>;
var
  LPair: TPair<K, V>;
begin
  Result := [];
  for LPair in Self do
    Result.Add(LPair.Key, AMappingFunc(LPair.Value));
end;

function TDictEx<K, V>.MaxKey: K;
var
  LPair: TPair<K, V>;
  LMaxKey: K;
  LIsFirst: Boolean;
begin
  LMaxKey := Default(K);
  LIsFirst := True;
  for LPair in Self do
  begin
    if LIsFirst or (TComparer<K>.Default.Compare(LPair.Key, LMaxKey) > 0) then
    begin
      LMaxKey := LPair.Key;
      LIsFirst := False;
    end;
  end;
  if LIsFirst then
    raise Exception.Create('The dictionary is empty.');
  Result := LMaxKey;
end;

function TDictEx<K, V>.MinKey: K;
var
  LPair: TPair<K, V>;
  LMinKey: K;
  LIsFirst: Boolean;
begin
  if Count = 0 then
    raise Exception.Create('The dictionary is empty.');

  LMinKey := Default(K);
  LIsFirst := True;
  for LPair in Self do
  begin
    if LIsFirst or (TComparer<K>.Default.Compare(LPair.Key, LMinKey) < 0) then
    begin
      LMinKey := LPair.Key;
      LIsFirst := False;
    end;
  end;
  if LIsFirst then
    raise Exception.Create('No minimum key found in the dictionary.');
  Result := LMinKey;
end;

function TDictEx<K, V>.Partition(
  const APredicate: TPredicate<V>): TPair<TMap<K, V>, TMap<K, V>>;
var
  LPair: TPair<K, V>;
  LTrueMap, LFalseMap: TMap<K, V>;
begin
  LTrueMap := [];
  LFalseMap := [];
  for LPair in Self do
  begin
    if APredicate(LPair.Value) then
      LTrueMap.Add(LPair.Key, LPair.Value)
    else
      LFalseMap.Add(LPair.Key, LPair.Value);
  end;
  Result := TPair<TMap<K, V>, TMap<K, V>>.Create(LTrueMap, LFalseMap);
end;

function TDictEx<K, V>.PartitionBy(
  const APredicate: TPredicate<V>): TMap<Boolean, TVector<V>>;
var
  LSortedKeys: TArray<K>;
  LKey: K;
  LValue: V;
  LResult: Boolean;
  LList: TVector<V>;
begin
  Result := [];
  LSortedKeys := Self.SortedKeys;
  for LKey in LSortedKeys do
  begin
    LValue := Self[LKey];
    LResult := APredicate(LValue);
    if not Result.Contains(LResult) then
    begin
      LList := TVector<V>.Create([]);
      Result.Add(LResult, LList);
    end;
    LList.Add(LValue);
    Result[LResult] := LList;
  end;
end;

function TDictEx<K, V>.Reduce(const AAccumulator: TFunc<V, V, V>): V;
var
  LPair: TPair<K, V>;
  LAccumulatedValue: V;
begin
  LAccumulatedValue := Default(V);
  for LPair in Self do
  begin
    LAccumulatedValue := AAccumulator(LAccumulatedValue, LPair.Value);
  end;
  Result := LAccumulatedValue;
end;

function TDictEx<K, V>.Rotate(const ACount: Integer): TArray<TPair<K, V>>;
var
  LSortedKeysArray: TArray<K>;
  LIndex, LNewIndex: Integer;
begin
  LSortedKeysArray := SortedKeys;
  SetLength(Result, Length(LSortedKeysArray));

  for LIndex := 0 to High(LSortedKeysArray) do
  begin
    LNewIndex := (LIndex + (ACount + Length(LSortedKeysArray))) mod Length(LSortedKeysArray);
    Result[LNewIndex] := TPair<K, V>.Create(LSortedKeysArray[LIndex], Self[LSortedKeysArray[LIndex]]);
  end;
end;

function TDictEx<K, V>.ShuffleKeys: TArray<K>;
var
  LKeysList: TListEx<K>;
begin
  LKeysList := TListEx<K>.Create(Self.Keys.ToArray);
  try
    LKeysList.Shuffle;
    Result := LKeysList.ToArray;
  finally
    LKeysList.Free;
  end;
end;

function TDictEx<K, V>.Skip(const ACount: Integer): TMap<K, V>;
var
  LSortedKeys: TArray<K>;
  LIndex: Integer;
begin
  Result := [];
  LSortedKeys := Self.SortedKeys;
  for LIndex := ACount to Length(LSortedKeys) - 1 do
    Result.Add(LSortedKeys[LIndex], Self[LSortedKeys[LIndex]]);
end;

function TDictEx<K, V>.SkipWhile(
  const APredicate: TPredicate<K>): TMap<K, V>;
var
  LKey: K;
  LFound: Boolean;
  LSortedKeys: TArray<K>;
begin
  Result := [];
  LFound := False;
  LSortedKeys := SortedKeys;
  for LKey in LSortedKeys do
  begin
    if not LFound and not APredicate(LKey) then
      LFound := True;
    if LFound then
      Result.Add(LKey, Self[LKey]);
  end;
end;

function TDictEx<K, V>.Slice(const AStartIndex: Integer;
  const AEndIndex: Integer): TMap<K, V>;
var
  LSortedKeys: TArray<K>;
  LIndex: Integer;
begin
  Result := [];
  LSortedKeys := Self.SortedKeys;
  for LIndex := AStartIndex to TStd.Min(AEndIndex, High(LSortedKeys)) do
    Result.Add(LSortedKeys[LIndex], Self[LSortedKeys[LIndex]]);
end;

function TDictEx<K, V>.SortedKeys: TArray<K>;
var
  LList: TList<K>;
begin
  LList := TList<K>.Create(Self.Keys);
  try
    LList.Sort;
    Result := LList.ToArray;
  finally
    LList.Free;
  end;
end;

function TDictEx<K, V>.Take(const ACount: Integer): TMap<K, V>;
var
  LSortedKeys: TArray<K>;
  LKey: K;
begin
  Result := [];
  LSortedKeys := Self.SortedKeys;
  for LKey in LSortedKeys do
  begin
    if Result.Count >= ACount then
      break;
    if Self.ContainsKey(LKey) then
      Result.Add(LKey, Self[LKey]);
  end;
end;


function TDictEx<K, V>.TakeWhile(
  const APredicate: TPredicate<K>): TMap<K, V>;
var
  LPair: TPair<K, V>;
begin
  Result := [];
  for LPair in Self do
  begin
    if APredicate(LPair.Key) then
      Result.Add(LPair.Key, LPair.Value)
  end;
end;

function TDictEx<K, V>.ToString: String;
var
  LPair: TPair<K, V>;
  LResultBuilder: TStringBuilder;
  LKey: TValue;
  LValue: TValue;
begin
  LResultBuilder := TStringBuilder.Create;
  try
    for LPair in Self do
    begin
      LKey := TValue.From<K>(LPair.Key);
      LValue := TValue.From<V>(LPair.Value);
      if LKey.IsObject then
        LResultBuilder.AppendLine(Format('%s=%s', [LKey.AsObject.ToString, LValue.ToString]))
      else
        LResultBuilder.AppendLine(Format('%s=%s', [LKey.ToString, LValue.ToString]));
    end;
    Result := TrimRight(LResultBuilder.ToString);
  finally
    LResultBuilder.Free;
  end;
end;

procedure TDictEx<K, V>.Unique;
var
  LUniqueValues: TDictionary<V, Boolean>;
  LPair: TPair<K, V>;
begin
  LUniqueValues := TDictionary<V, Boolean>.Create;
  try
    for LPair in Self do
    begin
      if not LUniqueValues.ContainsKey(LPair.Value) then
        LUniqueValues.AddOrSetValue(LPair.Value, True)
      else
        Self.Remove(LPair.Key);
    end;
  finally
    LUniqueValues.Free;
  end;
end;

function TDictEx<K, V>.Zip<T, R>(const AList: TDictEx<K, T>;
  const AFunc: TFunc<V, T, R>): TMap<K, R>;
var
  LKey: K;
  LValue1: V;
  LValue2: T;
begin
  Result := [];
  for LKey in Self.Keys do
  begin
    if AList.TryGetValue(LKey, LValue2) then
    begin
      LValue1 := Self[LKey];
      Result.Add(LKey, AFunc(LValue1, LValue2));
    end;
  end;
end;

end.
