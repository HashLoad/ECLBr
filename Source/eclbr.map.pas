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

unit eclbr.map;

interface

uses
  Rtti,
  Classes,
  SysUtils,
  StrUtils,
  TypInfo,
  Generics.Defaults,
  Generics.Collections;

type
  TMapPair<K,V> = record
    Key: K;
    Value: V;
    constructor Create(const AKey: K; const AValue: V);
  end;

  IMapEnumerator<K, V> = interface
    ['{5BC9B8F2-7503-4896-82C6-E8EFA27E9555}']
    function _GetCurrent: TMapPair<K, V>;
    function MoveNext: Boolean;
    property Current: TMapPair<K, V> read _GetCurrent;
  end;

  TDefaultCapacity = class
  public
    class var DefaultCapacity: integer;
  end;

  TMap<K, V> = record
  private
    type
      PItemPair = ^TItemPair;
      TItemPair = record
        HashCode: integer;
        Key: K;
        Value: V;
        constructor Create(const AKey: K; const AValue: V; const AHashCode: integer = -1);
      end;
      TArrayPair = TArray<TItemPair>;
      PArrayPair = ^TArrayPair;

      TMapEnumerator = class(TEnumerator<TMapPair<K,V>>)
      private
        FItems: PArrayPair;
        FIndex: integer;
        function _GetCurrent: TMapPair<K, V>;
        function _IsEquals<T>(const ALeft: T; ARight: T): boolean;
      protected
        function DoGetCurrent: TMapPair<K,V>; override;
        function DoMoveNext: boolean; override;
      public
        constructor Create(const AItems: PArrayPair);
        destructor Destroy; override;
        function MoveNext: boolean;
        property Current: TMapPair<K, V> read _GetCurrent;
      end;
  private
    FMapItems: TArrayPair;
    PMapItems: ^TArrayPair;
    FDefaultCapacity: TDefaultCapacity;
    FCapacity: integer;
    function _GetBucketIndex(const AKey: K; const AHashCode: integer = -1): integer;
    procedure _SetNewLength(const ALength: integer);
    function _GetCount: integer;
    function _Hash(const Key: K): integer;
    function _IsEquals<T>(const ALeft: T; ARight: T): boolean;
    procedure _DoAdd(const AKey: K; const AValue: V; const AIndex: integer);
  public
    class operator Implicit(const V: TMap<K, V>): TArrayPair;
    class operator Implicit(const V: TArrayPair): TMap<K, V>;

    /// <summary>
    ///   Creates and returns a new empty dictionary of type TMap<K, V>.
    /// </summary>
    /// <returns>
    ///   A new empty dictionary of type TMap<K, V>.
    /// </returns>
    /// <remarks>
    ///   This static function creates and returns a new empty dictionary of type TMap<K, V>.
    ///   The dictionary is ready for use and contains no key-value pairs.
    /// </remarks>
    class function Empty: TMap<K, V>; static;

    /// <summary>
    ///   Creates a new map instance initialized with the key-value pairs provided in AValue.
    /// </summary>
    /// <param name="AValue">The initial key-value pairs to populate the map.</param>
    constructor Create(const AValue: TArrayPair); overload;

    /// <summary>
    ///   Creates a new map instance with an initial key-value pair.
    /// </summary>
    /// <param name="AKey">The key for the initial pair.</param>
    /// <param name="AValue">The value for the initial pair.</param>
    constructor Create(const AKey: K; AValue: V); overload;

    /// <summary>
    ///   Iterates through the key-value pairs in the map, applying the specified action to each pair.
    /// </summary>
    /// <param name="AAction">The action to apply to each key-value pair.</param>
    procedure ForEach(const AAction: TProc<K, V>);

    /// <summary>
    ///   Adds all key-value pairs from the specified map to this map.
    /// </summary>
    /// <param name="ACollection">The map containing key-value pairs to add.</param>
    procedure AddRange(const ACollection: TMap<K, V>);

    /// <summary>
    ///   Sets the capacity of the map, allowing it to hold a specified number of key-value pairs.
    /// </summary>
    /// <param name="ACapacity">The desired capacity for the map.</param>
    procedure SetCapacity(const ACapacity: integer);

    /// <summary>
    ///   Sets the default capacity used when resizing the map.
    /// </summary>
    /// <param name="ADefault">The default capacity for the map.</param>
    procedure SetDefaultCapacity(const ADefault: integer);

    /// <summary>
    ///   Inserts a key-value pair at the specified index in the map.
    /// </summary>
    /// <param name="AIndex">The index at which to insert the key-value pair.</param>
    /// <param name="AItem">The key-value pair to insert.</param>
    procedure Insert(const AIndex: integer; const AItem: TItemPair);

    /// <summary>
    ///   Deletes the key-value pair at the specified index in the map.
    /// </summary>
    /// <param name="AIndex">The index of the key-value pair to delete.</param>
    procedure Delete(const AIndex: integer);

    /// <summary>
    ///   Removes all key-value pairs from the map, resetting it to an empty state.
    /// </summary>
    procedure Clear;

    /// <summary>
    ///   Returns an enumerator for iterating through key-value pairs in the map.
    /// </summary>
    function GetEnumerator: TEnumerator<TMapPair<K,V>>;

    /// <summary>
    ///   Retrieves the value associated with the specified key.
    /// </summary>
    /// <param name="AKey">The key to retrieve the value for.</param>
    /// <returns>The value associated with the specified key.</returns>
    function GetValue(const AKey: K): V;

    /// <summary>
    ///   Attempts to retrieve the value associated with the specified key in the dictionary.
    /// </summary>
    /// <param name="AKey">
    ///   The key whose associated value will be retrieved.
    /// </param>
    /// <param name="AValue">
    ///   [out] The value associated with the key if found.
    /// </param>
    /// <returns>
    ///   Returns True if the key was found in the dictionary, and the associated value
    ///   was successfully retrieved. Returns False if the key was not found in the dictionary.
    /// </returns>
    /// <remarks>
    ///   This function allows you to check if a key is present in the dictionary and, if it is,
    ///   retrieve the value associated with that key.
    /// </remarks>
    function TryGetValue(const AKey: K; var AValue: V): boolean;

    /// <summary>
    ///   Retrieves the key-value pair associated with the specified key.
    /// </summary>
    /// <param name="AKey">The key to retrieve the key-value pair for.</param>
    /// <returns>The key-value pair associated with the specified key.</returns>
    function GetPair(const AKey: K): TItemPair;

    /// <summary>
    ///   Adds a key-value pair to the map.
    /// </summary>
    /// <param name="APair">The key-value pair to add.</param>
    /// <returns>The index at which the key-value pair was added.</returns>
    function Add(const APair: TMapPair<K, V>): integer; overload;

    /// <summary>
    ///   Adds a key-value pair to the map.
    /// </summary>
    /// <param name="AKey">The key to add.</param>
    /// <param name="AValue">The value to add.</param>
    /// <returns>The index at which the key-value pair was added.</returns>
    function Add(const AKey: K; const AValue: V): integer; overload;

    /// <summary>
    ///   Adds or updates a key-value pair in the map.
    /// </summary>
    /// <param name="AKey">The key to add or update.</param>
    /// <param name="AValue">The value associated with the key.</param>
    procedure AddOrUpdate(const AKey: K; const AValue: V);

    /// <summary>
    ///   Checks if the map contains the specified key.
    /// </summary>
    /// <param name="AKey">The key to check for.</param>
    /// <returns>True if the map contains the key; otherwise, False.</returns>
    function Contains(const AKey: K): boolean;

    /// <summary>
    ///   Merges key-value pairs from the specified array into the map.
    /// </summary>
    /// <param name="ASourceArray">The array containing key-value pairs to merge.</param>
    /// <returns>The updated map with merged key-value pairs.</returns>
    function Merge(const ASourceArray: TArray<TMapPair<K, V>>): TMap<K, V>;

    /// <summary>
    ///   Filters key-value pairs in the map based on a specified predicate.
    /// </summary>
    /// <param name="APredicate">The predicate function used for filtering.</param>
    /// <returns>The map containing filtered key-value pairs.</returns>
    function Filter(const APredicate: TFunc<K, V, boolean>): TMap<K, V>;

    /// <summary>
    ///   Maps the values of the dictionary to a new dictionary of results.
    /// </summary>
    /// <typeparam name="TResult">The type of the mapped results.</typeparam>
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
    /// <typeparam name="R">
    ///   The type of values in the resulting new map.
    /// </typeparam>
    /// <param name="AMappingFunc">
    ///   The mapping function that transforms each key-value pair from the original map into a value of the specified type R in the new map.
    /// </param>
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
    ///   Removes a key-value pair from the map by its key.
    /// </summary>
    /// <param name="AKey">The key of the key-value pair to remove.</param>
    /// <returns>True if the key-value pair was successfully removed; otherwise, False.</returns>
    function Remove(const AKey: K): boolean;

    /// <summary>
    ///   Retrieves the first key-value pair in the map.
    /// </summary>
    /// <returns>The first key-value pair in the map.</returns>
    function First: TItemPair;

    /// <summary>
    ///   Retrieves the last key-value pair in the map.
    /// </summary>
    /// <returns>The last key-value pair in the map.</returns>
    function Last: TMapPair<K, V>;

    /// <summary>
    ///   Converts the map to a JSON string.
    /// </summary>
    /// <returns>A JSON representation of the map.</returns>
    function ToJson: string;

    /// <summary>
    ///   Converts the map to a string.
    /// </summary>
    /// <returns>A string representation of the map.</returns>
    function ToString: string;

    /// <summary>
    ///   Returns the current capacity of the map.
    /// </summary>
    /// <returns>The current capacity of the map.</returns>
    function Capacity: integer;

    /// <summary>
    ///   Returns the number of key-value pairs in the map.
    /// </summary>
    /// <returns>The number of key-value pairs in the map.</returns>
    function Count: integer;

    /// <summary>
    ///   Returns an array containing all key-value pairs in the map.
    /// </summary>
    /// <remarks>
    ///   The returned array contains all key-value pairs in the map, preserving
    ///   their order. Each element of the array is a pair of type TItemPair, where
    ///   the first item is the key and the second item is the value.
    /// </remarks>
    /// <returns>An array containing all key-value pairs in the map.</returns>
    function ToArray: TArray<TItemPair>;

    /// <summary>
    ///   Provides access to key-value pairs in the map using array indexing.
    /// </summary>
    /// <param name="AKey">The key used for accessing the corresponding value.</param>
    /// <returns>The value associated with the specified key.</returns>
    property Items[const AKey: K]: V read GetValue write AddOrUpdate; default;
  end;

implementation

{ TMap<TKey, TValue> }

function TMap<K, V>._GetCount: integer;
var
  LFor: integer;
begin
  Result := 0;
  for LFor := 0 to High(FMapItems) do
  begin
    if _IsEquals<TItemPair>(FMapItems[LFor], Default(TItemPair)) then
      break
    else
      Inc(Result);
  end;
end;

function TMap<K, V>._GetBucketIndex(const AKey: K; const AHashCode: integer): integer;
//var
//  LLength: integer;
//  LHashCode: integer;
//  LMapItems: PItemPair;
//begin
//  LLength := Length(FMapItems);
//  if LLength = 0 then
//    exit(-1);
//  LHashCode := _Hash(AKey);
//  Result := LHashCode mod Count; //and (LLength - 1);
//  LMapItems := @FMapItems[Result];
//  while True do
//  begin
//    if _IsEquals<K>(LMapItems^.Key, Default(K)) and (LMapItems^.HashCode = 0) then
//      exit(Result);
//
//    if _IsEquals<K>(LMapItems^.Key, AKey) and (LMapItems^.HashCode = LHashCode) then
//      exit(Result);
//
//    Inc(Result);
//    Inc(LMapItems);
//    if Result >= LLength then
//    begin
//      Result := 0;
//      LMapItems := @FMapItems[0];
//    end;
//  end;

var
  LFor: integer;
begin
  Result := -1;
  for LFor := 0 to System.Length(FMapItems) -1 do
  begin
    if _IsEquals<K>(FMapItems[LFor].Key, AKey) and (FMapItems[LFor].HashCode > 0) then
    begin
      Result := LFor;
      break;
    end;
  end;
end;

procedure TMap<K, V>.SetCapacity(const ACapacity: integer);
begin
  FCapacity := ACapacity;
  _SetNewLength(ACapacity);
end;

procedure TMap<K, V>.SetDefaultCapacity(const ADefault: integer);
begin
  FDefaultCapacity.DefaultCapacity := ADefault;
end;

procedure TMap<K, V>._SetNewLength(const ALength: integer);
begin
  SetLength(FMapItems, ALength);
end;

procedure TMap<K, V>.AddRange(const ACollection: TMap<K, V>);
var
  LPair: TItemPair;
begin
  for LPair in ACollection.FMapItems do
  begin
    if _IsEquals<K>(LPair.Key, Default(K)) then
      continue;
    Add(LPair.Key, LPair.Value);
  end;
end;

function TMap<K, V>.GetValue(const AKey: K): V;
var
  LPair: TItemPair;
begin
  LPair := GetPair(AKey);
  if _IsEquals<K>(LPair.Key, AKey) then
    Result := LPair.Value
  else
    Result := Default(V);
end;

class operator TMap<K, V>.Implicit(const V: TMap<K, V>): TArrayPair;
begin
  Result := V.FMapItems;
end;

class operator TMap<K, V>.Implicit(const V: TArrayPair): TMap<K, V>;
begin
  Result.FMapItems := V;
end;

procedure TMap<K, V>.Insert(const AIndex: integer; const AItem: TItemPair);
var
  LFor: integer;
  LLength: integer;
begin
  LLength := FCapacity;
  if LLength = 0 then
  begin
    SetCapacity(FDefaultCapacity.DefaultCapacity);
    LLength := FCapacity;
  end
  else if LLength <= AIndex then
  begin
    LLength := AIndex + FDefaultCapacity.DefaultCapacity;
    SetCapacity(LLength);
  end;
  for LFor := LLength - 1 downto AIndex + 1 do
    FMapItems[LFor] := FMapItems[LFor - 1];
  FMapItems[AIndex] := AItem;
end;

function TMap<K, V>.GetEnumerator: TEnumerator<TMapPair<K,V>>;
begin
  Result := TMapEnumerator.Create(@FMapItems);
end;

function TMap<K, V>.GetPair(const AKey: K): TItemPair;
var
  LIndex: integer;
begin
  LIndex := _GetBucketIndex(AKey);
  if LIndex <> -1 then
    Result := FMapItems[LIndex]
  else
    Result := TItemPair.Create(Default(K), Default(V));
end;

function TMap<K, V>.Add(const APair: TMapPair<K, V>): integer;
var
  LIndex: integer;
  LLength: integer;
begin
  LIndex := -1;
  for LIndex := Low(FMapItems) to High(FMapItems) do
  begin
    if _IsEquals(FMapItems[LIndex], Default(TItemPair)) then
    begin
      FMapItems[LIndex].Key := APair.Key;
      FMapItems[LIndex].Value := APair.Value;
      FMapItems[LIndex].HashCode := _Hash(APair.Key);
      Result := LIndex;
      exit;
    end;
  end;
  _DoAdd(APair.Key, APair.Value, LIndex);

  Result := FCapacity;
end;

function TMap<K, V>.Add(const AKey: K; const AValue: V): integer;
begin
  Result := Add(TMapPair<K, V>.Create(AKey, AValue));
end;

procedure TMap<K, V>.AddOrUpdate(const AKey: K; const AValue: V);
var
  LIndex: integer;
begin
  LIndex := _GetBucketIndex(AKey);
  if LIndex > -1 then
    FMapItems[LIndex] := TItemPair.Create(AKey, AValue, _Hash(AKey))
  else
    Add(TMapPair<K, V>.Create(AKey, AValue));
end;

procedure TMap<K, V>._DoAdd(const AKey: K; const AValue: V; const AIndex: integer);
var
  LLength: integer;
  LIndex: integer;
begin
  LIndex := AIndex;
  if (LIndex = -1) or (LIndex = System.Length(FMapItems)) then
  begin
    LIndex := _GetCount;
    LLength := LIndex + FDefaultCapacity.DefaultCapacity;
    SetCapacity(LLength);
  end;
  FMapItems[LIndex].Key := AKey;
  FMapItems[LIndex].Value := AValue;
  FMapItems[LIndex].HashCode := _Hash(AKey);
end;

function TMap<K, V>.Capacity: integer;
begin
  Result := FCapacity;
end;

procedure TMap<K, V>.Clear;
begin
  FMapItems := nil;
end;

function TMap<K, V>.Contains(const AKey: K): boolean;
begin
  Result := _GetBucketIndex(AKey) <> -1;
end;

constructor TMap<K, V>.Create(const AValue: TArrayPair);
begin
  FMapItems := AValue;
end;

constructor TMap<K, V>.Create(const AKey: K; AValue: V);
begin
  Add(AKey, AValue);
end;

procedure TMap<K, V>.Delete(const AIndex: integer);
var
  LFor: integer;
begin
  for LFor := AIndex + 1 to FCapacity - 1 do
    FMapItems[LFor - 1] := FMapItems[LFor];
  SetCapacity(FCapacity - 1);
end;

class function TMap<K, V>.Empty: TMap<K, V>;
begin
  Result.FMapItems := [];
end;

function TMap<K, V>.Remove(const AKey: K): boolean;
var
  LIndex: integer;
  LCount: integer;
begin
  LIndex := _GetBucketIndex(AKey);
  Result := LIndex >= 0;
  if Result then
    Delete(LIndex);
end;

function TMap<K, V>.Last: TMapPair<K, V>;
var
  LFor: integer;
begin
  for LFor := System.Length(FMapItems) - 1 downto 0 do
  begin
    if _IsEquals<TItemPair>(FMapItems[LFor], Default(TItemPair)) then
      continue;
    Result.Key := FMapItems[LFor].Key;
    Result.Value := FMapItems[LFor].Value;
    exit;
  end;
  raise Exception.Create('No non-empty elements found');
end;

function TMap<K, V>.Count: integer;
begin
  Result := _GetCount;
end;

function TMap<K, V>.Map(const AMappingFunc: TFunc<V, V>): TMap<K, V>;
var
  LPair: TMapPair<K, V>;
begin
  Result := [];
  try
    for LPair in Self do
      Result.Add(LPair.Key, AMappingFunc(LPair.Value));
  except
    raise;
  end;
end;

function TMap<K, V>.Map(const AMappingFunc: TFunc<K, V, V>): TMap<K, V>;
var
  LPair: TMapPair<K, V>;
begin
  Result := [];
  try
    for LPair in Self do
      Result.Add(LPair.Key, AMappingFunc(LPair.Key, LPair.Value));
  except
    raise;
  end;
end;

function TMap<K, V>.Map<R>(const AMappingFunc: TFunc<K, V, R>): TMap<K, R>;
var
  LPair: TMapPair<K, V>;
begin
  Result := [];
  try
    for LPair in Self do
      Result.Add(LPair.Key, AMappingFunc(LPair.Key, LPair.Value));
  except
    raise;
  end;
end;

function TMap<K, V>.Map<R>(const AMappingFunc: TFunc<V, R>): TMap<K, R>;
var
  LPair: TMapPair<K, V>;
begin
  Result := [];
  try
    for LPair in Self do
      Result.Add(LPair.Key, AMappingFunc(LPair.Value));
  except
    raise;
  end;
end;

function TMap<K, V>.Merge(const ASourceArray: TArray<TMapPair<K, V>>): TMap<K, V>;
var
  LItem: TMapPair<K, V>;
begin
  for LItem in ASourceArray do
  begin
    if not Contains(LItem.Key) then
      Add(LItem.Key, LItem.Value);
  end;
  Result := Self;
end;

function TMap<K, V>.Filter(const APredicate: TFunc<K, V, boolean>): TMap<K, V>;
var
  LItem: TItemPair;
begin
  Result := [];
  for LItem in FMapItems do
  begin
    if _IsEquals<TItemPair>(LItem, Default(TItemPair)) then
      continue;
    if APredicate(LItem.Key, LItem.Value) then
      Result.Add(LItem.Key, LItem.Value);
  end;
end;

function TMap<K, V>.First: TItemPair;
begin
  if System.Length(FMapItems) = 0 then
    exit;
  Result := FMapItems[0];
end;

procedure TMap<K, V>.ForEach(const AAction: TProc<K, V>);
var
  LPair: TItemPair;
begin
  for LPair in FMapItems do
    AAction(LPair.Key, LPair.Value);
end;

function TMap<K, V>.ToString: string;
var
  LPair: TItemPair;
  LBuilder: TStringBuilder;
  LKey: TValue;
  LValue: TValue;
begin
  LBuilder := TStringBuilder.Create;
  try
    for LPair in FMapItems do
    begin
      if _IsEquals<TItemPair>(LPair, Default(TItemPair)) then
        continue;
      LKey := TValue.From<K>(LPair.Key);
      LValue := TValue.From<V>(LPair.Value);
      if LKey.IsObject then
        LBuilder.Append(Format('%s=%s ', [LKey.AsObject.ToString, LValue.ToString]))
      else
        LBuilder.Append(Format('%s=%s ', [LKey.ToString, LValue.ToString]));
      LBuilder.AppendLine(TrimRight(''));
    end;
    Result := TrimRight(LBuilder.ToString);
    Result := StringReplace(Result, #$D, '', [rfReplaceAll]);
    Result := StringReplace(Result, #$A, '', [rfReplaceAll]);
  finally
    LBuilder.Free;
  end;
end;

function TMap<K, V>.TryGetValue(const AKey: K; var AValue: V): boolean;
var
  LIndex: integer;
begin
  LIndex := _GetBucketIndex(AKey);
  Result := LIndex >= 0;
  if Result then
    AValue := FMapItems[LIndex].Value
  else
    AValue := Default(V);
end;

function TMap<K, V>.ToArray: TArray<TItemPair>;
var
  LPair: TItemPair;
begin
  SetLength(Result, 0);
  for LPair in FMapItems do
  begin
    if _IsEquals<TItemPair>(LPair, Default(TItemPair)) then
      continue;
    SetLength(Result, System.Length(Result) + 1);
    Result[System.Length(Result) -1] := LPair;
  end;
end;

function TMap<K, V>.ToJson: string;
var
  LPair: TItemPair;
  LJsonPairs: TStringBuilder;
  LKey: TValue;
  LValue: TValue;
  LFirstPair: Boolean;
begin
  LJsonPairs := TStringBuilder.Create;
  LFirstPair := True;
  try
    for LPair in FMapItems do
    begin
      if _IsEquals<TItemPair>(LPair, Default(TItemPair)) then
        continue;
      LKey := TValue.From<K>(LPair.Key);
      LValue := TValue.From<V>(LPair.Value);
      if not LFirstPair then
        LJsonPairs.Append(', ');
      LJsonPairs.Append(Format('"%s": "%s"', [LKey.ToString, LValue.ToString]));
      LFirstPair := False;
    end;
    Result := '{' + LJsonPairs.ToString + '}';
  finally
    LJsonPairs.Free;
  end;
end;

function TMap<K, V>._Hash(const Key: K): integer;
const
  POSITIVEMASK = not integer($80000000);
begin
  Result := POSITIVEMASK and ((POSITIVEMASK and TEqualityComparer<K>.Default.GetHashCode(Key)) + 1);
end;

function TMap<K, V>._IsEquals<T>(const ALeft: T; ARight: T): boolean;
begin
  Result := TEqualityComparer<T>.Default.Equals(ALeft, ARight);
end;

{ TMap<K, V>.TMapEnumerator }

constructor TMap<K, V>.TMapEnumerator.Create(const AItems: PArrayPair);
begin
  FItems := AItems;
  FIndex := -1;
end;

destructor TMap<K, V>.TMapEnumerator.Destroy;
begin
  FItems := nil;
  inherited;
end;

function TMap<K, V>.TMapEnumerator.DoGetCurrent: TMapPair<K, V>;
begin
  Result := _GetCurrent;
end;

function TMap<K, V>.TMapEnumerator.DoMoveNext: boolean;
begin
  Result := MoveNext;
end;

function TMap<K, V>.TMapEnumerator._GetCurrent: TMapPair<K, V>;
begin
  Result.Key := FItems^[FIndex].Key;
  Result.Value := FItems^[FIndex].Value;
end;

function TMap<K, V>.TMapEnumerator._IsEquals<T>(const ALeft: T; ARight: T): boolean;
begin
  Result := TEqualityComparer<T>.Default.Equals(ALeft, ARight);
end;

function TMap<K, V>.TMapEnumerator.MoveNext: boolean;
begin
  repeat
    Inc(FIndex);
  until (FIndex >= System.Length(FItems^)) or
        (not _IsEquals<K>(FItems^[FIndex].Key, Default(K))) or
        (not _IsEquals<V>(FItems^[FIndex].Value, Default(V)));
  Result := FIndex < System.Length(FItems^);
end;

{ TMap<K, V>.TItemPair }

constructor TMap<K, V>.TItemPair.Create(const AKey: K; const AValue: V;
  const AHashCode: integer);
begin
  Key := AKey;
  Value := AValue;
  HashCode := AHashCode;
end;

{ TMapPair<K, V> }

constructor TMapPair<K, V>.Create(const AKey: K; const AValue: V);
begin
  Key := AKey;
  Value := AValue;
end;

initialization
  TDefaultCapacity.DefaultCapacity := 16;

end.

