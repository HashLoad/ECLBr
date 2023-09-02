{
             ECL Brasil - Essential Core Library for Delphi

                   Copyright (c) 2016, Isaque Pinheiro
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

unit eclbr.sysdictionary;

interface

uses
  Rtti,
  SysUtils,
  eclbr.syslist,
  Generics.Defaults,
  Generics.Collections;

type
  TDictionaryHelper<K,V> = class(TDictionary<K,V>)
  private
    function _ComparePairs(const Left, Right: TPair<K, V>): Integer;
    procedure SortByKey(const ASelector: TFunc<K, V, TValue>);
  public
    procedure AddDictionary(const ASource: TDictionary<K,V>);
    procedure ForEach(const AAction: TProc<K, V>);
    procedure ForEachIndexed(const AAction: TProc<Integer, K, V>);
    procedure Rotate(const ACount: Integer);
    procedure Unique;
    function SortedKeys: TArray<K>;
    function ShuffleKeys: TArray<K>;
    function Map<TResult>(const AMappingFunc: TFunc<V, TResult>): TDictionaryHelper<K, TResult>;
    function Filter(const APredicate: TFunc<V, Boolean>): TDictionaryHelper<K, V>;
    function Reduce(const AAccumulator: TFunc<V, V, V>): V;
    function GroupBy<TKey>(const AKeySelector: TFunc<V, TKey>): TDictionary<TKey, TList<V>>;
    function Join(const ASeparator: string): string;
    function Partition(const APredicate: TFunc<V, Boolean>): TPair<TDictionaryHelper<K, V>, TDictionaryHelper<K, V>>;
    function Take(const ACount: Integer): TDictionaryHelper<K, V>;
    function Skip(const ACount: Integer): TDictionaryHelper<K, V>;
    function Slice(AStartIndex, AEndIndex: Integer): TDictionaryHelper<K, V>;
    function Zip<T1, T2, TResult>(const AList1: TDictionaryHelper<K, T1>; const AList2: TDictionaryHelper<K, T2>;
      const AFunc: TFunc<T1, T2, TResult>): TDictionaryHelper<K, TResult>;
    function FlatMap<TResult>(const AFunc: TFunc<TValue, TArray<TResult>>): TDictionaryHelper<K, TResult>;
    function Intersect(const AOtherDict: TDictionaryHelper<K, V>): TDictionaryHelper<K, V>;
    function &Except(const AOtherDict: TDictionaryHelper<K, V>): TDictionaryHelper<K, V>;
    function Max: K;
    function Min: K;
    function DistinctBy<TKey>(const AKeySelector: TFunc<K, TKey>): TDictionary<TKey, V>;
    function FindAll(const APredicate: TFunc<V, Boolean>): TDictionary<K, V>;
    function TakeWhile(const APredicate: TFunc<K, Boolean>): TDictionary<K, V>;
    function SkipWhile(const APredicate: TFunc<K, Boolean>): TDictionary<K, V>;
    function PartitionBy(const APredicate: TFunc<V, Boolean>): TDictionary<Boolean, TList<V>>;
    function LastKey: K;
    function LastValue: V;
    function ToString: string; override;
  end;

implementation

{ TDictionaryHelper<K, V> }

function TDictionaryHelper<K, V>.DistinctBy<TKey>(
  const AKeySelector: TFunc<K, TKey>): TDictionary<TKey, V>;
var
  LPair: TPair<K, V>;
  LKey: TValue;
begin
  Result := TDictionary<TKey, V>.Create;
  for LPair in Self do
  begin
    if not Result.ContainsKey(AKeySelector(LPair.Key)) then
    begin
      LKey := TValue.From<K>(LPair.Key);
      Result.Add(LKey.AsType<TKey>, LPair.Value);
    end;
  end;
end;

function TDictionaryHelper<K, V>.&Except(
  const AOtherDict: TDictionaryHelper<K, V>): TDictionaryHelper<K, V>;
var
  LPair: TPair<K, V>;
begin
  Result := TDictionaryHelper<K, V>.Create;

  for LPair in Self do
  begin
    if not AOtherDict.ContainsKey(LPair.Key) then
      Result.Add(LPair.Key, LPair.Value);
  end;
end;

procedure TDictionaryHelper<K, V>.AddDictionary(
  const ASource: TDictionary<K, V>);
var
  LPair: TPair<K, V>;
begin
  for LPair in ASource do
    Self.Add(LPair.Key, LPair.Value);
end;

function TDictionaryHelper<K, V>.Filter(
  const APredicate: TFunc<V, Boolean>): TDictionaryHelper<K, V>;
var
  LPair: TPair<K, V>;
begin
  Result := TDictionaryHelper<K, V>.Create;
  try
    for LPair in Self do
    begin
      if APredicate(LPair.Value) then
        Result.Add(LPair.Key, LPair.Value);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TDictionaryHelper<K, V>.FindAll(
  const APredicate: TFunc<V, Boolean>): TDictionary<K, V>;
var
  LPair: TPair<K, V>;
begin
  Result := TDictionary<K, V>.Create;

  for LPair in Self do
  begin
    if APredicate(LPair.Value) then
      Result.Add(LPair.Key, LPair.Value);
  end;
end;

function TDictionaryHelper<K, V>.FlatMap<TResult>(
  const AFunc: TFunc<TValue, TArray<TResult>>): TDictionaryHelper<K, TResult>;
var
  LPair: TPair<K, V>;
  LValue: TValue;
  LResultArray: TArray<TResult>;
  LResult: TResult;
begin
  Result := TDictionaryHelper<K, TResult>.Create;

  for LPair in Self do
  begin
    LValue := TValue.From<V>(LPair.Value);
    LResultArray := AFunc(LValue);

    for LResult in LResultArray do
      Result.Add(LPair.Key, LResult);
  end;
end;

procedure TDictionaryHelper<K, V>.ForEach(const AAction: TProc<K, V>);
var
  LPair: TPair<K, V>;
begin
  for LPair in Self do
    AAction(LPair.Key, LPair.Value);
end;

procedure TDictionaryHelper<K, V>.ForEachIndexed(
  const AAction: TProc<Integer, K, V>);
var
  LIndex: Integer;
  LPair: TPair<K, V>;
begin
  LIndex := 0;
  for LPair in Self do
  begin
    AAction(LIndex, LPair.Key, LPair.Value);
    Inc(LIndex);
  end;
end;

function TDictionaryHelper<K, V>.GroupBy<TKey>(
  const AKeySelector: TFunc<V, TKey>): TDictionary<TKey, TList<V>>;
var
  LPair: TPair<K, V>;
  LKey: TKey;
  LGroupedDict: TDictionary<TKey, TList<V>>;
  LList: TList<V>;
begin
  LGroupedDict := TDictionary<TKey, TList<V>>.Create;

  for LPair in Self do
  begin
    LKey := AKeySelector(LPair.Value);
    if not LGroupedDict.TryGetValue(LKey, LList) then
    begin
      LList := TList<V>.Create;
      LGroupedDict.Add(LKey, LList);
    end;
    LList.Add(LPair.Value);
  end;
  Result := LGroupedDict;
end;

function TDictionaryHelper<K, V>.Intersect(
  const AOtherDict: TDictionaryHelper<K, V>): TDictionaryHelper<K, V>;
var
  LPair: TPair<K, V>;
begin
  Result := TDictionaryHelper<K, V>.Create;

  for LPair in Self do
  begin
    if AOtherDict.ContainsKey(LPair.Key) then
      Result.Add(LPair.Key, LPair.Value);
  end;
end;

function TDictionaryHelper<K, V>.Join(const ASeparator: string): string;
var
  LPair: TPair<K, V>;
  LList: TListHelper<string>;
  LValue: TValue;
begin
  LList := TListHelper<string>.Create;
  for LPair in Self do
  begin
    LValue := TValue.From<V>(LPair.Value);
    LList.Add(LValue.ToString);
  end;
  Result := LList.Join(ASeparator);
  LList.Free;
end;

function TDictionaryHelper<K,V>.LastKey: K;
var
  LKey: K;
begin
  Result := Default(K);
  for LKey in Self.Keys do
    Result := LKey;
end;

function TDictionaryHelper<K, V>.LastValue: V;
var
  LValue: V;
begin
  Result := Default(V);
  for LValue in Self.Values do
    Result := LValue;
end;

function TDictionaryHelper<K, V>.Map<TResult>(
  const AMappingFunc: TFunc<V, TResult>): TDictionaryHelper<K, TResult>;
var
  LPair: TPair<K, V>;
begin
  Result := TDictionaryHelper<K, TResult>.Create;
  try
    for LPair in Self do
      Result.Add(LPair.Key, AMappingFunc(LPair.Value));
  except
    Result.Free;
    raise;
  end;
end;

function TDictionaryHelper<K, V>.Max: K;
var
  LPair: TPair<K, V>;
  LMaxKey: K;
  LIsFirst: Boolean;
begin
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

function TDictionaryHelper<K, V>.Min: K;
var
  LPair: TPair<K, V>;
  LMinKey: K;
  LIsFirst: Boolean;
begin
  if Count = 0 then
    raise Exception.Create('The dictionary is empty.');

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

function TDictionaryHelper<K, V>.Partition(
  const APredicate: TFunc<V, Boolean>): TPair<TDictionaryHelper<K, V>,
                                              TDictionaryHelper<K, V>>;
var
  LPair: TPair<K, V>;
  LTrueDict, LFalseDict: TDictionaryHelper<K, V>;
begin
  LTrueDict := TDictionaryHelper<K, V>.Create;
  LFalseDict := TDictionaryHelper<K, V>.Create;
  for LPair in Self do
  begin
    if APredicate(LPair.Value) then
      LTrueDict.Add(LPair.Key, LPair.Value)
    else
      LFalseDict.Add(LPair.Key, LPair.Value);
  end;
  Result := TPair<TDictionaryHelper<K, V>,
                  TDictionaryHelper<K, V>>.Create(LTrueDict, LFalseDict);
end;

function TDictionaryHelper<K, V>.PartitionBy(
  const APredicate: TFunc<V, Boolean>): TDictionary<Boolean, TList<V>>;
var
  LPair: TPair<K, V>;
  LValue: V;
  LKey: Boolean;
begin
  Result := TDictionary<Boolean, TList<V>>.Create;

  for LPair in Self do
  begin
    LValue := LPair.Value;
    LKey := APredicate(LValue);

    if not Result.ContainsKey(LKey) then
      Result[LKey] := TList<V>.Create;

    Result[LKey].Add(LValue);
  end;
end;

function TDictionaryHelper<K, V>.Reduce(const AAccumulator: TFunc<V, V, V>): V;
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

procedure TDictionaryHelper<K, V>.Rotate(const ACount: Integer);
var
  LList: TList<TPair<K, V>>;
  LIndex, LNewIndex: Integer;
  LPair: TPair<K, V>;
begin
  LList := TList<TPair<K, V>>.Create;
  try
    LList.AddRange(Self);
    LList.Count := Self.Count;

    for LIndex := 0 to LList.Count - 1 do
    begin
      LNewIndex := (LIndex + Count) mod LList.Count;
      LPair := LList[LNewIndex];
      LList[LNewIndex] := LList[LIndex];
      LList[LIndex] := LPair;
    end;

    Self.Clear;
    for LPair in LList do
      Self.Add(LPair.Key, LPair.Value);
  finally
    LList.Free;
  end;
end;

function TDictionaryHelper<K, V>.ShuffleKeys: TArray<K>;
var
  LKeysList: TListHelper<K>;
begin
  LKeysList := TListHelper<K>.Create(Self.Keys.ToArray);
  try
    LKeysList.Shuffle;
    Result := LKeysList.ToArray;
  finally
    LKeysList.Free;
  end;
end;

function TDictionaryHelper<K, V>.Skip(
  const ACount: Integer): TDictionaryHelper<K, V>;
var
  LPair: TPair<K, V>;
  LCount: Integer;
begin
  Result := TDictionaryHelper<K, V>.Create;
  LCount := 0;

  for LPair in Self do
  begin
    if LCount >= ACount then
      Result.Add(LPair.Key, LPair.Value)
    else
      Inc(LCount);
  end;
end;

function TDictionaryHelper<K, V>.SkipWhile(
  const APredicate: TFunc<K, Boolean>): TDictionary<K, V>;
var
  LPair: TPair<K, V>;
  LFound: Boolean;
begin
  Result := TDictionary<K, V>.Create;
  LFound := False;

  for LPair in Self do
  begin
    if not LFound and not APredicate(LPair.Key) then
      LFound := True;

    if LFound then
      Result.Add(LPair.Key, LPair.Value);
  end;
end;

function TDictionaryHelper<K, V>.Slice(AStartIndex,
  AEndIndex: Integer): TDictionaryHelper<K, V>;
var
  LPair: TPair<K, V>;
  LIndex: Integer;
begin
  Result := TDictionaryHelper<K, V>.Create;
  LIndex := 0;

  for LPair in Self do
  begin
    if (LIndex >= AStartIndex) and (LIndex <= AEndIndex) then
      Result.Add(LPair.Key, LPair.Value);

    if LIndex > AEndIndex then
      Break;

    Inc(LIndex);
  end;
end;

procedure TDictionaryHelper<K, V>.SortByKey(
  const ASelector: TFunc<K, V, TValue>);
begin

end;

function TDictionaryHelper<K, V>.SortedKeys: TArray<K>;
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

function TDictionaryHelper<K, V>.Take(
  const ACount: Integer): TDictionaryHelper<K, V>;
var
  LPair: TPair<K, V>;
  LCount: Integer;
begin
  Result := TDictionaryHelper<K, V>.Create;
  LCount := 0;
  for LPair in Self do
  begin
    if LCount >= ACount then
      Break;

    Result.Add(LPair.Key, LPair.Value);
    Inc(LCount);
  end;
end;

function TDictionaryHelper<K, V>.TakeWhile(
  const APredicate: TFunc<K, Boolean>): TDictionary<K, V>;
var
  LPair: TPair<K, V>;
begin
  Result := TDictionary<K, V>.Create;

  for LPair in Self do
  begin
    if APredicate(LPair.Key) then
      Result.Add(LPair.Key, LPair.Value)
    else
      break;
  end;
end;

function TDictionaryHelper<K, V>.ToString: string;
var
  LPair: TPair<K, V>;
  ResultBuilder: TStringBuilder;
  LKey: TValue;
  LValue: TValue;
begin
  ResultBuilder := TStringBuilder.Create;
  try
    for LPair in Self do
    begin
      LKey := TValue.From<K>(LPair.Key);
      LValue := TValue.From<V>(LPair.Value);
      if LKey.IsObject then
        ResultBuilder.AppendLine(Format('%s: %s', [LKey.AsObject.ToString, LValue.ToString]))
      else
        ResultBuilder.AppendLine(Format('%s: %s', [LKey.ToString, LValue.ToString]));
    end;
    Result := ResultBuilder.ToString;
  finally
    ResultBuilder.Free;
  end;
end;

function TDictionaryHelper<K, V>._ComparePairs(const Left, Right: TPair<K, V>): Integer;
begin
  Result := TComparer<K>.Default.Compare(Left.Key, Right.Key);
end;

procedure TDictionaryHelper<K, V>.Unique;
var
  LUnique: TDictionary<K, V>;
  LPair: TPair<K, V>;
begin
  LUnique := TDictionary<K, V>.Create;
  try
    for LPair in Self do
      LUnique.AddOrSetValue(LPair.Key, LPair.Value);
    Self.Clear;
    for LPair in LUnique do
      Self.AddOrSetValue(LPair.Key, LPair.Value);
  finally
    LUnique.Free;
  end;
end;

function TDictionaryHelper<K, V>.Zip<T1, T2, TResult>(
  const AList1: TDictionaryHelper<K, T1>;
  const AList2: TDictionaryHelper<K, T2>;
  const AFunc: TFunc<T1, T2, TResult>): TDictionaryHelper<K, TResult>;
var
  LKey: K;
  LValue1, LValue2: TValue;
begin
  Result := TDictionaryHelper<K, TResult>.Create;

  for LKey in AList1.Keys do
  begin
    if AList2.TryGetValue(LKey, LValue2) then
    begin
      LValue1 := TValue.From<T1>(AList1[LKey]);
      Result.Add(LKey, AFunc(LValue1.AsType<T1>, LValue2.AsType<T2>));
    end;
  end;
end;

end.
