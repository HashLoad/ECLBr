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

unit eclbr.list;

interface

uses
  Rtti,
  SysUtils,
  Generics.Defaults,
  Generics.Collections;

type
  TPairList<L, R> = record
    Left: L;
    Right: R;
    constructor Create(const ALeft: L; const ARight: R);
  end;

  TListHelper<T> = class(TList<T>)
  public
    /// <summary>
    ///   Sorts the list based on a specified key selector function.
    /// </summary>
    /// <typeparam name="T">The type of elements in the list.</typeparam>
    /// <param name="ASelector">The key selector function used for sorting.</param>
    procedure ForEach(const Action: TProc<T>);

    /// <summary>
    ///   Executes a specified action for each element in the list, providing the index along with the element.
    /// </summary>
    /// <param name="Action">The action to be executed for each element, with the index as the first parameter and the element as the second parameter.</param>
    procedure ForEachIndexed(const Action: TProc<integer, T>);

    /// <summary>
    ///   Randomly shuffles the elements within the list.
    /// </summary>
    /// <typeparam name="T">The type of elements in the list.</typeparam>
    /// <param name="AList">The list to be shuffled.</param>
    procedure Shuffle;

    /// <summary>
    ///   Sorts the list based on a specified key selector function.
    /// </summary>
    /// <typeparam name="T">The type of elements in the list.</typeparam>
    /// <param name="ASelector">The key selector function used for sorting.</param>
    procedure SortBy(const ASelector: TFunc<T, TValue>);

    /// <summary>
    ///   Rotates the elements in the list by a specified number of positions.
    /// </summary>
    /// <param name="Count">The number of positions by which the elements are rotated. A positive count rotates to the right; a negative count rotates to the left.</param>
    procedure Rotate(const Count: integer);

    /// <summary>
    ///   Removes duplicate elements from the list, leaving only unique elements.
    /// </summary>
    procedure Unique;

    /// <summary>
    ///   Transforms each element in the list using a specified mapping function and returns a new list containing the results.
    /// </summary>
    /// <typeparam name="TResult">The type of elements in the resulting list.</typeparam>
    /// <param name="AMappingFunc">The mapping function used to transform each element.</param>
    /// <returns>A new list containing the transformed elements.</returns>
    function Map<TResult>(const AMappingFunc: TFunc<T, TResult>): TList<TResult>;

    /// <summary>
    ///   Filters the elements in the list based on a specified predicate function and returns a new list containing the matching elements.
    /// </summary>
    /// <param name="APredicate">The predicate function used to filter elements.</param>
    /// <returns>A new list containing the elements that satisfy the predicate.</returns>
    function Filter(const APredicate: TFunc<T, boolean>): TList<T>;

    /// <summary>
    ///   Combines the elements in the list using a specified accumulator function and returns the accumulated result.
    /// </summary>
    /// <param name="AAccumulator">The accumulator function used to combine elements.</param>
    /// <returns>The accumulated result.</returns>
    function Reduce(const AAccumulator: TFunc<T, T, T>): T;

    /// <summary>
    ///   Groups elements in the list based on a specified key selector function and returns a dictionary of lists where each key corresponds to a group of elements.
    /// </summary>
    /// <typeparam name="TKey">The type of keys used for grouping.</typeparam>
    /// <param name="AKeySelector">The key selector function used for grouping elements.</param>
    /// <returns>A dictionary of lists where keys represent groups of elements.</returns>
    function GroupBy<TKey>(const AKeySelector: TFunc<T, TKey>): TDictionary<TKey, TList<T>>;

    /// <summary>
    ///   Combines the elements in the list into a single string with each element separated by a specified separator.
    /// </summary>
    /// <param name="ASeparator">The separator used to separate elements in the resulting string.</param>
    /// <returns>A string containing the combined elements.</returns>
    function Join(const ASeparator: string): string;

    /// <summary>
    ///   Divides the elements in the list into two separate lists based on a specified predicate function and returns a pair of lists representing the partitioned elements.
    /// </summary>
    /// <param name="APredicate">The predicate function used to partition elements.</param>
    /// <returns>A pair of lists representing the partitioned elements.</returns>
    function Partition(const APredicate: TFunc<T, boolean>): TPairList<TList<T>, TList<T>>;

    /// <summary>
    ///   Returns a new list containing the first 'n' elements from the list, where 'n' is the specified count.
    /// </summary>
    /// <param name="ACount">The number of elements to take from the beginning of the list.</param>
    /// <returns>A new list containing the first 'n' elements.</returns>
    function Take(const ACount: integer): TList<T>;

    /// <summary>
    ///   Returns a new list containing all elements in the list except the first 'n' elements, where 'n' is the specified count.
    /// </summary>
    /// <param name="ACount">The number of elements to skip from the beginning of the list.</param>
    /// <returns>A new list containing the remaining elements after skipping 'n' elements.</returns>
    function Skip(const ACount: integer): TList<T>;

    /// <summary>
    ///   Searches for an element in the list that satisfies a specified predicate and returns the first matching element.
    /// </summary>
    /// <param name="APredicate">The predicate function used to search for an element.</param>
    /// <returns>The first element that satisfies the predicate, or a default value if not found.</returns>
    function Find(const APredicate: TFunc<T, boolean>): T;

    /// <summary>
    ///   Returns a new list containing a portion of elements from the source list, starting from the specified start index and ending at the specified end index.
    /// </summary>
    /// <typeparam name="T">The type of elements in the lists.</typeparam>
    /// <param name="AList">The source list to slice.</param>
    /// <param name="AStartIndex">The index of the first element to include in the slice.</param>
    /// <param name="AEndIndex">The index of the last element to include in the slice.</param>
    /// <returns>A new list containing the sliced elements.</returns>
    function Slice<T>(const AList: TList<T>; AStartIndex, AEndIndex: integer): TList<T>;

    /// <summary>
    ///   Combines the elements of two lists into a new list using a specified function and returns the resulting list of combined elements.
    /// </summary>
    /// <typeparam name="T1">The type of elements in the first list.</typeparam>
    /// <typeparam name="T2">The type of elements in the second list.</typeparam>
    /// <typeparam name="TResult">The type of elements in the resulting list.</typeparam>
    /// <param name="AList1">The first list to combine.</param>
    /// <param name="AList2">The second list to combine.</param>
    /// <param name="AFunc">The function used to combine elements from both lists.</param>
    /// <returns>A new list containing elements resulting from the combination of elements from the source lists.</returns>
    function Zip<T1, T2, TResult>(const AList1: TList<T1>; const AList2: TList<T2>;
      const AFunc: TFunc<T1, T2, TResult>): TList<TResult>;

    /// <summary>
    ///   Maps each element of the source list to an array of elements using a specified function and returns a new list containing the flattened elements.
    /// </summary>
    /// <typeparam name="T">The type of elements in the source list.</typeparam>
    /// <typeparam name="TResult">The type of elements in the resulting list.</typeparam>
    /// <param name="AList">The source list to flatmap.</param>
    /// <param name="AFunc">The function used to map each element to an array of elements.</param>
    /// <returns>A new list containing the flattened elements.</returns>
    function FlatMap<T, TResult>(const AList: TList<T>;
      const AFunc: TFunc<T, TArray<TResult>>): TList<TResult>;

    /// <summary>
    ///   Returns a new list containing the elements that are common between two lists.
    /// </summary>
    /// <typeparam name="T">The type of elements in the lists.</typeparam>
    /// <param name="List1">The first list to intersect.</param>
    /// <param name="List2">The second list to intersect.</param>
    /// <returns>A new list containing the elements common to both input lists.</returns>
    function Intersect<T>(const List1, List2: TList<T>): TList<T>;

    /// <summary>
    ///   Returns a new list containing the elements from the first list that are not present in the second list.
    /// </summary>
    /// <typeparam name="T">The type of elements in the lists.</typeparam>
    /// <param name="List1">The first list to extract elements from.</param>
    /// <param name="List2">The second list used for exclusion.</param>
    /// <returns>A new list containing the elements from the first list that are not found in the second list.</returns>
    function &Except<T>(const List1, List2: TList<T>): TList<T>;

    /// <summary>
    ///   Finds and returns the maximum element in the list based on their natural order.
    /// </summary>
    /// <typeparam name="T">The type of elements in the list.</typeparam>
    /// <param name="AList">The list to search for the maximum element.</param>
    /// <returns>The maximum element in the list.</returns>
    function Max: T;

    /// <summary>
    ///   Finds and returns the minimum element in the list based on their natural order.
    /// </summary>
    /// <typeparam name="T">The type of elements in the list.</typeparam>
    /// <param name="AList">The list to search for the minimum element.</param>
    /// <returns>The minimum element in the list.</returns>
    function Min: T;

    /// <summary>
    ///   Returns a new list containing distinct elements based on the specified key extracted from each element using a given selector function.
    /// </summary>
    /// <typeparam name="TKey">The type of keys used for comparison.</typeparam>
    /// <param name="AKeySelector">The function used to extract keys from elements.</param>
    /// <returns>A new list containing distinct elements based on the specified key.</returns>
    function DistinctBy<TKey>(const AKeySelector: TFunc<T, TKey>): TList<T>;

    /// <summary>
    ///   Returns a new list containing all elements from the source list that satisfy a given predicate.
    /// </summary>
    /// <param name="APredicate">The function used to determine whether an element should be included in the result.</param>
    /// <returns>A new list containing elements that satisfy the predicate.</returns>
    function FindAll(const APredicate: TFunc<T, boolean>): TList<T>;

    /// <summary>
    ///   Returns a new list containing elements from the beginning of the source list until the first element that does not satisfy a given predicate.
    /// </summary>
    /// <param name="APredicate">The function used to determine whether an element should be included in the result.</param>
    /// <returns>A new list containing elements from the beginning of the source list that satisfy the predicate.</returns>
    function TakeWhile(const APredicate: TFunc<T, boolean>): TList<T>;

    /// <summary>
    ///   Returns a new list containing elements from the source list after the first element that does not satisfy a given predicate.
    /// </summary>
    /// <param name="APredicate">The function used to determine whether an element should be included in the result.</param>
    /// <returns>A new list containing elements from the source list after the first element that does not satisfy the predicate.</returns>
    function SkipWhile(const APredicate: TFunc<T, boolean>): TList<T>;

    /// <summary>
    ///   Groups the elements in the list by a specified key and returns a dictionary with key-to-count associations.
    /// </summary>
    /// <typeparam name="TKey">The type of keys used for grouping.</typeparam>
    /// <returns>A dictionary where keys are distinct elements from the list, and values are the counts of each element.</returns>
    function GroupByAndCount<TKey>: TDictionary<TKey, integer>;

    /// <summary>
    ///   Partitions the elements in the list into two groups based on a given predicate and returns a dictionary where keys represent the partition status.
    /// </summary>
    /// <returns>A dictionary where keys indicate whether an element satisfies the predicate, and values are lists of elements for each partition.</returns>
    function PartitionBy(const APredicate: TFunc<T, boolean>): TDictionary<boolean, TList<T>>;

    /// <summary>
    ///   Returns a new list containing only the elements from the source list that are different from their immediate predecessors.
    /// </summary>
    /// <returns>A new list containing elements that are distinct from their immediate predecessors.</returns>
    function DistinctUntilChanged: TList<T>;

    /// <summary>
    ///   Returns the index of the first element in the list that satisfies a given predicate or -1 if no such element is found.
    /// </summary>
    /// <param name="APredicate">The function used to search for an element.</param>
    /// <returns>The index of the first element that satisfies the predicate or -1 if not found.</returns>
    function IndexOfItem(const APredicate: TFunc<T, boolean>): integer;

    /// <summary>
    ///   Determines whether the list is empty.
    /// </summary>
    /// <returns>True if the list is empty; otherwise, False.</returns>
    function IsEmpty: boolean;
    function ToString: string; override;
  end;

implementation

uses
  eclbr.utils;

{ TListHelper<T> }

function TListHelper<T>.DistinctBy<TKey>(const AKeySelector: TFunc<T, TKey>): TList<T>;
var
  LDict: TDictionary<TKey, T>;
  LItem: T;
begin
  LDict := TDictionary<TKey, T>.Create;
  try
    for LItem in Self do
      LDict.AddOrSetValue(AKeySelector(LItem), LItem);

    Result := TList<T>.Create(LDict.Values);
  finally
    LDict.Free;
  end;
end;

function TListHelper<T>.DistinctUntilChanged: TList<T>;
var
  LItem: T;
begin
  Result := TList<T>.Create;
  for LItem in Self do
  begin
    if Result.Count = 0 then
      Result.Add(LItem)
    else
    if not TEqualityComparer<T>.Default.Equals(LItem, Result.Last) then
      Result.Add(LItem);
  end;
end;

function TListHelper<T>.&Except<T>(const List1, List2: TList<T>): TList<T>;
var
  LItem: T;
begin
  Result := TList<T>.Create;
  for LItem in List1 do
  begin
    if not List2.Contains(LItem) then
      Result.Add(LItem);
  end;
end;

function TListHelper<T>.Filter(const APredicate: TFunc<T, boolean>): TList<T>;
var
  LItem: T;
begin
  Result := TList<T>.Create;
  for LItem in Self do
    if APredicate(LItem) then
      Result.Add(LItem);
end;

function TListHelper<T>.Find(const APredicate: TFunc<T, boolean>): T;
var
  LItem: T;
begin
  for LItem in Self do
  begin
    if APredicate(LItem) then
      exit(LItem);
  end;
  Result := Default(T);
end;

function TListHelper<T>.FindAll(const APredicate: TFunc<T, boolean>): TList<T>;
var
  LItem: T;
begin
  Result := TListHelper<T>.Create;
  for LItem in Self do
  begin
    if APredicate(LItem) then
      Result.Add(LItem);
  end;
end;

function TListHelper<T>.FlatMap<T, TResult>(const AList: TList<T>;
  const AFunc: TFunc<T, TArray<TResult>>): TList<TResult>;
var
  LItem: T;
  LResultArray: TArray<TResult>;
  LResultItem: TResult;
begin
  Result := TList<TResult>.Create;
  for LItem in AList do
  begin
    LResultArray := AFunc(LItem);
    for LResultItem in LResultArray do
      Result.Add(LResultItem);
  end;
end;

procedure TListHelper<T>.ForEach(const Action: TProc<T>);
var
  LItem: T;
begin
  for LItem in Self do
    Action(LItem);
end;

procedure TListHelper<T>.ForEachIndexed(const Action: TProc<integer, T>);
var
  LIndex: integer;
  LItem: T;
begin
  for LIndex := 0 to Count - 1 do
  begin
    LItem := Items[LIndex];
    Action(LIndex, LItem);
  end;
end;

function TListHelper<T>.GroupBy<TKey>(const AKeySelector: TFunc<T, TKey>): TDictionary<TKey, TList<T>>;
var
  LItem: T;
  LKey: TKey;
begin
  Result := TDictionary<TKey, TList<T>>.Create;
  for LItem in Self do
  begin
    LKey := AKeySelector(LItem);
    if not Result.ContainsKey(LKey) then
      Result.Add(LKey, TList<T>.Create);
    Result[LKey].Add(LItem);
  end;
end;

function TListHelper<T>.GroupByAndCount<TKey>: TDictionary<TKey, integer>;
var
  LGroupDict: TDictionary<TKey, integer>;
  LItem: T;
  LKey: TKey;
begin
  LGroupDict := TDictionary<TKey, integer>.Create;
  try
    for LItem in Self do
    begin
      LKey := TValue.From<T>(LItem).AsType<TKey>;

      if LGroupDict.ContainsKey(LKey) then
        LGroupDict[LKey] := LGroupDict[LKey] + 1
      else
        LGroupDict.Add(LKey, 1);
    end;
    Result := LGroupDict;
  except
    LGroupDict.Free;
    raise;
  end
end;

function TListHelper<T>.IndexOfItem(
  const APredicate: TFunc<T, boolean>): integer;
var
  LIndex: Integer;
begin
  for LIndex := 0 to Count - 1 do
  begin
    if APredicate(Items[LIndex]) then
      exit(LIndex);
  end;
  Result := -1;
end;

function TListHelper<T>.Intersect<T>(const List1, List2: TList<T>): TList<T>;
var
  LItem: T;
begin
  Result := TList<T>.Create;
  for LItem in List1 do
  begin
    if List2.Contains(LItem) then
      Result.Add(LItem);
  end;
end;

function TListHelper<T>.IsEmpty: boolean;
begin
  Result := Self.Count = 0;
end;

function TListHelper<T>.Join(const ASeparator: string): string;
var
  LItem: T;
begin
  Result := '';
  for LItem in Self do
  begin
    if Result <> '' then
      Result := Result + ASeparator;
    Result := Result + TValue.From<T>(LItem).ToString;
  end;
end;

function TListHelper<T>.Map<TResult>(const AMappingFunc: TFunc<T, TResult>): TList<TResult>;
var
  LItem: T;
begin
  Result := TList<TResult>.Create;
  for LItem in Self do
    Result.Add(AMappingFunc(LItem));
end;

function TListHelper<T>.Max: T;
var
  LItem: T;
  LIsFirst: Boolean;
begin
  if Count = 0 then
    raise Exception.Create('The list is empty.');

  LIsFirst := True;
  Result := Default(T);
  for LItem in Self do
  begin
    if LIsFirst or (TComparer<T>.Default.Compare(LItem, Result) > 0) then
    begin
      Result := LItem;
      LIsFirst := False;
    end;
  end;
end;

function TListHelper<T>.Min: T;
var
  LItem: T;
  LIsFirst: boolean;
begin
  if Count = 0 then
    raise Exception.Create('The list is empty.');

  LIsFirst := True;
  Result := Default(T);
  for LItem in Self do
  begin
    if LIsFirst or (TComparer<T>.Default.Compare(LItem, Result) < 0) then
    begin
      Result := LItem;
      LIsFirst := False;
    end;
  end;
  if LIsFirst then
    raise Exception.Create('No minimum value found in the list.');
end;

function TListHelper<T>.Partition(const APredicate: TFunc<T, boolean>): TPairList<TList<T>, TList<T>>;
var
  LLeftList, LRightList: TList<T>;
  LItem: T;
begin
  LLeftList := TList<T>.Create;
  LRightList := TList<T>.Create;

  for LItem in Self do
  begin
    if APredicate(LItem) then
      LLeftList.Add(LItem)
    else
      LRightList.Add(LItem);
  end;
  Result.Left := LLeftList;
  Result.Right := LRightList;
end;

function TListHelper<T>.PartitionBy(const APredicate: TFunc<T, boolean>): TDictionary<boolean, TList<T>>;
var
  LPartitions: TDictionary<boolean, TList<T>>;
  LPartList: TList<T>;
  LItem: T;
begin
  LPartitions := TDictionary<boolean, TList<T>>.Create;
  try
    for LItem in Self do
    begin
      if not LPartitions.ContainsKey(APredicate(LItem)) then
        LPartitions[APredicate(LItem)] := TList<T>.Create;
      LPartitions[APredicate(LItem)].Add(LItem);
    end;
    Result := LPartitions;
  except
    on E: Exception do
    begin
      for LPartList in LPartitions.Values do
        LPartList.Free;
      LPartitions.Free;
      raise;
    end;
  end;
end;

function TListHelper<T>.Reduce(const AAccumulator: TFunc<T, T, T>): T;
var
  LItem: T;
begin
  if Self.Count = 0 then
    raise Exception.Create('List is empty, cannot reduce.');

  Result := Self[0];
  for LItem in Self do
    Result := AAccumulator(Result, LItem);
end;

procedure TListHelper<T>.Rotate(const Count: integer);
var
  LTotalCount, LFor, LShiftedIndex: integer;
  LTempList: TList<T>;
begin
  LTotalCount := Count mod Count;
  if LTotalCount < 0 then
    LTotalCount := LTotalCount + Count;

  if LTotalCount = 0 then
    exit;

  LTempList := TList<T>.Create;
  try
    for LFor := 0 to Self.Count - 1 do
      LTempList.Add(Self[LFor]);

    Self.Clear;
    for LFor := 0 to LTempList.Count - 1 do
    begin
      LShiftedIndex := (LFor + LTotalCount) mod LTempList.Count;
      Self.Add(LTempList[LShiftedIndex]);
    end;
  finally
    LTempList.Free;
  end
end;

procedure TListHelper<T>.Shuffle;
var
  LI, LJ: integer;
  LTemp: T;
begin
  Randomize;
  for LI := Count - 1 downto 1 do
  begin
    LJ := Random(LI + 1);
    LTemp := Items[LI];
    Items[LI] := Items[LJ];
    Items[LJ] := LTemp;
  end;
end;

function TListHelper<T>.Skip(const ACount: integer): TList<T>;
var
  LIndex: integer;
begin
  Result := TList<T>.Create;
  for LIndex := Count to Self.Count - 1 do
    Result.Add(Self[LIndex]);
end;

function TListHelper<T>.SkipWhile(
  const APredicate: TFunc<T, boolean>): TList<T>;
var
  LFor: integer;
begin
  Result := TList<T>.Create;
  LFor := 0;

  while (LFor < Count) and APredicate(Items[LFor]) do
    Inc(LFor);

  while LFor < Count do
  begin
    Result.Add(Items[LFor]);
    Inc(LFor);
  end;
end;

function TListHelper<T>.Slice<T>(const AList: TList<T>; AStartIndex,
  AEndIndex: integer): TList<T>;
var
  LFor: integer;
begin
  Result := TList<T>.Create;
  for LFor := AStartIndex to AEndIndex do
    Result.Add(AList[LFor]);
end;

procedure TListHelper<T>.SortBy(const ASelector: TFunc<T, TValue>);
begin
  Sort(TComparer<T>.Construct(
    function(const Left, Right: T): integer
    begin
      Result := TComparer<TValue>.Default.Compare(ASelector(Left), ASelector(Right));
    end
  ));
end;

function TListHelper<T>.Take(const ACount: integer): TList<T>;
var
  LIndex: integer;
begin
  Result := TList<T>.Create;
  for LIndex := 0 to TUtils.Min(Count - 1, Count - 1) do
  begin
    if LIndex >= Self.Count then
      break;
    Result.Add(Self[LIndex]);
  end;
end;

function TListHelper<T>.TakeWhile(
  const APredicate: TFunc<T, boolean>): TList<T>;
var
  LFor: integer;
begin
  Result := TList<T>.Create;
  for LFor := 0 to Count - 1 do
  begin
    if not APredicate(Items[LFor]) then
      break;
    Result.Add(Items[LFor]);
  end;
end;

function TListHelper<T>.ToString: string;
var
  LItem: T;
  LValue: TValue;
begin
  Result := '[';
  for LItem in Self do
  begin
    LValue := TValue.From<T>(LItem);
    if Result <> '[' then
      Result := Result + ', ';
    Result := Result + LValue.ToString;
  end;
  Result := Result + ']';
end;

procedure TListHelper<T>.Unique;
var
  LUniqueItems: TList<T>;
  LItem: T;
begin
  LUniqueItems := TList<T>.Create;
  try
    for LItem in Self do
    begin
      if not LUniqueItems.Contains(LItem) then
        LUniqueItems.Add(LItem);
    end;
    Self.Clear;
    Self.AddRange(LUniqueItems);
  finally
    LUniqueItems.Free;
  end;
end;

function TListHelper<T>.Zip<T1, T2, TResult>(const AList1: TList<T1>;
  const AList2: TList<T2>; const AFunc: TFunc<T1, T2, TResult>): TList<TResult>;
var
  LFor: integer;
begin
  Result := TList<TResult>.Create;
  for LFor := 0 to TUtils.Min(AList1.Count, AList2.Count) - 1 do
    Result.Add(AFunc(AList1[LFor], AList2[LFor]));
end;

{ TPairList<L, R> }

constructor TPairList<L, R>.Create(const ALeft: L; const ARight: R);
begin
  Left := ALeft;
  Right := ARight;
end;

end.
