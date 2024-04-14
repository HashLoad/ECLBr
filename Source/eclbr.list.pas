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

unit eclbr.list;

interface

uses
  Rtti,
  SysUtils,
  Generics.Defaults,
  Generics.Collections,
  eclbr.map,
  eclbr.std,
  eclbr.vector;

type
  Tuple = eclbr.std.Tuple;

  TPairList<L, R> = record
    Left: L;
    Right: R;
    constructor Create(const ALeft: L; const ARight: R);
  end;

  TListEx<T> = class(TList<T>)
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
    procedure ForEachIndexed(const Action: TProc<Integer, T>);

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
    procedure Rotate(const Count: Integer);

    /// <summary>
    ///   Reverses the order of elements in the list.
    /// </summary>
    /// <remarks>
    ///   The Reverse procedure reverses the order of elements in the current list,
    ///   changing the position of each element such that the first element becomes
    ///   the last, the second becomes the second-to-last, and so on. This operation
    ///   affects the order of elements in the original list.
    /// </remarks>
    procedure Reverse;

    /// <summary>
    ///   Removes duplicate elements from the list.
    /// </summary>
    /// <remarks>
    ///   The Unique procedure removes all duplicate elements from the current list,
    ///   retaining only the first occurrence of each unique element. The original
    ///   order of elements is preserved, and the list will contain only distinct elements.
    /// </remarks>
    function Unique: TVector<T>;

    /// <summary>
    ///   Transforms each element in the list using a specified mapping function and returns a new list containing the results.
    /// </summary>
    /// <typeparam name="TResult">The type of elements in the resulting list.</typeparam>
    /// <param name="AMappingFunc">The mapping function used to transform each element.</param>
    /// <returns>A new list containing the transformed elements.</returns>
    function Map<R>(const AMappingFunc: TFunc<T, R>): TVector<R>;

    /// <summary>
    ///   Filters the elements in the list based on a specified predicate function and returns a new list containing the matching elements.
    /// </summary>
    /// <param name="APredicate">The predicate function used to filter elements.</param>
    /// <returns>A new list containing the elements that satisfy the predicate.</returns>
    function Filter(const APredicate: TPredicate<T>): TVector<T>; overload;

    /// <summary>
    ///   Filters the elements of the vector based on a provided predicate.
    /// </summary>
    /// <param name="APredicate">
    ///   A function that determines whether an element should be included in the filtered vector.
    ///   The function takes two parameters: the element of the vector and its index, and should return True
    ///   if the element should be included in the filtered vector, or False if it should be excluded.
    /// </param>
    /// <returns>
    ///   A new vector containing the elements that satisfy the predicate.
    /// </returns>
    function Filter(const APredicate: TFunc<T, Integer, Boolean>): TVector<T>; overload;

    /// <summary>
    ///   Combines the elements in the list using a specified accumulator function and returns the accumulated result.
    /// </summary>
    /// <param name="AAccumulator">The accumulator function used to combine elements.</param>
    /// <returns>The accumulated result.</returns>
    function Reduce(const AAccumulator: TFunc<T, T, T>): T; overload;

    /// <summary>
    ///   Combines the elements in the list using a specified accumulator function and returns the accumulated result.
    /// </summary>
    /// <param name="AAccumulator">The accumulator function used to combine elements.</param>
    /// <returns>The accumulated result.</returns>
    function Reduce(const AAccumulator: TFunc<T, T, T>; const AInitial: T): T; overload;

    /// <summary>
    ///   Reduces a tuple based on a provided accumulator function.
    /// </summary>
    /// <param name="AAccumulator">
    ///   A function that accumulates values in the tuple.
    ///   The function takes two parameters: the current value of the tuple and the accumulated value,
    ///   and should return a new accumulated value.
    /// </param>
    /// <param name="ATuple">
    ///   The initial tuple to start the reduction process.
    /// </param>
    /// <returns>
    ///   A new tuple resulting from the reduction process.
    /// </returns>
    function Reduce(const AAccumulator: TFunc<T, Tuple, Tuple>; const ATuple: Tuple): Tuple; overload;

    /// <summary>
    ///   Groups elements in the list based on a specified key selector function and returns a dictionary of lists where each key corresponds to a group of elements.
    /// </summary>
    /// <typeparam name="TKey">The type of keys used for grouping.</typeparam>
    /// <param name="AKeySelector">The key selector function used for grouping elements.</param>
    /// <returns>A dictionary of lists where keys represent groups of elements.</returns>
    function GroupBy<K>(const AKeySelector: TFunc<T, K>): TMap<K, TVector<T>>;

    /// <summary>
    ///   Combines the elements in the list into a single String with each element separated by a specified separator.
    /// </summary>
    /// <param name="ASeparator">The separator used to separate elements in the resulting String.</param>
    /// <returns>A String containing the combined elements.</returns>
    function Join(const ASeparator: String): String;

    /// <summary>
    ///   Divides the elements in the list into two separate lists based on a specified predicate function and returns a pair of lists representing the partitioned elements.
    /// </summary>
    /// <param name="APredicate">The predicate function used to partition elements.</param>
    /// <returns>A pair of lists representing the partitioned elements.</returns>
    function Partition(const APredicate: TPredicate<T>): TPairList<TVector<T>, TVector<T>>;

    /// <summary>
    ///   Returns a new list containing the first 'n' elements from the list, where 'n' is the specified count.
    /// </summary>
    /// <param name="ACount">The number of elements to take from the beginning of the list.</param>
    /// <returns>A new list containing the first 'n' elements.</returns>
    function Take(const ACount: Integer): TVector<T>;

    /// <summary>
    ///   Returns a new list containing all elements in the list except the first 'n' elements, where 'n' is the specified count.
    /// </summary>
    /// <param name="ACount">The number of elements to skip from the beginning of the list.</param>
    /// <returns>A new list containing the remaining elements after skipping 'n' elements.</returns>
    function Skip(const ACount: Integer): TVector<T>;

    /// <summary>
    ///   Searches for an element in the list that satisfies a specified predicate and returns the first matching element.
    /// </summary>
    /// <param name="APredicate">The predicate function used to search for an element.</param>
    /// <returns>The first element that satisfies the predicate, or a default value if not found.</returns>
    function Find(const APredicate: TPredicate<T>): T;

    /// <summary>
    ///   Returns a new list containing a portion of elements from the source list, starting from the specified start index and ending at the specified end index.
    /// </summary>
    /// <typeparam name="T">The type of elements in the lists.</typeparam>
    /// <param name="AList">The source list to slice.</param>
    /// <param name="AStartIndex">The index of the first element to include in the slice.</param>
    /// <param name="AEndIndex">The index of the last element to include in the slice.</param>
    /// <returns>A new list containing the sliced elements.</returns>
    function Slice(const AList: TList<T>; AStartIndex, AEndIndex: Integer): TVector<T>;

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
    function Zip<T1, T2, R>(const AList1: TList<T1>; const AList2: TList<T2>;
      const AFunc: TFunc<T1, T2, R>): TVector<R>;

    /// <summary>
    ///   Maps each element of the source list to an array of elements using a specified function and returns a new list containing the flattened elements.
    /// </summary>
    /// <typeparam name="T">The type of elements in the source list.</typeparam>
    /// <typeparam name="TResult">The type of elements in the resulting list.</typeparam>
    /// <param name="AList">The source list to flatmap.</param>
    /// <param name="AFunc">The function used to map each element to an array of elements.</param>
    /// <returns>A new list containing the flattened elements.</returns>
    function FlatMap<R>(const AList: TList<T>;
      const AFunc: TFunc<T, TArray<R>>): TVector<R>;

    /// <summary>
    ///   Returns a new list containing the elements that are common between two lists.
    /// </summary>
    /// <typeparam name="T">The type of elements in the lists.</typeparam>
    /// <param name="List1">The first list to intersect.</param>
    /// <param name="List2">The second list to intersect.</param>
    /// <returns>A new list containing the elements common to both input lists.</returns>
    function Intersect(const List2: TList<T>): TVector<T>;

    /// <summary>
    ///   Returns a new list containing the elements from the first list that are not present in the second list.
    /// </summary>
    /// <typeparam name="T">The type of elements in the lists.</typeparam>
    /// <param name="List1">The first list to extract elements from.</param>
    /// <param name="List2">The second list used for exclusion.</param>
    /// <returns>A new list containing the elements from the first list that are not found in the second list.</returns>
    function &Except(const List2: TList<T>): TVector<T>;

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
    function DistinctBy<K>(const AKeySelector: TFunc<T, K>): TVector<T>;

    /// <summary>
    ///   Returns a new list containing all elements from the source list that satisfy a given predicate.
    /// </summary>
    /// <param name="APredicate">The function used to determine whether an element should be included in the result.</param>
    /// <returns>A new list containing elements that satisfy the predicate.</returns>
    function FindAll(const APredicate: TPredicate<T>): TVector<T>;

    /// <summary>
    ///   Returns a new list containing elements from the beginning of the source list until the first element that does not satisfy a given predicate.
    /// </summary>
    /// <param name="APredicate">The function used to determine whether an element should be included in the result.</param>
    /// <returns>A new list containing elements from the beginning of the source list that satisfy the predicate.</returns>
    function TakeWhile(const APredicate: TPredicate<T>): TVector<T>;

    /// <summary>
    ///   Returns a new list containing elements from the source list after the first element that does not satisfy a given predicate.
    /// </summary>
    /// <param name="APredicate">The function used to determine whether an element should be included in the result.</param>
    /// <returns>A new list containing elements from the source list after the first element that does not satisfy the predicate.</returns>
    function SkipWhile(const APredicate: TPredicate<T>): TVector<T>;

    /// <summary>
    ///   Groups the elements in the list by a specified key and returns a dictionary with key-to-count associations.
    /// </summary>
    /// <typeparam name="TKey">The type of keys used for grouping.</typeparam>
    /// <returns>A dictionary where keys are distinct elements from the list, and values are the counts of each element.</returns>
    function GroupByAndCount<K>: TMap<K, Integer>;

    /// <summary>
    ///   Partitions the elements in the list into two groups based on a given predicate and returns a dictionary where keys represent the partition status.
    /// </summary>
    /// <returns>A dictionary where keys indicate whether an element satisfies the predicate, and values are lists of elements for each partition.</returns>
    function PartitionBy(const APredicate: TPredicate<T>): TMap<Boolean, TVector<T>>;

    /// <summary>
    ///   Returns a new list containing only the elements from the source list that are different from their immediate predecessors.
    /// </summary>
    /// <returns>A new list containing elements that are distinct from their immediate predecessors.</returns>
    function DistinctUntilChanged: TVector<T>;

    /// <summary>
    ///   Returns the index of the first element in the list that satisfies a given predicate or -1 if no such element is found.
    /// </summary>
    /// <param name="APredicate">The function used to search for an element.</param>
    /// <returns>The index of the first element that satisfies the predicate or -1 if not found.</returns>
    function IndexOfItem(const APredicate: TPredicate<T>): Integer;

    /// <summary>
    ///   Determines whether the list is empty.
    /// </summary>
    /// <returns>True if the list is empty; otherwise, False.</returns>
    function IsEmpty: Boolean;

    /// <summary>
    ///   Retorna uma representação de String deste objeto.
    /// </summary>
    /// <remarks>
    ///   Esta função é chamada automaticamente quando você usa funções que exigem
    ///   uma representação de String deste objeto, como WriteLn ou String.Format.
    /// </remarks>
    /// <returns>
    ///   Uma String que representa este objeto.
    /// </returns>
    function ToString: String; override;
  end;

implementation

{ TListHelper<T> }

function TListEx<T>.DistinctBy<K>(const AKeySelector: TFunc<T, K>): TVector<T>;
var
  LDict: TDictionary<K, T>;
  LItem: T;
begin
  Result := TVector<T>.Create([]);
  LDict := TDictionary<K, T>.Create;
  try
    for LItem in Self do
      LDict.AddOrSetValue(AKeySelector(LItem), LItem);
    for LItem in LDict.Values do
      Result.Add(LItem);
  finally
    LDict.Free;
  end;
end;

function TListEx<T>.DistinctUntilChanged: TVector<T>;
var
  LItem: T;
begin
  Result := TVector<T>.Create([]);
  for LItem in Self do
  begin
    if Result.Count = 0 then
      Result.Add(LItem)
    else
    if not TEqualityComparer<T>.Default.Equals(LItem, Result.Last) then
      Result.Add(LItem);
  end;
end;

function TListEx<T>.&Except(const List2: TList<T>): TVector<T>;
var
  LItem: T;
begin
  Result := TVector<T>.Create([]);
  for LItem in Self do
  begin
    if not List2.Contains(LItem) then
      Result.Add(LItem);
  end;
end;

function TListEx<T>.Filter(const APredicate: TPredicate<T>): TVector<T>;
var
  LItem: T;
begin
  Result := TVector<T>.Create([]);
  for LItem in Self do
    if APredicate(LItem) then
      Result.Add(LItem);
end;

function TListEx<T>.Filter(const APredicate: TFunc<T, Integer, Boolean>): TVector<T>;
var
  LItem: T;
  LIndex: Integer;
begin
  Result := TVector<T>.Create([]);
  LIndex := 0;
  for LItem in Self do
  begin
    if APredicate(LItem, LIndex) then
      Result.Add(LItem);
    Inc(LIndex);
  end;
end;

function TListEx<T>.Find(const APredicate: TPredicate<T>): T;
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

function TListEx<T>.FindAll(const APredicate: TPredicate<T>): TVector<T>;
var
  LItem: T;
begin
  Result := TVector<T>.Create([]);
  for LItem in Self do
  begin
    if APredicate(LItem) then
      Result.Add(LItem);
  end;
end;

function TListEx<T>.FlatMap<R>(const AList: TList<T>;
  const AFunc: TFunc<T, TArray<R>>): TVector<R>;
var
  LItem: T;
  LResultArray: TArray<R>;
  LResultItem: R;
begin
  Result := TVector<R>.Create([]);
  for LItem in AList do
  begin
    LResultArray := AFunc(LItem);
    for LResultItem in LResultArray do
      Result.Add(LResultItem);
  end;
end;

procedure TListEx<T>.ForEach(const Action: TProc<T>);
var
  LItem: T;
begin
  for LItem in Self do
    Action(LItem);
end;

procedure TListEx<T>.ForEachIndexed(const Action: TProc<Integer, T>);
var
  LIndex: Integer;
  LItem: T;
begin
  for LIndex := 0 to Count - 1 do
  begin
    LItem := Items[LIndex];
    Action(LIndex, LItem);
  end;
end;

function TListEx<T>.GroupBy<K>(const AKeySelector: TFunc<T, K>): TMap<K, TVector<T>>;
var
  LItem: T;
  LKey: K;
  LList: TVector<T>;
begin
  Result := TMap<K, TVector<T>>.Create([]);
  for LItem in Self do
  begin
    LKey := AKeySelector(LItem);
    if not Result.TryGetValue(LKey, LList) then
    begin
      LList := TVector<T>.Create([]);
      Result.Add(LKey, LList);
    end;
    LList.Add(LItem);
    Result[LKey] := LList;
  end;
end;

function TListEx<T>.GroupByAndCount<K>: TMap<K, Integer>;
var
  LGroupDict: TMap<K, Integer>;
  LItem: T;
  LKey: K;
begin
  LGroupDict := TMap<K, Integer>.Create([]);
  try
    for LItem in Self do
    begin
      LKey := TValue.From<T>(LItem).AsType<K>;
      if LGroupDict.Contains(LKey) then
        LGroupDict[LKey] := LGroupDict[LKey] + 1
      else
        LGroupDict.Add(LKey, 1);
    end;
    Result := LGroupDict;
  except
    raise;
  end
end;

function TListEx<T>.IndexOfItem(const APredicate: TPredicate<T>): Integer;
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

function TListEx<T>.Intersect(const List2: TList<T>): TVector<T>;
var
  LItem: T;
begin
  Result := TVector<T>.Create([]);
  for LItem in Self do
  begin
    if List2.Contains(LItem) then
      Result.Add(LItem);
  end;
end;

function TListEx<T>.IsEmpty: Boolean;
begin
  Result := Self.Count = 0;
end;

function TListEx<T>.Join(const ASeparator: String): String;
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

function TListEx<T>.Map<R>(const AMappingFunc: TFunc<T, R>): TVector<R>;
var
  LItem: T;
begin
  Result := TVector<R>.Create([]);
  for LItem in Self do
    Result.Add(AMappingFunc(LItem));
end;

function TListEx<T>.Max: T;
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

function TListEx<T>.Min: T;
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
    if LIsFirst or (TComparer<T>.Default.Compare(LItem, Result) < 0) then
    begin
      Result := LItem;
      LIsFirst := False;
    end;
  end;
  if LIsFirst then
    raise Exception.Create('No minimum value found in the list.');
end;

function TListEx<T>.Partition(const APredicate: TPredicate<T>): TPairList<TVector<T>, TVector<T>>;
var
  LLeftList, LRightList: TVector<T>;
  LItem: T;
begin
  LLeftList := TVector<T>.Create([]);
  LRightList := TVector<T>.Create([]);
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

function TListEx<T>.PartitionBy(const APredicate: TPredicate<T>): TMap<Boolean, TVector<T>>;
var
  LPartitions: TMap<Boolean, TVector<T>>;
  LItem: T;
begin
  LPartitions := TMap<Boolean, TVector<T>>.Create([]);
  try
    for LItem in Self do
    begin
      if not LPartitions.Contains(APredicate(LItem)) then
        LPartitions[APredicate(LItem)] := TVector<T>.Create([]);
      LPartitions[APredicate(LItem)].Add(LItem);
    end;
    Result := LPartitions;
  except
    on E: Exception do
    begin
      raise;
    end;
  end;
end;

function TListEx<T>.Reduce(const AAccumulator: TFunc<T, T, T>): T;
var
  LItem: T;
begin
  if Self.Count = 0 then
    raise Exception.Create('List is empty, cannot reduce.');

  Result := Default(T);
  for LItem in Self do
    Result := AAccumulator(Result, LItem);
end;

function TListEx<T>.Reduce(const AAccumulator: TFunc<T, Tuple, Tuple>;
  const ATuple: Tuple): Tuple;
var
  LItem: T;
begin
  if Self.Count = 0 then
    raise Exception.Create('List is empty, cannot reduce.');

  Result := ATuple;
  for LItem in Self do
    Result := AAccumulator(LItem, Result);
end;

procedure TListEx<T>.Reverse;
var
  LTemp: T;
  LItemB, LItemA: Integer;
begin
  LItemB := 0;
  LItemA := Self.Count - 1;
  while LItemB < LItemA do
  begin
    LTemp := Self.Items[LItemB];
    Self.Items[LItemB] := Self.Items[LItemA];
    Self.Items[LItemA] := LTemp;
    Inc(LItemB);
    Dec(LItemA);
  end;
end;

function TListEx<T>.Reduce(const AAccumulator: TFunc<T, T, T>;
  const AInitial: T): T;
var
  LItem: T;
begin
  if Self.Count = 0 then
    raise Exception.Create('List is empty, cannot reduce.');

  Result := AInitial;
  for LItem in Self do
    Result := AAccumulator(Result, LItem);
end;

procedure TListEx<T>.Rotate(const Count: Integer);
var
  LTotalCount, LFor, LShiftedIndex: Integer;
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

procedure TListEx<T>.Shuffle;
var
  LI, LJ: Integer;
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

function TListEx<T>.Skip(const ACount: Integer): TVector<T>;
var
  LIndex: Integer;
begin
  Result := TVector<T>.Create([]);
  for LIndex := Count to Self.Count - 1 do
    Result.Add(Self[LIndex]);
end;

function TListEx<T>.SkipWhile(
  const APredicate: TPredicate<T>): TVector<T>;
var
  LFor: Integer;
begin
  Result := TVector<T>.Create([]);
  LFor := 0;
  while (LFor < Count) and APredicate(Items[LFor]) do
    Inc(LFor);
  while LFor < Count do
  begin
    Result.Add(Items[LFor]);
    Inc(LFor);
  end;
end;

function TListEx<T>.Slice(const AList: TList<T>; AStartIndex,
  AEndIndex: Integer): TVector<T>;
var
  LFor: Integer;
begin
  Result := TVector<T>.Create([]);
  for LFor := AStartIndex to AEndIndex do
    Result.Add(AList[LFor]);
end;

procedure TListEx<T>.SortBy(const ASelector: TFunc<T, TValue>);
begin
  Sort(TComparer<T>.Construct(
    function(const Left, Right: T): Integer
    begin
      Result := TComparer<TValue>.Default.Compare(ASelector(Left), ASelector(Right));
    end));
end;

function TListEx<T>.Take(const ACount: Integer): TVector<T>;
var
  LIndex: Integer;
begin
  Result := TVector<T>.Create([]);
  for LIndex := 0 to TStd.Min(ACount - 1, Self.Count - 1) do
  begin
    if LIndex >= Self.Count then
      break;
    Result.Add(Self[LIndex]);
  end;
end;

function TListEx<T>.TakeWhile(const APredicate: TPredicate<T>): TVector<T>;
var
  LFor: Integer;
begin
  Result := TVector<T>.Create([]);
  for LFor := 0 to Count - 1 do
  begin
    if not APredicate(Items[LFor]) then
      break;
    Result.Add(Items[LFor]);
  end;
end;

function TListEx<T>.ToString: String;
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

function TListEx<T>.Unique: TVector<T>;
var
  LItem: T;
begin
  Result := TVector<T>.Create([]);
  for LItem in Self do
  begin
    if not Result.Contains(LItem) then
      Result.Add(LItem);
  end;
end;

function TListEx<T>.Zip<T1, T2, R>(const AList1: TList<T1>;
  const AList2: TList<T2>; const AFunc: TFunc<T1, T2, R>): TVector<R>;
var
  LFor: Integer;
begin
  Result := TVector<R>.Create([]);
  for LFor := 0 to TStd.Min(AList1.Count, AList2.Count) - 1 do
    Result.Add(AFunc(AList1[LFor], AList2[LFor]));
end;

{ TPairList<L, R> }

constructor TPairList<L, R>.Create(const ALeft: L; const ARight: R);
begin
  Left := ALeft;
  Right := ARight;
end;

end.














