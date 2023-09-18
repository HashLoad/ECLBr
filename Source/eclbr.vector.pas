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

unit eclbr.vector;

interface

uses
  Rtti,
  SysUtils,
  StrUtils,
  TypInfo,
  Generics.Defaults,
  Generics.Collections;

type
  IVectorEnumerator<T> = interface
    ['{1E9F92D8-4EF1-4D15-9160-9B00013BA97D}']
    function GetCurrent: T;
    function MoveNext: boolean;
    property Current: T read GetCurrent;
  end;

  TVector<T> = record
  private
    type
      PArrayType = ^TArrayType;
      TArrayType = TArray<T>;

      TArrayManager = record
      private
        class var FCapacity: integer;
      private
        class function _GetCapacity: integer; static;
        class procedure _SetCapacity(var AArray: TArrayType; const ACapacity: integer); static;
        class function _GetCount(var AArray: TArrayType): integer; static;
        class function _IsEquals<I>(const ALeft: I; ARight: I): boolean; static;
      public
        class procedure Add(var AArray: TArrayType; const AItem: T); static;
        class procedure Insert(var AArray: TArrayType; const AIndex: integer; const AItem: T); static;
        class procedure Delete(var AArray: TArrayType; const AIndex: integer); static;
        class procedure Remove(var AArray: TArrayType; const AItem: T); static;
        class procedure SetLength(var AArray: TArrayType; ALength: integer); static;
        class function GetEnumerator(var AArray: TArrayType): IVectorEnumerator<T>; static;
        class function Contains(const AArray: TArrayType; const AItem: T): boolean; static;
        class function IndexOf(const AArray: TArrayType; const AItem: T): integer; static;
      end;
    type
      TVectorEnumerator = class(TInterfacedObject, IVectorEnumerator<T>)
      private
        FItems: PArrayType;
        FIndex: integer;
      protected
        function GetCurrent: T;
        function MoveNext: boolean;
      public
        constructor Create(const AArray: PArrayType);
        destructor Destroy; override;
      end;
  private
    FItems: TArrayType;
    function _GetItem(Index: integer): T;
    procedure _SetItem(Index: integer; const V: T);
    function _TrimItems: TArrayType;
    function _IsEquals<I>(const ALeft: I; ARight: I): boolean;
  public
    class operator Implicit(const V: TVector<T>): TArrayType;
    class operator Implicit(const V: TArrayType): TVector<T>;
    class operator Equal(const Left, Right: TVector<T>): boolean;
    class operator NotEqual(const Left, Right: TVector<T>): boolean;
    class function Empty: TVector<T>; static;

    /// <summary>
    ///   Creates a new vector initialized with the provided array of elements.
    /// </summary>
    /// <param name="Value">The array of elements used to initialize the vector.</param>
    constructor Create(const Value: TArrayType);

    /// <summary>
    ///   Adds an element to the end of the vector.
    /// </summary>
    /// <param name="AValue">The element to be added.</param>
    procedure Add(const AValue: T);

    /// <summary>
    ///   Inserts an element at the specified index in the vector.
    /// </summary>
    /// <param name="AIndex">The index at which to insert the element.</param>
    /// <param name="AItem">The element to be inserted.</param>
    procedure Insert(const AIndex: integer; const AItem: T);

    /// <summary>
    ///   Deletes the element at the specified index in the vector.
    /// </summary>
    /// <param name="AIndex">The index of the element to be deleted.</param>
    procedure Delete(const AIndex: integer);

    /// <summary>
    ///   Removes the first occurrence of a specified element from the vector.
    /// </summary>
    /// <param name="AItem">The element to be removed.</param>
    procedure Remove(const AItem: T);

    /// <summary>
    ///   Executes a provided action for each element in the vector.
    /// </summary>
    /// <param name="Action">The action to be executed for each element.</param>
    procedure ForEach(const Action: TProc<T>); overload;

    /// <summary>
    ///   Sets the current length of the vector.
    /// </summary>
    /// <param name="ALength">The new length of the vector.</param>
    procedure SetLength(const ALength: integer);

    /// <summary>
    ///   Sets the current capacity of the vector.
    /// </summary>
    /// <param name="ACapacity">The new capacity of the vector.</param>
    procedure SetCapacity(const ACapacity: integer);

    /// <summary>
    ///   Adds a collection of elements to the vector.
    /// </summary>
    /// <param name="ACollection">The collection of elements to be added.</param>
    procedure AddRange(const ACollection: TArrayType);

    /// <summary>
    ///   Concatenates a string with each element of the vector using a specified separator.
    /// </summary>
    /// <param name="AValue">The string to be concatenated with the elements.</param>
    /// <param name="ASeparator">The separator to be used between the elements.</param>
    procedure JoinStrings(const AValue: string; const ASeparator: string);

    /// <summary>
    ///   Clears the vector by removing all elements.
    /// </summary>
    procedure Clear;

    /// <summary>
    ///   Sorts the elements of the dictionary based on their keys in ascending order.
    /// </summary>
    /// <remarks>
    ///   This procedure rearranges the elements of the dictionary so that they are
    ///   sorted in ascending order based on their keys. After calling this procedure,
    ///   the dictionary will be sorted, and you can access its elements in a sorted
    ///   order using iterators or other methods.
    /// </remarks>
    procedure Sort;

    /// <summary>
    ///   Removes duplicate elements from the vector, keeping only one occurrence of each element.
    /// </summary>
    procedure Unique;

    /// <summary>
    ///   Returns an enumerator that allows iterating through the elements of the vector.
    /// </summary>
    function GetEnumerator: IVectorEnumerator<T>;

    /// <summary>
    ///   Determines whether the vector contains a specific element.
    /// </summary>
    /// <param name="AItem">The element to locate in the vector.</param>
    /// <returns><c>True</c> if the element is found in the vector; otherwise, <c>False</c>.</returns>
    function Contains(const AItem: T): boolean;

    /// <summary>
    ///   Searches for the specified element and returns the zero-based index of the first occurrence.
    /// </summary>
    /// <param name="AItem">The element to locate in the vector.</param>
    /// <returns>The zero-based index of the first occurrence of the element in the vector; otherwise, -1 if not found.</returns>
    function IndexOf(const AItem: T): integer;

    /// <summary>
    ///   Merges the elements of the source array into the vector, adding non-duplicate elements.
    /// </summary>
    /// <param name="ASourceArray">The source array to merge into the vector.</param>
    /// <returns>The vector with elements merged from the source array.</returns>
    function Merge(const ASourceArray: TArrayType): TVector<T>;

    /// <summary>
    ///   Filters the elements of the vector based on a provided predicate.
    /// </summary>
    /// <param name="APredicate">A function that determines whether an element should be included in the filtered vector.</param>
    /// <returns>A new vector containing the elements that satisfy the predicate.</returns>
    function Filter(const APredicate: TPredicate<T>): TVector<T>;

    /// <summary>
    ///   Mapeia os elementos desta coleção para uma nova coleção de tipo TResult usando a função de mapeamento fornecida.
    /// </summary>
    /// <param name="AMappingFunc">
    ///   A função de mapeamento que transforma cada elemento da coleção atual em um elemento do tipo TResult.
    /// </param>
    /// <returns>
    ///   Uma nova coleção contendo os elementos resultantes da aplicação da função de mapeamento.
    /// </returns>
    function Map<TResult: class>(const AMappingFunc: TFunc<T, TResult>): TVector<TResult>;

    /// <summary>
    ///   Returns the first element in the vector.
    /// </summary>
    /// <returns>The first element in the vector.</returns>
    function First: T;

    /// <summary>
    ///   Returns the last non-empty element in the vector.
    /// </summary>
    /// <returns>The last non-empty element in the vector.</returns>
    /// <exception cref="Exception">Thrown if no non-empty elements are found in the vector.</exception>
    function Last: T;

    /// <summary>
    ///   Determines whether the vector is empty (contains no non-empty elements).
    /// </summary>
    /// <returns><c>True</c> if the vector is empty; otherwise, <c>False</c>.</returns>
    function IsEmpty: boolean;

    /// <summary>
    ///   Returns a pointer to the TypeInfo of the vector's element type.
    /// </summary>
    /// <returns>A pointer to the TypeInfo of the vector's element type.</returns>
    function AsType: PTypeInfo;

    /// <summary>
    ///   Returns a pointer to the internal array of the vector.
    /// </summary>
    /// <returns>A pointer to the internal array of the vector.</returns>
    function AsPointer: PArrayType;

    /// <summary>
    ///   Converts the vector into a TList containing non-empty elements.
    /// </summary>
    /// <returns>A TList containing non-empty elements from the vector.</returns>
    function AsList: TList<T>;

    /// <summary>
    ///   Converte os elementos desta coleção em um array de tipo T.
    /// </summary>
    /// <remarks>
    ///   Este método cria um novo array contendo todos os elementos da coleção atual.
    /// </remarks>
    /// <returns>
    ///   Um array contendo os elementos da coleção.
    /// </returns>
    function ToArray: TArray<T>;

    /// <summary>
    ///   Converts the vector into a string representation containing non-empty elements.
    /// </summary>
    /// <returns>A string representation of the vector containing non-empty elements.</returns>
    function ToString: string;

    /// <summary>
    ///   Returns the number of non-empty elements in the vector.
    /// </summary>
    /// <returns>The number of non-empty elements in the vector.</returns>
    function Length: integer;

    /// <summary>
    ///   Returns the number of non-empty elements in the vector.
    /// </summary>
    /// <returns>The number of non-empty elements in the vector.</returns>
    function Count: integer;

    /// <summary>
    ///   Returns the capacity of the vector.
    /// </summary>
    /// <returns>The current capacity of the vector.</returns>
    function Capacity: integer;

    /// <summary>
    ///   Provides indexed access to elements of the vector.
    /// </summary>
    /// <param name="Index">The index of the element to access.</param>
    /// <returns>The element at the specified index.</returns>
    property Items[Index: integer]: T read _GetItem write _SetItem; default;
  end;

implementation

{ TArrayDictionary<T> }

function TVector<T>._GetItem(Index: integer): T;
begin
  Result := FItems[Index];
end;

function TVector<T>._IsEquals<I>(const ALeft: I; ARight: I): boolean;
begin
  Result := TEqualityComparer<I>.Default.Equals(ALeft, ARight);
end;

procedure TVector<T>._SetItem(Index: integer; const V: T);
begin
  FItems[Index] := V;
end;

function TVector<T>._TrimItems: TArrayType;
var
  LFor: integer;
  LIndex: integer;
begin
  System.SetLength(Result, Length);
  LIndex := 0;
  for LFor := 0 to High(FItems) do
  begin
    if _IsEquals<T>(FItems[LFor], Default(T)) then
      continue;
    Result[Lindex] := FItems[LFor];
    Inc(LIndex);
  end;
end;

procedure TVector<T>.SetCapacity(const ACapacity: integer);
begin
  TArrayManager._SetCapacity(FItems, ACapacity);
end;

function TVector<T>.GetEnumerator: IVectorEnumerator<T>;
begin
  Result := TArrayManager.GetEnumerator(FItems);
end;

procedure TVector<T>.Add(const AValue: T);
begin
  TArrayManager.Add(FItems, AValue);
end;

procedure TVector<T>.Insert(const AIndex: integer; const AItem: T);
begin
  TArrayManager.Insert(FItems, AIndex, AItem);
end;

function TVector<T>.IsEmpty: boolean;
begin
  Result := TArrayManager._GetCount(FItems) = 0;
end;

procedure TVector<T>.JoinStrings(const AValue: string; const ASeparator: string);
var
  LArray: TArray<string>;
  LValue: T;
  LItem: string;
begin
  FItems := nil;
  LArray := SplitString(AValue, ASeparator);
  for LItem in LArray do
  begin
    LValue := TValue.From<Variant>(LItem).AsType<T>;
    Self.Add(LValue);
  end;
end;

procedure TVector<T>.Unique;
var
  LUnique: TVector<T>;
  LItem: T;
begin
  LUnique := TVector<T>.Empty;
  for LItem in FItems do
  begin
    if LUnique.IndexOf(LItem) = -1 then
      LUnique.Add(LItem);
  end;
  FItems := LUnique.FItems;
end;

function TVector<T>.Last: T;
var
  LFor: integer;
begin
  for LFor := System.Length(FItems) - 1 downto 0 do
  begin
    if not _IsEquals<T>(FItems[LFor], Default(T)) then
    begin
      Result := FItems[LFor];
      exit;
    end;
  end;
  raise Exception.Create('No non-empty elements found');
end;

function TVector<T>.Map<TResult>(
  const AMappingFunc: TFunc<T, TResult>): TVector<TResult>;
var
  LItem: T;
begin
  Result := TVector<TResult>.Create([]);
  for LItem in Self do
    Result.Add(AMappingFunc(LItem));
end;

function TVector<T>.Merge(const ASourceArray: TArrayType): TVector<T>;
var
  LItem: T;
begin
  for LItem in ASourceArray do
  begin
    if IndexOf(LItem) = -1 then
      Add(LItem);
  end;
  Result := Self;
end;

class operator TVector<T>.NotEqual(const Left, Right: TVector<T>): boolean;
begin
  Result := not (Left = Right);
end;

function TVector<T>.AsPointer: PArrayType;
begin
  Pointer(Result) := FItems;
end;

procedure TVector<T>.Delete(const AIndex: integer);
begin
  TArrayManager.Delete(FItems, AIndex);
end;

function TVector<T>.Filter(const APredicate: TPredicate<T>): TVector<T>;
var
  LFiltered: TArrayType;
  LItem: T;
begin
  LFiltered := [];
  for LItem in FItems do
  begin
    if APredicate(LItem) then
    begin
      System.SetLength(LFiltered, System.Length(LFiltered) + 1);
      LFiltered[High(LFiltered)] := LItem;
    end;
  end;
  Result := TVector<T>.Create(LFiltered);
end;

function TVector<T>.First: T;
begin
  Result := Default(T);
  if System.Length(FItems) = 0 then
    exit;
  Result := FItems[0];
end;

function TVector<T>.AsType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

procedure TVector<T>.ForEach(const Action: TProc<T>);
var
  LItem: T;
begin
  for LItem in FItems do
    Action(LItem);
end;

procedure TVector<T>.Remove(const AItem: T);
begin
  TArrayManager.Remove(FItems, AItem);
end;

procedure TVector<T>.SetLength(const ALength: integer);
begin
  TArrayManager.SetLength(FItems, ALength);
end;

procedure TVector<T>.Sort;
begin
  TArray.Sort<T>(FItems);
end;

procedure TVector<T>.AddRange(const ACollection: TArrayType);
var
  LItem: T;
begin
  for LItem in ACollection do
    Add(LItem);
end;

function TVector<T>.AsList: TList<T>;
var
  LFor: Integer;
begin
  Result := TList<T>.Create;
  for LFor := 0 to High(FItems) do
  begin
    if not _IsEquals<T>(FItems[LFor], Default(T)) then
      Result.Add(FItems[LFor]);
  end;
end;

function TVector<T>.ToArray: TArray<T>;
var
  LItem: T;
  LIndex: integer;
begin
  System.SetLength(Result, Self.Count);
  LIndex := -1;
  for LItem in FItems do
  begin
    if _IsEquals<T>(LItem, Default(T)) then
      continue;
    Inc(LIndex);
    Result[LIndex] := LItem;
  end;
end;

function TVector<T>.ToString: string;
var
  LItem: T;
  LFormat: TValue;
  FirstNonEmpty: boolean;
begin
  Result := '';
  FirstNonEmpty := true;
  for LItem in FItems do
  begin
    if not _IsEquals<T>(LItem, Default(T)) then
    begin
      LFormat := TValue.From<T>(LItem);
      if not FirstNonEmpty then
        Result := Result + ', ';
      Result := Result + LFormat.ToString;
      FirstNonEmpty := False;
    end;
  end;
end;

function TVector<T>.Capacity: integer;
begin
  Result := TArrayManager._GetCapacity;
end;

procedure TVector<T>.Clear;
begin
  TArrayManager._SetCapacity(FItems, 0);
end;

function TVector<T>.Contains(const AItem: T): boolean;
begin
  Result := TArrayManager.Contains(FItems, AItem);
end;

function TVector<T>.Count: integer;
begin
  Result := TArrayManager._GetCount(FItems);
end;

function TVector<T>.Length: integer;
begin
  Result := Count;
end;

constructor TVector<T>.Create(const Value: TArrayType);
begin
  FItems := Value;
end;

function TVector<T>.IndexOf(const AItem: T): integer;
begin
  Result := TArrayManager.IndexOf(FItems, AItem);
end;

class function TVector<T>.Empty: TVector<T>;
begin
  Result.FItems := [];
end;

class operator TVector<T>.Equal(const Left, Right: TVector<T>): boolean;
var
  LFor: Integer;
begin
  Result := false;
  if System.Length(Left.FItems) <> System.Length(Right.FItems) then
    exit;
  for LFor := 0 to High(Left.FItems) do
  begin
    if not TEqualityComparer<T>.Default.Equals(Left.FItems[LFor], Right.FItems[LFor]) then
    begin
      exit;
    end;
  end;
  Result := True;
end;

class operator TVector<T>.Implicit(const V: TVector<T>): TArrayType;
begin
  Result := V._TrimItems;
end;

class operator TVector<T>.Implicit(const V: TArrayType): TVector<T>;
begin
  Result.FItems := V;
end;

{ TArrayDictionary<T>.TArrayHelper }

class function TVector<T>.TArrayManager._IsEquals<I>(const ALeft: I;
  ARight: I): boolean;
begin
  Result := TEqualityComparer<I>.Default.Equals(ALeft, ARight);
end;

class function TVector<T>.TArrayManager._GetCapacity: integer;
begin
  Result := FCapacity;
end;

class function TVector<T>.TArrayManager._GetCount(
  var AArray: TArrayType): integer;
var
  LFor: Integer;
begin
  Result := 0;
  for LFor := 0 to High(AArray) do
  begin
    if not _IsEquals<T>(AArray[LFor], Default(T)) then
      Inc(Result);
  end;
end;

class procedure TVector<T>.TArrayManager._SetCapacity(var AArray: TArrayType; const ACapacity: integer);
begin
  FCapacity := ACapacity;
  System.SetLength(AArray, FCapacity);
end;

class function TVector<T>.TArrayManager.GetEnumerator(var AArray: TArrayType): IVectorEnumerator<T>;
begin
  Result := TVectorEnumerator.Create(@AArray);
end;

class procedure TVector<T>.TArrayManager.Add(var AArray: TArrayType; const AItem: T);
var
  LIndex: Integer;
  LLength: Integer;
begin
  LIndex := -1;
  LLength := -1;
  if System.Length(AArray) > 0 then
  begin
    for LIndex := 0 to FCapacity do
    begin
      if _IsEquals<T>(AArray[LIndex], Default(T)) then
        break;
    end;
  end;
  if LIndex = -1 then
  begin
    LIndex := _GetCount(AArray);
    LLength := (LIndex + LIndex shr 3) + 32;
    _SetCapacity(AArray, LLength);
  end;
  AArray[LIndex] := AItem;
end;

class procedure TVector<T>.TArrayManager.Insert(var AArray: TArrayType;
  const AIndex: integer; const AItem: T);
var
  LFor: integer;
  LLength: integer;
begin
  LLength := FCapacity;
  if LLength = 0 then
  begin
    _SetCapacity(AArray, 32);
    LLength := FCapacity;
  end
  else if LLength <= AIndex then
  begin
    LLength := (AIndex + AIndex shr 3) + 32;
    _SetCapacity(AArray, LLength);
  end;
  for LFor := LLength - 1 downto AIndex + 1 do
    AArray[LFor] := AArray[LFor - 1];

  AArray[AIndex] := AItem;
end;

class procedure TVector<T>.TArrayManager.Delete(var AArray: TArrayType;
  const AIndex: integer);
var
  LFor: integer;
begin
  for LFor := AIndex + 1 to FCapacity - 1 do
    AArray[LFor - 1] := AArray[LFor];
  _SetCapacity(AArray, FCapacity - 1);
end;

class procedure TVector<T>.TArrayManager.Remove(var AArray: TArrayType; const AItem: T);
var
  LFor, LIndex: integer;
  LFound: boolean;
begin
  LIndex := -1;
  LFound := false;
  for LFor := 0 to FCapacity - 1 do
  begin
    if _IsEquals<T>(AArray[LFor], AItem) then
    begin
      LIndex := LFor;
      LFound := true;
      break;
    end;
  end;
  if LFound then
    Delete(AArray, LIndex);
end;

class function TVector<T>.TArrayManager.Contains(const AArray: TArrayType;
  const AItem: T): boolean;
var
  LFor: integer;
begin
  for LFor := 0 to System.Length(AArray) - 1 do
  begin
    if _IsEquals<T>(AArray[LFor], AItem) then
      exit(true);
  end;
  Result := false;
end;

class function TVector<T>.TArrayManager.IndexOf(const AArray: TArrayType;
  const AItem: T): integer;
var
  LFor: integer;
begin
  Result := -1;
  for LFor := Low(AArray) to High(AArray) do
  begin
    if not _IsEquals<T>(AArray[LFor], AItem) then
      continue;
    Result := LFor;
    break;
  end;
end;

class procedure TVector<T>.TArrayManager.SetLength(var AArray: TArrayType; ALength: integer);
begin
  if System.Length(AArray) >= ALength then
    raise Exception.Create('Memory allocation was not performed.');
  System.SetLength(AArray, ALength);
end;

{ TArrayData<T>.TArrayEnumerator }

constructor TVector<T>.TVectorEnumerator.Create(const AArray: PArrayType);
begin
  FItems := AArray;
  FIndex := -1;
end;

destructor TVector<T>.TVectorEnumerator.Destroy;
begin
  FItems := nil;
  inherited;
end;

function TVector<T>.TVectorEnumerator.GetCurrent: T;
begin
  Result := FItems^[FIndex];
end;

function TVector<T>.TVectorEnumerator.MoveNext: boolean;
begin
  repeat
    Inc(FIndex);
  until (FIndex >= System.Length(FItems^)) or not TEqualityComparer<T>.Default.Equals(FItems^[FIndex], Default(T));
  Result := FIndex < System.Length(FItems^);
end;

end.

