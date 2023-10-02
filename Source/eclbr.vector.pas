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
  @Discord(https://discord.gg/S5yvvGu7)
}

unit eclbr.vector;

interface

uses
  Rtti,
  SysUtils,
  StrUtils,
  TypInfo,
  Generics.Defaults,
  Generics.Collections,
  eclbr.core;

type
  Tuple = eclbr.core.Tuple;

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
        class function _GetCapacity: integer; static; inline;
        class procedure _SetCapacity(var AArray: TArrayType; const ACapacity: integer); static; inline;
        class function _GetCount(var AArray: TArrayType): integer; static; inline;
        class function _IsEquals<I>(const ALeft: I; ARight: I): boolean; static; inline;
      public
        class procedure Add(var AArray: TArrayType; const AItem: T); static; inline;
        class procedure Insert(var AArray: TArrayType; const AIndex: integer; const AItem: T); static; inline;
        class procedure Delete(var AArray: TArrayType; const AIndex: integer); static; inline;
        class procedure Remove(var AArray: TArrayType; const AItem: T); static; inline;
        class procedure SetLength(var AArray: TArrayType; ALength: integer); static; inline;
        class function GetEnumerator(var AArray: TArrayType): IVectorEnumerator<T>; static; inline;
        class function Contains(const AArray: TArrayType; const AItem: T): boolean; static; inline;
        class function IndexOf(const AArray: TArrayType; const AItem: T): integer; static; inline;
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
    procedure _SetItem(Index: integer; const V: T);
    function _GetItem(Index: integer): T;
    function _TrimItems: TArrayType;
    function _IsEquals<I>(const ALeft: I; ARight: I): boolean;
  public
    class operator Implicit(const V: TVector<T>): TArrayType; inline;
    class operator Implicit(const V: TArrayType): TVector<T>; inline;
    class operator Equal(const Left, Right: TVector<T>): boolean; inline;
    class operator NotEqual(const Left, Right: TVector<T>): boolean; inline;
    class operator Add(const Left, Right: TVector<T>): TVector<T>; inline;
    class operator Add(const Left: TVector<T>; const Right: TArrayType): TVector<T>; inline;
    class operator Add(const Left: TArrayType; const Right: TVector<T>): TVector<T>; inline;
    class operator Add(const Left: TVector<T>; const Right: T): TVector<T>; inline;
    class operator Add(const Left: T; const Right: TVector<T>): TVector<T>; inline;
    class operator Subtract(const Left, Right: TVector<T>): TVector<T>; inline;
    class operator Subtract(const Left: TVector<T>; const Right: T): TVector<T>; inline;
    class operator In(const Left: T; const Right: TVector<T>): boolean; inline;
    class operator In(const Left, Right: TVector<T>): boolean; inline;
    class operator In(const Left: TArrayType; const Right: TVector<T>): boolean; inline;

    /// <summary>
    ///   Creates and returns an empty vector of the specified type.
    /// </summary>
    /// <returns>
    ///   An empty vector of type <typeparamref name="T"/>.
    /// </returns>
    /// <remarks>
    ///   The class method Empty creates and returns an empty vector of the specified
    ///   type (<typeparamref name="T"/>). This vector contains no elements, and its
    ///   size is zero. It can serve as a basis for constructing a vector with elements
    ///   later on.
    /// </remarks>
    class function Empty: TVector<T>; static; inline;

    /// <summary>
    ///   Creates a new instance of the class and initializes it with an array of values.
    /// </summary>
    /// <param name="Value">
    ///   An array of elements of type <typeparamref name="TArrayType"/> to initialize the instance.
    /// </param>
    /// <remarks>
    ///   The constructor Create creates a new instance of the class and initializes it
    ///   with the elements provided in the <paramref name="Value"/> array. This allows
    ///   you to create an object of the class with initial values ready for use.
    /// </remarks>
    constructor Create(const Value: TArrayType);

    /// <summary>
    ///   Adds an element to the list.
    /// </summary>
    /// <param name="AValue">
    ///   The element of type <typeparamref name="T"/> to be added to the list.
    /// </param>
    /// <remarks>
    ///   The Add method adds the specified element, <paramref name="AValue"/>, to the
    ///   current list. The element is inserted at the end of the list. After addition,
    ///   the list contains all previous elements as well as the new element.
    /// </remarks>
    procedure Add(const AValue: T); inline;

    /// <summary>
    ///   Inserts an element at the specified index in the list.
    /// </summary>
    /// <param name="AIndex">
    ///   The index at which the element of type <typeparamref name="T"/> should be inserted.
    /// </param>
    /// <param name="AItem">
    ///   The element of type <typeparamref name="T"/> to be inserted into the list.
    /// </param>
    /// <remarks>
    ///   The Insert method inserts the specified element, <paramref name="AItem"/>, at
    ///   the position indicated by <paramref name="AIndex"/> in the current list. The
    ///   elements that were at or after the specified index are shifted to accommodate
    ///   the new element. After insertion, the list contains all previous elements as
    ///   well as the newly inserted element.
    /// </remarks>
    procedure Insert(const AIndex: integer; const AItem: T); inline;

    /// <summary>
    ///   Deletes the element at the specified index from the list.
    /// </summary>
    /// <param name="AIndex">
    ///   The index of the element to be deleted from the list.
    /// </param>
    /// <remarks>
    ///   The Delete method removes the element at the position indicated by
    ///   <paramref name="AIndex"/> from the current list. Any elements that were
    ///   after the deleted element are shifted to fill the gap. After deletion,
    ///   the list contains all previous elements except for the one that was deleted.
    /// </remarks>
    procedure Delete(const AIndex: integer); inline;

    /// <summary>
    ///   Removes the first occurrence of a specific element from the list.
    /// </summary>
    /// <param name="AItem">
    ///   The element of type <typeparamref name="T"/> to be removed from the list.
    /// </param>
    /// <remarks>
    ///   The Remove method searches for the first occurrence of the specified element,
    ///   <paramref name="AItem"/>, in the current list and removes it if found. If the
    ///   element is not present in the list, the list remains unchanged. If there are
    ///   multiple occurrences of the element, only the first one encountered is removed.
    /// </remarks>
    procedure Remove(const AItem: T); overload; inline;

    /// <summary>
    ///   Removes multiple occurrences of specific elements from the list.
    /// </summary>
    /// <param name="AItems">
    ///   An array of elements of type <typeparamref name="T"/> to be removed from the list.
    /// </param>
    /// <remarks>
    ///   The Remove method searches for all occurrences of the elements specified in the
    ///   <paramref name="AItems"/> array and removes them from the current list. If any
    ///   of the specified elements are not present in the list, the list remains unchanged.
    ///   If there are multiple occurrences of any element in the array, all of them are removed.
    /// </remarks>
    procedure Remove(const AItems: TArray<T>); overload; inline;

    /// <summary>
    ///   Executes a specified action for each element in the list.
    /// </summary>
    /// <param name="Action">
    ///   A delegate of type TProc<T> representing the action to be performed on each element.
    /// </param>
    /// <remarks>
    ///   The ForEach method iterates over each element in the current list and invokes
    ///   the specified action, <paramref name="Action"/>, passing each element as a parameter.
    ///   This allows you to perform a custom operation on each element in the list without
    ///   the need for explicit loops.
    /// </remarks>
    procedure ForEach(const Action: TProc<T>); overload; inline;

    /// <summary>
    ///   Sets the length of the list to the specified value.
    /// </summary>
    /// <param name="ALength">
    ///   The new length of the list, represented as an integer.
    /// </param>
    /// <remarks>
    ///   The SetLength method changes the length of the current list to the value specified
    ///   in <paramref name="ALength"/>. If <paramref name="ALength"/> is greater than the
    ///   current number of elements, new elements are added to the end of the list, each
    ///   initialized with default values. If <paramref name="ALength"/> is less than the
    ///   current number of elements, excess elements are removed from the end of the list.
    /// </remarks>
    procedure SetLength(const ALength: integer); inline;

    /// <summary>
    ///   Sets the capacity of the list to the specified value.
    /// </summary>
    /// <param name="ACapacity">
    ///   The new capacity of the list, represented as an integer.
    /// </param>
    /// <remarks>
    ///   The SetCapacity method changes the capacity of the current list to the value specified
    ///   in <paramref name="ACapacity"/>. Capacity refers to the maximum number of elements
    ///   the list can hold without resizing. If <paramref name="ACapacity"/> is greater than
    ///   the current capacity, the list may be reallocated to accommodate more elements.
    ///   If <paramref name="ACapacity"/> is less than the current capacity, the list remains
    ///   unchanged, but it may release excess memory if applicable.
    /// </remarks>
    procedure SetCapacity(const ACapacity: integer); inline;

    /// <summary>
    ///   Adds multiple elements from an array to the end of the list.
    /// </summary>
    /// <param name="ACollection">
    ///   An array of elements of type <typeparamref name="TArrayType"/> to be added to the list.
    /// </param>
    /// <remarks>
    ///   The AddRange method appends all elements from the specified array
    ///   <paramref name="ACollection"/> to the end of the current list. After
    ///   this operation, the list contains all its previous elements as well
    ///   as the new elements from the array.
    /// </remarks>
    procedure AddRange(const ACollection: TArrayType); inline;

    /// <summary>
    ///   Concatenates the elements of the list into a single string with separators.
    /// </summary>
    /// <param name="AValue">
    ///   The string to be inserted between the elements of the list in the resulting string.
    /// </param>
    /// <param name="ASeparator">
    ///   The string to be inserted between adjacent elements in the resulting string.
    /// </param>
    /// <remarks>
    ///   The JoinStrings method combines the elements of the list into a single string,
    ///   using the value specified in <paramref name="AValue"/> to separate the elements
    ///   and the value specified in <paramref name="ASeparator"/> to separate adjacent
    ///   elements. The resulting string is returned, but the original list is not modified.
    /// </remarks>
    procedure JoinStrings(const AValue: string; const ASeparator: string); inline;

    /// <summary>
    ///   Removes all elements from the list.
    /// </summary>
    /// <remarks>
    ///   The Clear method removes all elements from the current list, leaving it empty.
    ///   Any memory allocated for the elements is released, and the list is ready to be
    ///   populated with new elements.
    /// </remarks>
    procedure Clear; inline;

    /// <summary>
    ///   Assigns elements from an array to the list.
    /// </summary>
    /// <param name="Items">
    ///   An array containing elements of type <typeparamref name="T"/> to be assigned to the list.
    /// </param>
    /// <remarks>
    ///   The Assign procedure copies the elements from the specified array
    ///   <paramref name="Items"/> to the current list. The existing elements in
    ///   the list, if any, are replaced by the elements from the array. After
    ///   the assignment, the list contains the same elements as the array.
    /// </remarks>
    procedure Assign(const Items: TArray<T>);

    /// <summary>
    ///   Sorts the elements in the list in ascending order.
    /// </summary>
    /// <remarks>
    ///   The Sort procedure rearranges the elements in the current list so that they
    ///   are in ascending order based on their natural order. This operation modifies
    ///   the order of elements in the list.
    /// </remarks>
    procedure Sort; inline;

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
    procedure Unique;

    /// <summary>
    ///   Returns an enumerator for iterating through the elements in the list.
    /// </summary>
    /// <returns>
    ///   An instance of <typeparamref name="IVectorEnumerator<T>"/> that can be used to
    ///   iterate through the elements in the list.
    /// </returns>
    /// <remarks>
    ///   The GetEnumerator function provides an enumerator object that allows you to
    ///   iterate through the elements in the list in a forward direction. You can use
    ///   this enumerator in loops to access each element sequentially.
    /// </remarks>
    function GetEnumerator: IVectorEnumerator<T>; inline;

    /// <summary>
    ///   Checks if the list contains a specific element.
    /// </summary>
    /// <param name="AItem">
    ///   The element of type <typeparamref name="T"/> to be checked for existence in the list.
    /// </param>
    /// <returns>
    ///   True if the element <paramref name="AItem"/> is found in the list; otherwise, False.
    /// </returns>
    /// <remarks>
    ///   The Contains function with overload checks if the specified element
    ///   <paramref name="AItem"/> exists in the current list. It returns True if
    ///   the element is found, and False if it is not found.
    /// </remarks>
    function Contains(const AItem: T): boolean; overload; inline;

    /// <summary>
    ///   Checks if the list contains all elements from a specified array.
    /// </summary>
    /// <param name="AItems">
    ///   An array of elements of type <typeparamref name="TArrayType"/> to be checked for
    ///   existence in the list.
    /// </param>
    /// <returns>
    ///   True if all elements from the array <paramref name="AItems"/> are found in the list;
    ///   otherwise, False.
    /// </returns>
    /// <remarks>
    ///   The Contains function with overload checks if all elements specified in the
    ///   <paramref name="AItems"/> array exist in the current list. It returns True if
    ///   all elements are found, and False if any of them are not found.
    /// </remarks>
    function Contains(const AItems: TArrayType): boolean; overload;

    /// <summary>
    ///   Returns the index of the first occurrence of a specific element in the list.
    /// </summary>
    /// <param name="AItem">
    ///   The element of type <typeparamref name="T"/> to search for in the list.
    /// </param>
    /// <returns>
    ///   The zero-based index of the first occurrence of the element <paramref name="AItem"/>
    ///   in the list, or -1 if the element is not found.
    /// </returns>
    /// <remarks>
    ///   The IndexOf function searches for the first occurrence of the specified element
    ///   <paramref name="AItem"/> in the current list and returns its zero-based index if
    ///   found. If the element is not found in the list, it returns -1.
    /// </remarks>
    function IndexOf(const AItem: T): integer; inline;

    /// <summary>
    ///   Merges the elements of the list with those from a specified array.
    /// </summary>
    /// <param name="ASourceArray">
    ///   An array of elements of type <typeparamref name="TArrayType"/> to be merged
    ///   with the elements of the list.
    /// </param>
    /// <returns>
    ///   A new instance of <typeparamref name="TVector<T>"/> containing the merged elements.
    /// </returns>
    /// <remarks>
    ///   The Merge function combines the elements of the current list with those from
    ///   the specified array <paramref name="ASourceArray"/> to create a new instance
    ///   of <typeparamref name="TVector<T>"/>. The order of elements is preserved.
    /// </remarks>
    function Merge(const ASourceArray: TArrayType): TVector<T>; inline;

    /// <summary>
    ///   Filters the elements of the list based on a specified predicate function.
    /// </summary>
    /// <param name="APredicate">
    ///   A predicate function of type TPredicate<T> that
    ///   determines whether an element should be included in the filtered result.
    /// </param>
    /// <returns>
    ///   A new instance of TVector<T> containing the elements
    ///   that satisfy the given predicate.
    /// </returns>
    /// <remarks>
    ///   The Filter function with overload applies the specified predicate function
    ///   APredicate to each element in the current list and includes
    ///   only the elements that satisfy the predicate in the filtered result.
    /// </remarks>
    function Filter(const APredicate: TPredicate<T>): TVector<T>; overload; inline;

    /// <summary>
    ///   Filters the elements of the list based on a specified predicate function
    ///   with an index parameter.
    /// </summary>
    /// <param name="APredicate">
    ///   A predicate function of type TFunc<T, integer, boolean> that determines
    ///   whether an element should be included in the filtered result.
    /// </param>
    /// <returns>
    ///   A new instance of TVector<T> containing the elements that satisfy the given predicate.
    /// </returns>
    /// <remarks>
    ///   The Filter function with overload applies the specified predicate function
    ///   APredicate to each element in the current list, along with its index, and
    ///   includes only the elements that satisfy the predicate in the filtered result.
    /// </remarks>
    function Filter(const APredicate: TFunc<T, integer, boolean>): TVector<T>; overload; inline;

    /// <summary>
    ///   Applies a mapping function to each element in the list and returns a new list
    ///   containing the results of the mapping.
    /// </summary>
    /// <typeparam name="R">
    ///   The type of the result elements after applying the mapping function.
    /// </typeparam>
    /// <param name="AMappingFunc">
    ///   A mapping function of type TFunc<T, R> that transforms each element in the list
    ///   to a result of type R.
    /// </param>
    /// <returns>
    ///   A new instance of TVector<R> containing the results of applying the mapping
    ///   function to each element in the original list.
    /// </returns>
    /// <remarks>
    ///   The Map function applies the specified mapping function AMappingFunc to each
    ///   element in the current list and constructs a new list of type TVector<R> that
    ///   contains the results of the mapping.
    /// </remarks>
    function Map<R>(const AMappingFunc: TFunc<T, R>): TVector<R>; inline;

    /// <summary>
    ///   Reduces the elements in the list to a single value using an accumulator function.
    /// </summary>
    /// <param name="AAccumulator">
    ///   An accumulator function of type TFunc<T, T, T> that combines two elements to
    ///   produce a single result.
    /// </param>
    /// <returns>
    ///   The result of applying the accumulator function to all elements in the list.
    /// </returns>
    /// <remarks>
    ///   The Reduce function with overload applies the specified accumulator function
    ///   AAccumulator to combine the elements in the current list into a single result.
    ///   It starts with the first element as an initial value and combines each subsequent
    ///   element with the accumulated result until all elements have been processed.
    /// </remarks>
    function Reduce(const AAccumulator: TFunc<T, T, T>): T; overload; inline;

    /// <summary>
    ///   Reduces the elements in the list to a single value using an accumulator function
    ///   and an initial value.
    /// </summary>
    /// <param name="AAccumulator">
    ///   An accumulator function of type TFunc<T, T, T> that combines two elements to
    ///   produce a single result.
    /// </param>
    /// <param name="AInitial">
    ///   The initial value of type T used as the starting point for the reduction process.
    /// </param>
    /// <returns>
    ///   The result of applying the accumulator function to all elements in the list,
    ///   starting with the initial value.
    /// </returns>
    /// <remarks>
    ///   The Reduce function with overload applies the specified accumulator function
    ///   AAccumulator to combine the elements in the current list into a single result.
    ///   It starts with the initial value AInitial and combines each subsequent element
    ///   with the accumulated result until all elements have been processed.
    /// </remarks>
    function Reduce(const AAccumulator: TFunc<T, T, T>; const AInitial: T): T; overload; inline;

    /// <summary>
    ///   Reduces the elements in the list to a single value using an accumulator function
    ///   and an initial value represented as a tuple.
    /// </summary>
    /// <param name="AAccumulator">
    ///   An accumulator function of type TFunc<T, Tuple, Tuple> that combines an element
    ///   and a tuple into a new tuple.
    /// </param>
    /// <param name="ATuple">
    ///   The initial value represented as a tuple, used as the starting point for the
    ///   reduction process.
    /// </param>
    /// <returns>
    ///   The result of applying the accumulator function to all elements in the list,
    ///   starting with the initial value represented as a tuple.
    /// </returns>
    /// <remarks>
    ///   The Reduce function with overload applies the specified accumulator function
    ///   AAccumulator to combine the elements in the current list into a single result.
    ///   It starts with the initial value represented as a tuple (ATuple) and combines
    ///   each subsequent element with the accumulated result (a tuple) until all elements
    ///   have been processed.
    /// </remarks>
    function Reduce(const AAccumulator: TFunc<T, Tuple, Tuple>; const ATuple: Tuple): Tuple; overload; inline;

    /// <summary>
    ///   Retrieves the first element in the list.
    /// </summary>
    /// <returns>
    ///   The first element of type T in the list.
    /// </returns>
    /// <remarks>
    ///   The First function returns the first element in the current list. If the list is
    ///   empty, it may raise an exception or return a default value depending on the
    ///   implementation. Ensure that the list is not empty before calling this function.
    /// </remarks>
    function First: T; inline;

    /// <summary>
    ///   Retrieves the last element in the list.
    /// </summary>
    /// <returns>
    ///   The last element of type T in the list.
    /// </returns>
    /// <remarks>
    ///   The Last function returns the last element in the current list. If the list is
    ///   empty, it may raise an exception or return a default value depending on the
    ///   implementation. Ensure that the list is not empty before calling this function.
    /// </remarks>
    function Last: T; inline;

    /// <summary>
    ///   Checks if the list is empty.
    /// </summary>
    /// <returns>
    ///   True if the list is empty; otherwise, False.
    /// </returns>
    /// <remarks>
    ///   The IsEmpty function determines whether the current list contains any elements.
    ///   It returns True if the list is empty and contains no elements, and False if the
    ///   list contains one or more elements.
    /// </remarks>
    function IsEmpty: boolean; inline;

    /// <summary>
    ///   Retrieves the type information for the elements in the list.
    /// </summary>
    /// <returns>
    ///   A pointer to type information (PTypeInfo) representing the type of elements
    ///   stored in the list.
    /// </returns>
    /// <remarks>
    ///   The AsType function returns a pointer to type information (PTypeInfo) that
    ///   represents the data type of the elements stored in the current list. This can
    ///   be used to determine the type of elements in the list at runtime.
    /// </remarks>
    function AsType: PTypeInfo;

    /// <summary>
    ///   Retrieves a pointer to the internal array of elements in the list.
    /// </summary>
    /// <returns>
    ///   A pointer to an array of elements of type PArrayType that represents the
    ///   internal storage of elements in the list.
    /// </returns>
    /// <remarks>
    ///   The AsPointer function returns a pointer to the internal array of elements
    ///   in the current list. This can be used when direct access to the underlying
    ///   array is needed for specific operations or optimizations.
    /// </remarks>
    function AsPointer: PArrayType;

    /// <summary>
    ///   Retrieves a reference to a TList<T> that wraps the elements in the list.
    /// </summary>
    /// <returns>
    ///   A reference to a TList<T> that provides access to the elements in the list.
    /// </returns>
    /// <remarks>
    ///   The AsList function returns a reference to a TList<T> that wraps the elements
    ///   in the current list. This allows you to work with the list as a TList<T> object
    ///   and use its methods and properties for list manipulation.
    /// </remarks>
    function AsList: TList<T>;

    /// <summary>
    ///   Converts the elements in the list to an array of type TArray<T>.
    /// </summary>
    /// <returns>
    ///   An array of type TArray<T> containing the elements from the list.
    /// </returns>
    /// <remarks>
    ///   The ToArray function converts the elements in the current list into an array
    ///   of type TArray<T>. This allows you to work with the list's elements as an
    ///   array and provides compatibility with array-based operations.
    /// </remarks>
    function ToArray: TArray<T>; inline;

    /// <summary>
    ///   Converts the elements in the list to a string representation.
    /// </summary>
    /// <returns>
    ///   A string representation of the elements in the list.
    /// </returns>
    /// <remarks>
    ///   The ToString function converts the elements in the current list into a string
    ///   representation. This allows you to obtain a human-readable or log-friendly
    ///   representation of the list's contents.
    /// </remarks>
    function ToString: string; inline;

    /// <summary>
    ///   Retrieves the number of elements in the list.
    /// </summary>
    /// <returns>
    ///   The number of elements in the list as an integer.
    /// </returns>
    /// <remarks>
    ///   The Length function returns the count of elements in the current list, providing
    ///   the total number of elements stored in the list.
    /// </remarks>
    function Length: integer; inline;

    /// <summary>
    ///   Retrieves the number of elements in the list.
    /// </summary>
    /// <returns>
    ///   The number of elements in the list as an integer.
    /// </returns>
    /// <remarks>
    ///   The Count function returns the count of elements in the current list, providing
    ///   the total number of elements stored in the list.
    /// </remarks>
    function Count: integer; inline;

    /// <summary>
    ///   Retrieves the current capacity of the list.
    /// </summary>
    /// <returns>
    ///   The current capacity of the list as an integer.
    /// </returns>
    /// <remarks>
    ///   The Capacity function returns the current capacity of the list, which represents
    ///   the maximum number of elements that the list can hold efficiently without
    ///   reallocation. It can be used to check the current capacity of the list.
    /// </remarks>
    function Capacity: integer; inline;

    /// <summary>
    ///   Provides access to elements in the list using an indexer.
    /// </summary>
    /// <param name="Index">
    ///   The zero-based index of the element to access.
    /// </param>
    /// <returns>
    ///   The element of type T at the specified index in the list.
    /// </returns>
    /// <value>
    ///   The value to assign to the element at the specified index.
    /// </value>
    /// <remarks>
    ///   The Items property allows you to access elements in the list using an indexer.
    ///   You can retrieve the value of an element at a specific index using the "read"
    ///   accessor and assign a value to an element at a specific index using the "write"
    ///   accessor. This property enables you to treat the list like an array for element
    ///   access.
    /// </remarks>
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

function TVector<T>.Map<R>(const AMappingFunc: TFunc<T, R>): TVector<R>;
var
  LItem: T;
begin
  Result := TVector<R>.Create([]);
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

procedure TVector<T>.Assign(const Items: TArray<T>);
begin
  FItems := TArray.Copy<T>(Items);
end;

procedure TVector<T>.Delete(const AIndex: integer);
begin
  TArrayManager.Delete(FItems, AIndex);
end;

function TVector<T>.Filter(const APredicate: TPredicate<T>): TVector<T>;
var
  LItem: T;
begin
  Result := TVector<T>.Create([]);
  for LItem in FItems do
  begin
    if APredicate(LItem) then
      Result.Add(LItem);
  end;
end;

function TVector<T>.Filter(const APredicate: TFunc<T, integer, boolean>): TVector<T>;
var
  LItem: T;
  LIndex: integer;
begin
  Result := TVector<T>.Create([]);
  LIndex := 0;
  for LItem in FItems do
  begin
    if APredicate(LItem, LIndex) then
      Result.Add(LItem);
    Inc(LIndex);
  end;
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

function TVector<T>.Reduce(const AAccumulator: TFunc<T, T, T>): T;
var
  LItem: T;
begin
  if Self.Count = 0 then
    raise Exception.Create('Vector is empty, cannot reduce.');

  Result := Default(T);
  for LItem in Self do
    Result := AAccumulator(Result, LItem);
end;

function TVector<T>.Reduce(const AAccumulator: TFunc<T, Tuple, Tuple>;
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

procedure TVector<T>.Remove(const AItems: TArray<T>);
var
  LFor, LIndex: integer;
begin
  for LFor := 0 to System.Length(AItems) - 1 do
  begin
    LIndex := IndexOf(AItems[LFor]);
    if LIndex > -1 then
      Delete(LIndex);
  end;
end;

function TVector<T>.Reduce(const AAccumulator: TFunc<T, T, T>;
  const AInitial: T): T;
var
  LItem: T;
begin
  if Self.Count = 0 then
    raise Exception.Create('Vector is empty, cannot reduce.');

  Result := AInitial;
  for LItem in Self do
    Result := AAccumulator(Result, LItem);
end;

procedure TVector<T>.Remove(const AItem: T);
begin
  TArrayManager.Remove(FItems, AItem);
end;

procedure TVector<T>.Reverse;
var
  LTemp: T;
  LItemB, LItemA: Integer;
begin
  LItemB := 0;
  LItemA := Self.Count - 1;
  while LItemB < LItemA do
  begin
    LTemp := FItems[LItemB];
    FItems[LItemB] := FItems[LItemA];
    FItems[LItemA] := LTemp;
    Inc(LItemB);
    Dec(LItemA);
  end;
end;

procedure TVector<T>.SetLength(const ALength: integer);
begin
  TArrayManager.SetLength(FItems, ALength);
end;

procedure TVector<T>.Sort;
begin
  TArray.Sort<T>(FItems);
end;

class operator TVector<T>.Add(const Left: TArrayType;
  const Right: TVector<T>): TVector<T>;
begin
  Result := Left;
  Result.AddRange(Right.FItems);
end;

class operator TVector<T>.Add(const Left: TVector<T>;
  const Right: TArrayType): TVector<T>;
begin
  Result := Left;
  Result.AddRange(Right);
end;

class operator TVector<T>.Add(const Left, Right: TVector<T>): TVector<T>;
begin
  Result := Left;
  Result.AddRange(Right.FItems);
end;

class operator TVector<T>.Add(const Left: TVector<T>;
  const Right: T): TVector<T>;
begin
  Result := Left;
  Result.Add(Right);
end;

class operator TVector<T>.Add(const Left: T;
  const Right: TVector<T>): TVector<T>;
begin
  System.SetLength(Result.FItems, 1);
  Result.FItems[0] := Left;
  Result.AddRange(Right);
end;

class operator TVector<T>.Subtract(const Left: TVector<T>;
  const Right: T): TVector<T>;
begin
  Result := Left;
  Result.Remove(Right);
end;

class operator TVector<T>.Subtract(const Left, Right: TVector<T>): TVector<T>;
begin
  Result := Left;
  Result.Remove(Right.FItems);
end;

class operator TVector<T>.In(const Left: T; const Right: TVector<T>): boolean;
begin
  Result := Right.Contains(Left);
end;

class operator TVector<T>.In(const Left, Right: TVector<T>): boolean;
begin
  Result := Right.Contains(Left.FItems);
end;

class operator TVector<T>.In(const Left: TArrayType; const Right: TVector<T>): boolean;
begin
  Result := Right.Contains(Left);
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

function TVector<T>.Contains(const AItems: TArrayType): boolean;
var
  LFor: Integer;
begin
  Result := false;
  for LFor := 0 to System.Length(AItems) - 1 do
    if IndexOf(AItems[LFor]) = -1 then
      exit;
  Result := true;
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
  LComparer: IEqualityComparer<T>;
  LFor: Integer;
begin
  Result := false;
  if System.Length(Left.FItems) <> System.Length(Right.FItems) then
    exit;

  LComparer := TEqualityComparer<T>.Default;
  for LFor := 0 to High(Left.FItems) do
  begin
    if not LComparer.Equals(Left.FItems[LFor], Right.FItems[LFor]) then
      exit;
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


