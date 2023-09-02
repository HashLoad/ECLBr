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

unit eclbr.sysmatch;

interface

uses
  Rtti,
  SysUtils,
  TypInfo,
  Variants,
  Generics.Collections,
  Generics.Defaults,
  eclbr.regexlib,
  eclbr.result.pair;

type
  // Enumeration to represent different states of the match session
  TMatchSession = (sMatch, sGuard, sCase, sDefault, sTryExcept);

  {$REGION 'Doc - TCaseGroup'}
  ///  <summary>
  ///    A class representing a group of custom match cases.
  ///    A case group is defined as a dictionary where the key (TValue) represents
  ///    the value of the case to be compared, and the value (TValue) represents the action associated with that case.
  ///    This allows you to define custom match operators, such as CaseGt and CaseLt,
  ///    that check specific conditions to perform custom actions.
  ///    For example, you can create a TCaseGroup to represent a series of "greater than" comparisons
  ///    and associate each comparison with a specific procedure to be executed if the condition is met.
  ///  </summary>
  ///  <typeparam name="TValue">
  ///    The type of value for match cases. It can be any type for which comparison and associated actions are defined.
  ///  </typeparam>
  ///  <remarks>
  ///    Note that the responsibility for checking match conditions and executing associated actions
  ///    lies with the user of the TCaseGroup class.
  ///  </remarks>
  {$ENDREGION}
  TCaseGroup = TDictionary<TValue, TValue>;

  // Class implementing the pattern matching
  TMatch<T> = record
  private
    // Class variables to store relevant matching information
    FValue: TValue;                           // Value to be matched
    FSession: TMatchSession;                  // Current state of the matching session
    FUseGuard: boolean;                       // Indicates if guard is being used
    FUseRegex: boolean;                       // Indicates if regex is being used
    FGuardCount: integer;                     // Counter for guard
    FRegexCount: integer;                     // Counter for regex
    FCaseDictionary: TDictionary<string, TCaseGroup>; // Dictionary of simple cases
  private
    // Private methods for different types of matching
    function _MatchingCaseEq: boolean;
    function _MatchingCaseGt: boolean;
    function _MatchingCaseLt: boolean;
    function _MatchingCaseIn: boolean;
    function _MatchingCaseIfProc: boolean;
    function _MatchingCaseIfFunc: boolean;
    function _MatchingCaseRange: boolean;
    function _MatchingCaseIs: boolean;
    function _MatchingDefault: boolean;
    function _MatchingTryExcept: boolean;
    // Private methods for array comparison
    function _ArraysAreEqual(const Arr1: TValue; const Arr2: TValue): boolean;
    function _ArraysAreEqualInteger(const Arr1: TValue; const Arr2: TValue): boolean;
    function _ArraysAreEqualChar(const Arr1: TValue; const Arr2: TValue): boolean;
    function _ArraysAreEqualString(const Arr1: TValue; const Arr2: TValue): boolean;
    function _ArraysAreNotEqualCaseIf(const APair: TPair<TValue, TValue>): boolean;
    // Private methods for checking array types
    function _IsArrayInteger(const AValue: TValue): boolean;
    function _IsArrayChar(const AValue: TValue): boolean;
    function _IsArrayString(const AValue: TValue): boolean;
    // Private methods for executing anonymous methods
    procedure _ExecuteProcMatchingEq(const ProcValue: TValue);
    procedure _ExecuteProcMatchingIn(const ProcValue: TValue);
    procedure _ExecuteProcMatchingIs(const ProcValue: TValue);
    procedure _ExecuteProcMatchingLt(const ProcValue: TValue);
    procedure _ExecuteProcMatchingGt(const ProcValue: TValue);
    procedure _ExecuteProcMatchingRange(const ProcValue: TValue);
    procedure _ExecuteProcMatchingCaseIf(const ProcValue: TValue);
    function _ExecuteFuncMatchingCaseIf(const ProcValue: TValue): boolean;
    // Private methods for managing the toxicity of the Execute() method
    function _ExecuteSession: boolean;
    function _ExecuteGuard: boolean;
    function _ExecuteRegex: boolean;
    function _ExecuteCaseEq: boolean;
    function _ExecuteCaseGt: boolean;
    function _ExecuteCaseLt: boolean;
    function _ExecuteCaseIn: boolean;
    function _ExecuteCaseIs: boolean;
    function _ExecuteCaseRange: boolean;
    function _ExecuteCaseDefault: boolean;
    function _ExecuteCaseIf_Proc: boolean;
    function _ExecuteCaseIf_Func: boolean;
    function _ExecuteCasesValidation: TResultPair<boolean, string>;
    // Private method for resetting variables
    procedure _StartVariables;
    // Private method for releasing variables
    procedure _Dispose;
    constructor Create(const AValue: T);
  public
    {$REGION 'Doc - Match'}
    /// <summary>
    ///   Initializes a new instance of the matching pattern for the specified type.
    /// </summary>
    /// <param name="AValue">
    ///   The value to be matched.
    /// </param>
    /// <returns>
    ///   A new instance of the TPattern&lt;T&gt; class to allow the definition of matching cases.
    /// </returns>
    {$ENDREGION}
    class function Value(const AValue: T): TMatch<T>; static;

    {$REGION 'Doc - CaseIf'}
    /// <summary>
    ///   Defines a matching condition for the current case, enabling a code block to be executed
    ///   only if the specified condition is evaluated as true.
    /// </summary>
    /// <param name="ACondition">
    ///   The condition to be evaluated to determine if the case should be executed.
    /// </param>
    /// <returns>
    ///   An instance of the current matching pattern, allowing the definition of actions
    ///   to be executed if the specified condition is true.
    /// </returns>
    {$ENDREGION}
    function CaseIf(const ACondition: boolean): TMatch<T>; overload;

    {$REGION 'Doc - CaseIf'}
    /// <summary>
    ///   Defines a matching condition for the current case, enabling a code block to be executed
    ///   only if the specified condition is evaluated as true.
    /// </summary>
    /// <param name="ACondition">
    ///   The condition to be evaluated to determine if the case should be executed.
    /// </param>
    /// <param name="AProc">
    ///   The procedure to be executed if the specified condition is true.
    ///   This procedure does not receive any parameters.
    /// </param>
    /// <returns>
    ///   An instance of the current matching pattern, allowing the definition of
    ///   additional actions to be executed if the condition is true.
    /// </returns>
    {$ENDREGION}
    function CaseIf(const ACondition: boolean; const AProc: TProc): TMatch<T>; overload;

    {$REGION 'Doc - CaseIf'}
    /// <summary>
    ///   Defines a matching condition for the current case, enabling a code block to be executed
    ///   only if the specified condition is evaluated as true.
    /// </summary>
    /// <param name="ACondition">
    ///   The condition to be evaluated to determine if the case should be executed.
    /// </param>
    /// <param name="AProc">
    ///   The procedure to be executed if the specified condition is true.
    ///   This procedure receives a parameter of generic type <typeparamref name="T"/>.
    /// </param>
    /// <returns>
    ///   An instance of the current matching pattern, allowing the definition of
    ///   additional actions to be executed if the condition is true.
    /// </returns>
    {$ENDREGION}
    function CaseIf(const ACondition: boolean; const AProc: TProc<T>): TMatch<T>; overload;

    {$REGION 'Doc - CaseIf'}
    /// <summary>
    ///   Defines a matching condition for the current case, enabling a code block to be executed
    ///   only if the specified condition is evaluated as true.
    /// </summary>
    /// <param name="ACondition">
    ///   The condition to be evaluated to determine if the case should be executed.
    /// </param>
    /// <param name="AFunc">
    ///   A function that evaluates the condition. It should return a boolean value,
    ///   where true indicates that the case should be executed, and false indicates otherwise.
    /// </param>
    /// <returns>
    ///   An instance of the current matching pattern, allowing the definition of
    ///   additional actions to be executed if the condition is true.
    /// </returns>
    {$ENDREGION}
    function CaseIf(const ACondition: boolean; const AFunc: TFunc<boolean>): TMatch<T>; overload;

    {$REGION 'Doc - CaseIf'}
    /// <summary>
    ///   Defines a matching condition for the current case, enabling a code block to be executed
    ///   only if the specified condition is evaluated as true.
    /// </summary>
    /// <param name="ACondition">
    ///   The condition to be evaluated to determine if the case should be executed.
    /// </param>
    /// <param name="AFunc">
    ///   A function that evaluates the condition based on the case's value. It should receive the case's value
    ///   as a parameter and return a boolean value, where true indicates that the case should be executed,
    ///   and false indicates otherwise.
    /// </param>
    /// <returns>
    ///   An instance of the current matching pattern, allowing the definition of
    ///   additional actions to be executed if the condition is true.
    /// </returns>
    {$ENDREGION}
    function CaseIf(const ACondition: boolean; const AFunc: TFunc<T, boolean>): TMatch<T>; overload;

    {$REGION 'Doc - CaseEq'}
    /// <summary>
    ///   Defines a matching case that is activated when the case's value is equal to the specified value.
    ///   The code block associated with this case will be executed when equality is verified.
    /// </summary>
    /// <param name="AValue">
    ///   The value to be compared with the case's value to check for equality.
    /// </param>
    /// <param name="AProc">
    ///   A procedure that will be executed if the case's value is equal to the specified value.
    /// </param>
    /// <returns>
    ///   An instance of the current matching pattern, allowing the definition of more
    ///   actions to be executed if equality is verified.
    /// </returns>
    {$ENDREGION}
    function CaseEq(const AValue: T; const AProc: TProc): TMatch<T>; overload;

    {$REGION 'Doc - CaseEq'}
    /// <summary>
    ///   Defines a matching case that is activated when the case's value is equal to the specified value.
    ///   The code block associated with this case will be executed when equality is verified.
    /// </summary>
    /// <param name="AValue">
    ///   The value to be compared with the case's value to check for equality.
    /// </param>
    /// <param name="AProc">
    ///   A procedure that will be executed if the case's value is equal to the specified value.
    ///   The case's value will be passed as a parameter to the procedure.
    /// </param>
    /// <returns>
    ///   An instance of the current matching pattern, allowing the definition of more
    ///   actions to be executed if equality is verified.
    /// </returns>
    {$ENDREGION}
    function CaseEq(const AValue: T; const AProc: TProc<T>): TMatch<T>; overload;

    {$REGION 'Doc - CaseGt'}
    /// <summary>
    ///   Defines a matching case that is activated when the case's value is greater than the specified value.
    ///   The code block associated with this case will be executed if the "greater than" condition is verified.
    /// </summary>
    /// <param name="AValue">
    ///   The value to be compared with the case's value to check the "greater than" condition.
    /// </param>
    /// <param name="AProc">
    ///   A procedure that will be executed if the case's value is greater than the specified value.
    /// </param>
    /// <returns>
    ///   An instance of the current matching pattern, allowing the definition of more actions
    ///   to be executed if the "greater than" condition is verified.
    /// </returns>
    {$ENDREGION}
    function CaseGt(const AValue: T; const AProc: TProc): TMatch<T>; overload;

    {$REGION 'Doc - CaseGt'}
    /// <summary>
    ///   Defines a matching case that is activated when the case's value is greater than the specified value.
    ///   The code block associated with this case will be executed if the "greater than" condition is verified.
    /// </summary>
    /// <param name="AValue">
    ///   The value to be compared with the case's value to check the "greater than" condition.
    /// </param>
    /// <param name="AProc">
    ///   A procedure that will be executed if the case's value is greater than the specified value.
    ///   The case's value will be passed as a parameter to the procedure.
    /// </param>
    /// <returns>
    ///   An instance of the current matching pattern, allowing the definition of more actions
    ///   to be executed if the "greater than" condition is verified.
    /// </returns>
    {$ENDREGION}
    function CaseGt(const AValue: T; const AProc: TProc<T>): TMatch<T>; overload;

    {$REGION 'Doc - CaseLt'}
    /// <summary>
    ///   Defines a matching case that is activated when the case's value is less than the specified value.
    ///   The code block associated with this case will be executed if the "less than" condition is verified.
    /// </summary>
    /// <param name="AValue">
    ///   The value to be compared with the case's value to check the "less than" condition.
    /// </param>
    /// <param name="AProc">
    ///   A procedure that will be executed if the case's value is less than the specified value.
    /// </param>
    /// <returns>
    ///   An instance of the current matching pattern, allowing the definition of more actions
    ///   to be executed if the "less than" condition is verified.
    /// </returns>
    {$ENDREGION}
    function CaseLt(const AValue: T; const AProc: TProc): TMatch<T>; overload;

    {$REGION 'Doc - CaseLt'}
    /// <summary>
    ///   Defines a matching case that is activated when the case's value is less than the specified value.
    ///   The code block associated with this case will be executed if the "less than" condition is verified.
    /// </summary>
    /// <param name="AValue">
    ///   The value to be compared with the case's value to check the "less than" condition.
    /// </param>
    /// <param name="AProc">
    ///   A procedure that will be executed if the case's value is less than the specified value.
    ///   The case's value will be passed as a parameter to the procedure.
    /// </param>
    /// <returns>
    ///   An instance of the current matching pattern, allowing the definition of more actions
    ///   to be executed if the "less than" condition is verified.
    /// </returns>
    {$ENDREGION}
    function CaseLt(const AValue: T; const AProc: TProc<T>): TMatch<T>; overload;

    {$REGION 'Doc - CaseIn'}
    /// <summary>
    ///   Defines a matching case that is activated when the case's value is contained within the specified range of values.
    ///   The code block associated with this case will be executed when the case's value is present in the range.
    /// </summary>
    /// <param name="ARange">
    ///   The range of values to be compared with the case's value to check for inclusion.
    /// </param>
    /// <param name="AProc">
    ///   A procedure that will be executed if the case's value is contained within the specified range.
    ///   The procedure will be called without any parameters.
    /// </param>
    /// <returns>
    ///   An instance of the current matching pattern, allowing the definition of more
    ///   actions to be executed if inclusion in the range is verified.
    /// </returns>
    {$ENDREGION}
    function CaseIn(const ARange: TArray<T>; const AProc: TProc): TMatch<T>; overload;

    {$REGION 'Doc - CaseIn'}
    /// <summary>
    ///   Defines a matching case that is activated when the case's value is contained within the specified range of values.
    ///   The code block associated with this case will be executed when the case's value is present in the range.
    /// </summary>
    /// <param name="ARange">
    ///   The range of values to be compared with the case's value to check for inclusion.
    /// </param>
    /// <param name="AProc">
    ///   A procedure that will be executed if the case's value is contained within the specified range.
    ///   The procedure will receive the case's value as a parameter.
    /// </param>
    /// <returns>
    ///   An instance of the current matching pattern, allowing the definition of more
    ///   actions to be executed if inclusion in the range is verified.
    /// </returns>
    {$ENDREGION}
    function CaseIn(const ARange: TArray<T>; const AProc: TProc<T>): TMatch<T>; overload;

    {$REGION 'Doc - CaseIs'}
    /// <summary>
    ///   Defines a matching case that is activated when the case's value is of the specified type.
    ///   The code block associated with this case will be executed if the case's value is of the specified type.
    /// </summary>
    /// <typeparam name="Typ">
    ///   The expected type for the case's value.
    /// </typeparam>
    /// <param name="AProc">
    ///   A procedure that will be executed if the case's value is of the specified type.
    ///   The procedure does not receive any parameters.
    /// </param>
    /// <returns>
    ///   An instance of the current matching pattern, allowing the definition of more
    ///   actions to be executed if the corresponding type is verified.
    /// </returns>
    {$ENDREGION}
    function CaseIs<Typ>(const AProc: TProc): TMatch<T>; overload;

    {$REGION 'Doc - CaseIs'}
    /// <summary>
    ///   Defines a matching case that is activated when the case's value is of the specified type.
    ///   The code block associated with this case will be executed if the case's value is of the specified type.
    /// </summary>
    /// <typeparam name="Typ">
    ///   The expected type for the case's value.
    /// </typeparam>
    /// <param name="AProc">
    ///   A procedure that will be executed if the case's value is of the specified type.
    ///   The procedure receives a parameter of the expected type.
    /// </param>
    /// <returns>
    ///   An instance of the current matching pattern, allowing the definition of more
    ///   actions to be executed if the corresponding type is verified.
    /// </returns>
    {$ENDREGION}
    function CaseIs<Typ>(const AProc: TProc<Typ>): TMatch<T>; overload;

    {$REGION 'Doc - CaseRange'}
    /// <summary>
    ///   Defines a matching case that is activated when the case's value is within the specified range.
    ///   The code block associated with this case will be executed if the case's value is within the specified range.
    /// </summary>
    /// <param name="AStart">
    ///   The lower limit of the range.
    /// </param>
    /// <param name="AEnd">
    ///   The upper limit of the range.
    /// </param>
    /// <param name="AProc">
    ///   A procedure that will be executed if the case's value is within the specified range.
    /// </param>
    /// <returns>
    ///   An instance of the current matching pattern, allowing the definition of more
    ///   actions to be executed within the specified range.
    /// </returns>
    {$ENDREGION}
    function CaseRange(const AStart, AEnd: T; const AProc: TProc): TMatch<T>; overload;

    {$REGION 'Doc - CaseRange'}
    /// <summary>
    ///   Defines a matching case that is activated when the case's value is within the specified range.
    ///   The code block associated with this case will be executed if the case's value is within the specified range.
    /// </summary>
    /// <param name="AStart">
    ///   The lower limit of the range.
    /// </param>
    /// <param name="AEnd">
    ///   The upper limit of the range.
    /// </param>
    /// <param name="AProc">
    ///   A procedure that will be executed with the case's value as a parameter if the case's value is within the specified range.
    /// </param>
    /// <returns>
    ///   An instance of the current matching pattern, allowing the definition of more
    ///   actions to be executed within the specified range.
    /// </returns>
    {$ENDREGION}
    function CaseRange(const AStart, AEnd: T; const AProc: TProc<T>): TMatch<T>; overload;

    {$REGION 'Doc - CaseRegex'}
    /// <summary>
    ///   Defines a matching case that is activated when the case's value matches the specified regular expression pattern.
    ///   The code block associated with this case will be executed if the case's value matches the pattern.
    /// </summary>
    /// <param name="AInput">
    ///   The case's value to be checked against the regular expression pattern.
    /// </param>
    /// <param name="APattern">
    ///   The regular expression pattern used to check if the case's value matches.
    /// </param>
    /// <returns>
    ///   An instance of the current matching pattern, allowing the definition of more actions
    ///   to be executed if the case's value matches the regular expression pattern.
    /// </returns>
    {$ENDREGION}
    function CaseRegex(const AInput: string; const APattern: string): TMatch<T>;

    {$REGION 'Doc - Default'}
    /// <summary>
    ///   Defines the default case that will be activated if none of the previous cases match the pattern's value.
    ///   The code block associated with this case will be executed if none of the other cases match.
    /// </summary>
    /// <param name="AProc">
    ///   The procedure to be executed if none of the previous cases match the pattern's value.
    /// </param>
    /// <returns>
    ///   An instance of the current matching pattern, allowing the definition of more actions
    ///   to be executed in the default case.
    /// </returns>
    {$ENDREGION}
    function Default(const AProc: TProc): TMatch<T>; overload;

    {$REGION 'Doc - Default'}
    /// <summary>
    ///   Defines the default case that will be activated if none of the previous cases match the pattern's value.
    ///   The code block associated with this case will be executed if none of the other cases match.
    ///   This overload allows providing a value to be used along with the procedure.
    /// </summary>
    /// <param name="AValue">
    ///   The value to be associated with the default case.
    /// </param>
    /// <param name="AProc">
    ///   The procedure to be executed if none of the previous cases match the pattern's value.
    ///   The associated value will be passed as a parameter to the procedure.
    /// </param>
    /// <returns>
    ///   An instance of the current matching pattern, allowing the definition of more actions
    ///   to be executed in the default case.
    /// </returns>
    {$ENDREGION}
    function Default(const AValue: T; const AProc: TProc<T>): TMatch<T>; overload;

    {$REGION 'Doc - Combine'}
    /// <summary>
    ///   Combines multiple patterns into a single composite pattern. This allows grouping various matching logics
    ///   into a single pattern, making code reuse and organization easier.
    /// </summary>
    /// <param name="APatterns">
    ///   An array of patterns to be combined into a composite pattern.
    /// </param>
    /// <returns>
    ///   An instance of the resulting matching pattern obtained by combining the provided patterns.
    /// </returns>
    {$ENDREGION}
    function Combine(var AMatch: TMatch<T>): TMatch<T>;

    {$REGION 'Doc - TryExcept'}
    /// <summary>
    ///   Defines an exception handling section for the current pattern. The code block provided in <paramref name="AProc"/>
    ///   will be executed within a try-except block, and any exception that occurs during execution will be caught and handled.
    /// </summary>
    /// <param name="AProc">
    ///   A procedure (TProc) containing the code to be executed within the try-except block.
    /// </param>
    /// <returns>
    ///   The current pattern itself, allowing the continuation of pattern construction after the try-except clause.
    /// </returns>
    {$ENDREGION}
    function TryExcept(AProc: TProc): TMatch<T>;

    {$REGION 'Doc - Execute'}
    /// <summary>
    ///   Executes the pattern built so far and evaluates whether the current value matches any of the defined patterns.
    ///   If a matching pattern is found, the pattern execution is halted, and a result pair is returned,
    ///   consisting of a string representing the matching pattern and a boolean indicating whether the match was successful.
    ///   If no matching pattern is found, a default value is returned, indicating a match with the "default" pattern if defined.
    /// </summary>
    /// <returns>
    ///   A result pair containing a string representing the found matching pattern and a boolean
    ///   indicating whether the match was successful.
    /// </returns>
    {$ENDREGION}
    function Execute: TResultPair<boolean, string>; overload;
  end;

implementation

class function TMatch<T>.Value(const AValue: T): TMatch<T>;
begin
  Result := TMatch<T>.Create(AValue);
end;

constructor TMatch<T>.Create(const AValue: T);
begin
  FCaseDictionary := TDictionary<string, TCaseGroup>.Create;
  FCaseDictionary.Add('CaseIf_Proc', TCaseGroup.Create);
  FCaseDictionary.Add('CaseIf_Func', TCaseGroup.Create);
  FCaseDictionary.Add('CaseEq', TCaseGroup.Create);
  FCaseDictionary.Add('CaseGt', TCaseGroup.Create);
  FCaseDictionary.Add('CaseLt', TCaseGroup.Create);
  FCaseDictionary.Add('CaseIn', TCaseGroup.Create);
  FCaseDictionary.Add('CaseIs', TCaseGroup.Create);
  FCaseDictionary.Add('CaseRange', TCaseGroup.Create);
  FCaseDictionary.Add('Default', TCaseGroup.Create);
  FCaseDictionary.Add('TryExcept', TCaseGroup.Create);
  FValue := TValue.From<T>(AValue);
  // Private method for resetting variables.
  _StartVariables;
end;

function TMatch<T>.TryExcept(AProc: TProc): TMatch<T>;
begin
  Result := Self;
  if not (FSession in [sCase, sDefault]) then
    exit;
  FCaseDictionary['TryExcept'].AddOrSetValue(TValue.From<boolean>(true),
                                             TValue.From<TProc>(AProc));
  Result.FSession := TMatchSession.sTryExcept;
end;

function TMatch<T>.CaseRange(const AStart, AEnd: T;
  const AProc: TProc<T>): TMatch<T>;
var
  LRange: TPair<T, T>;
begin
  Result := Self;
  if not (FSession in [sMatch, sGuard, sCase]) then
    exit;
  LRange := TPair<T, T>.Create(AStart, AEnd);
  FCaseDictionary['CaseRange'].AddOrSetValue(TValue.From<TPair<T, T>>(LRange),
                                             TValue.From<TProc<T>>(AProc));
  Result.FSession := TMatchSession.sCase;
end;

function TMatch<T>.CaseRegex(const AInput: String; const APattern: String): TMatch<T>;
begin
  Result := Self;
  if not (FSession in [sMatch, sGuard, sCase]) then
    exit;
  if TRegExLib.IsMatch(AInput, APattern) then
    Result.FRegexCount := Result.FRegexCount + 1
  else
    Result.FRegexCount := Result.FRegexCount - 1;
  Result.FSession := TMatchSession.sCase;
  Result.FUseRegex := true;
end;

function TMatch<T>.CaseIs<Typ>(const AProc: TProc<Typ>): TMatch<T>;
begin
  Result := Self;
  if not (FSession in [sMatch, sGuard, sCase]) then
    exit;
  if TypeInfo(Typ) = TypeInfo(TDateTime) then
    FCaseDictionary['CaseIs'].AddOrSetValue(TValue.From<TTypeKind>(tkFloat),
                                            TValue.From<TProc<Typ>>(AProc))
  else
    FCaseDictionary['CaseIs'].AddOrSetValue(TValue.From<TTypeKind>(PTypeInfo(TypeInfo(Typ)).Kind),
                                                                   TValue.From<TProc<Typ>>(AProc));
  Result.FSession := TMatchSession.sCase;
end;

function TMatch<T>.CaseLt(const AValue: T; const AProc: TProc): TMatch<T>;
begin
  Result := Self;
  if not (FSession in [sMatch, sGuard, sCase]) then
    exit;
  FCaseDictionary['CaseLt'].AddOrSetValue(TValue.From<T>(AValue),
                                          TValue.From<TProc>(AProc));
  Result.FSession := TMatchSession.sCase;
end;

function TMatch<T>.CaseLt(const AValue: T; const AProc: TProc<T>): TMatch<T>;
begin
  Result := Self;
  if not (FSession in [sMatch, sGuard, sCase]) then
    exit;
  FCaseDictionary['CaseLt'].AddOrSetValue(TValue.From<T>(AValue),
                                          TValue.From<TProc<T>>(AProc));
  Result.FSession := TMatchSession.sCase;
end;

function TMatch<T>.CaseRange(const AStart, AEnd: T; const AProc: TProc): TMatch<T>;
var
  LRange: TPair<T, T>;
begin
  Result := Self;
  if not (FSession in [sMatch, sGuard, sCase]) then
    exit;
  LRange := TPair<T, T>.Create(AStart, AEnd);
  FCaseDictionary['CaseRange'].AddOrSetValue(TValue.From<TPair<T , T>>(LRange),
                                             TValue.From<TProc>(AProc));
  Result.FSession := TMatchSession.sCase;
end;

function TMatch<T>.CaseEq(const AValue: T; const AProc: TProc): TMatch<T>;
begin
  Result := Self;
  if not (FSession in [sMatch, sGuard, sCase]) then
    exit;
  FCaseDictionary['CaseEq'].AddOrSetValue(TValue.From<T>(AValue),
                                          TValue.From<TProc>(AProc));
  Result.FSession := TMatchSession.sCase;
end;

function TMatch<T>.CaseEq(const AValue: T; const AProc: TProc<T>): TMatch<T>;
begin
  Result := Self;
  if not (FSession in [sMatch, sGuard, sCase]) then
    exit;
  FCaseDictionary['CaseEq'].AddOrSetValue(TValue.From<T>(AValue),
                                          TValue.From<TProc<T>>(AProc));
  Result.FSession := TMatchSession.sCase;
end;

function TMatch<T>.CaseGt(const AValue: T; const AProc: TProc): TMatch<T>;
begin
  Result := Self;
  if not (FSession in [sMatch, sGuard, sCase]) then
    exit;
  FCaseDictionary['CaseGt'].AddOrSetValue(TValue.From<T>(AValue),
                                          TValue.From<TProc>(AProc));
  Result.FSession := TMatchSession.sCase;
end;

function TMatch<T>.CaseGt(const AValue: T; const AProc: TProc<T>): TMatch<T>;
begin
  Result := Self;
  if not (FSession in [sMatch, sGuard, sCase]) then
    exit;
  FCaseDictionary['CaseGt'].AddOrSetValue(TValue.From<T>(AValue),
                                          TValue.From<TProc<T>>(AProc));
  Result.FSession := TMatchSession.sCase;
end;

function TMatch<T>.CaseIf(const ACondition: boolean; const AProc: TProc<T>): TMatch<T>;
begin
  Result := Self;
  if not (FSession in [sMatch, sGuard]) then
    exit;
  if ACondition then
  begin
    FCaseDictionary['CaseIf_Proc'].AddOrSetValue(TValue.From<boolean>(ACondition),
                                                 TValue.From<TProc<T>>(AProc));
    Result.FSession := TMatchSession.sGuard;
  end;
end;

function TMatch<T>.CaseIf(const ACondition: boolean; const AProc: TProc): TMatch<T>;
begin
  Result := Self;
  if not (FSession in [sMatch, sGuard]) then
    exit;
  if ACondition then
  begin
    FCaseDictionary['CaseIf_Proc'].AddOrSetValue(TValue.From<boolean>(ACondition),
                                                 TValue.From<TProc>(AProc));
    Result.FSession := TMatchSession.sGuard;
  end;
end;

function TMatch<T>.CaseIf(const ACondition: boolean; const AFunc: TFunc<T, boolean>): TMatch<T>;
begin
  Result := Self;
  if not (FSession in [sMatch, sGuard]) then
    exit;
  if ACondition then
  begin
    FCaseDictionary['CaseIf_Func'].AddOrSetValue(TValue.From<boolean>(ACondition),
                                                 TValue.From<TFunc<T, boolean>>(AFunc));
    Result.FSession := TMatchSession.sGuard;
  end;
end;

function TMatch<T>.CaseIf(const ACondition: boolean;
  const AFunc: TFunc<boolean>): TMatch<T>;
begin
  Result := Self;
  if not (FSession in [sMatch, sGuard]) then
    exit;
  if ACondition then
  begin
    FCaseDictionary['CaseIf_Func'].AddOrSetValue(TValue.From<boolean>(ACondition),
                                                 TValue.From<TFunc<boolean>>(AFunc));
    Result.FSession := TMatchSession.sGuard;
  end;
end;

function TMatch<T>.CaseIn(const ARange: TArray<T>; const AProc: TProc<T>): TMatch<T>;
var
  LFor: integer;
begin
  Result := Self;
  if not (FSession in [sMatch, sGuard, sCase]) then
    exit;
  for LFor := Low(ARange) to High(ARange) do
    FCaseDictionary['CaseIn'].AddOrSetValue(TValue.From<T>(ARange[LFor]),
                                            TValue.From<TProc<T>>(AProc));
  Result.FSession := TMatchSession.sCase;
end;

function TMatch<T>.CaseIs<Typ>(const AProc: TProc): TMatch<T>;
begin
  Result := Self;
  if not (FSession in [sMatch, sGuard, sCase]) then
    exit;
  if TypeInfo(Typ) = TypeInfo(TDateTime) then
    FCaseDictionary['CaseIs'].AddOrSetValue(TValue.From<TTypeKind>(tkFloat),
                                            TValue.From<TProc>(AProc))
  else
    FCaseDictionary['CaseIs'].AddOrSetValue(TValue.From<TTypeKind>(PTypeInfo(TypeInfo(Typ)).Kind),
                                            TValue.From<TProc>(AProc));
  Result.FSession := TMatchSession.sCase;
end;

function TMatch<T>.Default(const AValue: T; const AProc: TProc<T>): TMatch<T>;
begin
  Result := Self;
  if not (FSession in [sCase]) then
    exit;
  FCaseDictionary['Default'].AddOrSetValue(TValue.From<T>(AValue),
                                           TValue.From<TProc<T>>(AProc));
  Result.FSession := TMatchSession.sDefault;
end;

function TMatch<T>.Default(const AProc: TProc): TMatch<T>;
begin
  Result := Self;
  if not (FSession in [sCase]) then
    exit;
  FCaseDictionary['Default'].AddOrSetValue(TValue.From<boolean>(true),
                                           TValue.From<TProc>(AProc));
  Result.FSession := TMatchSession.sDefault;
end;

function TMatch<T>.CaseIn(const ARange: TArray<T>; const AProc: TProc): TMatch<T>;
var
  LFor: integer;
begin
  Result := Self;
  if not (FSession in [sMatch, sGuard, sCase]) then
    exit;
  for LFor := Low(ARange) to High(ARange) do
    FCaseDictionary['CaseIn'].AddOrSetValue(TValue.From<T>(ARange[LFor]),
                                            TValue.From<TProc>(AProc));
  Result.FSession := TMatchSession.sCase;
end;

function TMatch<T>.Execute: TResultPair<boolean, string>;
begin
  try
    if not _ExecuteSession then
    begin
      Result.Failure('Use Execute after session [sCase, sDefault, sTryExcept]');
      exit;
    end
    else
    if (not _ExecuteGuard) or (not _ExecuteRegex) then
    begin
      Result.Failure('No matching Guard/Regex.');
      exit;
    end;

    try
      Result := _ExecuteCasesValidation;
    except
      on E: Exception do
      begin
        Result.Failure(E.Message);
        try
          if (FCaseDictionary['TryExcept'].Count > 0) and (_MatchingTryExcept) then
            exit;
        except
          on E: Exception do
            Result.Failure(Result.ValueFailure + sLineBreak + E.Message);
        end;
      end;
    end;
  finally
    _Dispose;
  end;
end;

function TMatch<T>._ExecuteCasesValidation: TResultPair<boolean, string>;
begin
  if (not _ExecuteCaseIf_Proc) or (not _ExecuteCaseIf_Func) then
  begin
    Result.Failure('No matching Guard.');
    exit;
  end
  else
  if (_ExecuteCaseEq) or (_ExecuteCaseGt) or (_ExecuteCaseLt) or
     (_ExecuteCaseIn) or (_ExecuteCaseIs) or (_ExecuteCaseRange) or
     (_ExecuteCaseDefault) then
  begin
    Result.Success(true);
    exit;
  end;
  Result.Failure('No matching case found.');
end;

function TMatch<T>._ExecuteSession: boolean;
begin
  Result := true;
  if not (FSession in [sCase, sDefault, sTryExcept]) then
    Result := false;
end;

function TMatch<T>._ExecuteGuard: boolean;
begin
  Result := true;
  if not FUseGuard then
    FGuardCount := 1;
  if FGuardCount <= 0 then
    Result := false
end;

function TMatch<T>._ExecuteRegex: boolean;
begin
  Result := true;
  if not FUseRegex then
    FRegexCount := 1;
  if FRegexCount <= 0 then
    Result := false;
end;

function TMatch<T>._ExecuteCaseIf_Proc: boolean;
begin
  Result := true;
  if (FCaseDictionary['CaseIf_Proc'].Count > 0) and (not _MatchingCaseIfProc) then
    Result := false;
end;

function TMatch<T>._ExecuteCaseEq: boolean;
begin
  Result := false;
  if (FCaseDictionary['CaseEq'].Count > 0) and  (_MatchingCaseEq) then
    Result := true;
end;

function TMatch<T>._ExecuteCaseGt: boolean;
begin
  Result := false;
  if (FCaseDictionary['CaseGt'].Count > 0) and  (_MatchingCaseGt) then
    Result := true;
end;

function TMatch<T>._ExecuteCaseIn: boolean;
begin
  Result := false;
  if (FCaseDictionary['CaseIn'].Count > 0) and (_MatchingCaseIn) then
    Result := true;
end;

function TMatch<T>._ExecuteCaseLt: boolean;
begin
  Result := false;
  if (FCaseDictionary['CaseLt'].Count > 0) and  (_MatchingCaseLt) then
    Result := true;
end;

function TMatch<T>._ExecuteCaseRange: boolean;
begin
  Result := false;
  if (FCaseDictionary['CaseRange'].Count > 0) and (_MatchingCaseRange) then
    Result := true;
end;

function TMatch<T>._ExecuteCaseDefault: boolean;
begin
  Result := false;
  if (FCaseDictionary['Default'].Count > 0) and (_MatchingDefault) then
    Result := true;
end;

function TMatch<T>._ExecuteCaseIs: boolean;
begin
  Result := false;
  if (FCaseDictionary['CaseIs'].Count > 0) and (_MatchingCaseIs) then
    Result := true;
end;

function TMatch<T>._ExecuteCaseIf_Func: boolean;
begin
  Result := true;
  if (FCaseDictionary['CaseIf_Func'].Count > 0) and (not _MatchingCaseIfFunc) then
    Result := false;
end;

function TMatch<T>._ArraysAreNotEqualCaseIf(const APair: TPair<TValue, TValue>): boolean;
begin
  Result := false;
  if not _ArraysAreEqual(FValue, APair.Key) then
    Result := true;
end;

function TMatch<T>._ArraysAreEqual(const Arr1, Arr2: TValue): boolean;
begin
  Result := false;
  if _IsArrayInteger(Arr1) then
    Result := _ArraysAreEqualInteger(Arr1, Arr2)
  else
  if _IsArrayString(Arr1) then
    Result := _ArraysAreEqualString(Arr1, Arr2)
  else
  if _IsArrayChar(Arr1) then
    Result := _ArraysAreEqualChar(Arr1, Arr2)
end;

function TMatch<T>._ArraysAreEqualChar(const Arr1, Arr2: TValue): boolean;
var
  LArray1: TArray<Char>;
  LArray2: TArray<Char>;
  LFor: integer;
begin
  Result := false;
  LArray1 := Arr1.AsType<TArray<Char>>;
  LArray2 := Arr2.AsType<TArray<Char>>;
  if Length(LArray1) <> Length(LArray2) then
    exit;
  for LFor := Low(LArray1) to High(LArray1) do
  begin
    if LArray1[LFor] <> LArray2[LFor] then
      exit;
  end;
  Result := true;
end;

function TMatch<T>._ArraysAreEqualInteger(const Arr1, Arr2: TValue): boolean;
var
  LArray1: TArray<integer>;
  LArray2: TArray<integer>;
  LFor: integer;
begin
  Result := false;
  LArray1 := Arr1.AsType<TArray<integer>>;
  LArray2 := Arr2.AsType<TArray<integer>>;
  if Length(LArray1) <> Length(LArray2) then
    exit;
  for LFor := Low(LArray1) to High(LArray1) do
  begin
    if LArray1[LFor] <> LArray2[LFor] then
      exit;
  end;
  Result := true;
end;

function TMatch<T>._ArraysAreEqualString(const Arr1, Arr2: TValue): boolean;
var
  LArray1: TArray<string>;
  LArray2: TArray<string>;
  LFor: integer;
begin
  Result := false;
  LArray1 := Arr1.AsType<TArray<string>>;
  LArray2 := Arr2.AsType<TArray<string>>;
  if Length(LArray1) <> Length(LArray2) then
    exit;
  for LFor := Low(LArray1) to High(LArray1) do
  begin
    if LArray1[LFor] <> LArray2[LFor] then
      exit;
  end;
  Result := true;
end;

procedure TMatch<T>._Dispose;
begin
  if Assigned(FCaseDictionary) then
  begin
    FCaseDictionary['CaseIf_Proc'].Free;
    FCaseDictionary['CaseIf_Func'].Free;
    FCaseDictionary['CaseEq'].Free;
    FCaseDictionary['CaseGt'].Free;
    FCaseDictionary['CaseLt'].Free;
    FCaseDictionary['CaseIn'].Free;
    FCaseDictionary['CaseIs'].Free;
    FCaseDictionary['CaseRange'].Free;
    FCaseDictionary['Default'].Free;
    FCaseDictionary['TryExcept'].Free;
    FCaseDictionary.Free;
  end;
  // Private method for resetting variables.
  _StartVariables;
end;

function TMatch<T>._IsArrayChar(const AValue: TValue): boolean;
begin
  Result := LowerCase(string(AValue.TypeInfo.Name)) = LowerCase('TArray<System.Char>');
end;

function TMatch<T>._IsArrayInteger(const AValue: TValue): boolean;
begin
  Result := LowerCase(string(AValue.TypeInfo.Name)) = LowerCase('TArray<System.Integer>');
end;

function TMatch<T>._IsArrayString(const AValue: TValue): boolean;
begin
  Result := LowerCase(string(AValue.TypeInfo.Name)) = LowerCase('TArray<System.String>');
end;

function TMatch<T>._MatchingCaseEq: boolean;
var
  LProcPair: TPair<TValue, TValue>;
begin
  Result := false;
  for LProcPair in FCaseDictionary['CaseEq'] do
  begin
    if LProcPair.Key.IsArray then
    begin
      if not _ArraysAreEqual(FValue, LProcPair.Key) then
        continue;
    end
    else
    begin
      if not TEqualityComparer<T>.Default.Equals(FValue.AsType<T>, LProcPair.Key.AsType<T>) then
        continue;
    end;
    _ExecuteProcMatchingEq(LProcPair.Value);
    Result := true;
  end;
end;

procedure TMatch<T>._ExecuteProcMatchingEq(const ProcValue: TValue);
begin
  if ProcValue.IsType<TProc> then
    ProcValue.AsType<TProc>()()
  else
  if ProcValue.IsType<TProc<T>> then
    ProcValue.AsType<TProc<T>>()(FValue.AsType<T>);
end;

function TMatch<T>._MatchingCaseGt: boolean;
var
  LProcPair: TPair<TValue, TValue>;
begin
  Result := false;
  for LProcPair in FCaseDictionary['CaseGt'] do
  begin
    if TComparer<T>.Default.Compare(FValue.AsType<T>, LProcPair.Key.AsType<T>) <= 0 then
      continue;

    _ExecuteProcMatchingGt(LProcPair.Value);
    Result := true;
  end;
end;

procedure TMatch<T>._ExecuteProcMatchingGt(const ProcValue: TValue);
begin
  if ProcValue.IsType<TProc> then
    ProcValue.AsType<TProc>()()
  else
  if ProcValue.IsType<TProc<T>> then
    ProcValue.AsType<TProc<T>>()(FValue.AsType<T>);
end;

function TMatch<T>._MatchingDefault: boolean;
var
  LProcPair: TPair<TValue, TValue>;
begin
  Result := false;
  for LProcPair in FCaseDictionary['Default'] do
  begin
    if LProcPair.Value.IsType<TProc> then
      LProcPair.Value.AsType<TProc>()()
    else
    if LProcPair.Value.IsType<TProc<T>> then
      LProcPair.Value.AsType<TProc<T>>()(LProcPair.Key.AsType<T>);
    Result := true;
  end;
end;

function TMatch<T>._MatchingTryExcept: boolean;
var
  LProcPair: TPair<TValue, TValue>;
begin
  Result := false;
  for LProcPair in FCaseDictionary['TryExcept'] do
  begin
    if LProcPair.Value.AsType<TProc> <> nil then
    begin
      LProcPair.Value.AsType<TProc>()();
      Result := true;
    end;
  end;
end;

function TMatch<T>._MatchingCaseRange: boolean;
var
  LProcPair: TPair<TValue, TValue>;
  LComparer: IComparer<T>;
begin
  Result := false;
  LComparer := TComparer<T>.Default;
  for LProcPair in FCaseDictionary['CaseRange'] do
  begin
    if (LComparer.Compare(FValue.AsType<T>, LProcPair.Key.AsType<TPair<T, T>>.Key) < 0) or
       (LComparer.Compare(FValue.AsType<T>, LProcPair.Key.AsType<TPair<T, T>>.Value) > 0) then
      continue;

    _ExecuteProcMatchingRange(LProcPair.Value);
    Result := true;
  end;
end;

procedure TMatch<T>._ExecuteProcMatchingRange(const ProcValue: TValue);
begin
  if ProcValue.IsType<TProc> then
    ProcValue.AsType<TProc>()()
  else
  if ProcValue.IsType<TProc<T>> then
    ProcValue.AsType<TProc<T>>()(FValue.AsType<T>);
end;

function TMatch<T>._MatchingCaseIn: boolean;
var
  LProcPair: TPair<TValue, TValue>;
begin
  Result := false;
  for LProcPair in FCaseDictionary['CaseIn'] do
  begin
    if LProcPair.Key.IsArray then
    begin
      if not _ArraysAreEqual(FValue, LProcPair.Key) then
        continue;
    end
    else
    if FValue.AsType<T> <> LProcPair.Key.AsType<T> then
      continue;

    _ExecuteProcMatchingIn(LProcPair.Value);
    Result := true;
  end;
end;

procedure TMatch<T>._ExecuteProcMatchingIn(const ProcValue: TValue);
begin
  if ProcValue.IsType<TProc> then
    ProcValue.AsType<TProc>()()
  else if ProcValue.IsType<TProc<T>> then
    ProcValue.AsType<TProc<T>>()(FValue.AsType<T>);
end;

function TMatch<T>._MatchingCaseIs: boolean;
var
  LProcPair: TPair<TValue, TValue>;
  LKey: TTypeKind;
  LProc: TValue;
begin
  Result := false;
  LKey := FValue.TypeInfo.Kind;
  // TDateTime Check
  if FValue.TypeInfo = TypeInfo(TDateTime) then
    LKey := tkFloat;

  if FCaseDictionary['CaseIs'].TryGetValue(TValue.From<TTypeKind>(LKey), LProc) then
  begin
    _ExecuteProcMatchingIs(LProc);
    Result := true;
  end;
end;

procedure TMatch<T>._ExecuteProcMatchingIs(const ProcValue: TValue);
begin
  if ProcValue.IsType<TProc> then
    ProcValue.AsType<TProc>()()
  else
  if ProcValue.IsType<TProc<T>> then
    ProcValue.AsType<TProc<T>>()(FValue.AsType<T>);
end;

function TMatch<T>._MatchingCaseLt: boolean;
var
  LProcPair: TPair<TValue, TValue>;
begin
  Result := false;
  for LProcPair in FCaseDictionary['CaseLt'] do
  begin
    if TComparer<T>.Default.Compare(FValue.AsType<T>, LProcPair.Key.AsType<T>) >= 0 then
      continue;

    _ExecuteProcMatchingLt(LProcPair.Value);
    Result := true;
  end;
end;

procedure TMatch<T>._ExecuteProcMatchingLt(const ProcValue: TValue);
begin
  if ProcValue.IsType<TProc> then
    ProcValue.AsType<TProc>()()
  else
  if ProcValue.IsType<TProc<T>> then
    ProcValue.AsType<TProc<T>>()(FValue.AsType<T>);
end;

function TMatch<T>._MatchingCaseIfFunc: boolean;
var
  LProcPair: TPair<TValue, TValue>;
begin
  Result := false;
  for LProcPair in FCaseDictionary['CaseIf_Func'] do
  begin
    if _ArraysAreNotEqualCaseIf(LProcPair) then
      continue;
    if FValue.AsType<T> <> LProcPair.Key.AsType<T> then
      continue;

    Result := _ExecuteFuncMatchingCaseIf(LProcPair.Value);
  end;
end;

function TMatch<T>._ExecuteFuncMatchingCaseIf(const ProcValue: TValue): boolean;
begin
  Result := false;
  if ProcValue.IsType<TFunc<boolean>> then
    Result := ProcValue.AsType<TFunc<boolean>>()()
  else
  if ProcValue.IsType<TFunc<T, boolean>> then
    Result := ProcValue.AsType<TFunc<T, boolean>>()(FValue.AsType<T>);
end;

function TMatch<T>._MatchingCaseIfProc: boolean;
var
  LProcPair: TPair<TValue, TValue>;
begin
  Result := false;
  for LProcPair in FCaseDictionary['CaseIf_Proc'] do
  begin
    if _ArraysAreNotEqualCaseIf(LProcPair) then
      continue;
    if FValue.AsType<T> <> LProcPair.Key.AsType<T> then
      continue;

    _ExecuteProcMatchingCaseIf(LProcPair.Value);
    Result := true;
  end;
end;

procedure TMatch<T>._ExecuteProcMatchingCaseIf(const ProcValue: TValue);
begin
  if ProcValue.IsType<TProc> then
    ProcValue.AsType<TProc>()()
  else
  if ProcValue.IsType<TProc<T>> then
    ProcValue.AsType<TProc<T>>()(FValue.AsType<T>);
end;

procedure TMatch<T>._StartVariables;
begin
  FGuardCount := 0;
  FRegexCount := 0;
  FUseGuard := false;
  FUseRegex := false;
  FSession := TMatchSession.sMatch;
end;

function TMatch<T>.CaseIf(const ACondition: boolean): TMatch<T>;
begin
  if not (FSession in [sMatch, sGuard]) then
    raise Exception.Create('Use Guard after session [sMatch, sGuard]');

  Result := Self;
  if ACondition then
    Result.FGuardCount := Result.FGuardCount + 1
  else
    Result.FGuardCount := Result.FGuardCount - 1;
  Result.FSession := TMatchSession.sGuard;
  Result.FUseGuard := true;
end;

function TMatch<T>.Combine(var AMatch: TMatch<T>): TMatch<T>;
var
  LGroup: TPair<string, TCaseGroup>;
  LPair: TPair<TValue, TValue>;
begin
  Result := Self;
  if not (FSession in [sMatch, sGuard, sCase]) then
    exit;
  for LGroup in AMatch.FCaseDictionary do
  begin
    for LPair in LGroup.Value do
      FCaseDictionary[LGroup.Key].AddOrSetValue(LPair.Key, LPair.Value);
  end;
  // After transferring, clear all received TMatch instances
  AMatch._Dispose;
  // Set the type after clearing everything
  Result.FSession := TMatchSession.sCase;
end;

end.
