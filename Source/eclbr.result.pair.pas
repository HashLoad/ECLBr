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

unit eclbr.result.pair;

interface

uses
  Rtti,
  TypInfo,
  Classes,
  SysUtils;

type
  TResultType = (rtNone, rtSuccess, rtFailure);

  EFailureException<F> = class(Exception)
  public
    constructor Create(const Value: F);
  end;

  ESuccessException<S> = class(Exception)
  public
    constructor Create(const Value: S);
  end;

  ETypeIncompatibility = class(Exception)
  public
    constructor Create(const AMessage: string = '');
  end;

  TResultPairValue<T> = record
  private
    FValue: T;
    FHasValue: boolean;
  public
    constructor Create(AValue: T);
    class function CreateNil: TResultPairValue<T>; static;
    function GetValue: T;
    function HasValue: boolean;
  end;

  TResultPair<S, F> = record
  private
    type
      TMapFunc<Return> = reference to function(const ASelf: TResultPair<S, F>): Return;
      TFuncOk = reference to function(const Success: S): TResultPair<S, F>;
      TFuncFail = reference to function(const Success: F): TResultPair<S, F>;
      TFuncExec = reference to function: TResultPair<S, F>;
  private
    FSuccess: ^TResultPairValue<S>;
    FFailure: ^TResultPairValue<F>;
    FSuccessFuncs: ^TArray<TFuncOk>;
    FFailureFuncs: ^TArray<TFuncFail>;
    FResultType: TResultType;
  private
    ///  <summary>
    ///    Libera os recursos associados ao valor de sucesso da instância.
    ///  </summary>
    procedure _DestroySuccess;

    ///  <summary>
    ///    Libera os recursos associados ao valor de falha da instância.
    ///  </summary>
    procedure _DestroyFailure;

    /// <summary>
    ///   Sets the success value for the TResultPair.
    /// </summary>
    /// <remarks>
    ///   Use this procedure to set the success value for the current TResultPair instance.
    ///   The success value represents the successful result of an operation in the Railway Pattern.
    /// </remarks>
    /// <param name="ASuccess">
    ///   The success value of type S to set.
    /// </param>
    procedure _SetSuccessValue(const ASuccess: S);

    /// <summary>
    ///   Sets the failure value for the TResultPair.
    /// </summary>
    /// <remarks>
    ///   Use this procedure to set the failure value for the current TResultPair instance.
    ///   The failure value represents an error or failure in the Railway Pattern.
    /// </remarks>
    /// <param name="AFailure">
    ///   The failure value of type F to set.
    /// </param>
    procedure _SetFailureValue(const AFailure: F);

    /// <summary>
    ///   Returns a TResultPair with the success value set.
    /// </summary>
    /// <remarks>
    ///   Use this function to create and return a new TResultPair instance with the success
    ///   value set to the value specified in <paramref name="ASuccess"/>.
    /// </remarks>
    /// <param name="ASuccess">
    ///   The success value of type S to set in the returned TResultPair.
    /// </param>
    /// <returns>
    ///   A new TResultPair instance with the success value set to <paramref name="ASuccess"/>.
    /// </returns>
    function _ReturnSuccess: TResultPair<S, F>;

    /// <summary>
    ///   Returns a TResultPair with the failure value set.
    /// </summary>
    /// <remarks>
    ///   Use this function to create and return a new TResultPair instance with the failure
    ///   value set to the value specified in <paramref name="AFailure"/>.
    /// </remarks>
    /// <param name="AFailure">
    ///   The failure value of type F to set in the returned TResultPair.
    /// </param>
    /// <returns>
    ///   A new TResultPair instance with the failure value set to <paramref name="AFailure"/>.
    /// </returns>
    function _ReturnFailure: TResultPair<S, F>;

    ///  <summary>
    ///    Aloca os recursos associados ao valor de falha e sucesso.
    ///  </summary>
    constructor Create(const AResultType: TResultType);
  public
    class operator Implicit(const V: TResultPair<S, F>): TResultPairValue<S>;
    class operator Implicit(const V: TResultPairValue<S>): TResultPair<S, F>;
    class operator Implicit(const V: TResultPair<S, F>): TResultPairValue<F>;
    class operator Implicit(const V: TResultPairValue<F>): TResultPair<S, F>;
    class operator Equal(const Left, Right: TResultPair<S, F>): Boolean;
    class operator NotEqual(const Left, Right: TResultPair<S, F>): Boolean;

    /// <summary>
    ///   Creates and returns a new TResultPair<S, F> instance, initiating a Railway Pattern workflow.
    /// </summary>
    /// <returns>
    ///   A new TResultPair<S, F> instance.
    /// </returns>
    class function New: TResultPair<S, F>; static;

    /// <summary>
    ///   Releases any resources associated with the current TResultPair instance.
    /// </summary>
    /// <remarks>
    ///   Use this procedure to release any resources, if applicable, and perform necessary cleanup
    ///   for the current TResultPair instance. It's recommended to call this method when you are
    ///   finished using the TResultPair object to ensure proper resource management.
    /// </remarks>
    procedure Dispose;

    /// <summary>
    ///   Creates a new instance of TResultPair representing a success result.
    /// </summary>
    /// <param name="ASuccess">
    ///   The success value to be stored in the instance.
    /// </param>
    /// <returns>
    ///   A TResultPair instance with the given success value.
    /// </returns>
    function Success(const ASuccess: S): TResultPair<S, F>;

    /// <summary>
    ///   Creates a new instance of TResultPair representing a failure result.
    /// </summary>
    /// <param name="AFailure">
    ///   The failure value to be stored in the instance.
    /// </param>
    /// <returns>
    ///   A TResultPair instance with the given failure value.
    /// </returns>
    function Failure(const AFailure: F): TResultPair<S, F>;

    /// <summary>
    ///   Executes a success procedure if the current instance represents a success result,
    ///   otherwise, executes a failure procedure.
    /// </summary>
    /// <param name="AFailureProc">
    ///   The procedure to be executed in case of failure.
    /// </param>
    /// <param name="ASuccessProc">
    ///   The procedure to be executed in case of success.
    /// </param>
    /// <returns>
    ///   The same TResultPair instance for chaining.
    /// </returns>
    function TryException(const ASuccessProc: TProc<S>; const AFailureProc: TProc<F>): TResultPair<S, F>;

    /// <summary>
    ///   Performs a "fold" over the result, applying the success function if it is a success result
    ///   or the failure function if it is a failure result.
    /// </summary>
    /// <typeparam name="R">
    ///   The type of return resulting from the application of the success or failure functions.
    /// </typeparam>
    /// <param name="AFailureFunc">
    ///   The function to be applied in case of failure.
    /// </param>
    /// <param name="ASuccessFunc">
    ///   The function to be applied in case of success.
    /// </param>
    /// <returns>
    ///   The result of the application of the function corresponding to the type of result.
    /// </returns>
    function Fold<R>(const AFunc: TFunc<S, F, R>): R;

    /// <summary>
    ///   Evaluates the result and executes the success function if it is a success result,
    ///   or executes the failure function if it is a failure result.
    /// </summary>
    /// <typeparam name="R">
    ///   The type of return resulting from the execution of the success or failure function.
    /// </typeparam>
    /// <param name="ASuccessFunc">
    ///   The function to be executed in case of success.
    /// </param>
    /// <param name="AFailureFunc">
    ///   The function to be executed in case of failure.
    /// </param>
    /// <returns>
    ///   The result of the execution of the function corresponding to the type of result.
    /// </returns>
    function When<R>(const ASuccessFunc: TFunc<S, R>;
      const AFailureFunc: TFunc<F, R>): R;

    /// <summary>
    ///   Applies a mapping function to the success part of the result, producing a new result
    ///   containing the mapped value, keeping the failure part unchanged.
    /// </summary>
    /// <typeparam name="R">
    ///   The type of the value resulting after the mapping function is applied.
    /// </typeparam>
    /// <param name="ASuccessFunc">
    ///   The mapping function to be applied to the success part of the result.
    /// </param>
    /// <returns>
    ///   A new result with the success part mapped according to the specified function
    ///   and the failure part kept intact.
    /// </returns>
    function Map<R>(const ASuccessFunc: TFunc<S, R>): TResultPair<S, F>;

    /// <summary>
    ///   Applies a mapping function to the failure part of the result, producing a new result
    ///   containing the mapped failure part, keeping the success part unchanged.
    /// </summary>
    /// <typeparam name="R">
    ///   The type of the value resulting after the mapping function is applied to the failure part.
    /// </typeparam>
    /// <param name="AFailureFunc">
    ///   The mapping function to be applied to the failure part of the result.
    /// </param>
    /// <returns>
    ///   A new result with the failure part mapped according to the specified function
    ///   and the success part kept intact.
    /// </returns>
    function MapFailure<R>(const AFailureFunc: TFunc<F, R>): TResultPair<S, F>;

    /// <summary>
    ///   Applies a mapping function that operates on the success part of the result, producing
    ///   a new result that can contain a mapped success part or be converted to a failure,
    ///   depending on the result of the mapping function.
    /// </summary>
    /// <typeparam name="R">
    ///   The type of the value resulting after the mapping function is applied to the success part.
    /// </typeparam>
    /// <param name="ASuccessFunc">
    ///   The mapping function to be applied to the success part of the result.
    /// </param>
    /// <returns>
    ///   A new result with the success part mapped or converted to failure based on the result
    ///   of the mapping function applied.
    /// </returns>
    function FlatMap<R>(const ASuccessFunc: TFunc<S, R>): TResultPair<S, F>;

    /// <summary>
    ///   Applies a mapping function that operates on the failure part of the result, producing
    ///   a new result that can contain a mapped failure part or be converted to success,
    ///   depending on the result of the mapping function.
    /// </summary>
    /// <typeparam name="R">
    ///   The type of the value resulting after the mapping function is applied to the failure part.
    /// </typeparam>
    /// <param name="AFailureFunc">
    ///   The mapping function to be applied to the failure part of the result.
    /// </param>
    /// <returns>
    ///   A new result with the failure part mapped or converted to success based on the result
    ///   of the mapping function applied.
    /// </returns>
    function FlatMapFailure<R>(const AFailureFunc: TFunc<F, R>): TResultPair<S, F>;

    /// <summary>
    ///   Creates an instance of a success result containing the specified value.
    /// </summary>
    /// <param name="ASuccess">
    ///   The value to be placed in the success part of the result.
    /// </param>
    /// <returns>
    ///   An instance of a result containing the success part filled with the specified value
    ///   and the failure part empty.
    /// </returns>
    function Pure(const ASuccess: S): TResultPair<S, F>;

    /// <summary>
    ///   Creates an instance of a failure result containing the specified error.
    /// </summary>
    /// <param name="AFailure">
    ///   The error to be placed in the failure part of the result.
    /// </param>
    /// <returns>
    ///   An instance of a result containing the failure part filled with the specified error
    ///   and the success part empty.
    /// </returns>
    function PureFailure(const AFailure: F): TResultPair<S, F>;

    /// <summary>
    ///   Swaps the success and failure parts of the result.
    /// </summary>
    /// <returns>
    ///   A new instance of result with the success and failure parts swapped.
    /// </returns>
    function Swap: TResultPair<F, S>;

    /// <summary>
    ///   Attempts to recover the failure, applying a conversion function <paramref name="AFailureFunc"/>
    ///   to obtain a new value of type <typeparamref name="N"/> and keeping the success part
    ///   unchanged.
    /// </summary>
    /// <typeparam name="N">
    ///   The new type for the recovered failure part.
    /// </typeparam>
    /// <param name="AFailureFunc">
    ///   The conversion function to be applied to the current failure part to obtain a value of <typeparamref name="N"/>.
    /// </param>
    /// <returns>
    ///   A new instance of result with the recovered failure part or the success part.
    /// </returns>
    function Recover<R>(const AFailureFunc: TFunc<F, R>): TResultPair<R, S>;

    /// <summary>
    ///   Gets the success value, applying a function <paramref name="ASuccessFunc"/> if
    ///   the result is a success (<paramref name="rtSuccess"/>), or returning a default value
    ///   if it is a failure (<paramref name="rtFailure"/>).
    /// </summary>
    /// <param name="ASuccessFunc">
    ///   The function to be applied to the success value.
    /// </param>
    /// <returns>
    ///   The success value, or the default value of the success part if it is a failure.
    /// </returns>
    function GetSuccessOrElse(const ASuccessFunc: TFunc<S, S>): S;

    /// <summary>
    ///   Gets the success value, returning it if the result is a success
    ///   (<paramref name="rtSuccess"/>), or throwing the exception contained in the failure part
    ///   (<paramref name="rtFailure"/>).
    /// </summary>
    /// <returns>
    ///   The success value, or throws the exception contained in the failure part.
    /// </returns>
    /// <exception cref="EFailureValue">
    ///   Exception contained in the failure part, if the result is a failure.
    /// </exception>
    function GetSuccessOrException: S;

    /// <summary>
    ///   Gets the success value, returning it if the result is a success
    ///   (<paramref name="rtSuccess"/>), or a default value if it is a failure result
    ///   (<paramref name="rtFailure"/>).
    /// </summary>
    /// <returns>
    ///   The success value if the result is a success, otherwise, a value
    ///   provided as a default.
    /// </returns>
    function GetSuccessOrDefault: S; overload;

    /// <summary>
    ///   Gets the success value, returning it if the result is a success
    ///   (<paramref name="rtSuccess"/>), or a default value provided as a parameter if
    ///   it is a failure result (<paramref name="rtFailure"/>).
    /// </summary>
    /// <param name="ADefault">
    ///   The default value to be returned if the result is a failure.
    /// </param>
    /// <returns>
    ///   The success value if the result is a success, otherwise, the value
    ///   provided as a default.
    /// </returns>
    function GetSuccessOrDefault(const ADefault: S): S; overload;

    /// <summary>
    ///   Gets the failure value, throwing it as an exception if the result is a failure
    ///   (<paramref name="rtFailure"/>).
    /// </summary>
    /// <exception cref="EFailureValueException">
    ///   Thrown when the result is a failure, containing the failure value.
    /// </exception>
    /// <returns>
    ///   The failure value, if the result is a failure.
    /// </returns>
    function GetFailureOrElse(const AFailureFunc: TFunc<F, F>): F;

    /// <summary>
    ///   Gets the failure value, throwing it as an exception if the result is a failure
    ///   (<paramref name="rtFailure"/>).
    /// </summary>
    /// <exception cref="EFailureValueException">
    ///   Thrown when the result is a failure, containing the failure value.
    /// </exception>
    /// <returns>
    ///   The failure value, if the result is a failure.
    /// </returns>
    function GetFailureOrException: F;

    /// <summary>
    ///   Gets the failure value or the default value of type <typeparamref name="F"/>,
    ///   if the result is a failure (<paramref name="rtFailure"/>).
    /// </summary>
    /// <returns>
    ///   The failure value, if the result is a failure, otherwise, a default value of <typeparamref name="F"/>.
    /// </returns>
    function GetFailureOrDefault: F; overload;

    /// <summary>
    ///   Gets the failure value or a default value provided, if the result is a failure
    ///   (<paramref name="rtFailure"/>).
    /// </summary>
    /// <param name="ADefault">
    ///   The default value to be returned if the result is a failure.
    /// </param>
    /// <returns>
    ///   The failure value, if the result is a failure, otherwise, the default value provided.
    /// </returns>
    function GetFailureOrDefault(const ADefault: F): F; overload;

    /// <summary>
    ///   Determines whether the result is a success (<paramref name="rtSuccess"/>).
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the result is a success, otherwise <c>False</c>.
    /// </returns>
    function isSuccess: boolean;

    /// <summary>
    ///   Determines whether the result is a failure (<paramref name="rtFailure"/>).
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the result is a failure, otherwise <c>False</c>.
    /// </returns>
    function isFailure: boolean;

    /// <summary>
    ///   Gets the success value contained in the result, throwing an exception if it is a failure result.
    /// </summary>
    /// <returns>
    ///   The success value, if the result is a success.
    /// </returns>
    /// <exception cref="EInvalidOperation">
    ///   Thrown if the result is a failure.
    /// </exception>
    function ValueSuccess: S;

    /// <summary>
    ///   Gets the failure value contained in the result, throwing an exception if it is a success result.
    /// </summary>
    /// <returns>
    ///   The failure value, if the result is a failure.
    /// </returns>
    /// <exception cref="EInvalidOperation">
    ///   Thrown if the result is a success.
    /// </exception>
    function ValueFailure: F;

    /// <summary>
    ///   Executes a custom function specified by AFunc. The result of this function determines whether
    ///   the workflow continues down the success or failure path.
    /// </summary>
    /// <param name="AFunc">
    ///   A custom function that receives the current TResultPair<S, F> instance and returns a new
    ///   TResultPair<S, F>.
    /// </param>
    /// <returns>
    ///   The TResultPair<S, F> instance after executing the custom function.
    /// </returns>
    function Exec(const AFunc: TFuncExec): TResultPair<S, F>;

    /// <summary>
    ///   Marks the current step as successful and provides a value ASuccess to carry forward in the
    ///   success path.
    /// </summary>
    /// <param name="ASuccess">
    ///   The value representing the successful outcome of the current step.
    /// </param>
    /// <returns>
    ///   The TResultPair<S, F> instance after marking the step as successful.
    /// </returns>
    function Ok(const ASuccessProc: TProc<S>): TResultPair<S, F>;

    /// <summary>
    ///   Marks the current step as a failure and provides a value AFailure to carry forward in the
    ///   failure path.
    /// </summary>
    /// <param name="AFailure">
    ///   The value representing the failure outcome of the current step.
    /// </param>
    /// <returns>
    ///   The TResultPair<S, F> instance after marking the step as a failure.
    /// </returns>
    function Fail(const AFailureProc: TProc<F>): TResultPair<S, F>;

    /// <summary>
    ///   Specifies a custom function AFunc to execute if the previous step was successful. It continues
    ///   the success path.
    /// </summary>
    /// <param name="AFunc">
    ///   A custom function that processes the successful outcome and returns a new TResultPair<S, F>.
    /// </param>
    /// <returns>
    ///   The TResultPair<S, F> instance after executing the custom function.
    /// </returns>
    function ThenOf(const AFunc: TFuncOk): TResultPair<S, F>;

    /// <summary>
    ///   Specifies a custom function AFunc to execute if the previous step resulted in failure. It
    ///   continues the failure path.
    /// </summary>
    /// <param name="AFunc">
    ///   A custom function that processes the failure outcome and returns a new TResultPair<S, F>.
    /// </param>
    /// <returns>
    ///   The TResultPair<S, F> instance after executing the custom function.
    /// </returns>
    function ExceptOf(const AFunc: TFuncFail): TResultPair<S, F>;

    /// <summary>
    ///   Returns the current TResultPair<S, F> instance, allowing you to retrieve the final result of
    ///   the Railway Pattern workflow.
    /// </summary>
    /// <returns>
    ///   The current TResultPair<S, F> instance.
    /// </returns>
    function Return: TResultPair<S, F>;
  end;

implementation

{ TResultPairBr<S, F> }

procedure TResultPair<S, F>._DestroySuccess;
var
  LTypeInfo: PTypeInfo;
  LObject: TValue;
begin
  if Assigned(Pointer(FSuccess)) then
  begin
    LTypeInfo := TypeInfo(S);
    if LTypeInfo.Kind = tkClass then
    begin
      LObject := TValue.From<S>(FSuccess^.GetValue);
      LObject.AsObject.Free;
    end;
    System.Dispose(FSuccess);
    FSuccess := nil;
    System.Dispose(FSuccessFuncs);
    FSuccessFuncs := nil;
  end;
end;

procedure TResultPair<S, F>._SetFailureValue(const AFailure: F);
begin
  FFailure^ := TResultPairValue<F>.Create(AFailure);
  FResultType := TResultType.rtFailure;
end;

procedure TResultPair<S, F>._SetSuccessValue(const ASuccess: S);
begin
  FSuccess^ := TResultPairValue<S>.Create(ASuccess);
  FResultType := TResultType.rtSuccess;
end;

constructor TResultPair<S, F>.Create(const AResultType: TResultType);
begin
  System.New(FSuccess);
  System.New(FFailure);
  System.New(FSuccessFuncs);
  System.New(FFailureFuncs);
  FResultType := AResultType;
end;

procedure TResultPair<S, F>.Dispose;
begin
  _DestroySuccess;
  _DestroyFailure;
end;

procedure TResultPair<S, F>._DestroyFailure;
var
  LTypeInfo: PTypeInfo;
  LObject: TValue;
begin
  if Assigned(Pointer(FFailure)) then
  begin
    LTypeInfo := TypeInfo(F);
    if LTypeInfo.Kind = tkClass then
    begin
      LObject := TValue.From<F>(FFailure^.GetValue);
      LObject.AsObject.Free;
    end;
    System.Dispose(FFailure);
    FFailure := nil;
    System.Dispose(FFailureFuncs);
    FFailureFuncs := nil;
  end;
end;

function TResultPair<S, F>.Fail(const AFailureProc: TProc<F>): TResultPair<S, F>;
begin
  Result := Self;
  if not Assigned(AFailureProc) then
    exit;
  case FResultType of
    TResultType.rtFailure: AFailureProc(FFailure^.GetValue);
  end;
end;

function TResultPair<S, F>.Failure(const AFailure: F): TResultPair<S, F>;
begin
  Result := Self;
  Result._SetFailureValue(AFailure);
end;

function TResultPair<S, F>.Return: TResultPair<S, F>;
begin
  if Self.FResultType in [TResultType.rtSuccess] then
    Result := _ReturnSuccess
  else
  if Self.FResultType in [TResultType.rtFailure] then
    Result := _ReturnFailure;
end;

function TResultPair<S, F>._ReturnFailure: TResultPair<S, F>;
var
  LFor: integer;
  LResult: TResultPair<S, F>;
begin
  Result := Self;
  for LFor := Low(Result.FFailureFuncs^) to High(Result.FFailureFuncs^) do
  begin
    LResult := Result.FFailureFuncs^[LFor](Result.FFailure^.GetValue);
    try
      if LResult.isSuccess then
        Result._SetSuccessValue(LResult.ValueSuccess)
      else
      if LResult.isFailure then
        Result._SetFailureValue(LResult.ValueFailure);
    finally
      LResult.Dispose;
    end;
  end;
end;

function TResultPair<S, F>._ReturnSuccess: TResultPair<S, F>;
var
  LFor: integer;
  LResult: TResultPair<S, F>;
begin
  Result := Self;
  for LFor := Low(Result.FSuccessFuncs^) to High(Result.FSuccessFuncs^) do
  begin
    LResult := Result.FSuccessFuncs^[LFor](Result.FSuccess^.GetValue);
    try
      if LResult.isSuccess then
        Result._SetSuccessValue(LResult.ValueSuccess)
      else
      if LResult.isFailure then
        Result._SetFailureValue(LResult.ValueFailure);
    finally
      LResult.Dispose;
    end;
  end;
end;

function TResultPair<S, F>.Success(const ASuccess: S): TResultPair<S, F>;
begin
  Result := Self;
  Result._SetSuccessValue(ASuccess);
end;

function TResultPair<S, F>.isFailure: boolean;
begin
  Result := FResultType = TResultType.rtFailure;
end;

function TResultPair<S, F>.isSuccess: boolean;
begin
  Result := FResultType = TResultType.rtSuccess;
end;

function TResultPair<S, F>.Map<R>(const ASuccessFunc: TFunc<S, R>): TResultPair<S, F>;
var
  LCast: TValue;
begin
  Result := Self;
  if not Assigned(ASuccessFunc) then
    exit;
  case FResultType of
    TResultType.rtSuccess:
    begin
      LCast := TValue.From<R>(ASuccessFunc(FSuccess^.GetValue));
      Result._SetSuccessValue(LCast.AsType<S>);
    end;
  end;
end;

function TResultPair<S, F>.When<R>(const ASuccessFunc: TFunc<S, R>;
  const AFailureFunc: TFunc<F, R>): R;
begin
  if (not Assigned(ASuccessFunc)) and (not Assigned(AFailureFunc)) then
    exit;
  case FResultType of
    TResultType.rtSuccess: Result := ASuccessFunc(FSuccess^.GetValue);
    TResultType.rtFailure: Result := AFailureFunc(FFailure^.GetValue);
  end;
end;

function TResultPair<S, F>.Fold<R>(const AFunc: TFunc<S, F, R>): R;
begin
  if not Assigned(AFunc) then
    exit;
  case FResultType of
    TResultType.rtSuccess: Result := AFunc(FSuccess^.GetValue, Default(F));
    TResultType.rtFailure: Result := AFunc(Default(S), FFailure^.GetValue);
  end;
end;

function TResultPair<S, F>.ThenOf(const AFunc: TFuncOk): TResultPair<S, F>;
begin
  Result := Self;
  if (FResultType in [TResultType.rtSuccess]) and Assigned(AFunc) then
  begin
    SetLength(Result.FSuccessFuncs^, Length(Result.FSuccessFuncs^) + 1);
    Result.FSuccessFuncs^[Length(Result.FSuccessFuncs^) - 1] := AFunc;
  end;
end;

function TResultPair<S, F>.TryException(const ASuccessProc: TProc<S>;
  const AFailureProc: TProc<F>): TResultPair<S, F>;
begin
  Result := Self;
  if (not Assigned(ASuccessProc)) and (not Assigned(AFailureProc)) then
    exit;
  case FResultType of
    TResultType.rtSuccess: ASuccessProc(FSuccess^.GetValue);
    TResultType.rtFailure: AFailureProc(FFailure^.GetValue);
  end;
end;

function TResultPair<S, F>.GetSuccessOrException: S;
begin
  if FResultType = TResultType.rtFailure then
    raise EFailureException<F>.Create(FFailure^.GetValue);
  Result := FSuccess^.GetValue;
end;

class operator TResultPair<S, F>.Implicit(
  const V: TResultPairValue<F>): TResultPair<S, F>;
begin
  Result.FFailure^ := V;
end;

class operator TResultPair<S, F>.Implicit(
  const V: TResultPair<S, F>): TResultPairValue<F>;
begin
  Result := V.FFailure^;
end;

class operator TResultPair<S, F>.Implicit(
  const V: TResultPairValue<S>): TResultPair<S, F>;
begin
  Result.FSuccess^ := V;
end;

class operator TResultPair<S, F>.Implicit(
  const V: TResultPair<S, F>): TResultPairValue<S>;
begin
  Result := V.FSuccess^;
end;

function TResultPair<S, F>.ValueFailure: F;
begin
  Result := FFailure^.GetValue;
end;

function TResultPair<S, F>.ValueSuccess: S;
begin
  Result := FSuccess^.GetValue;
end;

function TResultPair<S, F>.MapFailure<R>(
  const AFailureFunc: TFunc<F, R>): TResultPair<S, F>;
var
  LCast: TValue;
begin
  Result := Self;
  if not Assigned(AFailureFunc) then
    exit;
  case FResultType of
    TResultType.rtFailure:
    begin
      LCast := TValue.From<R>(AFailureFunc(FFailure^.GetValue));
      Result._SetFailureValue(LCast.AsType<F>);
    end;
  end;
end;

class function TResultPair<S, F>.New: TResultPair<S, F>;
begin
  Result := TResultPair<S, F>.Create(TResultType.rtNone);
end;

class operator TResultPair<S, F>.NotEqual(const Left,
  Right: TResultPair<S, F>): Boolean;
begin
  Result := not (Left = Right);
end;

function TResultPair<S, F>.Ok(const ASuccessProc: TProc<S>): TResultPair<S, F>;
begin
  Result := Self;
  if not Assigned(ASuccessProc) then
    exit;
  case FResultType of
    TResultType.rtSuccess: ASuccessProc(FSuccess^.GetValue);
  end;
end;

function TResultPair<S, F>.FlatMap<R>(
  const ASuccessFunc: TFunc<S, R>): TResultPair<S, F>;
var
  LCast: TValue;
begin
  Result := Self;
  if not Assigned(ASuccessFunc) then
    exit;
  case FResultType of
    TResultType.rtSuccess:
    begin
      LCast := TValue.From<R>(ASuccessFunc(FSuccess^.GetValue));
      Result._SetSuccessValue(LCast.AsType<S>);
    end;
    TResultType.rtFailure: Result._SetFailureValue(FFailure^.GetValue);
  end;
end;

function TResultPair<S, F>.FlatMapFailure<R>(
  const AFailureFunc: TFunc<F, R>): TResultPair<S, F>;
var
  LCast: TValue;
begin
  Result := Self;
  if not Assigned(AFailureFunc) then
    exit;
  case FResultType of
    TResultType.rtFailure:
    begin
      LCast := TValue.From<R>(AFailureFunc(FFailure^.GetValue));
      Result._SetFailureValue(LCast.AsType<F>);
    end;
  end;
end;

function TResultPair<S, F>.Pure(const ASuccess: S): TResultPair<S, F>;
begin
  if Assigned(Pointer(FSuccess)) then
  begin
    Result := Self;
    Result._SetSuccessValue(ASuccess);
  end
  else
    Self._SetSuccessValue(ASuccess);
end;

function TResultPair<S, F>.PureFailure(const AFailure: F): TResultPair<S, F>;
begin
  if Assigned(Pointer(FFailure)) then
  begin
    Result := Self;
    Result._SetFailureValue(AFailure);
  end
  else
    Self._SetFailureValue(AFailure);
end;

function TResultPair<S, F>.GetSuccessOrElse(const ASuccessFunc: TFunc<S, S>): S;
begin
  case FResultType of
    TResultType.rtSuccess: Result := FSuccess^.GetValue;
    TResultType.rtFailure: Result := ASuccessFunc(FSuccess^.GetValue);
  end;
end;

function TResultPair<S, F>.GetSuccessOrDefault(const ADefault: S): S;
begin
  case FResultType of
    TResultType.rtSuccess: Result := FSuccess^.GetValue;
    TResultType.rtFailure: Result := ADefault;
  end;
end;

function TResultPair<S, F>.GetSuccessOrDefault: S;
begin
  case FResultType of
    TResultType.rtSuccess: Result := FSuccess^.GetValue;
    TResultType.rtFailure: Result := Default(S);
  end;
end;

class operator TResultPair<S, F>.Equal(const Left,
  Right: TResultPair<S, F>): Boolean;
begin
  Result := (Left = Right);
end;

function TResultPair<S, F>.ExceptOf(const AFunc: TFuncFail): TResultPair<S, F>;
begin
  Result := Self;
  if (FResultType in [TResultType.rtFailure]) and Assigned(AFunc) then
  begin
    SetLength(Result.FFailureFuncs^, Length(Result.FFailureFuncs^) + 1);
    Result.FFailureFuncs^[Length(Result.FFailureFuncs^) - 1] := AFunc;
  end;
end;

function TResultPair<S, F>.Exec(const AFunc: TFuncExec): TResultPair<S, F>;
var
  LResult: TResultPair<S, F>;
begin
  if not Assigned(AFunc) then
    exit;
  LResult := AFunc();
  if LResult.isSuccess then
    Result.Success(LResult.FSuccess^.GetValue)
  else
  if LResult.isFailure then
    Result.Failure(LResult.FFailure^.GetValue);
end;

function TResultPair<S, F>.GetFailureOrDefault: F;
begin
  case FResultType of
    TResultType.rtSuccess: Result := Default(F);
    TResultType.rtFailure: Result := FFailure^.GetValue;
  end;
end;

function TResultPair<S, F>.GetFailureOrDefault(const ADefault: F): F;
begin
  case FResultType of
    TResultType.rtSuccess: Result := ADefault;
    TResultType.rtFailure: Result := FFailure^.GetValue;
  end;
end;

function TResultPair<S, F>.GetFailureOrElse(const AFailureFunc: TFunc<F, F>): F;
begin
  case FResultType of
    TResultType.rtSuccess: Result := AFailureFunc(FFailure^.GetValue);
    TResultType.rtFailure: Result := FFailure^.GetValue;
  end;
end;

function TResultPair<S, F>.GetFailureOrException: F;
begin
  if FResultType = TResultType.rtSuccess then
    raise ESuccessException<F>.Create(FFailure^.GetValue);
  Result := FFailure^.GetValue;
end;

function TResultPair<S, F>.Swap: TResultPair<F, S>;
begin
  try
    case FResultType of
      TResultType.rtSuccess: Result := TResultPair<F, S>.New.Failure(FSuccess^.GetValue);
      TResultType.rtFailure: Result := TResultPair<F, S>.New.Success(FFailure^.GetValue);
    end;
  except
    on E: Exception do
      raise ETypeIncompatibility.Create('[Success/Failure]');
  end;
end;

function TResultPair<S, F>.Recover<R>(const AFailureFunc: TFunc<F, R>): TResultPair<R, S>;
var
  LCast: TValue;
begin
  if not Assigned(AFailureFunc) then
    exit;
  case FResultType of
    TResultType.rtFailure:
    begin
      LCast := TValue.From<R>(AFailureFunc(FFailure^.GetValue));
      Result := TResultPair<R, S>.New.Success(LCast.AsType<R>);
    end;
  end;
end;

{ TResultPairValue<T> }

constructor TResultPairValue<T>.Create(AValue: T);
begin
  FValue := AValue;
  FHasValue := True;
end;

class function TResultPairValue<T>.CreateNil: TResultPairValue<T>;
begin
  Result.FHasValue := False;
end;

function TResultPairValue<T>.GetValue: T;
begin
  if not FHasValue then
    raise Exception.Create('Value is nil.');
  Result := FValue;
end;

function TResultPairValue<T>.HasValue: boolean;
begin
  Result := FHasValue;
end;

{ EFailureException<S> }

constructor EFailureException<F>.Create(const Value: F);
begin
  inherited CreateFmt('A generic exception occurred with value %s', [TValue.From<F>(Value).AsString]);
end;

{ ESuccessException<S> }

constructor ESuccessException<S>.Create(const Value: S);
begin
  inherited CreateFmt('A generic exception occurred with value %s', [TValue.From<S>(Value).AsString]);
end;

{ ETypeIncompatibility }

constructor ETypeIncompatibility.Create(const AMessage: string);
begin
  inherited CreateFmt('Type incompatibility: %s', [AMessage]);
end;

end.
