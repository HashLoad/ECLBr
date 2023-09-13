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

unit eclbr.ifthen;

interface

uses
  SysUtils,
  Generics.Collections;

type
  TValue<T> = record
    Value: T;
    class operator Implicit(AValue: T): TValue<T>;
  end;

  TIfThen<T> = packed record
  private
    class var
      FCondition: boolean;
      FTrueValue: TValue<T>;
      FFalseValue: TValue<T>;
      FTrueFunc: TFunc<T>;
      FFalseFuncs: TArray<TPair<boolean, TFunc<T>>>;
      FTryExcept: TProc<Exception>;
  private
    /// <summary>
    ///   Conditionally adds a function to be executed if the specified condition is false.
    /// </summary>
    /// <remarks>
    ///   This procedure allows you to conditionally add a function to the collection to be executed
    ///   if the specified condition evaluates to false when processing a conditional chain.
    /// </remarks>
    /// <param name="ACondition">
    ///   A boolean value representing the condition to evaluate.
    /// </param>
    /// <param name="AFalseFunc">
    ///   A TFunc<T> representing the function to be executed if the condition is false.
    /// </param>
    procedure _AddElseFunc(const ACondition: boolean; const AFalseFunc: TFunc<T>);
  public
    /// <summary>
    ///   Sets the condition for conditional evaluation.
    /// </summary>
    /// <param name="ACondition">The condition to be evaluated.</param>
    class function When(const ACondition: boolean): TIfThen<T>; overload; static;

    /// <summary>
    ///   Sets the condition for conditional evaluation.
    /// </summary>
    /// <param name="AConditionFunc">The function that returns the condition to be evaluated.</param>
    class function When(const AConditionFunc: TFunc<boolean>): TIfThen<T>; overload; static;

    /// <summary>
    ///   Sets the value to be returned when the condition is true.
    /// </summary>
    /// <param name="ATrueValue">The value to be returned if the condition is true.</param>
    function ThenOf(const ATrueValue: T): TIfThen<T>; overload;

    /// <summary>
    ///   Sets the value to be returned when the condition is true.
    /// </summary>
    /// <param name="ATrueFunc">The function that returns the value to be returned if the condition is true.</param>
    function ThenOf(const ATrueFunc: TFunc<T>): TIfThen<T>; overload;

    /// <summary>
    ///   Sets the value to be returned when the condition is false.
    /// </summary>
    /// <param name="AFalseValue">The value to be returned if the condition is false.</param>
    function ElseOf(const AFalseValue: T): TIfThen<T>; overload;

    /// <summary>
    ///   Sets the value to be returned when the condition is false.
    /// </summary>
    /// <param name="AFelseFunc">The function that returns the value to be returned if the condition is false.</param>
    function ElseOf(const AFalseFunc: TFunc<T>): TIfThen<T>; overload;

    /// <summary>
    ///   Sets the value to be returned when the condition is false.
    /// </summary>
    /// <param name="ACondition">The condition to be evaluated.</param>
    /// <param name="AFelseFunc">The function that returns the value to be returned if the condition is false.</param>
    function ElseOf(const ACondition: boolean; const AFalseFunc: TFunc<T>): TIfThen<T>; overload;

    /// <summary>
    ///   Sets the value to be returned when the condition is false.
    /// </summary>
    /// <param name="ACondition">The condition to be evaluated.</param>
    /// <param name="AFalseValue">The value to be returned if the condition is false.</param>
    function ElseOf(const ACondition: boolean; const AFalseValue: T): TIfThen<T>; overload;

    /// <summary>
    ///   Wraps the specified action in a try-except block to catch exceptions and handle them gracefully.
    /// </summary>
    /// <param name="AProc">
    ///   The procedure to be executed within the try-except block. This procedure may throw exceptions,
    ///   which will be caught and handled within the exception block.
    /// </param>
    /// <returns>
    ///   An instance of the <see cref="TIfThen{T}"/> class, which allows you to handle the outcome of the operation
    ///   and decide how to proceed based on success or failure.
    /// </returns>
    function TryExcept(const AProc: TProc<Exception>): TIfThen<T>;

    /// <summary>
    ///   Returns the value corresponding to conditional evaluation.
    /// </summary>
    function Return: T;
  end;

implementation

class operator TValue<T>.Implicit(AValue: T): TValue<T>;
begin
  Result.Value := AValue;
end;

class function TIfThen<T>.When(const ACondition: boolean): TIfThen<T>;
begin
  Result.FCondition := ACondition;
  Result.FTrueValue := Default(TValue<T>);
  Result.FFalseValue := Default(TValue<T>);
  Result.FTrueFunc := nil;
  Result.FFalseFuncs := [];
end;

function TIfThen<T>.ThenOf(const ATrueValue: T): TIfThen<T>;
begin
  FTrueValue := ATrueValue;
  Result := Self;
end;

function TIfThen<T>.ElseOf(const AFalseValue: T): TIfThen<T>;
begin
  FFalseValue := AFalseValue;
  Result := Self;
end;

function TIfThen<T>.ElseOf(const AFalseFunc: TFunc<T>): TIfThen<T>;
begin
  _AddElseFunc(true, AFalseFunc);
  Result := Self;
end;

function TIfThen<T>.ElseOf(const ACondition: boolean; const AFalseFunc: TFunc<T>): TIfThen<T>;
begin
  _AddElseFunc(ACondition, AFalseFunc);
  Result := Self;
end;

function TIfThen<T>.ElseOf(const ACondition: boolean; const AFalseValue: T): TIfThen<T>;
begin
  if ACondition then
    FFalseValue := AFalseValue
  else
    FFalseValue := Default(T);
  Result := Self;
end;

function TIfThen<T>.Return: T;
var
  LFunc: TPair<boolean, TFunc<T>>;
begin
  try
    try
      if FCondition then
      begin
        if Assigned(FTrueFunc) then
          Result := FTrueFunc()
        else
          Result := FTrueValue.Value;
      end
      else
      begin
        for LFunc in FFalseFuncs do
        begin
          if Assigned(LFunc.Value) and (LFunc.Key) then
          begin
            Result := LFunc.Value();
            exit;
          end;
        end;
        Result := FFalseValue.Value;
      end;
    except
      on E: Exception do
      begin
        if Assigned(FTryExcept) then
          FTryExcept(E)
        else
          raise;
      end;
    end;
  finally
    FCondition := false;
    FTrueValue.Value := Default(T);
    FFalseValue.Value := Default(T);
    FTrueFunc := nil;
    FFalseFuncs := [];
  end;
end;

function TIfThen<T>.ThenOf(const ATrueFunc: TFunc<T>): TIfThen<T>;
begin
  FTrueFunc := ATrueFunc;
  Result := Self;
end;

function TIfThen<T>.TryExcept(const AProc: TProc<Exception>): TIfThen<T>;
begin
  FTryExcept := AProc;
  Result := Self;
end;

class function TIfThen<T>.When(const AConditionFunc: TFunc<boolean>): TIfThen<T>;
begin
  Result.FCondition := AConditionFunc();
  Result.FTrueValue := Default(TValue<T>);
  Result.FFalseValue := Default(TValue<T>);
  Result.FTrueFunc := nil;
  Result.FFalseFuncs := [];
end;

procedure TIfThen<T>._AddElseFunc(const ACondition: boolean; const AFalseFunc: TFunc<T>);
begin
  SetLength(FFalseFuncs, Length(FFalseFuncs) + 1);
  FFalseFuncs[High(FFalseFuncs)] := TPair<boolean, TFunc<T>>.Create(ACondition, AFalseFunc);
end;

end.

