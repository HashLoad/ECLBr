{
             ECL Brasil - Essential Core Library for Delphi

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers�o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos � permitido copiar e distribuir c�pias deste documento de
       licen�a, mas mud�-lo n�o � permitido.

       Esta vers�o da GNU Lesser General Public License incorpora
       os termos e condi��es da vers�o 3 da GNU General Public License
       Licen�a, complementado pelas permiss�es adicionais listadas no
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
  SysUtils,
  eclbr.result.pair.value;

type
  TResultType = (rtSuccess, rtFailure);

  TResultPair<S, F> = record
  private
    FSuccess: ^TResultPairValue<S>;
    FFailure: ^TResultPairValue<F>;
    FResultType: TResultType;
  private
    type
      TMapFunc<Return> = reference to function(const ASelf: TResultPair<S, F>): Return;
  private
    ///  <summary>
    ///    Libera os recursos associados ao valor de sucesso da inst�ncia.
    ///  </summary>
    procedure _DestroySuccess;

    ///  <summary>
    ///    Libera os recursos associados ao valor de falha da inst�ncia.
    ///  </summary>
    procedure _DestroyFailure;

    ///  <summary>
    ///    Aloca os recursos associados ao valor de falha e sucesso.
    ///  </summary>
    procedure _AllocResultPair;
  public
    class operator Implicit(const V: TResultPair<S, F>): TResultPairValue<S>;
    class operator Implicit(const V: TResultPairValue<S>): TResultPair<S, F>;
    class operator Implicit(const V: TResultPair<S, F>): TResultPairValue<F>;
    class operator Implicit(const V: TResultPairValue<F>): TResultPair<S, F>;
    class operator Equal(const Left, Right: TResultPair<S, F>): Boolean;
    class operator NotEqual(const Left, Right: TResultPair<S, F>): Boolean;

    procedure Dispose;
    ///  <summary>
    ///    Cria uma nova inst�ncia de TResultPair representando um resultado de sucesso.
    ///  </summary>
    ///  <param name="ASuccess">
    ///    O valor de sucesso a ser armazenado na inst�ncia.
    ///  </param>
    ///  <returns>
    ///    Uma inst�ncia de TResultPair com o valor de sucesso fornecido.
    ///  </returns>
    function Success(const ASuccess: S): TResultPair<S, F>;

    ///  <summary>
    ///    Cria uma nova inst�ncia de TResultPair representando um resultado de falha.
    ///  </summary>
    ///  <param name="AFailure">
    ///    O valor de falha a ser armazenado na inst�ncia.
    ///  </param>
    ///  <returns>
    ///    Uma inst�ncia de TResultPair com o valor de falha fornecido.
    ///  </returns>
    function Failure(const AFailure: F): TResultPair<S, F>;

    ///  <summary>
    ///    Executa um procedimento de sucesso se a inst�ncia atual representar um resultado de sucesso,
    ///    caso contr�rio, executa um procedimento de falha.
    ///  </summary>
    ///  <param name="AFailureProc">
    ///    O procedimento a ser executado em caso de falha.
    ///  </param>
    ///  <param name="ASuccessProc">
    ///    O procedimento a ser executado em caso de sucesso.
    ///  </param>
    ///  <returns>
    ///    A pr�pria inst�ncia de TResultPair para encadeamento.
    ///  </returns>
    function TryException(const ASuccessProc: TProc<S>; const AFailureProc: TProc<F>): TResultPair<S, F>;

    ///  <summary>
    ///    Realiza um "fold" sobre o resultado, aplicando a fun��o de sucesso se for um resultado de sucesso
    ///    ou a fun��o de falha se for um resultado de falha.
    ///  </summary>
    ///  <typeparam name="R">
    ///    O tipo de retorno resultante da aplica��o das fun��es de sucesso ou falha.
    ///  </typeparam>
    ///  <param name="AFailureFunc">
    ///    A fun��o a ser aplicada em caso de falha.
    ///  </param>
    ///  <param name="ASuccessFunc">
    ///    A fun��o a ser aplicada em caso de sucesso.
    ///  </param>
    ///  <returns>
    ///    O resultado da aplica��o da fun��o correspondente ao tipo de resultado.
    ///  </returns>
    function Fold<R>(const AFunc: TFunc<S, F, R>): R;

    ///  <summary>
    ///    Avalia o resultado e executa a fun��o de sucesso se for um resultado de sucesso,
    ///    ou executa a fun��o de falha se for um resultado de falha.
    ///  </summary>
    ///  <typeparam name="R">
    ///    O tipo de retorno resultante da execu��o da fun��o de sucesso ou falha.
    ///  </typeparam>
    ///  <param name="ASuccessFunc">
    ///    A fun��o a ser executada em caso de sucesso.
    ///  </param>
    ///  <param name="AFailureFunc">
    ///    A fun��o a ser executada em caso de falha.
    ///  </param>
    ///  <returns>
    ///    O resultado da execu��o da fun��o correspondente ao tipo de resultado.
    ///  </returns>
    function When<R>(const ASuccessFunc: TFunc<S, R>;
      const AFailureFunc: TFunc<F, R>): R;

    ///  <summary>
    ///    Aplica uma fun��o de mapeamento � parte de sucesso do resultado, produzindo um novo resultado
    ///    contendo o valor mapeado, mantendo a parte de falha inalterada.
    ///  </summary>
    ///  <typeparam name="R">
    ///    O tipo do valor resultante ap�s a aplica��o da fun��o de mapeamento.
    ///  </typeparam>
    ///  <param name="ASuccessFunc">
    ///    A fun��o de mapeamento a ser aplicada � parte de sucesso do resultado.
    ///  </param>
    ///  <returns>
    ///    Um novo resultado com a parte de sucesso mapeada de acordo com a fun��o especificada
    ///    e a parte de falha mantida intacta.
    ///  </returns>
    function Map<R>(const ASuccessFunc: TFunc<S, R>): TResultPair<S, F>;

    ///  <summary>
    ///    Aplica uma fun��o de mapeamento � parte de falha do resultado, produzindo um novo resultado
    ///    contendo a parte de falha mapeada, mantendo a parte de sucesso inalterada.
    ///  </summary>
    ///  <typeparam name="R">
    ///    O tipo do valor resultante ap�s a aplica��o da fun��o de mapeamento � parte de falha.
    ///  </typeparam>
    ///  <param name="AFailureFunc">
    ///    A fun��o de mapeamento a ser aplicada � parte de falha do resultado.
    ///  </param>
    ///  <returns>
    ///    Um novo resultado com a parte de falha mapeada de acordo com a fun��o especificada
    ///    e a parte de sucesso mantida intacta.
    ///  </returns>
    function MapFailure<R>(const AFailureFunc: TFunc<F, R>): TResultPair<S, F>;

    ///  <summary>
    ///    Aplica uma fun��o de mapeamento que opera sobre a parte de sucesso do resultado, produzindo
    ///    um novo resultado que pode conter uma parte de sucesso mapeada ou ser convertido em uma falha,
    ///    dependendo do resultado da fun��o de mapeamento.
    ///  </summary>
    ///  <typeparam name="R">
    ///    O tipo do valor resultante ap�s a aplica��o da fun��o de mapeamento � parte de sucesso.
    ///  </typeparam>
    ///  <param name="ASuccessFunc">
    ///    A fun��o de mapeamento a ser aplicada � parte de sucesso do resultado.
    ///  </param>
    ///  <returns>
    ///    Um novo resultado com a parte de sucesso mapeada ou convertida em falha com base no resultado
    ///    da fun��o de mapeamento aplicada.
    ///  </returns>
    function FlatMap<R>(const ASuccessFunc: TFunc<S, R>): TResultPair<S, F>;

    ///  <summary>
    ///    Aplica uma fun��o de mapeamento que opera sobre a parte de falha do resultado, produzindo
    ///    um novo resultado que pode conter uma parte de falha mapeada ou ser convertido em sucesso,
    ///    dependendo do resultado da fun��o de mapeamento.
    ///  </summary>
    ///  <typeparam name="R">
    ///    O tipo do valor resultante ap�s a aplica��o da fun��o de mapeamento � parte de falha.
    ///  </typeparam>
    ///  <param name="AFailureFunc">
    ///    A fun��o de mapeamento a ser aplicada � parte de falha do resultado.
    ///  </param>
    ///  <returns>
    ///    Um novo resultado com a parte de falha mapeada ou convertida em sucesso com base no resultado
    ///    da fun��o de mapeamento aplicada.
    ///  </returns>
    function FlatMapFailure<R>(const AFailureFunc: TFunc<F, R>): TResultPair<S, F>;

    ///  <summary>
    ///    Cria uma inst�ncia de resultado de sucesso contendo o valor especificado.
    ///  </summary>
    ///  <param name="ASuccess">
    ///    O valor a ser colocado na parte de sucesso do resultado.
    ///  </param>
    ///  <returns>
    ///    Uma inst�ncia de resultado contendo a parte de sucesso preenchida com o valor especificado
    ///    e a parte de falha vazia.
    ///  </returns>
    function Pure(const ASuccess: S): TResultPair<S, F>;

    ///  <summary>
    ///    Cria uma inst�ncia de resultado de falha contendo o erro especificado.
    ///  </summary>
    ///  <param name="AFailure">
    ///    O erro a ser colocado na parte de falha do resultado.
    ///  </param>
    ///  <returns>
    ///    Uma inst�ncia de resultado contendo a parte de falha preenchida com o erro especificado
    ///    e a parte de sucesso vazia.
    ///  </returns>
    function PureFailure(const AFailure: F): TResultPair<S, F>;

    ///  <summary>
    ///    Troca as partes de sucesso e falha do resultado.
    ///  </summary>
    ///  <returns>
    ///    Uma nova inst�ncia de resultado com as partes de sucesso e falha trocadas.
    ///  </returns>
    function Swap: TResultPair<F, S>;

    ///  <summary>
    ///    Tenta recuperar a falha, aplicando uma fun��o de convers�o <paramref name="AFailureFunc"/>
    ///    para obter um novo valor do tipo <typeparamref name="N"/> e mantendo a parte de sucesso
    ///    inalterada.
    ///  </summary>
    ///  <typeparam name="N">
    ///    O novo tipo para a parte de falha recuperada.
    ///  </typeparam>
    ///  <param name="AFailureFunc">
    ///    A fun��o de convers�o a ser aplicada � parte de falha atual para obter um valor de <typeparamref name="N"/>.
    ///  </param>
    ///  <returns>
    ///    Uma nova inst�ncia de resultado com a parte de falha recuperada ou a parte de sucesso.
    ///  </returns>
    function Recover<R>(const AFailureFunc: TFunc<F, R>): TResultPair<R, S>;

    ///  <summary>
    ///    Obt�m o valor da parte de sucesso, aplicando uma fun��o <paramref name="ASuccessFunc"/> caso
    ///    o resultado seja de sucesso (<paramref name="rtSuccess"/>), ou retornando um valor padr�o
    ///    caso seja de falha (<paramref name="rtFailure"/>).
    ///  </summary>
    ///  <param name="ASuccessFunc">
    ///    A fun��o a ser aplicada ao valor da parte de sucesso.
    ///  </param>
    ///  <returns>
    ///    O valor da parte de sucesso, ou o valor padr�o da parte de sucesso caso seja de falha.
    ///  </returns>
    function GetSuccessOrElse(const ASuccessFunc: TFunc<S, S>): S;

    ///  <summary>
    ///    Obt�m o valor da parte de sucesso, retornando-o caso o resultado seja de sucesso
    ///    (<paramref name="rtSuccess"/>), ou lan�ando a exce��o contida na parte de falha
    ///    (<paramref name="rtFailure"/>).
    ///  </summary>
    ///  <returns>
    ///    O valor da parte de sucesso, ou lan�a a exce��o contida na parte de falha.
    ///  </returns>
    ///  <exception cref="EFailureValue">
    ///    Exce��o contida na parte de falha, caso o resultado seja de falha.
    ///  </exception>
    function GetSuccessOrException: S;

    ///  <summary>
    ///    Obt�m o valor da parte de sucesso, retornando-o caso o resultado seja de sucesso
    ///    (<paramref name="rtSuccess"/>), ou um valor padr�o caso seja um resultado de falha
    ///    (<paramref name="rtFailure"/>).
    ///  </summary>
    ///  <returns>
    ///    O valor da parte de sucesso se o resultado for de sucesso, caso contr�rio, um valor
    ///    padr�o.
    ///  </returns>
    function GetSuccessOrDefault: S; overload;

    ///  <summary>
    ///    Obt�m o valor da parte de sucesso, retornando-o caso o resultado seja de sucesso
    ///    (<paramref name="rtSuccess"/>), ou um valor padr�o fornecido como par�metro caso
    ///    seja um resultado de falha (<paramref name="rtFailure"/>).
    ///  </summary>
    ///  <param name="ADefault">
    ///    O valor padr�o a ser retornado caso o resultado seja de falha.
    ///  </param>
    ///  <returns>
    ///    O valor da parte de sucesso se o resultado for de sucesso, caso contr�rio, o valor
    ///    padr�o fornecido.
    ///  </returns>
    function GetSuccessOrDefault(const ADefault: S): S; overload;

    ///  <summary>
    ///    Obt�m o valor da parte de falha, retornando-o caso o resultado seja de falha
    ///    (<paramref name="rtFailure"/>), ou o resultado da fun��o fornecida como par�metro caso
    ///    seja um resultado de sucesso (<paramref name="rtSuccess"/>).
    ///  </summary>
    ///  <param name="AFailureFunc">
    ///    A fun��o a ser executada para gerar o valor de falha, caso o resultado seja de sucesso.
    ///  </param>
    ///  <returns>
    ///    O valor da parte de falha se o resultado for de falha, caso contr�rio, o resultado
    ///    da fun��o fornecida.
    ///  </returns>
    function GetFailureOrElse(const AFailureFunc: TFunc<F, F>): F;

    ///  <summary>
    ///    Obt�m o valor da parte de falha, lan�ando-o como uma exce��o caso o resultado seja de falha
    ///    (<paramref name="rtFailure"/>).
    ///  </summary>
    ///  <exception cref="EFailureValueException">
    ///    Lan�ada quando o resultado � de falha, contendo o valor da parte de falha.
    ///  </exception>
    ///  <returns>
    ///    O valor da parte de falha, se o resultado for de falha.
    ///  </returns>
    function GetFailureOrException: F;

    ///  <summary>
    ///    Obt�m o valor da parte de falha ou o valor padr�o do tipo <typeparamref name="F"/>,
    ///    caso o resultado seja de falha (<paramref name="rtFailure"/>).
    ///  </summary>
    ///  <returns>
    ///    O valor da parte de falha, se o resultado for de falha, caso contr�rio, um valor padr�o de <typeparamref name="F"/>.
    ///  </returns>
    function GetFailureOrDefault: F; overload;

    ///  <summary>
    ///    Obt�m o valor da parte de falha ou um valor padr�o fornecido, caso o resultado seja de falha
    ///    (<paramref name="rtFailure"/>).
    ///  </summary>
    ///  <param name="ADefault">
    ///    O valor padr�o a ser retornado se o resultado for de falha.
    ///  </param>
    ///  <returns>
    ///    O valor da parte de falha, se o resultado for de falha, caso contr�rio, o valor padr�o fornecido.
    ///  </returns>
    function GetFailureOrDefault(const ADefault: F): F; overload;

    ///  <summary>
    ///    Verifica se o resultado � de sucesso (<paramref name="rtSuccess"/>).
    ///  </summary>
    ///  <returns>
    ///    <c>True</c> se o resultado for de sucesso, caso contr�rio, <c>False</c>.
    ///  </returns>
    function isSuccess: boolean;

    ///  <summary>
    ///    Verifica se o resultado � de falha (<paramref name="rtFailure"/>).
    ///  </summary>
    ///  <returns>
    ///    <c>True</c> se o resultado for de falha, caso contr�rio, <c>False</c>.
    ///  </returns>
    function isFailure: boolean;

    ///  <summary>
    ///    Obt�m o valor de sucesso contido no resultado, lan�ando uma exce��o se for um resultado de falha.
    ///  </summary>
    ///  <returns>
    ///    O valor de sucesso, se o resultado for de sucesso.
    ///  </returns>
    ///  <exception cref="EInvalidOperation">
    ///    Lan�ada se o resultado for de falha.
    ///  </exception>
    function ValueSuccess: S;

    ///  <summary>
    ///    Obt�m o valor de falha contido no resultado, lan�ando uma exce��o se for um resultado de sucesso.
    ///  </summary>
    ///  <returns>
    ///    O valor de falha, se o resultado for de falha.
    ///  </returns>
    ///  <exception cref="EInvalidOperation">
    ///    Lan�ada se o resultado for de sucesso.
    ///  </exception>
    function ValueFailure: F;
  end;

implementation

uses
  eclbr.result.pair.exception;

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
  end;
end;

procedure TResultPair<S, F>.Dispose;
begin
  _DestroySuccess;
  _DestroyFailure;
end;

procedure TResultPair<S, F>._AllocResultPair;
begin
  System.New(FSuccess);
  System.New(FFailure);
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
  end;
end;

function TResultPair<S, F>.Failure(const AFailure: F): TResultPair<S, F>;
begin
  _AllocResultPair;
  FFailure^ := TResultPairValue<F>.Create(AFailure);
  FResultType := TResultType.rtFailure;
  Result := Self;
end;

function TResultPair<S, F>.Success(const ASuccess: S): TResultPair<S, F>;
begin
  _AllocResultPair;
  FSuccess^ := TResultPairValue<S>.Create(ASuccess);
  FResultType := TResultType.rtSuccess;
  Result := Self;
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
      FSuccess^.SetValue(LCast.AsType<S>);
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
    TResultType.rtSuccess: FSuccess^.SetValue(FSuccess^.GetValue);
    TResultType.rtFailure:
    begin
      LCast := TValue.From<R>(AFailureFunc(FFailure^.GetValue));
      FFailure^.SetValue(LCast.AsType<F>);
    end;
  end;
end;

class operator TResultPair<S, F>.NotEqual(const Left,
  Right: TResultPair<S, F>): Boolean;
begin
  Result := not (Left = Right);
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
      FSuccess^.SetValue(LCast.AsType<S>);
    end;
    TResultType.rtFailure: FFailure^.SetValue(FFailure^.GetValue);
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
    TResultType.rtSuccess: FSuccess.SetValue(FSuccess^.GetValue);
    TResultType.rtFailure:
    begin
      LCast := TValue.From<R>(AFailureFunc(FFailure^.GetValue));
      FFailure.SetValue(LCast.AsType<F>);
    end;
  end;
end;

function TResultPair<S, F>.Pure(const ASuccess: S): TResultPair<S, F>;
begin
  if Assigned(Pointer(FSuccess)) then
  begin
    _AllocResultPair;
    FSuccess^ := TResultPairValue<S>.Create(ASuccess);
    FResultType := TResultType.rtSuccess;
    Result := Self;
  end;
  FSuccess^.SetValue(ASuccess);
end;

function TResultPair<S, F>.PureFailure(const AFailure: F): TResultPair<S, F>;
begin
  if Assigned(Pointer(FSuccess)) then
  begin
    _AllocResultPair;
    FFailure^ := TResultPairValue<F>.Create(AFailure);
    FResultType := TResultType.rtFailure;
    Result := Self;
  end;
  FFailure^.SetValue(AFailure);
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
var
  LResult: TResultPair<F, S>;
  LSuccess: TValue;
  LFailure: TValue;
begin
  try
    case FResultType of
      TResultType.rtSuccess: LResult.Failure(FSuccess^.GetValue);
      TResultType.rtFailure: LResult.Success(FFailure^.GetValue);
    end;
    Result := LResult;
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
      Result.Success(LCast.AsType<R>);
    end;
  end;
end;

end.
