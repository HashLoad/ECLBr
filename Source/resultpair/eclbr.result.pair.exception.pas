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

unit eclbr.result.pair.exception;

interface

uses
  Rtti,
  SysUtils;

type
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

implementation

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
