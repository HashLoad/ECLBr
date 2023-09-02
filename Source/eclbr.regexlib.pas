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

unit eclbr.regexlib;

interface

uses
  RegularExpressions;

type
  TMatchLib = RegularExpressions.TMatch;
  TOptionLib = RegularExpressions.TRegExOption;
  TOptionsLib = RegularExpressions.TRegExOptions;
  TGroupLib = RegularExpressions.TGroupCollection;

  TRegExLib = class
  public
    class function IsMatch(const AInput: string; const APattern: string): boolean; overload; static;
    class function IsMatch(const AInput: string; const APattern: string; const AOptions: TOptionsLib): boolean; overload; static;
    class function MatchExpression(const AValue: string; const AExpression: string): TMatchLib; static;
    class function Replace(const AInput, APattern: string; AEvaluator: TMatchEvaluator): string; overload; static;
    class function Replace(const AInput, APattern, AReplacement: string; AOptions: TOptionsLib): string; overload; static;
    class function Replace(const AInput, APattern: string; AEvaluator: TMatchEvaluator; AOptions: TOptionsLib): string; overload; static;
    class function Replace(const AInput, APattern, AReplacement: string): string; overload; static;
    //
    class function IsValidEmail(const AEmail: string): boolean;
    class function IsUUID(const AUUID: string): boolean;
    class function IsIPV4(const AIPV4: string): boolean;
  end;

implementation

{ TInfraRegEx }

class function TRegExLib.IsMatch(const AInput, APattern: string): boolean;
begin
  Result := TRegEx.IsMatch(AInput, APattern, [roIgnoreCase]);
end;

class function TRegExLib.IsIPV4(const AIPV4: string): boolean;
begin
  Result := TRegEx.IsMatch(AIPV4, '^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$');
end;

class function TRegExLib.IsMatch(const AInput, APattern: string;
  const AOptions: TOptionsLib): boolean;
begin
  Result := TRegEx.IsMatch(AInput, APattern, AOptions);
end;

class function TRegExLib.IsUUID(const AUUID: string): boolean;
begin
  Result := TRegEx.IsMatch(AUUID, '^(\{)?[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}(\})?$');
end;

class function TRegExLib.IsValidEmail(const AEmail: string): boolean;
begin
  Result := TRegEx.IsMatch(AEmail, '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$');
end;

class function TRegExLib.MatchExpression(const AValue,
  AExpression: string): TMatchLib;
var
  LRegEx: TRegEx;
  LMatch: TMatch;
begin
  LRegEx := TRegEx.Create(AExpression, [roIgnoreCase]);
  LMatch := LRegEx.Match(AValue);
  Result := LMatch;
end;

class function TRegExLib.Replace(const AInput, APattern: string;
  AEvaluator: TMatchEvaluator): string;
begin
   Result := TRegEx.Replace(AInput, APattern, AEvaluator);
end;

class function TRegExLib.Replace(const AInput, APattern, AReplacement: string;
  AOptions: TOptionsLib): string;
begin
   Result := TRegEx.Replace(AInput, APattern, AReplacement, AOptions);
end;

class function TRegExLib.Replace(const AInput, APattern: string;
  AEvaluator: TMatchEvaluator; AOptions: TOptionsLib): string;
begin
   Result := TRegEx.Replace(AInput, APattern, AEvaluator, AOptions);
end;

class function TRegExLib.Replace(const AInput, APattern,
  AReplacement: string): string;
begin
   Result := TRegEx.Replace(AInput, APattern, AReplacement);
end;

end.
