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

unit eclbr.regexlib;

interface

uses
  SysUtils,
  RegularExpressions;

type
  TMatchLib = RegularExpressions.TMatch;
  TOptionLib = RegularExpressions.TRegExOption;
  TOptionsLib = RegularExpressions.TRegExOptions;
  TGroupLib = RegularExpressions.TGroupCollection;

  TRegExLib = class
  public
    /// <summary>
    ///   Determines whether the specified input String matches the specified regular expression pattern.
    /// </summary>
    /// <param name="AInput">The input String to match.</param>
    /// <param name="APattern">The regular expression pattern to match against.</param>
    /// <returns>True if the input String matches the pattern; otherwise, False.</returns>
    class function IsMatch(const AInput: String; const APattern: String): Boolean; overload; static;

    /// <summary>
    ///   Determines whether the specified input String matches the specified regular expression pattern with the specified options.
    /// </summary>
    /// <param name="AInput">The input String to match.</param>
    /// <param name="APattern">The regular expression pattern to match against.</param>
    /// <param name="AOptions">Additional options for matching.</param>
    /// <returns>True if the input String matches the pattern; otherwise, False.</returns>
    class function IsMatch(const AInput: String; const APattern: String; const AOptions: TOptionsLib): Boolean; overload; static;

    /// <summary>
    ///   Searches the specified input String for a match to the specified regular expression pattern and returns a match result.
    /// </summary>
    /// <param name="AValue">The input String to search.</param>
    /// <param name="AExpression">The regular expression pattern to search for.</param>
    /// <returns>A match result object containing information about the match.</returns>
    class function MatchExpression(const AValue: String; const AExpression: String): TMatchLib; static;

    /// <summary>
    ///   Replaces all occurrences of the specified pattern in the input String with the result of a specified evaluator function.
    /// </summary>
    /// <param name="AInput">The input String to search.</param>
    /// <param name="APattern">The regular expression pattern to search for.</param>
    /// <param name="AEvaluator">A function that provides replacement values for matched patterns.</param>
    /// <returns>The input String with all matched patterns replaced by the values provided by the evaluator function.</returns>
    class function Replace(const AInput, APattern: String; AEvaluator: TMatchEvaluator): String; overload; static;

    /// <summary>
    ///   Replaces all occurrences of the specified pattern in the input String with a specified replacement String using the specified options.
    /// </summary>
    /// <param name="AInput">The input String to search.</param>
    /// <param name="APattern">The regular expression pattern to search for.</param>
    /// <param name="AReplacement">The replacement String to use for matched patterns.</param>
    /// <param name="AOptions">Additional options for the replacement operation.</param>
    /// <returns>The input String with all matched patterns replaced by the specified replacement String.</returns>
    class function Replace(const AInput, APattern, AReplacement: String; AOptions: TOptionsLib): String; overload; static;

    /// <summary>
    ///   Replaces all occurrences of the specified pattern in the input String with the result of a specified evaluator function using the specified options.
    /// </summary>
    /// <param name="AInput">The input String to search.</param>
    /// <param name="APattern">The regular expression pattern to search for.</param>
    /// <param name="AEvaluator">A function that provides replacement values for matched patterns.</param>
    /// <param name="AOptions">Additional options for the replacement operation.</param>
    /// <returns>The input String with all matched patterns replaced by the values provided by the evaluator function.</returns>
    class function Replace(const AInput, APattern: String; AEvaluator: TMatchEvaluator; AOptions: TOptionsLib): String; overload; static;

    /// <summary>
    ///   Replaces all occurrences of the specified pattern in the input String with a specified replacement String.
    /// </summary>
    /// <param name="AInput">The input String to search.</param>
    /// <param name="APattern">The regular expression pattern to search for.</param>
    /// <param name="AReplacement">The replacement String to use for matched patterns.</param>
    /// <returns>The input String with all matched patterns replaced by the specified replacement String.</returns>
    class function Replace(const AInput, APattern, AReplacement: String): String; overload; static;

    /// <summary>
    ///   Determines whether the specified String is a valid email address.
    /// </summary>
    /// <param name="AEmail">The String to check for valid email format.</param>
    /// <returns>True if the String is a valid email address; otherwise, False.</returns>
    class function IsMatchValidEmail(const AEmail: String): Boolean;

    /// <summary>
    ///   Determines whether the specified String is a valid UUID (Universally Unique Identifier).
    /// </summary>
    /// <param name="AUUID">The String to check for valid UUID format.</param>
    /// <returns>True if the String is a valid UUID; otherwise, False.</returns>
    class function IsMatchUUID(const AUUID: String): Boolean;

    /// <summary>
    ///   Determines whether the specified String is a valid IPv4 address.
    /// </summary>
    /// <param name="AIPV4">The String to check for valid IPv4 format.</param>
    /// <returns>True if the String is a valid IPv4 address; otherwise, False.</returns>
    class function IsMatchIPV4(const AIPV4: String): Boolean;

    class function IsMatchCEP(const ACEP: String): Boolean;
    class function IsMatchCPF(const ACPF: String): Boolean;
    class function IsMatchCNPJ(const ACNPJ: String): Boolean;
    class function IsMatchDDDPhone(const APhone: String): Boolean;
    class function IsMatchPlacaMercosul(const APlaca: String): Boolean;
    class function IsMatchPlaca(const APlaca: String): Boolean;
    class function IsMatchData(const ADate: String): Boolean;
    class function IsMatchCredCard(const ANumber: String): Boolean;
    class function IsMatchURL(const AURL: String): Boolean;
  end;

implementation

{ TInfraRegEx }

class function TRegExLib.IsMatch(const AInput, APattern: String): Boolean;
begin
  Result := TRegEx.IsMatch(AInput, APattern, [roIgnoreCase]);
end;

class function TRegExLib.IsMatchCredCard(const ANumber: String): Boolean;
begin
  Result := TRegEx.IsMatch(ANumber, '^(0[1-9]|[12][0-9]|3[01])/(0[1-9]|1[0-2])/\d{4}$');
end;

class function TRegExLib.IsMatchCEP(const ACEP: String): Boolean;
begin
  Result := TRegEx.IsMatch(ACEP, '^\d{8}$');
end;

class function TRegExLib.IsMatchCNPJ(const ACNPJ: String): Boolean;
begin
  Result := TRegEx.IsMatch(ACNPJ, '^\d{2}\.\d{3}\.\d{3}/\d{4}-\d{2}$');
end;

class function TRegExLib.IsMatchCPF(const ACPF: String): Boolean;
begin
  Result := TRegEx.IsMatch(ACPF, '^\d{3}\.\d{3}\.\d{3}-\d{2}$');
end;

class function TRegExLib.IsMatchData(const ADate: String): Boolean;
begin
  Result := TRegEx.IsMatch(ADate, '^(0[1-9]|[12][0-9]|3[01])/(0[1-9]|1[0-2])/\d{4}$');
end;

class function TRegExLib.IsMatchDDDPhone(const APhone: String): Boolean;
begin
  Result := TRegEx.IsMatch(APhone, '^\(\d{2}\) 9\d{4}-\d{4}$|^\(\d{2}\) [2-5]\d{3}-\d{4}$');
end;

class function TRegExLib.IsMatchIPV4(const AIPV4: String): Boolean;
begin
  Result := TRegEx.IsMatch(AIPV4, '^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$');
end;

class function TRegExLib.IsMatch(const AInput, APattern: String;
  const AOptions: TOptionsLib): Boolean;
begin
  Result := TRegEx.IsMatch(AInput, APattern, AOptions);
end;

class function TRegExLib.IsMatchPlaca(const APlaca: String): Boolean;
begin
  Result := TRegEx.IsMatch(APlaca, '^[A-Z]{2,3}-\d{4}$');
end;

class function TRegExLib.IsMatchPlacaMercosul(const APlaca: String): Boolean;
begin
  Result := TRegEx.IsMatch(APlaca, '^[A-Z]{3}\d{1}[A-Z]\d{2}$|^[A-Z]{2}\d{2}[A-Z]\d{1}$');
end;

class function TRegExLib.IsMatchURL(const AURL: String): Boolean;
begin
  Result := TRegEx.IsMatch(AURL, '^(https?|ftp)://[-A-Z0-9+&@#/%?=~_|!:,.;]*[-A-Z0-9+&@#/%=~_|]', [roIgnoreCase]);
end;

class function TRegExLib.IsMatchUUID(const AUUID: String): Boolean;
begin
  Result := TRegEx.IsMatch(AUUID, '^(\{)?[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}(\})?$');
end;

class function TRegExLib.IsMatchValidEmail(const AEmail: String): Boolean;
begin
  Result := TRegEx.IsMatch(AEmail, '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$');
end;

class function TRegExLib.MatchExpression(const AValue,
  AExpression: String): TMatchLib;
var
  LRegEx: TRegEx;
  LMatch: TMatch;
begin
  LRegEx := TRegEx.Create(AExpression, [roIgnoreCase]);
  LMatch := LRegEx.Match(AValue);
  Result := LMatch;
end;

class function TRegExLib.Replace(const AInput, APattern: String;
  AEvaluator: TMatchEvaluator): String;
begin
   Result := TRegEx.Replace(AInput, APattern, AEvaluator);
end;

class function TRegExLib.Replace(const AInput, APattern, AReplacement: String;
  AOptions: TOptionsLib): String;
begin
   Result := TRegEx.Replace(AInput, APattern, AReplacement, AOptions);
end;

class function TRegExLib.Replace(const AInput, APattern: String;
  AEvaluator: TMatchEvaluator; AOptions: TOptionsLib): String;
begin
   Result := TRegEx.Replace(AInput, APattern, AEvaluator, AOptions);
end;

class function TRegExLib.Replace(const AInput, APattern,
  AReplacement: String): String;
begin
   Result := TRegEx.Replace(AInput, APattern, AReplacement);
end;

end.
