unit UTestEclbr.Str;

interface

uses
  DUnitX.TestFramework,
  SysUtils,
  DateUtils,
  Classes,
  Generics.Collections,
  eclbr.str;

type
  [TestFixture]
  TTesTStd = class
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  end;

implementation

procedure TTesTStd.Setup;
begin

end;

procedure TTesTStd.TearDown;
begin

end;

initialization
  TDUnitX.RegisterTestFixture(TTesTStd);

end.

