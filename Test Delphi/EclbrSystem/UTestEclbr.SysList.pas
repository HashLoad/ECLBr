unit UTestEclbr.SysList;

interface

uses
  DUnitX.TestFramework,
  Generics.Collections,
  eclbr.sysvector;

type
  TListTest = class
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  end;

implementation

{ TArrayDataTest }

procedure TListTest.Setup;
begin

end;

procedure TListTest.TearDown;
begin

end;


initialization
  TDUnitX.RegisterTestFixture(TListTest);

end.

