unit UAboutProgram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TAboutProgramForm }

  TAboutProgramForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutProgramForm: TAboutProgramForm;

implementation

{$R *.lfm}

end.

