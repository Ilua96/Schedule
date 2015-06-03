unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  UMetaData, UListView, UAboutProgram, USchedule;

type

  { TMenuForm }

  TMenuForm = class(TForm)
    MainMenu: TMainMenu;
    ScheduleMenu: TMenuItem;
    ReferenceMenu: TMenuItem;
    AboutTheProgramMenu: TMenuItem;
    ExitMenu: TMenuItem;
    ModalWinControl: TWinControl;
    procedure AboutTheProgramMenuClick(Sender: TObject);
    procedure ExitMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ScheduleMenuClick(Sender: TObject);
  end;

var
  MenuForm: TMenuForm;

implementation

{$R *.lfm}

{ TMenuForm }

procedure TMenuForm.ExitMenuClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMenuForm.AboutTheProgramMenuClick(Sender: TObject);
begin
  AboutProgramForm.Show;
end;

procedure TMenuForm.FormCreate(Sender: TObject);
var
  i: Integer;
  MenuItem: array of TMenuItem;

begin
  SetLength(MenuItem, Length(Tables.TablesInf));
  SetLength(ListViewForm, Length(Tables.TablesInf));
  for i := 0 to Length(Tables.TablesInf) - 1 do
  begin
    MenuItem[i] := TMenuItem.Create(Self);
    With MenuItem[i] do
    begin
      Tag := i;
      Caption := Tables.TablesInf[i].Caption;
      OnClick := @MenuItemClick;
    end;
  end;
  ReferenceMenu.Add(MenuItem);
end;

procedure TMenuForm.MenuItemClick(Sender: TObject);
begin
  With (Sender as TMenuItem) do
  begin
    Checked := True;
    if ListViewForm[Tag] = nil then
    begin
      ListViewForm[Tag] := TListViewForm.Create(Self);
      ListViewForm[Tag].CreateAndShowForm(Tag);
      ListViewForm[Tag].OnClose := @FormClose;
    end
    else
      ListViewForm[Tag].Show;
  end;
end;

procedure TMenuForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ReferenceMenu[(Sender as TForm).Tag].Checked := False;
  FreeAndNil(ListViewForm[(Sender as TForm).Tag]);
  CloseAction := caFree;
end;

procedure TMenuForm.ScheduleMenuClick(Sender: TObject);
begin
  ScheduleForm.Show;
end;

end.

