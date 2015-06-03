unit UFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Graphics, UMetaData;

type

  TEvent = Procedure of Object;

  { TFilterPanel }

  TFilterPanel = class(TPanel)
    NameComboBox: TComboBox;
    SignComboBox: TComboBox;
    CancelButton: TButton;
    DeleteButton: TButton;
    ExecuteButton: TButton;
    ValueEdit: TEdit;
    procedure CreateFilter(ATag, ANum: Integer; AParent: TScrollBox);
    function CreateComboBox(ALeft: Integer): TCombobox;
    function CreateEdit: TEdit;
    function CreateButton(ALeft: Integer; ACaption: String): TButton;
    procedure ExecuteFilter(Sender: TObject);
    procedure ChangeValue(Sender: TObject);
    procedure CancelFilter(Sender: TObject);
  public
    AQueryEvent: TEvent;
  end;

  { TFilterListBox }

  TFilterListBox = class(TScrollBox)
    procedure AddFilterButtonClick(Sender: TObject);
    procedure CancelFiltersButtonClick(Sender: TObject);
    procedure DeleteFilter(Sender: TObject; Button: TMouseButton;
                           Shift: TShiftState; X, Y: Integer);
    procedure DeleteFiltersButtonClick(Sender: TObject);
    procedure ExecuteFiltersButtonClick(Sender: TObject);
  public
    FilterPanels: array of TFilterPanel;
    QueryEvent: TEvent;
  end;

const NumberOfSigns = 6;
const indent = 37;

implementation

{ TFilterListBox }

procedure TFilterListBox.DeleteFiltersButtonClick(Sender: TObject);
var
  i: Integer;

begin
  for i := 0 to High(FilterPanels) do
    DeleteFilter((FilterPanels[0].DeleteButton as TObject), mbLeft, [], 0, 0);
  SetLength(FilterPanels, 0);
end;

procedure TFilterListBox.ExecuteFiltersButtonClick(Sender: TObject);
var
  i: Integer;

begin
  for i := 0 to High(FilterPanels) do
    FilterPanels[i].ExecuteFilter(Sender);;
end;

procedure TFilterListBox.AddFilterButtonClick(Sender: TObject);
var
  i: Integer;

begin
  SetLength(FilterPanels, Length(FilterPanels) + 1);
  FilterPanels[High(FilterPanels)] := TFilterPanel.Create(Self);
  With FilterPanels[High(FilterPanels)] do
  begin
    CreateFilter(Self.Tag, High(FilterPanels), Self);
    DeleteButton.OnMouseUp := @DeleteFilter;
    AQueryEvent := QueryEvent;
  end;
end;

procedure TFilterListBox.CancelFiltersButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to High(FilterPanels) do
    FilterPanels[i].CancelFilter(Sender);
end;

procedure TFilterListBox.DeleteFilter(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ATag, i: Integer;

begin
  ATag := (Sender as TButton).Parent.Tag;
  With FilterPanels[ATag] do
    CancelFilter(Sender);
  FreeAndNil(FilterPanels[ATag]);
  for i := ATag to High(FilterPanels) - 1 do
  begin
    FilterPanels[i] := FilterPanels[i + 1];
    With FilterPanels[i] do
    begin
      Top := Top - indent;
      Tag := Tag - 1;
    end;
  end;
  SetLength(FilterPanels, Length(FilterPanels) - 1);
end;

{ TFilterPanel }

procedure TFilterPanel.CreateFilter(ATag, ANum: Integer; AParent: TScrollBox);
var
  i: Integer;
  Signs: array[0..NumberOfSigns] of string =
    ('<', '>', '=', '<=', '>=', 'Начинается с', 'Включает');

begin
  Visible := True;
  Height := 32;
  Width := 592;
  Top := 5 + indent * ANum;
  Left := 10;
  Tag := ANum;
  Parent := AParent;
  Color := clRed;

  NameComboBox := CreateComboBox(6);
  With NameComboBox, Tables.TablesInf[ATag] do
  begin
    for i := 0 to High(Columns) do
      if (Columns[i].Visible) or (Columns[i].ReferenceColumnCaption <> '') then
        if Columns[i].ReferenceColumnCaption <> '' then
          Items.Add(Columns[i].ReferenceColumnCaption)
        else
          Items.Add(Columns[i].Caption);
    ItemIndex := 0;
  end;

  SignComboBox := CreateComboBox(120);
  With SignComboBox do
  begin
    for i := 0 to NumberOfSigns do
      Items.Add(Signs[i]);
    ItemIndex := 0;
  end;

  ValueEdit := CreateEdit;

  ExecuteButton := CreateButton(352, 'Применить');
  ExecuteButton.OnClick := @ExecuteFilter;

  CancelButton := CreateButton(432, 'Отменить');
  CancelButton.OnClick := @CancelFilter;

  DeleteButton := CreateButton(512, 'Удалить');
end;

procedure TFilterPanel.CancelFilter(Sender: TObject);
begin
  Color := clRed;
  ExecuteButton.Enabled := True;
  AQueryEvent;
end;

procedure TFilterPanel.ChangeValue(Sender: TObject);
begin
  Color := clRed;
  ExecuteButton.Enabled := True;
end;


procedure TFilterPanel.ExecuteFilter(Sender: TObject);
begin
  ExecuteButton.Enabled := False;
  AQueryEvent;
  Color := clGreen;
end;

function TFilterPanel.CreateComboBox(ALeft: Integer): TCombobox;
begin
  Result := TComboBox.Create(Self);
  With Result do
  begin
    Visible := True;
    Left := ALeft;
    Top := 6;
    Width := 113;
    Height := 23;
    ReadOnly := True;
    Style := csDropDownList;
    Parent := Self;
    OnChange := @ChangeValue;
  end;
end;

function TFilterPanel.CreateEdit: TEdit;
begin
  Result := TEdit.Create(Self);
  With Result do
  begin
    Visible := True;
    Left := 240;
    Top := 6;
    Width := 103;
    Height := 23;
    Parent := Self;
    OnChange := @ChangeValue;
  end;
end;

function TFilterPanel.CreateButton(ALeft: Integer; ACaption: String): TButton;
begin
  Result := TButton.Create(Self);
  With Result do
  begin
    Visible := True;
    Left := ALeft;
    Top := 6;
    Width := 75;
    Height := 25;
    Caption := ACaption;
    Parent := Self;
  end;
end;

end.

