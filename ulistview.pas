unit UListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, DbCtrls, Menus, StdCtrls, ExtCtrls, UMetaData, USQLRequest;

type

  TFilterPanel = class(TPanel)
    NameComboBox: TComboBox;
    SignComboBox: TComboBox;
    DeleteButton: TButton;
    ExecuteButton: TButton;
    ValueEdit: TEdit;
    function CreateComboBox(ALeft: Integer): TCombobox;
    function CreateEdit: TEdit;
    function CreateButton(ALeft: Integer; ACaption: String): TButton;
    procedure ExecuteFilter(Sender: TObject);
    procedure ChangeValue(Sender: TObject);
  private
    ActiveQuery: String;
  end;

  { TListViewForm }

  TListViewForm = class(TForm)
    AddFilterButton: TButton;
    DataSource: TDataSource;
    DBNavigator: TDBNavigator;
    SQLQuery: TSQLQuery;
    DBGrid: TDBGrid;
    procedure AddFilterButtonClick(Sender: TObject);
    procedure ChangeColumns(ATag: Integer);
    procedure DBGridTitleClick(Column: TColumn);
    procedure DeleteFilter(Sender: TObject; Button: TMouseButton;
                           Shift: TShiftState; X, Y: Integer);
    constructor CreateAndShowForm(ATag: Integer);
  private
    FilterPanels: array of TFilterPanel;
  end;



Const indent = 37;
Const NumberOfSigns = 6;

var
  ListViewForm: array of TListViewForm;

implementation

{$R *.lfm}

{ TListViewForm }

constructor TListViewForm.CreateAndShowForm(ATag: Integer);
begin
  if ListViewForm[ATag] = nil then
  begin
    ListViewForm[ATag] := TListViewForm.Create(Self);
    With ListViewForm[ATag] do
    begin
      Tag := ATag;
      Caption := Tables.TablesInf[ATag].Caption;
      Name := Tables.TablesInf[ATag].Name;
      With SQLQuery do
      begin
        Active := False;
        SQL.Text := SQLRequest.WriteQuery(ATag);
        Active := True;
      end;
      ChangeColumns(ATag);
    end;
  end;
  ListViewForm[ATag].Show;
end;

procedure TListViewForm.ChangeColumns(ATag: Integer);
var
  i: Integer = 0;
  j: Integer = 0;

begin
  With Tables.TablesInf[ATag] do
    While i <= High(Columns) do
    begin
      DBGrid.Columns[j].Visible := Columns[i].Visible;
      DBGrid.Columns[j].FieldName := Columns[i].Name;
      DBGrid.Columns[j].Title.Caption := Columns[i].Caption;
      DBGrid.Columns[j].Width := Columns[i].Width;
      DBGrid.Columns[j].Tag := i;
      if Columns[j].ReferenceTableName <> '' then
      begin
        inc(j);
        DBGrid.Columns[j].FieldName := Columns[i].ReferenceColumnSName;
        DBGrid.Columns[j].Title.Caption := Columns[i].ReferenceColumnCaption;
        DBGrid.Columns[j].Width := Columns[i].ReferenceColumnWidth;
        DBGrid.Columns[j].Tag := i;
      end;
      inc(i);
      inc(j);
    end;
end;

procedure TListViewForm.DBGridTitleClick(Column: TColumn);
var
  AName: String;

begin
  With Tables.TablesInf[Tag] do
  begin
    if Columns[Column.Tag].ReferenceTableName <> '' then
      AName := Columns[Column.Tag].ReferenceTableName + '.' + Column.FieldName
    else
      AName := Name + '.' + Column.FieldName;
  end;
  With SQLQuery do
  begin
    Active := False;
    SQL.Text := SQLRequest.SortField(SQL.Text, AName);
    Active := True;
  end;
  ChangeColumns(Tag);
end;

procedure TListViewForm.AddFilterButtonClick(Sender: TObject);
var
  i: Integer;
  Signs: array[0..NumberOfSigns] of string = ('<', '>', '=', '<=', '>=',
                                              'Начинается с', 'Включает');

begin
  if Length(FilterPanels) = 15 then
    exit;
  SetLength(FilterPanels, Length(FilterPanels) + 1);
  FilterPanels[High(FilterPanels)] := TFilterPanel.Create(Self);
  With FilterPanels[High(FilterPanels)] do
  begin
    Visible := True;
    Height := 32;
    Width := 512;
    Top := 44 + indent * High(FilterPanels);
    Left := AddFilterButton.Left;
    Tag := High(FilterPanels);
    Parent := Self;
    Anchors := [akTop,akRight];
    Color := clRed;

    NameComboBox := CreateComboBox(6);
    With NameComboBox do
    begin
      for i := 0 to DBGrid.Columns.Count - 1 do
        if DBGrid.Columns[i].Visible then
          Items.Add(DBGrid.Columns[i].Title.Caption);
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

    ExecuteButton := CreateButton(352, 'Выполнить');
    ExecuteButton.OnClick := @ExecuteFilter;

    DeleteButton := CreateButton(432, 'Удалить');
    DeleteButton.OnMouseUp := @DeleteFilter;
  end;
end;

procedure TListViewForm.DeleteFilter(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ATag, i: Integer;

begin
  ATag := (Sender as TButton).Parent.Tag;
  With FilterPanels[ATag] do
    if ActiveQuery <> '' then
    begin
      With SQLQuery do
      begin
        Active := False;
        SQL.Text := SQLRequest.DeleteFilter(SQL.Text, ActiveQuery);
        Active := True;
      end;
    end;
  ChangeColumns(Tag);
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
  if ATag <= High(FilterPanels) - 1 then
    With FilterPanels[0] do
      if Pos('And', ActiveQuery) = 1 then
      begin
        Insert('Where', ActiveQuery, Pos('And', ActiveQuery));
        Delete(ActiveQuery, Pos('And', ActiveQuery), Length('And'));
      end;
  SetLength(FilterPanels, Length(FilterPanels) - 1);
end;

{ TFilterPanel }

procedure TFilterPanel.ChangeValue(Sender: TObject);
begin
  Color := clRed;
  ExecuteButton.Enabled := True;
end;


procedure TFilterPanel.ExecuteFilter(Sender: TObject);
var
  i: Integer;
  AName: String;
  Signs: array[0..NumberOfSigns] of string = ('< :', '> :', '= :', '<= :', '>= :',
                                              'like :', 'like :');

begin
  if Color = clGreen then
    exit;
  Color := clYellow;
  ExecuteButton.Enabled := False;
  With ListViewForm[Parent.Tag] do
  begin
    for i := 0 to DBGrid.Columns.Count - 1 do
      if DBGrid.Columns[i].Title.Caption = NameComboBox.Caption then
        With Tables.TablesInf[Tag] do
          if Columns[DBGrid.Columns[i].Tag].ReferenceTableName <> '' then
            AName := Columns[DBGrid.Columns[i].Tag].ReferenceTableName + '.' +
              Columns[DBGrid.Columns[i].Tag].ReferenceColumnSName
          else
            AName := Name + '.' + Columns[DBGrid.Columns[i].Tag].Name;
    With SQLQuery do
    begin
      Active := False;
      Prepare;
      SQL.Text := SQLRequest.AddFilter(SQL.Text, AName,
                    Signs[SignComboBox.ItemIndex], IntToStr(Params.Count),
                    ActiveQuery);
      if SignComboBox.ItemIndex = 5 then
        Params[Params.Count - 1].AsString :=  ValueEdit.Text + '%'
      else
        if SignComboBox.ItemIndex = 6 then
          Params[Params.Count - 1].AsString := '%' + ValueEdit.Text + '%'
        else
          Params[Params.Count - 1].AsString := ValueEdit.Text;
      Active := True;
    end;
    ChangeColumns(Tag);
  end;
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

