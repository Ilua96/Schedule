unit USchedule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, CheckLst, ExtCtrls, UMetaData, sqldb, db, USQLRequest,
  UListView, UFilter, UEdit;

type

  { TScheduleForm }

  TScheduleForm = class(TForm)
    AddFilterButton: TButton;
    CancelFiltersButton: TButton;
    ExecuteFiltersButton: TButton;
    DeleteFiltersButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    TitleCheckBox: TCheckBox;
    CheckListBox: TCheckListBox;
    DataSource: TDataSource;
    ExecuteButton: TButton;
    SQLQuery: TSQLQuery;
    XComboBox: TComboBox;
    YComboBox: TComboBox;
    DrawGrid: TDrawGrid;
    EditIcon: TIcon;
    AddIcon: TIcon;
    DelIcon: TIcon;
    procedure CheckListBoxItemClick(Sender: TObject; Index: integer);
    procedure DrawGridDblClick(Sender: TObject);
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ExecuteButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MakeQueryInf;
    procedure MakeQueryColOrRow(AText: String);
    procedure FillCols;
    procedure FillRows;
    procedure FreeGrid;
    procedure FillFields;
    procedure RefreshGrid;
    procedure CreateIcon;
    procedure ShowEdit(ACol, ARow: Integer);
    procedure ShowAdd(ACol, ARow: Integer);
    procedure ShowDel(ACol, ARow, ATag: Integer);
    procedure DragAndDrop(AColL, ARowL, ACol, ARow: Integer);
    procedure CreateAddAndDel;
    procedure CloseAddAndDel(Sender: TObject; var CloseAction: TCloseAction);
  private
    ACols, ARows: array of String;
    ArrayRecID, AFieldsList: array of array of TStringList;
    FilterlistBox: TFilterListBox;
    Edits: array of TListViewForm;
    AddAndDel: array of TEditForm;
    SelectedCol, SelectedRow, aX, aY, HeighRecord, DragRecord: Integer;
    EditRect, AddRect: TRect;
    DelRect: array of TRect;
  end;

const indent = 20;

var
  ScheduleForm: TScheduleForm;

implementation

{$R *.lfm}

{ TScheduleForm }

procedure TScheduleForm.ExecuteButtonClick(Sender: TObject);
var
  i: Integer;

begin
  FreeGrid;
  for i := 0 to CheckListBox.Count - 1 do
    if CheckListBox.Checked[i] then
      HeighRecord += DrawGrid.Canvas.TextHeight('I');
  DrawGrid.DefaultRowHeight := HeighRecord;
  MakeQueryColOrRow(XComboBox.Text);
  FillCols;
  MakeQueryColOrRow(YComboBox.Text);
  FillRows;
  MakeQueryInf;
  FillFields;
  CreateIcon;
  DrawGrid.Visible := True;
end;

procedure TScheduleForm.CreateIcon;
begin
  EditIcon := TIcon.Create;
  AddIcon := TIcon.Create;
  DelIcon := TIcon.Create;
  Editicon.LoadFromFile('Icons\Edit.ico');
  AddIcon.LoadFromFile('Icons\Add.ico');
  DelIcon.LoadFromFile('Icons\Del.ico');
end;

procedure TScheduleForm.FormCreate(Sender: TObject);
var
  i: Integer;

begin
  With Tables.TablesInf[Tag] do
    for i := 1 to High(Columns) do
    begin
      XComboBox.Items.Add(Columns[i].ReferenceColumnCaption);
      YComboBox.Items.Add(Columns[i].ReferenceColumnCaption);
      CheckListBox.Items.Add(Columns[i].ReferenceColumnCaption);
      CheckListBox.Checked[i - 1] := True;
    end;
  XComboBox.ItemIndex := 0;
  YComboBox.ItemIndex := 0;
  FilterlistBox := TFilterListBox.Create(Self);
  With FilterlistBox do
  begin
    Width := 504;
    Height := 137;
    Left := 408;
    Top := 440;
    Anchors := [akTop,akLeft,akRight,akBottom];
    Tag := Self.Tag;
    Parent := Self;
    AddFilterButton.OnClick := @AddFilterButtonClick;
    ExecuteFiltersButton.OnClick := @ExecuteFiltersButtonClick;
    DeleteFiltersButton.OnClick := @DeleteFiltersButtonClick;
    CancelFiltersButton.OnClick := @CancelFiltersButtonClick;
    QueryEvent := @RefreshGrid;
  end;
end;

procedure TScheduleForm.RefreshGrid;
begin
  ExecuteButtonClick(Self);
end;

procedure TScheduleForm.MakeQueryInf;
begin
  With SQLQuery do
  begin
    Close;
    SQL.Text := SQLRequest.CreateQuery(Self.Tag);
    SQLRequest.WriteFilters(SQLQuery, FilterlistBox);
    Open;
  end;
end;

procedure TScheduleForm.MakeQueryColOrRow(AText: String);
var
  i: Integer;

begin
  With Tables.TablesInf[Tag] do
    for i := 0 to High(Columns) do
      if Columns[i].ReferenceColumnCaption = AText then
        With SQLQuery do
        begin
          Close;
          SQL.Text := 'Select ' + Columns[i].ReferenceTableName + '.' +
            Columns[i].ReferenceColumnSName + ' From ' +
            Columns[i].ReferenceTableName;
          Open;
          exit;
        end;
end;

procedure TScheduleForm.FillCols;
var
  i: Integer;

begin
  With DrawGrid do
  begin
    SetLength(ACols, DataSource.DataSet.RecordCount);
    for i := 0 to DataSource.DataSet.RecordCount - 1 do
    begin
      ColCount := ColCount + 1;
      ACols[i] := DataSource.DataSet.Fields[0].Value;
      SQLQuery.Next;
    end;
  end;
end;

procedure TScheduleForm.FillRows;
var
  i: Integer;

begin
  With DrawGrid do
  begin
    SetLength(ARows, DataSource.DataSet.RecordCount);
    for i := 0 to DataSource.DataSet.RecordCount - 1 do
    begin
      RowCount := RowCount + 1;
      ARows[i] := DataSource.DataSet.Fields[0].Value;
      SQLQuery.Next;
    end;
  end;
end;

procedure TScheduleForm.FillFields;
var
  i, j, k, l: Integer;

begin
  SetLength(AFieldsList, Length(ACols));
  SetLength(ArrayRecID, Length(ACols));
  for i := 0 to High(ACols) do
  begin
    SetLength(ArrayRecID[i], Length(ARows));
    SetLength(AFieldsList[i], Length(ARows));
  end;
  for j := 0 to High(ACols) do
    for k := 0 to High(ARows) do
    begin
      ArrayRecID[j][k] := TStringList.Create;
      AFieldsList[j][k] := TStringList.Create;
    end;
  With SQLQuery do
    for i := 0 to RecordCount - 1 do
    begin
      for j := 0 to High(ACols) do
      begin
        for k := 0 to High(ARows) do
          if (ACols[j] = Fields[XComboBox.ItemIndex * 2 + 2].Value) and
            (ARows[k] = Fields[YComboBox.ItemIndex * 2 + 2].Value) then
          begin
            ArrayRecID[j][k].Append(IntToStr(Fields[0].Value));
            With Tables.TablesInf[Self.Tag] do
              for l := 0 to High(Columns) - 1 do
                if CheckListBox.Checked[l] then
                  if TitleCheckBox.Checked then
                    AFieldsList[j][k].Append(Columns[l + 1].ReferenceColumnCaption + ':' + Fields[l * 2 + 2].Value)
                  else
                    AFieldsList[j][k].Append(Fields[l * 2 + 2].Value);
            AFieldsList[j][k].Append('');
          end;
      end;
      Next;
    end;
end;

procedure TScheduleForm.FreeGrid;
begin
  SetLength(ARows, 0);
  SetLength(ACols, 0);
  SetLength(AFieldsList, 0);
  SetLength(ArrayRecID, 0);
  With DrawGrid do
  begin
    ColCount := 1;
    RowCount := 1;
  end;
  HeighRecord := 0;
end;

procedure TScheduleForm.DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  i, DelCount: Integer;
  function SaveRect(X, Y: Integer): TRect;
  begin
    Result.TopLeft := Point(X, Y);
    Result.BottomRight := Point(X + 20, Y + 20);
  end;

begin
  With DrawGrid do
  begin
    if (aRow = 0) and (aCol <> 0) then
      Canvas.TextOut(aRect.Left, aRect.Top, ACols[aCol - 1]);
    if (aCol = 0) and (aRow <> 0) then
      Canvas.TextOut(aRect.Left, aRect.Top, ARows[aRow - 1]);
    if (aCol <> 0) and (aRow <> 0) then
    begin
      for i := 0 to AFieldsList[aCol - 1][aRow - 1].Count - 1 do
      begin
        Canvas.TextOut(aRect.Left, aRect.Top + i * Canvas.TextHeight('I'),
          AFieldsList[aCol - 1][aRow - 1].ValueFromIndex[i]);
      end;
      if (aCol = SelectedCol) and (aRow = SelectedRow) then
      begin
        Canvas.Draw(aRect.Right - indent, aRect.Top, EditIcon);
        EditRect := SaveRect(aRect.Right - indent ,ARect.Top);
        Canvas.Draw(aRect.Right - indent, aRect.Top + indent, AddIcon);
        AddRect := SaveRect(aRect.Right - indent, ARect.Top + indent);
        DelCount := (AFieldsList[aCol - 1][aRow - 1].Count - 1) div
          (HeighRecord div Canvas.TextHeight('I'));
        SetLength(DelRect, DelCount + 1);
        if AFieldsList[aCol - 1][aRow - 1].Count <> 0 then
          for i := 0 to DelCount do
          begin
            Canvas.Draw(aRect.Right - indent,
              aRect.Top + indent * 2 + HeighRecord * i + Canvas.TextHeight('I') * i, DelIcon);
            DelRect[i] := SaveRect(aRect.Right - indent,
              aRect.Top + indent * 2 + HeighRecord * i + Canvas.TextHeight('I') * i);
          end;
      end;
    end;
  end;
end;

procedure TScheduleForm.CheckListBoxItemClick(Sender: TObject; Index: integer);
begin
  ExecuteButtonClick(Self);
end;

procedure TScheduleForm.DrawGridDblClick(Sender: TObject);
var
  aCol, aRow: Integer;

begin
  With DrawGrid do
  begin
    MouseToCell(aX, aY, aCol, aRow);
    if RowHeights[aRow] <> 105 then
      RowHeights[aRow] := 105
    else
      RowHeights[aRow] := (AFieldsList[aCol][arow].Count - 1) * Canvas.TextHeight('I');
  end;
end;

procedure TScheduleForm.DrawGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aCol, aRow, i: Integer;

begin
  aX := X;
  aY := Y;
  DrawGrid.MouseToCell(X, Y, aCol, aRow);
  for i := 0 to ArrayRecID[aCol - 1][aRow - 1].Count - 2 do
    if (Y - aRow * HeighRecord > i * HeighRecord) and
       (Y - aRow * HeighRecord < (i + 1) * HeighRecord) then
      DragRecord := i;
end;

procedure TScheduleForm.DrawGridMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  aCol, aRow, i: Integer;
  indent: String = #10;
  AHint: String;

begin
  With DrawGrid do
  begin
    MouseToCell(X, Y, aCol, aRow);
    if (aCol = 0) or (aRow = 0) then
      Hint := ''
    else
    begin
      Hint := '';
      With AFieldsList[aCol - 1][aRow - 1] do
        for i := 0 to Count - 1 do
          AHint := AHint + Strings[i] + indent;
      Delete(AHint, Length(AHint) - Length(indent) + 1, Length(indent));
      Hint := AHint;
    end;
  end;
end;

procedure TScheduleForm.DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow, LastCol, LastRow, i: Integer;

begin
  DrawGrid.MouseToCell(X, Y, ACol, ARow);
  DrawGrid.MouseToCell(aX, aY, LastCol, LastRow);
  SelectedCol := ACol;
  SelectedRow := ARow;
  if (EditRect.Left < X) and (EditRect.Right > X) and
     (EditRect.Top < Y) and (EditRect.Bottom > Y) then
  begin
    ShowEdit(ACol, ARow);
    exit;
  end;
  if (AddRect.Left < X) and (AddRect.Right > X) and
     (AddRect.Top < Y) and (AddRect.Bottom > Y) then
  begin
    ShowAdd(ACol, ARow);
    exit;
  end;
  for i := 0 to High(DelRect) do
    if (DelRect[i].Left < X) and (DelRect[i].Right > X) and
       (DelRect[i].Top < Y) and (DelRect[i].Bottom > Y) then
    begin
      ShowDel(ACol, ARow, i);
      exit;
    end;
  if (LastCol <> 0) and (LastRow <> 0) and ((LastRow <> ARow) or (LastCol <> ACol)) then
  begin
    if (XComboBox.Text = YComboBox.Text) and (ACols[ACol] <> ARows[ARow]) then
      exit;
    DragAndDrop(LastCol, LastRow, ACol, ARow);
  end;
end;

procedure TScheduleForm.ShowEdit(ACol, ARow: Integer);
begin
  SetLength(Edits, Length(Edits) + 1);
  Edits[High(Edits)] := TListViewForm.Create(Self);
  With Edits[High(Edits)] do
  begin
    CreateAndShowForm(Self.Tag);
    ImportFilters(XComboBox, YComboBox, FilterlistBox, ACols[ACol- 1], ARows[ARow - 1]);
  end;
end;

procedure TScheduleForm.CreateAddAndDel;
begin
  SetLength(AddAndDel, Length(AddAndDel) + 1);
  AddAndDel[High(AddAndDel)] := TEditForm.Create(Self);
  AddAndDel[High(AddAndDel)].OnClose := @CloseAddAndDel;
end;

procedure TScheduleForm.CloseAddAndDel(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;

begin
  for i := (Sender as TForm).Tag to High(AddAndDel) - 1 do
    AddAndDel[i] := AddAndDel[i + 1];
  SetLength(AddAndDel, Length(AddAndDel) - 1);
  RefreshGrid;
end;

procedure TScheduleForm.ShowAdd(ACol, ARow: Integer);
var
  i, ARecID: Integer;
  AStringList: TStringList;

begin
  CreateAddAndDel;
  AStringList := TStringList.Create;
  for i := 0 to CheckListBox.Count - 1 do
    if XComboBox.Text = CheckListBox.Items[i] then
      AStringList.Append(ACols[ACol - 1])
    else
      if YComboBox.Text = CheckListBox.Items[i] then
        AStringList.Append(ARows[ARow - 1])
      else
        AStringList.Append('');
  AddAndDel[High(AddAndDel)].ShowForm(Tag, ARecID, AStringList, True).Show;
end;

procedure TScheduleForm.ShowDel(ACol, ARow, ATag: Integer);
var
  i, ARecID, AHeighRecord: Integer;
  s: String;
  AStringList: TStringList;

begin
  CreateAddAndDel;
  AStringList := TStringList.Create;
  AHeighRecord := HeighRecord div DrawGrid.Canvas.TextHeight('I');
  for i := 0 to CheckListBox.Count - 1 do
  begin
    s := AFieldsList[ACol - 1][ARow - 1].Strings[i + ATag * (AHeighRecord + 1)];
    if Pos(':', s) <> 0 then
      Delete(s, 1, Length(s) - (Length(s) - Pos(':', s)));
    AStringList.Append(s);
  end;
  ARecID := StrToInt(ArrayRecID[ACol - 1][ARow - 1].Strings[ATag]);
  AddAndDel[High(AddAndDel)].ShowForm(Tag, ARecID, AStringList, False).Show;
end;

procedure TScheduleForm.DragAndDrop(AColL, ARowL, ACol, ARow: Integer);
var
  AStringList: TStringList;
  AHeighRecord, i, ARecID: Integer;
  s: String;

begin
  AStringList := TStringList.Create;
  AHeighRecord := HeighRecord div DrawGrid.Canvas.TextHeight('I');
  for i := 0 to CheckListBox.Count - 1 do
    if XComboBox.Text = CheckListBox.Items[i] then
      AStringList.Append(ACols[ACol - 1])
    else
      if YComboBox.Text = CheckListBox.Items[i] then
        AStringList.Append(ARows[ARow - 1])
      else
      begin
        s := AFieldsList[AColL - 1][ARowL - 1].Strings[i + DragRecord * (AHeighRecord + 1)];
        if Pos(':', s) <> 0 then
          Delete(s, 1, Length(s) - (Length(s) - Pos(':', s)));
        AStringList.Append(s);
      end;
  SetLength(AddAndDel, Length(AddAndDel) + 1);
  AddAndDel[High(AddAndDel)] := TEditForm.Create(Self);
  ARecID := StrToInt(ArrayRecID[AColL - 1][ARowL - 1].Strings[DragRecord]);
  AddAndDel[High(AddAndDel)].ShowForm(Tag, ARecID, AStringList, False);
  AddAndDel[High(AddAndDel)].SaveButtonClick(Self);
  ExecuteButtonClick(Self);
end;

end.

