//
// dialog to send feedback on application uninstall 
// 
// usage:
// put the following lines into your iss file:
// 
// #include "fixfonts.iss" // see http://www.gerixsoft.com/blog/delphi/system-font-innosetup
// #include "feedback.iss"
// 
// procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
// begin
//   if CurUninstallStep = usUninstall then
//   begin
//     UninstallFeedback('IndieVolume Uninstall Feedback', 'Send', 'Cancel',
//       'To help us with future versions of IndieVolume, we want to know about any troubles or difficulties you have experienced while using IndieVolume.'#13#10  +
//       ''#13#10'Please let us know why are you uninstalling IndieVolume. Thank You.',
//       'support@gerixsoft.com', 'IndieVolume Uninstall Feedback');
//   end;
// end;
//

function UninstallFeedback(FormCaption: String; 
  SendButtonCaption, CancelButtonCaption, FeedbackLabelCaption: String;
  EmailAddress: String; EmailSubject: String): Boolean;
var
  Form: TSetupForm;
  FeedbackLabel: TLabel;
  SendButton, CancelButton: TButton;
  FeedbackMemo: TMemo;
  Feedback: String;
  Url: String;
  ErrorCode: Integer;
begin

  Result := false;
  Form := CreateCustomForm();
  try
    Form.ClientWidth := ScaleX(400);
    Form.ClientHeight := ScaleY(10+75 +10+100+10 +25+10); // feedback label + feedback memo + buttons
    Form.Caption := FormCaption;
    Form.BorderIcons := [biSystemMenu];
    Form.BorderStyle := bsDialog;
    Form.Center;

    SendButton := TButton.Create(Form);
    SendButton.Parent := Form;
    SendButton.Left := Form.ClientWidth - ScaleX(75 + 10 + 75 + 10);
    SendButton.Top := Form.ClientHeight - ScaleY(25 + 10);
    SendButton.Width := ScaleX(75);
    SendButton.Height := ScaleY(23);
    SendButton.Caption := SendButtonCaption;
    SendButton.ModalResult := mrOk;
    SendButton.Default := true;

    CancelButton := TButton.Create(Form);
    CancelButton.Parent := Form;
    CancelButton.Left := Form.ClientWidth - ScaleX(75 + 10);
    CancelButton.Top := Form.ClientHeight - ScaleY(25 + 10);
    CancelButton.Width := ScaleX(75);
    CancelButton.Height := ScaleY(25);
    CancelButton.Caption := CancelButtonCaption;
    CancelButton.ModalResult := mrCancel;
    CancelButton.Cancel := True;

    FeedbackLabel := TLabel.Create(Form);
    FeedbackLabel.Parent := Form;
    FeedbackLabel.AutoSize := False;
    FeedbackLabel.Left := ScaleX(10);
    FeedbackLabel.Top := ScaleY(10);
    FeedbackLabel.Width := Form.ClientWidth - ScaleX(10 + 10);
    FeedbackLabel.Height := ScaleY(75);
    FeedbackLabel.WordWrap := True;
    FeedbackLabel.Caption := FeedbackLabelCaption;

    FeedbackMemo := TMemo.Create(Form);
    FeedbackMemo.Parent := Form;
    FeedbackMemo.Left := ScaleX(10);
    FeedbackMemo.Top := FeedbackLabel.Top + FeedbackLabel.Height + ScaleY(10);
    FeedbackMemo.Width := Form.ClientWidth - ScaleX(10 + 10);
    FeedbackMemo.Height := ScaleY(100);

    FeedbackLabel.FocusControl := FeedbackMemo;

    Form.ActiveControl := FeedbackMemo;

    if Form.ShowModal() = mrOk then
    begin
      Feedback := Trim(FeedbackMemo.Lines.Text);
      if Feedback <> '' then
      begin
        StringChange(Feedback, '&', '%26');
        StringChange(Feedback, #13#10, '%0A');
        StringChange(Feedback, #13, '%0A');
        StringChange(Feedback, #10, '%0D');
        StringChange(Feedback, ' ', '%20');
        StringChange(EmailSubject, ' ', '%20');
        Url :='mailto:' + EmailAddress + '?Subject=' + EmailSubject + '&Body=' + Feedback;
        ShellExec('open', Url, '', '', SW_SHOW, ewNoWait, ErrorCode);
      end;
    end;
  finally
    Form.Free();
  end;
end;
