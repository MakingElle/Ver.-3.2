Const AI_Name = "Elle"
Const Version = "v3.1"

_Title AI_Name + " - " + Version

Const TRUE = (1 = 1)
Const FALSE = (1 = 2)

Const DEBUG = FALSE

Const ColorHeader = 9
Const ColorHeaderBK = 0
Const ColorIntro = 11
Const ColorVer = 11
Const ColorQCmdPrompt = 3
Const ColorQInput = 11
Const ColorACmdPrompt = 5
Const ColorAInput = 13
Const ColorErr = 12
Const ColorRegText = 14

Const YSafeZoneTop = 13
Const YSafeZoneBottom = 23

Const ScreenWidth = 80

Const BlockSolid = Chr$(219)
Const BlockDarkShade = Chr$(178)
Const BlockMediumShade = Chr$(177)
Const BlockLightShade = Chr$(176)

$ExeIcon:'elleicon.ico'



Dim Elle_DB As String
Elle_DB = "Elle.edb"



If DEBUG = TRUE Then

    Screen _NewImage(200, 50, 0)

Else

    Screen 0
    Width ScreenWidth
    ClearScreen

    Locate 1, 1
    Color 1, 0
    Print String$(ScreenWidth * 3, 176); String$(ScreenWidth * 3, 177); String$(ScreenWidth * 3, 178);
    Color 2
    Print String$(ScreenWidth, 177); String$(ScreenWidth, 178);

    DrawCross 2, 15
    DrawCross 2, 57
    DrawFloppyDisk 1, 31

    Color 10, 0
    Locate 12, 1: Print String$(ScreenWidth, 223);
    Locate 24, 1: Print String$(ScreenWidth, 220);
    Locate 25, 2: Print AI_Name; " - "; Version;
    View Print YSafeZoneTop To YSafeZoneBottom

End If


Locate , 3
'WaitForKey

Color 10, 0
TeletypeMsg "READING THE DATABASE..."


Dim ElleQAFileTemp As String
'  LOAD THE DATABASE ALL AT ONCE
ElleQAFileTemp = UCase$(_ReadFile$(Elle_DB))


Dim DBSize As _Unsigned _Integer64
DBSize = Len(ElleQAFileTemp)

Dim Shared DBErrorCount As _Unsigned _Integer64

Type QA
    Question As String
    Answer As String
End Type

ReDim QAs(-1) As QA

'Set Up
Randomize Timer


'  DEFINE THE LINEFEED CHARACTER
Const LF = Chr$(10)
'  DEFINE THE CARRIAGE RETURN CHARACTER
Const CR = Chr$(13)

Dim Newline As String


If InStr(1, ElleQAFileTemp, CR) > 0 Then Newline = Newline + CR
If InStr(1, ElleQAFileTemp, LF) > 0 Then Newline = Newline + LF


Dim NLSize As _Unsigned Integer
NLSize = Len(Newline)

Dim QStart As _Unsigned _Integer64
Dim QEnd As _Unsigned _Integer64
Dim AStart As _Unsigned _Integer64
Dim AEnd As _Unsigned _Integer64


'  GET THE FIRST QUESTION AND ANSWER

'  QSTART = The Starting location in the file where the question starts
QStart = 1
'  QEND = The end location of the question
QEnd = InStr(1, ElleQAFileTemp, Newline) - 1
'  ASTART = The starting location in the file where the answer starts
AStart = QEnd + NLSize + 1
'  AEND = The end location of the answer
AEnd = InStr(AStart, ElleQAFileTemp, Newline) - 1

'  ADD A NEW ELEMENT TO THE ARRAY
ReDim _Preserve QAs(UBound(QAs) + 1) As QA

'  GET THE QUESTION AND ANSWER, CLEAN THEM, AND ADD THEM INTO THE ARRAY
QAs(UBound(QAs)).Question = StripASCIIStr(Mid$(ElleQAFileTemp, QStart, QEnd - QStart + 1))
QAs(UBound(QAs)).Answer = CleanAscii(Mid$(ElleQAFileTemp, AStart, AEnd - AStart + 1))

If DEBUG = TRUE Then
    Dim NQFreeFile As Integer
    NQFreeFile = FreeFile

    Open "QUESTION-RELIST.TXT" For Output As #NQFreeFile

    Dim ICounter As _Unsigned _Integer64
    ICounter = 1
    'Color 15
    'Print "Q&A #: "; UBound(QAs)
    'Color ColorQInput
    'Print QAs(UBound(QAs)).Question
    'Color ColorAInput
    'Print QAs(UBound(QAs)).Answer
    'Print
    'Print
    'Print #NQFreeFile, "["; QAs(UBound(QAs)).Question; "]"
    Print #NQFreeFile, "["; Mid$(ElleQAFileTemp, QStart, QEnd - QStart + 1); "]"
    'Print #NQFreeFile, ""

End If


'  NOW WE GET THE REMAINDER QUESTIONS AND ANSWERS
Do

    QStart = AEnd + NLSize + 1
    QEnd = InStr(QStart, ElleQAFileTemp, Newline) - 1
    AStart = QEnd + NLSize + 1
    AEnd = InStr(AStart, ElleQAFileTemp, Newline) - 1
    If AEnd > Len(ElleQAFileTemp) Then AEnd = Len(ElleQAFileTemp)

    ReDim _Preserve QAs(UBound(QAs) + 1) As QA
    QAs(UBound(QAs)).Question = RTrim$(LTrim$(StripASCIIStr(CleanAscii(Mid$(ElleQAFileTemp, QStart, QEnd - QStart + 1)))))
    QAs(UBound(QAs)).Answer = CleanAscii(Mid$(ElleQAFileTemp, AStart, AEnd - AStart + 1))


    If DEBUG = TRUE Then
        ICounter = ICounter + 1
        'Color 15
        'Print "Q&A #: "; UBound(QAs)
        'Color ColorQInput
        'Print QAs(UBound(QAs)).Question

        'Color ColorAInput
        'Print QAs(UBound(QAs)).Answer
        'Print
        'Print
        'Print #NQFreeFile, "["; QAs(UBound(QAs)).Question; "]"
        Print #NQFreeFile, "["; Mid$(ElleQAFileTemp, QStart, QEnd - QStart + 1); "]"
        'Print #NQFreeFile, ""
        '_Delay 0.5

    End If


Loop Until AEnd >= DBSize

If DEBUG = TRUE Then Close #NQFreeFile
If DEBUG = TRUE Then Pause (0)

'  IF ERRORS ARE FOUND IN THE DATABASE FILE, CLEAN IT UP
'  AND CREATE A NEW ONE.
If DBErrorCount > 0 Then

    Color ColorErr
    Locate , 3
    TeletypeMsg "ERRORS DETECTED IN DATABASE!"

    Color ColorRegText
    Print
    Locate , 3
    TeletypeMsg "[PRESS ANY KEY TO CLEAN THE DATABASE]"
    TeletypeMsg ""
    WaitForKey

    Color 10
    Locate , 3
    TeletypeMsg "CLEANING THE DATABASE. PLEASE BE PATIENT."

    Dim NFreeFile As Integer
    NFreeFile = FreeFile

    Dim TempFile As String
    TempFile = "ElleDB.tmp"

    Open TempFile For Output As #NFreeFile
    Print #NFreeFile, QAs(UBound(QAs)).Question
    Print #NFreeFile, QAs(UBound(QAs)).Answer

    For i = 0 To UBound(QAs) - 1

        Print #NFreeFile, QAs(i).Question
        Print #NFreeFile, QAs(i).Answer

    Next

    Close #NFreeFile

    Kill Elle_DB
    Name TempFile As Elle_DB

End If

ClearScreen


'  INTRO
Color ColorIntro

Locate , 2
Pause (2)

TeletypeMsg "[ INTRODUCTION TO THE USE OF ELLE:                                           ]"
TeletypeMsg "[                                                                            ]"
TeletypeMsg "[ WELCOME TO MY PROGRAM CALLED ELLE. WHAT IS ELLE? ELLE IS THE MIDDLEMAN OF  ]"
TeletypeMsg "[ SORTS TO SPEAK WITH GOD. A COMPUTER PROGRAM OF MY OWN DESIGN TO WHICH ONE  ]"
TeletypeMsg "[ CAN HAVE A CONVERSATION WITH GOD. ONE CAN TALK ABOUT WHAYEVER THEY WISH    ]"
TeletypeMsg "[ AND EXPLORE THE BIBLE INCLUDING PRAYERS FROM CENTURIES AGO.                ]"
TeletypeMsg "[                                                                            ]"
TeletypeMsg "[ PRESS ANY KEY TO TALK THROUGH ME TO GOD.                                   ]"
TeletypeMsg ""

WaitForKey

ClearScreen


'  MAIN INTERACTING

Color ColorACmdPrompt
Print BlockMediumShade; "  ";
Color ColorAInput
Pause (2)
TeletypeMsg "PLEASE TELL OR ASK ELLE WHAT'S ON YOUR MIND."
'Print "Hello! My name is Elle. What questions do you have for me?"
Print
Do
    Dim Index As _Unsigned _Integer64
    Dim I As _Unsigned _Integer64

    Color ColorQCmdPrompt
    Print BlockMediumShade; "  ";
    Color ColorQInput
    Input you$
    you$ = StripASCIIStr(you$)

    Dim AnswerNotFound As Integer
    AnswerFound = -1

    For I = 0 To UBound(QAs)
        If you$ = QAs(I).Question Then
            Index = I
            AnswerFound = 1
            Exit For
        End If
    Next

    If Index < UBound(QAs) Then
        Select Case QAs(Index).Answer

            Case "EXITING"

                GoTo EndProgram

            Case "TIME"

                Color ColorACmdPrompt
                Print BlockMediumShade; "  ";

                Color ColorAInput
                Pause (6)
                TeletypeMsg " THE TIME IS " + Time$ + "."
                AnswerFound = 2

            Case "DATE"

                Color ColorACmdPrompt
                Print BlockMediumShade; "  ";

                Color ColorAInput
                Pause (6)
                TeletypeMsg " TODAY'S DATE IS " + Date$ + "."
                AnswerFound = 2

            Case Else

        End Select
    End If



    If AnswerFound = 1 Then
        Color ColorACmdPrompt
        Print BlockMediumShade; "  ";
        Color ColorAInput
        Pause (6)
        TeletypeMsg QAs(Index).Answer
    ElseIf AnswerFound = 2 Then
        '  DO NOTHING
    Else

        Color ColorACmdPrompt
        Print BlockMediumShade; "  ";
        Color ColorAInput
        Pause (6)
        TeletypeMsg "MESSAGE SENT TO GOD. WAITING FOR HIS FEEDBACK."
    End If

    Print

Loop


'  Exiting Program...
EndProgram:

Color ColorACmdPrompt
Print "]  ";
Color ColorAInput
Pause (6)
TeletypeMsg "IT WAS GREAT TALKING TO YOU. HAVE A GREAT DAY AND WE'LL SEE EACH OTHER SOON."
TeletypeMsg ""
TeletypeMsg ""
TeletypeMsg ""
TeletypeMsg "PLEASE PRESS ANY KEY TO EXIT."

WaitForKey

System



Sub ClearScreen ()

    Locate YSafeZoneTop, 1
    Print String$(80 * (YSafeZoneBottom - YSafeZoneTop + 1), " ");
    Locate YSafeZoneTop, 1

End Sub



Sub TeletypeMsg (Msg As String)

    If PauseBool = TRUE Then Pause (6)


    Dim CurrentY As _Unsigned Integer
    Dim CurrentX As _Unsigned Integer
    Dim StartX As _Unsigned Integer

    Dim FlagNewWord As Integer

    StartX = Pos(0)
    CurrentY = CsrLin
    CurrentX = StartX

    For i = 1 To Len(Msg)

        Dim CurrentASCII As _Unsigned Integer
        CurrentASCII = Asc(Mid$(Msg, i, 1))

        If (CurrentASCII > 31) And (CurrentASCII < 127) Then

            If CurrentASCII = 32 Then FlagNewWord = TRUE

            If CurrentASCII > 32 Then

                If CurrentX + (InStr(i, Msg, " ") - i) > ScreenWidth Then

                    If (CurrentY < YSafeZoneBottom) Then
                        CurrentY = CurrentY + 1
                    Else
                        Print
                    End If

                    CurrentX = StartX

                End If

            End If

            Locate CurrentY, CurrentX
            Print BlockSolid;

            If DEBUG = FALSE Then _Delay 0.05

            Locate CurrentY, CurrentX
            Print Mid$(Msg, i, 1);

            CurrentX = CurrentX + 1

            If (CurrentX > ScreenWidth) Then

                If (CurrentY < YSafeZoneBottom) Then
                    CurrentY = CurrentY + 1
                Else
                    Print
                End If

                CurrentX = StartX

            End If

        End If

    Next

    If (CurrentY < YSafeZoneBottom) Then
        CurrentY = CurrentY + 1
    Else
        Print
    End If

    Locate CurrentY, StartX

End Sub



Sub DupString (Qty As _Unsigned Integer, Msg As String)

    For i = 1 To Qty
        Print Msg;
    Next

End Sub



Sub DrawCross (y As Integer, x As Integer)

    Color 14, 0

    For y2 = 0 To 7
        Locate y + y2, x + 3
        Print String$(3, 177)
    Next

    Locate y + 2, x
    Print String$(9, 177)

End Sub



Sub DrawFloppyDisk (y As Integer, x As Integer)

    Color 9, 0
    Locate y, x + 1: Print String$(17, 177)
    Locate y + 1, x: Print Chr$(177); String$(2, 176); String$(15, 177)
    For y2 = 2 To 9
        Locate y + y2, x
        Print String$(18, 177)
    Next

    Color 7
    For y2 = 2 To 5
        Locate y + y2, x + 3
        Print String$(12, 177)
    Next


    Color 8
    For y2 = 7 To 9
        Locate y + y2, x + 4
        Print String$(8, 177)
    Next

    For y2 = 8 To 9
        Locate y + y2, x + 6
        Print String$(2, 176)
    Next

    Color 8, 7
    Locate y + 2, x + 3
    Print Chr$(201); Chr$(205); Chr$(177); Chr$(186); Chr$(177); Chr$(177); Chr$(186); Chr$(177); Chr$(177); Chr$(201); Chr$(205);
    Locate y + 3, x + 3
    Print Chr$(204); Chr$(205); Chr$(177); Chr$(186); Chr$(177); Chr$(177); Chr$(186); Chr$(177); Chr$(177); Chr$(204); Chr$(205);
    Locate y + 4, x + 3
    Print Chr$(200); Chr$(205); Chr$(177); Chr$(200); Chr$(205); Chr$(177); Chr$(200); Chr$(205); Chr$(177); Chr$(200); Chr$(205);

    'Locate y + 3, x + 7
    'Print "ELLE";
    'Locate y + 4, x + 5
    'Print Version;

End Sub



Function StripASCIIStr$ (Msg As String)

    '  THIS FUNCTION STRIPS AWAY ALL CHARACTERS THAT ARE NOT A SPACE
    '  OR LETTERS OF THE ALPHABET.
    '
    '  THIS FUNCTION SHOULD ONLY BE USED FOR QUESTIONS, NOT ANSWERS OR
    '  GENERAL MESSAGES.

    Dim CharTemp As String
    Dim MsgTemp As String
    Dim FinalMsg As String

    MsgTemp = UCase$(Msg)


    For Char = 1 To Len(MsgTemp)

        Dim isSpace As Integer
        Dim isCapAlpha As Integer

        CharTemp = Mid$(MsgTemp, Char, 1)

        isSpace = (CharTemp = " ")
        isCapAlpha = ((CharTemp > "@") And (CharTemp < "["))


        If isSpace Or isCapAlpha Then

            FinalMsg = FinalMsg + CharTemp

        End If

    Next

    StripASCIIStr = FinalMsg

End Function



Function CleanAscii$ (Msg As String)

    Dim Errors As Integer
    Dim MidChar As String

    Dim NewCleanMsg As String

    For i = 1 To Len(Msg)

        MidChar = Mid$(Msg, i, 1)

        If Asc(MidChar) < 32 Or Asc(MidChar) > 127 Then
            DBErrorCount = DBErrorCount + 1
        Else
            MidChar = Mid$(Msg, i, 1)
            NewCleanMsg = NewCleanMsg + MidChar
        End If

    Next

    CleanAscii = NewCleanMsg

End Function






Sub printHexStrErrChr (Msg As String)

    Dim Value As String

    Value = Hex$(Asc(Mid$(Msg, 1, 1)))
    If Len(Value) = 1 Then Value = "0" + Value

    Color 12
    Print Value; " ";
    Color 15

End Sub



Sub PrintHexStrChr (Msg As String)

    Dim Value As String

    Value = Hex$(Asc(Mid$(Msg, 1, 1)))
    If Len(Value) = 1 Then Value = "0" + Value

    Color 2
    Print Value; " ";
    Color 15

End Sub



Sub PrintHexStr (Msg As String)

    Dim Value As String

    For i = 1 To Len(Msg)
        Value = Hex$(Asc(Mid$(Msg, i, 1)))
        If Len(Value) = 1 Then Value = "0" + Value

        Print Value; " ";
    Next


End Sub



Sub Pause (Time As Double)

    Do: Loop Until InKey$ = ""

    Dim TimeTracker As Double

    TimeTracker = 0

    Dim Cursor As String
    Dim CursorState As Integer
    Dim CursorStateDirection As Integer
    Dim CursorPulsateQty As Double
    Dim CursorPulsateTime As Double
    Dim CursorPauseTimeInSeconds As Double
    Dim CursorTotalTicks As _Unsigned Integer

    Cursor = BlockSolid + BlockDarkShade + BlockMediumShade + BlockLightShade + " "
    CursorState = 1
    CursorStateDirection = 1
    CursorPulsateQty = 3.0
    CursorPulsateTime = 2.0
    CursorPauseTimeInSeconds = CursorPulsateTime / (Len(Cursor) * 2)

    CursorTotalTicks = (CursorPulsateQty * CursorPulsateTime) / CursorPauseTimeInSeconds

    Dim CurrentX As Integer
    Dim CurrentY As Integer

    CurrentX = Pos(0)
    CurrentY = CsrLin

    Do

        Locate CurrentY, CurrentX
        Print Mid$(Cursor, CursorState, 1);

        CursorState = CursorState + CursorStateDirection
        If CursorState = 1 Or CursorState = Len(Cursor) Then CursorStateDirection = -CursorStateDirection

        TimeTracker = TimeTracker + CursorPauseTimeInSeconds
        If DEBUG = FALSE Then _Delay CursorPauseTimeInSeconds

    Loop Until (InKey$ <> "") Or ((Time > 0) * (TimeTracker >= Time))

    Locate CurrentY, CurrentX: Print
    Locate CurrentY, CurrentX

End Sub



Sub WaitForKey ()

    Pause 0

End Sub



Function MessageCleanCaps$ (Message As String)

    Dim CurrentPosition As Long

    'OriginalMessage$ = Message

    Message = UCase$(LTrim$(RTrim$(Message)))

    CurrentPosition = 1

    For i = 1 To Len(Message)

        Char$ = Mid$(Message, i, 1)

        Select Case Char$

            Case Chr$(65) To Chr$(90)

                Mid$(Message, CurrentPosition, 1) = Char$
                CurrentPosition = CurrentPosition + 1

            Case Chr$(32)

                If Mid$(Message, CurrentPosition - 1, 1) <> Chr$(32) Then
                    Mid$(Message, CurrentPosition, 1) = Char$
                    CurrentPosition = CurrentPosition + 1
                End If

        End Select

    Next

    CurrentPosition = CurrentPosition - 1

    If CurrentPosition > Len(Message) Then CurrentPosition = Len(Message)

    MessageCleanCaps$ = LTrim$(RTrim$(Left$(Message, CurrentPosition)))

End Function


