#Global

Question count@Int: #{plural count "Frage" "Fragen"}
Answer count@Int: #{plural count "Antwort" "Antworten"}
GetBack: Zurück!
TitleMisMatch: Tut mir leid, die Eingabe stimmt nicht mit dem Klausurnamen überein.
DelExam: Klausur löschen


#QuizCreator.hs

Nr: Nr
IsCorrect: Stimmt sie?
SubmitQuest count@Int: #{plural count "Frage" "Fragen"} abschicken
ExamTitle: Klausurname
PassPercentage: Bestehensquote
QuestionNum: Anzahl der Fragen
StartExam: Starte die Klausurerstellung
ErrorCookie: Problem beim Lesen des Cookies
ErrExamTitle: Maximale Länge sind 200 Zeichen, Leerzeichen werden abgeschnitten
ErrPassPercentage: Muss zwischen 0% und 100% sein
ErrQuestionNum: Sie muss mindestens eine Frage beinhalten

#Assets.hs

InputNeg: Tut mir leid, diese Eingabe darf nicht negativ sein.

#Exam.hs

PassExam exam@Text: Gratulation, sie haben die Klausur "#{exam}" bestanden!
NoPassExam exam@Text: Tut mir leid, sie haben die Klausur "#{exam}" leider nicht bestanden.
Evaluate: Auswerten
Points: Punkte

#Upload.hs

FileRec fInfo@Text: File erhalten: #{fInfo}.
ErrInXML_P1: Ein Fehler ist aufgetreten, bitte schauen sie nach ob es ein
ErrInXML_P2: valides xml file ist.
ChooseXML: Wählen sie bitte ein xml file zu Hochladen aus.
Upload: Hochladen
ExampleXML: Muster für eine xml Datei
Doctype: Muss in der ersten Zeile der xml stehen, siehe Beispiel


#Delete.hs

ConfirmDelExam: Wollen sie wirklich diese Klausur löschen?
ConfirmDelExam2: Tippen sie bitte den Klausurnamen ein und drücken sie auf Löschen.
SuccessDelete title@Text: Sie haben erfolgreich #{title} gelöscht.


#Widgets.hs

Exams: Klausuren
NoExamsInDB: Konnte keine Einträgen in der Datenbank finden. Bitte laden sie die Seite neu!
ShowXML: Zeige xml
Error txt@Text: Etwas lief schief...in #{txt}
QuizCreatorTitle: Klick hier um eine neue Klausur zu erstellen
UploadXMLTitle: Klick hier um eine neue xml hochzuladen
English: Englisch
German: Deutsch
Russian: Russisch

#Home.hs

ClickOnExam: Klick auf eine Klausur um dein Wissen zu testen!

#ExampleXML.hs

And: und
ShouldLookLike: Wie sollte meine XML aussehen?
ObeyRules: Ihre Datei sollte folgende Regeln einhalten:
WithAttribute count@Int: mit #{plural count "dem Attribut" "den Attributen"}
ContainsChars: kann Zahlen und Buchstaben enthalten
ContainsBool: kann "true" oder "false" enthalten
ContainsNumber: kann eine Zahl zwischen 0 und 1 enthalten
OneQuestion: Ihre Klausur muss mindestens eine Frage enthalten
FourAnswers: Jede Frage muss genau 4 Antworten enthalten
ContentTags: Die Antwort muss zwischen den Answertags stehen
ValidXML: Eine valide XML könnte so aussehen:
GotIt: Ok, ich habs verstanden!
