#Global

Question count@Int: #{plural count "Question" "Questions"}
Answer count@Int: #{plural count "Answer" "Answers"}
GetBack: Get back!
TitleMisMatch: Sorry, the input does not match the title.
DelExam: Delete exam


#QuizCreator.hs

Nr: Nr
IsCorrect: Is it correct?
SubmitQuest count@Int: Submit #{plural count "question" "questions"}
ExamTitle: Exam title
PassPercentage: Passing percentage
QuestionNum: Number of questions
StartExam: Start exam creation
ErrorCookie: Error in reading the exam cookie
ErrExamTitle: Maximum length is 200 chars w/o leading and trailing spaces
ErrPassPercentage: Has to be between 0% and 100%
ErrQuestionNum: There has to be at least one question

#Assets.hs

InputNeg: Sorry, this input can't be negative!

#Exam.hs

PassExam exam@Text: Congratulations, you passed the test "#{exam}"!
NoPassExam exam@Text: Sorry, you didn't pass "#{exam}".
Evaluate: Evaluate
Points: Points
  
#Upload.hs

FileRec fInfo@Text: File recieved: #{fInfo}.
ErrInXML_P1: Error occurred, please check if it is a
ErrInXML_P2: valid xml file.
ChooseXML: Please choose a xml file to upload.
Upload: Upload
ExampleXML: Example xml file


#Delete.hs

ConfirmDelExam: Do you really want to delete this exam?
ConfirmDelExam2: Please type the examtitle correctly and click delete.
SuccessDelete title@Text: You successfully deleted #{title}.


#Widgets.hs

Exams: Exams
NoExamsInDB: Couldn't find any exams in the DB. Please refresh!
ShowXML: Show xml
Error txt@Text: Something went wrong...in #{txt}
QuizCreatorTitle: Click this to create a new exam
UploadXMLTitle: Click this to upload an xml file
English: English
German: German
Russian: Russian

#Home.hs

ClickOnExam: Click on an exam to start practicing!

#ExampleXML.hs

And: and
ShouldLookLike: What should my XML look like?
ObeyRules: Your file should obey the following rules:
WithAttribute count@Int: with the #{plural count "attribute" "attributes"}
ContainsChars: contain alphanumerical characters
ContainsBool: contains either "true" or "false"
ContainsNumber: contains a number between 0 and 1
OneQuestion: your exam has to have at least one question
FourAnswers: every question has to have exactly four answers
ContentTags: the answer content has to be between the answer tags
ValidXML: A valid XML file might look like this:
GotIt: Ok, got it!
