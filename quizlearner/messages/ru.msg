#Global

Question count@Int: #{plural count "Вопрос" "Вопросы"}
Answer count@Int: #{plural count "Ответ" "Ответы"}
GetBack: Возвратится
TitleMisMatch: Ваши входные данные не совпадают с именем экзамена.
DelExam: Удалить экзамен
LoogedIn email@Text:  Вошёл как #{email}
ConfirmExam: Подтвердить
Get404: Извините пожалуйста, этот сайт не существует :(
MakePublic: Сделать публичным

#QuizCreator.hs

Nr: H
IsCorrect: Oн правильный?
SubmitQuest count@Int: Отправить #{plural count "вопрос" "вопросы"}
ExamTitle: Hазвание экзаменa
PassPercentage: Процент для прохождения
QuestionNum: Количество вопросов
StartExam: Начать создание экзамена
ErrorCookie: Проблема в чтении кооки
ErrExamTitle: Максимальная длина 200 символов без ненужных пробелов
ErrPassPercentage: Должен быть между 0% и 100%
ErrQuestionNum: Содержит минимум один вопрос


#Assets.hs

InputNeg: Ваши входные данные не могут быть негативные!

#Exam.hs

PassExam exam@Text: Поздравляю вас с прохождением экзамена "#{exam}"!
NoPassExam exam@Text: Вы не сдали экзамен "#{exam}".
Evaluate: Оценить
Points: Очки

#Upload.hs

FileRec fInfo@Text: Получил файл: #{fInfo}.
ErrInXML_P1: Произошла ошибка, проверьте на
ErrInXML_P2:  валидность xml файла.
ChooseXML: Выберите xml файл для загрузки.
Upload: Загрузить
ExampleXML: Пример xml файла


#Delete.hs

ConfirmDelExam: Вы действительно хотите удалить этот экзамен?
ConfirmDelExam2: Напишите имя экзамена и подтвердите ваш ввод.
SuccessDelete title@Text: Вы стёрли #{title}.


#Widgets.hs

Exams: Экзамены
NoExamsInDB: Не мог найти экзаменов в базе данных. Пожалуйста перезагрузитье саит.
ShowXML: Показать xml
Error txt@Text: Что то сломалось...в #{txt}
QuizCreatorTitle: Нажмите эту кнопку для создания экзамена
UploadXMLTitle: Нажмите эту кнопку для загрузки xml фаила
English: английский
German: немецкий
Russian: русский
PrivExams: Личные экзамены
NoPrivExams: Не найденны личные экзамены

#Home.hs

ClickOnExam: Нажмите на один из экзаменов для началы учёбы!

#ExampleXML.hs

And: и
ShouldLookLike: Как должен мой XML файл выглядеть?
ObeyRules: Ваш файл должен подчиняться следующим правилам:
WithAttribute count@Int: с #{plural count "атрибутом" "атрибутами"}
ContainsChars: состоит из буквенно-цифровых символов
ContainsBool: состоит из "true" или "false"
ContainsNumber: состоит из цифры между нулем и сотней
OneQuestion: ваш экзамен содержит по крайней мере один вопрос
FourAnswers: каждый вопрос содержит точно четыре ответа
ContentTags: содержание ответа должен быть между answer ярлыками
ValidXML: Пример валидного XML файла который может выглядеть как таковой:
GotIt: Спасибо я, всё понял!
Doctype: должно стоять в начале каждого файла, как в примере

#AccManager.hs

HaveToBeLoggedIn: Вы должны быть зарегестрирваны на этом сайте чтобы выполнить это действие.

#SetPublic.hs

SetExamPublic examtitle@Text: Вы публицырвали  #{examtitle}!
ExamAlreadyPublic: Этот экзамен уже публицырван!
ConfirmPublic: Вы действительно хотите публицырвать этот экзамен?
ConfirmPublic2: Напишите имя экзамена и подтвердите ваш ввод.