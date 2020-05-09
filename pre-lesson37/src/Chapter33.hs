module Chapter33 where

import System.IO
import Control.Monad
import Control.Applicative

data Name = Name
    {   firstName :: String
    ,   lastName :: String
    }

instance Show Name where
    show (Name first last) = mconcat [first, " ", last]

print1toN [] = putStrLn "done!" >> return ()
print1toN (x:xs) = do
    putStrLn [x]
    print1toN xs

data GradeLevel = Freshman
    | Sophomore
    | Junior
    | Senior
    deriving (Show, Enum, Ord, Eq)

data Student = Student
    {   studentId :: Int
    ,   gradeLevel :: GradeLevel
    ,   studentName :: Name
    } deriving Show

data Teacher = Teacher
    {   teacherId :: Int
    ,   teacherName :: Name
    } deriving Show

data Course = Course
    {   courseId :: Int
    ,   courseTitle :: String
    ,   teacher :: Int
    } deriving Show

students :: [Student]
students =
    [   Student 1 Senior (Name "Audre" "Lorde")
    ,   Student 2 Junior (Name "Leslie" "Silko")
    ,   Student 3 Freshman (Name "Judith" "Butler")
    ,   Student 4 Senior (Name "Guy" "Debord")
    ,   Student 5 Sophomore (Name "Jean" "Baudrillard")
    ,   Student 6 Junior (Name "Julia" "Kristeva")
    ]

teachers :: [Teacher]
teachers =
    [   Teacher 100 (Name "Simon" "De Beauvoir")
    ,   Teacher 200 (Name "Susan" "Sontag")
    ]

courses :: [Course]
courses =
    [   Course 101 "French" 100
    ,   Course 201 "English" 200
    ]

_select :: Monad m => (a -> b) -> m a -> m b
_select prop vals = do
    val <- vals
    return (prop val)

_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where test vals = do
    val <- vals
    guard (test val)
    return val

startsWith :: Char -> String -> Bool
startsWith c str = c == (head str)

jStudents = _where (startsWith 'J' . firstName) (_select studentName students)


_join :: (Eq c, Monad m, Alternative m) => m a -> m b -> (a -> c) -> (b -> c) -> m (a, b)
_join data1 data2 prop1 prop2 = do
    d1 <- data1
    d2 <- data2
    let dpairs = (d1, d2)
    guard ((prop1 (fst dpairs)) == (prop2 (snd dpairs)))
    return dpairs

joinData = _join teachers courses teacherId teacher
whereResult = _where ((== "English") . courseTitle . snd) joinData
selectResult = _select (teacherName . fst) whereResult

_hinq selectQuery joinQuery whereQuery =
    (\joinData ->
        (\whereResult -> selectQuery whereResult)
        (whereQuery joinData)
    ) joinQuery

finalResult = _hinq (_select (teacherName . fst))
                    (_join teachers courses teacherId teacher)
                    (_where ((=="English") . courseTitle .snd))

data HINQ monad data_ result
    =   HINQ (monad data_ -> monad result) (monad data_) (monad data_ -> monad data_)
    |   HINQ_ (monad data_ -> monad result) (monad data_)

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ selectC joinC whereC) = _hinq selectC joinC whereC
runHINQ (HINQ_ selectC joinC) = _hinq selectC joinC (_where (\_ -> True))

query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ   (_select (teacherName . fst))
                (_join teachers courses teacherId teacher)
                (_where ((=="English") . courseTitle .snd))

query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName) teachers

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

maybeQuery1 :: HINQ Maybe (Teacher,Course) Name
maybeQuery1 = HINQ (_select (teacherName . fst))
    (_join possibleTeacher possibleCourse
        teacherId teacher)
    (_where ((== "French") . courseTitle . snd))

data Enrollment = Enrollment
    {   student :: Int
    ,   course :: Int } deriving Show

enrollments :: [Enrollment]
enrollments = [(Enrollment 1 101)
    ,   (Enrollment 2 101)
    ,   (Enrollment 2 201)
    ,   (Enrollment 3 101)
    ,   (Enrollment 4 201)
    ,   (Enrollment 4 101)
    ,   (Enrollment 5 101)
    ,   (Enrollment 6 201)  ]

studentEnrollmentsQuery = HINQ_ (_select (\(st,en) ->
            (studentName st, course en)))
    (_join students enrollments studentId student)

studentEnrollments :: [(Name, Int)]
studentEnrollments = runHINQ studentEnrollmentsQuery

englishStudentsQ = HINQ (_select (fst . fst))
    (_join studentEnrollments
        courses
        snd
        courseId)
    (_where ((== "English") . courseTitle . snd))

englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQ

getEnrollments :: String -> [Name]
getEnrollments courseName = runHINQ courseQuery
    where
        courseQuery = HINQ
            (_select (fst . fst))
            (_join studentEnrollments
                courses
                snd
                courseId)
            (_where ((==courseName) . courseTitle . snd))
