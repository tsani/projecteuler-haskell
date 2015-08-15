import Data.Digits
import Data.List ( maximumBy )
import Data.Function ( on )

digitSum = sum . digits 10

range = [x^y | x <- [1..99], y <- [1..99]]

answerIn = maximum . map digitSum

answer = answerIn range
