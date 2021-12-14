import Data.Char
import Data.Maybe
import Prelude

newtype Survey = Survey {category :: [Category]}

data Category = Category {name :: String, questions :: [Question]} | Result deriving (Show)

data Question = Question {identifier :: String, questionType :: QuestionType, dependsOn :: Maybe (String, [String])} deriving (Show)

type Dependant = Bool

-- dependsOn (QuestionId, ChoiceString)
data QuestionType = RadioGroup {choices :: [String]} | TextInput | NumberInput deriving (Show)

printCategory :: Category -> Maybe [Char] -> Maybe [Char] -> [Char]
printCategory (Category name questions) previousCategory nextCategory =
  "import { GlobalState, useStateMachine } from 'little-state-machine'; \n\
  \import type { NextPage } from 'next';\n\
  \import { serverSideTranslations } from 'next-i18next/serverSideTranslations';\n\
  \import { useRouter } from 'next/router';\n\
  \import { SubmitHandler, useForm } from 'react-hook-form';\n\
  \import { useTranslation } from 'react-i18next';\n\
  \import updateAction from './updateAction';\n\
  \\n\
  \type FormValues = GlobalState\n\
  \\n\
  \ const "
    ++ name
    ++ ": NextPage = () => {\n\
       \  const { register, handleSubmit, formState: { errors } } = useForm<FormValues>();\n\
       \  const { actions, state } = useStateMachine({ updateAction });\n\
       \\n\
       \  const router = useRouter();\n\
       \  const { t } = useTranslation();\n\
       \\n\
       \  const onSubmit: SubmitHandler<FormValues> = data => {\n\
       \    actions.updateAction(data);\n\
       \    router.push('./"
    ++ (toLower <$> fromMaybe "result" nextCategory)
    ++ "');\n\
       \  }\n\
       \  return <form onSubmit={handleSubmit(onSubmit)}>\n"
    ++ (printQuestion =<< questions)
    ++ "\n"
    ++ printPreviousNext previousCategory
    ++ "\n\
       \  </form>\n\
       \}\n\
       \\n\
       \//@ts-ignore locale has type any.\n\
       \export const getStaticProps = async ({ locale }) => ({\n\
       \  props: {\n\
       \    ...await serverSideTranslations(locale, ['survey', 'common']),\n\
       \  },\n\
       \})\n\
       \\n\
       \\n\
       \export default "
    ++ name
printCategory _ _ _ = ""

printPreviousNext :: Maybe [Char] -> [Char]
printPreviousNext (Just previous) =
  "    <input type='button' onClick={() => router.push('./"
    ++ (toLower <$> previous)
    ++ "')} value={t('common:previous') as string} />\n    <input type='submit' value={t('common:next') as string} />\n"
printPreviousNext _ = "    <input type='submit' value={t('common:next') as string} />\n"

printQuestion :: Question -> [Char]
printQuestion (Question name questionType Nothing) =
  "    <h1 className='font-bold'>{t('survey:"
    ++ name
    ++ ".question')}</h1>\n    <div className='flex flex-col'>\n"
    ++ printQuestionType questionType
    ++ "    </div>\n    {/* errors will return when field validation fails  */}\n    {errors."
    ++ name
    ++ " && <span>This field is required</span>}\n"
  where
    printQuestionType (RadioGroup choices) = printChoice name =<< choices
printQuestion _ = ""

printChoice :: [Char] -> [Char] -> [Char]
printChoice questionName choice =
  "      <label >\n\
  \        <input type='radio' className='border-2 border-black' defaultChecked={state."
    ++ questionName
    ++ " === '"
    ++ choice
    ++ "'} value='"
    ++ choice
    ++ "' {...register('"
    ++ questionName
    ++ "', { required: true })} />\n\
       \        {t('survey:"
    ++ questionName
    ++ "."
    ++ choice
    ++ "')}\n\
       \      </label>"

survey :: [Category]
survey =
  [ Category
      "Allgemein"
      [Question "firstQuestion" (RadioGroup ["firstAnswer", "secondAnswer"]) Nothing],
    Category
      "Firma"
      [ Question "secondQuestion" (RadioGroup ["firstAnswer", "secondAnswer"]) Nothing,
        Question "fourthQuestion" (RadioGroup ["firstAnswer", "secondAnswer"]) Nothing
      ],
    Category
      "Zielgruppe"
      [Question "thirdQuestion" (RadioGroup ["1to10", "11to21"]) Nothing]
  ]

printForm :: (Maybe Category, Category, Maybe Category) -> String
printForm (a, b, c) = printCategory b (name <$> a) (name <$> c)

writeForm :: [(Maybe Category, Category, Maybe Category)] -> IO ()
writeForm = mapM_ (\f@(a, b, c) -> writeFile (name b ++ ".tsx") $ printForm f)

surveyPrevNext :: [(Maybe Category, Category, Maybe Category)]
surveyPrevNext = zipWith3 f (Nothing : (Just <$> survey)) survey ((Just <$> drop 1 survey) ++ repeat Nothing)
  where
    f a b c = (a, b, c)

main = writeForm surveyPrevNext
