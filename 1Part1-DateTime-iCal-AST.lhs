\documentclass[fleqn]{scrartcl}

\usepackage[utf8x]{inputenc}
\usepackage{amstext}
\usepackage{amsthm}
%\usepackage{mathpazo}
\usepackage{hyperref}
\usepackage{booktabs}
\usepackage{graphicx}
\usepackage{amsmath}

\theoremstyle{definition}
\newtheorem{exercise}{}

\DeclareMathOperator{\separation}{separation}

%include polycode.fmt
%include spacing.fmt
\let\Keyword\texttt
\def\bksl{@\@}

\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\texttt}


\begin{document}

    %subst keyword a = "\Keyword{" a "}"
    %subst varsym a = "\mathbin{\texttt{" a "}}"
    %subst consym a = "\texttt{" a "}"
    %subst numeral a = "\texttt{" a "}"
    %subst space a = "\;\,"
    %format : = "\texttt{:}"
    %format ^^ = "\;\,"
    %format not = "\Varid{not}"
    %format | = "\mathbin{\texttt{|}}"
    %format ( = "\mathopen{\texttt{(}}"
    %format ) = "\mathclose{\texttt{)}}"
    %format [ = "\mathopen{\texttt{[}}"
    %format ] = "\mathclose{\texttt{]}}"
    %format || = "\mathbin{\texttt{||}}"
    %format && = "\mathbin{\texttt{\&\&}}"
    %format * = "\mathbin{\texttt{*}}"
    %format > = "\mathbin{\texttt{>}}"
    %format :: = "\mathrel{\texttt{:$\!$:}}"
    %format ++ = "\mathbin{\texttt{+$\!$+}}"
    %format -> = "\mathbin{\texttt{-$\!$>}}"
    %format => = "\mathbin{\texttt{=$\!$>}}"
    %format >%> = "\mathbin{\texttt{>\percent>}}"
    %format ,  = "\mathpunct{\texttt{,}}"
    %format ; = "\mathpunct{\texttt{;}}"
    %format = = "\mathrel{\texttt{=$\!\!\!$=}}"
    %format / = "\mathbin{\texttt{/}}"
    %format == = "\mathbin{\texttt{==}}"
    %format \ = "\mathord{\texttt{\bksl}}"
    %format _elem = elem
    %format `elem` = `_elem`
    %format >= = "\mathbin{\texttt{>=}}"
    %format . = "\mathbin{\texttt{.}}"
    %format _ = "\mathord{\texttt{\underscore}}"
    \let\Varid\texttt
    \let\Conid\texttt
    \def\osprompt{@$@}%
    \def\ghciprompt{@>@}%
    \def\percent{@%@}%
    \def\underscore{@_@}%


    \title{INFOB3TC -- Assignment 1 -- Part 1}
    \titlehead{Department of Information and Computing Sciences \\ Utrecht University}
    \author{Jeroen Bransen, João Pizani}
    \date{Deadline: Wednesday, 29 November 2017 15:00}
    \maketitle


    As a result of the first \emph{two} assignments of the course,
    we will end up building a parser (and a few so-called ``semantic functions'')
    for files in (a simplified version of) the \emph{iCalendar} format, a calendar exchange format.
    See for instance Wikipedia at
    \[
    \text{\url{http://en.wikipedia.org/wiki/ICalendar}}
    \]
    for an informal explanation of the format.

    The iCalendar format is used to store and exchange meeting requests, tasks and appointments in a standardized format.
    The format is supported by a large number of products, including Google Calendar and Apple iCal.

    In this first assignment, we will implement a parser for a standard date/time format,
    and we will define a set of datatypes to represent an iCalendar file.
    There are some bonus exercises for implementing more features.

    \subsection*{Parser combinators}
        For this task, you are supposed to use parser combinators as discussed
        in the lectures. These are contained in a Haskell package called @uu-tc@
        which is available from Hackage\footnote{\url{http://hackage.haskell.org/package/uu-tc}}.

        There are two versions of the parser combinator library in that package.
        By saying @import ParseLib.Simple@ you get the combinators from the lecture notes.
        In the lectures, a variant is used that keeps the implementation details hidden.
        To use this version, you need to @import ParseLib.Abstract@.

        You can choose your variant, but I recommend that you @import ParseLib.Abstract@,
        for easier-to-understand error messages.

        For bonus points you can use the @uu-parsinglib@ package,
        see exercise~\ref{ex:uuparsing} for more information.
        If you want to try to get the bonus, I recommend you first make sure that all your solutions work
        with the simple library \emph{first} (@uu-tc@), and \emph{only after} everything is working
        you try to switch to @uu-parsinglib@.

    \subsection*{General remarks}
        Here are a few remarks:
        \begin{itemize}
            \item Make sure your program compiles (with an installed @uu-tc@ package).
            Verify that @ghc --make -O DateTime.hs@ succeeds prior to submission.
            Please do not submit attempts which don't even compile. Compile them in your computer first.

            \item Include \emph{useful} comments in your code. Do not paraphrase the
            code, but describe the structure of your program, special cases, preconditions, etc.

            \item Try to write readable and idiomatic Haskell. Style influences the grade!
            The use of existing higher-order functions
            such as @map@, @foldr@, @filter@, @zip@ -- just to name a few -- is highly encouraged.
            The use of existing libraries is allowed (as long as the program still compiles with the above invocation).

            \item Copying solutions from the internet is not allowed.

            \item We \textbf{strongly} prefer teams of size two,
            if you cannot find a partner talk to the teaching assistants and they will help.
            A team must submit a single assignment and put both names on it.
            It makes the lives of the assistants much easier if only one person in the team is responsible
            for submission\ldots

            \item Textual answers to tasks can be included as comments in the source file submitted.

            \item Submission is done through DomJudge, at \url{https://domjudge.cs.uu.nl/tc/team}.
            In the same zip package where you got this PDF file there is a file named \texttt{DateTime.hs}.
            This is the \emph{starting framework} file in which you should write the answers to the programming questions.
            Please, before submitting your file to DomJudge, try to compile it first in your own machine.
            This avoids having dozens of bogus submissions for each group and makes the work of the assistants
            easier when grading.

            Some datatypes and type signatures might already be defined. The rest is up to you to define.
            The outputs of your solution will be compared with a ``model solution'' through the automatic
            \emph{DomJudge} system. You can submit as many attempts as you want until the deadline.
            The last correct submission will be considered for grading.
            When there are no correct submissions, the latest is graded (even if incorrect).
        \end{itemize}


    \subsection*{Date and time}
        Let's start our journey towards the iCalendar format with a simple task: parsing a date/time format.

        The concrete syntax of a date and time value in so-called \emph{Standard Algebraic Notation} (SAN)
        is given by the following grammar:
        %{
        \begingroup
        \renewcommand\Varid[1]{\mathit{#1}}
        %format epsilon = "\varepsilon "
        %format ::= = "\mathrel{::=} "
        %format T = "\Conid{T}"
        %format Z = "\Conid{Z}"
        %format 0 = "\Conid{0}"
        %format 1 = "\Conid{1}"
        %format 2 = "\Conid{2}"
        %format 3 = "\Conid{3}"
        %format 4 = "\Conid{4}"
        %format 5 = "\Conid{5}"
        %format 6 = "\Conid{6}"
        %format 7 = "\Conid{7}"
        %format 8 = "\Conid{8}"
        %format 9 = "\Conid{9}"
        %format | = "\mathrel{\vert}"
        %format = = "\Conid{=}"

> datetime                          ::=  date datesep time
> date                              ::=  year month day
> time                              ::=  hour minute second timeutc
> year                              ::=  digit digit digit digit
> month, day, hour, minute, second  ::=  digit digit
>
> timeutc                           ::=  epsilon | Z
> digit                             ::=  0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
> datesep                           ::=  T

        Terminals are written in typewriter font, nonterminals in italics.
        A @datetime@ value has a fixed length and contains the date and time values as fixed length integers.
        The optional trailing @Z@ is used to indicate that this date/time is expressed in UTC (Coordinated Universal Time).
        If the @Z@ is omitted the time should be interpreted as local time.

        \emph{No whitespace is allowed anywhere in a date/time value!}
        \endgroup
        %}

        When writing a parser, one of the most important decisions is which data structure to use as \emph{target}
        of the parser, that is, which datatype will be produced by the parser.
        In this case, we will represent date/time values using the following Haskell datatypes:

> data DateTime = DateTime  { date  :: Date
>                           , time  :: Time
>                           , utc   :: Bool }
>     deriving Eq

> data Date = Date  { year   :: Year
>                   , month  :: Month
>                   , day    :: Day }
>     deriving Eq

> newtype Year   = Year   { unYear :: Int }   deriving Eq
> newtype Month  = Month  { unMonth :: Int }  deriving Eq
> newtype Day    = Day    { unDay :: Int }    deriving Eq

> data Time = Time  { hour    :: Hour
>                   , minute  :: Minute
>                   , second  :: Second }
>     deriving Eq

> newtype Hour    = Hour    { unHour :: Int }    deriving Eq
> newtype Minute  = Minute  { unMinute :: Int }  deriving Eq
> newtype Second  = Second  { unSecond :: Int }  deriving Eq

        \begin{exercise}[3 pt, medium]
            Define a parser

> parseDateTime :: Parser Char DateTime

            that can parse a single date and time value.
            This implies that you have to define parsers for all the other types (@Date@, @Time@, @Hour@, etc.) too.
        \end{exercise}


        \begin{exercise}[1 pt]
            Define a function

> run :: Parser a b -> [a] -> Maybe b

            that applies the parser to the given input. Of all the results the parser returns, we are interested in the
            \emph{first} result that is a \emph{complete} parse, i.e., where the remaining list of input symbols is empty.
            If such a result exists, it is returned. Otherwise, @run@ should return @Nothing@.
        \end{exercise}


        \begin{exercise}[1 pt]
            Define a printer

> printDateTime :: DateTime -> String

            that turns a date and time value back into SAN notation.
            The idea is that for any value @dt@ of type @DateTime@ we have that

> run parseDateTime (printDateTime dt) == Just dt

            i.e., that printing the date and time and then parsing it again succeeds
            and results in the same abstract representation of the date and time.
            Similarly, for valid SAN strings @s@ we should have that

> parsePrint s == Just s

            where

> parsePrint s = printDateTime <$> run parseDateTime s

        \end{exercise}


        %format >>> = "\mathord{" * "}" Main "\mathord{" > "}" ^^
        \begin{exercise}[0 pt]
            Test your parser for date and time on a couple of examples, by parsing and printing examples:

< >>> parsePrint "19970610T172345Z"
< Just "19970610T172345Z"
< >>> parsePrint "19970715T040000Z"
< Just "19970715T040000Z"
< >>> parsePrint "19970715T40000Z"
< Nothing
< >>> parsePrint "20111012T083945"
< Just "20111012T083945"
< >>> parsePrint "20040230T431337Z"
< Just "20040230T431337Z"

        \end{exercise}

        As you might have noticed,
        the last example demonstrates that our grammar (and hence, our parser) also accepts some \emph{invalid} values:
        for instance, hours can only range from @0@ to @23@, but currently all 2-digit integers are accepted.


        \begin{exercise}[2 pt]
            Write a function

> checkDateTime :: DateTime -> Bool

            that verifies that a @DateTime@ represents a valid date and time.
            Any 4-digit value should be accepted as a valid year, and years in ``BC'' (\emph{Before Christ}) are ignored.
            Valid months are in the range @1-12@, and valid days are in the range @1-28@, @1-29@, @1-30@ or @1-31@,
            depending on the month.
            Valid times are those where the hour is in the range @0-23@ and minute and seconds are in the range @0-59@.

            Refer to
            \[
            \text{\url{http://en.wikipedia.org/wiki/Month\#Julian_and_Gregorian_calendars}}
            \]
            for more information about the number of days per month.
            Make sure that you handle leap years in the correct way!

< >>> let parseCheck s = checkDateTime <$> run parseDateTime s
< >>> parseCheck "19970610T172345Z"
< Just True
< >>> parseCheck "20040230T431337Z"
< Just False
< >>> parseCheck "20040229T030000"
< Just True

            \textbf{Tip:} Write functions to work with the datatypes produced by the parser, even for the simple ones.
            For example, if you need to subtract two values of @Year@, define a function @subYears :: Year -> Year -> Year@.

        \end{exercise}


    \subsection*{Events and full calendar file}
        We will now extend our definition to events. The concrete syntax of events is as follows:
        %{
        \begingroup
        \renewcommand\Varid[1]{\mathit{#1}}%
        \newcommand\dollar{@$@}%
        \newcommand\dq{@"@}%
        \newcommand\bs{@\@}%
        %format | = "\mathrel{\vert}"
        %format epsilon = "\varepsilon "
        %format ::= = "\mathrel{::=} "
        %format BEGINVEVENT = "\Conid{BEGIN:VEVENT}"
        %format ENDVEVENT = "\Conid{END:VEVENT}"
        %format DTSTAMP = "\Conid{DTSTAMP:}"
        %format DTSTART = "\Conid{DTSTART:}"
        %format DTEND = "\Conid{DTEND:}"
        %format UID = "\Conid{UID:}"
        %format DESCRIPTION = "\Conid{DESCRIPTION:}"
        %format SUMMARY = "\Conid{SUMMARY:}"
        %format LOCATION = "\Conid{LOCATION:}"
        %format BEGINCALENDAR = "\Conid{BEGIN:VCALENDAR}"
        %format ENDCALENDAR = "\Conid{END:VCALENDAR}"
        %format PRODID = "\Conid{PRODID:}"
        %format VERSION = "\Conid{VERSION:2.0}"
        %format many(a) = a "^{*}"
        %format (optional(a)) = a "?"
        \invisiblecomments

> event        ::=  BEGINVEVENT crlf
>                   many(eventprop)
>                   ENDVEVENT crlf
> eventprop    ::=  dtstamp | uid | dtstart | dtend | description | summary | location
> dtstamp      ::=  DTSTAMP      datetime  crlf
> uid          ::=  UID          text      crlf
> dtstart      ::=  DTSTART      datetime  crlf
> dtend        ::=  DTEND        datetime  crlf
> description  ::=  DESCRIPTION  text      crlf
> summary      ::=  SUMMARY      text      crlf
> location     ::=  LOCATION     text      crlf

        Here @crlf@ is a Carriage Return and Line Feed, represented in Haskell as @"\r\n"@,
        and @text@ is a string of characters containing neither carriage return nor line feed.
        No extra whitespace is allowed anywhere in an event.

        Finally, the concrete syntax of a full iCalendar file is defined as:

> calendar     ::=  BEGINCALENDAR crlf
>                   many(calprop)
>                   many(event)
>                   ENDCALENDAR crlf
> calprop      ::=  prodid | version
> prodid       ::=  PRODID text crlf
> version      ::=  VERSION crlf

        Here is an informal explanation of the syntax:
        An iCalendar file consists of a standard header and a sequence of events.
        Both the standard header and the event consist of a set of properties.
        The properties are name-value pairs, separated by a colon,
        each on a separate line ended by a carriage return and line feed.
        Events and the main calendar object are blocks surrounded by \texttt{BEGIN} and \texttt{END} lines.

        Some of the properties are required and most properties must appear exactly once.
        The order in which the properties must appear within an event is not defined.
        In the header both @prodid@ and @version@ are required and must appear exactly once.
        In an event the properties @dtstamp@, @uid@, @dtstart@ and @dtend@ are required and must appear exactly once.
        @description@, @summary@ and @location@ are optional but must not appear more than once.

        \endgroup
        %}


        \begin{exercise}[3 pt]
            Define Haskell datatypes (or type synonyms)  to describe the abstract syntax of an iCalendar file.
            Call the type for a whole iCalendar file @Calendar@.

            \emph{Hint}: The abstract syntax \textbf{does not need} to have the same structure as the concrete syntax.
            Read the informal explanation of the format several times, and think about the best way to represent the calendar.
        \end{exercise}

        \newpage


    \subsection*{Bonus exercises}
        \begin{exercise}[bonus, 1 pt, medium] \label{ex:uuparsing}
            With the parser combinators from the @uu-tc@ library, there is no elegant way to define parsers
            for properties that must appear exactly once but can appear in any order.
            In the @uu-parsinglib@\footnote{\url{http://hackage.haskell.org/package/uu-parsinglib}} library
            there is a @MergeAndPermute@ module which allows you to specify this in a nice way.

            Implement all parsers in your solution using the @uu-parsinglib@.
        \end{exercise}

\end{document}

