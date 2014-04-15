\documentclass[a4paper,bibliography=totocnumbered,parskip=half]{scrbook}
%include polycode.fmt
\usepackage{lhs}
\usepackage[utf8]{inputenc}
\usepackage[backend=biber]{biblatex}
\usepackage[english]{babel}
\usepackage{csquotes}
\usepackage{hyperref}
\usepackage{url}
\usepackage{scrhack}
\usepackage{graphicx}
\usepackage[T1]{fontenc}
\usepackage{amsmath}
\usepackage[obeyFinal]{todonotes}
\usepackage{blindtext}

% Kapitelüberschrift in der Kopfzeile
\usepackage[automark]{scrpage2} % Schickerer Satzspiegel mit KOMA-Script
\pagestyle{scrheadings}
\setheadsepline{.4pt}

\clubpenalty10000
\widowpenalty10000

\bibliography{accelerate}

\newcommand{\executeIffilenewer}[3]{
 \ifnum\pdfstrcmp{\pdffilemoddate{#1}}
 {\pdffilemoddate{#2}}>0
 {\immediate\write18{#3}}\fi
}
\newcommand{\includesvg}[2][]{
 \executeIffilenewer{#2.svg}{#2.pdf}
 {inkscape -z -D --file=#2.svg --export-pdf=#2.pdf}
 \includegraphics[#1]{#2.pdf}
}

\begin{document}

\frontmatter

% Titelseite - ganz einfach
\titlehead{
  {\large Programming Languages and Compiler Construction}\\
  Department of Computer Science\\
  Christian-Albrechts-University of Kiel}
\subject{Master Thesis}
\date{\today}
%opening
\title{An LLVM Backend for Accelerate}
\author{Timo von Holtz}
\publishers{Advised By:\\Priv.-Doz. Dr. Frank Huch\\Assoc. Prof. Dr. Manuel M T Chakravarty}
\maketitle

\chapter*{Erklärung der Urheberschaft}

Ich erkläre hiermit an Eides statt, dass ich die vorliegende Arbeit ohne Hilfe Dritter und ohne Benutzung anderer als der angegebenen Hilfsmittel angefertigt habe;
die aus fremden Quellen direkt oder indirekt übernommenen Gedanken sind als solche kenntlich gemacht. 
Die Arbeit wurde bisher in gleicher oder ähnlicher Form in keiner anderen Prüfungsbehörde vorgelegt und auch noch nicht veröffentlicht.

\today
\begin{flushright}
\rule{6cm}{0.4pt} \\
Timo von Holtz
\end{flushright} 
\clearpage
% oder auch manuell

% Verzeichnisse
\listoftodos
\tableofcontents   % Inhaltsverzeichnis
\listoffigures     % Abbildungsverzeichnis
\listoftables      % Tabellenverzeichnis
%\lstlistoflistings % Abbildungsverzeichnis
\mainmatter

\chapter{Introduction}
\todo{Write Introduction}

\chapter{Contributions}
\section{llvm-general-quote}
When writing a companyiler using LLVM in Haskell there is a good tutorial on how to do it at \citeurl{diehl2014jit}.
It uses \citetitle{scarlet2013llvm} to interface with LLVM.
The general idea is to use a monadic generator to produce the AST on the fly.
Let's look at an code fragment to get an idea how this works.
\begin{code}
for :: Type                             -- type of the index
    -> Operand                          -- starting index
    -> (Operand -> CodeGen Operand)     -- loop test to keep going
    -> (Operand -> CodeGen Operand)     -- increment the index
    -> (Operand -> CodeGen ())          -- body of the loop
    -> CodeGen ()
for ti start test incr body = do
  loop  <- newBlock "for.top"
  exit  <- newBlock "for.exit"

  -- entry test
  c     <- test start
  top   <- cbr c loop exit

  -- Main loop
  setBlock loop
  c_i   <- freshName
  let i  = local c_i

  body i

  i'    <- incr i
  c'    <- test i'
  bot   <- cbr c' loop exit
  _     <- phi loop c_i ti [(i',bot), (start,top)]

  setBlock exit
\end{code}
As you can tell this is much boilerplate code.
We have to define the basic blocks manually and add the instructions one by one.
This has some obvious drawbacks, as the code can get unreadable pretty quickly.

A solution is to use quasiquotation\cite{mainland2007quote} instead.
The idea behind quasiquotation is, that you can define a DSL with arbitrary syntax, which you can then directly transform into Haskell data structures.
This is done at compile-time, so you get the same type safety as writing the AST by hand.

I implemented \citetitle{holtz2014quote}, a quasiquotation library for LLVM.
Using my library, the code using a loop looks like this:
\begin{verbatim}
[llg|
define i64 @foo(i64 %start, i64 %end) {
  entry:
    br label %for
  
  for:
    for i64 %i in %start to %end with i64 [0,%entry] as %x {
        %y = add i64 %i, %x
        ret i64 %y
    }
}
|]
\end{verbatim}
This will expand to the following LLVM IR:
\begin{verbatim}
define i64 @foo(i64 %start, i64 %end) {
entry:
  br label %for

for:                                   ; preds = %for.body, %entry
  %i = phi i64 [ %i.new, %for.body ], [ %start, %entry ]
  %x = phi i64 [ %y, %for.body ], [ 0, %entry ]
  %for.cond = icmp ule i64 %i, %end
  %i.new = add nuw nsw i64 %i, 1
  br i1 %for.cond, label %for.body, label %for.end

for.end:                               ; preds = %for
  ret i64 %x

for.body:                              ; preds = %for
  %y = add i64 %i, %x
  br label %for
}
\end{verbatim}
This is clearly more readable.
Furthermore, one can see much more clearly what the produced code will be.

Another advantage of quasiquotation is antiquotation.
This means you can still reference arbitrary Haskell variables from within the quotation.
Using this the following are equivalent:
\begin{itemize}
\item @[llinstr| add i64 %x, 1 |]@
\item @let y = 1 in [llinstr| add i64 %x, $opr:(y) |]@
\end{itemize}

The design of \citetitle{holtz2014quote} is inspired by \citetitle{mainland2007c}, which is also used in the cuda implementation of Accelerate.
I use \citetitle{gill1995happy} and \citetitle{alex}.


\appendix

\backmatter
\printbibliography
\end{document}
