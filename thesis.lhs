\documentclass[a4paper,bibliography=totocnumbered,parskip=half]{scrbook}
%include polycode.fmt
\usepackage{lhs}
\usepackage[utf8]{inputenc}
\usepackage[backend=biber,style=alphabetic]{biblatex}
\usepackage[english]{babel}
\usepackage{csquotes}
\usepackage{hyperref}
\usepackage{url}
\usepackage{scrhack}
\usepackage{graphicx}
\usepackage[T1]{fontenc}
\usepackage{amsmath}
\usepackage[obeyFinal]{todonotes}
\usepackage{multirow}% http://ctan.org/pkg/multirow

% Kapitelüberschrift in der Kopfzeile
\usepackage[automark]{scrpage2} % Schickerer Satzspiegel mit KOMA-Script
\pagestyle{scrheadings}
\setheadsepline{.5pt}

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

\pagestyle{empty}

\begin{center}
{\huge \it Master Thesis}

\vspace{2cm}

{\Large \bf An LLVM Backend for Accelerate}

\vspace{1.75cm}

\includegraphics[height=6cm]{CAU-Siegel}

\vspace{1.75cm}

{\large Programming Languages and Compiler Construction\\
   Department of Computer Science\\
   Christian-Albrechts-University of Kiel
}

\end{center}

\vspace{2cm}

\begin{tabular}{ll}
angefertigt von: & {\bf Timo von Holtz} \\
\multirow{2}{*}{advised by:} & Priv.-Doz. Dr. Frank Huch\\
 & Assoc. Prof. Dr. Manuel M T Chakravarty
\end{tabular}

\vspace{1cm}

\begin{center}
Kiel, \today
\end{center}

\cleardoublepage

\chapter*{Selbstständigkeitserklärung}

Ich erkläre hiermit, dass ich die vorliegende Arbeit selbstständig und nur unter
Verwendung der angegebenen Literatur und Hilfsmittel angefertigt habe.


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

\chapter{Technologies}
\section{LLVM}
LLVM\cite{lattner2002llvm} is a compiler infrastructure written in C++.
In contrast to GCC it is designed to be used as a library by compilers.
Originally implemented for C and C++, the language-agnostic design (and the success) of LLVM has since spawned a wide variety of front ends: languages with compilers that use LLVM include ActionScript, Ada, D, Fortran, OpenGL Shading Language, Haskell, Java bytecode, Julia, Objective-C, Python, Ruby, Rust, Scala and C.

\subsection{LLVM IR}
LLVM defines it's own language to represent programs.
It uses Static Single Assignment (SSA) form.\cite{alpern1988detecting,rosen1988global}
A program is said to be in SSA form if each of its variables is defined exactly once, and each use of a variable is dominated by that variable’s definition.
SSA form greatly simplifies many dataflow optimizations because only a single definition can reach a particular use of a value, and finding that definition is trivial.

\begin{figure}[htb]
\begin{verbatim}
double dotp(double* a, double* b, int length) {
  double x = 0;
  for (int i=0;i<length;i++) {
    x += a[i]*b[i];
  }
  return x;
}
\end{verbatim}
\end{figure}

\begin{figure}[htb]
\begin{verbatim}
define double @sum(double* %a, i32 %length) {
  %1 = icmp sgt i32 %length, 0
  br i1 %1, label %.lr.ph, label %._crit_edge

.lr.ph:                                           ; preds = %0, %.lr.ph
  %indvars.iv = phi i64 [ %indvars.iv.next, %.lr.ph ], [ 0, %0 ]
  %x.01 = phi double [ %4, %.lr.ph ], [ 0.000000e+00, %0 ]
  %2 = getelementptr double* %a, i64 %indvars.iv
  %3 = load double* %2
  %4 = fadd double %x.01, %3
  %indvars.iv.next = add i64 %indvars.iv, 1
  %lftr.wideiv = trunc i64 %indvars.iv.next to i32
  %exitcond = icmp eq i32 %lftr.wideiv, %length
  br i1 %exitcond, label %._crit_edge, label %.lr.ph

._crit_edge:                                      ; preds = %.lr.ph, %0
  %x.0.lcssa = phi double [ 0.000000e+00, %0 ], [ %4, %.lr.ph ]
  ret double %x.0.lcssa
}
\end{verbatim}
\end{figure}

\section{Accelerate}

\chapter{Contributions}
\section{llvm-general-quote}
When writing a companyiler using LLVM in Haskell there is a good tutorial on how to do it at \citeurl{diehl2014jit}.
It uses \citetitle{scarlet2013llvm} to interface with LLVM.
The general idea is to use a monadic generator to produce the AST on the fly.

Figure \ref{fig:formonad} shows how to implement a simple for loop using monadic generators.
\begin{figure}
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
  c    <- test start
  top  <- cbr c loop exit

  -- Main loop
  setBlock loop
  c_i <- freshName
  let i = local c_i

  body i

  i'   <- incr i
  c'   <- test i'
  bot  <- cbr c' loop exit
  _    <- phi loop c_i ti [(i',bot), (start,top)]

  setBlock exit
\end{code}
\caption{Monadic generation of for loop}
\label{fig:formonad}
\end{figure}
As you can tell this is much boilerplate code.
We have to define the basic blocks manually and add the instructions one by one.
This has some obvious drawbacks, as the code can get unreadable pretty quickly.

A solution is to use quasiquotation\cite{mainland2007quote} instead.
The idea behind quasiquotation is, that you can define a DSL with arbitrary syntax, which you can then directly transform into Haskell data structures.
This is done at compile-time, so you get the same type safety as writing the AST by hand.

\begin{figure}
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
\caption{For Loop using \citetitle{holtz2014quote}}
\label{fig:forquote}
\end{figure}

I implemented \citetitle{holtz2014quote}, a quasiquotation library for LLVM.
Figure \ref{fig:forquote} shows a for loop using my library.

\begin{figure}
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
\caption{Expanded For Loop}
\label{fig:forquote1}
\end{figure}

Figure \ref{fig:forquote1} shows the resulting LLVM IR.
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

\chapter{Conclusion}
\section{Related Work}


\appendix

\backmatter
\sloppy
\printbibliography
\end{document}
