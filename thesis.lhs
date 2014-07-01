\documentclass[a4paper,bibliography=totocnumbered,parskip=half]{scrbook}
%include polycode.fmt
\renewcommand{\tt}{\ttfamily} %bug with lhs2tex
\usepackage{lhs}
\usepackage[utf8]{inputenc}
\usepackage[backend=biber,citestyle=alphabetic,bibstyle=din]{biblatex}
\usepackage[english]{babel}
\usepackage{csquotes}
\usepackage{hyperref}
\usepackage{url}
\usepackage{scrhack}
\usepackage{graphicx}
\usepackage{courier}
\usepackage[T1]{fontenc}
\usepackage{amsmath}
\usepackage[obeyFinal]{todonotes}
\usepackage{multirow}% http://ctan.org/pkg/multirow
\usepackage{listings}

\lstset{basicstyle=\footnotesize\ttfamily,
        breaklines=true,
        numbers=left,
        numberstyle=\tiny,
%        frame=single,
        language=llvm}

% Kapitelüberschrift in der Kopfzeile
\usepackage[automark]{scrpage2} % Schickerer Satzspiegel mit KOMA-Script
\pagestyle{scrheadings}
\setheadsepline{.5pt}

\widowpenalty=1000
\clubpenalty=1000
% Use cite/entry key as fallback label
\DeclareLabelalphaTemplate{
  \labelelement{
    \field[final]{shorthand}
    \field{label}
    \field[strwidth=3,strside=left,ifnames=1]{labelname}
    \field[strwidth=1,strside=left]{labelname}
    \field[final,strwidth=3]{citekey}
  }
  \labelelement{
    \field[strwidth=2,strside=right]{year}
  }
}

% Add labels to bibliography - taken from alphabetic.bbx
\DeclareFieldFormat{labelalphawidth}{\mkbibbrackets{#1}}
\defbibenvironment{bibliography}
  {\list
     {\printtext[labelalphawidth]{%
        \printfield{prefixnumber}%
        \printfield{labelalpha}%
        \printfield{extraalpha}}}
     {\setlength{\labelwidth}{\labelalphawidth}%
      \setlength{\leftmargin}{\labelwidth}%
      \setlength{\labelsep}{\biblabelsep}%
      \addtolength{\leftmargin}{\labelsep}%
      \setlength{\itemsep}{\bibitemsep}%
      \setlength{\parsep}{\bibparsep}}%
      \renewcommand*{\makelabel}[1]{##1\hss}}
  {\endlist}
  {\item}
\DefineBibliographyStrings{english}{%
  bibliography = {References},
}
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

\begin{titlepage}

\begin{center}
{\huge \textit{Master Thesis}}

\vspace{2cm}

{\Large \textbf{An LLVM Backend for Accelerate}}

\vspace{1.75cm}

\includegraphics[height=6cm]{CAU-Siegel}

\vspace{1.75cm}

{\large Christian-Albrechts-Universität zu Kiel\\
   Department of Computer Science\\
   Programming Languages and Compiler Construction
}

\end{center}

\vspace{2cm}

\begin{tabular}{ll}
student: & \textbf{Timo von Holtz} \\
\multirow{2}{*}{advised by:} & Priv.-Doz. Dr. Frank Huch\\
 & Assoc. Prof. Dr. Manuel M. T. Chakravarty
\end{tabular}

\vspace{1cm}

\begin{center}
Kiel, \today
\end{center}

\end{titlepage}

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

To get idea of how this looks like in practice, let's look at an example.
Figure \ref{fig:sumc} shows a simple C function to sum up the elements of an array. The corresponding LLVM code is shown in figure \ref{fig:sumll}.

\begin{figure}
\begin{lstlisting}[language=C]
double sum(double* a, int length) {
  double x = 0;
  for (int i=0;i<length;i++) {
    x += a[i];
  }
  return x;
}
\end{lstlisting}
\caption{sum as a C function}
\label{fig:sumc}
\end{figure}

\begin{figure}
\begin{lstlisting}
define double @@sum(double*  %a, i32 %length) {
entry:
  %cmp4 = icmp sgt i32 %length, 0
  br i1 %cmp4, label %for.body, label %for.end

for.body:                                         ; preds = %entry, %for.body
  %indvars.iv = phi i64 [ %indvars.iv.next, %for.body ], [ 0, %entry ]
  %x.05 = phi double [ %add, %for.body ], [ 0.000000e+00, %entry ]
  %arrayidx = getelementptr inbounds double* %a, i64 %indvars.iv
  %0 = load double* %arrayidx, align 8
  %add = fadd double %x.05, %0
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  %lftr.wideiv = trunc i64 %indvars.iv.next to i32
  %exitcond = icmp eq i32 %lftr.wideiv, %length
  br i1 %exitcond, label %for.end, label %for.body

for.end:                                          ; preds = %for.body, %entry
  %x.0.lcssa = phi double [ 0.000000e+00, %entry ], [ %add, %for.body ]
  ret double %x.0.lcssa
}
\end{lstlisting}
\caption{sum as a LLVM}
\label{fig:sumll}
\end{figure}

The first obvious difference is how the for-loop is translated.
In LLVM every function is divided into basic blocks.
A basic block is a continuous stream of instructions with a terminator at the end.
Instructions are  add, mult, call, \dots, but also call.
Terminators can either be used to jump to another block (branch) or return to the calling function.

To allow for dynamic control flow, there are $\Phi$-nodes.
These specify the the value of a new variable depending on what the last block was.
The $\Phi$-nodes in the SSA are represented with \lstinline{phi}-instructions.
In LLVM, these have to preceed every other instruction in a given basic block.

\subsubsection{Types}
The LLVM type system is one of the most important features of the intermediate representation.
Being typed enables a number of optimizations to be performed directly, without having to do extra analyses on the side before the transformation.

Important Types are: \todo{description of types}
\begin{itemize}
\item \lstinline{void}, which represents no value
\item integers with specified length N: \lstinline{iN}
\item floating point numbers: \lstinline{half, float, double, ...}
\item pointers: \lstinline{<type> *}
\item function types: \lstinline{<returntype> (<parameter list>)}
\item vector types: \lstinline{< <# elements> x <elementtype> >}
\item array types: \lstinline{[<# elements> x <elementtype>]}
\item structure types: \lstinline!{ <type list> }!
\end{itemize}

\subsection{Vectorization}
Modern CPUs all have SIMD units to execute an instruction on multiple datasets in parallel.
Usind these units is easy with LLVM.
All operations (\lstinline{add}, \lstinline{fadd}, \lstinline{sub}, \dots) can be used with vector arguments the same way as with scalar arguments.

To manually exploit this can be tricky however.
LLVM has multiple strategies to fuse similar instructions or tight inner loops into vectorized code.

\subsubsection{Loop Vectorizer}


\subsubsection{SLP Vectorizer}
The goal of SLP vectorization (a.k.a. superword-level parallelism) is to combine similar independent instructions into vector instructions. Memory accesses, arithmetic operations, comparison operations, PHI-nodes, can all be vectorized using this technique.
For example, the following function performs very similar operations on its inputs (a1, b1) and (a2, b2). The basic-block vectorizer may combine these into vector operations.

\begin{lstlisting}[language=C]
void foo(int a1, int a2, int b1, int b2, int *A) {
  A[0] = a1*(a1 + b1)/b1 + 50*b1/a1;
  A[1] = a2*(a2 + b2)/b2 + 50*b2/a2;
}
\end{lstlisting}

\subsubsection{Fast-Math Flags}
To vectorize code the operations involved need to be associative.
When working with floating point numbers, this property is violated.
To vectorize the code regardless, LLVM has the notion of fast-math-flags.
These tell the optimizer to assume certain properties that aren't true in general.

Available flags are\cite{llvmref}:
\begin{itemize}
\item[nnan] No NaNs - Allow optimizations to assume the arguments and result are not NaN. Such optimizations are required to retain defined behavior over NaNs, but the value of the result is undefined.
\item[ninf] No Infs - Allow optimizations to assume the arguments and result are not +/-Inf. Such optimizations are required to retain defined behavior over +/-Inf, but the value of the result is undefined.
\item[nsz] No Signed Zeros - Allow optimizations to treat the sign of a zero argument or result as insignificant.
\item[arcp] Allow Reciprocal - Allow optimizations to use the reciprocal of an argument rather than perform division.
\item[fast] Fast - Allow algebraically equivalent transformations that may dramatically change results in floating point (e.g. reassociate). This flag implies all the others.
\end{itemize}

\section{Accelerate}
Similar to Repa\cite{keller2010regular}, uses gang workers.\cite{chakravarty2007data}
\chapter{Contributions}
\section{llvm-general}
\begin{itemize}
\item Targetmachine (Optimization)
\item fast-math
\end{itemize}
\section{llvm-general-quote}
When writing a compiler using LLVM in Haskell there is a good tutorial on how to do it at \citeurl{diehl2014jit}.
It uses \citetitle{scarlet2013llvm} to interface with LLVM.
The Idea followed in this tutorial is to use a monadic generator to produce the AST.

\todo{longer introduction to monadic code generation}

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
Unfortunately, this produces a lot of boilerplate code.
We have to define the basic blocks manually and add the instructions one by one.
This has some obvious drawbacks, as the code can get unreadable pretty quickly.

Another approach would be to use an EDSL.
\todo{write about llvm-general-typed}

I propose a third approach using quasiquotation\cite{mainland2007quote}.
The idea behind quasiquotation is, that you can define a DSL with arbitrary syntax, which you can then directly transform into Haskell data structures.
This is done at compile-time, so you get the same type safety as writing the AST by hand.

\begin{figure}
\begin{lstlisting}
define i32 @@foo(i32 %x) #0 {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %res.0 = phi i32 [ 0, %entry ], [ %add, %for.inc ]
  %i.0 = phi i32 [ 0, %entry ], [ %inc, %for.inc ]
  %cmp = icmp slt i32 %i.0, %x
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %add = add nsw i32 %res.0, %x
  %inc = add nsw i32 %i.0, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  ret i32 %res.0
}
\end{lstlisting}
\label{fig:simplequote}
\end{figure}

Figure \ref{fig:simplequote} shows how to implement a simple function in LLVM using quasiquotation.
Compared to the other 2 solutions, this has already the advantage of being very close to the produced LLVM IR.

Without the ability to reference Haskell variables, this would be fairly useless in most cases.
But as quasiquotation allows for antiquotation as well.
This means you can still reference arbitrary Haskell variables from within the quotation.
Using this the following are equivalent:
\begin{itemize}
 \item \lstinline{[llinstr|| add i64 %x, 1 |]}
 \item \lstinline{let y = 1 in [llinstr|| add i64 %x, $opr:y |]}
\end{itemize}

But you still have to specify the $\Phi$-nodes by hand.
This is fairly straight forward in a simple example, but can get more complicated very quickly.

The real goal is a language that ``feels'' like a high-level language, but can be trivially translated into LLVM.
This means
\begin{itemize}
 \item using LLVM instructions unmodified.
 \item introduce higher-level control structures like for, while and if-then-else.
\end{itemize}

\begin{figure}
\begin{lstlisting}
[llg||
define i64 @@foo(i64 %start, i64 %end) {
  entry:
    br label %for

  for:
    for i64 %i in %start to %end with i64 [0,%entry] as %x {
        %y = add i64 %i, %x
        ret i64 %y
    }

  exit:
    ret i64 %y
}
||]
\end{lstlisting}
\caption{For Loop using \citetitle{holtz2014quote}}
\label{fig:forquote}
\end{figure}

Figure \ref{fig:forquote} shows a for loop using this approach.
The loop-variable (\%x in this case) is specified explicitly and can be referenced inside and after the for-loop.
To update the \%x, I overload the return statement to specify which value should be propagated.
Figure \ref{fig:forquote1} shows the produced LLVM code. \todo{update figure to correct ordering of bbs}
This is clearly more readable.

\begin{figure}
\begin{lstlisting}
define i64 @@foo(i64 %start, i64 %end) {
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
\end{lstlisting}
\caption{Expanded For Loop}
\label{fig:forquote1}
\end{figure}

This approach unfortunately only works for some well-defined cases.
With multiple loop-variables for example, it quickly becomes cluttered.
On top of this, it relies on some ``magic'' to resolve the translation and legibility suffers.

The main reason why it is not possible to define a better syntax is that LLVM code has to be in SSA form.
This means, that
\begin{enumerate}
 \item every variable name can only be written to once.
 \item $\Phi$-nodes are necessary where control flow merges.
\end{enumerate}

To loosen this restriction, I implemented SSA recovery pass as part of the quasiquoter.
Figure \ref{fig:forquoteSSA} and \ref{fig:forquoteSSA1} show the quoted and the produced code respectively.
\footnote{The produced code is after applying the InstCombine optimization as I place more phi-nodes than necessary.}

\begin{figure}
\begin{lstlisting}
[llg||
define i64 @@foo(i64 %start, i64 %end) {
  entry:
    %x = i64 0

  for:
    for i64 %i in %start to %end {
        %x = add i64 %i, %x
    }

  exit:
    ret i64 %x
}
||]
\end{lstlisting}
\caption{For Loop using \citetitle{holtz2014quote} and SSA}
\label{fig:forquoteSSA}
\end{figure}

\begin{figure}
\begin{lstlisting}
define i64 @@foo(i64 %start, i64 %end) {
entry:
  br label %for.head

for.head:                                         ; preds = %n0, %entry
  %x.12 = phi i64 [ 0, %entry ], [ %x.6, %n0 ]
  %i.4 = phi i64 [ %start, %entry ], [ %i.9, %n0 ]
  %for.cond.3 = icmp slt i64 %i.4, %end
  br i1 %for.cond.3, label %n0, label %for.end

n0:                                               ; preds = %for.head
  %x.6 = add i64 %i.4, %x.12
  %i.9 = add nuw nsw i64 %i.4, 1
  br label %for.head

for.end:                                          ; preds = %for.head
  ret i64 %x.12
}

\end{lstlisting}
\caption{Expanded For Loop (SSA)}
\label{fig:forquoteSSA1}
\end{figure}

\chapter{Implementation}
\section{Quasiquoter}
The design of \citetitle{holtz2014quote} is inspired by \citetitle{mainland2007c}, which is also used in the cuda implementation of Accelerate.
I use \citetitle{gill1995happy} and \citetitle{alex}.
\section{Extension for-loop}
\section{SSA}
The standard method to achieve this is to use stack allocated variables instead of registers.
The LLVM optimizer then uses the mem2reg pass to transform these into registers, adding the necessary phi-nodes in the process.
\cite{braun13simple}
\chapter{Skeletons}
\section{map}
\section{fold}
\section{scan}
\cite{ladner1980parallel}
My plan is the following:

\begin{lstlisting}
void scan(double* in, double* out, double* tmp, unsigned length, unsigned start, unsigned end, unsigned tid) {
  double acc = 0;
  if (end==length) {
    tmp[0] = 0;
  } else {
    for (unsigned i=start;i<end;i++) {
      acc += in[i];
    }
    tmp[tid+1] = acc;
  }
  printf("block");
  acc = tmp[tid];
  for (unsigned j=start;j<end;j++) {
    acc += in[j];
    out[j] = acc;
  }
}

void scanAlt(double* in, double* out, unsigned length, unsigned start, unsigned end) {
  double acc = 0;
  for (unsigned j=start;j<end;j++) {
    acc += in[j];
    out[j] = acc;
  }
  printf("block");
  double add=0;
  if (start>0) {
    add = out[start-1];
  }
  for (unsigned i=start;i<end;i++) {
    out[i] += add;
  }
}
\end{lstlisting}

\chapter{Conclusion}
\section{Related Work}


\appendix
\chapter{Listings}
\begin{lstlisting}[language=llvm]
; Function Attrs: nounwind readonly
define double @@sum(double* nocapture readonly %a, i32 %length) #0 {
  %1 = icmp sgt i32 %length, 0
  br i1 %1, label %.lr.ph.preheader, label %._crit_edge

.lr.ph.preheader:                                 ; preds = %0
  %2 = add i32 %length, -1
  %3 = zext i32 %2 to i64
  %4 = add i64 %3, 1
  %end.idx = add i64 %3, 1
  %n.vec = and i64 %4, 8589934576
  %cmp.zero = icmp eq i64 %n.vec, 0
  br i1 %cmp.zero, label %middle.block, label %vector.body

vector.body:                                      ; preds = %.lr.ph.preheader, %vector.body
  %index = phi i64 [ %index.next, %vector.body ], [ 0, %.lr.ph.preheader ]
  %vec.phi = phi <4 x double> [ %13, %vector.body ],
                              [ zeroinitializer, %.lr.ph.preheader ]
  %vec.phi6 = phi <4 x double> [ %14, %vector.body ],
                               [ zeroinitializer, %.lr.ph.preheader ]
  %vec.phi7 = phi <4 x double> [ %15, %vector.body ],
                               [ zeroinitializer, %.lr.ph.preheader ]
  %vec.phi8 = phi <4 x double> [ %16, %vector.body ],
                               [ zeroinitializer, %.lr.ph.preheader ]
  %5 = getelementptr double* %a, i64 %index
  %6 = bitcast double* %5 to <4 x double>*
  %wide.load = load <4 x double>* %6, align 8
  %.sum22 = or i64 %index, 4
  %7 = getelementptr double* %a, i64 %.sum22
  %8 = bitcast double* %7 to <4 x double>*
  %wide.load9 = load <4 x double>* %8, align 8
  %.sum23 = or i64 %index, 8
  %9 = getelementptr double* %a, i64 %.sum23
  %10 = bitcast double* %9 to <4 x double>*
  %wide.load10 = load <4 x double>* %10, align 8
  %.sum24 = or i64 %index, 12
  %11 = getelementptr double* %a, i64 %.sum24
  %12 = bitcast double* %11 to <4 x double>*
  %wide.load11 = load <4 x double>* %12, align 8
  %13 = fadd <4 x double> %vec.phi, %wide.load
  %14 = fadd <4 x double> %vec.phi6, %wide.load9
  %15 = fadd <4 x double> %vec.phi7, %wide.load10
  %16 = fadd <4 x double> %vec.phi8, %wide.load11
  %index.next = add i64 %index, 16
  %17 = icmp eq i64 %index.next, %n.vec
  br i1 %17, label %middle.block, label %vector.body, !llvm.loop !0

middle.block:                                     ; preds = %vector.body, %.lr.ph.preheader
  %resume.val = phi i64 [ 0, %.lr.ph.preheader ], [ %n.vec, %vector.body ]
  %rdx.vec.exit.phi = phi <4 x double> [ zeroinitializer, %.lr.ph.preheader ],
                                       [ %13, %vector.body ]
  %rdx.vec.exit.phi14 = phi <4 x double> [ zeroinitializer, %.lr.ph.preheader ],
                                         [ %14, %vector.body ]
  %rdx.vec.exit.phi15 = phi <4 x double> [ zeroinitializer, %.lr.ph.preheader ],
                                         [ %15, %vector.body ]
  %rdx.vec.exit.phi16 = phi <4 x double> [ zeroinitializer, %.lr.ph.preheader ],
                                         [ %16, %vector.body ]
  %bin.rdx = fadd <4 x double> %rdx.vec.exit.phi14, %rdx.vec.exit.phi
  %bin.rdx17 = fadd <4 x double> %rdx.vec.exit.phi15, %bin.rdx
  %bin.rdx18 = fadd <4 x double> %rdx.vec.exit.phi16, %bin.rdx17
  %rdx.shuf = shufflevector <4 x double> %bin.rdx18, <4 x double> undef, <4 x i32> <i32 2, i32 3, i32 undef, i32 undef>
  %bin.rdx19 = fadd <4 x double> %bin.rdx18, %rdx.shuf
  %rdx.shuf20 = shufflevector <4 x double> %bin.rdx19, <4 x double> undef, <4 x i32> <i32 1, i32 undef, i32 undef, i32 undef>
  %bin.rdx21 = fadd <4 x double> %bin.rdx19, %rdx.shuf20
  %18 = extractelement <4 x double> %bin.rdx21, i32 0
  %cmp.n = icmp eq i64 %end.idx, %resume.val
  br i1 %cmp.n, label %._crit_edge, label %.lr.ph

.lr.ph:                                           ; preds = %middle.block, %.lr.ph
  %indvars.iv = phi i64 [ %indvars.iv.next, %.lr.ph ], [ %resume.val, %middle.block ]
  %x.01 = phi double [ %21, %.lr.ph ], [ %18, %middle.block ]
  %19 = getelementptr double* %a, i64 %indvars.iv
  %20 = load double* %19, align 8
  %21 = fadd fast double %x.01, %20
  %indvars.iv.next = add i64 %indvars.iv, 1
  %lftr.wideiv1 = trunc i64 %indvars.iv.next to i32
  %exitcond2 = icmp eq i32 %lftr.wideiv1, %length
  br i1 %exitcond2, label %._crit_edge, label %.lr.ph, !llvm.loop !3

._crit_edge:                                      ; preds = %.lr.ph, %middle.block, %0
  %x.0.lcssa = phi double [ 0.000000e+00, %0 ], [ %21, %.lr.ph ], [ %18, %middle.block ]
  ret double %x.0.lcssa
}

attributes #0 = { nounwind readonly }

!0 = metadata !{metadata !0, metadata !1, metadata !2}
!1 = metadata !{metadata !"llvm.vectorizer.width", i32 1}
!2 = metadata !{metadata !"llvm.vectorizer.unroll", i32 1}
!3 = metadata !{metadata !3, metadata !1, metadata !2}
\end{lstlisting}

\backmatter
\sloppy
\printbibliography[heading=bibintoc]
\end{document}
