\documentclass[a4paper,bibliography=totocnumbered,parskip=half]{scrbook}
%include polycode.fmt
\renewcommand{\tt}{\ttfamily} %bug with lhs2tex
\usepackage{lhs}
\usepackage[utf8]{inputenc}
\usepackage[backend=biber,style=numeric]{biblatex}
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
        frame=single,
        language=llvm}

% Kapitelüberschrift in der Kopfzeile
\usepackage[automark]{scrpage2} % Schickerer Satzspiegel mit KOMA-Script
\pagestyle{scrheadings}
\setheadsepline{.5pt}

\widowpenalty=1000
\clubpenalty=1000
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

\pagestyle{empty}

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
double dotp(double* a, double* b, int length) {
  double x = 0;
  for (int i=0;i<length;i++) {
    x += a[i]*b[i];
  }
  return x;
}
\end{lstlisting}
\caption{sum as a C function}
\label{fig:sumc}
\end{figure}

\begin{figure}
\begin{lstlisting}
define double @@sum(double* %a, i32 %length) {
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
\end{lstlisting}
\caption{sum as a LLVM}
\label{fig:sumll}
\end{figure}

The first obvious difference is how the for-loop is translated.
In LLVM every function is divided into basic blocks.
A basic block is a continuous stream of instructions (add, mult, call, ...) with a terminator at the end.
Terminators can either be used to jump to another block (branch) or return to the calling function.

The $\Phi$--nodes in the SSA are represented with \lstinline{phi}-instructions.
These have to preceed every other instruction in a basic block.

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
All operations (\lstinline{add}, \lstinline{fadd}, \lstinline{sub}, ...) can be used with vector arguments the same way as with scalar arguments.

To manually exploit this can be tricky however.
LLVM has multiple strategies to fuse similar instructions or tight inner loops into vectorized code.



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
}
||]
\end{lstlisting}
\caption{For Loop using \citetitle{holtz2014quote}}
\label{fig:forquote}
\end{figure}

I implemented \citetitle{holtz2014quote}, a quasiquotation library for LLVM.
Figure \ref{fig:forquote} shows a for loop using my library.

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

Figure \ref{fig:forquote1} shows the resulting LLVM IR.
This is clearly more readable.
Furthermore, one can see much more clearly what the produced code will be.

Another advantage of quasiquotation is antiquotation.
This means you can still reference arbitrary Haskell variables from within the quotation.
Using this the following are equivalent:
\begin{itemize}
\item \lstinline{[llinstr|| add i64 %x, 1 |]}
\item \lstinline{let y = 1 in [llinstr|| add i64 %x, $opr:(y) |]}
\end{itemize}

The design of \citetitle{holtz2014quote} is inspired by \citetitle{mainland2007c}, which is also used in the cuda implementation of Accelerate.
I use \citetitle{gill1995happy} and \citetitle{alex}.

\section{Skeletons}
\subsection{map}
\subsection{fold}
\subsection{scan}
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
