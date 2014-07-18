\documentclass[a4paper,bibliography=totocnumbered,parskip=half]{scrbook}
%include polycode.fmt
%include greek.fmt
%include forall.fmt
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
\usepackage{enumitem}
\usepackage[section]{placeins}
\usepackage{tikz}
\usetikzlibrary{shapes.geometric, arrows}

\lstset{basicstyle=\footnotesize\ttfamily,
        breaklines=true,
        numbers=left,
        numberstyle=\tiny,
        xleftmargin=.07\textwidth,
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

\tikzstyle{startstop} = [rectangle, rounded corners, minimum width=3cm, minimum height=1cm,text centered, draw=black, fill=red!30]
\tikzstyle{io} = [trapezium, trapezium left angle=70, trapezium right angle=110, minimum width=3cm, minimum height=1cm, text centered, draw=black, fill=blue!30]
\tikzstyle{process} = [rectangle, minimum width=3cm, minimum height=1cm, text centered, draw=black, fill=orange!30]
\tikzstyle{decision} = [diamond, minimum width=3cm, minimum height=1cm, text centered, draw=black, fill=green!30]
\tikzstyle{arrow} = [thick,->,>=stealth]

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

\chapter*{Abstract}
Implementing parallel computations is traditionally done in C or FORTRAN.
This allows great control over the details of execution.
Unfortunately, this also comes with the complexities of using a low-level language.
To make this easier, \citeauthor{chakravarty2011accelerating} have developed Accelerate, an embedded array language for computations for high-performance computing in Haskell.
The main execution target of this EDSL is GPUs via CUDA.

\citeauthor{trevor2014llvm} started working on an alternative implementation using LLVM.
I contribute to this work by implementing a different approach to code-generation for LLVM using quasi-quotation.
I use this quasi-quoter to implement the skeletons in the LLVM backend.

% Verzeichnisse
\listoftodos
\tableofcontents   % Inhaltsverzeichnis
\listoffigures     % Abbildungsverzeichnis
\listoftables      % Tabellenverzeichnis
\mainmatter

\chapter{Introduction}
\todo{write introduction}

\chapter{Technologies}
\todo{write intro to Technologies chapter}
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
int sum(int* a, int length) {
  int x = 0;
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
define i32 @@sum(i32* nocapture readonly %a, i32 %length) {
entry:
  %cmp4 = icmp sgt i32 %length, 0
  br i1 %cmp4, label %for.body, label %for.end

for.body:                                         ; preds = %entry, %for.body
  %indvars.iv = phi i64 [ %indvars.iv.next, %for.body ], [ 0, %entry ]
  %x.05 = phi i32 [ %add, %for.body ], [ 0, %entry ]
  %arrayidx = getelementptr inbounds i32* %a, i64 %indvars.iv
  %0 = load i32* %arrayidx, align 4
  %add = add nsw i32 %0, %x.05
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  %lftr.wideiv = trunc i64 %indvars.iv.next to i32
  %exitcond = icmp eq i32 %lftr.wideiv, %length
  br i1 %exitcond, label %for.end, label %for.body

for.end:                                          ; preds = %for.body, %entry
  %x.0.lcssa = phi i32 [ 0, %entry ], [ %add, %for.body ]
  ret i32 %x.0.lcssa
}
\end{lstlisting}
\caption{sum as a LLVM}
\label{fig:sumll}
\end{figure}

The first obvious difference is the lack of sophisticated control structures.
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

Important Types are:
\begin{itemize}[noitemsep]
\item[-] \lstinline{void}, which represents no value
\item[-] integers with specified length N: \lstinline{iN}
\item[-] floating point numbers: \lstinline{half, float, double, ...}
\item[-] pointers: \lstinline{<type> *}
\item[-] function types: \lstinline{<returntype> (<parameter list>)}
\item[-] vector types: \lstinline{< <# elements> x <elementtype> >}
\item[-] array types: \lstinline{[<# elements> x <elementtype>]}
\item[-] structure types: \lstinline!{ <type list> }!
\end{itemize}

\subsection{Vectorization}
Modern CPUs all have SIMD units to execute an instruction on multiple datasets in parallel.
Usind these units is easy with LLVM.
All operations (\lstinline{add}, \lstinline{fadd}, \lstinline{sub}, \dots) can be used with vector arguments the same way as with scalar arguments.

To manually exploit this can be tricky however.
LLVM has multiple strategies to fuse similar instructions or tight inner loops into vectorized code.\cite{llvmauto}

\subsubsection{Loop Vectorizer}
The Loop Vectorizer tries to vectorize tight inner loops.
To get an idea of how these work, lets look at the example from figure \ref{fig:sumc}.
To build the sum, every element of the array is added to an accumulator.
Since addition is associative and commutative, the additions can be reordered.
Given a vector width of 2, the sum of $2*n$ and $2*n+1$ are calculated in parallel and then added together.
In addition to this, the vectorizer will also unroll the inner loop to make fewer jumps necessary.
Figure \ref{fig:sumllvec} shows the corresponding llvm code without loop unroll, as this increases code size dramatically.

\begin{figure}
\begin{lstlisting}[basicstyle=\scriptsize\ttfamily, xleftmargin=0pt]
define i32 @@sum(i32* nocapture readonly %a, i32 %length) #0 {
entry:
  %cmp4 = icmp sgt i32 %length, 0
  br i1 %cmp4, label %for.body.preheader, label %for.end

for.body.preheader:                               ; preds = %entry
  %0 = add i32 %length, -1
  %1 = zext i32 %0 to i64
  %2 = add i64 %1, 1
  %end.idx = add i64 %1, 1
  %n.vec = and i64 %2, 8589934590
  %cmp.zero = icmp eq i64 %n.vec, 0
  br i1 %cmp.zero, label %middle.block, label %vector.body

vector.body:                                      ; preds = %for.body.preheader, %vector.body
  %index = phi i64 [ %index.next, %vector.body ], [ 0, %for.body.preheader ]
  %vec.phi = phi <2 x i32> [ %5, %vector.body ], [ zeroinitializer, %for.body.preheader ]
  %3 = getelementptr inbounds i32* %a, i64 %index
  %4 = bitcast i32* %3 to <2 x i32>*
  %wide.load = load <2 x i32>* %4, align 4
  %5 = add nsw <2 x i32> %wide.load, %vec.phi
  %index.next = add i64 %index, 2
  %6 = icmp eq i64 %index.next, %n.vec
  br i1 %6, label %middle.block, label %vector.body

middle.block:                                     ; preds = %vector.body, %for.body.preheader
  %resume.val = phi i64 [ 0, %for.body.preheader ], [ %n.vec, %vector.body ]
  %rdx.vec.exit.phi = phi <2 x i32> [ zeroinitializer, %for.body.preheader ], [ %5, %vector.body ]
  %rdx.shuf = shufflevector <2 x i32> %rdx.vec.exit.phi, <2 x i32> undef, <2 x i32> <i32 1, i32 undef>
  %bin.rdx = add <2 x i32> %rdx.vec.exit.phi, %rdx.shuf
  %7 = extractelement <2 x i32> %bin.rdx, i32 0
  %cmp.n = icmp eq i64 %end.idx, %resume.val
  br i1 %cmp.n, label %for.end, label %for.body

for.body:                                         ; preds = %middle.block, %for.body
  %indvars.iv = phi i64 [ %indvars.iv.next, %for.body ], [ %resume.val, %middle.block ]
  %x.05 = phi i32 [ %add, %for.body ], [ %7, %middle.block ]
  %arrayidx = getelementptr inbounds i32* %a, i64 %indvars.iv
  %8 = load i32* %arrayidx, align 4
  %add = add nsw i32 %8, %x.05
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  %lftr.wideiv = trunc i64 %indvars.iv.next to i32
  %exitcond = icmp eq i32 %lftr.wideiv, %length
  br i1 %exitcond, label %for.end, label %for.body

for.end:                                          ; preds = %for.body, %middle.block, %entry
  %x.0.lcssa = phi i32 [ 0, %entry ], [ %add, %for.body ], [ %7, %middle.block ]
  ret i32 %x.0.lcssa
}
\end{lstlisting}
\caption{Vectorized Sum}
\label{fig:sumllvec}
\end{figure}

Apart from reductions, LLVM can also vectorize the following:
\begin{itemize}[noitemsep]
 \item[-] Loops with unknown trip count
 \item[-] Runtime Checks of Pointers
 \item[-] Reductions
 \item[-] Inductions
 \item[-] If Conversion
 \item[-] Pointer Induction Variables
 \item[-] Reverse Iterators
 \item[-] Scatter / Gather
 \item[-] Vectorization of Mixed Types
 \item[-] Global Structures Alias Analysis
 \item[-] Vectorization of function calls
 \item[-] Partial unrolling during vectorization
\end{itemize}
A detailed description can be found at \citeurl{llvmauto}

\subsubsection{SLP Vectorizer}
The goal of SLP vectorization (a.k.a. superword-level parallelism) is to combine similar independent instructions into vector instructions.
Memory accesses, arithmetic operations, comparison operations, PHI-nodes, can all be vectorized using this technique.
For example, the following function performs very similar operations on its inputs (a1, b1) and (a2, b2).

\begin{lstlisting}[language=C]
void foo(double a1, double a2, double b1, double b2, double *A) {
  A[0] = a1*(a1 + b1)/b1 + 50*b1/a1;
  A[1] = a2*(a2 + b2)/b2 + 50*b2/a2;
}
\end{lstlisting}

The SLP vectorizer may combine these into vector operations.
Figures \ref{fig:slpll1} and \ref{fig:slpll2} show the the corresponding LLVM before and after SLP vectorization.

\begin{figure}
\begin{lstlisting}
define void @@foo(double %a1, double %a2, double %b1, double %b2, double* %A) {
entry:
  %add = fadd double %a1, %b1
  %mul = fmul double %add, %a1
  %div = fdiv double %mul, %b1
  %mul1 = fmul double %b1, 5.000000e+01
  %div2 = fdiv double %mul1, %a1
  %add3 = fadd double %div, %div2
  store double %add3, double* %A, align 8
  %add4 = fadd double %a2, %b2
  %mul5 = fmul double %add4, %a2
  %div6 = fdiv double %mul5, %b2
  %mul7 = fmul double %b2, 5.000000e+01
  %div8 = fdiv double %mul7, %a2
  %add9 = fadd double %div6, %div8
  %arrayidx10 = getelementptr inbounds double* %A, i64 1
  store double %add9, double* %arrayidx10, align 8
  ret void
}
\end{lstlisting}
\caption{\lstinline{foo} without SLP vectorization}
\label{fig:slpll1}
\end{figure}

\begin{figure}
\begin{lstlisting}
define void @@foo(double %a1, double %a2, double %b1, double %b2, double* %A) {
entry:
  %0 = insertelement <2 x double> undef, double %a1, i32 0
  %1 = insertelement <2 x double> %0, double %a2, i32 1
  %2 = insertelement <2 x double> undef, double %b1, i32 0
  %3 = insertelement <2 x double> %2, double %b2, i32 1
  %4 = fadd <2 x double> %1, %3
  %5 = fmul <2 x double> %4, %1
  %6 = fdiv <2 x double> %5, %3
  %7 = fmul <2 x double> %3, <double 5.000000e+01, double 5.000000e+01>
  %8 = fdiv <2 x double> %7, %1
  %9 = fadd <2 x double> %6, %8
  %10 = bitcast double* %A to <2 x double>*
  store <2 x double> %9, <2 x double>* %10, align 8
  ret void
}
\end{lstlisting}
\caption{\lstinline{foo} with SLP vectorization}
\label{fig:slpll2}
\end{figure}

\subsubsection{Fast-Math Flags}
In order for the Loop vectorizer to work, the operations involved need to be associative.
When dealing with floating point numbers, the basic operations like \lstinline{fadd} and \lstinline{fmul} don't satisfy this condition.
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

\subsection{llvm-general}
There are multiple bindings to LLVM in Haskell.
The most complete is llvm-general.\cite{scarlet2013llvm}
Instead of exposing the LLVM API directly, it uses an ADT to represent LLVM IR.
This can then be translated into the corresponding C++ object.
It also supports the other way, transforming the object back into the ADT.

Using an ADT instead of writing directly to the C++ object has many advantages.
This way, the code can be produced and manipulated outside the IO context.
It is also much easier to reason about within Haskell.

llvm-general also supports optimization and jit-compilation.
The optimization is controlled by a |PassSetSpec|
This offers the user 2 different options: |CuratedPassSetSpec| and |PassSetSpec|.
Using |CuratedPassSetSpec| is the easier option of the 2.
It offers a similar level of control as specifying @-On@ at the command line.
Using |PassSetSpec| gives much more control over the exact passes run, but you have to specify them one by one.
Both of these specs also come with fields to specify information about the target.


\section{Accelerate}
Accelerate\cite{chakravarty2011accelerating,mcdonelloptimising} is an an embedded array language for computations for high-performance computing in Haskell.
Computations on multi-dimensional, regular arrays are expressed in the form of parameterised collective operations, such as maps, reductions, and permutations.
It is very similar to Repa\cite{keller2010regular} in the way computations are specified.
The difference is in the way the computation gets evaluated.
While Repa produces its result immediately, Accelerate collects the computation which can then be executed.
This approach is necessaray, as Accelerate isn't limited to be run on a CPU.
The main back-end of accelerate is in fact based on CUDA.

\subsection{Usage}
To give an example of how to use Accelerate, lets look at the dot product of 2 vectors.
This could easily implemented in Haskell like this.

\begin{code}
dotp_list :: [Float] -> [Float] -> Float
dotp_list xs ys = foldl (+) 0 (zipWith (*) xs ys)
\end{code}

You can write nearly exactly the same function in Accelerate:

\begin{code}
dotp :: Acc (Vector Float)
     -> Acc (Vector Float)
     -> Acc (Scalar Float)
dotp xs ys = fold (+) 0 (zipWith (*) xs ys)
\end{code}

The first is the difference is the use of a different datastructure.
A |Vector| is an |Array| with one dimension.
Similarly, a |Scalar| is an |Array| with 0 dimensions.
These are then wrapped in the |Acc| type.
|Acc a| is a computation, that yields an |a|.

The second difference is the |fold|.
Unlike the |foldl| in the Haskell example, this |fold| doesn't specify the order of operations.
This is important as it allows for an efficient implementation.
In order of this to work, the operation passed to |fold| has to be associative.

If we want to call this function, we have to somehow produce values of type |Acc (Vector Float)|.

First the list is converted with |fromList|.
To see what it does, lets first look at its type.

\begin{code}
fromList :: (Elt e, Shape sh) => sh -> [e] -> Array sh e
\end{code}

In addition to the list of elements, |fromList| takes the shape of the resulting |Array|.
Shapes can have an arbitrary amount of dimensions, but in this case it is only one.
To construct a shape, there are 2 datatypes:

\begin{code}
data Z = Z
data tail :. head = tail :. head
\end{code}

The head in this case represents the innermost dimension.
The tail has to also be a shape.

With this, we get
\begin{code}
type DIM0 = Z
type DIM1 = DIM0 :. Int
type DIM2 = DIM1 :. Int
...
\end{code}
and
\begin{code}
type Scalar a = Array DIM0 a
type Vector a = Array DIM1 a
\end{code}

To lift the Arrays we get via |fromList| we use the function
\begin{code}
use :: Arrays arrays => arrays -> Acc arrays
\end{code}

This is all we need to call |dotp|.

\begin{code}
dotp' :: [Float] -> [Float] -> Acc (Scalar Float)
dotp' xs ys =
  let  xs' = use (fromList (Z :.length xs) xs)
       ys' = use (fromList (Z :.length xs) ys)
  in dotp xs' ys'
\end{code}

But we still only have the computation.
To get the result out of the computation, we have to |run| it.
After that we can convert the |Array| back to a list and extract the element.

\begin{code}
dotp_list1 :: [Float] -> [Float] -> Float
dotp_list1 xs ys = head . toList $ run $ (dotp' xs ys)
\end{code}

\subsection{Representation}
Accelerate represents its arrays internally as regular unboxed one-dimensional arrays.
In addition to this, the lengths of each dimension is stored.
In case of a matrix, this would be height and width.

Rather than just simple types like |Int| or |Float|, Accelerate also supports tuples as elements
These cannot be easily stored in an unboxed array.
Instead, arrays of tuples are stored as tuples of arrays.

\begin{code}
Vector (Int, Float) ~ (Vector Int, Vector Float)
\end{code}

\subsection{Skeletons}
To execute any computation on a given architecture, the involved functions like |fold| or |map| have to be implemented.
This is easy if the target is Haskell.
The reason however that it is easy is the fact, that Haskell has support for higher-level functions like |fold| and |map|.
In C, this can be emulated by using function pointers.
This approach however is not very performant if the function is relatively simple as it involves multiple jumps and the manipulation of the call stack.

Accelerate uses skeletons instead.
The idea is to write the function as you would normally, but leave blanks for the concrete types and passed function.
When needed it can then be instantiated with the correct types and function.
Figure \ref{fig:mapskelC} illustrates the idea using |map|.

\begin{figure}
\begin{lstlisting}[language=C]
void map
(
  TyOut *           d_out,
  const TyIn *      d_in,
  const int         length
){
  for (int i=0; i < length; ix += 1) {
    d_out[i] = apply(d_in[i]);
  }
}
\end{lstlisting}
\caption{|map| Skeleton in C}
\label{fig:mapskelC}
\end{figure}

Instead of the single array for input and output however, there will often be multiple.
This is because a singe array in Accelerate be represented by multiple arrays internally.

\subsection{Fusion}
Consider the following example from earlier.

\begin{code}
dotp xs ys = fold (+) 0 (zipWith (*) xs ys)
\end{code}

This can be rewritten as

\begin{code}
dotp xs ys =
  let zs = zipWith (*) xs ys
  in fold (+) 0 zs
\end{code}

First, the elements of the 2 arrays are combined pair-wise using |zipWith| and stored in a temporary array.
As a second step, these elements are then combined into a |Scalar| using |fold|.
Although this works perfectly well, it produces an intermediate array.
This means additional writes and reads from memory.
Considering that memory access is very costly this should better be avoided.

The solution is to delay the computation of the intermediate array.
This means, that instead of computing the array in memory, it is done on-the-fly.
This changes the construction of the skeletons somewhat.
Instead of declaring the inputs directly, the arrays needed are passed in as an array environment.
This is then accessed through a function |delayedLinearIndex|.

\subsection{LLVM Backend}
Apart from the CUDA-backend and the Interpreter, There are a number of incomplete backends for Accelerate.
One of those that looks promising is the LLVM backend.\cite{trevor2014llvm}
It uses llvm-general to bind to the LLVM API.
It supports both CPUs as well as NVIDIA GPUs through PTX.
In contrast to the CUDA backend, this has the advantage that no external program has to be called for compilation of the kernels.
This is also true for the CPU side.
Unfortunately, it is not possible to ``write once, run everywhere'' with LLVM.
Although the instructions are exactly the same between architectures, the supplied libraries are not.
That is why the Skeletons cannot be shared between the 2.

To run a computation in the LLVM native backend, the skeletons are first optimized and compiled to a callable function.
Similar to repa, it uses uses gang workers\cite{chakravarty2007data} for execution.
These are multiple threads, typically as many as there are capabilities available.
They wait on an |MVar| for IO actions to perform.
At first, the load is split evenly between the workers.
If a thread finishes early however, it looks for more work.
If it finds that another thread has excess work available, it will steal half of it.
This process is called work stealing.
On a shared memory architecture, this has very little overhead.

In this thesis, I will mainly work on the native backend, but the results should be transferable to the PTX backend as well.

\chapter{Contributions}
\todo{write intro to contribution chapter}

\section{accelerate}
While testing some of the skeletons, in particular the first stage of |scanl|, I had trouble with lost writes.
These happened only if multiple threads wrote to adjacent memory locations.
Normally this behaviour should be prevented by cache coherency protocols.
These, however can be disabled if the memory involved is believed to be constant.

The Internal representation of |Array|s in Accelerate uses a |UArray| to store it's data.
This type of Array is uses a |ByteArray#|, which is assumed to be constant.
Switching the representation from |UArray| to |StorableArray| fixed the problem.

\section{llvm-general}
llvm-general offers a nice set of bindings to the LLVM API.
It is not feature-complete however.
One area where I could make an improvement on this is the optimization.
The sole difference between the |PassSetSpect| and |CuratedPassSetSpec| should be the way they define passes.
The |CuratedPassSetSpec| however was lacking important fields for data layout and target machine.\footnote{\url{https://github.com/bscarlet/llvm-general/pull/101}}
Also, they were mostly ignored when supplied to the |PassSetSpec|.\footnote{\url{https://github.com/bscarlet/llvm-general/issues/91}}
In the process, I also added support for loop and slp vectorization to |CuratedPassSetSpec|.

Another issue was the lack of support for fast-math flags.\footnote{\url{https://github.com/bscarlet/llvm-general/issues/90}}
They are now supported with the following datatype:
\begin{code}
data FastMathFlags
  = NoFastMathFlags
  | UnsafeAlgebra
  | FastMathFlags {
      noNaNs :: Bool,
      noInfs :: Bool,
      noSignedZeros :: Bool,
      allowReciprocal :: Bool
    }
\end{code}

\section{llvm-general-quote}
When writing a compiler using LLVM in Haskell there is a good tutorial on how to do it at \citeurl{diehl2014jit}.
It uses \citetitle{scarlet2013llvm} to interface with LLVM.
The Idea followed in this tutorial is to use a monadic generator to produce the AST.
The goal of monadic code generation is to use the state of the monad to store the instructions.

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

A more clean approach is to use a complete EDSL.
The idea here is to use block structure to specify the individual elements.
An implementation of this idea is \citetitle{nathan2014llvm}.
It actually goes further, as it also typechecks the produced code.

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
But quasiquotation allows for antiquotation as well.
This means you can still reference arbitrary Haskell variables from within the quotation.
Using this the following are equivalent:
\begin{itemize}
 \item \lstinline{[llinstr|| add i64 %x, 1 |]}
 \item \lstinline{let y = 1 in [llinstr|| add i64 %x, $opr:y |]}
\end{itemize}

\subsection{Control Structures}
You still have to specify the $\Phi$-nodes by hand.
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
At the end of the for-loop the code automatically jumps to the next block.
Figure \ref{fig:forquote1} shows the produced LLVM code.
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

for.body:                              ; preds = %for
  %y = add i64 %i, %x
  br label %for

for.end:                               ; preds = %for
  ret i64 %x
}
\end{lstlisting}
\caption{Expanded For Loop}
\label{fig:forquote1}
\end{figure}

\subsection{SSA}
The above approach unfortunately only works for some well-defined cases.
With multiple loop-variables for example, it quickly becomes cluttered.
On top of this, it relies on some ``magic'' to resolve the translation and legibility suffers.

The main reason why it is not possible to define a better syntax is that LLVM code has to be in SSA form.
This means, that
\begin{enumerate}
 \item every variable name can only be written to once.
 \item $\Phi$-nodes are necessary where control flow merges.
\end{enumerate}

To loosen this restriction, I implemented SSA recovery pass as part of the quasiquoter.
This means that I can now reassign variables inside the LLVM code.
Using this, I was able to define a much more simple syntax for the \lstinline{for}-loop.
The placement of $\Phi$-nodes is not optimal, but redundant ones can be easily remove by LLVM's InstCombine pass.
Figure \ref{fig:forquoteSSA} and \ref{fig:forquoteSSA1} show the quoted and the produced code after InstCombine respectively.
The code produced by the for loop using SSA recovery is not identical to the code other code, but it is equivalent.
In the new approach, the new loop counter is calculated at the end, whereas it was calculated in the loop head before.

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

\subsection{syntax extensions}
llvm-general-quote supports all llvm instructions.
For better usage however, I added control structures like the for loop discussed earlier.
These are the additions I made to the syntax:
\begin{itemize}
 \item direct assignment: \lstinline!<var> = <ty> <val>!
 \item if: \lstinline!if <cond> { <instructions> } [ else { <instructions> } ]!
 \item while: \lstinline!while <cond> { <instructions> }!
 \item for: \lstinline!for <ty> <var> in <val1> ( to || downto ) <val2> { <instructions> }!
\end{itemize}

\chapter{Implementation}
\todo{write intro to Implementation chapter}

\section{Quasiquoter}
A quasiquoter basically works like a mini compiler.
You get a string as input and have to generate Haskell code out of that.
In it's simplest form this would be just a parser.
But it is not limited to that, since all you need to define for a quasiquoter is a function
\begin{code}
quoteExp :: String -> Q Exp
\end{code}
The |Q Exp| in this case is defined by Template Haskell.
Since it uses Template Haskell to construct expressions, it is possible to reference variables in scope using antiquotation.
Figure \ref{fig:quasichart} shows a the design of a typical quasiquoter.

\begin{figure}
\begin{center}
\begin{tikzpicture}[node distance=2.5cm]
\node (start) [draw=none, fill=none] {};
\node (lexer) [process, right of=start, xshift=1.5cm] {Lexer};
\node (parser) [process, below of=lexer, xshift=2.5cm] {Parser};
\node (trans) [process, above of=parser, xshift=2.5cm, align=center] {Desugaring \& \\  Antiquotation};
\node (res) [draw=none, fill=none, right of=trans, xshift=1.5cm] {};

\draw [arrow] (start) -- node[anchor=south] {|String|} (lexer);
\draw [arrow] (lexer) -- node[anchor=west] {|[Token]|} (parser);
\draw [arrow] (parser) -- node[anchor=west, align=center] {|Module|\\(with additions)} (trans);
\draw [arrow] (trans) -- node[anchor=south] {|Module|} (res);
\end{tikzpicture}
\end{center}
\caption{flowchart quasiquoter}
\label{fig:quasichart}
\end{figure}

The design of \citetitle{holtz2014quote}, the quasiquoter I defined for LLVM, is inspired by \citetitle{mainland2007c}, which is also used in the cuda implementation of Accelerate.
This means I use \citetitle{alex} to specify the lexer and \citetitle{gill1995happy} for the parser.
The goal of this quasiquoter is to generate an AST as defined by llvm-general-pure.
If I want to do more than just parse complete LLVM code, I can't use this as a target for the parser.
This means it is necessary to copy almost the whole AST and add additional constructors for antiquotation etc.
This step wasn't necessary in \citetitle{mainland2007c} since it's target datastructure still has constructors for antiquotation.

At this point the construction of the 2 quasiquoters differs significantly.
\citetitle{mainland2007c} assumes that constructors stay the same, as long as there isn't a different case available.
\todo{pickup writing here}

\begin{code}
type Conversion a b = forall m.(CodeGenMonad m) => a -> TExpQ (m b)

class QQExp a b where
  qqExpM :: Conversion a b
  qqExp :: a -> TExpQ b
  qqExp x =
    [||let s = ((0,M.empty) :: (Int,M.Map L.Name [L.Operand])))
       in fst runState $$(qqExpM x) s||]
\end{code}

\begin{code}
class (Applicative m, Monad m) => CodeGenMonad m where
  newVariable    :: m L.Name
  exec           :: m () -> m [L.BasicBlock]
\end{code}

\section{Control Structures}
When implementing the control structures, I had to decide on what level I wanted to introduce it.
In a traditional procedural language like C, they would sit alongside the other expressions like assignments or function calls.
This would correspond to the instructions in LLVM.
For this to be possible however, I need to be able to extend them into just a sequence of instructions.
In case of an if-then-else this is still kind of possible using select to get the result value.
The loop structures however are not possible, as they require an arbitrary number of jumps.

\subsection{direct approach}
The solution is to have the control structures on the level of basic blocks.
This way it is possible to just produce multiple basic blocks.
For something to behave like a basic block, it must have all the elements of a basic block.
In llvm-general, a basic block is represented as

\begin{code}
data BasicBlock =
  BasicBlock Name [Named Instruction] (Named Terminator)
\end{code}

It consists of a name, a list of instructions and a terminator.
The name is used as a target label for jumps.
The terminator can either be a jump to a different block or a return statement.
The instructions are just a stream without any jumps, but can also contain function calls.

Lets look at a classic for loop.
It starts by initializing a counter.
Then the counter is checked against a maximum.
If this is not yet reached, the loop body gets executed and the counter incremented.
If it is, then the loop exits and the next expression is evaluated.
Figure \ref{fig:forchart} shows the process.

\begin{figure}
\begin{center}
\begin{tikzpicture}[node distance=2cm]
\node (start) [startstop] {Start};
\node (init) [process, below of=start] {i = 0};
\node (check) [decision, below of=init] {i < n?};
\node (body) [io, below of=check] {loop body};
\node (inc) [process, right of=check, xshift=2cm] {i = i + 1};
\node (stop) [startstop, left of=body, xshift=-2cm] {Stop};

\draw [arrow] (start) -- (init);
\draw [arrow] (init) -- (check);
\draw [arrow] (check) --node[anchor=east] {true} (body);
\draw [arrow] (check) -|| node[anchor=east] {false} (stop);
\draw [arrow] (body) -|| (inc);
\draw [arrow] (inc) -- (check);
\end{tikzpicture}
\end{center}
\caption{flowchart for-loop}
\label{fig:forchart}
\end{figure}

To translate this into LLVM, I need a label, the name of the counter, it's minimum and maximum value and the loop body.
In a normal language these would be sufficient.
LLVM doesn't have mutable variables, however.
This means that any state, like an accumulator, needs to be explicitly defined.
The syntax I chose for this is the following:

\begin{lstlisting}[numbers=none]
for <ty1> <var1> in <val1> to <val2> with <ty2> <values> as <var2>,
           label <jumptarget> { <loop body> }
\end{lstlisting}

The values here work the same way as with \lstinline!phi!-instructions.
It is often necessary to nest loops.
To allow for this, the loop body can consist of multiple basic blocks.
This way, it works much like function body, with the loop counter and the accumulator as arguments.
With this analogy, I reuse the return statement to indicate the end of the loop.
The value returned is used as the new value of the accumulator.

\todo{examples}

When implementing this, there are a few things that make matters non-trivial.
Since LLVM doesn't have mutable variables, it is necessary to introduce \lstinline{phi} instructions manually.
This node has to have a value specified for every incoming block.
First these are the blocks and values specified in the loop header.
On top this, these are all the blocks inside the loop returning a value.
The values are then extracted and appended to the existing list.

\todo{examples}

This is relatively straight forward if you were to implement it as a regular Haskell function.
Working with quasiquoters though, It all has to be implemented in a Template Haskell.
This means that it is sometimes necessary to move code around just so that it compiles, although the types are correct.
The reason for this is the stage restriction of Template Haskell.
A value cannot be spliced into an expression if it was defined locally.
Another big difference is type safety.
Before GHC 7.8, the expressions inside a quoted block would not be typechecked.
You would still get type errors for the code, but rather than complaining at the definition site it would complain at the usage site.
This makes defining complex functions nearly impossible.
But even with GHC 7.8 you get some warnings when splicing code in rather than where you defined them.
For example, it is not checked if a pattern match is exhaustive.
Although this check could be done in the type checker, it is done while desugaring to core.
I filed this as a bug in GHC (\#9113) and it seems that there is work done which would solve this issue.

\todo{write about desugaring}

\subsection{nextblock}
In principle basic blocks are not ordered.
The only exception is the first block, which also cannot have any predecessors.
In reality however, they are mostly ordered in the order of control flow.
With this, it is not strictly necessary to specify which block will be next after a loop as it is mostly the textually next one.
This simplifies the syntax somewhat.

\begin{lstlisting}[numbers=none]
for <ty1> <var1> in <val1> to <val2> with <ty2> <values> as <var2>
           { <loop body> } 
\end{lstlisting}

\todo{examples}
\todo{write about desugaring}

\begin{itemize}
\item cumbersome syntax
\item limited functionality
\item jumpnext
\end{itemize}

\subsection{mutable variables}
Although the above methods work, there are some fundamental problems with them.
The fact that you have to specify the variables in the loop header and then return the new value is awkward.
It abuses the return for something that it is not, namely jumping to the loop header.
I could have chosen to use a jump instead, but then there would be no way to update the accumulator.
Another more important flaw is the limitation to one value.
It is possible, of course, to bundle all needed values together in a struct, but this adds a lot of necessary boilerplate code.

In an ordinary language a for loop would consist of just the loop counter and would handle accumulators through mutable variables.
LLVM doesn't support mutable variables however.
Since I need however, I implemented support for them through my quasiquoter.
The implementation is described in \ref{sec:ssa}.

With this idea, the syntax for the for loop is now reduced to
\begin{lstlisting}[numbers=none]
for <ty1> <var1> in <val1> to <val2> { <loop body> }
\end{lstlisting}
This is a great improvement upon the first approach.
The main benefit now is the increased flexibility.
It is also more concise and doesn't abuse the return to pass through variables.

As a side effect, the implementation got a lot simpler.
It is now no longer necessary to scan for return statements to fill \lstinline!phi!-instructions for both counter and accumulator.
Instead, the last block in the loop body just jumps to an end-block, where the counter gets increased and then jumps to the head.
Now the for-loop can be implemented exactly like shown in figure \ref{fig:forchart}.

To allow mutable variables, there has to be a way to initialize them.
The syntax for this is
\begin{lstlisting}[numbers=none]
 <var> = <ty> <val>
\end{lstlisting}
To implement this, I used the select statement.
The above code gets translated into
\begin{lstlisting}[numbers=none]
 <var> = select i1 true, <ty> <val>, <ty> <val>
\end{lstlisting}
It always selects the first value.
Another way would be to leave the second value undefined.
\begin{lstlisting}[numbers=none]
 <var> = select i1 true, <ty> <val>, <ty> undef
\end{lstlisting}
Obviously this adds another unnecessary instruction to the produced code.
This is negligible however, as it is easily removed by LLVM's constant propagation.

\section{SSA}
\label{sec:ssa}
The standard method of producing SSA form in LLVM is to use stack allocated variables instead of registers to store values.
The LLVM optimizer then uses the mem2reg pass to transform these into registers, adding the necessary phi-nodes in the process.

Since one of the goals of the quasiquoter was to produce llvm-code that is as close to what the programmer wrote as possible, I decided against this approach.
Instead I do the transformation myself.
To this end, I provide a single function

\begin{code}
toSSA :: [BasicBlock] -> [BasicBlock]
\end{code}

This is called with the list of all |BasicBlock|'s of a given function.

To produce SSA form, variables have to be versioned, so that it is clear which value it holds at each given time.
There are a multitude of algorithms to do this.
The one most widely used (including in LLVM) is \citeauthor{cytron91efficiently}'s algorithm.
It is however is rather involved as it relies on dominance frontiers.
An easier approach was presented by \citeauthor{braun13simple}.

In this algorithm, the SSA is produced directly from the AST, without the need of extra analysis passes.
To do this, the definitions of variables are tracked and updated as necessary.
Without $\Phi$nodes, this is straight forward, as Figure \ref{fig:localnum} shows.

\begin{figure}
\begin{lstlisting}[language=ruby]
writeVariable(variable, block, value):
    currentDef[variable][block] <- value

readVariable(variable, block):
    if currentDef[variable] contains block:
        # local value numbering
        return currentDef[variable][block]
    # global value numbering
    return readVariableRecursive(variable, block)
\end{lstlisting}
\caption{Implementation of local value numbering}
\label{fig:localnum}
\end{figure}

If the given variable has no definition in the current block, an empty $\Phi$-node is inserted and the local definition is set to the $\Phi$-node.
Now the lookup is done recursively and the values are added to the $\Phi$-node.
This extra step is necessary to detect loops correctly.
I use a slightly different approach than presented in the paper.
The original version tracks if the predecessors of a block are all processed.
This is necessary since only then can the empty $\Phi$-nodes be filled.
This approach makes sense if this is the norm.
In case the input is an unordered list of blocks however, I can't make predictions about this.
Instead I assume that no block is processed until all blocks are.
Figure \ref{fig:globalnum} shows the modified |readVariableRecursive|.

\begin{figure}
\begin{lstlisting}[language=ruby]
readVariableRecursive(variable, block):
    if ||block.preds|| = 0:
        # First block
        val <- variable
    else:
        # Break potential cycles with operandless phi
        val <- new Phi(block)
        writeVariable(variable, block, val)
    writeVariable(variable, block, val)
    return val
\end{lstlisting}
\caption{Implementation of global value numbering}
\label{fig:globalnum}
\end{figure}

Like most efficient graph algorithms, this algorithm relies heavily on mutable data structures.
Although Haskell - being purely functional - doesn't support these normally, there are multiple methods to implement them inside a pure Monad.
The obvious choices are |State| and |ST|.
|IO| also provides mutable state, but is not an option here, since there is no safe way of running it in a pure context.
I use |ST|, since it natively provides an arbitrary number of mutable variables with direct access.

To use this however, the |BasicBlock|s have to be in a mutable format.

\begin{code}
type CFG s =[(Name, MutableBlock s)]

data MutableBlock s = MutableBlock {
  blockName :: Name,
  blockIncompletePhis :: STRef s (M.Map Name (MutableInstruction s)),
  blockPhis :: STRef s [MutableInstruction s],
  blockInstructions :: [MutableInstruction s],
  blockTerminator :: MutableTerminator s,
  blockPreds :: [Name],
  blockDefs :: STRef s (M.Map Name Name)
  }

type MutableInstruction s = STRef s (Named Instruction)
type MutableTerminator  s = STRef s (Named Terminator)
\end{code}

Although not everything is mutable in this representation, it is sufficient.
After the conversion, the |BasicBlock|s can be processed one by one.
First, the variables created by |Instruction|s are adjusted.
The same is done for usages.
If there a variable is not defined in a given block and there are predecessors, an incomplete |Phi| instruction is added.
Incomplete here means, that the values for every given predecessor are left undefined.
When this is done, the variables referenced by existing |Phi| instructions are replaced by the new values.

Every time a recursive read is done, a new incomplete |Phi| instruction is added.
These are handled last by reading the variable in all preceding blocks and replacing the undefined values.
Since this involved reading however, new incomplete |Phi| instructions can be added in the process.
To handle all of them, the function is executed until there are no incomplete |Phi| instructions.

Figure \ref{fig:tossa} shows the complete |toSSA| function.

\begin{figure}
\begin{code}
toSSA :: [BasicBlock] -> [BasicBlock]
toSSA bbs = runST $ do
  cfg <- toCFG $ bbs
  ctr <- newSTRef 1

  -- process all Instructions
  mapM_ (blockToSSAPre ctr) (map snd cfg)
  -- replace names in Phis with correct references
  mapM_ (blockToSSAPhi ctr cfg) (map snd cfg)
  -- replace names in newly added Phis with correct references
  handleIncompletePhis ctr cfg

  fromCFG cfg
\end{code}
\caption{|toSSA|}
\label{fig:tossa}
\end{figure}

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
\section{stencil}

\chapter{Conclusion}
\section{Benchmarks}
\section{Related Work}


\appendix
%\chapter{Listings}

\backmatter
\sloppy
\printbibliography[heading=bibintoc]
\end{document}
