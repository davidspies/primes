%% LyX 2.1.4 created this file.  For more info, see http://www.lyx.org/.
%% Do not edit unless you really know what you are doing.
\documentclass[english]{jfp1}
\usepackage[T1]{fontenc}
\usepackage[latin9]{inputenc}
\usepackage{babel}
\usepackage{varioref}
\usepackage{textcomp}
\usepackage{graphicx}

\makeatletter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Textclass specific LaTeX commands.
\usepackage{amsfonts}
\usepackage{bm}
\usepackage{mathptmx}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% User specified LaTeX commands.
\usepackage{verbatim}
\newenvironment{code}{\verbatim}{\endverbatim}

%include polycode.fmt

\makeatother

\begin{document}

\author{David Spies}


\title{A list-based Sieve of Eratosthenes}
\maketitle
\begin{abstract}
We introduce a Sieve of Eratosthenes in Haskell which produces an
infinite list of prime numbers, and achieves $O\left(n\log n\log\log n\right)$
performance without making use of \emph{any} data structure besides
infinite lists.
\end{abstract}

\section{Introduction}

In ``The Genuine Sieve of Eratosthenes'' \cite{o2009genuine}, they
refute the claim that a well-known Haskell one-liner for generating
a list of prime numbers is an implementation of the Sieve of Eratosthenes
and show that its computaitonal behavior is fundamentally different
(not to mention significantly slower). They then go on to produce
an actual implementation making use first of binary-tree-based maps
and later of queues claiming:
\begin{quote}
An implementation of the actual sieve has its own elegance, showing
the utility of well-known data structures over the simplicity of lists
{[}...{]} choosing the right data structure for the problem at hand
{[}can make{]} an order of magnitude performance difference.
\end{quote}
Here, we produce an implementation of the Sieve of Eratosthenes which
\emph{is} computationally similar to O'Neill's in that it has the
same big-$O$ performance and ultimately works by merging together
streams of composites and then excluding them from a base-list. But
it does so without ever making explicit use of any data-structure
besides infinite lists. The heap which merges together lists of composites
is implicitly built into the way the function calls which generate
these lists are structured rather than being encoded explicitly as
data.


\subsection{Literate Haskell}

This document (or rather \texttt{primes.lhs}, the \LaTeX{} file used
to generate this document) is literate Haskell. It has been tested
on GHC 8.2.2. To compile it we will need a main method. We can take
a positive number $n$ as the sole command line argument print out
the $n^{\mbox{th}}$ prime by indexing into the infinite list of primes,\texttt{
primes }\vpageref{primescomposites}: \label{sub:main-method}

\begin{code}
import System.Environment (getArgs)

main :: IO ()
main = do
  [ind] <- map read <$> getArgs
  print $ primes !! (ind - 1)
\end{code}

To run it, try:
\begin{verbatim}
$ ghc -O2 primes.lhs
$ ./primes 1000000
15485863
\end{verbatim}
Finding the millionth prime takes about 7 seconds on an Intel Core\texttrademark{}
i5-4200M CPU.

Additionally, ``\texttt{ghci -i primes.lhs}'' can be used to otherwise
play with the various functions and structures defined in this paper.


\section{Merging Lists}

All lists used in this document are infinite and sorted. All lists
of lists are infinite and sorted on their heads. When dealing with
sorted lists, it is useful to have a merge function.

\begin{code}
merge :: Ord a => [a] -> [a] -> [a]
merge (x : xs) (y : ys)
  | y < x = y : merge (x : xs) ys
  | otherwise = x : merge xs (y : ys) 
\end{code}

This takes two sorted lists and merges them into a single sorted list.
Note that evaluating the head of the result triggers the evaluation
of both arguments' heads. Instead of this merge function, we will
need one which prepends the head of the left list before merging the
remainder with the right list. If we can prove via some \emph{other}
means that the head of the left input is less than the head of the
right, then this will return the same result as just calling \texttt{merge}.
But obtaining the head of the result won't require any evaluation
of the right argument.

\begin{code}
fmerge :: Ord a => [a] -> [a] -> [a]
fmerge (x : xs) ys = x : merge xs ys
\end{code} 

To see how this might be useful, here is an ordered list of all numbers
whose only prime factors are 2 and 3:

\begin{code}
twosThreesOnly :: [Integer]
twosThreesOnly = fmerge (iterate (2 *) 1) [3 * x | x <- twosThreesOnly]
\end{code}

Were we to define it using \texttt{merge} rather than \texttt{fmerge},
attempting to evaluate \texttt{twosThreesOnly} would simply cause
our program to loop forever unable to determine the recursively-defined
first element.

\texttt{fmerge} can \emph{also} be used to merge an infinite list
of sorted lists, provided we know the heads of the lists are already
sorted.

\begin{code}
fmergeAllNaive :: Ord a => [[a]] -> [a]
fmergeAllNaive (x : xs) = fmerge x (fmergeAllNaive xs)
\end{code}

This works and uses an impressively small amount of code, but theoretically
is not very performant. In the worst case, evaluating the $k$th element
can require $\Omega\left(k\right)$ running time. To see why this
is, take a look at what structure results from evaluating \texttt{fmergeAllNaive}
on a list. Suppose we have the list:
\begin{verbatim}
xs = x1 : x2 : x3 : x4 : ...
\end{verbatim}
When we call \texttt{fmergeAllNaive xs}, the resulting structure looks
something like Figure \ref{fig:fmergeallnaive}.

\begin{figure}


\caption{\label{fig:fmergeallnaive}Structure of \texttt{fmergeAllNaive}}


\includegraphics[height=0.3\textheight]{images/fig1}
\end{figure}


If an element belongs to list $x_{k}$, then to ``bubble up'' to
the top of our evaluation structure, it must be compared against the
first element of $x_{k-1}$, followed by the first element of $x_{k-2}$
etc. until finally being compared against $x_{1}$.

To rectify this, we will first create a helper function for efficiently
merging \emph{just the length-$k$ prefix} of a list of lists (where
$k>0$). The \texttt{fmergePrefix} function returns both the merged
prefix and the unmerged remainder. As before, we assume the heads
of the lists are themselves already sorted.

\begin{code}
fmergePrefix :: Ord a => Int -> [[a]] -> ([a], [[a]])
fmergePrefix 1 (x : xs) = (x, xs)
fmergePrefix k xs = (fmerge y z, zs)
  where
    (y, ys) = fmergePrefix (k `quot` 2) xs
    (z, zs) = fmergePrefix ((k + 1) `quot` 2) ys
\end{code} 

This should look familliarly like a standard merge-sort, except that
we're using \texttt{fmerge} rather than \texttt{merge} (and all our
lists are infinite).

Notice that evaluating the first $n$ elements of the resulting merged
prefix requires at most $O\left(n\log k\right)$ steps as any element
needs to be compared with at most $\log_{2}k$ others to bubble to
the top. Now here is a more efficient \texttt{fmergeAll} implementation
which makes use of \texttt{fmergePrefix}. 

\begin{code}
fmergeAll :: Ord a => [[a]] -> [a]
fmergeAll = go 1
  where
    go k xs = let (ys, zs) = fmergePrefix k xs in fmerge ys (go (k * 2) zs)
\end{code} 

\texttt{fmergeAll} is quite similar to \texttt{fmergeAllNaive} except
that instead of merging lists together one at a time, we merge them
in batches of exponentially growing size. Each batch is efficiently
merged using \texttt{fmergePrefix}. A consequence is that any element
from the $k^{\mbox{th}}$ list needs to be compared with at most $O\left(\log k\right)$
elements to bubble to the top of the resulting structure of thunks
(see Figure \ref{fig:fmergeall}).

\begin{figure}


\caption{\label{fig:fmergeall}Structure of \texttt{fmergeAll}}


\includegraphics[height=0.3\textheight]{images/fig2}

\end{figure}



\section{Excluding From a List}

In addition to merging lists, it will also be useful to \emph{exclude}
elements from a list. The implementation is straightforward.

\begin{code}
excluding :: Ord a => [a] -> [a] -> [a]
(x : xs) `excluding` (y : ys) = case compare x y of
  LT -> x : (xs `excluding` (y : ys))
  EQ -> xs `excluding` ys
  GT -> (x : xs) `excluding` ys
\end{code}


\section{Primes and Composites}

With this in hand, we can mutually recursively define two lists: \texttt{primes}
and \texttt{composites} which respectively are lists of all the prime
and composite integers. The composites are the merged multiples of
the prime numbers. Note that if we start from the square of each prime,
then any composite number $n$ will occur once in the composites list
for each of its prime factors $p\le\sqrt{n}$ (every composite number
must have such a factor). The primes are then just the list of all
numbers larger than $1$ excluding the composites. The number $2$
is explicitly given as the head of the list to avoid infinite looping
to find the head of the list at runtime. \label{primescomposites}

\begin{code}
primes :: [Integer]
primes = 2 : ([3..] `excluding` composites)

composites :: [Integer]
composites = fmergeAll [[p * p, p * (p + 1)..] | p <- primes]
\end{code}

Since the list of primes is in ascending order and the operation of
squaring is monotonic on positive integers, we can be sure that the
heads of the composite lists to be merged are also in ascending order.
Thus it is safe to use \texttt{fmergeAll} here for merging together
all the lists of composites.


\section{Rolling The Wheel}

In \cite{o2009genuine} they additionally show how to build a ``wheel''
sieve by skipping multiples of the smallest (and hence most common)
primes. We can do the same with the list-based version. First, observe
how we can ignore all the even numbers in our computation:

\begin{code}
oddPrimes :: [Integer]
oddPrimes = 3 : ([5,7..] `excluding` oddComposites)

oddComposites :: [Integer]
oddComposites = fmergeAll [[p * p, p * (p + 2)..] | p <- primes]

primes2 :: [Integer]
primes2 = 2 : oddPrimes
\end{code}

Re-running with \texttt{primes} changed to \texttt{primes2} in the
main method \vpageref{sub:main-method} gives us the millionth prime
in only 3.5 seconds. Nearly exactly half of what it costs using the
straightforward sieve.

To ignore multiples of 3 as well, we start with the list of prime
\texttt{candidates} and then filter out the composites based on those.

\begin{code}
wheelCandidates :: [Integer]
wheelCandidates = 5 : concat [[n + 1, n + 5] | n <- [6, 12..]]

wheelPrimes :: [Integer]
wheelPrimes = 5 : (tail wheelCandidates) `excluding` wheelComposites

wheelComposites :: [Integer]
wheelComposites =
  fmergeAll [[p * k | k <- dropWhile (< p) wheelCandidates]
    | p <- wheelPrimes]

primes3 :: [Integer]
primes3 = 2 : 3 : wheelPrimes
\end{code}

And by changing \texttt{primes} to \texttt{primes3} we observe an
almost-negligible speedup from 3.5 to 3.3 seconds to find the millionth
prime. Perhaps a greater speedup could be obtained by replacing the
\texttt{dropWhile} above with a direct computation of the first term
for each list or by finding a way to generate successive composites
by addition rather than multiplication as is done in the \texttt{primes}
and \texttt{primes2} case.

\bibliographystyle{jfp}
\bibliography{primes}

\end{document}
