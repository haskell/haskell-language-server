\documentstyle{article}

\begin{document}

\section{Introduction}

This is a trivial program that prints the first 20 factorials.

\begin{code}
module TLHSLaTeX where

-- >>> prod
prod =  [ (n, product [1..n]) | n <- [1..3]]
\end{code}

\end{document}
