#!/usr/bin/env julia
using Lazy
using Combinatorics

State = Union{Int,String,Any}
Character = Char
Alphabet = Set{Character}

struct DFA
    Q:: Set{State}
    Σ:: Alphabet
    δ:: Dict{Tuple{State,Character},State}
    q0:: State
    F:: Set{State}
end

struct NFA
    Q:: Set{State}
    Σ:: Alphabet
    δ:: Dict{Tuple{State,Character},Set{State}}
    q0:: State
    F:: Set{State}
end

∅=Set([])

function concatenate(A,B)
    unique([a*b for a ∈ A for b ∈ B])
end

function reverseL(L)
    map(reverse,L)
end

function languageToTheN(A,n)
    if n == 0
        return [""]
    else
        return @>> languageToTheN(A,n-1) concatenate(A)
    end
end


function kleen(A)
    @>> Lazy.range(0) Lazy.map(x -> languageToTheN(A,x))
end

function recognizeDFA(A,w)
    Q = A.Q
    Σ = A.Σ
    δ = A.δ
    q0 = A.q0
    F = A.F
    if q0 ∉ Q
        error("q0 is not in Q")
    end
    currentState = q0

    if w == "" 
        return q0 ∈ F
    end

    for c in w
        if c ∉ Σ
            error("Current character ('$c') is not in Σ")
        end
        oldState = currentState
        currentState = δ[(currentState,c)]
        if currentState ∉ Q
            error("δ[($oldState,$c)]∉ Q")
        end 
    end

    return currentState ∈ F
end

function L_FA(A; limit = 0, group = false)
    if typeof(A) == DFA 
        recognize=recognizeDFA
    elseif typeof(A) == NFA
        recognize=recognizeNFA
    else
        error("A is neither DFA nor NFA")
    end
    if limit == 0 && group == true
        error("Limit must be greater than 0 if group is true, cannot group a possibly inifinite language")
    end
    if limit == 0
        takeFnc = function (_, x)
            return x
        end
    else 
        takeFnc = Lazy.take
    end

    Σ = A.Σ
    if group 
        @>> kleen(Σ) Lazy.map(seq) takeFnc(limit) flatten reverse groupby(w -> recognize(A,w))
    else
        @>> kleen(Σ) Lazy.map(seq) Lazy.map(list -> Lazy.map(w-> (
        (w, recognize(A,w))
        ),list)) takeFnc(limit)
    end
end

flattenVectorOfSets(X::Vector{Set{Any}})=reduce(∪,X,init=∅)

function recognizeNFA(A,w)
    Q = A.Q
    Σ = A.Σ
    δ = A.δ
    q0 = A.q0
    F = A.F
    if q0 ∉ Q
        error("q0 is not in Q")
    end
    currentPossibleStates = Set([q0])
    for c ∈ w
        if c ∉ Σ
            error("Current character ('$c') is not in Σ")
        end
        oldStates = currentPossibleStates
        currentPossibleStatesArray = [
            (if haskey(δ,(q,c));
                δ[(q,c)];
            else;
                ∅;
            end)
            for q ∈ currentPossibleStates]
        currentPossibleStates = flattenVectorOfSets(currentPossibleStatesArray)
        # println(currentPossibleStates)
        if currentPossibleStates ⊈ Q
            error("δ[($oldStates,$c)]⊈ Q")
        end 
    end

    # println(currentPossibleStates∩ F)
    return currentPossibleStates ∩ F ≠ ∅
end


# struct TokenTreeLeaf
#     isEmpty::Bool
#     character::Character
# end
#
#
# struct TokenTreeInnerNode
#     type::@enum Concatenation,KleeneStar,Union
#     children::Tuple{Union{TokenTreeLeaf,TokenTreeInnerNode}, Union{TokenTreeLeaf,TokenTreeInnerNode}} # for the star only the first and second elements of the tuple are used
# end
#
# TokenTree=Union{TokenTreeLeaf,TokenTreeInnerNode}
#
#
# function parseRegEx(regexString::String)
#
#     collectString = ""
#     for i ∈ [1:length(regexString)]
#         c = regexString[i]
#         if c == '*'
#
#         elseif c == '|'
#
#         elseif collectString == ""
#             collectString = c
#         else
#             regex = regexString
#             tokenTree = (Concatenation,ParsedRegEx(collectString),ParsedRegEx(c))
#         end
#     end
#     parsedRegEx = ParsedRegEx(regexString,tokens)
# end
#
# function L_RegEx(regex::ParsedRegEx)
#
# end

E = 0:2:8
O = map(x->x+1,E)
function count(w,M)
    if w == ""
        return 0
    else
        map(bool -> bool ? 1 : 0, [parse(Int, c) ∈ M for c in w]) |> sum
    end
end

function powersetconstruction(A::NFA)
    Σ = A.Σ
    q0 = Set(A.q0)

    δ=Dict{Tuple{Set{State},Char},Set{State}}
    currentPossibleStates = q0
    # TODO end condition
    # TODO rewrite this with branching for each c
    for c ∈ Σ
        newPossibleStatesByState = [(q,A.δ[(q,c)]) for q ∈ currentPossibleStates]
        newPossibleStates = map((_,set)->set,newPossibleStatesByState) |> flattenVectorOfSets
        for (qFrom,qToSet) ∈ newPossibleStatesByState
            #TODO think about how to encode the set as an Int, for now a string will do
            qTo = join(qToSet,"/")
            push!(δ,(qFrom,c)=>qTo)
        end
        currentPossibleStates=newPossibleStates
    end
end

# TODO this isn't right yet:
# A=DFA(Set([0,1,2]),Set(['a','b']),Dict((0,'a')=>1, (1,'b')=>2, (2,'a')=>2, (2,'b')=>2,(0,'b')=>0,(1,'a')=>1),0,Set([2]))
# dfaToRegex(A) should not accept "b", but it does
function dfaToRegex(A)
    Q = A.Q
    if (typeof(Q) == "Set{String}")
        # TODO maybe implement an automatic conversion
        error("only supported on int DFAs")
    end
    Σ = A.Σ
    δ = A.δ
    q0 = A.q0
    F = A.F
    function Rk_ij(k,i,j)
        if k == 0
            result = [ "$a" for a in Σ if δ[(i,a)] == j ]
            if i==j 
                push!(result, "")
            end
        else
            error("not defined, as it isn't needed for the conversion")
        end
        return result
    end

    function αk_ij(k,i,j)
        if k == 0
            r0_ij = Rk_ij(0,i,j)
            join(replace(r0_ij, ""=>"eps"), " | ")
        else
            "$(αk_ij(k-1,i,j)) | $(αk_ij(k-1,i,k))($(αk_ij(k-1,k,k)))^*$(αk_ij(k-1,k,j))"
        end
    end
    minimumStateNumber = findmin([ q for q ∈ Q ]) |> first
    join([αk_ij(length(Q)-1-minimumStateNumber,q0,f) for f ∈ F], " | ")
end

# TODO: filter unnecessary stuff in conversion, this is an attempt
# function dfaToRegex(A)
#     Q = A.Q
#     if (typeof(Q) == "Set{String}")
#         # TODO maybe implement an automatic conversion
#         error("only supported on int DFAs")
#     end
#     Σ = A.Σ
#     δ = A.δ
#     q0 = A.q0
#     F = A.F
#     function Rk_ij(k,i,j)
#         if k == 0
#             result = [ "$a" for a in Σ if δ[(i,a)] == j ]
#             if i==j
#                 push!(result, "")
#             end
#         else
#             error("not defined, as it isn't needed for the conversion")
#         end
#         return result
#     end
#
#     # this returns a list of regex which can either contain a single regex or another list of regex. They are joined with " | " in the end
#     function αk_ij(k,i,j)
#         if k == 0
#             r0_ij = Rk_ij(0,i,j)
#             replace(r0_ij, ""=>"ϵ")
#         else
#             [αk_ij(k-1,i,j), string(αk_ij(k-1,i,k),"(",αk_ij(k-1,k,k),")^*",αk_ij(k-1,k,j))]
#         end
#     end
#     minimumStateNumber = findmin([ q for q ∈ Q ]) |> first
#     resultOrVector=[αk_ij(length(Q)-1-minimumStateNumber,q0,f) for f ∈ F]
#     function joiner(x::Union{Any,Vector{String},String})
#         if typeof(x) == String
#             return x
#         elseif typeof(x) == Vector{String}
#             return "($(join(x," | ")))"
#         else # typeof(x) == "Vector{Any}"
#             if length(x) > 0
#                 rest=x[(firstindex(x)+1):end]
#                 x=first(x)
#                 return "($(joiner(x))) | ($(joiner(rest)))"
#             else
#                 return ""
#             end
#         end
#     end
#     joiner(resultOrVector)
# end

Terminal=Char
NonTerminal=Char
Production=Vector{Union{Terminal,NonTerminal}}

struct CFG
    V:: Vector{NonTerminal}
    Σ:: Vector{Terminal}
    P:: Dict{NonTerminal,Vector{Production}}
    S:: NonTerminal
end

function recognizeCFG(G, w)
    # TODO convert to CNF
    return G.S ∈ CYK(G,w)[1,1]
end

function CYK(G,w)
    V = G.V
    P = G.P
    n = length(w)
    v = Array{Vector{NonTerminal}}(undef,n,n)

    for i in 1:n
        v[i,i]=unique([A for A in V, p in P if [w[i]]∈ P[A] ])
    end

    for jMinusI in 1:(n-1)
        # for jMinusI=0: 11, 22, 33, ..., nn: already done in the base case above
        for i in 1:(n-jMinusI)
            j=i+jMinusI
            v[i,j] = unique([ A for k in i:(j-1) for  A in V, B in v[i,k], C in v[k+1,j] if ([B,C] in P[A]) ])
        end
    end

    EasyOutputMatrix=Matrix{Vector{NonTerminal}}(undef,n,n)
    for jMinusI in 0:(n)
        for i in 1:(n-jMinusI)
            j=i+jMinusI
            EasyOutputMatrix[n-jMinusI,i]=v[i,j]
        end
    end
    return EasyOutputMatrix
end

# S->(S)S|eps in CFG:
#
# S->AB|AR|LB|LR
# L->(
# R->)
# A->LS
# B->RS
# hilfreich :-).
function inputCFG()
    println("
Enter the grammar you want to parse in the format A->A|B|CD|e|eps, line by line
Please note:
- Non-Terminals are single uppercase letters
- Terminals are single lowercase letters or any other symbol
- The first occuring non-terminal is the start symbol
- eps represents the empty string

")
    return parseCFG(inputLineByLineUntilEmpty())
end

function inputLineByLineUntilEmpty()
    lines=[]
    while true
        line = readline()
        if line == ""
            return lines
        else
            push!(lines, line)
        end
    end
end

isNonTerminal(c)=isuppercase(c)
isTerminal(c)=!isNonTerminal(c)

function parseCFG(inputLines)
    S = inputLines[1][1]
    if(!isNonTerminal(S))
        error("The LHS of the first line must be the start symbol")
    end
    V = [ S ]
    Σ = []
    P = Dict{NonTerminal,Vector{Production}}()
    for line ∈ inputLines
        splitted = split(line, "->")
        if(length(splitted)!=2 || length(splitted[1])!=1)
            error("Line \""+line+"\" is not in the correct format")
        end
        LHS = splitted[1][1]
        not(x)=!x
        RHS = filter(not ∘ isspace,splitted[2]) #filter spaces
        if(length(RHS) < 1)
            error("Line \""+line+"\" is not in the correct format")
        end
        RHSSplit = split(RHS, "|")
        for RHSItem in RHSSplit
            RHSItem=replace(RHSItem, "eps"=>"")

            terminals=unique(filter(isTerminal, RHSItem))
            nonterminals=unique(filter(isNonTerminal, RHSItem))
            if(nonterminals ∉ V)
                append!(V, nonterminals)
            end
            if(terminals ∉ Σ)
                append!(Σ, terminals)
            end
            if(!haskey(P,LHS))
                P[LHS] = []
            end
            push!(P[LHS],collect(RHSItem))
        end
    end
    @show P
    return CFG(unique(V),unique(Σ),P,S)
end

function chomskyNormalizer(G)
    # 1. step: remove A->aB etc.
    # (assume that X_a's are not already in the grammar)
    V′ = G.V
    XVs = Dict{NonTerminal,Terminal} # new terminals to make A->aB into A->X_aB, X_a -> a
    P′ = Dict{NonTerminal,Vector{Production}}()
    for A in G.V
        for p in G.P[A]
            if length(p) == 1
                push!(P′, p)
            else
                rightSideInConstruction= []
                for c∈ p
                    if isTerminal(c)
                        constructed="X_$c" # weeeeell characters as nonterminals, huh?...
                        if !haskey(XVs,constructed)
                            XVs[constructed] = c
                        end
                        c=constructed
                    end
                    push!(rightSideInConstruction, c)
                end
            end
        end
    end
    @show XVs
end

function main()
    println("Ctrl+D to exit orderly!")
    functionalities = [ 0 => "CYK Algorithm" ]
    println("Choose ID what you want to do:")
    println("<ID>: <functionality>")
    for (id, name) ∈ functionalities
        println("$id: $name")
    end
    read = readline()
    if(read== "" && eof(stdin))
        println("Exiting...")
        exit(0)
    end
    if(read=="0") 
        println("Input CFG in CNF:")
        G = inputCFG()
        while true
            print("CYK Word?: ")
            read = readline()
            if(read == "" && eof(stdin))
                println("Exiting...")
                exit(0)
            end
            display(CYK(G, read))
            println()
        end
    else
        println("Invalid functionality, choose one from the list above")
        exit(1)
    end
end

main()
