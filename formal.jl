#!/usr/bin/env julia
using Lazy

State = Union{Int,String}
Alphabet = Set{Char}

struct DFA
    Q:: Set{State}
    Σ:: Alphabet
    δ:: Dict{Tuple{State,Char},State}
    q0:: State
    F:: Set{State}
end

struct NFA
    Q:: Set{State}
    Σ:: Alphabet
    δ:: Dict{Tuple{State,Char},Set{State}}
    q0:: State
    F:: Set{State}
end

∅=Set([])

function concatenate(A,B)
    unique([a*b for a in A for b in B])
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
    for c in w
        if c ∉ Σ
            error("Current character ('$c') is not in Σ")
        end
        oldStates = currentPossibleStates
        currentPossibleStatesArray = [(if haskey(δ,(q,c));
                δ[(q,c)];
            else;
                ∅;
            end)
            for q in currentPossibleStates]
        currentPossibleStates = reduce(∪,currentPossibleStatesArray)
        # println(currentPossibleStates)
        if currentPossibleStates ⊈ Q
            error("δ[($oldStates,$c)]⊈ Q")
        end 
    end

    # println(currentPossibleStates∩ F)
    return currentPossibleStates ∩ F ≠ ∅
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
