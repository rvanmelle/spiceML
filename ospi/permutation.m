function Perm = FormPermutationMatrix(nTimePoints,MNA_Size)

% function 
%       Perm = FormPermutationMatrix(nTimePoints,MNA_Size) returns a
%
% permutation matrix Perm such that given a vector B in
% Harmonic-major/Node-Minor indexing mode the vecotor Perm*B is ordered in
% Node-Major/Harmonic-Minor mode.

Perm = spalloc(MNA_Size*nTimePoints,MNA_Size*nTimePoints,MNA_Size*nTimePoints);

VectorT = sparse(zeros(1,nTimePoints));
VectorN = sparse(zeros(1,MNA_Size));
CurrentRow = 1;

for n=1:MNA_Size
    VectorN(n) = 1;
    for t=1:nTimePoints
        VectorT(t) = 1;
        Perm(CurrentRow,:) = kron(VectorT,VectorN);
        VectorT(t) = 0;
        CurrentRow = CurrentRow+1;
    end
    VectorN(n) = 0;
end
        
        
    
