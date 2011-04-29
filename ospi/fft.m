clear all, close all;
nTimePoints = 512;
T = 10;
Time = linspace(0,T-T/nTimePoints,nTimePoints);
x = 2*((Time <= T/2)-0.5)';
plot(Time,x);
% Use fft to find the Fourier Coefficients
Xscaled = fft(x);
% rescale the Fourier coefficients
X = Xscaled/nTimePoints';
% Extract the DC coefficient
XDC = X(1);

% Extract the cos and sin coefficients
if (rem(nTimePoints,2) == 0)
  % Even number of points
  Xc = 2*real(X([2:nTimePoints/2]));
  Xs = -2*imag(X([2:nTimePoints/2]));
  figure, semilogy([1:(nTimePoints/2-1)],abs(Xc),'o',
  [1:(nTimePoints/2-1)],abs(Xs),'*'),
  grid,xlabel('k Harmonic number')
else
  % Odd number of points
  Xc = 2*real(X([2:(nTimePoints-1)/2+1]));
  Xs = 2*imag(X([2:(nTimePoints-1)/2+1]));
end
