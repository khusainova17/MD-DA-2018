gmp.df<-data.frame(read.table("https://raw.githubusercontent.com/SergeyMirvoda/MD-DA-2018/master/data/gmp.dat"))
gmp.df$pop <- gmp.df$gmp / gmp.df$pcgmp

estimate.scaling.exponent <- function(a, y0=6611, response=gmp.df$pcgmp,
                                      predictor = gmp.df$pop, maximum.iterations=100, deriv.step = 1/100,
                                      step.scale = 1e-12, stopping.deriv = 1/100) {
  mse <- function(a) { mean((response - y0*predictor^a)^2) }
  for (iteration in 1:maximum.iterations) {
    deriv <- (mse(a+deriv.step) - mse(a))/deriv.step
    a <- a - step.scale*deriv
    if (abs(deriv) <= stopping.deriv) { break() }
  }
  fit <- list(a=a,iterations=iteration,
              converged=(iteration < maximum.iterations))
  return(fit)
}

#������ ������ � ��������� ��������� a
a<-estimate.scaling.exponent(0.15)
a #0.1211533

#� ������� ����������� ������������ ��������� ������ (������� curve) �����������
plot(pcgmp~pop, data=gmp.df, xlab="���������", log="xy",
     ylab="����� �� ���� ��������� ($/��������-���)", main="���������� ���, 2006")
curve(6611*x^a, add = TRUE, col = "red")

#������� ����� �� ������ �������� ������ ��������� �������, ��� ���������� �������������� ������ ������������ a?
gmp.df<-gmp.df[1:365,]
a<-estimate.scaling.exponent(0.15)
a #0.1212088

#��������� ������ ��������� ��� � ������ ��������� �����. ��� ���������� �������� a?
a<-estimate.scaling.exponent(0.2)
a #0.1212088
a<-estimate.scaling.exponent(0.1)
a #0.1212088
a<-estimate.scaling.exponent(0.05)
a #0.1212088
a<-estimate.scaling.exponent(0.3)
a #-2.854871
