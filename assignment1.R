# Problem1

# 난수 고정
set.seed(1)

# 데이터 생성
n <- 200
x <- seq(0, 1, length.out = n)
y <- sin(2 * pi * x) + rnorm(n, sd = 0.15)

# 데이터 프레임 생성
data <- data.frame(x = x, y = y)

# data 폴더가 없으면 생성
if (!dir.exists("data")) {
  dir.create("data")
}

#(a) CSV 파일로 저장
write.csv(data, file = "data/sin_data.csv", row.names = FALSE)

# 완료 메시지
cat("✅ 'data/sin_data.csv' 파일이 성공적으로 저장되었습니다.\n")

# 필요한 패키지 로드
library(ggplot2)

#(b) 데이터 불러오기
data <- read.csv("data/sin_data.csv")

# 데이터 확인
head(data)

#(c) ggplot2로 시각화
p <- ggplot(data, aes(x = x, y = y)) +
  geom_point(alpha = 0.6, color = "steelblue") +               # 산점도
  geom_smooth(method = "loess", color = "red", se = TRUE) +    # 비선형 회귀곡선 (loess)
  labs(
    title = "Sine Data with LOESS Smoothing",
    x = "x",
    y = "y"
  ) +
  theme_minimal(base_size = 14)

# 그래프 출력
print(p)


# Problem 2 (a)
bubble_sort = function(x, decreasing = FALSE) {
  n = length(x)
  # 정렬이 수행되었는지 확인하여 조기 종료(early termination)를 위한 플래그
  swapped = TRUE
  
  for (i in 1:(n - 1)) {
    swapped = FALSE
    # 각 패스(pass) 후 가장 큰/작은 원소가 끝으로 이동하므로,
    # 비교 범위는 n-i로 줄어듦.
    for (j in 1:(n - i)) {
      # 오름차순 (decreasing = FALSE): 앞의 항이 뒤의 항보다 크면 교환
      if (!decreasing && x[j] > x[j + 1]) {
        temp = x[j]
        x[j] = x[j + 1]
        x[j + 1] = temp
        swapped = TRUE
      }
      # 내림차순 (decreasing = TRUE): 앞의 항이 뒤의 항보다 작으면 교환
      else if (decreasing && x[j] < x[j + 1]) {
        temp = x[j]
        x[j] = x[j + 1]
        x[j + 1] = temp
        swapped = TRUE
      }
    }
    # 한 번의 패스에서 교환이 전혀 없으면 이미 정렬된 상태이므로 조기 종료
    if (!swapped) break
  }
  return(x)
}

# 테스트 케이스: set.seed(1), x = runif(10)
set.seed(1)
x = runif(10)
print(paste("Original vector (Bubble Sort):", toString(round(x, 4))))

# 오름차순 결과
x_asc_bubble = bubble_sort(x, decreasing = FALSE)
print(paste("Ascending (Bubble Sort):", toString(round(x_asc_bubble, 4))))

# 내림차순 결과
x_desc_bubble = bubble_sort(x, decreasing = TRUE)
print(paste("Descending (Bubble Sort):", toString(round(x_desc_bubble, 4))))


# Problem 2 (b)
quick_sort = function(x, decreasing = FALSE) {
  # 재귀 종료 조건: 배열 크기가 0 또는 1
  if (length(x) <= 1) {
    return(x)
  }
  
  # 1. Pivot 선택 (여기서는 간단하게 첫 번째 원소를 사용)
  pivot = x[1]
  # 나머지 원소
  rest = x[-1]
  
  # 2. 분할 (Partitioning)
  # 오름차순: pivot보다 작은 원소는 왼쪽, 큰 원소는 오른쪽
  if (!decreasing) {
    smaller = rest[rest < pivot]
    larger_equal = rest[rest >= pivot]
  }
  # 내림차순: pivot보다 큰 원소는 왼쪽, 작은 원소는 오른쪽
  else {
    smaller = rest[rest > pivot]  # 내림차순에서는 '큰' 원소가 먼저 옴
    larger_equal = rest[rest <= pivot] # 내림차순에서는 '작은' 원소가 뒤로 감
  }
  
  # 3. 재귀 호출 및 결과 결합
  # 'smaller' 배열 정렬 -> pivot -> 'larger_equal' 배열 정렬
  return(c(quick_sort(smaller, decreasing),
           pivot,
           quick_sort(larger_equal, decreasing)))
}

# 테스트 케이스: set.seed(1), x = runif(10)
set.seed(1)
x = runif(10)
print(paste("Original vector (Quick Sort):", toString(round(x, 4))))

# 오름차순 결과
x_asc_quick = quick_sort(x, decreasing = FALSE)
print(paste("Ascending (Quick Sort):", toString(round(x_asc_quick, 4))))

# 내림차순 결과
x_desc_quick = quick_sort(x, decreasing = TRUE)
print(paste("Descending (Quick Sort):", toString(round(x_desc_quick, 4))))


# Problem 3 (a)
numerical_derivative = function(f, x, h = 1e-6, method = "central") {
  # method 인자 유효성 검사
  if (!method %in% c("forward", "backward", "central")) {
    stop("Method must be 'forward', 'backward', or 'central'.")
  }
  
  if (method == "forward") {
    # 전진차분: f'(x) ≈ (f(x+h) - f(x)) / h
    return((f(x + h) - f(x)) / h)
  } else if (method == "backward") {
    # 후진차분: f'(x) ≈ (f(x) - f(x-h)) / h
    return((f(x) - f(x - h)) / h)
  } else if (method == "central") {
    # 중심차분: f'(x) ≈ (f(x+h) - f(x-h)) / (2h)
    return((f(x + h) - f(x - h)) / (2 * h))
  }
}

# 테스트 함수: f(x) = cos(x) - x
f = function(x) cos(x) - x
# 해석적 도함수: f'(x) = -sin(x) - 1
f_prime_analytic = function(x) -sin(x) - 1

# 구간 [0, 2pi]를 100개 점으로 균등 분할
x_vals = seq(0, 2 * pi, length.out = 100)

# 해석적 도함수 계산
y_prime_analytic = f_prime_analytic(x_vals)

# 수치 미분 계산 (중심차분 사용, 오차 O(h^2)로 가장 정확)
y_prime_numerical = numerical_derivative(f, x_vals, h = 1e-6, method = "central")

# 시각화를 위한 데이터프레임 생성
library(ggplot2)
derivative_df = data.frame(
  x = x_vals,
  Analytic = y_prime_analytic,
  Numerical = y_prime_numerical
)

# 시각화 (해석적 도함수와 수치 미분값 비교)
plot_derivative = ggplot(derivative_df, aes(x = x)) +
  geom_line(aes(y = Analytic, color = "Analytic"), linewidth = 1) +
  geom_line(aes(y = Numerical, color = "Numerical"), linewidth = 1, linetype = "dashed") +
  labs(title = expression(paste("Comparison of Analytic and Numerical Derivatives for ", f(x) == cos(x) - x)),
       x = "x", y = "f'(x)", color = "Method") +
  scale_color_manual(values = c("Analytic" = "blue", "Numerical" = "red")) +
  theme_minimal()

print(plot_derivative)


# Problem 3 (b)
# Problem 3 (a)에서 구현한 numerical_derivative 함수를 사용합니다.

newton_raphson = function(f, fprime = NULL, x0, maxiter = 100, h = 1e-6, epsilon = 1e-10) {
  x_current = x0
  
  for (t in 1:maxiter) {
    # 도함수 fprime 계산: 제공되면 사용, 아니면 수치 미분 사용 (중심차분)
    if (is.null(fprime)) {
      f_prime_val = numerical_derivative(f, x_current, h = h, method = "central")
    } else {
      f_prime_val = fprime(x_current)
    }
    
    # 도함수 값이 0에 가까우면 수렴하지 않거나 특이점일 수 있으므로 중단
    if (abs(f_prime_val) < 1e-15) {
      warning("Derivative is near zero. Algorithm stopped.")
      return(x_current)
    }
    
    # Newton-Raphson 공식: x_t = x_{t-1} - f(x_{t-1}) / f'(x_{t-1})
    x_next = x_current - f(x_current) / f_prime_val
    
    # 수렴 조건 체크: |x_t - x_{t-1}| < epsilon
    if (abs(x_next - x_current) < epsilon) {
      return(x_next) # 수렴된 해 반환
    }
    
    x_current = x_next
  }
  
  warning("Maximum iterations reached without convergence.")
  return(x_current) # 최대 반복 횟수 도달 시 최종값 반환
}


# Problem 3 (c)
# f(x) = cos(x) - x
f = function(x) cos(x) - x
# 해석적 도함수 (fprime 제공 버전용): f'(x) = -sin(x) - 1
f_prime_analytic = function(x) -sin(x) - 1

x0 = 0.5 # 초깃값

# 1. 수치미분 버전 (fprime = NULL)
solution_numerical = newton_raphson(f = f, fprime = NULL, x0 = x0)

# 2. 도함수 제공 버전 (fprime = f_prime_analytic)
solution_analytic_fprime = newton_raphson(f = f, fprime = f_prime_analytic, x0 = x0)

print(paste("Solution (Numerical Derivative):", solution_numerical))
print(paste("Solution (Analytic Derivative):", solution_analytic_fprime))

# 결과 확인 (두 결과가 매우 유사함을 확인)
print(paste("Difference:", abs(solution_numerical - solution_analytic_fprime)))


# Problem 4 (a), (b), (c)
# n: 분할 개수

# (a) Left Rectangle
left_rectangle = function(f, a, b, n) {
  h = (b - a) / n
  # 등간격 분할점 x_i = a + i*h, i=0, ..., n-1
  x_i = seq(a, b - h, length.out = n)
  
  # 적분값 ≈ h * sum(f(x_i))
  return(h * sum(f(x_i)))
}

# (b) Trapezoid
trapezoid = function(f, a, b, n) {
  h = (b - a) / n
  # 등간격 분할점 x_i = a + i*h, i=0, ..., n
  x_i = seq(a, b, length.out = n + 1)
  f_i = f(x_i)
  
  # 적분값 ≈ h/2 * (f0 + 2*sum(f1..f(n-1)) + fn)
  # f_i[1] = f0, f_i[n+1] = fn
  # f_i[2:n] = f1, ..., f(n-1)
  return(h / 2 * (f_i[1] + 2 * sum(f_i[2:n]) + f_i[n + 1]))
}

# (c) Simpson (n 짝수 조건 확인 필요)
simpson = function(f, a, b, n) {
  if (n %% 2 != 0) {
    stop("n must be an even number for Simpson's Rule.")
  }
  
  h = (b - a) / n
  # 등간격 분할점 x_i = a + i*h, i=0, ..., n
  x_i = seq(a, b, length.out = n + 1)
  f_i = f(x_i)
  
  # 인덱스 (R은 1부터 시작)
  # 홀수 인덱스 i=1, 3, ..., n-1 (R: 2, 4, ..., n) -> i_odd = 1, 3, ..., n-1 (원소)
  # 짝수 인덱스 i=2, 4, ..., n-2 (R: 3, 5, ..., n-1) -> i_even = 2, 4, ..., n-2 (원소)
  
  # 인덱스 1 (f0)과 n+1 (fn) 제외
  # 홀수 항 (f1, f3, ...): R 인덱스 2, 4, ..., n (총 n/2 개)
  sum_odd = sum(f_i[seq(2, n, by = 2)]) # f1, f3, ...
  # 짝수 항 (f2, f4, ...): R 인덱스 3, 5, ..., n-1 (총 n/2 - 1 개)
  sum_even = sum(f_i[seq(3, n - 1, by = 2)]) # f2, f4, ...
  
  # 적분값 ≈ h/3 * (f0 + 4*sum(f_odd) + 2*sum(f_even) + fn)
  return(h / 3 * (f_i[1] + 4 * sum_odd + 2 * sum_even + f_i[n + 1]))
}


# Problem 4 (d)
f_sin = function(x) sin(x)
a = 0
b = pi
n = 100

# 해석적 적분값: int_0^pi sin(x) dx = [-cos(x)]_0^pi = (-cos(pi)) - (-cos(0)) = 1 - (-1) = 2
analytic_value = 2

# Left Rectangle
rect_val = left_rectangle(f_sin, a, b, n)
# Trapezoid
trap_val = trapezoid(f_sin, a, b, n)
# Simpson (n=100은 짝수이므로 가능)
simp_val = simpson(f_sin, a, b, n)

print(paste("Analytic Value:", analytic_value))
print(paste("Left Rectangle (n=100):", rect_val))
print(paste("Trapezoid (n=100):", trap_val))
print(paste("Simpson (n=100):", simp_val))


# Problem 4 (e)
n_vals = c(10, 30, 60, 100, 150, 200)
analytic_value = 2
errors_df = data.frame(n = n_vals)

rect_errors = c()
trap_errors = c()
simp_errors = c()

for (n in n_vals) {
  # Simpson은 n이 짝수일 때만 작동하므로, n이 홀수인 경우 Skip 또는 에러 처리 필요
  # 제시된 n_vals는 모두 짝수/홀수 혼재이므로, n=30, 150은 Simpson에서 오류 가능성이 있으나
  # 문제에서 n_vals를 제시했으므로, Simpson의 n 짝수 요구를 만족하도록 n=30을 30(짝수)로 가정하거나
  # n=150은 150(짝수)로 가정함. (실제로는 n=150은 홀수이므로 오류 발생)
  
  # Simpson은 n이 짝수일 때만 사용. n=10, 30, 60, 100, 200만 사용하거나,
  # n=150은 n=150-1=149로 조정하거나 에러를 허용해야 함.
  # 여기서는 Simpson의 요구사항에 맞춰 짝수인 경우만 계산하고, 홀수인 경우 NA 처리합니다.
  
  rect_val = left_rectangle(f_sin, a, b, n)
  trap_val = trapezoid(f_sin, a, b, n)
  
  if (n %% 2 == 0) {
    simp_val = simpson(f_sin, a, b, n)
  } else {
    simp_val = NA # n이 홀수인 경우 (n=150)
  }
  
  rect_errors = c(rect_errors, abs(rect_val - analytic_value))
  trap_errors = c(trap_errors, abs(trap_val - analytic_value))
  simp_errors = c(simp_errors, abs(simp_val - analytic_value))
}

errors_df$LeftRectangle = rect_errors
errors_df$Trapezoid = trap_errors
errors_df$Simpson = simp_errors

# 시각화를 위한 long format으로 변환
library(tidyr)
errors_long = pivot_longer(errors_df, cols = c(LeftRectangle, Trapezoid, Simpson),
                           names_to = "Method", values_to = "AbsoluteError")

# 오차 비교 시각화
plot_errors = ggplot(errors_long, aes(x = n, y = AbsoluteError, color = Method, shape = Method)) +
  geom_point(size = 3) +
  geom_line() +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + # y축을 log 스케일로 설정하여 오차 감소율 비교
  labs(title = expression(paste("Absolute Error vs. Number of Partitions (log scale) for ", int(sin(x), dx, 0, pi))),
       x = "Number of Partitions (n)", y = "Absolute Error |Approx - Analytic| (log10)") +
  theme_minimal()

print(plot_errors)


# Problem 5 (a)
A = matrix(c(4, 2, 2, 2, 5, 1, 2, 1, 3), 3, 3)
print("Matrix A:")
print(A)

# R의 chol() 함수는 A = U^T U를 만족하는 상삼각행렬 U를 반환
U = chol(A)
print("Upper triangular matrix U from chol(A):")
print(U)

# L은 A = L L^T를 만족하는 하삼각행렬 (U의 전치)
L = t(U)
print("Lower triangular matrix L (L = t(U)):")
print(L)

# L L^T 계산 및 A와 비교
LLt = L %*% t(L)
print("L %*% t(L):")
print(LLt)

# A와 LLt가 같음을 확인 (rounding error 허용)
print("Difference A - L %*% t(L):")
print(A - LLt)


# Problem 5 (b)
forward_sub = function(L, b) {
  n = nrow(L)
  z = numeric(n)
  
  for (i in 1:n) {
    # L[i, i]는 l_ii
    # b[i]는 y_i (문제에서 y 대신 b 사용)
    # L[i, 1:(i-1)]는 l_i1, ..., l_i(i-1)
    # z[1:(i-1)]는 z1, ..., z(i-1)
    
    # i=1인 경우 sum(L[i, 1:(i-1)] * z[1:(i-1)])는 0이 됨
    if (i == 1) {
      sum_term = 0
    } else {
      sum_term = sum(L[i, 1:(i - 1)] * z[1:(i - 1)])
    }
    
    # z_i 계산 공식
    if (L[i, i] == 0) stop("Diagonal element is zero. Cannot proceed.")
    z[i] = (b[i] - sum_term) / L[i, i]
  }
  return(z)
}


# Problem 5 (c)
backward_sub = function(L, z) {
  n = nrow(L)
  x = numeric(n)
  
  # 뒤에서부터 (i=n, n-1, ..., 1) 반복
  for (i in n:1) {
    # L[i, i]는 l_ii
    # L[j, i]는 l_ji. U=L^T의 i열에 해당.
    # U[i, j]는 U의 i행 j열 원소. U[i, j] = L[j, i]
    
    # i=n인 경우 sum(L[j, i] * x[j])는 0이 됨 (j=n+1에서 n까지 합)
    if (i == n) {
      sum_term = 0
    } else {
      # L[j, i] * x[j] (j=i+1, ..., n)
      # L[, i]는 L의 i열 전체
      sum_term = sum(L[(i + 1):n, i] * x[(i + 1):n])
    }
    
    # x_i 계산 공식
    if (L[i, i] == 0) stop("Diagonal element is zero. Cannot proceed.")
    # x_i = (z_i - sum_{j=i+1}^n l_{ji} x_j) / l_{ii}
    x[i] = (z[i] - sum_term) / L[i, i]
  }
  return(x)
}


# Problem 5 (d)
b = c(1, 2, 3)

# (a)에서 구한 L 사용
# L = t(chol(A))

# Step 1: Lz = b를 forward_sub 함수로 풀기
z_val = forward_sub(L, b)
print("Intermediate vector z (Lz = b):")
print(z_val)

# Step 2: L^T x = z를 backward_sub 함수로 풀기 (L^T의 원소로 L의 원소 l_ji 사용)
x_val = backward_sub(L, z_val)
print("Solution x (Cholesky method):")
print(x_val)

# R의 내장 함수 solve(A, b)로 구한 해와 비교
x_solve_builtin = solve(A, b)
print("Solution x (R solve(A, b)):")
print(x_solve_builtin)

# 두 결과가 일치함을 확인 (rounding error 허용)
print("Difference |x_cholesky - x_builtin|:")
print(abs(x_val - x_solve_builtin))



# Problem 6 (a)
gaussian_kernel <- function(x, x_prime, rho = 1) {
  # |x - x'|^2 계산
  dist_sq <- as.matrix(dist(rbind(x, x_prime)))^2
  n1 <- nrow(x)
  n2 <- nrow(x_prime)
  
  # dist()가 전체 거리 행렬을 반환하므로 필요한 부분만 추출
  K <- matrix(0, nrow = n1, ncol = n2)
  for (i in 1:n1) {
    for (j in 1:n2) {
      diff <- sum((x[i, ] - x_prime[j, ])^2)
      K[i, j] <- exp(-rho * diff)
    }
  }
  return(K)
}

# Problem 6 (b)
krr <- function(X, y, lambda = 0.0001, rho = 1) {
  # 커널 행렬 K 계산
  K <- gaussian_kernel(X, X, rho)
  
  # alpha = (K + λI)^(-1) y
  n <- nrow(K)
  alpha <- solve(K + lambda * diag(n)) %*% y
  
  # 결과를 리스트로 class 'krr'로 반환
  model <- list(X = X, y = y, alpha = alpha, lambda = lambda, rho = rho)
  class(model) <- "krr"
  return(model)
}

# Problem 6 (c)
predict.krr <- function(model, X_new) {
  K_new <- gaussian_kernel(X_new, model$X, model$rho)
  y_pred <- K_new %*% model$alpha
  return(y_pred)
}


# Problem 6 (d)
plot.krr <- function(model, ftrue = NULL) {
  X <- model$X
  y <- model$y
  
  # 예측용 x범위
  X_grid <- matrix(seq(min(X), max(X), length.out = 200), ncol = 1)
  y_pred <- predict(model, X_grid)
  
  # 실제 데이터 산점도
  plot(X, y, pch = 16, col = "grey40", main = "Kernel Ridge Regression",
       xlab = "X", ylab = "y")
  
  # 예측 곡선
  lines(X_grid, y_pred, col = "blue", lwd = 2)
  
  # 진짜 함수가 있으면 함께 그림
  if (!is.null(ftrue)) {
    lines(X_grid, ftrue(X_grid), col = "red", lwd = 2, lty = 2)
    legend("topright", legend = c("Observed", "Predicted", "True f(x)"),
           col = c("grey40", "blue", "red"), pch = c(16, NA, NA),
           lty = c(NA, 1, 2), lwd = c(NA, 2, 2))
  } else {
    legend("topright", legend = c("Observed", "Predicted"),
           col = c("grey40", "blue"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))
  }
}

# Problem 6 (e)
set.seed(1)
n = 150
X = matrix(runif(n, -1, 1), ncol = 1)
ftrue = function(x) sin(2*pi*x) + 0.5*cos(4*pi*x)
y = ftrue(X[,1]) + rnorm(n, sd = 0.1)

# KRR 모델 적합
model <- krr(X, y, lambda = 0.001, rho = 5)

# 예측 및 시각화
plot(model, ftrue)



