# Problem 3
# (a)
# Simulation and Visualization (for initial plotting)
set.seed(1)
n <- 150
X <- matrix(runif(n, min = 0, max = 1), ncol = 1) # Note: max should be 1, not 1, 1 as in the source [cite: 27, 28]
ftrue <- function(x) sin(2 * pi * x) + 0.5 * cos(8 * pi * x)
y <- ftrue(X[, 1]) + rnorm(n, sd = 0.1)

plot(X, y, main = "Simulated Data and True Function", xlab = "X", ylab = "Y")
x_seq <- seq(0, 1, length.out = 100)
lines(x_seq, ftrue(x_seq), col = "blue", lwd = 2)

# (b)
library(shiny)
library(ggplot2)

# --- UI Definition ---
ui <- fluidPage(
  titlePanel("KRR Parameter Explorer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("rho", "Kernel Bandwidth (rho):", min = 0.1, max = 20, value = 5, step = 0.1),
      sliderInput("lambda", "Penalty (lambda):", min = 0.0001, max = 0.1, value = 0.01, step = 0.0001)
    ),
    mainPanel(
      plotOutput("krrPlot")
    )
  )
)

# --- Server Definition ---
server <- function(input, output) {
  
  # Re-simulate data (or load pre-simulated data)
  X <- matrix(runif(150, min = 0, max = 1), ncol = 1)
  ftrue <- function(x) sin(2 * pi * x) + 0.5 * cos(8 * pi * x)
  y <- ftrue(X[, 1]) + rnorm(150, sd = 0.1)
  x_seq <- seq(0, 1, length.out = 100)
  
  output$krrPlot <- renderPlot({
    # 1. Fit KRR
    krr_model <- krr_fit(X, y, rho = input$rho, lambda = input$lambda)
    
    # 2. Predict on a sequence
    y_pred <- predict(krr_model, newdata = matrix(x_seq, ncol = 1))
    
    # 3. Create Plot Data
    data_points <- data.frame(X = X[, 1], Y = y)
    prediction_line <- data.frame(X = x_seq, Y_pred = y_pred, Y_true = ftrue(x_seq))
    
    # 4. Plot using ggplot2
    ggplot(data_points, aes(x = X, y = Y)) +
      geom_point(alpha = 0.6) + # Simulated data points
      geom_line(data = prediction_line, aes(x = X, y = Y_true), color = "blue", linewidth = 1, linetype = "dashed") + # True function
      geom_line(data = prediction_line, aes(x = X, y = Y_pred), color = "red", linewidth = 1.2) + # KRR prediction
      labs(title = paste("KRR Fit (rho=", input$rho, ", lambda=", input$lambda, ")"),
           subtitle = "Red: KRR Prediction, Blue Dashed: True Function",
           x = "X", y = "Y") +
      theme_minimal()
  })
}

# Run the application
# shinyApp(ui = ui, server = server)

# (c)
파라미터
1. ρ (Kernel Bandwidth) 
역할: 커널의 폭 또는 영향 범위를 결정, ρ가 클수록 커널의 폭이 좁아져 가까운 데이터만 영향을 미침.
관찰되는 변화: 과적합 경향이 강해짐, 예측 곡선이 데이터 포인트에 더욱 민감하게 반응하여 노이즈까지 포착함(유연성 증가).

2. λ (Penalty Parameter)
역할: 정규화(Regularization) 정도를 결정, λ가 클수록 모델의 복잡도에 대한 페널티가 커짐.
관찰되는 변화: 과소적합 경향이 강해짐, 예측 곡선이 단순화되어 노이즈에 덜 민감해짐(평활성 증가).

적절한 ρ와 λ값: 눈으로 확인했을 때, 예측 곡선(빨간색)이 참 함수(파란색 점선)를 적절히 추정하면서 노이즈에 과하게 반응하지 않는 지점을 찾는다.
ρ=5.0, λ=0.005
