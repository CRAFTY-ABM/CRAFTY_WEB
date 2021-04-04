# https://bearloga.github.io/bayesopt-tutorial-r/
# https://github.com/bearloga/bayesopt-tutorial-r


# library(magrittr)
library(zeallot)
library(glue)
library(GPfit)
library(animation)

# Goal: find x and y which maximize f
f <- function(x) {
    return((6 * x - 2)^2 * sin(12 * x - 4))
}

c(max_evals, seed_evals) %<-% c(8, 4)

# par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))
# curve(f(x), x_min, x_max)
# points(evaluations[1:seed_evals, ], pch = 16)

# evaluations of f:
evaluations <- matrix(
    as.numeric(NA),
    ncol = 2, nrow = seed_evals,
    dimnames = list(NULL, c("x", "y"))
)
# seed with a few evaluations:
evaluations[1:seed_evals, "x"] <- seq(0, 1, length.out = seed_evals)
evaluations[1:seed_evals, "y"] <- f(evaluations[1:seed_evals, "x"])

set.seed(0)
bayesian_optimize <- function(optmize_function, init_evals, max_iter, acquisition_function, minimize = TRUE, control = NULL) {
    # expand to hold additional evaluations:
    evaluations <- rbind(init_evals, matrix(
        as.numeric(NA),
        ncol = 2, nrow = max_iter,
        dimnames = list(NULL, c("x", "y"))
    ))
    
    if (is.null(control)) {
        control <- list(cov = list(type = "exponential", power = 1.95))
        # control <- list(type = "matern", nu = 5/2)
    }
    if (acquisition_function == "cb") {
        if (is.null(control$kappa)) {
            control$kappa <- 2
        }
    }
    x_new <- seq(0, 1, length.out = 100) # potential x's to evaluate
    
    for (eval_iter in (nrow(init_evals) + 1):(nrow(init_evals) + max_iter)) {
        
        fit <- GP_fit(
            X = evaluations[1:(eval_iter - 1), "x"],
            Y = evaluations[1:(eval_iter - 1), "y"],
            corr = control$cov
        )
        
        predictions <- predict.GP(fit, xnew = data.frame(x = x_new))
        mu <- predictions$Y_hat
        sigma <- sqrt(predictions$MSE)
        
        if (minimize) {
            y_best <- min(evaluations[, "y"], na.rm = TRUE)
        } else {
            y_best <- max(evaluations[, "y"], na.rm = TRUE)
        }
        
        if (acquisition_function == "poi") {
            # Probability of improvement:
            acquisition <- purrr::map2_dbl(mu, sigma, function(m, s) {
                if (s == 0) return(0)
                else return(pnorm((y_best - m) / s))
            })
            if (!minimize) {
                acquisition <- 1 - acquisition
            }
            x_next <- x_new[which.max(acquisition)]
            plot(x_new, acquisition, type = "l", col = "red", ylim = c(0, 1), xlab = "x", ylab = expression("a"["POI"]))
        } else if (acquisition_function == "ei") {
            # Expected improvement:
            acquisition <- purrr::map2_dbl(mu, sigma, function(m, s) {
                if (s == 0) return(0)
                gamma <- (y_best - m) / s
                if (minimize) {
                    phi <- pnorm(gamma)
                } else {
                    phi <- 1 - pnorm(gamma)
                }
                return(s * (gamma * phi + dnorm(gamma)))
            })
            x_next <- x_new[which.max(acquisition)]
            plot(x_new, acquisition, type = "l", col = "red", xlab = "x", ylab = expression("a"["EI"]))
        } else if (acquisition_function == "cb") {
            # GB upper/lower confidence bound:
            if (minimize) {
                acquisition <- mu - control$kappa * sigma
                x_next <- x_new[which.min(acquisition)]
                plot(x_new, acquisition, type = "l", col = "red", xlab = "x", ylab = expression("a"["LCB"]))
            } else {
                acquisition <- mu + control$kappa * sigma
                x_next <- x_new[which.max(acquisition)]
                plot(x_new, acquisition, type = "l", col = "red", xlab = "x", ylab = expression("a"["UCB"]))
            }
        } else {
            stop("acquisition_function must be 'poi', 'ei', 'cb'")
        }
        
        abline(v = x_next, lty = "dashed", col = "red", lwd = 2)
        acquisition_function_label <- switch(
            acquisition_function,
            "poi" = "probability of improvement",
            "ei" = "expected improvement",
            "cb" = paste("GP", ifelse(minimize, "lower"), "confidence bound")
        )
        legend("topleft", glue("proposal via {acquisition_function_label}"), bty = "n", col = "red", lty = "dashed", lwd = 2)
        
        # Visualize hidden function and GP fit:
        curve(f(x), 0, 1, lwd = 1.5)
        lines(x_new, mu, col = "blue", lwd = 2, lty = "dotted")
        polygon(c(x_new, rev(x_new)), c(mu + sigma, rev(mu - sigma)), col = rgb(0, 0, 1, 0.25), border = NA)
        points(evaluations, pch = 16)
        points(evaluations[(eval_iter - 1), "x"], evaluations[(eval_iter - 1), "y"], pch = 16, col = "red")
        abline(v = x_next, lty = "dashed", col = "red", lwd = 2)
        legend("topleft", "most recent evaluation", bty = "n", col = "red", pch = 16)
        
        y_next <- f(x_next)
        evaluations[eval_iter, ] <- c(x_next, y_next)
    }
    
    return(list(x = x_next, y = y_next))
}

# Visualize | mar = c(5.1, 4.1, 4.1, 2.1) (bottom, left, top, right)
purrr::walk(c("poi", "ei", "cb"), function(af) {
    # Static images:
    png(glue("bayesopt_{af}.png"), width = 12, height = 12, units = "in", res = 300)
    par(mfrow = c(4, 2), mar = c(4.1, 4.1, 0.5, 0.5), cex = 1.1)
    bayesian_optimize(f, evaluations, max_evals - seed_evals, af)
    dev.off()
    # Animated GIF:
    saveGIF(
        {
            par(mfrow = c(1, 2), mar = c(4.1, 4.1, 0.5, 0.5), cex = 1.1)
            bayesian_optimize(f, evaluations, max_evals - seed_evals, af)
        },
        glue("bayesopt_{af}.gif"), nmax = 4, loop = TRUE, interval = 1.5,
        ani.width = 900, ani.height = 300, ani.dev = "png",
        autobrowse = FALSE
    )
})

# Optimization with gradient descent in TensorFlow:
library(tensorflow)
library(animation)
sess = tf$Session()

x <- tf$Variable(0.0, trainable = TRUE)
f <- function(x) (6 * x - 2)**2 * tf$sin(12 * x - 4)

adam <- tf$train$AdamOptimizer(learning_rate = 0.3)

f_x <- f(x)
opt <- adam$minimize(f_x, var_list = x)
sess$run(tf$global_variables_initializer())

saveGIF(
    {
        par(mfrow = c(1, 1), mar = c(4.1, 4.1, 0.5, 0.5), cex = 2)
        for (i in 1:20) {
            sess$run(opt)
            curve((6 * x - 2)**2 * sin(12 * x - 4), 0, 1, xlab = "x", ylab = "f(x)", lwd = 2)
            x_best <- sess$run(x)
            y_best <- sess$run(f_x)
            points(x_best, y_best, cex = 2, col = "red", pch = 16)
            points(x_best, y_best, cex = 2, col = "black", lwd = 2)
        }
    },
    "tf_adam.gif", nmax = 20, loop = TRUE, interval = 0.3,
    ani.width = 900, ani.height = 600, ani.dev = "png",
    autobrowse = FALSE
)



















# The ideal scenario is that f is known, has a closed, analytical form, and is differentiable – which would enable us to use gradient descent-based algorithms For example, here’s how we might optimize it with Adam [2]  in TensorFlow [3]  :
library(tensorflow)
sess = tf$Session()

x <- tf$Variable(0.0, trainable = TRUE)
f <- function(x) (6 * x - 2)^2 * tf$sin(12 * x - 4)

adam <- tf$train$AdamOptimizer(learning_rate = 0.3)
opt <- adam$minimize(f(x), var_list = x)

sess$run(tf$global_variables_initializer())

for (i in 1:20) {
    sess$run(opt)
}

x_best <- sess$run(x)


library(DiceOptim)

# Evaluation of the Augmented Expected Improvement (AEI) criterion, which is a modification of the classical EI criterion for noisy functions. The AEI consists of the regular EI multiplied by a penalization function that accounts for the disminishing payoff of observation replicates. The current minimum y.min is chosen as the kriging predictor of the observation with smallest kriging quantile.

set.seed(421)

# Set test problem parameters
doe.size <- 12
dim <- 2
test.function <- get("branin2")
lower <- rep(0,1,dim)
upper <- rep(1,1,dim)
noise.var <- 0.2

# Generate DOE and response
doe <- as.data.frame(matrix(runif(doe.size*dim),doe.size))
y.tilde <- rep(0, 1, doe.size)
for (i in 1:doe.size)  {
    y.tilde[i] <- test.function(doe[i,]) + sqrt(noise.var)*rnorm(n=1)
}
y.tilde <- as.numeric(y.tilde)

# Create kriging model
model <- km(y~1, design=doe, response=data.frame(y=y.tilde),
            covtype="gauss", noise.var=rep(noise.var,1,doe.size), 
            lower=rep(.1,dim), upper=rep(1,dim), control=list(trace=FALSE))

# Compute actual function and criterion on a grid
n.grid <- 12 # Change to 21 for a nicer picture
x.grid <- y.grid <- seq(0,1,length=n.grid)
design.grid <- expand.grid(x.grid, y.grid)
nt <- nrow(design.grid)
crit.grid <- rep(0,1,nt)
func.grid <- rep(0,1,nt)

crit.grid <- apply(design.grid, 1, AEI, model=model, new.noise.var=noise.var)
func.grid <- apply(design.grid, 1, test.function)

# Compute kriging mean and variance on a grid
names(design.grid) <- c("V1","V2")
pred <- predict.km(model, newdata=design.grid, type="UK")
mk.grid <- pred$m
sk.grid <- pred$sd

# Plot actual function
z.grid <- matrix(func.grid, n.grid, n.grid)
filled.contour(x.grid,y.grid, z.grid, nlevels=50, color = rainbow,
               plot.axes = {title("Actual function");
                   points(model@X[,1],model@X[,2],pch=17,col="blue"); 
                   axis(1); axis(2)})

# Plot Kriging mean
z.grid <- matrix(mk.grid, n.grid, n.grid)
filled.contour(x.grid,y.grid, z.grid, nlevels=50, color = rainbow,
               plot.axes = {title("Kriging mean");
                   points(model@X[,1],model@X[,2],pch=17,col="blue"); 
                   axis(1); axis(2)})

# Plot Kriging variance
z.grid <- matrix(sk.grid^2, n.grid, n.grid)
filled.contour(x.grid,y.grid, z.grid, nlevels=50, color = rainbow,
               plot.axes = {title("Kriging variance");
                   points(model@X[,1],model@X[,2],pch=17,col="blue"); 
                   axis(1); axis(2)})

# Plot AEI criterion
z.grid <- matrix(crit.grid, n.grid, n.grid)
filled.contour(x.grid,y.grid, z.grid, nlevels=50, color = rainbow,
               plot.axes = {title("AEI");
                   points(model@X[,1],model@X[,2],pch=17,col="blue"); 
                   axis(1); axis(2)})
