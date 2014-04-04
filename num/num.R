
# The total number of certicates in two solutions
total_num_dttm <- function(N, beta = 0.3, p = 0.3, k = 8)
{
    
    num <- N * 100 / (beta * k) + (N * (k - 1)) + (beta / 100) * p * N^2 + k^2 * (k - 1) + (beta / 100) * p * N^2
    
    return (num)
}

total_num_ttm <- function(N, beta = 0.3, p = 0.3, k = 8)
{
    num <- (N * 100 / beta) + (2 * beta / 100) * p * N^2

    return(num)
}

# The number of certificates hold by single normal node
num_dttm_single_normal_node <- function(N, beta = 0.3, p = 0.3, k = 8)
{
    num <- 100 / (beta * k) + (k - 1) + (p * N * beta / 100) 
    
    return (num)
}

num_ttm_single_normal_node <- function(N, beta = 0.3, p = 0.3, k = 8)
{
    num <- (100 / beta) + (p * N * beta / 100)
    
    return(num)
}

# The number of certificates hold by single hub node
num_dttm_single_hub_node <- function(N, beta = 0.3, p = 0.3, k = 8)
{
    num <- (k - 1) + (p * N / k)
    
    return (num)
}


num_ttm_single_hub_node <- function(N, beta = 0.3, p = 0.3, k = 8)
{
    num <- (p * N)

    return (num)
}


######################################################################
# function name: plot_total_num
# parameters: none
# description: plot a figure for comparing num_dttm and num_ttm
# Date: 27 March, 2014.
######################################################################
plot_total_num <- function()
{   
    x1 <- vector()
    y1 <- vector()
    
    x2 <- vector()
    y2 <- vector()
    
    index = 25000
    i = 1
    while(index <= 65000)
    {
        print(total_num_dttm(index))
        
        x1[i] <- index
        y1[i] <- total_num_dttm(index)

        x2[i] <- index
        y2[i] <- total_num_ttm(index)
        
        i = i + 1
        index = index + 5000    
    }

    # Calculate range from min to max value of y1 and y2
    g_range <- range(y1, y2)
    print("g_range")
    print(g_range)
    
    print("x1")
    print(x1)
    print("y1")
    print(y1)
    print("y2")
    print(y2)
    
    # Graph autos using y axis that ranges from 0 to max value in y1 or y2 vector.  
    # Turn off axes and annotations (axis labels) so we can specify them ourself
    plot(y1, type = "b", col = "blue", ylim = g_range, axes = FALSE, ann = FALSE)
    
    x_coordinate <- vector()

    for(index in 1:length(x1))
    {
        x_coordinate[index] <- x1[index] / 1000
    } 

    axis(1, at = 1:length(x_coordinate), labels = x_coordinate)

    # Make y axis with horizontal labels that display ticks at every 4 marks. 4*0:g_range[2] is equivalent to c(0,4,8,12).
    axis(2, las = 1, at = 10000000*0:g_range[2])

    # Create box around plot
    box()
    
    # Graph y2 with red dashed line and square points
    lines(y2, type = "o", pch = 22, lty = 2, col = "red")

    # Create a title with a red, bold/italic font
    title(main = "The total number of certicates in two solutions", col.main = "red", font.main = 4)
    
    # Label the x and y axes with dark green text
    title(xlab = "N (K)", col.lab = rgb(0, 0.5, 0))
    title(ylab = "Num", col.lab = rgb(0, 0.5, 0))
    
    # Create a legend at (25000, g_range[2]) that is slightly smaller (cex) and uses the same line colors and points used by the actual plots
    legend(25000, g_range[2], c("total_num_dttm", "total_num_ttm"), cex = 0.7, col = c("blue", "red"), pch = 21:22, lty = 1:2);
}

######################################################################
# function name: plot_num
# parameters: none
# description: plot a figure for comparing num_dttm and num_ttm from 
#              single normal node and single hub node
# Date: 27 March, 2014.
######################################################################
plot_num <- function()
{   
    x1 <- vector()
    y1 <- vector()
    
    x2 <- vector()
    y2 <- vector()
    
    x3 <- vector()
    y3 <- vector()
    
    x4 <- vector()
    y4 <- vector()

    index = 25000
    i = 1
    while(index <= 65000)
    { 
        x1[i] <- index
        y1[i] <- num_dttm_single_normal_node(index)

        x2[i] <- index
        y2[i] <- num_ttm_single_normal_node(index)

        x3[i] <- index
        y3[i] <- num_dttm_single_hub_node(index)

        x4[i] <- index
        y4[i] <- num_ttm_single_hub_node(index)
        
        i = i + 1
        index = index + 5000    
    }

    print(x1)
    print(y1)
    print(y2)
    print(y3)
    print(y4)
 
    g_range <- range(y1, y2, y3, y4)
    
    # Graph autos using y axis that ranges from 0 to max value in y1 or y2 vector.  
    # Turn off axes and annotations (axis labels) so we can specify them ourself    
    plot(y1, type = "b", col = "blue", ylim = g_range, axes = FALSE, ann = FALSE)
    
    # Make x axis using Mon-Fri labels
    x_coordinate <- vector() 

    for(index in 1:length(x1))
    {
        x_coordinate[index] <- x1[index] / 1000
    } 

    axis(1, at = 1:length(x_coordinate), labels = x_coordinate)

    # Make y axis with horizontal labels that display ticks at every 5000 marks. 5000*0:g_range[2] is equivalent to c(0, 5000, 10000, 15000).
    axis(2, las = 1, at = 5000 * 0:g_range[2])
    
    # Create box around plot
    box()
    
    # Graph trucks with red dashed line and square points
    lines(y2, type = "o", pch = 22, lty = 2, col = "red")
    lines(y3, type = "o", pch = 23, lty = 3, col = "green")
    lines(y4, type = "o", pch = 24, lty = 4, col = "black")

    # Create a title with a red, bold/italic font
    title(main = "The number of certificates hold by single normal node and single hub node", col.main = "red", font.main = 4)
    
    # Label the x and y axes with dark green text
    title(xlab = "N (K)", col.lab = rgb(0, 0.5, 0))
    title(ylab = "Num", col.lab = rgb(0, 0.5, 0))
    
    # Create a legend at (1, g_range[2]) that is slightly smaller (cex) and uses the same line colors and points used by the actual plots
    legend(1, g_range[2], c("num_dttm_single_normal_node", "num_ttm_single_normal_node", "num_dttm_single_hub_node", "num_ttm_single_hub_node"), cex = 0.7, col = c("blue", "red", "green", "black"), pch = 21:24, lty = 1:4);

}
