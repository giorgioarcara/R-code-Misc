plotcircle <- function (x, y, r, ...) 
	{
		#x^2+y^2 = r^2
	
		angle = seq(0, 2*pi, length = 200)
		xc = x + (r * cos(angle))
		yc = y + (r * sin(angle))
		polygon(xc, yc, ...)
		return(list(x = xc, y = yc))	
	}
