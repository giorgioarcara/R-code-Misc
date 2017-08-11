
library(erpR)


if(require(akima)){
  
  data(ERPsets)
  
  word=grandaverage("Exp1_word_subj", 1:20, erplist=ERPsets)
  
  # check if some electrodes are not present in the list 
  # and create an object with these electrode names.
  
  notfound=topoplot(word, return.notfound=TRUE)
  
  #make a topoplot excluding not found electrode
  topoplot(word, startmsec=-200, endmsec=1500, win.ini=400,
           win.end=600, exclude=notfound)
}

# ho disegnato l'orecchio sinistro ad occhio, usando locator()
# nota che non vedevo i punti perchè locator non li mostra quindi
# andavo ad immaginazione 

# ear=locator()

ear=locator()

ear_left=structure(list(x = c(-0.973859844271413, -0.996106785317019, 
                              -1.00444938820912, -1.01279199110122, -1.02391546162403, -1.02947719688543, 
                              -1.03781979977753, -1.03781979977753, -1.03781979977753, -1.03781979977753, 
                              -1.04060066740823, -1.04338153503893, -1.04616240266963, -1.04616240266963, 
                              -1.03781979977753, -1.01001112347052, -0.990545050055617, -0.979421579532814
), y = c(0.181964824643563, 0.21578728275575, 0.21578728275575, 
         0.202258299510875, 0.181964824643563, 0.168435841398689, 0.127848891664065, 
         0.0872619419294414, 0.0534394838172549, 0.0263815173275055, -0.0209699240295555, 
         -0.0480278905193045, -0.0953793318763657, -0.129201789988552, 
         -0.156259756478301, -0.217140181080237, -0.2103756894578, -0.203611197835363
)), .Names = c("x", "y"))

ear_right=list(x=-ear_left$x, y=ear_left$y)


topoplot(word, startmsec=-200, endmsec=1500, win.ini=400,
         win.end=600, exclude=notfound, draw.elec.lab = F, draw.nose = T, draw.elec.pos = F)

lines(ear_right)
lines(ear_left)


ear=locator()



new_ear=structure(list(x = c(-0.977931701030928, -1.00236254295533, -1.01632302405498, 
                     -1.03028350515464, -1.04075386597938, -1.0473410652921, -1.0442439862543, 
                     -1.0342439862543, -1.03395386597938, -1.0372439862543, -1.0423410652921, 
                     -1.04273410652921, -1.0452439862543, -1.0472439862543, -1.035079338487972, 
                     -1.03283290378007, -0.995382302405498, -0.977931701030928, -0.967461340206185
), y = c(0.222551774378187, 0.236080757623062, 0.222551774378187, 
         0.202258299510875, 0.181964824643563, 0.154906858153814, 0.121084400041628, 
         0.0737329586845665, 0.033146008949943, -0.014205432407118, -0.054792382141742, 
         -0.10890831512124, -0.13596628161099, -0.169788739723176, -0.203611197835363, 
         -0.223904672702674, -0.244198147569986, -0.237433655947549, -0.230669164325112
)), .Names = c("x", "y"))


source("C:/Users/Mamma/Desktop/nuovi files/R files/erpr/pkg/R/topoplot.R")

png("prova.png", res=200, height=1000, width=1000)
topoplot(word, startmsec=-200, endmsec=1500, win.ini=400, head.lwd=2,
        ,win.end=600, exclude=NULL, draw.elec.lab = F, draw.nose = T, draw.ears=T,draw.elec.pos = T, interp.points=100)
dev.off()


topoplot.explore(word, startmsec=-200, endmsec=1500, win.ini=400,
         win.end=600, exclude=NULL, draw.elec.lab = F, draw.nose = T, draw.ears=T,draw.elec.pos = F, interp.points=50)

