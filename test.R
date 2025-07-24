mycars <- within(mtcars, {
  vs <- factor(vs, labels = c("V", "S"))
  am <- factor(am, labels = c("automatic", "manual"))
  cyl <- ordered(cyl)
  gear <- ordered(gear)
  carb <- ordered(carb)
})

gears <- table(mycars$gear)

barplot(gears, main = "Title: Car gear distribution", xlab = "Number of Gears", col = "#05ae99")
am <- table(
  mycars$am
)
print(am)
