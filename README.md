# profilr

Profile predictions given a fitted model.

```{r}
library(ggplot2)
library(profilr)
fit <- glm(hp ~ mpg + wt, data = mtcars)
ci  <- make_confidence_intervals(fit, newdata = build_mesh(fit))
ggplot(data = ci, mapping = aes(x = mpg, y = .estimate)) +
  geom_point(data = mtcars, mapping = aes(y = hp)) +
  geom_line() +
  geom_ribbon(mapping = aes(ymin = .lower, ymax = .upper),
              alpha = 0.3)
```

