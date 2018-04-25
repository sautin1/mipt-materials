# QR Code Finder Pattern (FIP) Detector
## Результаты
### TestSet1
Среднее время работы на одном изображении: ~47.0 ms.

|                    | real positive | real negative |
|--------------------|---------------|---------------|
| predicted positive | TP=104        | FP=2          |
| predicted negative | FN=41         | -             |

![](https://latex.codecogs.com/gif.latex?\inline&space;PPV&space;=&space;precision&space;=&space;\frac&space;{TP}{TP&space;&plus;&space;FP}&space;\approx&space;0.981)

![](https://latex.codecogs.com/gif.latex?\inline&space;TPR&space;=&space;recall&space;=&space;\frac&space;{TP}{TP&space;&plus;&space;FN}&space;\approx&space;0.717)

![](https://latex.codecogs.com/gif.latex?\inline&space;F_1&space;=&space;\frac&space;{2&space;\cdot&space;precision&space;\cdot&space;recall}{precision&space;&plus;&space;recall}&space;=&space;0.829)

### TestSet2
Среднее время работы на одном изображении: ~37.2 ms.

|                    | real positive | real negative |
|--------------------|---------------|---------------|
| predicted positive | TP=92         | FP=1          |
| predicted negative | FN=55         | -             |

![](https://latex.codecogs.com/gif.latex?\inline&space;PPV&space;=&space;precision&space;=&space;\frac&space;{TP}{TP&space;&plus;&space;FP}&space;\approx&space;0.989)

![](https://latex.codecogs.com/gif.latex?\inline&space;TPR&space;=&space;recall&space;=&space;\frac&space;{TP}{TP&space;&plus;&space;FN}&space;\approx&space;0.626)

![](https://latex.codecogs.com/gif.latex?\inline&space;F_1&space;=&space;\frac&space;{2&space;\cdot&space;precision&space;\cdot&space;recall}{precision&space;&plus;&space;recall}&space;=&space;0.767)

### TestSet3
Среднее время работы на одном изображении: ~36.3 ms.

|                    | real positive | real negative |
|--------------------|---------------|---------------|
| predicted positive | TP=152        | FP=16         |
| predicted negative | FN=331        | -             |

![](https://latex.codecogs.com/gif.latex?\inline&space;PPV&space;=&space;precision&space;=&space;\frac&space;{TP}{TP&space;&plus;&space;FP}&space;\approx&space;0.905)

![](https://latex.codecogs.com/gif.latex?\inline&space;TPR&space;=&space;recall&space;=&space;\frac&space;{TP}{TP&space;&plus;&space;FN}&space;\approx&space;0.315)

![](https://latex.codecogs.com/gif.latex?\inline&space;F_1&space;=&space;\frac&space;{2&space;\cdot&space;precision&space;\cdot&space;recall}{precision&space;&plus;&space;recall}&space;=&space;0.467)

## Ссылки
[AI Shack](http://aishack.in/tutorials/scanning-qr-codes-verify-finder/)
