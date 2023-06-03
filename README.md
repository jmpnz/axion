# axion : a small, statically typed and compiled language for writing scripts

```javascript

const answer : int = 42;

fn sumArray(arr: array[int], size: int) -> integer {
    let sum: int = 0;
    let i:int = 0;
    for (i = 0;i < size;i++) {
        sum = sum + arr[i];
    }
    return sum;
}

```


