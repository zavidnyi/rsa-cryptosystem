# RSA cryptosystem
[RSA](https://people.csail.mit.edu/rivest/Rsapaper.pdf) cryptosystem implementation made in Haskell programming language. Note, that this implementation is not secure and was mainly made for course completion requirements 😊

## Setup

Download the code:
```shell
git clone https://github.com/zavidnyi/rsa-cryptosystem.git
```

From repository directory run the following command to generate executable:
```shell
./build.sh
```
This will generate executable in main folder which you can run as
```
./rsa [arguments]
```

## Key generation
To generate private and public keys pair run:
```shell
./rsa -gen [key-size in bits, defaults to 512]
```

## To be continued