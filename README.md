# RSA cryptosystem
[RSA cryptosystem](https://people.csail.mit.edu/rivest/Rsapaper.pdf) implementation made in Haskell programming language. Note, that this implementation is not secure and was mainly made for course completion requirements 😊

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

## Encryption/Decryption
You can encrypt any ASCII text file using:
```
./rsa -encrypt [path to key] [path to text]
```


Similary to decrypt:
```
./rsa -decrypt [path to key] [path to text]
```

The commands above print text to standart ouput if you want to write to file use ```> [filename]```, for example this command writes encrypted text to file `A.txt`
```
./rsa -encrypt pub.key text > A.txt
```