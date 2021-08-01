# Xats2cc-H0

For compiling from the H0-level syntax of ATS3/Xanadu to C/C++

## Download

```
git clone --recursive https://github.com/xanadu-lang/Xats2cc-H0.git
```

### Prerequisites

ATS2-0.4.2 or a later version is required,
which is available [here](http://www.ats-lang.org/Downloads.html)

### Build with ATS2

```
(cd Xats2cc-H0 && sh build.sh)
```

### Testing

```
cd Xats2cc-H0
./bin/xats2cc -h
make -C srcgen/TEST -f Makefile_test
```

### Remarks

- Xats2cc-H0 is for debugging the implementation ATS3/Xanadu

- Xats2cc-H0 is meant to be a practical compiler from XATSCML to C/C++

- This project should be constantly under construction for quite some time
